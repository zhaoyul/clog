# CLOG 3 Hypermedia Counter

`clog/hypermedia-counter` 是 CLOG 3 Hypermedia Runtime 的第一个完整纵向示例. 它刻意保持很小, 但覆盖一条真实生产链路: full-page GET, session-scoped component, CSRF-protected action, HTMX 4 `outerMorph`, 离线 vendored assets, JavaScript-off Post/Redirect/Get, 双 session 隔离以及显式 server lifecycle.

## 1. 加载并启动

在 REPL 中:

```lisp
(ql:quickload :clog/hypermedia-counter)

(defparameter *counter-server*
  (clog-hypermedia-counter:start-counter :port 0))

(clog-hypermedia-counter:counter-server-url *counter-server*)
;; => "http://127.0.0.1:49152"  ; 端口每次可能不同
```

`START-COUNTER` 不创建全局 server 单例. 返回的 `COUNTER-SERVER` handle 明确拥有 Clack/Hunchentoot 生命周期. `:port 0` 或 `:port nil` 会先选择一个当前可用的本地端口.

停止服务:

```lisp
(clog-hypermedia-counter:stop-counter *counter-server*)
```

`STOP-COUNTER` 可以重复调用, 第二次是 no-op. Counter 自己不创建额外后台 worker, subscription 或 timer.

## 2. 用浏览器观察完整页面

打开 `COUNTER-SERVER-URL` 后面的 `/counter`.

页面只有一个 session-scoped Counter component, 并提供 `+1`, `-1`, `Reset` 三个普通 `<form method="post">` 操作. 因此 HTML 本身就是可工作的基础协议, HTMX 只是渐进增强层.

启用 JavaScript 时, 每个 form 同时带有:

```html
hx-post="/_clog/action/<component-id>/<action>"
hx-target="#<component-id>"
hx-swap="outerMorph"
hx-nonce="<request-csp-nonce>"
```

提交后服务器执行 action, 提交 component revision, 重新 render 当前 component, 然后 HTMX 只更新 Counter root. 页面 URL 不发生导航.

关闭 JavaScript 时, 浏览器忽略 `hx-*` 属性并执行普通 form POST. HM-026 fallback 将成功 mutation 转换为 `303 See Other -> /counter`, 因此刷新页面只重复 GET, 不会再次执行 POST.

## 3. Session 隔离

用两个独立浏览器 profile 或两个独立 WebDriver session 打开 `/counter`.

```text
Session A: 0 -> +1 -> 1
Session B: 0 -> -1 -> -1
```

A 和 B 的值互不影响. Component ID 也是各自 session registry 中的独立对象.

## 4. 离线 HTMX 与 CSP

完整页面不访问 CDN. `MAKE-COUNTER-APPLICATION` 使用 Hypermedia 默认的 vendored asset mount 和 strict CSP, 页面加载:

```text
/_clog/static/vendor/htmx/4.0.0/htmx.min.js
/_clog/static/vendor/htmx/4.0.0/hx-csp.min.js
```

HTMX 4 的 `hx-csp` 扩展会校验 HTMX 元素上的 nonce. Counter 把每个请求上下文的 nonce 写入所有带 `hx-*` 的 form 的 `hx-nonce` 属性. Action fragment 使用 action 请求自己的 response nonce, `hx-csp` 再根据 response CSP header 将 fragment nonce 安全映射回当前页面 nonce. 缺少或不匹配的 nonce 会 fail closed.

## 5. Component 与 Action

Counter state 位于服务器端 `counter-component`. 三个 mutation 使用 HM-024/HM-025 的静态 `DEFACTION` registry. Browser 只提交固定 action external-name 和表单数据, 不会把 Lisp symbol, function name 或 JavaScript source 当作运行时协议发送到服务器.

每个 action form 还带有 `_csrf_token`, `_clog_revision`, `_clog_return_to`. CSRF 在 dispatch 之前由 Lack middleware 校验. `requires-current` action 使用 `_clog_revision` 防止 stale mutation.

## 6. REPL 迭代

Component renderer 和 action 都是普通 Common Lisp method/function. 在开发 image 中重新编译相关定义后, 后续 HTTP/HTMX 请求直接使用新定义, 不需要把 arbitrary JavaScript 从服务器发送给浏览器执行.

```text
HTTP request
-> immutable request-context
-> deterministic router
-> session / CSRF middleware
-> action registry + dispatcher
-> server-side component state
-> Spinneret render
-> HTML fragment
-> HTMX outerMorph
```

## 7. 运行自动验收

Common Lisp 测试:

```lisp
(asdf:test-system :clog/hypermedia-counter)
```

主 Hypermedia suite 也会把 HM-027 Counter tests 作为 test-op 依赖执行:

```lisp
(asdf:test-system :clog/hypermedia-tests)
```

在 GitHub Actions 中, Counter test-op 会自动启动随机端口的真实 Hunchentoot server, 再使用 Ubuntu runner 预装的 Chrome/Chromium + ChromeDriver 执行 `tests/browser/counter.spec.py`. Browser spec 只依赖 Python 标准库和 WebDriver HTTP 协议, 不需要安装 Playwright/Selenium Python package.

本地要执行同一道 browser gate, 需要 Chrome 或 Chromium 以及匹配的 ChromeDriver:

```bash
CLOG_RUN_BROWSER_E2E=1 sbcl --non-interactive \
  --eval '(ql:quickload :clog/hypermedia-counter-tests)' \
  --eval '(asdf:test-system :clog/hypermedia-counter-tests)'
```

也可以先在 REPL 启动 Counter, 然后直接运行:

```bash
python3 tests/browser/counter.spec.py --base-url http://127.0.0.1:49152
```

如浏览器或 driver 不在 PATH, 可通过 `--browser` 和 `--driver` 显式指定.

Browser acceptance 同时验证:

1. JavaScript-on HTMX forms 具有 `hx-nonce`.
2. JavaScript-on action 的真实网络请求携带 `HX-Request: true`.
3. `outerMorph` 后 Counter root ID 保持稳定, 页面不发生整页导航.
4. 两个独立 WebDriver session 的 state 相互隔离.
5. JavaScript-off increment/decrement/reset 可用.
6. JavaScript-off action 不携带 `HX-Request`, 网络日志出现 `303` PRG transition.
7. Reload 不重复上一次 POST.
8. HTMX script 来自 same-origin vendored path.

## 8. 当前边界

HM-027 故意不实现 multi-target partial, typed action effects, UI transaction, child component tree, SSE 或 WebSocket. 这些能力从 HM-030 开始逐层加入. Counter 的任务是把 P0-P2 已完成的能力压成一个可运行, 可测试, 可阅读的参考应用, 而不是提前把后续阶段塞进示例.
