;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime response tests                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun clack-response-header-values (clack-response name)
  "Return all values for keyword header NAME from a normal CLACK-RESPONSE."
  (loop for (key value) on (second clack-response) by #'cddr
        when (eq key name)
          collect value))

(defun clack-response-header (clack-response name &optional default)
  "Return the first NAME header from CLACK-RESPONSE, or DEFAULT."
  (let ((values (clack-response-header-values clack-response name)))
    (if values (first values) default)))

(test response/status-header-body-encoding
  (let* ((response (clog-hypermedia:html-response "hello"))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 200 (first clack)))
    (is (string= "text/html; charset=utf-8"
                 (clack-response-header clack :content-type)))
    (is (= 5 (clack-response-header clack :content-length)))
    (is (string= "hello" (third clack)))))

(test response/html-content-type-can-be-explicit
  (let* ((response
           (clog-hypermedia:html-response
            "<p>x</p>"
            :headers '(:content-type "text/html; charset=iso-8859-1")))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (equal '("text/html; charset=iso-8859-1")
               (clack-response-header-values clack :content-type)))
    (is (= 8 (clack-response-header clack :content-length)))))

(test response/multiple-set-cookie-values-are-preserved
  (let* ((cookies '("sid=one; Path=/; HttpOnly"
                    "flash=two; Path=/; SameSite=Lax"))
         (response
           (clog-hypermedia:html-response
            "ok"
            :headers (list :set-cookie (first cookies)
                           :set-cookie (second cookies))))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (equal cookies (clack-response-header-values clack :set-cookie)))
    (is (= 2 (length (clack-response-header-values clack :set-cookie))))))

(test response/crlf-and-control-characters-are-rejected
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:make-response
     :headers (list :x-test (format nil "safe~C~CInjected: yes"
                                    #\Return #\Linefeed))))
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:make-response
     :headers (list :x-test (format nil "bad~Cvalue" (code-char 0)))))
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:make-response :headers '("X-Test" "value"))))

(test response/mutated-header-is-revalidated-before-clack-encoding
  (let ((response
          (clog-hypermedia:make-response :headers '(:x-test "safe"))))
    (setf (second (clog-hypermedia:response-headers response))
          (format nil "unsafe~Cvalue" #\Linefeed))
    (signals clog-hypermedia:invalid-response-header
      (clog-hypermedia:response->clack-response response))))

(test response/utf8-content-length-uses-encoded-bytes
  (let* ((response (clog-hypermedia:html-response "你好"))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 6 (clack-response-header clack :content-length))))
  (let* ((response (clog-hypermedia:html-response '("A" "你" "B")))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 5 (clack-response-header clack :content-length)))))

(test response/explicit-content-length-must-match
  (let ((response
          (clog-hypermedia:html-response
           "hello"
           :headers '(:content-length 4))))
    (signals clog-hypermedia:response-content-length-mismatch
      (clog-hypermedia:response->clack-response response)))
  (let* ((response
           (clog-hypermedia:html-response
            "hello"
            :headers '(:content-length 5)))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 5 (clack-response-header clack :content-length)))))

(test response/pathname-body-remains-server-owned
  (let* ((pathname #p"/tmp/clog-hm011-response-test.txt")
         (response
           (clog-hypermedia:make-response
            :status 200
            :headers '(:content-type "text/plain; charset=utf-8")
            :body pathname
            :kind :stream))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (pathnamep (third clack)))
    (is (equal pathname (third clack)))
    (is (null (clack-response-header-values clack :content-length)))))

(test response/delayed-response-placeholder-follows-clack-protocol
  (let* ((responder-function
           (lambda (responder)
             (funcall responder '(200 (:content-type "text/plain") ("later")))))
         (response
           (clog-hypermedia:make-response
            :body responder-function
            :kind :stream)))
    (is (eq responder-function
            (clog-hypermedia:response->clack-response response))))
  (let ((response
          (clog-hypermedia:make-response
           :headers '(:x-test "would-be-lost")
           :body (lambda (responder) (declare (ignore responder)))
           :kind :stream)))
    (signals clog-hypermedia:invalid-response-body
      (clog-hypermedia:response->clack-response response))))

(test response/no-content-response-is-explicit
  (let* ((response (clog-hypermedia:no-content-response))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 204 (first clack)))
    (is (null (third clack)))
    (is (= 0 (clack-response-header clack :content-length)))
    (is (null (clack-response-header-values clack :content-type)))))

(test response/redirect-relative-path-is-safe-by-default
  (let* ((response (clog-hypermedia:redirect-response "/machines/42?tab=live"))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (= 303 (first clack)))
    (is (string= "/machines/42?tab=live"
                 (clack-response-header clack :location)))
    (is (= 0 (clack-response-header clack :content-length)))))

(test response/redirect-rejects-unsafe-default-targets
  (dolist (url '("//evil.example/path"
                 "https://evil.example/path"
                 "javascript:alert(1)"
                 "/\\evil.example/path"))
    (signals clog-hypermedia:invalid-redirect-url
      (clog-hypermedia:redirect-response url)))
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     (format nil "/safe~C~CLocation: https://evil.example"
             #\Return #\Linefeed))))

(test response/redirect-external-origin-requires-exact-allowlist-match
  (let* ((response
           (clog-hypermedia:redirect-response
            "https://trusted.example:8443/dashboard?q=1"
            :allowed-origins '("https://trusted.example:8443")))
         (clack (clog-hypermedia:response->clack-response response)))
    (is (string= "https://trusted.example:8443/dashboard?q=1"
                 (clack-response-header clack :location))))
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example.evil/dashboard"
     :allowed-origins '("https://trusted.example")))
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example/dashboard"
     :allowed-origins '("https://trusted.example/path-is-not-an-origin"))))

(test response/redirect-origin-allowlist-is-defensively-copied
  (let* ((origin (copy-seq "https://trusted.example"))
         (target "https://trusted.example/dashboard")
         (response
           (clog-hypermedia:redirect-response
            target
            :allowed-origins (list origin))))
    (setf (char origin 0) #\x)
    (let ((clack (clog-hypermedia:response->clack-response response)))
      (is (string= target (clack-response-header clack :location))))))

(test response/redirect-origin-allowlist-must-contain-exact-origins
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example/dashboard"
     :allowed-origins '("https://trusted.example/path")))
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example/dashboard"
     :allowed-origins '(42)))
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example/dashboard"
     :allowed-origins '("https://trusted.example" . "bad-tail"))))

(test response/redirect-absolute-target-rejects-backslash
  (signals clog-hypermedia:invalid-redirect-url
    (clog-hypermedia:redirect-response
     "https://trusted.example\\@evil.example/dashboard"
     :allowed-origins '("https://trusted.example"))))

(test response/redirect-location-is-single-and-status-is-bounded
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:redirect-response
     "/safe"
     :headers '(:location "/other")))
  (signals clog-hypermedia:invalid-response-status
    (clog-hypermedia:redirect-response "/safe" :status 200))
  (signals clog-hypermedia:invalid-response-status
    (clog-hypermedia:make-response :status 99)))

(test response/duplicate-singleton-header-is-rejected
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:make-response
     :headers '(:content-type "text/plain"
                :content-type "text/html")))
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:make-response
     :headers '(:content-length 1 :content-length 1))))

(test response/normalize-response-does-not-bless-raw-triples
  (let ((clack (clog-hypermedia:normalize-response "hello")))
    (is (= 200 (first clack)))
    (is (string= "hello" (third clack))))
  (let ((clack (clog-hypermedia:normalize-response nil)))
    (is (= 204 (first clack))))
  (signals clog-hypermedia:invalid-response-body
    (clog-hypermedia:normalize-response '(200 () ("raw triple")))))
