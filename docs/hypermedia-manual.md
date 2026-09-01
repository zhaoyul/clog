# CLOG Hypermedia Manual

This manual documents the public server-rendered Hypermedia API introduced by the CLOG 3 runtime. The first section covers the HM-031 HTMX attribute helpers and their security contract.

## Safe HTMX action attributes

Use `component-action-attributes` when rendering a form or control that invokes a registered component action. It is the preferred high-level API because it derives the route, target, swap policy, and CSP nonce from framework-owned metadata instead of asking application code to rebuild those values.

```lisp
(spinneret:with-html
  (:form :attrs
         (clog-hypermedia:component-action-attributes
          component
          :save
          context)
    (:button :type "submit" "Save")))
```

For a component whose DOM id is `clog-c-...`, an action descriptor whose external name is `save`, and an application action prefix of `/_clog/action`, the helper produces the equivalent of:

```lisp
(:action "/_clog/action/clog-c-.../save"
 :method "post"
 :hx-post "/_clog/action/clog-c-.../save"
 :hx-target "#clog-c-..."
 :hx-swap "outerMorph"
 :hx-nonce "<request-csp-nonce>")
```

The returned value is a normal property list and can be passed directly to Spinneret `:attrs`.

### What the high-level helper owns

`component-action-attributes` deliberately obtains values from existing framework authorities:

- The action descriptor supplies the external action name. The Lisp symbol name is never guessed into a URL.
- The application configuration supplies `configuration-action-prefix` and the default swap policy.
- The component supplies its opaque component id and therefore its default root target.
- The render context supplies the request-derived CSP nonce when strict CSP is enabled.
- Only actions whose descriptor allows `POST` can be projected into these form attributes.

By default, the target is the component root and the swap is the configured default, normally `outerMorph`. A caller may override `:target` or `:swap` explicitly:

```lisp
(clog-hypermedia:component-action-attributes
 component
 :save
 context
 :target "#results"
 :swap "innerMorph")
```

Unknown action symbols, actions that do not allow `POST`, missing application metadata, and missing strict-CSP nonces fail closed with `invalid-htmx-attribute` rather than emitting a guessed attribute set.

## Low-level helpers

The lower-level helpers are useful when framework metadata is not available, for example in small reusable rendering utilities.

### `action-attrs`

`action-attrs` creates progressive HTML form attributes plus the matching `hx-post` attributes:

```lisp
(clog-hypermedia:action-attrs
 "/_clog/action"
 component-id
 "save"
 :target "#machine-card"
 :swap "outerMorph"
 :nonce nonce)
```

It emits native `action` and `method="post"` attributes as well as `hx-post`. This keeps the form usable when JavaScript or HTMX is unavailable.

The component id and action name are encoded as independent URL path segments. A slash, space, or other reserved character contained in either value therefore remains data and cannot become additional route structure. The action prefix itself must pass the existing same-origin local-URL policy and may not contain query or fragment syntax.

### `hx-attrs`

`hx-attrs` builds a typed HTMX attribute plist:

```lisp
(clog-hypermedia:hx-attrs
 :post "/machines/42/save"
 :target "#machine-card"
 :swap "outerMorph"
 :vals '(("mode" . "safe")
         ("count" . 2))
 :nonce nonce)
```

Supported values are deliberately narrow:

- `:post` must be a validated same-origin local URL.
- `:target` must be bounded text without control characters.
- `:swap` must be one of the framework's closed swap vocabulary.
- `:vals` must be a proper alist with unique string keys and is serialized by Yason.
- `:nonce` must be bounded non-whitespace text.

### `hx-vals` is data, not code

Do not pass JavaScript expressions to `hx-vals`. HM-031 intentionally has no raw-code escape hatch:

```lisp
;; Correct: structured data is serialized as JSON.
(clog-hypermedia:hx-attrs
 :post "/save"
 :vals '(("message" . "hello")))

;; Rejected: executable HTMX expression syntax.
(clog-hypermedia:hx-attrs
 :post "/save"
 :vals "js:{message: window.location}")
```

Both `js:` and `javascript:` forms are rejected. JSON quoting and escaping are owned by Yason so quotes, newlines, angle brackets, and other data characters are not hand-concatenated into an executable expression.

## Merging HTML attributes

Use `merge-html-attrs` instead of appending attribute plists manually when multiple helpers contribute to one element:

```lisp
(clog-hypermedia:merge-html-attrs
 '(:class "machine selected"
   :data-kind "machine")
 '(:class "selected compact"
   :aria-label "Save"))
```

The result is deterministic:

```lisp
(:class "machine selected compact"
 :data-kind "machine"
 :aria-label "Save")
```

`class` is the only duplicate attribute with merge semantics. Its tokens are unioned in first-seen order. Every other duplicate attribute fails closed. This prevents accidental ambiguity such as two `action`, `id`, or `hx-post` values where browser or library precedence could otherwise decide which security-sensitive value wins.

The generic merge helper also rejects inline event-handler attributes and raw `js:` / `javascript:` values for `hx-vals`. It is not a route around the typed HTMX API.

## Security rules for renderers

Application renderers should follow these rules:

1. Prefer `component-action-attributes` for registered component actions.
2. Pass helper results directly to Spinneret `:attrs`; do not stringify or concatenate HTML attributes manually.
3. Treat component ids and action names as opaque data. Let the helper encode path segments.
4. Pass structured Lisp data to `:vals`; never construct `hx-vals` JavaScript strings.
5. Merge independently produced attribute sets with `merge-html-attrs` so duplicate sensitive attributes fail closed.
6. Keep the default component-root target and `outerMorph` swap unless a narrower target or another supported swap is intentionally required.
7. In strict CSP mode, use the request render context so the framework can propagate the request's nonce consistently.

These rules preserve progressive enhancement: the native form remains a valid POST path, while HTMX adds partial-page behavior without introducing a second request URL, raw JavaScript payload syntax, or an alternate client-side authority for action routing.
