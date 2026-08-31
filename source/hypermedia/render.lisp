;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime complete offline HTML page shell              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-render
  (:export #:render-page))

(defpackage #:clog-hypermedia
  (:import-from #:clog-render
                #:render-page)
  (:export #:render-page))

(in-package #:clog-render)

(defparameter +vendored-htmx-version+ "4.0.0"
  "The only HTMX version authenticated and vendored by HM-003.")

(defun html-language-tag-p (value)
  "Return true for a small deterministic HTML language tag."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (or (alphanumericp character)
                    (char= character #\-)))
              value)))

(defun validate-page-string (value reason &key allow-empty)
  "Return VALUE when it is a safe metadata string, otherwise signal ASSET-ERROR."
  (unless (safe-asset-string-p value :allow-empty allow-empty)
    (error 'asset-error :reason reason))
  value)

(defun safe-root-html-p (value)
  "Return true for server-rendered markup without forbidden control bytes."
  (and (stringp value)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (or (and (>= code 32) (/= code 127))
                      (member character '(#\Tab #\Newline #\Return)
                              :test #'char=))))
              value)))

(defun validate-root-html (value)
  "Return trusted server-rendered VALUE or signal ASSET-ERROR."
  (unless (safe-root-html-p value)
    (error 'asset-error :reason :invalid-root-html))
  value)

(defun application-page-title (application title)
  "Return explicit TITLE or a stable title derived from APPLICATION."
  (let ((value
          (or title
              (let ((name (clog-hypermedia:application-name application)))
                (etypecase name
                  (string name)
                  (symbol (symbol-name name)))))))
    (validate-page-string value :invalid-page-title)))

(defun validate-page-language (language)
  "Return LANGUAGE or signal ASSET-ERROR for an unsafe lang attribute."
  (unless (html-language-tag-p language)
    (error 'asset-error :reason :invalid-page-language))
  language)

(defun validate-page-feature-flag (value reason)
  "Return boolean feature VALUE or signal ASSET-ERROR with REASON."
  (validate-asset-boolean value reason))

(defun vendored-asset-prefix (configuration)
  "Return the configured local static prefix for vendored framework assets."
  (let ((prefix
          (clog-hypermedia:configuration-static-prefix configuration))
        (root
          (clog-hypermedia:configuration-static-root configuration)))
    (unless (and prefix root)
      (error 'asset-error :reason :vendored-assets-not-mounted))
    prefix))

(defun vendored-htmx-url (configuration filename)
  "Return the local URL for one authenticated HTMX distribution file."
  (let ((version
          (clog-hypermedia:configuration-htmx-version configuration)))
    (unless (string= version +vendored-htmx-version+)
      (error 'asset-error :reason :unsupported-vendored-htmx-version))
    (concatenate
     'string
     (vendored-asset-prefix configuration)
     "vendor/htmx/"
     version
     "/"
     filename)))

(defun make-vendored-script-asset
    (configuration filename key nonce-required-p)
  "Create one deferred local HTMX script descriptor."
  (make-asset :type :script
              :url (vendored-htmx-url configuration filename)
              :defer-p t
              :nonce-required-p nonce-required-p
              :key key))

(defun framework-page-assets (configuration sse-p websocket-p)
  "Return deterministic vendored HTMX assets selected for one full page."
  (ecase (clog-hypermedia:configuration-assets-mode configuration)
    (:none nil)
    (:vendored
     (let ((nonce-required-p
             (clog-hypermedia:configuration-strict-csp-p configuration)))
       (append
        (list
         (make-vendored-script-asset
          configuration "htmx.min.js" :clog-htmx-core nonce-required-p))
        (when nonce-required-p
          (list
           (make-vendored-script-asset
            configuration "hx-csp.min.js" :clog-htmx-csp nonce-required-p)))
        (when sse-p
          (list
           (make-vendored-script-asset
            configuration "hx-sse.min.js" :clog-htmx-sse nonce-required-p)))
        (when websocket-p
          (list
           (make-vendored-script-asset
            configuration "hx-ws.min.js" :clog-htmx-ws nonce-required-p))))))))

(defun partition-custom-page-assets (assets)
  "Return custom styles and scripts, preserving order inside each type."
  (let ((styles nil)
        (scripts nil))
    (dolist (descriptor (deduplicate-assets assets))
      (ecase (%asset-type descriptor)
        (:style (push descriptor styles))
        (:script (push descriptor scripts))))
    (values (nreverse styles) (nreverse scripts))))

(defun page-assets (configuration assets sse-p websocket-p)
  "Return the frozen full-page loading order.

Application styles load first, followed by HTMX core, CSP, SSE and WebSocket
extensions, followed by application scripts. Each subgroup preserves caller or
framework declaration order, and RENDER-ASSETS performs final cross-group
conflict detection and de-duplication."
  (multiple-value-bind (styles scripts)
      (partition-custom-page-assets assets)
    (append styles
            (framework-page-assets configuration sse-p websocket-p)
            scripts)))

(defun page-csp-nonce (context configuration)
  "Return the request-context nonce required by strict CSP, or NIL."
  (let ((nonce (clog-http:request-csp-nonce context)))
    (when (clog-hypermedia:configuration-strict-csp-p configuration)
      (validate-render-nonce nonce))
    nonce))

(defun page-csrf-token (context)
  "Return the Lack-owned CSRF token for CONTEXT."
  (let ((token (clog-session:csrf-token-for context)))
    (validate-page-string token :invalid-csrf-token)
    token))

(defun render-page-document
    (language title csrf-parameter csrf-token assets-markup root-html)
  "Return one deterministic complete HTML document string."
  (with-output-to-string (stream)
    (write-string "<!doctype html>" stream)
    (terpri stream)
    (write-string "<html lang=\"" stream)
    (write-html-escaped language stream)
    (write-string "\">" stream)
    (terpri stream)
    (write-string "<head>" stream)
    (terpri stream)
    (write-string "<meta charset=\"utf-8\">" stream)
    (terpri stream)
    (write-string
     "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
     stream)
    (terpri stream)
    (write-string "<title>" stream)
    (write-html-escaped title stream)
    (write-string "</title>" stream)
    (terpri stream)
    (write-string "<meta name=\"csrf-param\" content=\"" stream)
    (write-html-escaped csrf-parameter stream)
    (write-string "\">" stream)
    (terpri stream)
    (write-string "<meta name=\"csrf-token\" content=\"" stream)
    (write-html-escaped csrf-token stream)
    (write-string "\">" stream)
    (unless (zerop (length assets-markup))
      (terpri stream)
      (write-string assets-markup stream))
    (terpri stream)
    (write-string "</head>" stream)
    (terpri stream)
    (write-string "<body>" stream)
    (terpri stream)
    (write-string root-html stream)
    (terpri stream)
    (write-string "</body>" stream)
    (terpri stream)
    (write-string "</html>" stream)))

(defun render-page
    (application context root-html
     &key title
          (assets nil)
          (sse-p nil)
          (websocket-p nil)
          (language "en"))
  "Render a complete offline-capable HTML response for APPLICATION.

ROOT-HTML is trusted server-rendered markup and is inserted without escaping.
TITLE, CSRF metadata and all asset attributes are escaped. Vendored HTMX core
is loaded by default; strict CSP automatically adds hx-csp and propagates only
the nonce stored in CONTEXT. SSE and WebSocket extensions are opt-in per page.
Application styles precede framework scripts and application scripts follow
them, producing one stable execution order without CDN references."
  (check-type application clog-hypermedia:hypermedia-application)
  (check-type context clog-http:request-context)
  (validate-root-html root-html)
  (validate-page-feature-flag sse-p :invalid-sse-flag)
  (validate-page-feature-flag websocket-p :invalid-websocket-flag)
  (let* ((configuration
           (clog-hypermedia:application-configuration application))
         (title (application-page-title application title))
         (language (validate-page-language language))
         (csrf-parameter
           (clog-hypermedia:configuration-csrf-form-token configuration))
         (csrf-token (page-csrf-token context))
         (assets-markup
           (progn
             (page-csp-nonce context configuration)
             (render-assets
              (page-assets configuration assets sse-p websocket-p)
              context))))
    (clog-http:html-response
     (render-page-document
      language
      title
      csrf-parameter
      csrf-token
      assets-markup
      root-html))))
