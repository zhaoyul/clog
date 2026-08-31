;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic component and page rendering   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-render
  (:export #:render
           #:render-page))

(defpackage #:clog-hypermedia
  (:import-from #:clog-render
                #:render
                #:render-page)
  (:export #:render
           #:render-page))

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
       (safe-render-string-p value :allow-empty t)))

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

(defun render-page-shell
    (application context root-html
     &key title
          (assets nil)
          (sse-p nil)
          (websocket-p nil)
          (language "en"))
  "Render the HM-014 complete page shell around trusted ROOT-HTML."
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

(defun deterministic-spinneret-text (value)
  "Render VALUE as escaped text using deterministic Spinneret settings."
  (let ((*print-pretty* nil)
        (spinneret:*html-style* :tree)
        (spinneret:*suppress-inserted-spaces* t)
        (spinneret:*always-quote* t))
    (spinneret:with-html-string
      (spinneret:html value))))

(defun render-context-request-id (context)
  "Return CONTEXT's request ID, or NIL for isolated :TEST rendering."
  (let ((request (render-context-request context)))
    (and request (clog-http:request-id request))))

(defun render-component-correlation (component context)
  "Return COMPONENT and request identifiers for typed renderer conditions."
  (values (and component (clog-component:component-id component))
          (render-context-request-id context)))

(defun signal-rendering-error
    (condition-type reason component context &key cause kind)
  "Signal CONDITION-TYPE with bounded renderer correlation metadata."
  (multiple-value-bind (component-id request-id)
      (render-component-correlation component context)
    (if (eq condition-type 'render-purity-violation)
        (error condition-type
               :reason reason
               :component-id component-id
               :request-id request-id
               :cause cause
               :kind kind)
        (error condition-type
               :reason reason
               :component-id component-id
               :request-id request-id
               :cause cause))))

(defun ensure-renderable-component (component context)
  "Reject inactive components before user rendering begins."
  (unless (clog-component:mounted-p component)
    (signal-rendering-error
     'rendering-error :component-not-mounted component context))
  component)

(defun registry-membership-snapshot (component context)
  "Return a deterministic session registry membership snapshot, or :UNOBSERVED.

Store enumeration acquires and releases only the registry lock before user
rendering begins. The snapshot contains IDs and object identity, not mutable
component state, so unrelated revision changes do not create false positives."
  (let* ((application (render-context-application context))
         (request (render-context-request context))
         (store
           (and application
                (clog-hypermedia:application-component-store application)))
         (session-id
           (and request (clog-http:request-session-id request))))
    (if (and store
             (clog-component:component-store-p store)
             session-id
             (eq :session (clog-component:component-scope component)))
        (mapcar (lambda (member)
                  (cons (clog-component:component-id member) member))
                (clog-component:enumerate-components store session-id))
        :unobserved)))

(defun same-registry-membership-p (before after)
  "Return true when BEFORE and AFTER contain the same ordered IDs and objects."
  (or (and (eq before :unobserved) (eq after :unobserved))
      (and (listp before)
           (listp after)
           (= (length before) (length after))
           (every (lambda (left right)
                    (and (string= (car left) (car right))
                         (eq (cdr left) (cdr right))))
                  before
                  after))))

(defmacro with-component-purity-guard
    ((component context failure-reason) &body body)
  "Evaluate BODY as a pure component render protocol operation.

No component-store lock is held while BODY runs. Revision and session registry
membership are sampled before and after. A renderer that mutates either boundary
signals RENDER-PURITY-VIOLATION even when its own body also signaled an error."
  `(progn
     (ensure-renderable-component ,component ,context)
     (let* ((before-revision
              (clog-component:component-revision ,component))
            (before-registry
              (registry-membership-snapshot ,component ,context))
            (result nil)
            (failure nil))
       (with-current-render-bindings (,component ,context)
         (let ((*print-pretty* nil)
               (spinneret:*html-style* :tree)
               (spinneret:*suppress-inserted-spaces* t)
               (spinneret:*always-quote* t))
           (handler-case
               (setf result (progn ,@body))
             (error (condition)
               (setf failure condition)))))
       (let ((after-revision
               (clog-component:component-revision ,component))
             (after-registry
               (registry-membership-snapshot ,component ,context)))
         (cond
           ((/= before-revision after-revision)
            (signal-rendering-error
             'render-purity-violation
             :component-revision-changed
             ,component
             ,context
             :cause failure
             :kind :component-revision))
           ((not (same-registry-membership-p
                  before-registry after-registry))
            (signal-rendering-error
             'render-purity-violation
             :component-registry-changed
             ,component
             ,context
             :cause failure
             :kind :component-registry))))
       (when failure
         (if (typep failure 'rendering-error)
             (error failure)
             (signal-rendering-error
              'rendering-error
              ,failure-reason
              ,component
              ,context
              :cause failure)))
       result)))

(defun normalize-component-render-result (result component context)
  "Convert a component render RESULT to a validated HTML string."
  (let ((string
          (typecase result
            (trusted-html (%trusted-html-string result))
            (string result)
            (t
             (signal-rendering-error
              'invalid-render-result
              :component-renderer-returned-unsupported-value
              component
              context)))))
    (unless (safe-render-string-p string :allow-empty t)
      (signal-rendering-error
       'invalid-render-result
       :component-renderer-returned-invalid-unicode
       component
       context))
    (copy-seq string)))

(defmethod clog-component:render-component :around
    ((component clog-component:component) (context render-context))
  "Guard component rendering, bind context metadata and return validated HTML.

The concrete method may use SPINNERET:WITH-HTML-STRING or return TRUSTED-HTML.
It must not mutate component revision or session registry membership."
  (normalize-component-render-result
   (with-component-purity-guard
       (component context :component-render-failed)
     (call-next-method))
   component
   context))

(defun render (value context)
  "Render VALUE to a UTF-8 Common Lisp string using CONTEXT.

Components dispatch through the guarded RENDER-COMPONENT protocol. Ordinary
strings, characters, numbers and symbols are escaped by Spinneret. TRUSTED-HTML
is emitted verbatim through its explicit trust boundary. NIL renders as the
empty string. The function performs no response/network write."
  (check-type context render-context)
  (typecase value
    (clog-component:component
     (clog-component:render-component value context))
    (trusted-html
     (trusted-html-string value))
    (null "")
    ((or string character number symbol)
     (deterministic-spinneret-text value))
    (t
     (signal-rendering-error
      'invalid-render-result
      :unsupported-render-value
      nil
      context))))

(defun call-component-title (component context)
  "Read COMPONENT's optional title under the same pure-render guard."
  (let ((title
          (with-component-purity-guard
              (component context :component-title-failed)
            (clog-component:component-title component context))))
    (unless (or (null title)
                (safe-render-string-p title))
      (signal-rendering-error
       'invalid-render-result
       :invalid-component-title
       component
       context))
    (and title (copy-seq title))))

(defun call-component-assets (component context)
  "Read COMPONENT's immutable asset contribution under the pure-render guard."
  (let ((assets
          (with-component-purity-guard
              (component context :component-assets-failed)
            (clog-component:component-assets component context))))
    (unless (proper-asset-list-p assets)
      (signal-rendering-error
       'invalid-render-result
       :malformed-component-assets
       component
       context))
    (dolist (descriptor assets)
      (unless (asset-p descriptor)
        (signal-rendering-error
         'invalid-render-result
         :invalid-component-asset
         component
         context)))
    (copy-list assets)))

(defun ensure-page-render-context (component context)
  "Validate CONTEXT for complete page rendering of COMPONENT."
  (check-type context render-context)
  (unless (eq :page (render-context-mode context))
    (signal-rendering-error
     'invalid-render-context :page-mode-required component context))
  (unless (render-context-request context)
    (signal-rendering-error
     'invalid-render-context :request-context-required component context))
  (unless (render-context-application context)
    (signal-rendering-error
     'invalid-render-context :application-required-for-page component context))
  (let ((primary (render-context-primary-component-id context)))
    (when (and primary
               (not (string=
                     primary
                     (clog-component:component-id component))))
      (signal-rendering-error
       'invalid-render-context
       :primary-component-id-mismatch
       component
       context)))
  context)

(defun validate-extra-page-assets (assets component context)
  "Return a fresh validated list of page-level ASSETS."
  (unless (proper-asset-list-p assets)
    (signal-rendering-error
     'invalid-render-result
     :malformed-page-assets
     component
     context))
  (dolist (descriptor assets)
    (unless (asset-p descriptor)
      (signal-rendering-error
       'invalid-render-result
       :invalid-page-asset
       component
       context)))
  (copy-list assets))

(defun render-component-page
    (component context
     &key title
          (assets nil)
          (sse-p nil)
          (websocket-p nil)
          (language "en"))
  "Render COMPONENT into the complete offline page shell described by CONTEXT."
  (ensure-page-render-context component context)
  (let* ((application (render-context-application context))
         (request (render-context-request context))
         (component-title (or title (call-component-title component context)))
         (all-assets
           (append (render-context-assets context)
                   (call-component-assets component context)
                   (validate-extra-page-assets assets component context)))
         (root-html (render component context)))
    (render-page-shell
     application
     request
     root-html
     :title component-title
     :assets all-assets
     :sse-p sse-p
     :websocket-p websocket-p
     :language language)))

(defun render-trusted-page
    (trusted context
     &key title
          (assets nil)
          (sse-p nil)
          (websocket-p nil)
          (language "en"))
  "Render explicit TRUSTED-HTML through CONTEXT's complete page shell."
  (check-type context render-context)
  (unless (eq :page (render-context-mode context))
    (signal-rendering-error
     'invalid-render-context :page-mode-required nil context))
  (let ((application (render-context-application context))
        (request (render-context-request context)))
    (unless application
      (signal-rendering-error
       'invalid-render-context :application-required-for-page nil context))
    (unless request
      (signal-rendering-error
       'invalid-render-context :request-context-required nil context))
    (render-page-shell
     application
     request
     (trusted-html-string trusted)
     :title title
     :assets (append
              (render-context-assets context)
              (validate-extra-page-assets assets nil context))
     :sse-p sse-p
     :websocket-p websocket-p
     :language language)))

(defun render-page (subject context &rest arguments)
  "Render a complete HTML page while preserving the HM-014 call contract.

Legacy-compatible form:
  (RENDER-PAGE APPLICATION REQUEST-CONTEXT TRUSTED-ROOT-STRING &KEY ...)

Component form:
  (RENDER-PAGE COMPONENT RENDER-CONTEXT &KEY ...)

Explicit trusted form:
  (RENDER-PAGE TRUSTED-HTML RENDER-CONTEXT &KEY ...)

Component and trusted forms derive request, application, assets and CSP metadata
from the immutable render context. Fragment rendering uses RENDER instead and
never introduces html/head/body."
  (cond
    ((typep subject 'clog-hypermedia:hypermedia-application)
     (unless arguments
       (error 'asset-error :reason :missing-root-html))
     (let ((root-html (first arguments))
           (options (rest arguments)))
       (apply #'render-page-shell
              subject
              context
              root-html
              options)))
    ((typep subject 'clog-component:component)
     (apply #'render-component-page subject context arguments))
    ((trusted-html-p subject)
     (apply #'render-trusted-page subject context arguments))
    (t
     (if (typep context 'render-context)
         (signal-rendering-error
          'invalid-render-result
          :unsupported-page-root
          nil
          context)
         (error 'asset-error :reason :unsupported-page-root)))))
