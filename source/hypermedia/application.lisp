;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime application and middleware pipeline           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-hypermedia
  (:export #:hypermedia-application
           #:hypermedia-application-error
           #:hypermedia-application-error-reason
           #:make-hypermedia-application
           #:make-hypermedia-app
           #:application-name
           #:application-router
           #:application-layout
           #:application-configuration
           #:application-component-store
           #:application-event-bus
           #:application-handler
           #:as-clack-app))

(in-package #:clog-hypermedia)

(define-condition hypermedia-application-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :reader hypermedia-application-error-reason)
   (cause
    :initarg :cause
    :initform nil
    :reader %hypermedia-application-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Hypermedia application failure (~A)."
             (hypermedia-application-error-reason condition))))
  (:documentation
   "Redacted application construction or middleware pipeline failure."))

(defclass hypermedia-application ()
  ((name
    :initarg :name
    :reader %application-name)
   (router
    :initarg :router
    :reader %application-router)
   (layout
    :initarg :layout
    :reader %application-layout)
   (configuration
    :initarg :configuration
    :reader %application-configuration)
   (component-store
    :initarg :component-store
    :reader %application-component-store)
   (event-bus
    :initarg :event-bus
    :reader %application-event-bus)
   (handler
    :initarg :handler
    :reader %application-handler))
  (:documentation
   "Server-independent Hypermedia application and its composed Clack handler."))

(defun application-name (application)
  "Return a defensive copy of APPLICATION's name."
  (check-type application hypermedia-application)
  (let ((name (%application-name application)))
    (if (stringp name) (copy-seq name) name)))

(defun application-router (application)
  "Return APPLICATION's deterministic router."
  (check-type application hypermedia-application)
  (%application-router application))

(defun application-layout (application)
  "Return APPLICATION's optional layout function."
  (check-type application hypermedia-application)
  (%application-layout application))

(defun application-configuration (application)
  "Return APPLICATION's immutable configuration object."
  (check-type application hypermedia-application)
  (%application-configuration application))

(defun application-component-store (application)
  "Return APPLICATION's optional component store."
  (check-type application hypermedia-application)
  (%application-component-store application))

(defun application-event-bus (application)
  "Return APPLICATION's optional live event bus."
  (check-type application hypermedia-application)
  (%application-event-bus application))

(defun application-handler (application)
  "Return APPLICATION's standard one-argument Clack application function."
  (check-type application hypermedia-application)
  (%application-handler application))

(defun as-clack-app (application)
  "Return APPLICATION as a Clack application function."
  (application-handler application))

(defun response-header-value (response name)
  "Return the first keyword header NAME from a normal Clack RESPONSE."
  (loop for (key value) on (second response) by #'cddr
        when (eq key name)
          do (return value)))

(defun remove-response-header (headers name)
  "Return HEADERS without entries named NAME, preserving all other order."
  (loop for (key value) on headers by #'cddr
        unless (eq key name)
          append (list key value)))

(defun set-response-header (headers name value)
  "Return HEADERS with one trailing NAME/VALUE entry."
  (append (remove-response-header headers name)
          (list name value)))

(defun split-comma-values (value)
  "Split a comma-separated response header into trimmed non-empty tokens."
  (if (not (stringp value))
      nil
      (let ((tokens nil)
            (start 0)
            (length (length value)))
        (loop
          for comma = (position #\, value :start start)
          for end = (or comma length)
          for token = (string-trim '(#\Space #\Tab)
                                   (subseq value start end))
          when (plusp (length token))
            do (push token tokens)
          do (if comma
                 (setf start (1+ comma))
                 (return (nreverse tokens)))))))

(defun merge-vary-header (existing)
  "Return EXISTING Vary tokens plus the fixed HTMX representation keys."
  (let ((tokens (split-comma-values existing)))
    (dolist (required '("HX-Request" "HX-Request-Type" "HX-Target"))
      (unless (member required tokens :test #'string-equal)
        (setf tokens (append tokens (list required)))))
    (format nil "~{~A~^, ~}" tokens)))

(defun safe-token-p (value)
  "Return true for a non-empty token safe in HTTP headers and CSP nonces."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_." :test #'char=)))
              value)))

(defun generate-request-token (generator kind)
  "Invoke GENERATOR and validate its bounded token result."
  (handler-case
      (let ((value (funcall generator)))
        (unless (safe-token-p value)
          (error 'hypermedia-application-error
                 :reason kind
                 :cause nil))
        (copy-seq value))
    (hypermedia-application-error (condition)
      (error condition))
    (error (cause)
      (error 'hypermedia-application-error
             :reason kind
             :cause cause))))

(defun strict-csp-value (nonce)
  "Return the strict default CSP value containing NONCE."
  (format nil
          "default-src 'self'; script-src 'self' 'nonce-~A'; style-src 'self'; img-src 'self' data:; connect-src 'self'; object-src 'none'; base-uri 'self'; frame-ancestors 'self'"
          nonce))

(defun apply-security-headers-to-normal-response (response env configuration)
  "Return normal Clack RESPONSE with the frozen Hypermedia security headers."
  (destructuring-bind (status headers &rest body-tail) response
    (let* ((headers (copy-list headers))
           (request-id (getf env :clog.request-id))
           (nonce (getf env :clog.csp-nonce))
           (static-p (getf env :clog.static-request-p))
           (headers
             (set-response-header headers
                                  :x-content-type-options
                                  "nosniff"))
           (headers
             (set-response-header headers
                                  :referrer-policy
                                  "strict-origin-when-cross-origin"))
           (headers
             (set-response-header headers
                                  :x-frame-options
                                  "SAMEORIGIN"))
           (headers
             (set-response-header
              headers
              :vary
              (merge-vary-header
               (response-header-value response :vary))))
           (headers
             (if static-p
                 headers
                 (set-response-header headers :cache-control "no-store")))
           (headers
             (if request-id
                 (set-response-header headers :x-request-id request-id)
                 headers))
           (headers
             (if (configuration-strict-csp-p configuration)
                 (set-response-header
                  headers
                  :content-security-policy
                  (strict-csp-value
                   (or nonce
                       (generate-request-token
                        (configuration-csp-nonce-generator configuration)
                        :invalid-csp-nonce))))
                 headers)))
      (cons status (cons headers body-tail)))))

(defun transform-clack-response (response transformer)
  "Apply TRANSFORMER to normal and delayed Clack responses."
  (etypecase response
    (list (funcall transformer response))
    (function
     (lambda (responder)
       (funcall response
                (lambda (normal-response)
                  (funcall responder
                           (funcall transformer normal-response))))))))

(defun make-security-headers-middleware (configuration)
  "Return middleware adding base security, CSP, Vary and request-id headers."
  (lambda (app)
    (lambda (env)
      (handler-case
          (transform-clack-response
           (funcall app env)
           (lambda (response)
             (apply-security-headers-to-normal-response
              response env configuration)))
        (error ()
          (let ((request-id (getf env :clog.request-id)))
            (list 500
                  (append
                   '(:content-type "text/plain; charset=utf-8"
                     :x-content-type-options "nosniff"
                     :referrer-policy "strict-origin-when-cross-origin"
                     :x-frame-options "SAMEORIGIN"
                     :cache-control "no-store")
                   (when request-id
                     (list :x-request-id request-id)))
                  '("Internal Server Error"))))))))

(defun make-request-id-middleware (configuration)
  "Return middleware assigning one validated request id to each Clack ENV."
  (let ((generator (configuration-request-id-generator configuration)))
    (lambda (app)
      (lambda (env)
        (setf (getf env :clog.request-id)
              (generate-request-token generator :invalid-request-id))
        (funcall app env)))))

(defun make-csp-nonce-middleware (configuration)
  "Return middleware assigning a validated nonce when strict CSP is enabled."
  (let ((generator (configuration-csp-nonce-generator configuration)))
    (lambda (app)
      (lambda (env)
        (when (configuration-strict-csp-p configuration)
          (setf (getf env :clog.csp-nonce)
                (generate-request-token generator :invalid-csp-nonce)))
        (funcall app env)))))

(defun status-from-clack-response (response)
  "Return status from a normal response, or NIL for delayed responses."
  (and (listp response) (integerp (first response)) (first response)))

(defun invoke-access-log-hook (hook event)
  "Invoke HOOK with bounded EVENT data without destabilizing the request."
  (when hook
    (handler-case
        (funcall hook event)
      (error () nil))))

(defun make-access-log-middleware (configuration)
  "Return middleware emitting bounded request start and completion events."
  (let ((hook (configuration-access-log-hook configuration)))
    (lambda (app)
      (lambda (env)
        (let* ((request-id (getf env :clog.request-id))
               (method (getf env :request-method))
               (path (getf env :path-info))
               (started (get-internal-real-time)))
          (invoke-access-log-hook
           hook
           (list :event :request-started
                 :request-id request-id
                 :method method
                 :path (and (stringp path) (copy-seq path))))
          (let ((response (funcall app env)))
            (if (functionp response)
                (lambda (responder)
                  (funcall response
                           (lambda (normal-response)
                             (invoke-access-log-hook
                              hook
                              (list :event :request-completed
                                    :request-id request-id
                                    :method method
                                    :path (and (stringp path) (copy-seq path))
                                    :status (status-from-clack-response
                                             normal-response)
                                    :elapsed-internal-time
                                    (- (get-internal-real-time) started)))
                             (funcall responder normal-response))))
                (progn
                  (invoke-access-log-hook
                   hook
                   (list :event :request-completed
                         :request-id request-id
                         :method method
                         :path (and (stringp path) (copy-seq path))
                         :status (status-from-clack-response response)
                         :elapsed-internal-time
                         (- (get-internal-real-time) started)))
                  response))))))))

(defun plain-error-response (status text &optional headers)
  "Return a normalized plain-text error response."
  (clog-http:response->clack-response
   (clog-http:make-response
    :status status
    :headers (append
              '(:content-type "text/plain; charset=utf-8")
              headers)
    :body text
    :kind :html)))

(defun method-token (method)
  "Return a canonical uppercase HTTP token."
  (case method
    (:get "GET")
    (:head "HEAD")
    (:post "POST")
    (:put "PUT")
    (:patch "PATCH")
    (:delete "DELETE")
    (:options "OPTIONS")
    (:trace "TRACE")
    (:connect "CONNECT")
    (otherwise "UNKNOWN")))

(defun allow-header-value (methods)
  "Encode METHODS as a stable Allow header value."
  (format nil "~{~A~^, ~}" (mapcar #'method-token methods)))

(defun condition-status-and-headers (condition)
  "Return status and extra headers for a typed pipeline CONDITION."
  (typecase condition
    (clog-http:request-body-too-large (values 413 nil))
    (clog-router:path-decoding-error (values 400 nil))
    (clog-http:request-error (values 400 nil))
    (clog-render:static-asset-error
     (values (clog-render:static-asset-error-status condition) nil))
    (clog-router:route-not-found (values 404 nil))
    (clog-router:method-not-allowed
     (values 405
             (list :allow
                   (allow-header-value
                    (clog-router:method-not-allowed-allowed-methods
                     condition)))))
    (t (values 500 nil))))

(defun condition-response-text (condition status request-id development-p)
  "Return a redacted response message for CONDITION and STATUS."
  (declare (ignore condition))
  (if development-p
      (format nil "Development request failure. Status ~D.~@[ Request ID: ~A.~]"
              status request-id)
      (case status
        (400 "Bad Request")
        (403 "Forbidden")
        (404 "Not Found")
        (405 "Method Not Allowed")
        (413 "Payload Too Large")
        (otherwise
         (if request-id
             (format nil "Internal Server Error. Request ID: ~A." request-id)
             "Internal Server Error")))))

(defun notify-development-condition (configuration condition request-id)
  "Call the optional development condition hook with redacted request identity."
  (let ((hook (configuration-development-condition-hook configuration)))
    (when hook
      (funcall hook condition request-id))))

(defun make-error-boundary-middleware (configuration)
  "Return middleware normalizing request, route and handler conditions.

Production responses never include a condition report or stack. Development
responses remain bounded and may invoke an explicitly configured condition hook
for debugger integration."
  (lambda (app)
    (lambda (env)
      (handler-case
          (funcall app env)
        (error (condition)
          (let* ((request-id (getf env :clog.request-id))
                 (development-p
                   (configuration-development-p configuration)))
            (when development-p
              (notify-development-condition
               configuration condition request-id))
            (multiple-value-bind (status headers)
                (condition-status-and-headers condition)
              (plain-error-response
               status
               (condition-response-text
                condition status request-id development-p)
               headers))))))))

(defun make-authentication-middleware (configuration)
  "Return middleware injecting the configured user object into ENV."
  (let ((hook (configuration-authentication-hook configuration)))
    (lambda (app)
      (lambda (env)
        (when hook
          (setf (getf env :clog.user) (funcall hook env)))
        (funcall app env)))))

(defun make-router-terminal (router configuration)
  "Return the innermost Clack application that constructs context and dispatches."
  (lambda (env)
    (let ((context
            (clog-http:make-request-context
             env
             :request-id (getf env :clog.request-id)
             :user (getf env :clog.user)
             :csp-nonce (getf env :clog.csp-nonce)
             :body-limit-bytes
             (configuration-request-body-limit-bytes configuration))))
      (clog-router:dispatch-route router context :condition-handler nil))))

(defun compose-middleware (terminal wrappers)
  "Wrap TERMINAL with WRAPPERS listed from outermost to innermost."
  (let ((app terminal))
    (dolist (wrapper (reverse wrappers) app)
      (setf app (funcall wrapper app)))))

(defun build-application-handler (router configuration)
  "Compose the frozen HM-013 middleware pipeline around ROUTER."
  (let ((terminal (make-router-terminal router configuration)))
    (compose-middleware
     terminal
     (list
      (make-request-id-middleware configuration)
      (make-access-log-middleware configuration)
      (make-security-headers-middleware configuration)
      (make-error-boundary-middleware configuration)
      (clog-session:make-request-body-limit-middleware
       (configuration-request-body-limit-bytes configuration))
      (make-csp-nonce-middleware configuration)
      (clog-render:make-static-asset-middleware
       (configuration-static-prefix configuration)
       (configuration-static-root configuration))
      (clog-session:make-session-middleware configuration)
      (clog-session:make-csrf-middleware configuration)
      (make-authentication-middleware configuration)))))

(defun validate-application-name (name)
  "Return a defensively copied application NAME."
  (cond
    ((and (stringp name) (plusp (length name))) (copy-seq name))
    ((symbolp name) name)
    (t
     (error 'hypermedia-application-error
            :reason :invalid-application-name
            :cause nil))))

(defun make-hypermedia-application
    (&key
       (name "clog-hypermedia")
       (router (clog-router:make-router))
       layout
       (configuration (make-hypermedia-configuration))
       component-store
       event-bus)
  "Create a server-independent HYPERMEDIA-APPLICATION.

The resulting APPLICATION-HANDLER is a standard Clack application function.
Server lifecycle remains owned by CLACK:CLACKUP and CLACK:STOP."
  (unless (clog-router:router-p router)
    (error 'hypermedia-application-error
           :reason :invalid-router
           :cause nil))
  (unless (or (null layout) (functionp layout))
    (error 'hypermedia-application-error
           :reason :invalid-layout
           :cause nil))
  (check-type configuration hypermedia-configuration)
  (let ((name (validate-application-name name)))
    (make-instance
     'hypermedia-application
     :name name
     :router router
     :layout layout
     :configuration configuration
     :component-store component-store
     :event-bus event-bus
     :handler (build-application-handler router configuration))))

(defun make-hypermedia-app (&rest arguments)
  "Compatibility constructor forwarding ARGUMENTS to MAKE-HYPERMEDIA-APPLICATION."
  (apply #'make-hypermedia-application arguments))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HM-025 session-scoped component action endpoint                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia)

(defun hm-025-signal-component-not-mounted (component)
  "Signal the existing lifecycle condition without recursively acquiring the lock."
  (error 'clog-component:component-not-mounted
         :reason :component-not-mounted
         :component-id (clog-component:component-id component)
         :operation :handle-action
         :state (clog-component:component-lifecycle-state component)
         :lifecycle-reason :component-not-mounted))

(defmethod clog-component:handle-action :around
    ((component clog-component:component) action request-context)
  "Preserve direct-call locking while allowing the dispatcher to own the lock once."
  (declare (ignore action request-context))
  (if (eq component clog-action::*action-dispatch-lock-owner*)
      (progn
        (unless (clog-component:mounted-p component)
          (hm-025-signal-component-not-mounted component))
        (call-next-method))
      (bordeaux-threads:with-lock-held
          ((clog-component:component-lock component))
        (unless (clog-component:mounted-p component)
          (hm-025-signal-component-not-mounted component))
        (call-next-method))))

(defun hm-025-safe-component-id-p (value)
  "Return true for the frozen opaque CLOG component-id grammar."
  (and (stringp value)
       (= (length value) 39)
       (string= "clog-c-" value :end2 7)
       (loop for index from 7 below 39
             for character = (char value index)
             always (or (char<= #\0 character #\9)
                        (char<= #\a character #\f)))))

(defun hm-025-request-id-text (context)
  "Return CONTEXT's safe request id or a fixed fallback token."
  (or (clog-http:request-id context) "unavailable"))

(defun hm-025-error-fragment (kind text context &key (status 500) headers)
  "Create a redacted HTML error fragment without reports or submitted values."
  (let ((body
          (if (eq kind :internal)
              (format nil
                      "<div data-clog-error=\"internal\">Action failed. Request ID: ~A</div>"
                      (hm-025-request-id-text context))
              (format nil "<div data-clog-error=\"~A\">~A</div>" kind text))))
    (clog-http:html-response body :status status :headers headers)))

(defun hm-025-component-expired-response (context)
  "Return the frozen HTMX-safe expired-component refresh policy."
  (hm-025-error-fragment
   :component-expired "Component expired." context
   :status 200
   :headers '(:hx-refresh "true" :x-clog-reason "component-expired")))

(defun hm-025-action-method-response (descriptor context)
  "Return a safe 405 response when the descriptor does not permit POST."
  (hm-025-error-fragment
   :method-not-allowed "Method Not Allowed" context
   :status 405
   :headers
   (list :allow
         (format nil "~{~A~^, ~}"
                 (mapcar
                  (lambda (method)
                    (case method
                      (:get "GET") (:head "HEAD") (:post "POST")
                      (:put "PUT") (:patch "PATCH") (:delete "DELETE")
                      (:options "OPTIONS") (otherwise "UNKNOWN")))
                  (clog-action:action-descriptor-allowed-methods descriptor))))))

(defun hm-025-parse-revision (context)
  "Return the submitted non-negative decimal revision or signal validation failure."
  (let ((raw (clog-http:form-param context "_clog_revision" nil)))
    (unless (and (stringp raw) (plusp (length raw)) (every #'digit-char-p raw))
      (error 'clog-action::action-validation-error
             :reason :invalid-component-revision))
    (handler-case
        (let ((value (parse-integer raw :junk-allowed nil)))
          (unless (not (minusp value))
            (error 'clog-action::action-validation-error
                   :reason :invalid-component-revision))
          value)
      (clog-action::action-validation-error (condition) (error condition))
      (error ()
        (error 'clog-action::action-validation-error
               :reason :invalid-component-revision)))))

(defun hm-025-decode-action-input (descriptor context)
  "Decode CONTEXT and normalize arbitrary decoder failures to typed validation failure."
  (handler-case
      (funcall (clog-action:action-descriptor-parameter-decoder descriptor)
               context)
    (clog-action::action-validation-error (condition) (error condition))
    (error ()
      (error 'clog-action::action-validation-error
             :reason :parameter-decode-failed))))

(defun hm-025-render-current-component (application component context)
  "Render COMPONENT as the committed fragment for this action response."
  (let ((fragment-state
          (clog-render::make-render-context
           :request context
           :application application
           :mode :fragment
           :primary-component-id (clog-component:component-id component))))
    (clog-render::render component fragment-state)))

(defun hm-025-commit-component-change (component)
  "Commit one revision while the dispatcher already owns COMPONENT's lock."
  (incf (clog-component::%component-revision component))
  (setf (clog-component::%component-dirty-p component) t
        (clog-component::%component-last-access component)
        (get-universal-time))
  (clog-component::%component-revision component))

(defun hm-025-response-from-result (result application component context)
  "Validate RESULT then render the current component fragment."
  (unless (clog-action::valid-hm-025-action-result-p result component)
    (error 'clog-action::invalid-action-result :reason :invalid-action-result))
  (clog-http:html-response
   (hm-025-render-current-component application component context)
   :status (clog-action::%action-result-status result)
   :headers (clog-action::%action-result-response-headers result)))

(defun hm-025-stale-response (application component context)
  "Return the latest fragment without executing a stale action."
  (clog-http:html-response
   (hm-025-render-current-component application component context)
   :status 200
   :headers '(:hx-trigger "clog:stale-component"
              :x-clog-reason "stale-component")))

(defun hm-025-owned-session-component-p (component session-id)
  "Return true when COMPONENT is mounted and owned by exactly SESSION-ID."
  (and (typep component 'clog-component:component)
       (clog-component:mounted-p component)
       (eq :session (clog-component:component-scope component))
       (let ((owner (clog-component:component-owner-session-id component)))
         (and owner (string= owner session-id)))))

(defun hm-025-load-session-component (store session-id component-id)
  "Load COMPONENT-ID only from SESSION-ID, treating malformed and absent IDs alike."
  (when (hm-025-safe-component-id-p component-id)
    (handler-case
        (clog-component:load-component store session-id component-id)
      (clog-component:component-store-error () nil))))

(defun hm-025-dispatch-known-component
    (application component descriptor context decoded-input expected-revision)
  "Execute one authorized decoded action and render its committed representation."
  (bordeaux-threads:with-lock-held ((clog-component:component-lock component))
    (unless (clog-component:mounted-p component)
      (return-from hm-025-dispatch-known-component
        (hm-025-component-expired-response context)))
    (when (and (clog-action:action-descriptor-requires-current-p descriptor)
               (/= expected-revision
                   (clog-component:component-revision component)))
      (return-from hm-025-dispatch-known-component
        (hm-025-stale-response application component context)))
    (clog-component:validate-action
     component (clog-action:action-descriptor-symbol descriptor) context)
    (let* ((clog-action::*action-dispatch-lock-owner* component)
           (result
             (funcall (clog-action:action-descriptor-handler descriptor)
                      component decoded-input)))
      (unless (clog-action::valid-hm-025-action-result-p result component)
        (error 'clog-action::invalid-action-result :reason :invalid-action-result))
      (hm-025-commit-component-change component)
      (hm-025-response-from-result result application component context))))

(defun hm-025-dispatch-component-action (application context)
  "Execute the frozen HM-025 method/session/action/authorization/decode/revision pipeline."
  (let* ((store (application-component-store application))
         (component-id (clog-http:path-param context "component-id"))
         (action-name (clog-http:path-param context "action-name")))
    (unless (and store (clog-component:component-store-p store))
      (error 'hypermedia-application-error
             :reason :action-component-store-required :cause nil))
    (multiple-value-bind (registry session-id)
        (clog-session:ensure-session-component-registry store context)
      (declare (ignore registry))
      (let ((component
              (hm-025-load-session-component store session-id component-id)))
        (unless (and component
                     (hm-025-owned-session-component-p component session-id))
          (return-from hm-025-dispatch-component-action
            (hm-025-component-expired-response context)))
        (let ((descriptor (clog-action:find-action component action-name)))
          (unless descriptor
            (return-from hm-025-dispatch-component-action
              (hm-025-error-fragment
               :action-not-found "Not Found" context :status 404)))
          (unless (clog-action:action-method-allowed-p descriptor :post)
            (return-from hm-025-dispatch-component-action
              (hm-025-action-method-response descriptor context)))
          (unless
              (funcall (clog-action:action-descriptor-authorize-function descriptor)
                       component context)
            (return-from hm-025-dispatch-component-action
              (hm-025-error-fragment :forbidden "Forbidden" context :status 403)))
          (let* ((decoded-input (hm-025-decode-action-input descriptor context))
                 (expected-revision
                   (when (clog-action:action-descriptor-requires-current-p descriptor)
                     (hm-025-parse-revision context))))
            (hm-025-dispatch-known-component
             application component descriptor context
             decoded-input expected-revision)))))))

(defun hm-025-action-route-handler (application context)
  "Run action dispatch with production-safe condition normalization."
  (let ((development-p
          (configuration-development-p (application-configuration application))))
    (handler-case
        (hm-025-dispatch-component-action application context)
      (clog-action::action-validation-error (condition)
        (declare (ignore condition))
        (hm-025-error-fragment
         :validation "Action validation failed." context :status 422))
      (error (condition)
        (if development-p
            (error condition)
            (hm-025-error-fragment :internal "Action failed." context :status 500))))))

(defun hm-025-action-route-path (configuration)
  "Return the configured internal action route template."
  (let* ((prefix (configuration-action-prefix configuration))
         (normalized
           (if (and (> (length prefix) 1)
                    (char= #\/ (char prefix (1- (length prefix)))))
               (string-right-trim "/" prefix)
               prefix)))
    (concatenate 'string normalized "/:component-id/:action-name")))

(defun hm-025-install-action-route (application)
  "Register the POST-only internal component action route."
  (add-route
   (application-router application)
   :post
   (hm-025-action-route-path (application-configuration application))
   (lambda (context) (hm-025-action-route-handler application context))
   :name :clog-component-action
   :metadata '(:internal t :mutation t :csrf-required t)))

(defun make-hypermedia-application
    (&key (name "clog-hypermedia")
          (router (clog-router:make-router))
          layout
          (configuration (make-hypermedia-configuration))
          component-store
          event-bus)
  "Create an application and install HM-025 action dispatch when a component store is supplied."
  (unless (clog-router:router-p router)
    (error 'hypermedia-application-error :reason :invalid-router :cause nil))
  (unless (or (null layout) (functionp layout))
    (error 'hypermedia-application-error :reason :invalid-layout :cause nil))
  (check-type configuration hypermedia-configuration)
  (when component-store
    (unless (clog-component:component-store-p component-store)
      (error 'hypermedia-application-error
             :reason :invalid-component-store :cause nil)))
  (let* ((name (validate-application-name name))
         (application
           (make-instance
            'hypermedia-application
            :name name :router router :layout layout
            :configuration configuration
            :component-store component-store
            :event-bus event-bus :handler nil)))
    (when component-store
      (hm-025-install-action-route application))
    (setf (slot-value application 'handler)
          (build-application-handler router configuration))
    application))
