;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime Lack session and CSRF adapters                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-session
  (:import-from #:clog-component
                #:component-store-p
                #:component-store-error
                #:ensure-component-registry
                #:delete-session-components)
  (:export #:make-request-body-limit-middleware
           #:make-session-middleware
           #:make-csrf-middleware
           #:csrf-token-for
           #:component-store-session-key
           #:ensure-session-component-registry
           #:rotate-session-component-registry))

(defpackage #:clog-hypermedia
  (:import-from #:clog-session
                #:csrf-token-for
                #:component-store-session-key
                #:ensure-session-component-registry
                #:rotate-session-component-registry)
  (:export #:csrf-token-for
           #:component-store-session-key
           #:ensure-session-component-registry
           #:rotate-session-component-registry))

(in-package #:clog-session)

(defparameter +component-store-session-key+ "_clog_component_store"
  "Serializable Lack session metadata key indicating component-store participation.")

(defparameter +component-store-session-marker+ "v1"
  "Versioned lightweight marker stored in Lack session data, never a CLOS object.")

(defun component-store-session-key ()
  "Return a fresh copy of the Lack session metadata key used by HM-021."
  (copy-seq +component-store-session-key+))

(defun env-header (env name)
  "Return a lower-case Clack header from ENV, or NIL."
  (let ((headers (getf env :headers)))
    (and (hash-table-p headers)
         (gethash (string-downcase name) headers))))

(defun chunked-request-p (env)
  "Return true when ENV uses chunked request transfer encoding."
  (let ((value (env-header env "transfer-encoding")))
    (and (stringp value)
         (string-equal
          "chunked"
          (string-trim '(#\Space #\Tab #\Return #\Newline) value)))))

(defun request-body-present-p (env)
  "Return true when ENV declares a non-empty or chunked request body."
  (let ((length (getf env :content-length)))
    (or (and (integerp length) (plusp length))
        (chunked-request-p env))))

(defun validate-request-body-envelope (env limit)
  "Validate request framing before Lack CSRF can consume the body stream."
  (let ((length (getf env :content-length)))
    (unless (or (null length)
                (and (integerp length) (not (minusp length))))
      (error 'clog-http:request-error :reason :invalid-content-length))
    (when (and length (> length limit))
      (error 'clog-http:request-body-too-large
             :reason :declared-length-exceeded
             :limit limit
             :length length))))

(defun make-request-body-limit-middleware (limit)
  "Return middleware that enforces LIMIT before session and CSRF processing.

The middleware validates Content-Length and replaces a present raw body stream
with HM-010's bounded binary stream. Lack remains responsible for parsing form
content. When Lack CSRF parses a mutation request it appends BODY-PARAMETERS to
the same Clack environment, allowing the later request context to reuse the
parsed values without consuming the body twice."
  (unless (and (integerp limit) (plusp limit))
    (error 'clog-http:request-error :reason :invalid-body-limit))
  (lambda (app)
    (check-type app function)
    (lambda (env)
      (validate-request-body-envelope env limit)
      (when (and (request-body-present-p env)
                 (streamp (getf env :raw-body)))
        (setf (getf env :raw-body)
              (make-instance 'clog-http::bounded-request-body-stream
                             :source (getf env :raw-body)
                             :limit limit)))
      (funcall app env))))

(defun make-session-middleware (configuration)
  "Return the configured Lack session middleware wrapper."
  (check-type configuration clog-hypermedia:hypermedia-configuration)
  (let ((store (clog-hypermedia:configuration-session-store configuration))
        (state (clog-hypermedia:configuration-session-state configuration))
        (keep-empty
          (clog-hypermedia:configuration-session-keep-empty-p configuration)))
    (lambda (app)
      (let ((arguments (list :keep-empty keep-empty)))
        (when store
          (setf arguments (append arguments (list :store store))))
        (when state
          (setf arguments (append arguments (list :state state))))
        (apply lack.middleware.session:*lack-middleware-session*
               app
               arguments)))))

(defun session-identifier-character-safe-p (character)
  "Return true for a non-control character in an opaque Lack session ID."
  (let ((code (char-code character)))
    (and (>= code 32) (/= code 127))))

(defun valid-session-identifier-p (value)
  "Return true for a bounded opaque Lack session identifier."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 4096)
       (every #'session-identifier-character-safe-p value)))

(defun request-component-session-values (context)
  "Return CONTEXT's mutable Lack session hash and defensive session ID."
  (check-type context clog-http:request-context)
  (let ((session (clog-http:request-session context))
        (session-id (clog-http:request-session-id context)))
    (unless (hash-table-p session)
      (error 'component-store-error :reason :missing-lack-session))
    (unless (valid-session-identifier-p session-id)
      (error 'component-store-error :reason :missing-or-invalid-session-id))
    (values session (copy-seq session-id))))

(defun ensure-session-component-registry (store context)
  "Ensure STORE has a registry for CONTEXT's current Lack session.

Only the version string `v1` is written to the Lack session hash. Component
instances, locks and registry objects remain in STORE. The Lack session ID from
`:lack.session.options` is the sole component namespace used for lookup.

Returns the registry and a defensive copy of the current session ID."
  (unless (component-store-p store)
    (error 'component-store-error :reason :invalid-component-store))
  (multiple-value-bind (session session-id)
      (request-component-session-values context)
    (setf (gethash +component-store-session-key+ session)
          (copy-seq +component-store-session-marker+))
    (values (ensure-component-registry store session-id)
            session-id)))

(defun rotate-session-component-registry (store old-session-id context)
  "Bind CONTEXT's new Lack session and explicitly retire OLD-SESSION-ID.

Session rotation is intentionally explicit rather than inferred from
browser-controlled request parameters. The new registry is ensured first, then
the old namespace is deleted and all retained components are unmounted outside
store locks. When the IDs are equal, no deletion occurs.

Returns the new registry, new session ID and number of retired components."
  (unless (valid-session-identifier-p old-session-id)
    (error 'component-store-error :reason :invalid-old-session-id))
  (multiple-value-bind (registry new-session-id)
      (ensure-session-component-registry store context)
    (let ((removed
            (if (string= old-session-id new-session-id)
                nil
                (delete-session-components store old-session-id))))
      (values registry new-session-id (length removed)))))

(defun csrf-rejection-response (env)
  "Return a stable Clack response for a rejected CSRF request."
  (declare (ignore env))
  (clog-http:response->clack-response
   (clog-http:make-response
    :status 403
    :headers '(:content-type "text/plain; charset=utf-8")
    :body "Forbidden"
    :kind :html)))

(defun make-csrf-middleware (configuration)
  "Return the configured Lack CSRF middleware wrapper.

Token extraction, comparison and one-time semantics remain entirely owned by
Lack. This adapter supplies bounded names and a stable rejection response."
  (check-type configuration clog-hypermedia:hypermedia-configuration)
  (let ((session-key
          (clog-hypermedia:configuration-csrf-session-key configuration))
        (form-token
          (clog-hypermedia:configuration-csrf-form-token configuration))
        (one-time
          (clog-hypermedia:configuration-csrf-one-time-p configuration)))
    (lambda (app)
      (funcall lack.middleware.csrf:*lack-middleware-csrf*
               app
               :block-app #'csrf-rejection-response
               :one-time one-time
               :session-key session-key
               :form-token form-token))))

(defun csrf-token-for (context)
  "Return or create the Lack CSRF token for request CONTEXT.

This function is intended for route and render code executing inside the
configured Lack CSRF middleware dynamic scope."
  (check-type context clog-http:request-context)
  (let ((session (clog-http:request-session context)))
    (unless (hash-table-p session)
      (error 'clog-http:request-error :reason :missing-session))
    (lack.middleware.csrf:csrf-token session)))
