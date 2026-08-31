;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime Lack session and CSRF adapters                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-session
  (:export #:make-request-body-limit-middleware
           #:make-session-middleware
           #:make-csrf-middleware
           #:csrf-token-for))

(defpackage #:clog-hypermedia
  (:import-from #:clog-session
                #:csrf-token-for)
  (:export #:csrf-token-for))

(in-package #:clog-session)

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
