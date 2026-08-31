;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime configuration                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-hypermedia
  (:export #:hypermedia-configuration
           #:hypermedia-configuration-error
           #:hypermedia-configuration-error-reason
           #:make-hypermedia-configuration
           #:configuration-development-p
           #:configuration-htmx-version
           #:configuration-assets-mode
           #:configuration-default-swap
           #:configuration-action-prefix
           #:configuration-sse-prefix
           #:configuration-ws-prefix
           #:configuration-component-ttl-seconds
           #:configuration-max-components-per-session
           #:configuration-max-partials-per-response
           #:configuration-max-effect-header-bytes
           #:configuration-strict-csp-p
           #:configuration-request-body-limit-bytes
           #:configuration-static-prefix
           #:configuration-static-root
           #:configuration-session-store
           #:configuration-session-state
           #:configuration-session-keep-empty-p
           #:configuration-csrf-session-key
           #:configuration-csrf-form-token
           #:configuration-csrf-one-time-p
           #:configuration-authentication-hook
           #:configuration-access-log-hook
           #:configuration-development-condition-hook
           #:configuration-request-id-generator
           #:configuration-csp-nonce-generator))

(in-package #:clog-hypermedia)

(define-condition hypermedia-configuration-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :reader hypermedia-configuration-error-reason)
   (value
    :initarg :value
    :initform nil
    :reader %hypermedia-configuration-error-value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid Hypermedia configuration (~A)."
             (hypermedia-configuration-error-reason condition))))
  (:documentation
   "Signaled when a Hypermedia application configuration is invalid.

The rejected value is retained for controlled development diagnostics, but the
condition report deliberately omits it because hooks and paths may be sensitive."))

(defun default-security-token ()
  "Return a cryptographically random hexadecimal token supplied by Lack."
  (lack.util:generate-random-id))

(defun default-static-root ()
  "Return the canonical CLOG static-files directory."
  (truename
   (uiop:ensure-directory-pathname
    (asdf:system-relative-pathname :clog "static-files/"))))

(defun configuration-boolean-p (value)
  "Return true when VALUE is a Common Lisp boolean."
  (or (null value) (eq value t)))

(defun configuration-control-character-p (character)
  "Return true for ASCII control characters and DEL."
  (let ((code (char-code character)))
    (or (= code 127) (< code 32))))

(defun safe-configuration-string-p (value &key allow-empty)
  "Return true for a bounded configuration string without control characters."
  (and (stringp value)
       (or allow-empty (plusp (length value)))
       (notany #'configuration-control-character-p value)))

(defun safe-path-prefix-p (value &key trailing-slash-p)
  "Return true when VALUE is a same-origin absolute path prefix."
  (and (safe-configuration-string-p value)
       (char= (char value 0) #\/)
       (or (= (length value) 1)
           (char/= (char value 1) #\/))
       (not (position #\\ value))
       (not (position #\? value))
       (not (position #\# value))
       (or (not trailing-slash-p)
           (char= (char value (1- (length value))) #\/))))

(defun require-configuration (test reason value)
  "Signal HYPERMEDIA-CONFIGURATION-ERROR unless TEST is true."
  (unless test
    (error 'hypermedia-configuration-error :reason reason :value value))
  value)

(defun normalize-static-root (root)
  "Return a canonical directory pathname for ROOT, or NIL when disabled."
  (when root
    (handler-case
        (let ((pathname
                (truename
                 (uiop:ensure-directory-pathname (pathname root)))))
          (require-configuration
           (uiop:directory-exists-p pathname)
           :static-root-is-not-a-directory
           root)
          pathname)
      (hypermedia-configuration-error (condition)
        (error condition))
      (error ()
        (error 'hypermedia-configuration-error
               :reason :invalid-static-root
               :value root)))))

(defclass hypermedia-configuration ()
  ((development-p
    :initarg :development-p
    :reader %configuration-development-p)
   (htmx-version
    :initarg :htmx-version
    :reader %configuration-htmx-version)
   (assets-mode
    :initarg :assets-mode
    :reader %configuration-assets-mode)
   (default-swap
    :initarg :default-swap
    :reader %configuration-default-swap)
   (action-prefix
    :initarg :action-prefix
    :reader %configuration-action-prefix)
   (sse-prefix
    :initarg :sse-prefix
    :reader %configuration-sse-prefix)
   (ws-prefix
    :initarg :ws-prefix
    :reader %configuration-ws-prefix)
   (component-ttl-seconds
    :initarg :component-ttl-seconds
    :reader %configuration-component-ttl-seconds)
   (max-components-per-session
    :initarg :max-components-per-session
    :reader %configuration-max-components-per-session)
   (max-partials-per-response
    :initarg :max-partials-per-response
    :reader %configuration-max-partials-per-response)
   (max-effect-header-bytes
    :initarg :max-effect-header-bytes
    :reader %configuration-max-effect-header-bytes)
   (strict-csp-p
    :initarg :strict-csp-p
    :reader %configuration-strict-csp-p)
   (request-body-limit-bytes
    :initarg :request-body-limit-bytes
    :reader %configuration-request-body-limit-bytes)
   (static-prefix
    :initarg :static-prefix
    :reader %configuration-static-prefix)
   (static-root
    :initarg :static-root
    :reader %configuration-static-root)
   (session-store
    :initarg :session-store
    :reader %configuration-session-store)
   (session-state
    :initarg :session-state
    :reader %configuration-session-state)
   (session-keep-empty-p
    :initarg :session-keep-empty-p
    :reader %configuration-session-keep-empty-p)
   (csrf-session-key
    :initarg :csrf-session-key
    :reader %configuration-csrf-session-key)
   (csrf-form-token
    :initarg :csrf-form-token
    :reader %configuration-csrf-form-token)
   (csrf-one-time-p
    :initarg :csrf-one-time-p
    :reader %configuration-csrf-one-time-p)
   (authentication-hook
    :initarg :authentication-hook
    :reader %configuration-authentication-hook)
   (access-log-hook
    :initarg :access-log-hook
    :reader %configuration-access-log-hook)
   (development-condition-hook
    :initarg :development-condition-hook
    :reader %configuration-development-condition-hook)
   (request-id-generator
    :initarg :request-id-generator
    :reader %configuration-request-id-generator)
   (csp-nonce-generator
    :initarg :csp-nonce-generator
    :reader %configuration-csp-nonce-generator))
  (:documentation
   "Immutable configuration for one Hypermedia application pipeline."))

(defun make-hypermedia-configuration
    (&key
       (development-p nil)
       (htmx-version "4.0.0")
       (assets-mode :vendored)
       (default-swap "outerMorph")
       (action-prefix "/_clog/action")
       (sse-prefix "/_clog/sse")
       (ws-prefix "/_clog/ws")
       (component-ttl-seconds 1800)
       (max-components-per-session 2048)
       (max-partials-per-response 32)
       (max-effect-header-bytes 4096)
       (strict-csp-p t)
       (request-body-limit-bytes 1048576)
       (static-prefix "/_clog/static/")
       (static-root (default-static-root))
       session-store
       session-state
       (session-keep-empty-p t)
       (csrf-session-key "_csrf_token")
       (csrf-form-token "_csrf_token")
       (csrf-one-time-p nil)
       authentication-hook
       access-log-hook
       development-condition-hook
       (request-id-generator #'default-security-token)
       (csp-nonce-generator #'default-security-token))
  "Create and validate a HYPERMEDIA-CONFIGURATION.

STATIC-PREFIX and STATIC-ROOT may both be NIL to disable static serving. Hooks
are optional functions. The authentication hook accepts a Clack environment
and returns the user object stored in the request context. The access-log hook
accepts a bounded event plist. The development condition hook accepts a
condition and request id. Token generators are zero-argument functions and are
validated again at request time."
  (require-configuration (configuration-boolean-p development-p)
                         :invalid-development-flag development-p)
  (require-configuration (safe-configuration-string-p htmx-version)
                         :invalid-htmx-version htmx-version)
  (require-configuration (member assets-mode '(:vendored :none) :test #'eq)
                         :invalid-assets-mode assets-mode)
  (require-configuration (safe-configuration-string-p default-swap)
                         :invalid-default-swap default-swap)
  (dolist (entry `((,action-prefix . :invalid-action-prefix)
                   (,sse-prefix . :invalid-sse-prefix)
                   (,ws-prefix . :invalid-ws-prefix)))
    (require-configuration (safe-path-prefix-p (car entry))
                           (cdr entry)
                           (car entry)))
  (dolist (entry `((,component-ttl-seconds . :invalid-component-ttl)
                   (,max-components-per-session . :invalid-component-limit)
                   (,max-partials-per-response . :invalid-partial-limit)
                   (,max-effect-header-bytes . :invalid-effect-header-limit)
                   (,request-body-limit-bytes . :invalid-request-body-limit)))
    (require-configuration (and (integerp (car entry))
                                (plusp (car entry)))
                           (cdr entry)
                           (car entry)))
  (require-configuration (configuration-boolean-p strict-csp-p)
                         :invalid-strict-csp-flag strict-csp-p)
  (require-configuration (configuration-boolean-p session-keep-empty-p)
                         :invalid-session-keep-empty-flag session-keep-empty-p)
  (require-configuration (configuration-boolean-p csrf-one-time-p)
                         :invalid-csrf-one-time-flag csrf-one-time-p)
  (dolist (entry `((,csrf-session-key . :invalid-csrf-session-key)
                   (,csrf-form-token . :invalid-csrf-form-token)))
    (require-configuration (safe-configuration-string-p (car entry))
                           (cdr entry)
                           (car entry)))
  (dolist (entry `((,authentication-hook . :invalid-authentication-hook)
                   (,access-log-hook . :invalid-access-log-hook)
                   (,development-condition-hook . :invalid-development-condition-hook)))
    (require-configuration (or (null (car entry))
                               (functionp (car entry)))
                           (cdr entry)
                           (car entry)))
  (dolist (entry `((,request-id-generator . :invalid-request-id-generator)
                   (,csp-nonce-generator . :invalid-csp-nonce-generator)))
    (require-configuration (functionp (car entry))
                           (cdr entry)
                           (car entry)))
  (require-configuration
   (or (and (null static-prefix) (null static-root))
       (and static-prefix static-root
            (safe-path-prefix-p static-prefix :trailing-slash-p t)))
   :invalid-static-asset-mount
   static-prefix)
  (let ((static-root (normalize-static-root static-root)))
    (make-instance
     'hypermedia-configuration
     :development-p development-p
     :htmx-version (copy-seq htmx-version)
     :assets-mode assets-mode
     :default-swap (copy-seq default-swap)
     :action-prefix (copy-seq action-prefix)
     :sse-prefix (copy-seq sse-prefix)
     :ws-prefix (copy-seq ws-prefix)
     :component-ttl-seconds component-ttl-seconds
     :max-components-per-session max-components-per-session
     :max-partials-per-response max-partials-per-response
     :max-effect-header-bytes max-effect-header-bytes
     :strict-csp-p strict-csp-p
     :request-body-limit-bytes request-body-limit-bytes
     :static-prefix (and static-prefix (copy-seq static-prefix))
     :static-root static-root
     :session-store session-store
     :session-state session-state
     :session-keep-empty-p session-keep-empty-p
     :csrf-session-key (copy-seq csrf-session-key)
     :csrf-form-token (copy-seq csrf-form-token)
     :csrf-one-time-p csrf-one-time-p
     :authentication-hook authentication-hook
     :access-log-hook access-log-hook
     :development-condition-hook development-condition-hook
     :request-id-generator request-id-generator
     :csp-nonce-generator csp-nonce-generator)))

(defun ensure-configuration (configuration)
  "Return CONFIGURATION after checking its public type."
  (check-type configuration hypermedia-configuration)
  configuration)

(defun configuration-development-p (configuration)
  "Return true when development diagnostics are enabled."
  (%configuration-development-p (ensure-configuration configuration)))

(defun configuration-htmx-version (configuration)
  "Return a defensive copy of the configured HTMX version."
  (copy-seq (%configuration-htmx-version (ensure-configuration configuration))))

(defun configuration-assets-mode (configuration)
  "Return the configured asset mode keyword."
  (%configuration-assets-mode (ensure-configuration configuration)))

(defun configuration-default-swap (configuration)
  "Return a defensive copy of the default HTMX swap name."
  (copy-seq (%configuration-default-swap (ensure-configuration configuration))))

(defun configuration-action-prefix (configuration)
  "Return a defensive copy of the internal action path prefix."
  (copy-seq (%configuration-action-prefix (ensure-configuration configuration))))

(defun configuration-sse-prefix (configuration)
  "Return a defensive copy of the internal SSE path prefix."
  (copy-seq (%configuration-sse-prefix (ensure-configuration configuration))))

(defun configuration-ws-prefix (configuration)
  "Return a defensive copy of the internal WebSocket path prefix."
  (copy-seq (%configuration-ws-prefix (ensure-configuration configuration))))

(defun configuration-component-ttl-seconds (configuration)
  "Return the component inactivity lifetime in seconds."
  (%configuration-component-ttl-seconds (ensure-configuration configuration)))

(defun configuration-max-components-per-session (configuration)
  "Return the maximum number of components allowed in one session."
  (%configuration-max-components-per-session (ensure-configuration configuration)))

(defun configuration-max-partials-per-response (configuration)
  "Return the maximum number of partials allowed in one response."
  (%configuration-max-partials-per-response (ensure-configuration configuration)))

(defun configuration-max-effect-header-bytes (configuration)
  "Return the maximum encoded effect-header size in bytes."
  (%configuration-max-effect-header-bytes (ensure-configuration configuration)))

(defun configuration-strict-csp-p (configuration)
  "Return true when strict Content Security Policy headers are enabled."
  (%configuration-strict-csp-p (ensure-configuration configuration)))

(defun configuration-request-body-limit-bytes (configuration)
  "Return the request body ceiling in bytes."
  (%configuration-request-body-limit-bytes (ensure-configuration configuration)))

(defun configuration-static-prefix (configuration)
  "Return a defensive copy of the static URL prefix, or NIL when disabled."
  (let ((value (%configuration-static-prefix
                (ensure-configuration configuration))))
    (and value (copy-seq value))))

(defun configuration-static-root (configuration)
  "Return the canonical static root pathname, or NIL when disabled."
  (%configuration-static-root (ensure-configuration configuration)))

(defun configuration-session-store (configuration)
  "Return the optional Lack session store object."
  (%configuration-session-store (ensure-configuration configuration)))

(defun configuration-session-state (configuration)
  "Return the optional Lack session state object."
  (%configuration-session-state (ensure-configuration configuration)))

(defun configuration-session-keep-empty-p (configuration)
  "Return the Lack session keep-empty policy."
  (%configuration-session-keep-empty-p (ensure-configuration configuration)))

(defun configuration-csrf-session-key (configuration)
  "Return a defensive copy of the CSRF session key."
  (copy-seq (%configuration-csrf-session-key
             (ensure-configuration configuration))))

(defun configuration-csrf-form-token (configuration)
  "Return a defensive copy of the CSRF form field name."
  (copy-seq (%configuration-csrf-form-token
             (ensure-configuration configuration))))

(defun configuration-csrf-one-time-p (configuration)
  "Return the Lack CSRF one-time-token policy."
  (%configuration-csrf-one-time-p (ensure-configuration configuration)))

(defun configuration-authentication-hook (configuration)
  "Return the optional authentication hook function."
  (%configuration-authentication-hook (ensure-configuration configuration)))

(defun configuration-access-log-hook (configuration)
  "Return the optional bounded access-log hook function."
  (%configuration-access-log-hook (ensure-configuration configuration)))

(defun configuration-development-condition-hook (configuration)
  "Return the optional development condition hook function."
  (%configuration-development-condition-hook
   (ensure-configuration configuration)))

(defun configuration-request-id-generator (configuration)
  "Return the configured zero-argument request-id generator."
  (%configuration-request-id-generator (ensure-configuration configuration)))

(defun configuration-csp-nonce-generator (configuration)
  "Return the configured zero-argument CSP nonce generator."
  (%configuration-csp-nonce-generator (ensure-configuration configuration)))
