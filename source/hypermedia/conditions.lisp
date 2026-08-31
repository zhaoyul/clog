;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HTTP conditions                              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-http
  (:export #:clog-hypermedia-error
           #:request-error
           #:request-error-reason
           #:request-body-too-large
           #:request-body-too-large-limit
           #:request-body-too-large-length
           #:request-body-parse-error
           #:request-body-parse-content-type
           #:request-body-parse-cause
           #:response-error
           #:response-error-reason
           #:invalid-response-status
           #:invalid-response-status-value
           #:invalid-response-kind
           #:invalid-response-kind-value
           #:invalid-response-header
           #:invalid-response-header-name
           #:invalid-response-header-value
           #:invalid-response-body
           #:invalid-response-body-value
           #:invalid-redirect-url
           #:invalid-redirect-url-value
           #:invalid-redirect-url-reason
           #:response-content-length-mismatch
           #:response-content-length-expected
           #:response-content-length-actual))

(defpackage #:clog-hypermedia
  (:import-from #:clog-http
                #:clog-hypermedia-error
                #:request-error
                #:request-error-reason
                #:request-body-too-large
                #:request-body-too-large-limit
                #:request-body-too-large-length
                #:request-body-parse-error
                #:request-body-parse-content-type
                #:request-body-parse-cause
                #:response-error
                #:response-error-reason
                #:invalid-response-status
                #:invalid-response-status-value
                #:invalid-response-kind
                #:invalid-response-kind-value
                #:invalid-response-header
                #:invalid-response-header-name
                #:invalid-response-header-value
                #:invalid-response-body
                #:invalid-response-body-value
                #:invalid-redirect-url
                #:invalid-redirect-url-value
                #:invalid-redirect-url-reason
                #:response-content-length-mismatch
                #:response-content-length-expected
                #:response-content-length-actual)
  (:export #:clog-hypermedia-error
           #:request-error
           #:request-error-reason
           #:request-body-too-large
           #:request-body-too-large-limit
           #:request-body-too-large-length
           #:request-body-parse-error
           #:request-body-parse-content-type
           #:request-body-parse-cause
           #:response-error
           #:response-error-reason
           #:invalid-response-status
           #:invalid-response-status-value
           #:invalid-response-kind
           #:invalid-response-kind-value
           #:invalid-response-header
           #:invalid-response-header-name
           #:invalid-response-header-value
           #:invalid-response-body
           #:invalid-response-body-value
           #:invalid-redirect-url
           #:invalid-redirect-url-value
           #:invalid-redirect-url-reason
           #:response-content-length-mismatch
           #:response-content-length-expected
           #:response-content-length-actual))

(in-package #:clog-http)

(define-condition clog-hypermedia-error (error)
  ()
  (:documentation
   "Base condition for errors raised by the CLOG 3 Hypermedia Runtime."))

(define-condition request-error (clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader request-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid Hypermedia request~@[ (~A)~]."
             (request-error-reason condition))))
  (:documentation
   "Base condition for request normalization and request-body failures."))

(define-condition request-body-too-large (request-error)
  ((limit
    :initarg :limit
    :reader request-body-too-large-limit)
   (length
    :initarg :length
    :initform nil
    :reader request-body-too-large-length))
  (:report
   (lambda (condition stream)
     (format stream
             "Request body exceeds the configured ~D-byte limit~@[ (declared length ~D)~]."
             (request-body-too-large-limit condition)
             (request-body-too-large-length condition))))
  (:documentation
   "Signaled before or during body parsing when the configured byte limit is exceeded."))

(define-condition request-body-parse-error (request-error)
  ((content-type
    :initarg :content-type
    :initform nil
    :reader request-body-parse-content-type)
   (cause
    :initarg :cause
    :initform nil
    :reader request-body-parse-cause))
  (:report
   (lambda (condition stream)
     (format stream
             "Unable to parse request body~@[ with Content-Type ~S~]."
             (request-body-parse-content-type condition))))
  (:documentation
   "Typed wrapper for malformed or otherwise unparseable form request bodies.

The original parser condition is retained in REQUEST-BODY-PARSE-CAUSE for
debugging, while the report deliberately avoids printing the request body."))

(define-condition response-error (clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader response-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid Hypermedia response~@[ (~A)~]."
             (response-error-reason condition))))
  (:documentation
   "Base condition for invalid response objects and unsafe Clack encoding."))

(define-condition invalid-response-status (response-error)
  ((value
    :initarg :value
    :reader invalid-response-status-value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid HTTP response status ~S."
             (invalid-response-status-value condition))))
  (:documentation
   "Signaled when a response status is not an integer in the HTTP 100-599 range."))

(define-condition invalid-response-kind (response-error)
  ((value
    :initarg :value
    :reader invalid-response-kind-value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid Hypermedia response kind ~S."
             (invalid-response-kind-value condition))))
  (:documentation
   "Signaled when a response kind is outside the frozen response-kind vocabulary."))

(define-condition invalid-response-header (response-error)
  ((name
    :initarg :name
    :initform nil
    :reader invalid-response-header-name)
   (value
    :initarg :value
    :initform nil
    :reader invalid-response-header-value))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Invalid or unsafe HTTP response header.")))
  (:documentation
   "Signaled for malformed headers, unsupported header value types or control-character injection.

The report intentionally does not print the header value, because response
headers can carry cookies, tokens or other sensitive material."))

(define-condition invalid-response-body (response-error)
  ((value
    :initarg :value
    :reader invalid-response-body-value))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Response body cannot be represented by the selected response kind.")))
  (:documentation
   "Signaled when a response body cannot be represented safely by the selected Clack response kind."))

(define-condition invalid-redirect-url (response-error)
  ((value
    :initarg :value
    :reader invalid-redirect-url-value)
   (reason
    :initarg :reason
    :initform :not-allowed
    :reader invalid-redirect-url-reason))
  (:report
   (lambda (condition stream)
     (format stream "Redirect URL rejected (~A)."
             (invalid-redirect-url-reason condition))))
  (:documentation
   "Signaled when a redirect target is not a safe relative URL or an explicitly allowlisted origin."))

(define-condition response-content-length-mismatch (response-error)
  ((expected
    :initarg :expected
    :reader response-content-length-expected)
   (actual
    :initarg :actual
    :reader response-content-length-actual))
  (:report
   (lambda (condition stream)
     (format stream "Response Content-Length ~D does not match encoded body length ~D."
             (response-content-length-actual condition)
             (response-content-length-expected condition))))
  (:documentation
   "Signaled when an explicit Content-Length disagrees with a deterministically sized body."))

(setf (documentation 'request-error-reason 'function)
      "Return the bounded reason keyword carried by a REQUEST-ERROR.")
(setf (documentation 'request-body-too-large-limit 'function)
      "Return the configured byte limit from REQUEST-BODY-TOO-LARGE.")
(setf (documentation 'request-body-too-large-length 'function)
      "Return the declared body length from REQUEST-BODY-TOO-LARGE, or NIL when unknown.")
(setf (documentation 'request-body-parse-content-type 'function)
      "Return the Content-Type associated with REQUEST-BODY-PARSE-ERROR, or NIL.")
(setf (documentation 'request-body-parse-cause 'function)
      "Return the underlying parser condition retained by REQUEST-BODY-PARSE-ERROR, or NIL.")
(setf (documentation 'response-error-reason 'function)
      "Return the bounded reason keyword carried by a RESPONSE-ERROR.")
(setf (documentation 'invalid-response-status-value 'function)
      "Return the rejected HTTP status value.")
(setf (documentation 'invalid-response-kind-value 'function)
      "Return the rejected response kind value.")
(setf (documentation 'invalid-response-header-name 'function)
      "Return the rejected response header name, or NIL when the header list itself is malformed.")
(setf (documentation 'invalid-response-header-value 'function)
      "Return the rejected response header value; do not log it without an explicit redaction policy.")
(setf (documentation 'invalid-response-body-value 'function)
      "Return the rejected response body value; callers must not log it by default.")
(setf (documentation 'invalid-redirect-url-value 'function)
      "Return the rejected redirect target string.")
(setf (documentation 'invalid-redirect-url-reason 'function)
      "Return the bounded reason keyword describing why a redirect target was rejected.")
(setf (documentation 'response-content-length-expected 'function)
      "Return the byte length computed from the response body.")
(setf (documentation 'response-content-length-actual 'function)
      "Return the conflicting Content-Length supplied by response headers.")

(defpackage #:clog-router
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:export #:routing-error
           #:routing-error-reason
           #:route-definition-error
           #:route-definition-value
           #:route-conflict
           #:route-conflict-method
           #:route-conflict-template
           #:route-conflict-existing-template
           #:route-not-found
           #:route-not-found-method
           #:route-not-found-path
           #:method-not-allowed
           #:method-not-allowed-method
           #:method-not-allowed-path
           #:method-not-allowed-allowed-methods
           #:path-decoding-error
           #:path-decoding-error-path
           #:path-decoding-error-parameter
           #:path-decoding-error-cause
           #:route-handler-error
           #:route-handler-error-route
           #:route-handler-error-cause))

(defpackage #:clog-hypermedia
  (:import-from #:clog-router
                #:routing-error
                #:routing-error-reason
                #:route-definition-error
                #:route-definition-value
                #:route-conflict
                #:route-conflict-method
                #:route-conflict-template
                #:route-conflict-existing-template
                #:route-not-found
                #:route-not-found-method
                #:route-not-found-path
                #:method-not-allowed
                #:method-not-allowed-method
                #:method-not-allowed-path
                #:method-not-allowed-allowed-methods
                #:path-decoding-error
                #:path-decoding-error-path
                #:path-decoding-error-parameter
                #:path-decoding-error-cause
                #:route-handler-error
                #:route-handler-error-route
                #:route-handler-error-cause)
  (:export #:routing-error
           #:routing-error-reason
           #:route-definition-error
           #:route-definition-value
           #:route-conflict
           #:route-conflict-method
           #:route-conflict-template
           #:route-conflict-existing-template
           #:route-not-found
           #:route-not-found-method
           #:route-not-found-path
           #:method-not-allowed
           #:method-not-allowed-method
           #:method-not-allowed-path
           #:method-not-allowed-allowed-methods
           #:path-decoding-error
           #:path-decoding-error-path
           #:path-decoding-error-parameter
           #:path-decoding-error-cause
           #:route-handler-error
           #:route-handler-error-route
           #:route-handler-error-cause))

(in-package #:clog-router)

(define-condition routing-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader routing-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Hypermedia routing failed~@[ (~A)~]."
             (routing-error-reason condition))))
  (:documentation
   "Base condition for route registration, lookup, decoding and dispatch failures."))

(define-condition route-definition-error (routing-error)
  ((value
    :initarg :value
    :initform nil
    :reader route-definition-value))
  (:report
   (lambda (condition stream)
     (format stream "Invalid route definition~@[ (~A)~]."
             (routing-error-reason condition))))
  (:documentation
   "Signaled while an application registers a malformed route definition.

The rejected value is retained for development diagnostics but is not printed
by the condition report."))

(define-condition route-conflict (route-definition-error)
  ((method
    :initarg :method
    :reader route-conflict-method)
   (template
    :initarg :template
    :reader route-conflict-template)
   (existing-template
    :initarg :existing-template
    :reader route-conflict-existing-template))
  (:report
   (lambda (condition stream)
     (format stream "Conflicting route registration for HTTP method ~A (~A)."
             (route-conflict-method condition)
             (routing-error-reason condition))))
  (:documentation
   "Signaled immediately when a new route overlaps an existing route for the same method or reuses a route name."))

(define-condition route-not-found (routing-error)
  ((method
    :initarg :method
    :reader route-not-found-method)
   (path
    :initarg :path
    :reader route-not-found-path))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No Hypermedia route matched the request.")))
  (:documentation
   "Signaled when no route template matches a valid request method and path."))

(define-condition method-not-allowed (routing-error)
  ((method
    :initarg :method
    :reader method-not-allowed-method)
   (path
    :initarg :path
    :reader method-not-allowed-path)
   (allowed-methods
    :initarg :allowed-methods
    :reader %method-not-allowed-allowed-methods))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "The request path exists but does not allow this HTTP method.")))
  (:documentation
   "Signaled when a path matches one or more routes registered for other HTTP methods."))

(defun method-not-allowed-allowed-methods (condition)
  "Return a fresh deterministic list of methods allowed for the request path."
  (copy-list (%method-not-allowed-allowed-methods condition)))

(define-condition path-decoding-error (routing-error)
  ((path
    :initarg :path
    :initform nil
    :reader path-decoding-error-path)
   (parameter
    :initarg :parameter
    :initform nil
    :reader path-decoding-error-parameter)
   (cause
    :initarg :cause
    :initform nil
    :reader path-decoding-error-cause))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A route path parameter could not be decoded safely.")))
  (:documentation
   "Signaled when a selected named path parameter has malformed percent encoding or invalid UTF-8.

The raw path is retained for controlled development diagnostics but is not
printed by the report."))

(define-condition route-handler-error (routing-error)
  ((route
    :initarg :route
    :reader route-handler-error-route)
   (cause
    :initarg :cause
    :reader route-handler-error-cause))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "A Hypermedia route handler signaled an error.")))
  (:documentation
   "Redacted wrapper for a condition raised while invoking or normalizing a route handler result."))

(setf (documentation 'routing-error-reason 'function)
      "Return the bounded reason keyword carried by a ROUTING-ERROR.")
(setf (documentation 'route-definition-value 'function)
      "Return the rejected route-definition value; callers must apply an explicit logging policy.")
(setf (documentation 'route-conflict-method 'function)
      "Return the normalized HTTP method of a conflicting route registration.")
(setf (documentation 'route-conflict-template 'function)
      "Return the new route template involved in a registration conflict.")
(setf (documentation 'route-conflict-existing-template 'function)
      "Return the existing route template involved in a registration conflict.")
(setf (documentation 'route-not-found-method 'function)
      "Return the normalized method that did not match a route.")
(setf (documentation 'route-not-found-path 'function)
      "Return the unmatched path; callers must not log it without an explicit policy.")
(setf (documentation 'method-not-allowed-method 'function)
      "Return the rejected normalized request method.")
(setf (documentation 'method-not-allowed-path 'function)
      "Return the matched path; callers must not log it without an explicit policy.")
(setf (documentation 'path-decoding-error-path 'function)
      "Return the request path associated with a decoding failure.")
(setf (documentation 'path-decoding-error-parameter 'function)
      "Return the normalized route parameter name associated with a decoding failure, or NIL.")
(setf (documentation 'path-decoding-error-cause 'function)
      "Return the underlying percent/UTF-8 decoder condition retained for development diagnostics.")
(setf (documentation 'route-handler-error-route 'function)
      "Return the immutable route descriptor whose handler failed.")
(setf (documentation 'route-handler-error-cause 'function)
      "Return the underlying handler condition retained for development diagnostics.")
