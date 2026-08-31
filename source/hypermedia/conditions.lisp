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
