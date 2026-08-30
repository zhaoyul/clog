;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime request conditions                           ;;;;
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
           #:request-body-parse-cause))

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
                #:request-body-parse-cause)
  (:export #:clog-hypermedia-error
           #:request-error
           #:request-error-reason
           #:request-body-too-large
           #:request-body-too-large-limit
           #:request-body-too-large-length
           #:request-body-parse-error
           #:request-body-parse-content-type
           #:request-body-parse-cause))

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
