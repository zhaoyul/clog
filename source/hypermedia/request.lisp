;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime immutable request context                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-http
  (:export #:request-context
           #:make-request-context
           #:request-env
           #:request-object
           #:request-id
           #:request-session
           #:request-session-id
           #:request-path-params
           #:request-route
           #:request-method
           #:request-path
           #:request-header
           #:query-param
           #:query-param-values
           #:form-param
           #:form-param-values
           #:path-param
           #:request-user
           #:htmx-request-p
           #:htmx-request-type
           #:htmx-partial-request-p
           #:htmx-full-request-p
           #:htmx-request-source
           #:htmx-request-target
           #:htmx-request-trigger
           #:request-current-url
           #:request-csp-nonce))

(defpackage #:clog-hypermedia
  (:import-from #:clog-http
                #:request-context
                #:make-request-context
                #:request-env
                #:request-object
                #:request-id
                #:request-session
                #:request-session-id
                #:request-path-params
                #:request-route
                #:request-method
                #:request-path
                #:request-header
                #:query-param
                #:query-param-values
                #:form-param
                #:form-param-values
                #:path-param
                #:request-user
                #:htmx-request-p
                #:htmx-request-type
                #:htmx-partial-request-p
                #:htmx-full-request-p
                #:htmx-request-source
                #:htmx-request-target
                #:htmx-request-trigger
                #:request-current-url
                #:request-csp-nonce)
  (:export #:request-context
           #:make-request-context
           #:request-env
           #:request-object
           #:request-id
           #:request-session
           #:request-session-id
           #:request-path-params
           #:request-route
           #:request-method
           #:request-path
           #:request-header
           #:query-param
           #:query-param-values
           #:form-param
           #:form-param-values
           #:path-param
           #:request-user
           #:htmx-request-p
           #:htmx-request-type
           #:htmx-partial-request-p
           #:htmx-full-request-p
           #:htmx-request-source
           #:htmx-request-target
           #:htmx-request-trigger
           #:request-current-url
           #:request-csp-nonce))

(in-package #:clog-http)

(defconstant +default-request-body-limit-bytes+ 1048576
  "Default HM-010 request-body ceiling until HM-013 supplies application configuration.")

(defconstant +form-parameters-unparsed+ :clog-form-parameters-unparsed)

(defun copy-request-value (value)
  "Return a defensive recursive copy of request metadata VALUE.

Strings and cons structure are copied recursively. Opaque middleware objects
such as streams, session hash tables, route descriptors and user objects keep
their identity because their mutation semantics belong to their owning layer."
  (typecase value
    (string (copy-seq value))
    (cons (cons (copy-request-value (car value))
                (copy-request-value (cdr value))))
    (vector (copy-seq value))
    (t value)))

(defun copy-headers (headers)
  "Return a case-normalized defensive copy of the Clack HEADERS hash table."
  (let ((copy (make-hash-table :test 'equal)))
    (when (hash-table-p headers)
      (maphash (lambda (name value)
                 (when (stringp name)
                   (setf (gethash (string-downcase name) copy)
                         (copy-request-value value))))
               headers))
    copy))

(defun lack-compatible-headers (headers)
  "Return a header copy safe for the current Lack request adapter.

Lack eagerly splits the Accept header while constructing a request, so a
missing Accept header is represented as the empty string only in the private
adapter copy. The normalized public header table remains faithful to the wire."
  (let ((copy (copy-headers headers)))
    (multiple-value-bind (accept presentp) (gethash "accept" copy)
      (declare (ignore accept))
      (unless presentp
        (setf (gethash "accept" copy) "")))
    copy))

(defun copy-parameter-alist (parameters)
  "Copy a request parameter alist while preserving duplicate order."
  (mapcar (lambda (entry)
            (if (consp entry)
                (cons (copy-request-value (car entry))
                      (copy-request-value (cdr entry)))
                (copy-request-value entry)))
          parameters))

(defun snapshot-request-env (env)
  "Create the request metadata snapshot used by REQUEST-CONTEXT.

The Clack plist itself, headers, parser-produced parameter alists, cookies and
session options are copied. The Lack session hash and raw body stream retain
their middleware-owned identity because session mutation and body consumption
are protocol operations rather than plist mutation."
  (unless (listp env)
    (error 'request-error :reason :invalid-clack-environment))
  (let ((snapshot (copy-list env)))
    (setf (getf snapshot :headers)
          (copy-headers (getf env :headers)))
    (dolist (key '(:query-parameters :body-parameters :cookies :lack.session.options))
      (when (getf env key)
        (setf (getf snapshot key)
              (copy-request-value (getf env key)))))
    (loop for tail on snapshot by #'cddr
          while (cdr tail)
          for value = (cadr tail)
          when (stringp value)
            do (setf (cadr tail) (copy-seq value)))
    snapshot))

(defun metadata-request-env (snapshot)
  "Return an env copy safe for eager Lack metadata/query parsing.

RAW-BODY is removed so LACK.REQUEST:MAKE-REQUEST cannot consume or parse the
body while the request context is being constructed."
  (let ((env (copy-list snapshot)))
    (remf env :raw-body)
    (remf env :body-parameters)
    env))

(defun normalized-http-method (method)
  "Normalize METHOD to the fixed HTTP method vocabulary without INTERN."
  (labels ((known-method-p (candidate)
             (member candidate
                     '(:get :head :post :put :patch :delete :options :trace :connect)
                     :test #'eq)))
    (cond
      ((keywordp method)
       (if (known-method-p method)
           method
           (error 'request-error :reason :unsupported-http-method)))
      ((stringp method)
       (cond
         ((string-equal method "GET") :get)
         ((string-equal method "HEAD") :head)
         ((string-equal method "POST") :post)
         ((string-equal method "PUT") :put)
         ((string-equal method "PATCH") :patch)
         ((string-equal method "DELETE") :delete)
         ((string-equal method "OPTIONS") :options)
         ((string-equal method "TRACE") :trace)
         ((string-equal method "CONNECT") :connect)
         (t (error 'request-error :reason :unsupported-http-method))))
      ((null method)
       (error 'request-error :reason :missing-http-method))
      (t (error 'request-error :reason :invalid-http-method)))))

(defun true-header-value-p (value)
  "Return true only for the HTTP boolean token \"true\", ignoring ASCII whitespace."
  (and (stringp value)
       (string-equal "true" (string-trim '(#\Space #\Tab #\Return #\Newline) value))))

(defun normalize-htmx-request-type (value)
  "Map the bounded HX-Request-Type vocabulary to keywords without INTERN."
  (cond
    ((and (stringp value) (string-equal value "partial")) :partial)
    ((and (stringp value) (string-equal value "full")) :full)
    (t nil)))

(defun session-id-from-env (env)
  "Return the Lack server-side session id from :LACK.SESSION.OPTIONS, if present."
  (let ((options (getf env :lack.session.options)))
    (and (listp options) (getf options :id))))

(defun parameter-values (parameters name)
  "Return all values for parameter NAME in original wire order."
  (unless (stringp name)
    (error 'type-error :datum name :expected-type 'string))
  (loop for entry in parameters
        when (and (consp entry)
                  (stringp (car entry))
                  (string= (car entry) name))
          collect (copy-request-value (cdr entry))))

(defun parameter-value (parameters name default)
  "Return the first value for NAME, or DEFAULT when the parameter is absent."
  (let ((values (parameter-values parameters name)))
    (if values (first values) default)))

(defun media-type-token (content-type)
  "Return the lower-case type/subtype token preceding CONTENT-TYPE parameters."
  (when (stringp content-type)
    (let* ((semicolon (position #\; content-type))
           (token (string-trim '(#\Space #\Tab)
                               (subseq content-type 0 semicolon))))
      (string-downcase token))))

(defun http-token-character-p (character)
  "Return true when CHARACTER is valid in an ASCII media type token."
  (and (< (char-code character) 128)
       (or (alphanumericp character)
           (find character "!#$%&'*+-.^_`|~" :test #'char=))))

(defun valid-media-type-token-p (token)
  "Validate a simple type/subtype media type token without invoking a reader."
  (when (and (stringp token) (> (length token) 2))
    (let ((slash (position #\/ token)))
      (and slash
           (> slash 0)
           (< slash (1- (length token)))
           (null (position #\/ token :start (1+ slash)))
           (every #'http-token-character-p (subseq token 0 slash))
           (every #'http-token-character-p (subseq token (1+ slash)))))))

(defun form-content-type-p (content-type)
  "Return true for the two HTML form media types supported by HM-010."
  (let ((token (media-type-token content-type)))
    (and token
         (or (string= token "application/x-www-form-urlencoded")
             (string= token "multipart/form-data")))))

(defclass bounded-request-body-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:trivial-gray-stream-mixin)
  ((source
    :initarg :source
    :reader bounded-body-source)
   (limit
    :initarg :limit
    :reader bounded-body-limit)
   (count
    :initform 0
    :accessor bounded-body-count))
  (:documentation
   "Binary input stream that signals REQUEST-BODY-TOO-LARGE after LIMIT octets."))

(defmethod stream-element-type
    ((stream bounded-request-body-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream bounded-request-body-stream))
  (let ((octet (read-byte (bounded-body-source stream) nil :eof)))
    (if (eq octet :eof)
        :eof
        (progn
          (incf (bounded-body-count stream))
          (when (> (bounded-body-count stream) (bounded-body-limit stream))
            (error 'request-body-too-large
                   :reason :stream-limit-exceeded
                   :limit (bounded-body-limit stream)
                   :length nil))
          octet))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream bounded-request-body-stream) sequence start end &key)
  "Read into SEQUENCE through the same byte-limit accounting as STREAM-READ-BYTE."
  (loop for index from start below end
        for octet = (trivial-gray-streams:stream-read-byte stream)
        until (eq octet :eof)
        do (setf (elt sequence index) octet)
        finally (return index)))

(defclass request-context ()
  ((env
    :initarg :env
    :reader %request-env)
   (lack-request
    :initarg :lack-request
    :reader %request-object)
   (request-id
    :initarg :request-id
    :initform nil
    :reader %request-id)
   (session
    :initarg :session
    :initform nil
    :reader %request-session)
   (session-id
    :initarg :session-id
    :initform nil
    :reader %request-session-id)
   (path-params
    :initarg :path-params
    :initform nil
    :reader %request-path-params)
   (route
    :initarg :route
    :initform nil
    :reader %request-route)
   (user
    :initarg :user
    :initform nil
    :reader %request-user)
   (method
    :initarg :method
    :reader %request-method)
   (path
    :initarg :path
    :reader %request-path)
   (headers
    :initarg :headers
    :reader %request-headers)
   (query-parameters
    :initarg :query-parameters
    :initform nil
    :reader %request-query-parameters)
   (body-source
    :initarg :body-source
    :initform nil
    :reader %request-body-source)
   (body-content-length
    :initarg :body-content-length
    :initform nil
    :reader %request-body-content-length)
   (body-content-type
    :initarg :body-content-type
    :initform nil
    :reader %request-body-content-type)
   (body-limit-bytes
    :initarg :body-limit-bytes
    :reader %request-body-limit-bytes)
   (form-parameters
    :initarg :form-parameters
    :initform +form-parameters-unparsed+
    :accessor %request-form-parameters)
   (body-parse-lock
    :initform (bordeaux-threads:make-lock "clog request body parser")
    :reader %request-body-parse-lock)
   (htmx-p
    :initarg :htmx-p
    :reader %htmx-request-p)
   (htmx-request-type
    :initarg :htmx-request-type
    :reader %htmx-request-type)
   (htmx-source
    :initarg :htmx-source
    :initform nil
    :reader %htmx-request-source)
   (htmx-target
    :initarg :htmx-target
    :initform nil
    :reader %htmx-request-target)
   (htmx-trigger
    :initarg :htmx-trigger
    :initform nil
    :reader %htmx-request-trigger)
   (current-url
    :initarg :current-url
    :initform nil
    :reader %request-current-url)
   (csp-nonce
    :initarg :csp-nonce
    :initform nil
    :reader %request-csp-nonce))
  (:documentation
   "Immutable request metadata plus a bounded, memoized form-body parser.

Public accessors never mutate the source Clack plist. Form parsing is lazy and
serialized per context; the only internal mutation is memoization of the
parsed form parameter alist. The middleware-owned Lack session hash retains
its identity so later application code can use Lack session semantics."))

(defun request-object (context)
  "Return the Lack request adapter owned by CONTEXT.

The adapter was built from a defensive env copy and contains no raw body stream.
Its identity is stable for the lifetime of CONTEXT."
  (%request-object context))

(defun request-id (context)
  "Return a defensive copy of CONTEXT's request identifier, or NIL."
  (copy-request-value (%request-id context)))

(defun request-session (context)
  "Return CONTEXT's Lack session hash table, or NIL.

Session identity is intentionally preserved because Lack owns session mutation."
  (%request-session context))

(defun request-session-id (context)
  "Return a defensive copy of CONTEXT's server-side Lack session id, or NIL."
  (copy-request-value (%request-session-id context)))

(defun request-path-params (context)
  "Return a defensive copy of CONTEXT's route path-parameter alist."
  (copy-parameter-alist (%request-path-params context)))

(defun request-route (context)
  "Return the route descriptor associated with CONTEXT, or NIL.

The descriptor remains owned by the router layer; REQUEST-CONTEXT does not mutate it."
  (%request-route context))

(defun request-user (context)
  "Return the authorization/user object associated with CONTEXT, or NIL.

The object remains owned by the authorization layer; REQUEST-CONTEXT does not mutate it."
  (%request-user context))

(defun request-method (context)
  "Return CONTEXT's normalized HTTP method keyword, or NIL."
  (%request-method context))

(defun request-path (context)
  "Return a defensive copy of CONTEXT's normalized request path."
  (copy-request-value (%request-path context)))

(defun htmx-request-p (context)
  "Return true when CONTEXT carries an HX-Request header with value true."
  (%htmx-request-p context))

(defun htmx-request-type (context)
  "Return :PARTIAL, :FULL or NIL from the bounded HX-Request-Type vocabulary."
  (%htmx-request-type context))

(defun htmx-request-source (context)
  "Return a defensive copy of CONTEXT's HX-Source metadata, or NIL."
  (copy-request-value (%htmx-request-source context)))

(defun htmx-request-target (context)
  "Return a defensive copy of CONTEXT's HX-Target metadata, or NIL."
  (copy-request-value (%htmx-request-target context)))

(defun htmx-request-trigger (context)
  "Return a defensive copy of CONTEXT's HX-Trigger metadata, or NIL."
  (copy-request-value (%htmx-request-trigger context)))

(defun request-current-url (context)
  "Return a defensive copy of CONTEXT's HX-Current-URL metadata, or NIL."
  (copy-request-value (%request-current-url context)))

(defun request-csp-nonce (context)
  "Return a defensive copy of CONTEXT's request CSP nonce, or NIL."
  (copy-request-value (%request-csp-nonce context)))

(defun request-env (context)
  "Return a defensive copy of CONTEXT's normalized Clack environment snapshot.

Changing the returned plist or its headers cannot mutate the source env used
to construct CONTEXT. The middleware-owned Lack session object keeps its identity;
the raw body stream is intentionally omitted and remains private to bounded form parsing."
  (let ((snapshot (snapshot-request-env (%request-env context))))
    ;; The body stream is deliberately private to the context's bounded parser.
    ;; Returning the original stream here would let ordinary handler code mutate
    ;; request state merely by reading from an otherwise defensive env snapshot.
    (remf snapshot :raw-body)
    snapshot))

(defun request-header (context name)
  "Return request header NAME case-insensitively, or NIL when it is absent."
  (unless (stringp name)
    (error 'type-error :datum name :expected-type 'string))
  (copy-request-value
   (gethash (string-downcase name) (%request-headers context))))

(defun query-param-values (context name)
  "Return all query parameter NAME values in original wire order."
  (parameter-values (%request-query-parameters context) name))

(defun query-param (context name &optional default)
  "Return the first query parameter NAME value, or DEFAULT when absent.

Use QUERY-PARAM-VALUES when duplicate parameters are semantically meaningful."
  (parameter-value (%request-query-parameters context) name default))

(defun request-has-form-body-p (context)
  "Return true when CONTEXT has a non-empty body whose media type is a form type."
  (let ((length (%request-body-content-length context))
        (source (%request-body-source context))
        (transfer-encoding (request-header context "Transfer-Encoding")))
    (and source
         (or (and (integerp length) (plusp length))
             (and (stringp transfer-encoding)
                  (string-equal transfer-encoding "chunked")))
         (form-content-type-p (%request-body-content-type context)))))

(defun validate-body-content-type (context)
  "Validate the media type token before delegating form parsing to Lack/http-body."
  (let* ((content-type (%request-body-content-type context))
         (token (media-type-token content-type)))
    (when (and (%request-body-source context)
               (or (and (integerp (%request-body-content-length context))
                        (plusp (%request-body-content-length context)))
                   (string-equal (or (request-header context "Transfer-Encoding") "")
                                 "chunked"))
               (or (not (stringp content-type))
                   (not (valid-media-type-token-p token))))
      (error 'request-body-parse-error
             :reason :malformed-content-type
             :content-type content-type
             :cause nil))))

(defun parse-form-parameters (context)
  "Parse CONTEXT's form body once using Lack while enforcing the byte ceiling."
  (validate-body-content-type context)
  (unless (request-has-form-body-p context)
    (return-from parse-form-parameters nil))
  (let ((declared-length (%request-body-content-length context))
        (limit (%request-body-limit-bytes context)))
    (when (and declared-length (> declared-length limit))
      (error 'request-body-too-large
             :reason :declared-length-exceeded
             :limit limit
             :length declared-length))
    (let* ((snapshot (snapshot-request-env (%request-env context)))
           (bounded-stream
             (make-instance 'bounded-request-body-stream
                            :source (%request-body-source context)
                            :limit limit)))
      (setf (getf snapshot :raw-body) bounded-stream)
      (setf (getf snapshot :headers)
            (lack-compatible-headers (getf snapshot :headers)))
      (remf snapshot :body-parameters)
      (handler-case
          (let* ((request (lack.request:make-request snapshot))
                 (parameters (lack.request:request-body-parameters request)))
            (copy-parameter-alist parameters))
        (request-body-too-large (condition)
          (error condition))
        (error (cause)
          (error 'request-body-parse-error
                 :reason :form-parser-failure
                 :content-type (%request-body-content-type context)
                 :cause cause))))))

(defun ensure-form-parameters (context)
  "Return memoized form parameters, parsing the body at most once per context."
  (if (eq (%request-form-parameters context) +form-parameters-unparsed+)
      (bordeaux-threads:with-lock-held ((%request-body-parse-lock context))
        (when (eq (%request-form-parameters context) +form-parameters-unparsed+)
          (setf (%request-form-parameters context)
                (parse-form-parameters context)))
        (%request-form-parameters context))
      (%request-form-parameters context)))

(defun form-param-values (context name)
  "Return all form parameter NAME values in original wire order.

The body is parsed lazily on the first form accessor and is protected by the
context's configured byte limit. REQUEST-BODY-TOO-LARGE or
REQUEST-BODY-PARSE-ERROR may be signaled."
  (parameter-values (ensure-form-parameters context) name))

(defun form-param (context name &optional default)
  "Return the first form parameter NAME value, or DEFAULT when absent.

The same typed conditions as FORM-PARAM-VALUES may be signaled."
  (parameter-value (ensure-form-parameters context) name default))

(defun path-param (context name &optional default)
  "Return route path parameter NAME or DEFAULT without interning runtime input."
  (let ((entry
          (find name (%request-path-params context)
                :key #'car
                :test (if (stringp name) #'string-equal #'eql))))
    (if entry (copy-request-value (cdr entry)) default)))

(defun htmx-partial-request-p (context)
  "Return true when HX-Request-Type identifies a partial HTMX request."
  (eq (htmx-request-type context) :partial))

(defun htmx-full-request-p (context)
  "Return true when HX-Request-Type identifies a full HTMX request."
  (eq (htmx-request-type context) :full))

(defun make-request-context (env &key request-id path-params route user csp-nonce
                                       (body-limit-bytes
                                         +default-request-body-limit-bytes+))
  "Normalize Clack ENV into a stable REQUEST-CONTEXT.

ENV is never passed directly to LACK.REQUEST:MAKE-REQUEST, because the current
Lack implementation memoizes parsed data by mutating that plist. Instead a
defensive metadata snapshot is used. Query parameters are parsed eagerly from
that snapshot; form bodies are parsed lazily through FORM-PARAM/FORM-PARAM-VALUES.
BODY-LIMIT-BYTES is enforced from Content-Length and again while bytes are read.
The function signals REQUEST-ERROR for malformed adapter inputs and body
accessors may later signal REQUEST-BODY-TOO-LARGE or REQUEST-BODY-PARSE-ERROR.
No runtime string is interned or evaluated."
  (unless (and (integerp body-limit-bytes) (plusp body-limit-bytes))
    (error 'request-error :reason :invalid-body-limit))
  (let ((content-length (getf env :content-length)))
    (unless (or (null content-length)
                (and (integerp content-length) (not (minusp content-length))))
      (error 'request-error :reason :invalid-content-length))
    (when (and content-length (> content-length body-limit-bytes))
      (error 'request-body-too-large
             :reason :declared-length-exceeded
             :limit body-limit-bytes
             :length content-length)))
  (let* ((snapshot (snapshot-request-env env))
         (headers (copy-headers (getf snapshot :headers)))
         (method (normalized-http-method (getf snapshot :request-method)))
         (metadata-env (metadata-request-env snapshot)))
    ;; Lack's request structure requires a keyword METHOD. Normalize through our
    ;; finite HTTP vocabulary before the adapter sees user-supplied metadata so
    ;; invalid strings produce REQUEST-ERROR instead of a third-party TYPE-ERROR.
    (setf (getf metadata-env :request-method) method)
    (setf (getf metadata-env :headers) (lack-compatible-headers headers))
    (let* ((lack-request (lack.request:make-request metadata-env))
           (query-parameters
             (copy-parameter-alist
              (lack.request:request-query-parameters lack-request)))
           (path
             (copy-request-value
              (or (lack.request:request-path-info lack-request)
                  (getf snapshot :path-info)
                  "/")))
           (hx-request (gethash "hx-request" headers))
           (hx-request-type (gethash "hx-request-type" headers)))
      (make-instance
       'request-context
       :env snapshot
       :lack-request lack-request
       :request-id (copy-request-value
                    (or request-id (getf snapshot :clog.request-id)))
       :session (getf snapshot :lack.session)
       :session-id (session-id-from-env snapshot)
       :path-params (copy-parameter-alist path-params)
       :route route
       :user (or user (getf snapshot :clog.user))
       :method method
       :path path
       :headers headers
       :query-parameters query-parameters
       :form-parameters (if (getf snapshot :body-parameters)
                            (copy-parameter-alist (getf snapshot :body-parameters))
                            +form-parameters-unparsed+)
       :body-source (getf env :raw-body)
       :body-content-length (getf snapshot :content-length)
       :body-content-type (copy-request-value (getf snapshot :content-type))
       :body-limit-bytes body-limit-bytes
       :htmx-p (true-header-value-p hx-request)
       :htmx-request-type (normalize-htmx-request-type hx-request-type)
       :htmx-source (copy-request-value (gethash "hx-source" headers))
       :htmx-target (copy-request-value (gethash "hx-target" headers))
       :htmx-trigger (copy-request-value (gethash "hx-trigger" headers))
       :current-url (copy-request-value (gethash "hx-current-url" headers))
       :csp-nonce (copy-request-value
                    (or csp-nonce (getf snapshot :clog.csp-nonce)))))))
