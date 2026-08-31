;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic router                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-router
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:export #:routing-error
           #:routing-error-reason
           #:invalid-route-method
           #:invalid-route-method-value
           #:invalid-route-template
           #:invalid-route-template-value
           #:invalid-route-template-reason
           #:route-conflict
           #:route-conflict-method
           #:route-conflict-template
           #:route-conflict-existing-template
           #:duplicate-route-name
           #:duplicate-route-name-value
           #:route-not-found
           #:route-not-found-path
           #:route-not-found-name
           #:method-not-allowed
           #:method-not-allowed-path
           #:method-not-allowed-allowed-methods
           #:path-decoding-error
           #:path-decoding-error-path
           #:path-decoding-error-cause
           #:invalid-route-parameters
           #:invalid-route-parameters-reason
           #:route-handler-error
           #:route-handler-error-route
           #:route-handler-error-cause
           #:route
           #:route-p
           #:route-method
           #:route-template
           #:route-handler
           #:route-name
           #:route-middleware
           #:route-metadata
           #:route-parameter-names
           #:router
           #:router-p
           #:make-router
           #:router-routes
           #:add-route
           #:find-route
           #:dispatch-route
           #:route-url))

(defpackage #:clog-hypermedia
  (:import-from #:clog-router
                #:routing-error
                #:routing-error-reason
                #:invalid-route-method
                #:invalid-route-method-value
                #:invalid-route-template
                #:invalid-route-template-value
                #:invalid-route-template-reason
                #:route-conflict
                #:route-conflict-method
                #:route-conflict-template
                #:route-conflict-existing-template
                #:duplicate-route-name
                #:duplicate-route-name-value
                #:route-not-found
                #:route-not-found-path
                #:route-not-found-name
                #:method-not-allowed
                #:method-not-allowed-path
                #:method-not-allowed-allowed-methods
                #:path-decoding-error
                #:path-decoding-error-path
                #:path-decoding-error-cause
                #:invalid-route-parameters
                #:invalid-route-parameters-reason
                #:route-handler-error
                #:route-handler-error-route
                #:route-handler-error-cause
                #:route
                #:route-p
                #:route-method
                #:route-template
                #:route-handler
                #:route-name
                #:route-middleware
                #:route-metadata
                #:route-parameter-names
                #:router
                #:router-p
                #:make-router
                #:router-routes
                #:add-route
                #:find-route
                #:dispatch-route
                #:route-url)
  (:export #:routing-error
           #:routing-error-reason
           #:invalid-route-method
           #:invalid-route-method-value
           #:invalid-route-template
           #:invalid-route-template-value
           #:invalid-route-template-reason
           #:route-conflict
           #:route-conflict-method
           #:route-conflict-template
           #:route-conflict-existing-template
           #:duplicate-route-name
           #:duplicate-route-name-value
           #:route-not-found
           #:route-not-found-path
           #:route-not-found-name
           #:method-not-allowed
           #:method-not-allowed-path
           #:method-not-allowed-allowed-methods
           #:path-decoding-error
           #:path-decoding-error-path
           #:path-decoding-error-cause
           #:invalid-route-parameters
           #:invalid-route-parameters-reason
           #:route-handler-error
           #:route-handler-error-route
           #:route-handler-error-cause
           #:route
           #:route-p
           #:route-method
           #:route-template
           #:route-handler
           #:route-name
           #:route-middleware
           #:route-metadata
           #:route-parameter-names
           #:router
           #:router-p
           #:make-router
           #:router-routes
           #:add-route
           #:find-route
           #:dispatch-route
           #:route-url))

(in-package #:clog-router)

(define-condition routing-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform :routing-error
    :reader routing-error-reason))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "CLOG hypermedia routing failed." stream)))
  (:documentation
   "Base condition for deterministic router registration and dispatch failures."))

(define-condition invalid-route-method (routing-error)
  ((value
    :initarg :value
    :reader invalid-route-method-value))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route method is invalid or unsupported." stream)))
  (:documentation
   "Signaled when route registration or lookup receives an unsupported method."))

(define-condition invalid-route-template (routing-error)
  ((value
    :initarg :value
    :reader invalid-route-template-value)
   (template-reason
    :initarg :template-reason
    :reader invalid-route-template-reason))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route template is invalid." stream)))
  (:documentation
   "Signaled when a route template cannot be compiled safely."))

(define-condition route-conflict (routing-error)
  ((method
    :initarg :method
    :reader route-conflict-method)
   (template
    :initarg :template
    :reader route-conflict-template)
   (existing-template
    :initarg :existing-template
    :reader route-conflict-existing-template))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route conflicts with an existing route." stream)))
  (:documentation
   "Signaled at registration time when same-method route matching would be ambiguous."))

(define-condition duplicate-route-name (routing-error)
  ((value
    :initarg :value
    :reader duplicate-route-name-value))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route name is already registered." stream)))
  (:documentation
   "Signaled when a router receives a duplicate named-route identifier."))

(define-condition route-not-found (routing-error)
  ((path
    :initarg :path
    :initform nil
    :reader route-not-found-path)
   (name
    :initarg :name
    :initform nil
    :reader route-not-found-name))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "No matching route was found." stream)))
  (:documentation
   "Signaled for an unknown request path or named route without exposing its value."))

(define-condition method-not-allowed (routing-error)
  ((path
    :initarg :path
    :reader method-not-allowed-path)
   (allowed-methods
    :initarg :allowed-methods
    :reader method-not-allowed-allowed-methods))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The request method is not allowed for this path." stream)))
  (:documentation
   "Signaled when a path matches but its normalized method does not."))

(define-condition path-decoding-error (routing-error)
  ((path
    :initarg :path
    :reader path-decoding-error-path)
   (cause
    :initarg :cause
    :initform nil
    :reader path-decoding-error-cause))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The request path could not be decoded safely." stream)))
  (:documentation
   "Signaled for malformed percent encoding, invalid UTF-8, or unsafe decoded separators."))

(define-condition invalid-route-parameters (routing-error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route parameters are invalid." stream)))
  (:documentation
   "Signaled for invalid handlers, middleware, names, or named-route URL parameters."))

(defun invalid-route-parameters-reason (condition)
  "Return the bounded reason associated with CONDITION."
  (check-type condition invalid-route-parameters)
  (routing-error-reason condition))

(define-condition route-handler-error (routing-error)
  ((route
    :initarg :route
    :reader route-handler-error-route)
   (cause
    :initarg :cause
    :reader route-handler-error-cause))
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "The route handler failed." stream)))
  (:documentation
   "Redacted wrapper for failures raised by route middleware, handlers, or response normalization."))

(defparameter +route-methods+
  '(:get :head :post :put :patch :delete :options :trace :connect)
  "Frozen method order used for deterministic routing and Allow headers.")

(defstruct (route-segment
             (:constructor %make-route-segment (kind value)))
  kind
  value)

(defclass route ()
  ((method
    :initarg :method
    :reader %route-method)
   (template
    :initarg :template
    :reader %route-template)
   (segments
    :initarg :segments
    :reader %route-segments)
   (handler
    :initarg :handler
    :reader %route-handler)
   (name
    :initarg :name
    :initform nil
    :reader %route-name)
   (middleware
    :initarg :middleware
    :initform nil
    :reader %route-middleware)
   (metadata
    :initarg :metadata
    :initform nil
    :reader %route-metadata)
   (parameter-names
    :initarg :parameter-names
    :initform nil
    :reader %route-parameter-names)
   (exact-p
    :initarg :exact-p
    :reader %route-exact-p))
  (:documentation
   "Compiled immutable route descriptor owned by a ROUTER."))

(defclass router ()
  ((routes
    :initform nil
    :accessor %router-routes)
   (routes-by-name
    :initform (make-hash-table :test 'equal)
    :reader %router-routes-by-name))
  (:documentation
   "Mutable registration container whose lookup semantics do not depend on insertion order."))

(defun route-p (value)
  "Return true when VALUE is a compiled ROUTE descriptor."
  (typep value 'route))

(defun router-p (value)
  "Return true when VALUE is a ROUTER."
  (typep value 'router))

(defun copy-router-string (value)
  "Return a defensive copy of VALUE when it is a string."
  (if (stringp value) (copy-seq value) value))

(defun copy-string-list (values)
  "Return a defensive copy of a list whose members may be strings."
  (mapcar #'copy-router-string values))

(defun route-method (route)
  "Return ROUTE's normalized HTTP method keyword."
  (check-type route route)
  (%route-method route))

(defun route-template (route)
  "Return a defensive copy of ROUTE's path template."
  (check-type route route)
  (copy-seq (%route-template route)))

(defun route-handler (route)
  "Return ROUTE's handler function."
  (check-type route route)
  (%route-handler route))

(defun route-name (route)
  "Return ROUTE's name, or NIL. String names are copied."
  (check-type route route)
  (copy-router-string (%route-name route)))

(defun route-middleware (route)
  "Return a shallow copy of ROUTE's middleware chain."
  (check-type route route)
  (copy-list (%route-middleware route)))

(defun route-metadata (route)
  "Return ROUTE's application-owned metadata object."
  (check-type route route)
  (%route-metadata route))

(defun route-parameter-names (route)
  "Return defensive copies of ROUTE's parameter names in template order."
  (check-type route route)
  (copy-string-list (%route-parameter-names route)))

(defun make-router ()
  "Create an empty deterministic ROUTER."
  (make-instance 'router))

(defun router-routes (router)
  "Return a shallow copy of ROUTER's descriptors in deterministic order."
  (check-type router router)
  (copy-list (%router-routes router)))

(defun finite-proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (labels ((walk (slow fast)
             (cond
               ((null fast) t)
               ((atom fast) nil)
               ((null (cdr fast)) t)
               ((atom (cdr fast)) nil)
               (t
                (let ((next-slow (cdr slow))
                      (next-fast (cddr fast)))
                  (and (not (eq next-slow next-fast))
                       (walk next-slow next-fast)))))))
    (or (null value)
        (and (consp value)
             (walk value value)))))

(defun normalize-route-method (method)
  "Map METHOD to the fixed HTTP method vocabulary without creating symbols."
  (cond
    ((and (keywordp method)
          (member method +route-methods+ :test #'eq))
     method)
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
       (t
        (error 'invalid-route-method
               :reason :unsupported-method
               :value method))))
    (t
     (error 'invalid-route-method
            :reason :invalid-method
            :value method))))

(defun route-method-rank (method)
  "Return METHOD's deterministic position in +ROUTE-METHODS+."
  (or (position method +route-methods+ :test #'eq)
      (length +route-methods+)))

(defun method-token (method)
  "Return the canonical uppercase HTTP token for METHOD."
  (ecase method
    (:get "GET")
    (:head "HEAD")
    (:post "POST")
    (:put "PUT")
    (:patch "PATCH")
    (:delete "DELETE")
    (:options "OPTIONS")
    (:trace "TRACE")
    (:connect "CONNECT")))

(defun route-control-character-p (character)
  "Return true when CHARACTER is unsafe in a route path or template."
  (let ((code (char-code character)))
    (or (= code 127)
        (< code 32))))

(defun valid-route-parameter-character-p (character)
  "Return true for the bounded ASCII route-parameter alphabet."
  (and (< (char-code character) 128)
       (or (alphanumericp character)
           (char= character #\-)
           (char= character #\_))))

(defun valid-route-parameter-name-p (name)
  "Return true when NAME is a non-empty bounded route parameter name."
  (and (stringp name)
       (plusp (length name))
       (every #'valid-route-parameter-character-p name)))

(defun split-path-segments (path)
  "Split absolute PATH into segments while preserving empty request segments."
  (if (string= path "/")
      nil
      (loop with length = (length path)
            for start = 1 then (1+ slash)
            for slash = (position #\/ path :start start)
            collect (subseq path start (or slash length))
            while slash)))

(defun signal-invalid-template (template reason)
  "Signal INVALID-ROUTE-TEMPLATE for TEMPLATE and bounded REASON."
  (error 'invalid-route-template
         :reason reason
         :value template
         :template-reason reason))

(defun validate-route-template-string (template)
  "Validate TEMPLATE's absolute-path shape before segment compilation."
  (unless (and (stringp template)
               (plusp (length template))
               (char= (char template 0) #\/))
    (signal-invalid-template template :not-an-absolute-path))
  (when (or (find #\? template)
            (find #\# template)
            (find #\\ template)
            (some #'route-control-character-p template))
    (signal-invalid-template template :unsafe-template-character))
  template)

(defun parse-route-template (template)
  "Compile TEMPLATE into route segments and return segments plus parameter names."
  (validate-route-template-string template)
  (let ((parameter-names nil)
        (segments nil)
        (raw-segments (split-path-segments template)))
    (loop for raw in raw-segments
          for index from 0
          for last-p = (= index (1- (length raw-segments)))
          do
             (when (zerop (length raw))
               (signal-invalid-template template :empty-segment))
             (cond
               ((char= (char raw 0) #\:)
                (let ((name (string-downcase (subseq raw 1))))
                  (unless (valid-route-parameter-name-p name)
                    (signal-invalid-template template :invalid-parameter-name))
                  (when (member name parameter-names :test #'string=)
                    (signal-invalid-template template :duplicate-parameter-name))
                  (push name parameter-names)
                  (push (%make-route-segment :parameter name) segments)))
               ((char= (char raw 0) #\*)
                (let ((name (string-downcase (subseq raw 1))))
                  (unless last-p
                    (signal-invalid-template template :wildcard-must-be-final))
                  (unless (valid-route-parameter-name-p name)
                    (signal-invalid-template template :invalid-wildcard-name))
                  (when (member name parameter-names :test #'string=)
                    (signal-invalid-template template :duplicate-parameter-name))
                  (push name parameter-names)
                  (push (%make-route-segment :wildcard name) segments)))
               (t
                (when (find #\% raw)
                  (signal-invalid-template template :encoded-static-segment))
                (push (%make-route-segment :static (copy-seq raw)) segments))))
    (values (nreverse segments)
            (nreverse parameter-names))))

(defun exact-route-segments-p (segments)
  "Return true when every compiled segment is static."
  (every (lambda (segment)
           (eq :static (route-segment-kind segment)))
         segments))

(defun normalize-route-name (name &key lookup-p)
  "Validate and defensively copy a route NAME."
  (cond
    ((null name)
     (if lookup-p
         (error 'invalid-route-parameters :reason :missing-route-name)
         nil))
    ((keywordp name) name)
    ((and (stringp name) (plusp (length name))) (copy-seq name))
    (t
     (error 'invalid-route-parameters :reason :invalid-route-name))))

(defun normalize-route-middleware (middleware)
  "Normalize MIDDLEWARE to a finite list of functions."
  (cond
    ((null middleware) nil)
    ((functionp middleware) (list middleware))
    ((and (finite-proper-list-p middleware)
          (every #'functionp middleware))
     (copy-list middleware))
    (t
     (error 'invalid-route-parameters :reason :invalid-route-middleware))))

(defun route-segments-overlap-p (left right)
  "Return true when LEFT and RIGHT templates can match at least one same path."
  (labels ((walk (left right)
             (cond
               ((and (null left) (null right)) t)
               ((and left
                     (eq :wildcard (route-segment-kind (first left))))
                t)
               ((and right
                     (eq :wildcard (route-segment-kind (first right))))
                t)
               ((or (null left) (null right)) nil)
               ((and (eq :static (route-segment-kind (first left)))
                     (eq :static (route-segment-kind (first right))))
                (and (string= (route-segment-value (first left))
                              (route-segment-value (first right)))
                     (walk (rest left) (rest right))))
               (t
                (walk (rest left) (rest right))))))
    (walk left right)))

(defun route-precedes-p (left right)
  "Return true when LEFT sorts before RIGHT in the deterministic route table."
  (cond
    ((and (%route-exact-p left) (not (%route-exact-p right))) t)
    ((and (not (%route-exact-p left)) (%route-exact-p right)) nil)
    ((string< (%route-template left) (%route-template right)) t)
    ((string> (%route-template left) (%route-template right)) nil)
    (t (< (route-method-rank (%route-method left))
          (route-method-rank (%route-method right))))))

(defun ensure-no-route-conflict (router candidate)
  "Reject same-method registrations whose matching semantics are ambiguous."
  (dolist (existing (%router-routes router))
    (when (and (eq (%route-method existing) (%route-method candidate))
               (route-segments-overlap-p (%route-segments existing)
                                         (%route-segments candidate))
               (not (and (%route-exact-p existing)
                         (not (%route-exact-p candidate))))
               (not (and (%route-exact-p candidate)
                         (not (%route-exact-p existing)))))
      (error 'route-conflict
             :reason :overlapping-route
             :method (%route-method candidate)
             :template (%route-template candidate)
             :existing-template (%route-template existing)))))

(defun add-route (router method template handler
                  &key name middleware metadata)
  "Compile and register one deterministic route.

Static routes may overlap a dynamic route and always take precedence for the
same method. Two overlapping dynamic templates, or duplicate static templates,
fail immediately during registration. MIDDLEWARE is NIL, one function, or a
finite list of functions using the `(next context)` protocol."
  (check-type router router)
  (unless (functionp handler)
    (error 'invalid-route-parameters :reason :invalid-route-handler))
  (let* ((method (normalize-route-method method))
         (name (normalize-route-name name))
         (middleware (normalize-route-middleware middleware)))
    (multiple-value-bind (segments parameter-names)
        (parse-route-template template)
      (when name
        (multiple-value-bind (existing present-p)
            (gethash name (%router-routes-by-name router))
          (declare (ignore existing))
          (when present-p
            (error 'duplicate-route-name
                   :reason :duplicate-route-name
                   :value name))))
      (let ((candidate
              (make-instance 'route
                             :method method
                             :template (copy-seq template)
                             :segments segments
                             :handler handler
                             :name name
                             :middleware middleware
                             :metadata metadata
                             :parameter-names parameter-names
                             :exact-p (exact-route-segments-p segments))))
        (ensure-no-route-conflict router candidate)
        (setf (%router-routes router)
              (sort (cons candidate (copy-list (%router-routes router)))
                    #'route-precedes-p))
        (when name
          (setf (gethash name (%router-routes-by-name router)) candidate))
        candidate))))

(defun protect-path-plus-signs (segment)
  "Protect literal plus signs from form-style URL decoders."
  (with-output-to-string (stream)
    (loop for character across segment
          if (char= character #\+)
            do (write-string "%2B" stream)
          else
            do (write-char character stream))))

(defun invalid-decoded-path-character-p (character)
  "Return true when decoded CHARACTER could alter path segmentation or framing."
  (or (char= character #\/)
      (char= character #\\)
      (route-control-character-p character)))

(defun decode-path-segment (segment path)
  "Strictly percent-decode one path SEGMENT while preserving literal plus signs."
  (let ((decoded
          (if (find #\% segment)
              (handler-case
                  (quri:url-decode (protect-path-plus-signs segment)
                                   :encoding :utf-8
                                   :lenient nil)
                (error (cause)
                  (error 'path-decoding-error
                         :reason :invalid-percent-encoding
                         :path path
                         :cause cause)))
              (copy-seq segment))))
    (when (some #'invalid-decoded-path-character-p decoded)
      (error 'path-decoding-error
             :reason :unsafe-decoded-segment
             :path path
             :cause nil))
    decoded))

(defun decode-request-path (path)
  "Validate and decode absolute request PATH into path segments."
  (unless (and (stringp path)
               (plusp (length path))
               (char= (char path 0) #\/))
    (error 'path-decoding-error
           :reason :not-an-absolute-path
           :path path
           :cause nil))
  (when (or (find #\? path)
            (find #\# path)
            (find #\\ path)
            (some #'route-control-character-p path))
    (error 'path-decoding-error
           :reason :unsafe-path-character
           :path path
           :cause nil))
  (let ((segments (split-path-segments path)))
    (when (some (lambda (segment) (zerop (length segment))) segments)
      (error 'path-decoding-error
             :reason :empty-path-segment
             :path path
             :cause nil))
    (mapcar (lambda (segment)
              (decode-path-segment segment path))
            segments)))

(defun join-path-segments (segments)
  "Join decoded SEGMENTS with `/` without adding a leading slash."
  (with-output-to-string (stream)
    (loop for segment in segments
          for first-p = t then nil
          unless first-p do (write-char #\/ stream)
          do (write-string segment stream))))

(defun match-route-segments (route decoded-segments)
  "Return matching path parameters and true, or NIL and false."
  (labels ((walk (route-segments path-segments parameters)
             (cond
               ((null route-segments)
                (if (null path-segments)
                    (values (nreverse parameters) t)
                    (values nil nil)))
               ((eq :wildcard
                    (route-segment-kind (first route-segments)))
                (values
                 (nreverse
                  (acons (route-segment-value (first route-segments))
                         (join-path-segments path-segments)
                         parameters))
                 t))
               ((null path-segments)
                (values nil nil))
               ((eq :static
                    (route-segment-kind (first route-segments)))
                (if (string= (route-segment-value (first route-segments))
                             (first path-segments))
                    (walk (rest route-segments)
                          (rest path-segments)
                          parameters)
                    (values nil nil)))
               ((eq :parameter
                    (route-segment-kind (first route-segments)))
                (if (plusp (length (first path-segments)))
                    (walk (rest route-segments)
                          (rest path-segments)
                          (acons (route-segment-value (first route-segments))
                                 (copy-seq (first path-segments))
                                 parameters))
                    (values nil nil)))
               (t
                (values nil nil)))))
    (walk (%route-segments route) decoded-segments nil)))

(defun sorted-unique-methods (methods)
  "Return METHODS without duplicates in the frozen HTTP order."
  (sort (remove-duplicates (copy-list methods) :test #'eq)
        #'<
        :key #'route-method-rank))

(defun find-route (router method path)
  "Find METHOD and PATH in ROUTER.

Return three values: the selected ROUTE or NIL, its path-parameter alist or
NIL, and the sorted methods allowed for the path when METHOD does not match.
Lookup decodes path segments strictly before matching and never creates symbols
from request data."
  (check-type router router)
  (let* ((method (normalize-route-method method))
         (decoded-segments (decode-request-path path))
         (matches nil))
    (dolist (route (%router-routes router))
      (multiple-value-bind (parameters matched-p)
          (match-route-segments route decoded-segments)
        (when matched-p
          (push (cons route parameters) matches))))
    (setf matches (nreverse matches))
    (let ((selected
            (find method matches
                  :key (lambda (match)
                         (%route-method (car match)))
                  :test #'eq)))
      (if selected
          (values (car selected) (cdr selected) nil)
          (values nil
                  nil
                  (sorted-unique-methods
                   (mapcar (lambda (match)
                             (%route-method (car match)))
                           matches)))))))

(defun copy-path-parameters (parameters)
  "Defensively copy a route path-parameter alist."
  (mapcar (lambda (entry)
            (cons (copy-router-string (car entry))
                  (copy-router-string (cdr entry))))
          parameters))

(defun routed-request-context (context route parameters)
  "Clone CONTEXT with ROUTE and PARAMETERS while preserving lazy body state."
  (make-instance
   'clog-http:request-context
   :env (clog-http::%request-env context)
   :lack-request (clog-http::%request-object context)
   :request-id (clog-http::%request-id context)
   :session (clog-http::%request-session context)
   :session-id (clog-http::%request-session-id context)
   :path-params (copy-path-parameters parameters)
   :route route
   :user (clog-http::%request-user context)
   :method (clog-http::%request-method context)
   :path (clog-http::%request-path context)
   :headers (clog-http::%request-headers context)
   :query-parameters (clog-http::%request-query-parameters context)
   :body-source (clog-http::%request-body-source context)
   :body-content-length (clog-http::%request-body-content-length context)
   :body-content-type (clog-http::%request-body-content-type context)
   :body-limit-bytes (clog-http::%request-body-limit-bytes context)
   :form-parameters (clog-http::%request-form-parameters context)
   :htmx-p (clog-http::%htmx-request-p context)
   :htmx-request-type (clog-http::%htmx-request-type context)
   :htmx-source (clog-http::%htmx-request-source context)
   :htmx-target (clog-http::%htmx-request-target context)
   :htmx-trigger (clog-http::%htmx-request-trigger context)
   :current-url (clog-http::%request-current-url context)
   :csp-nonce (clog-http::%request-csp-nonce context)))

(defun ensure-request-context (request)
  "Return REQUEST as a request context, normalizing a raw Clack env when needed."
  (cond
    ((typep request 'clog-http:request-context) request)
    ((listp request) (clog-http:make-request-context request))
    (t
     (error 'type-error
            :datum request
            :expected-type '(or list clog-http:request-context)))))

(defun invoke-route-stack (route context)
  "Invoke ROUTE middleware and handler using the `(next context)` protocol."
  (labels ((invoke (remaining current-context)
             (unless (typep current-context 'clog-http:request-context)
               (error 'type-error
                      :datum current-context
                      :expected-type 'clog-http:request-context))
             (if remaining
                 (funcall
                  (first remaining)
                  (lambda (&optional (next-context current-context))
                    (invoke (rest remaining) next-context))
                  current-context)
                 (funcall (%route-handler route) current-context))))
    (invoke (%route-middleware route) context)))

(defun plain-clack-response (status text reason &optional additional-headers)
  "Create a stable plain-text Clack response for bounded router failures."
  (clog-http:response->clack-response
   (clog-http:make-response
    :status status
    :headers (append
              (list :content-type "text/plain; charset=utf-8"
                    :x-clog-reason reason)
              additional-headers)
    :body text
    :kind :html)))

(defun allow-header-value (methods)
  "Render normalized METHODS as a deterministic HTTP Allow value."
  (with-output-to-string (stream)
    (loop for method in methods
          for first-p = t then nil
          unless first-p do (write-string ", " stream)
          do (write-string (method-token method) stream))))

(defun dispatch-route (router request)
  "Dispatch REQUEST through ROUTER and return the Clack response protocol.

REQUEST may be a normalized REQUEST-CONTEXT or a raw Clack environment. Route
parameters and the selected descriptor are attached to a cloned immutable
context. Missing paths return 404, method mismatches return 405 with Allow,
invalid percent encodings return 400, and handler failures return a redacted
500 response."
  (check-type router router)
  (handler-case
      (let ((context (ensure-request-context request)))
        (multiple-value-bind (route parameters allowed-methods)
            (find-route router
                        (clog-http:request-method context)
                        (clog-http:request-path context))
          (cond
            (route
             (handler-case
                 (clog-http:normalize-response
                  (invoke-route-stack
                   route
                   (routed-request-context context route parameters)))
               (error (cause)
                 (error 'route-handler-error
                        :reason :handler-failure
                        :route route
                        :cause cause))))
            (allowed-methods
             (error 'method-not-allowed
                    :reason :method-not-allowed
                    :path (clog-http:request-path context)
                    :allowed-methods allowed-methods))
            (t
             (error 'route-not-found
                    :reason :route-not-found
                    :path (clog-http:request-path context)
                    :name nil)))))
    (clog-http:request-error ()
      (plain-clack-response 400 "Bad Request" "invalid-request"))
    (invalid-route-method ()
      (plain-clack-response 400 "Bad Request" "invalid-method"))
    (path-decoding-error ()
      (plain-clack-response 400 "Bad Request" "path-decoding-error"))
    (method-not-allowed (condition)
      (plain-clack-response
       405
       "Method Not Allowed"
       "method-not-allowed"
       (list :allow
             (allow-header-value
              (method-not-allowed-allowed-methods condition)))))
    (route-not-found ()
      (plain-clack-response 404 "Not Found" "route-not-found"))
    (route-handler-error ()
      (plain-clack-response 500 "Internal Server Error" "route-handler-error"))
    (routing-error ()
      (plain-clack-response 500 "Internal Server Error" "routing-error"))))

(defun route-parameter-key-name (key)
  "Normalize a route URL parameter key to a lower-case string."
  (cond
    ((stringp key) (string-downcase key))
    ((keywordp key) (string-downcase (symbol-name key)))
    (t
     (error 'invalid-route-parameters :reason :invalid-parameter-key))))

(defun route-parameter-string (value)
  "Convert a bounded route URL parameter VALUE to a string."
  (cond
    ((stringp value) (copy-seq value))
    ((integerp value) (princ-to-string value))
    (t
     (error 'invalid-route-parameters :reason :invalid-parameter-value))))

(defun route-parameter-alist (parameters)
  "Validate alternating PARAMETERS and return a normalized alist."
  (unless (and (finite-proper-list-p parameters)
               (evenp (length parameters)))
    (error 'invalid-route-parameters :reason :malformed-parameter-list))
  (let ((result nil))
    (loop for (key value) on parameters by #'cddr
          for name = (route-parameter-key-name key)
          do
             (when (assoc name result :test #'string=)
               (error 'invalid-route-parameters :reason :duplicate-parameter-key))
             (push (cons name (route-parameter-string value)) result))
    (nreverse result)))

(defun unreserved-path-character-p (character)
  "Return true when CHARACTER is an RFC 3986 unreserved ASCII character."
  (and (< (char-code character) 128)
       (or (alphanumericp character)
           (find character "-._~" :test #'char=))))

(defun character-utf-8-octets (character)
  "Return CHARACTER encoded as a list of UTF-8 octets."
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f)
       (list code))
      ((<= code #x7ff)
       (list (logior #xc0 (ash code -6))
             (logior #x80 (logand code #x3f))))
      ((or (> code #x10ffff)
           (<= #xd800 code #xdfff))
       (error 'invalid-route-parameters :reason :invalid-unicode-scalar))
      ((<= code #xffff)
       (list (logior #xe0 (ash code -12))
             (logior #x80 (logand (ash code -6) #x3f))
             (logior #x80 (logand code #x3f))))
      (t
       (list (logior #xf0 (ash code -18))
             (logior #x80 (logand (ash code -12) #x3f))
             (logior #x80 (logand (ash code -6) #x3f))
             (logior #x80 (logand code #x3f)))))))

(defun encode-path-component (value)
  "Percent-encode VALUE as one UTF-8 URI path component."
  (with-output-to-string (stream)
    (loop for character across value
          if (unreserved-path-character-p character)
            do (write-char character stream)
          else
            do (dolist (octet (character-utf-8-octets character))
                 (format stream "%~2,'0X" octet)))))

(defun wildcard-path-parts (value)
  "Validate a wildcard VALUE and return its slash-separated parts."
  (when (or (find #\? value)
            (find #\# value)
            (find #\\ value)
            (some #'route-control-character-p value))
    (error 'invalid-route-parameters :reason :unsafe-wildcard-value))
  (cond
    ((string= value "") nil)
    ((or (char= (char value 0) #\/)
         (char= (char value (1- (length value))) #\/))
     (error 'invalid-route-parameters :reason :invalid-wildcard-shape))
    (t
     (let ((parts (split-path-segments (concatenate 'string "/" value))))
       (when (some (lambda (part) (zerop (length part))) parts)
         (error 'invalid-route-parameters :reason :invalid-wildcard-shape))
       parts))))

(defun route-url (router name &rest parameters)
  "Generate a URL for named route NAME from alternating PARAMETERS.

Parameter keys may be strings or keywords. Named segment values must be
non-empty strings or integers and are encoded as one path component. A final
wildcard value may contain slash-separated components."
  (check-type router router)
  (let* ((name (normalize-route-name name :lookup-p t))
         (route (gethash name (%router-routes-by-name router))))
    (unless route
      (error 'route-not-found
             :reason :unknown-route-name
             :path nil
             :name name))
    (let* ((parameters (route-parameter-alist parameters))
           (expected (%route-parameter-names route)))
      (dolist (entry parameters)
        (unless (member (car entry) expected :test #'string=)
          (error 'invalid-route-parameters :reason :unknown-parameter-key)))
      (dolist (parameter expected)
        (unless (assoc parameter parameters :test #'string=)
          (error 'invalid-route-parameters :reason :missing-parameter)))
      (let ((parts nil))
        (dolist (segment (%route-segments route))
          (case (route-segment-kind segment)
            (:static
             (push (encode-path-component (route-segment-value segment)) parts))
            (:parameter
             (let ((value (cdr (assoc (route-segment-value segment)
                                      parameters
                                      :test #'string=))))
               (when (or (zerop (length value))
                         (find #\/ value)
                         (find #\\ value)
                         (some #'route-control-character-p value))
                 (error 'invalid-route-parameters
                        :reason :invalid-segment-value))
               (push (encode-path-component value) parts)))
            (:wildcard
             (let* ((value (cdr (assoc (route-segment-value segment)
                                       parameters
                                       :test #'string=)))
                    (wildcard-parts (wildcard-path-parts value)))
               (dolist (part wildcard-parts)
                 (push (encode-path-component part) parts))))))
        (setf parts (nreverse parts))
        (if parts
            (concatenate 'string "/" (join-path-segments parts))
            "/")))))
