;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic router                          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-router
  (:export #:route
           #:route-p
           #:route-method
           #:route-path
           #:route-handler
           #:route-name
           #:route-middleware
           #:route-metadata
           #:route-parameter-names
           #:route-exact-p
           #:router
           #:router-p
           #:make-router
           #:add-route
           #:find-route
           #:dispatch-route))

(defpackage #:clog-hypermedia
  (:import-from #:clog-router
                #:route
                #:route-p
                #:route-method
                #:route-path
                #:route-handler
                #:route-name
                #:route-middleware
                #:route-metadata
                #:route-parameter-names
                #:route-exact-p
                #:router
                #:router-p
                #:make-router
                #:add-route
                #:find-route
                #:dispatch-route)
  (:export #:route
           #:route-p
           #:route-method
           #:route-path
           #:route-handler
           #:route-name
           #:route-middleware
           #:route-metadata
           #:route-parameter-names
           #:route-exact-p
           #:router
           #:router-p
           #:make-router
           #:add-route
           #:find-route
           #:dispatch-route))

(in-package #:clog-http)

(defun %request-context-with-routing (context route path-params)
  "Return a request-context clone bound to ROUTE and PATH-PARAMS.

The clone preserves middleware-owned session, user and body-stream identities
while defensively copying request metadata. It also preserves a form body that
has already been parsed, avoiding a second read from the raw body stream."
  (check-type context request-context)
  (let ((form-parameters (%request-form-parameters context)))
    (make-instance
     'request-context
     :env (snapshot-request-env (%request-env context))
     :lack-request (%request-object context)
     :request-id (copy-request-value (%request-id context))
     :session (%request-session context)
     :session-id (copy-request-value (%request-session-id context))
     :path-params (copy-parameter-alist path-params)
     :route route
     :user (%request-user context)
     :method (%request-method context)
     :path (copy-request-value (%request-path context))
     :headers (copy-headers (%request-headers context))
     :query-parameters (copy-parameter-alist
                        (%request-query-parameters context))
     :body-source (%request-body-source context)
     :body-content-length (%request-body-content-length context)
     :body-content-type (copy-request-value
                         (%request-body-content-type context))
     :body-limit-bytes (%request-body-limit-bytes context)
     :form-parameters
     (if (eq form-parameters +form-parameters-unparsed+)
         +form-parameters-unparsed+
         (copy-parameter-alist form-parameters))
     :htmx-p (%htmx-request-p context)
     :htmx-request-type (%htmx-request-type context)
     :htmx-source (copy-request-value (%htmx-request-source context))
     :htmx-target (copy-request-value (%htmx-request-target context))
     :htmx-trigger (copy-request-value (%htmx-request-trigger context))
     :current-url (copy-request-value (%request-current-url context))
     :csp-nonce (copy-request-value (%request-csp-nonce context)))))

(in-package #:clog-router)

(defparameter +route-method-order+
  '(:get :head :post :put :patch :delete :options :trace :connect)
  "Stable method order used for deterministic Allow headers.")

(defstruct (compiled-route-segment
             (:constructor %make-compiled-route-segment (kind value))
             (:conc-name %compiled-segment-))
  "One compiled route segment. KIND is :STATIC or :PARAMETER."
  kind
  value)

(defstruct (route
             (:constructor %make-route
                 (&key method
                       path
                       handler
                       effective-handler
                       name
                       middleware
                       metadata
                       parameter-names
                       compiled-segments
                       exact-p))
             (:conc-name %route-))
  "Immutable route descriptor produced by ADD-ROUTE.

Public accessors return defensive copies for mutable descriptor metadata.
Internal compiled segments and the composed handler remain private."
  method
  path
  handler
  effective-handler
  name
  middleware
  metadata
  parameter-names
  compiled-segments
  exact-p)

(defstruct (router
             (:constructor %make-router ())
             (:conc-name %router-))
  "Thread-safe startup registry for exact and parameterized routes."
  (routes nil)
  (exact-routes (make-hash-table :test 'equal))
  (parameter-routes nil)
  (route-names (make-hash-table :test 'equal))
  (lock (bordeaux-threads:make-lock "clog deterministic router")))

(defun copy-route-value (value)
  "Return a defensive recursive copy suitable for route metadata."
  (typecase value
    (string (copy-seq value))
    (cons (cons (copy-route-value (car value))
                (copy-route-value (cdr value))))
    (vector (copy-seq value))
    (t value)))

(defun copy-route-string-list (strings)
  "Return a fresh list containing fresh copies of STRINGS."
  (mapcar #'copy-seq strings))

(defun route-method (route)
  "Return ROUTE's normalized HTTP method keyword."
  (check-type route route)
  (%route-method route))

(defun route-path (route)
  "Return a defensive copy of ROUTE's path template."
  (check-type route route)
  (copy-seq (%route-path route)))

(defun route-handler (route)
  "Return the original handler function registered for ROUTE."
  (check-type route route)
  (%route-handler route))

(defun route-name (route)
  "Return a defensive copy of ROUTE's optional name."
  (check-type route route)
  (copy-route-value (%route-name route)))

(defun route-middleware (route)
  "Return a fresh list of middleware wrapper functions for ROUTE."
  (check-type route route)
  (copy-list (%route-middleware route)))

(defun route-metadata (route)
  "Return a defensive copy of ROUTE's application metadata."
  (check-type route route)
  (copy-route-value (%route-metadata route)))

(defun route-parameter-names (route)
  "Return normalized path parameter names in template order."
  (check-type route route)
  (copy-route-string-list (%route-parameter-names route)))

(defun route-exact-p (route)
  "Return true when ROUTE contains only static path segments."
  (check-type route route)
  (%route-exact-p route))

(defun make-router ()
  "Create an empty deterministic ROUTER.

Routes are expected to be registered during application construction. Registry
operations are nevertheless serialized so development-time hot reload cannot
observe a partially installed descriptor."
  (%make-router))

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

(defun normalize-route-method (method &key definition-p)
  "Normalize METHOD through the fixed HTTP vocabulary.

When DEFINITION-P is true, malformed input is a ROUTE-DEFINITION-ERROR.
Otherwise it is a request-time ROUTING-ERROR."
  (handler-case
      (clog-http::normalized-http-method method)
    (clog-http:request-error ()
      (if definition-p
          (error 'route-definition-error
                 :reason :invalid-route-method
                 :value method)
          (error 'routing-error
                 :reason :invalid-request-method)))))

(defun route-path-control-character-p (character)
  "Return true when CHARACTER is unsafe in a route or request path."
  (let ((code (char-code character)))
    (or (= code 127)
        (< code 32))))

(defun validate-absolute-path (path &key definition-p)
  "Validate PATH as a path-info style absolute path and return it.

Query strings and fragments are rejected because Clack exposes them separately.
Backslashes and control characters fail closed to avoid proxy/browser path
interpretation differences."
  (flet ((fail (reason)
           (if definition-p
               (error 'route-definition-error :reason reason :value path)
               (error 'path-decoding-error
                      :reason reason
                      :path path
                      :parameter nil
                      :cause nil))))
    (unless (stringp path)
      (fail (if definition-p :route-path-must-be-string
                :request-path-must-be-string)))
    (unless (and (plusp (length path))
                 (char= (char path 0) #\/))
      (fail (if definition-p :route-path-must-be-absolute
                :request-path-must-be-absolute)))
    (when (or (position #\? path)
              (position #\# path))
      (fail (if definition-p :route-path-must-not-contain-query-or-fragment
                :request-path-must-not-contain-query-or-fragment)))
    (when (position #\\ path)
      (fail (if definition-p :route-path-must-not-contain-backslash
                :request-path-must-not-contain-backslash)))
    (when (some #'route-path-control-character-p path)
      (fail (if definition-p :route-path-contains-control-character
                :request-path-contains-control-character))))
  path)

(defun split-route-path (path)
  "Split validated absolute PATH into raw segments, preserving empty segments.

The root path `/` has no segments. A trailing slash is represented by a final
empty static segment, so `/items` and `/items/` remain distinct."
  (if (string= path "/")
      nil
      (let ((segments nil)
            (start 1)
            (path-length (length path)))
        (loop
          for slash = (position #\/ path :start start)
          do (if slash
                 (progn
                   (push (subseq path start slash) segments)
                   (setf start (1+ slash)))
                 (progn
                   (push (subseq path start path-length) segments)
                   (return (nreverse segments))))))))

(defun ascii-route-name-character-p (character)
  "Return true for the bounded named-segment identifier vocabulary."
  (or (and (char<= #\a character #\z))
      (and (char<= #\A character #\Z))
      (and (char<= #\0 character #\9))
      (char= character #\-)
      (char= character #\_)))

(defun valid-route-parameter-name-p (name)
  "Return true for a non-empty ASCII route parameter name."
  (and (stringp name)
       (plusp (length name))
       (every #'ascii-route-name-character-p name)))

(defun compile-route-template (path)
  "Compile PATH into segment descriptors and normalized parameter names."
  (validate-absolute-path path :definition-p t)
  (let ((compiled nil)
        (parameter-names nil))
    (dolist (segment (split-route-path path))
      (cond
        ((and (plusp (length segment))
              (char= (char segment 0) #\:))
         (let ((name (string-downcase (subseq segment 1))))
           (unless (valid-route-parameter-name-p name)
             (error 'route-definition-error
                    :reason :invalid-route-parameter-name
                    :value path))
           (when (member name parameter-names :test #'string=)
             (error 'route-definition-error
                    :reason :duplicate-route-parameter-name
                    :value path))
           (push name parameter-names)
           (push (%make-compiled-route-segment :parameter name) compiled)))
        ((and (plusp (length segment))
              (char= (char segment 0) #\*))
         (error 'route-definition-error
                :reason :wildcard-routes-not-enabled
                :value path))
        (t
         (push (%make-compiled-route-segment
                :static
                (copy-seq segment))
               compiled))))
    (values (nreverse compiled)
            (nreverse parameter-names))))

(defun normalize-route-name (name)
  "Validate and defensively copy optional route NAME."
  (cond
    ((null name) nil)
    ((stringp name)
     (if (plusp (length name))
         (copy-seq name)
         (error 'route-definition-error
                :reason :empty-route-name
                :value name)))
    ((symbolp name) name)
    (t
     (error 'route-definition-error
            :reason :invalid-route-name
            :value name))))

(defun normalize-route-middleware (middleware)
  "Normalize middleware to a finite proper list of wrapper functions."
  (let ((wrappers
          (cond
            ((null middleware) nil)
            ((functionp middleware) (list middleware))
            ((finite-proper-list-p middleware) (copy-list middleware))
            (t
             (error 'route-definition-error
                    :reason :invalid-route-middleware
                    :value middleware)))))
    (unless (every #'functionp wrappers)
      (error 'route-definition-error
             :reason :invalid-route-middleware
             :value middleware))
    wrappers))

(defun compose-route-handler (handler middleware)
  "Compose HANDLER with MIDDLEWARE, first wrapper outermost.

Each wrapper is called once at route-registration time with the next handler
and must return a function accepting one request context."
  (let ((effective handler))
    (dolist (wrapper (reverse middleware) effective)
      (handler-case
          (let ((wrapped (funcall wrapper effective)))
            (unless (functionp wrapped)
              (error 'route-definition-error
                     :reason :middleware-did-not-return-handler
                     :value wrapper))
            (setf effective wrapped))
        (route-definition-error (condition)
          (error condition))
        (error ()
          (error 'route-definition-error
                 :reason :middleware-composition-failed
                 :value wrapper))))))

(defun compiled-segments-overlap-p (left right)
  "Return true when LEFT and RIGHT templates can match the same raw path."
  (and (= (length left) (length right))
       (every
        (lambda (left-segment right-segment)
          (let ((left-kind (%compiled-segment-kind left-segment))
                (right-kind (%compiled-segment-kind right-segment))
                (left-value (%compiled-segment-value left-segment))
                (right-value (%compiled-segment-value right-segment)))
            (cond
              ((and (eq left-kind :static)
                    (eq right-kind :static))
               (string= left-value right-value))
              ((eq left-kind :static)
               (plusp (length left-value)))
              ((eq right-kind :static)
               (plusp (length right-value)))
              (t t))))
        left
        right)))

(defun route-registration-conflict (router method path compiled exact-p)
  "Return the existing conflicting route in ROUTER, or NIL."
  (if exact-p
      (gethash (list method path) (%router-exact-routes router))
      (find-if
       (lambda (route)
         (and (eq method (%route-method route))
              (compiled-segments-overlap-p
               compiled
               (%route-compiled-segments route))))
       (%router-parameter-routes router))))

(defun route-name-key (name)
  "Return a stable hash key for route NAME."
  (copy-route-value name))

(defun add-route (router method path handler
                  &key name middleware metadata)
  "Compile and register one route, returning its ROUTE descriptor.

METHOD accepts the fixed request-method keyword/string vocabulary. PATH supports
static segments and named segments written as `:name`; arbitrary regex,
wildcard and optional paths are deliberately outside HM-012. Exact paths always
take precedence over parameter paths. Two parameterized templates for the same
method are rejected when any request path could match both, making lookup
independent of registration order.

MIDDLEWARE is NIL, one wrapper function, or a proper list of wrappers. A wrapper
receives the next handler at registration time and returns a one-argument
handler. Conflicts and malformed definitions fail immediately during
application construction."
  (check-type router router)
  (unless (functionp handler)
    (error 'route-definition-error
           :reason :route-handler-must-be-function
           :value handler))
  (let* ((method (normalize-route-method method :definition-p t))
         (path (copy-seq (validate-absolute-path path :definition-p t)))
         (name (normalize-route-name name))
         (middleware (normalize-route-middleware middleware)))
    (multiple-value-bind (compiled parameter-names)
        (compile-route-template path)
      (let ((exact-p (null parameter-names)))
        (bordeaux-threads:with-lock-held ((%router-lock router))
          (let ((conflict
                  (route-registration-conflict
                   router method path compiled exact-p)))
            (when conflict
              (error 'route-conflict
                     :reason :overlapping-route
                     :method method
                     :template (copy-seq path)
                     :existing-template
                     (copy-seq (%route-path conflict)))))
          (when name
            (let ((existing
                    (gethash (route-name-key name)
                             (%router-route-names router))))
              (when existing
                (error 'route-conflict
                       :reason :duplicate-route-name
                       :method method
                       :template (copy-seq path)
                       :existing-template
                       (copy-seq (%route-path existing))))))
          (let* ((effective-handler
                   (compose-route-handler handler middleware))
                 (route
                   (%make-route
                    :method method
                    :path path
                    :handler handler
                    :effective-handler effective-handler
                    :name name
                    :middleware middleware
                    :metadata (copy-route-value metadata)
                    :parameter-names
                    (copy-route-string-list parameter-names)
                    :compiled-segments compiled
                    :exact-p exact-p)))
            (if exact-p
                (setf (gethash (list method path)
                               (%router-exact-routes router))
                      route)
                (push route (%router-parameter-routes router)))
            (when name
              (setf (gethash (route-name-key name)
                             (%router-route-names router))
                    route))
            (push route (%router-routes router))
            route))))))

(defun raw-route-match-p (route path segments)
  "Return true when ROUTE structurally matches raw PATH and SEGMENTS."
  (if (%route-exact-p route)
      (string= (%route-path route) path)
      (let ((compiled (%route-compiled-segments route)))
        (and (= (length compiled) (length segments))
             (every
              (lambda (compiled-segment request-segment)
                (case (%compiled-segment-kind compiled-segment)
                  (:static
                   (string= (%compiled-segment-value compiled-segment)
                            request-segment))
                  (:parameter
                   (plusp (length request-segment)))
                  (otherwise nil)))
              compiled
              segments)))))

(defun decode-percent-run (segment start end)
  "Decode a validated consecutive `%HH` run from SEGMENT as strict UTF-8."
  (quri:url-decode segment
                   :encoding :utf-8
                   :start start
                   :end end
                   :lenient nil))

(defun hex-digit-character-p (character)
  "Return true when CHARACTER is an ASCII hexadecimal digit."
  (or (and (char<= #\0 character #\9))
      (and (char<= #\a character #\f))
      (and (char<= #\A character #\F))))

(defun decode-path-parameter (raw path parameter)
  "Strictly percent-decode one path parameter while preserving literal `+`.

QURI's general URL decoder follows form semantics for `+`, so only consecutive
percent-encoded runs are delegated to it. Raw Unicode and literal plus
characters are copied unchanged."
  (handler-case
      (with-output-to-string (stream)
        (let ((index 0)
              (length (length raw)))
          (loop while (< index length)
                for character = (char raw index)
                do (if (char= character #\%)
                       (let ((start index))
                         (loop while (and (< index length)
                                          (char= (char raw index) #\%))
                               do (unless (and (< (+ index 2) length)
                                               (hex-digit-character-p
                                                (char raw (1+ index)))
                                               (hex-digit-character-p
                                                (char raw (+ index 2))))
                                    (error "Malformed percent encoding."))
                                  (incf index 3))
                         (write-string
                          (decode-percent-run raw start index)
                          stream))
                       (progn
                         (write-char character stream)
                         (incf index))))))
    (error (cause)
      (error 'path-decoding-error
             :reason :invalid-percent-or-utf8-encoding
             :path (copy-seq path)
             :parameter (and parameter (copy-seq parameter))
             :cause cause))))

(defun match-parameter-route (route path segments)
  "Return decoded path parameter alist when ROUTE matches, otherwise NIL."
  (when (raw-route-match-p route path segments)
    (loop for compiled-segment in (%route-compiled-segments route)
          for raw-segment in segments
          when (eq (%compiled-segment-kind compiled-segment) :parameter)
            collect
            (cons (copy-seq (%compiled-segment-value compiled-segment))
                  (decode-path-parameter
                   raw-segment
                   path
                   (%compiled-segment-value compiled-segment))))))

(defun route-for-method (router method path segments)
  "Return matching route and decoded path parameters for METHOD."
  (let ((exact
          (gethash (list method path)
                   (%router-exact-routes router))))
    (if exact
        (values exact nil)
        (let ((match
                (find-if
                 (lambda (route)
                   (and (eq method (%route-method route))
                        (raw-route-match-p route path segments)))
                 (%router-parameter-routes router))))
          (if match
              (values match
                      (match-parameter-route match path segments))
              (values nil nil))))))

(defun method-rank (method)
  "Return METHOD's index in the frozen Allow order."
  (or (position method +route-method-order+ :test #'eq)
      most-positive-fixnum))

(defun allowed-methods-for-path (router path segments)
  "Return methods with a route structurally matching PATH in stable order."
  (sort
   (remove-duplicates
    (loop for route in (%router-routes router)
          when (raw-route-match-p route path segments)
            collect (%route-method route))
    :test #'eq)
   #'<
   :key #'method-rank))

(defun find-route (router method path)
  "Find METHOD and PATH in ROUTER.

Return two values: the immutable ROUTE descriptor and a fresh alist of decoded
path parameters. Exact routes are considered before parameter routes. If PATH
exists for other methods, signal METHOD-NOT-ALLOWED with a deterministic method
list; otherwise signal ROUTE-NOT-FOUND. Malformed percent encoding or UTF-8 in
a selected named parameter signals PATH-DECODING-ERROR."
  (check-type router router)
  (let* ((method (normalize-route-method method))
         (path (copy-seq (validate-absolute-path path)))
         (segments (split-route-path path)))
    (bordeaux-threads:with-lock-held ((%router-lock router))
      (multiple-value-bind (route parameters)
          (route-for-method router method path segments)
        (when route
          (return-from find-route
            (values route
                    (mapcar (lambda (entry)
                              (cons (copy-seq (car entry))
                                    (copy-seq (cdr entry))))
                            parameters))))
        (let ((allowed-methods
                (allowed-methods-for-path router path segments)))
          (if allowed-methods
              (error 'method-not-allowed
                     :reason :method-not-allowed
                     :method method
                     :path path
                     :allowed-methods allowed-methods)
              (error 'route-not-found
                     :reason :route-not-found
                     :method method
                     :path path)))))))

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

(defun allow-header-value (methods)
  "Encode METHODS as a deterministic HTTP Allow header value."
  (format nil "~{~A~^, ~}" (mapcar #'method-token methods)))

(defun default-routing-condition-response (condition context)
  "Map a routing condition to a redacted typed HTTP response."
  (declare (ignore context))
  (typecase condition
    (path-decoding-error
     (clog-http:html-response "Bad Request" :status 400))
    (route-not-found
     (clog-http:html-response "Not Found" :status 404))
    (method-not-allowed
     (clog-http:html-response
      "Method Not Allowed"
      :status 405
      :headers
      (list :allow
            (allow-header-value
             (method-not-allowed-allowed-methods condition)))))
    (route-handler-error
     (clog-http:html-response "Internal Server Error" :status 500))
    (routing-error
     (clog-http:html-response "Bad Request" :status 400))
    (t
     (clog-http:html-response "Internal Server Error" :status 500))))

(defun handle-routing-condition (condition context condition-handler)
  "Normalize CONDITION through CONDITION-HANDLER or re-signal it."
  (if condition-handler
      (clog-http:normalize-response
       (funcall condition-handler condition context))
      (error condition)))

(defun invoke-route-handler (route context)
  "Invoke ROUTE's composed handler and normalize its return value."
  (handler-case
      (clog-http:normalize-response
       (funcall (%route-effective-handler route) context))
    (routing-error (condition)
      (error condition))
    (error (cause)
      (error 'route-handler-error
             :reason :route-handler-condition
             :route route
             :cause cause))))

(defun dispatch-route (router context
                       &key
                         (condition-handler
                           #'default-routing-condition-response))
  "Dispatch request CONTEXT through ROUTER and return a Clack response.

The selected handler receives a cloned request context whose REQUEST-ROUTE and
PATH-PARAM accessors expose the immutable route descriptor and decoded
parameters. Handler return values pass through NORMALIZE-RESPONSE.

By default, typed routing failures become redacted 400, 404, 405 or 500
responses. Supplying CONDITION-HANDLER customizes that boundary; it receives
the condition and original request context. Supplying NIL re-signals the typed
condition, which allows HM-013's application-level development error boundary
to retain debugger behavior."
  (check-type router router)
  (check-type context clog-http:request-context)
  (handler-case
      (multiple-value-bind (route path-params)
          (find-route router
                      (clog-http:request-method context)
                      (clog-http:request-path context))
        (invoke-route-handler
         route
         (clog-http::%request-context-with-routing
          context route path-params)))
    (routing-error (condition)
      (handle-routing-condition condition context condition-handler))))

(setf (documentation 'route-p 'function)
      "Return true when an object is a ROUTE descriptor.")
(setf (documentation 'router-p 'function)
      "Return true when an object is a ROUTER registry.")
