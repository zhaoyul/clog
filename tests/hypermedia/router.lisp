;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic router tests                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun router-clack-header-values (clack-response name)
  "Return every NAME value from a normal Clack response."
  (loop for (key value) on (second clack-response) by #'cddr
        when (eq key name)
          collect value))

(defun router-clack-header (clack-response name &optional default)
  "Return the first NAME header from CLACK-RESPONSE, or DEFAULT."
  (let ((values (router-clack-header-values clack-response name)))
    (if values (first values) default)))

(defun capture-condition-of-type (type thunk)
  "Run THUNK and return the signaled condition of TYPE, or NIL."
  (handler-case
      (progn (funcall thunk) nil)
    (condition (condition)
      (if (typep condition type)
          condition
          (error condition)))))

(defun package-symbol-count (package-designator)
  "Return the number of symbols accessible in PACKAGE-DESIGNATOR."
  (let ((count 0))
    (do-symbols (symbol (find-package package-designator) count)
      (declare (ignore symbol))
      (incf count))))

(test router/table-driven-static-and-parameter-matching
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/" handler :name :root)
    (clog-hypermedia:add-route router "GET" "/health" handler :name :health)
    (clog-hypermedia:add-route
     router :get "/users/:user-id" handler :name :user-show)
    (dolist (case '(("GET" "/" :root nil)
                    (:get "/health" :health nil)
                    (:get "/users/42" :user-show (("user-id" . "42")))
                    (:get "/users/alpha-beta" :user-show
                          (("user-id" . "alpha-beta")))))
      (destructuring-bind (method path expected-name expected-parameters) case
        (multiple-value-bind (route parameters)
            (clog-hypermedia:find-route router method path)
          (is (eql expected-name (clog-hypermedia:route-name route)))
          (is (equal expected-parameters parameters)))))))

(test router/exact-route-precedes-parameter-route-independent-of-registration-order
  (flet ((exercise (exact-first-p)
           (let* ((router (clog-hypermedia:make-router))
                  (exact-handler
                    (lambda (context)
                      (declare (ignore context))
                      "exact"))
                  (parameter-handler
                    (lambda (context)
                      (declare (ignore context))
                      "parameter")))
             (flet ((register-exact ()
                      (clog-hypermedia:add-route
                       router :get "/machines/new" exact-handler
                       :name :machine-new))
                    (register-parameter ()
                      (clog-hypermedia:add-route
                       router :get "/machines/:machine-id" parameter-handler
                       :name :machine-show)))
               (if exact-first-p
                   (progn (register-exact) (register-parameter))
                   (progn (register-parameter) (register-exact))))
             (multiple-value-bind (route parameters)
                 (clog-hypermedia:find-route router :get "/machines/new")
               (is (eq exact-handler
                       (clog-hypermedia:route-handler route)))
               (is (eql :machine-new
                        (clog-hypermedia:route-name route)))
               (is (null parameters)))
             (multiple-value-bind (route parameters)
                 (clog-hypermedia:find-route router :get "/machines/42")
               (is (eq parameter-handler
                       (clog-hypermedia:route-handler route)))
               (is (equal '(("machine-id" . "42")) parameters))))))
    (exercise t)
    (exercise nil)))

(test router/parameter-routes-with-overlapping-match-space-fail-at-registration
  (let ((handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (let ((router (clog-hypermedia:make-router)))
      (clog-hypermedia:add-route router :get "/users/:id" handler)
      (signals clog-hypermedia:route-conflict
        (clog-hypermedia:add-route router :get "/users/:name" handler)))
    (let ((router (clog-hypermedia:make-router)))
      (clog-hypermedia:add-route router :get "/users/:id" handler)
      (signals clog-hypermedia:route-conflict
        (clog-hypermedia:add-route router :get "/:collection/new" handler)))
    (let ((router (clog-hypermedia:make-router)))
      (clog-hypermedia:add-route router :get "/users/:id" handler)
      (signals clog-hypermedia:route-conflict
        (clog-hypermedia:add-route router :get "/users/:id" handler)))
    (let ((router (clog-hypermedia:make-router)))
      (clog-hypermedia:add-route router :get "/users/new" handler)
      (is (clog-hypermedia:route-p
           (clog-hypermedia:add-route
            router :get "/users/:id" handler))))))

(test router/exact-duplicates-and-duplicate-names-fail-at-registration
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route
     router :get "/health" handler :name :health)
    (signals clog-hypermedia:route-conflict
      (clog-hypermedia:add-route router :get "/health" handler))
    (signals clog-hypermedia:route-conflict
      (clog-hypermedia:add-route
       router :post "/different" handler :name :health))))

(test router/same-template-may-be-registered-for-different-methods
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/items/:id" handler)
    (clog-hypermedia:add-route router :post "/items/:id" handler)
    (multiple-value-bind (route parameters)
        (clog-hypermedia:find-route router :post "/items/7")
      (is (eq :post (clog-hypermedia:route-method route)))
      (is (equal '(("id" . "7")) parameters)))))

(test router/invalid-definitions-fail-before-first-request
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (dolist (path '("relative"
                    "/with?query=yes"
                    "/with#fragment"
                    "/users/:"
                    "/users/:bad$name"
                    "/users/:id/:ID"
                    "/files/*path"))
      (signals clog-hypermedia:route-definition-error
        (clog-hypermedia:add-route router :get path handler)))
    (signals clog-hypermedia:route-definition-error
      (clog-hypermedia:add-route router :unknown "/x" handler))
    (signals clog-hypermedia:route-definition-error
      (clog-hypermedia:add-route router :get "/x" 42))
    (signals clog-hypermedia:route-definition-error
      (clog-hypermedia:add-route router :get "/x" handler :name ""))
    (signals clog-hypermedia:route-definition-error
      (clog-hypermedia:add-route
       router :get "/x" handler :middleware '(identity . identity)))))

(test router/route-descriptor-defensively-copies-mutable-definition-values
  (let* ((router (clog-hypermedia:make-router))
         (path (copy-seq "/users/:id"))
         (name (copy-seq "user-show"))
         (metadata (list :label (copy-seq "safe")))
         (route
           (clog-hypermedia:add-route
            router :get path
            (lambda (context)
              (declare (ignore context))
              "ok")
            :name name
            :metadata metadata)))
    (setf (char path 1) #\X)
    (setf (char name 0) #\X)
    (setf (char (getf metadata :label) 0) #\X)
    (is (string= "/users/:id" (clog-hypermedia:route-path route)))
    (is (string= "user-show" (clog-hypermedia:route-name route)))
    (is (string= "safe"
                 (getf (clog-hypermedia:route-metadata route) :label)))
    (let ((returned-path (clog-hypermedia:route-path route))
          (returned-name (clog-hypermedia:route-name route))
          (returned-metadata (clog-hypermedia:route-metadata route)))
      (setf (char returned-path 1) #\Y)
      (setf (char returned-name 0) #\Y)
      (setf (char (getf returned-metadata :label) 0) #\Y))
    (is (string= "/users/:id" (clog-hypermedia:route-path route)))
    (is (string= "user-show" (clog-hypermedia:route-name route)))
    (is (string= "safe"
                 (getf (clog-hypermedia:route-metadata route) :label)))))

(test router/url-decoding-is-strict-utf8-and-preserves-literal-plus
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/users/:id" handler)
    (dolist (case '(("/users/hello%20world" . "hello world")
                    ("/users/%E4%BD%A0%E5%A5%BD" . "你好")
                    ("/users/A+%42" . "A+B")
                    ("/users/原始字符" . "原始字符")))
      (multiple-value-bind (route parameters)
          (clog-hypermedia:find-route router :get (car case))
        (declare (ignore route))
        (is (string= (cdr case) (cdr (first parameters))))))
    (dolist (path '("/users/%"
                    "/users/%2"
                    "/users/%ZZ"
                    "/users/%FF"))
      (signals clog-hypermedia:path-decoding-error
        (clog-hypermedia:find-route router :get path)))))

(test router/empty-segment-does-not-match-a-named-parameter
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/users/:id" handler)
    (signals clog-hypermedia:route-not-found
      (clog-hypermedia:find-route router :get "/users/"))))

(test router/runtime-path-values-do-not-grow-the-router-symbol-table
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/objects/:external-name" handler)
    (let ((before (package-symbol-count "CLOG-ROUTER")))
      (dotimes (index 100)
        (multiple-value-bind (route parameters)
            (clog-hypermedia:find-route
             router :get (format nil "/objects/runtime-value-~D" index))
          (declare (ignore route parameters))))
      (is (= before (package-symbol-count "CLOG-ROUTER"))))))

(test router/404-and-405-conditions-are-distinct-and-allow-is-deterministic
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    ;; Deliberately register in non-Allow order.
    (clog-hypermedia:add-route router :post "/items/:id" handler)
    (clog-hypermedia:add-route router :get "/items/:id" handler)
    (let ((condition
            (capture-condition-of-type
             'clog-hypermedia:method-not-allowed
             (lambda ()
               (clog-hypermedia:find-route
                router :delete "/items/42")))))
      (is (not (null condition)))
      (is (eq :delete
              (clog-hypermedia:method-not-allowed-method condition)))
      (is (equal '(:get :post)
                 (clog-hypermedia:method-not-allowed-allowed-methods
                  condition))))
    (signals clog-hypermedia:route-not-found
      (clog-hypermedia:find-route router :get "/missing"))))

(test router/dispatch-binds-route-and-decoded-parameters
  (let ((router (clog-hypermedia:make-router))
        (seen-route nil)
        (seen-id nil))
    (clog-hypermedia:add-route
     router :get "/users/:user-id"
     (lambda (context)
       (setf seen-route (clog-hypermedia:request-route context))
       (setf seen-id (clog-hypermedia:path-param context "user-id"))
       (format nil "user=~A" seen-id))
     :name :user-show)
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env
               :method :get
               :path "/users/hello%20world")))
           (clack
             (clog-hypermedia:dispatch-route router context)))
      (is (= 200 (first clack)))
      (is (string= "user=hello world" (third clack)))
      (is (clog-hypermedia:route-p seen-route))
      (is (eql :user-show
               (clog-hypermedia:route-name seen-route)))
      (is (string= "hello world" seen-id)))))

(test router/dispatch-preserves-an-already-parsed-form-body
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :post "/submit/:id"
     (lambda (context)
       (format nil "~A:~A"
               (clog-hypermedia:path-param context "id")
               (clog-hypermedia:form-param context "value"))))
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env
               :method :post
               :path "/submit/7"
               :body "value=kept"
               :content-type "application/x-www-form-urlencoded")))
           (first-read
             (clog-hypermedia:form-param context "value"))
           (clack
             (clog-hypermedia:dispatch-route router context)))
      (is (string= "kept" first-read))
      (is (string= "7:kept" (third clack))))))

(test router/middleware-is-composed-once-in-declared-order
  (let ((router (clog-hypermedia:make-router))
        (events nil)
        (composition-count 0))
    (flet ((wrapper (name)
             (lambda (next)
               (incf composition-count)
               (lambda (context)
                 (push (list name :before) events)
                 (prog1 (funcall next context)
                   (push (list name :after) events))))))
      (clog-hypermedia:add-route
       router :get "/wrapped"
       (lambda (context)
         (declare (ignore context))
         (push '(:handler) events)
         "ok")
       :middleware (list (wrapper :outer)
                         (wrapper :inner))))
    (is (= 2 composition-count))
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/wrapped")))
           (clack (clog-hypermedia:dispatch-route router context)))
      (is (= 200 (first clack)))
      (is (equal '((:outer :before)
                   (:inner :before)
                   (:handler)
                   (:inner :after)
                   (:outer :after))
                 (nreverse events))))
    (is (= 2 composition-count))))

(test router/dispatch-produces-explicit-404-405-and-400-responses
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/items/:id" handler)
    (let ((not-found
            (clog-hypermedia:dispatch-route
             router
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/missing")))))
      (is (= 404 (first not-found)))
      (is (string= "Not Found" (third not-found))))
    (let ((method-not-allowed
            (clog-hypermedia:dispatch-route
             router
             (clog-hypermedia:make-request-context
              (make-request-env :method :post :path "/items/1")))))
      (is (= 405 (first method-not-allowed)))
      (is (string= "GET"
                   (router-clack-header method-not-allowed :allow))))
    (let ((bad-request
            (clog-hypermedia:dispatch-route
             router
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/items/%ZZ")))))
      (is (= 400 (first bad-request)))
      (is (string= "Bad Request" (third bad-request))))))

(test router/handler-condition-is-redacted-and-can-be-resignaled
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :get "/explode"
     (lambda (context)
       (declare (ignore context))
       (error "secret-handler-detail")))
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/explode")))
           (clack
             (clog-hypermedia:dispatch-route router context)))
      (is (= 500 (first clack)))
      (is (string= "Internal Server Error" (third clack)))
      (is-false (search "secret-handler-detail" (third clack))))
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/explode")))
           (condition
             (capture-condition-of-type
              'clog-hypermedia:route-handler-error
              (lambda ()
                (clog-hypermedia:dispatch-route
                 router context :condition-handler nil)))))
      (is (not (null condition)))
      (is (typep (clog-hypermedia:route-handler-error-cause condition)
                 'simple-error))
      (is-false
       (search "secret-handler-detail"
               (princ-to-string condition))))))

(test router/custom-condition-handler-can-own-the-application-boundary
  (let ((router (clog-hypermedia:make-router))
        (seen-condition nil))
    (let* ((context
             (clog-hypermedia:make-request-context
              (make-request-env :method :get :path "/missing")))
           (clack
             (clog-hypermedia:dispatch-route
              router
              context
              :condition-handler
              (lambda (condition original-context)
                (declare (ignore original-context))
                (setf seen-condition condition)
                (clog-hypermedia:html-response
                 "custom"
                 :status 418)))))
      (is (= 418 (first clack)))
      (is (string= "custom" (third clack)))
      (is (typep seen-condition
                 'clog-hypermedia:route-not-found)))))

(test router/condition-reports-do-not-print-request-path-or-decoder-cause
  (let ((router (clog-hypermedia:make-router))
        (handler (lambda (context)
                   (declare (ignore context))
                   "ok")))
    (clog-hypermedia:add-route router :get "/secret/:value" handler)
    (let ((condition
            (capture-condition-of-type
             'clog-hypermedia:path-decoding-error
             (lambda ()
               (clog-hypermedia:find-route
                router :get "/secret/%ZZ-private")))))
      (is (not (null condition)))
      (is-false
       (search "%ZZ-private" (princ-to-string condition))))))
