;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic router tests                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(def-suite hypermedia-router-suite)
(in-suite hypermedia-router-suite)

(defun router-test-context (method path)
  "Create a normalized request context for METHOD and PATH."
  (let ((env
          (handler-case
              (make-request-env :method method :path path)
            (program-error ()
              (make-request-env)))))
    (setf (getf env :request-method) method
          (getf env :path-info) path)
    (clog-http:make-request-context env)))

(defun clack-response-body-string (response)
  "Collect the bounded response body from a Clack response triple."
  (let ((body (third response)))
    (cond
      ((null body) "")
      ((stringp body) body)
      ((listp body)
       (with-output-to-string (stream)
         (dolist (chunk body)
           (when chunk
             (write-string (princ-to-string chunk) stream)))))
      (t
       (princ-to-string body)))))

(defun response-header-value (response expected-name)
  "Return EXPECTED-NAME from RESPONSE's alternating header list."
  (loop for (key value) on (second response) by #'cddr
        for key-name = (etypecase key
                         (keyword (symbol-name key))
                         (symbol (symbol-name key))
                         (string key))
        when (string-equal key-name expected-name)
          return value))

(defun route-template-for (router method path)
  "Return the selected route template for METHOD and PATH."
  (multiple-value-bind (route parameters allowed-methods)
      (clog-router:find-route router method path)
    (declare (ignore parameters allowed-methods))
    (and route (clog-router:route-template route))))

(defun make-priority-router (registration-order)
  "Build the exact-versus-parameter router in REGISTRATION-ORDER."
  (let ((router (clog-router:make-router)))
    (dolist (kind registration-order router)
      (ecase kind
        (:parameter
         (clog-router:add-route
          router :get "/users/:id"
          (lambda (context)
            (clog-http:request-path-parameter context "id"))))
        (:exact
         (clog-router:add-route
          router :get "/users/new"
          (lambda (context)
            (declare (ignore context))
            "exact")))))))

(test route-selection-is-independent-of-registration-order
  (dolist (order '((:parameter :exact) (:exact :parameter)))
    (let ((router (make-priority-router order)))
      (is (string= "/users/new"
                   (route-template-for router :get "/users/new")))
      (is (string= "/users/:id"
                   (route-template-for router :get "/users/42"))))))

(test table-driven-static-and-parameter-matching
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/health"
                           (lambda (context)
                             (declare (ignore context))
                             "healthy"))
    (clog-router:add-route router :get "/projects/:project-id"
                           (lambda (context)
                             (clog-http:request-path-parameter
                              context "project-id")))
    (clog-router:add-route router :post "/projects/:project-id/jobs/:job-id"
                           (lambda (context)
                             (declare (ignore context))
                             "created"))
    (dolist (case '((:get "/health" "/health" nil)
                    (:get "/projects/alpha" "/projects/:project-id"
                          (("project-id" . "alpha")))
                    (:post "/projects/alpha/jobs/17"
                           "/projects/:project-id/jobs/:job-id"
                           (("project-id" . "alpha")
                            ("job-id" . "17")))))
      (destructuring-bind (method path expected-template expected-parameters)
          case
        (multiple-value-bind (route parameters allowed-methods)
            (clog-router:find-route router method path)
          (is (null allowed-methods))
          (is (clog-router:route-p route))
          (is (string= expected-template
                       (clog-router:route-template route)))
          (is (equal expected-parameters parameters)))))))

(test find-route-reports-allowed-methods-in-stable-order
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :post "/documents"
                           (lambda (context)
                             (declare (ignore context))
                             "post"))
    (clog-router:add-route router :get "/documents"
                           (lambda (context)
                             (declare (ignore context))
                             "get"))
    (multiple-value-bind (route parameters allowed-methods)
        (clog-router:find-route router :delete "/documents")
      (is (null route))
      (is (null parameters))
      (is (equal '(:get :post) allowed-methods)))))

(test dispatch-returns-405-with-allow-header
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :post "/documents"
                           (lambda (context)
                             (declare (ignore context))
                             "post"))
    (clog-router:add-route router :get "/documents"
                           (lambda (context)
                             (declare (ignore context))
                             "get"))
    (let ((response
            (clog-router:dispatch-route
             router
             (router-test-context :delete "/documents"))))
      (is (= 405 (first response)))
      (is (string= "GET, POST"
                   (response-header-value response "ALLOW")))
      (is (string= "Method Not Allowed"
                   (clack-response-body-string response))))))

(test dispatch-returns-explicit-404
  (let* ((router (clog-router:make-router))
         (response
           (clog-router:dispatch-route
            router
            (router-test-context :get "/missing"))))
    (is (= 404 (first response)))
    (is (string= "route-not-found"
                 (response-header-value response "X-CLOG-REASON")))
    (is (string= "Not Found"
                 (clack-response-body-string response)))))

(test strict-path-decoding-preserves-data
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/items/:id"
                           (lambda (context)
                             (clog-http:request-path-parameter context "id")))
    (dolist (case '(("/items/%E4%B8%AD%E6%96%87" "中文")
                    ("/items/a+b" "a+b")
                    ("/items/a%2Bb" "a+b")
                    ("/items/a%20b" "a b")))
      (destructuring-bind (path expected) case
        (multiple-value-bind (route parameters allowed-methods)
            (clog-router:find-route router :get path)
          (declare (ignore route allowed-methods))
          (is (string= expected
                       (cdr (assoc "id" parameters :test #'string=)))))))))

(test invalid-path-encodings-return-400
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/items/:id"
                           (lambda (context)
                             (declare (ignore context))
                             "ok"))
    (dolist (path '("/items/%"
                    "/items/%ZZ"
                    "/items/%FF"
                    "/items/%2F"
                    "/items/%5C"
                    "/items//nested"))
      (let ((response
              (clog-router:dispatch-route
               router
               (router-test-context :get path))))
        (is (= 400 (first response)))
        (is (string= "path-decoding-error"
                     (response-header-value response "X-CLOG-REASON")))))))

(test overlapping-dynamic-routes-fail-at-registration-time
  (dolist (pair '(("/users/:id" "/users/:name")
                  ("/:area/settings" "/admin/:page")
                  ("/files/*path" "/files/:name")
                  ("/files/*path" "/files/:folder/:name")))
    (let ((router (clog-router:make-router)))
      (clog-router:add-route router :get (first pair)
                             (lambda (context)
                               (declare (ignore context))
                               "first"))
      (signals clog-router:route-conflict
        (clog-router:add-route router :get (second pair)
                               (lambda (context)
                                 (declare (ignore context))
                                 "second"))))))

(test exact-routes-may-shadow-dynamic-routes-but-duplicates-fail
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/users/:id"
                           (lambda (context)
                             (clog-http:request-path-parameter context "id")))
    (clog-router:add-route router :get "/users/new"
                           (lambda (context)
                             (declare (ignore context))
                             "new"))
    (is (string= "/users/new"
                 (route-template-for router :get "/users/new")))
    (signals clog-router:route-conflict
      (clog-router:add-route router :get "/users/new"
                             (lambda (context)
                               (declare (ignore context))
                               "duplicate")))))

(test identical-dynamic-shapes-are-allowed-for-different-methods
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/users/:id"
                           (lambda (context)
                             (declare (ignore context))
                             "get"))
    (clog-router:add-route router :post "/users/:name"
                           (lambda (context)
                             (declare (ignore context))
                             "post"))
    (is (string= "/users/:id"
                 (route-template-for router :get "/users/9")))
    (is (string= "/users/:name"
                 (route-template-for router :post "/users/9")))))

(test duplicate-route-names-fail-immediately
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/alpha"
                           (lambda (context)
                             (declare (ignore context))
                             "alpha")
                           :name :entry)
    (signals clog-router:duplicate-route-name
      (clog-router:add-route router :get "/beta"
                             (lambda (context)
                               (declare (ignore context))
                               "beta")
                             :name :entry))))

(test final-wildcard-captures-the-remainder-and-yields-to-exact-routes
  (dolist (order '((:wildcard :exact) (:exact :wildcard)))
    (let ((router (clog-router:make-router)))
      (dolist (kind order)
        (ecase kind
          (:wildcard
           (clog-router:add-route
            router :get "/assets/*path"
            (lambda (context)
              (clog-http:request-path-parameter context "path"))))
          (:exact
           (clog-router:add-route
            router :get "/assets/app.css"
            (lambda (context)
              (declare (ignore context))
              "compiled")))))
      (multiple-value-bind (route parameters allowed-methods)
          (clog-router:find-route router :get "/assets/images/logo.svg")
        (declare (ignore allowed-methods))
        (is (string= "/assets/*path"
                     (clog-router:route-template route)))
        (is (string= "images/logo.svg"
                     (cdr (assoc "path" parameters :test #'string=)))))
      (is (string= "/assets/app.css"
                   (route-template-for router :get "/assets/app.css"))))))

(test named-route-url-generation-is-strict-and-utf8-safe
  (let ((router (clog-router:make-router)))
    (clog-router:add-route router :get "/projects/:project-id"
                           (lambda (context)
                             (declare (ignore context))
                             "project")
                           :name :project)
    (clog-router:add-route router :get "/files/*path"
                           (lambda (context)
                             (declare (ignore context))
                             "file")
                           :name "file")
    (is (string= "/projects/42"
                 (clog-router:route-url router :project :project-id 42)))
    (is (string= "/projects/%E4%B8%AD%E6%96%87%20A%2BB"
                 (clog-router:route-url
                  router :project :project-id "中文 A+B")))
    (is (string= "/files/manuals/API%20Guide.pdf"
                 (clog-router:route-url
                  router "file" :path "manuals/API Guide.pdf")))
    (signals clog-router:route-not-found
      (clog-router:route-url router :missing))
    (signals clog-router:invalid-route-parameters
      (clog-router:route-url router :project))
    (signals clog-router:invalid-route-parameters
      (clog-router:route-url router :project :project-id "a/b"))
    (signals clog-router:invalid-route-parameters
      (clog-router:route-url router :project :unknown "x"
                            :project-id "42"))))

(test route-middleware-wraps-the-handler-in-registration-order
  (let ((router (clog-router:make-router))
        (events nil))
    (labels ((record (event)
               (setf events (append events (list event))))
             (outer (next context)
               (record :outer-before)
               (prog1 (funcall next context)
                 (record :outer-after)))
             (inner (next context)
               (record :inner-before)
               (prog1 (funcall next context)
                 (record :inner-after))))
      (clog-router:add-route
       router :get "/middleware"
       (lambda (context)
         (declare (ignore context))
         (record :handler)
         "ok")
       :middleware (list #'outer #'inner))
      (let ((response
              (clog-router:dispatch-route
               router
               (router-test-context :get "/middleware"))))
        (is (= 200 (first response)))
        (is (string= "ok" (clack-response-body-string response)))
        (is (equal '(:outer-before :inner-before :handler
                     :inner-after :outer-after)
                   events))))))

(test dispatch-attaches-route-and-decoded-path-parameters
  (let ((router (clog-router:make-router))
        (seen-route nil)
        (seen-parameters nil))
    (let ((registered
            (clog-router:add-route
             router :get "/teams/:team-id/members/:member-id"
             (lambda (context)
               (setf seen-route (clog-http:request-route context)
                     seen-parameters
                     (clog-http:request-path-parameters context))
               "ok")
             :name :member)))
      (let ((response
              (clog-router:dispatch-route
               router
               (router-test-context
                :get "/teams/core/members/%E4%B8%AD%E6%96%87"))))
        (is (= 200 (first response)))
        (is (eq registered seen-route))
        (is (equal '(("team-id" . "core")
                     ("member-id" . "中文"))
                   seen-parameters))))))

(test handler-errors-are-redacted-as-explicit-500-responses
  (let ((router (clog-router:make-router)))
    (clog-router:add-route
     router :get "/failure"
     (lambda (context)
       (declare (ignore context))
       (error "secret-token-should-never-leak")))
    (let* ((response
             (clog-router:dispatch-route
              router
              (router-test-context :get "/failure")))
           (body (clack-response-body-string response)))
      (is (= 500 (first response)))
      (is (string= "route-handler-error"
                   (response-header-value response "X-CLOG-REASON")))
      (is (string= "Internal Server Error" body))
      (is (null (search "secret-token" body :test #'char-equal))))))

(test invalid-route-templates-fail-before-serving-requests
  (dolist (template '("relative"
                      ""
                      "/trailing/"
                      "/double//segment"
                      "/files/*path/more"
                      "/users/:id/:id"
                      "/encoded/%2F"))
    (signals clog-router:invalid-route-template
      (clog-router:add-route
       (clog-router:make-router)
       :get template
       (lambda (context)
         (declare (ignore context))
         "never")))))

(eval-when (:load-toplevel :execute)
  (let ((previous-run-tests (symbol-function 'run-tests)))
    (setf (symbol-function 'run-tests)
          (lambda ()
            (and (funcall previous-run-tests)
                 (fiveam:results-status
                  (fiveam:run 'hypermedia-router-suite)))))))
