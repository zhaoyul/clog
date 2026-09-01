;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime application pipeline tests                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun deterministic-token-generator (value)
  "Return a generator producing a fresh copy of VALUE."
  (lambda () (copy-seq value)))

(defun response-body-text (response)
  "Return a convenient text view of a normal Clack RESPONSE body."
  (let ((body (third response)))
    (cond
      ((null body) "")
      ((stringp body) body)
      ((and (listp body) (every #'stringp body))
       (format nil "~{~A~}" body))
      (t (princ-to-string body)))))

(defun cookie-pair-from-response (response)
  "Return the first Set-Cookie name/value pair from RESPONSE."
  (let ((cookie (clack-response-header response :set-cookie)))
    (when cookie
      (subseq cookie 0 (or (position #\; cookie) (length cookie))))))

(defun make-test-configuration (&rest arguments)
  "Create a deterministic test configuration, forwarding ARGUMENTS."
  (apply #'clog-hypermedia:make-hypermedia-configuration
         :request-id-generator (deterministic-token-generator "request-test")
         :csp-nonce-generator (deterministic-token-generator "nonce-test")
         arguments))

(defun make-test-application (router &rest configuration-arguments)
  "Create a Hypermedia application using ROUTER and deterministic tokens."
  (clog-hypermedia:make-hypermedia-application
   :name "hm013-test"
   :router router
   :configuration (apply #'make-test-configuration configuration-arguments)))

(test application/configuration-is-validated-and-defensive
  (let* ((prefix (copy-seq "/assets/"))
         (configuration
           (make-test-configuration
            :static-prefix prefix
            :static-root (asdf:system-relative-pathname :clog "static-files/"))))
    (setf (char prefix 1) #\X)
    (is (string= "/assets/"
                 (clog-hypermedia:configuration-static-prefix configuration)))
    (let ((returned
            (clog-hypermedia:configuration-static-prefix configuration)))
      (setf (char returned 1) #\Y)
      (is (string= "/assets/"
                   (clog-hypermedia:configuration-static-prefix configuration))))
    (is (= 1048576
           (clog-hypermedia:configuration-request-body-limit-bytes
            configuration)))
    (is-true
     (clog-hypermedia:configuration-strict-csp-p configuration)))
  (signals clog-hypermedia:hypermedia-configuration-error
    (clog-hypermedia:make-hypermedia-configuration
     :static-prefix "/assets/"
     :static-root nil))
  (signals clog-hypermedia:hypermedia-configuration-error
    (clog-hypermedia:make-hypermedia-configuration
     :request-body-limit-bytes 0)))

(test application/request-context-and-security-headers
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :get "/hello"
     (lambda (context)
       (is (string= "request-test" (clog-hypermedia:request-id context)))
       (is (string= "nonce-test" (clog-hypermedia:request-csp-nonce context)))
       (is (eq :authenticated (clog-hypermedia:request-user context)))
       (is (hash-table-p (clog-hypermedia:request-session context)))
       (clog-hypermedia:html-response "hello")))
    (let* ((application
             (clog-hypermedia:make-hypermedia-application
              :router router
              :configuration
              (make-test-configuration
               :static-prefix nil
               :static-root nil
               :authentication-hook
               (lambda (env)
                 (is (hash-table-p (getf env :lack.session)))
                 :authenticated))))
           (response
             (funcall (clog-hypermedia:as-clack-app application)
                      (make-request-env :method :get :path "/hello"))))
      (is (= 200 (first response)))
      (is (string= "hello" (response-body-text response)))
      (is (string= "request-test"
                   (clack-response-header response :x-request-id)))
      (is (string= "nosniff"
                   (clack-response-header response :x-content-type-options)))
      (is (string= "strict-origin-when-cross-origin"
                   (clack-response-header response :referrer-policy)))
      (is (string= "SAMEORIGIN"
                   (clack-response-header response :x-frame-options)))
      (is (string= "no-store"
                   (clack-response-header response :cache-control)))
      (is (search "HX-Request"
                  (clack-response-header response :vary)))
      (is (search "'nonce-nonce-test'"
                  (clack-response-header response
                                         :content-security-policy))))))

(test application/csp-nonce-is-one-request-capability
  (let ((router (clog-hypermedia:make-router))
        (nonce-calls 0)
        (observed-nonce nil))
    (clog-hypermedia:add-route
     router :get "/nonce"
     (lambda (context)
       (setf observed-nonce
             (clog-hypermedia:request-csp-nonce context))
       (clog-hypermedia:html-response "ok")))
    (let* ((application
             (clog-hypermedia:make-hypermedia-application
              :router router
              :configuration
              (clog-hypermedia:make-hypermedia-configuration
               :static-prefix nil
               :static-root nil
               :request-id-generator
               (deterministic-token-generator "request-nonce-test")
               :csp-nonce-generator
               (lambda ()
                 (format nil "nonce-call-~D" (incf nonce-calls))))))
           (response
             (funcall (clog-hypermedia:application-handler application)
                      (make-request-env :method :get :path "/nonce")))
           (csp
             (clack-response-header response :content-security-policy)))
      (is (= 1 nonce-calls))
      (is (string= "nonce-call-1" observed-nonce))
      (is (search "'nonce-nonce-call-1'" csp))
      (is (null (search "nonce-call-2" csp))))))

(test application/session-persists-through-lack-middleware
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :get "/session"
     (lambda (context)
       (let* ((session (clog-hypermedia:request-session context))
              (next (1+ (gethash "count" session 0))))
         (setf (gethash "count" session) next)
         (clog-hypermedia:html-response (format nil "~D" next)))))
    (let* ((application
             (make-test-application
              router :static-prefix nil :static-root nil))
           (handler (clog-hypermedia:application-handler application))
           (first
             (funcall handler
                      (make-request-env :method :get :path "/session")))
           (cookie (cookie-pair-from-response first)))
      (is (string= "1" (response-body-text first)))
      (is (and cookie (search "lack.session=" cookie)))
      (let ((second
              (funcall handler
                       (make-request-env
                        :method :get
                        :path "/session"
                        :headers (list (cons "cookie" cookie))))))
        (is (string= "2" (response-body-text second)))))))

(test application/csrf-is-lack-owned-and-form-body-remains-available
  (let ((router (clog-hypermedia:make-router))
        (mutations 0)
        (auth-calls 0))
    (clog-hypermedia:add-route
     router :get "/csrf-token"
     (lambda (context)
       (clog-hypermedia:html-response
        (clog-hypermedia:csrf-token-for context))))
    (clog-hypermedia:add-route
     router :post "/mutate"
     (lambda (context)
       (incf mutations)
       (is (eq :authenticated (clog-hypermedia:request-user context)))
       (clog-hypermedia:html-response
        (or (clog-hypermedia:form-param context "name") "missing"))))
    (let* ((application
             (clog-hypermedia:make-hypermedia-application
              :router router
              :configuration
              (make-test-configuration
               :static-prefix nil
               :static-root nil
               :authentication-hook
               (lambda (env)
                 (declare (ignore env))
                 (incf auth-calls)
                 :authenticated))))
           (handler (clog-hypermedia:application-handler application))
           (token-response
             (funcall handler
                      (make-request-env :method :get :path "/csrf-token")))
           (token (response-body-text token-response))
           (cookie (cookie-pair-from-response token-response)))
      (is (plusp (length token)))
      (is (plusp (length cookie)))
      (let ((rejected
              (funcall handler
                       (make-request-env
                        :method :post
                        :path "/mutate"
                        :headers (list (cons "cookie" cookie))
                        :content-type "application/x-www-form-urlencoded"
                        :body "name=Kevin"))))
        (is (= 403 (first rejected)))
        (is (= 0 mutations))
        (is (= 1 auth-calls)))
      (let* ((body (format nil "_csrf_token=~A&name=Kevin" token))
             (accepted
               (funcall handler
                        (make-request-env
                         :method :post
                         :path "/mutate"
                         :headers (list (cons "cookie" cookie))
                         :content-type "application/x-www-form-urlencoded"
                         :body body))))
        (is (= 200 (first accepted)))
        (is (string= "Kevin" (response-body-text accepted)))
        (is (= 1 mutations))
        (is (= 2 auth-calls))))))

(test application/body-limit-runs-before-csrf-parser
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :post "/limited"
     (lambda (context)
       (declare (ignore context))
       (clog-hypermedia:html-response "unexpected")))
    (let* ((application
             (make-test-application
              router
              :static-prefix nil
              :static-root nil
              :request-body-limit-bytes 4))
           (response
             (funcall
              (clog-hypermedia:application-handler application)
              (make-request-env
               :method :post
               :path "/limited"
               :content-type "application/x-www-form-urlencoded"
               :content-length 10
               :body "1234567890"))))
      (is (= 413 (first response)))
      (is (string= "Payload Too Large" (response-body-text response))))))

(test application/static-mount-is-explicit-and-traversal-safe
  (let* ((router (clog-hypermedia:make-router))
         (application (make-test-application router))
         (handler (clog-hypermedia:application-handler application))
         (asset
           (funcall handler
                    (make-request-env
                     :method :get
                     :path "/_clog/static/js/boot.js"))))
    (is (= 200 (first asset)))
    (is (pathnamep (third asset)))
    (is (null (clack-response-header asset :set-cookie)))
    (is (string= "nosniff"
                 (clack-response-header asset :x-content-type-options)))
    (dolist (path '("/_clog/static/../clog.asd"
                    "/_clog/static/%2e%2e/clog.asd"
                    "/_clog/static/%2Fetc/passwd"
                    "/_clog/static/js//boot.js"
                    "/_clog/static/js/%5cboot.js"))
      (let ((response
              (funcall handler
                       (make-request-env :method :get :path path))))
        (is (= 400 (first response)) "Traversal path must fail: ~A" path)))
    (let ((not-mounted
            (funcall handler
                     (make-request-env
                      :method :get
                      :path "/_clog/staticity/js/boot.js"))))
      (is (= 404 (first not-mounted))))))

(test application/production-errors-are-redacted
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :get "/explode"
     (lambda (context)
       (declare (ignore context))
       (error "secret-password=do-not-leak")))
    (let* ((application
             (clog-hypermedia:make-hypermedia-application
              :router router
              :configuration
              (clog-hypermedia:make-hypermedia-configuration
               :static-prefix nil
               :static-root nil
               :request-id-generator
               (deterministic-token-generator "production-request")
               :csp-nonce-generator
               (deterministic-token-generator "production-nonce"))))
           (response
             (funcall (clog-hypermedia:application-handler application)
                      (make-request-env :method :get :path "/explode")))
           (body (response-body-text response)))
      (is (= 500 (first response)))
      (is (search "production-request" body))
      (is (null (search "secret-password" body)))
      (is (null (search "do-not-leak" body)))
      (is (null (search "backtrace" body))))))

(test application/development-errors-use-bounded-hook
  (let ((router (clog-hypermedia:make-router))
        (observed nil))
    (clog-hypermedia:add-route
     router :get "/explode"
     (lambda (context)
       (declare (ignore context))
       (error "secret-development-value")))
    (let* ((application
             (clog-hypermedia:make-hypermedia-application
              :router router
              :configuration
              (make-test-configuration
               :development-p t
               :static-prefix nil
               :static-root nil
               :development-condition-hook
               (lambda (condition request-id)
                 (setf observed
                       (list (typep condition 'error)
                             (copy-seq request-id)))))))
           (response
             (funcall (clog-hypermedia:application-handler application)
                      (make-request-env :method :get :path "/explode")))
           (body (response-body-text response)))
      (is (= 500 (first response)))
      (is (equal '(t "request-test") observed))
      (is (search "Development request failure" body))
      (is (null (search "secret-development-value" body))))))

(test application/404-and-405-are-normalized-by-application-boundary
  (let ((router (clog-hypermedia:make-router)))
    (clog-hypermedia:add-route
     router :get "/resource"
     (lambda (context)
       (declare (ignore context))
       (clog-hypermedia:html-response "ok")))
    (clog-hypermedia:add-route
     router :post "/resource"
     (lambda (context)
       (declare (ignore context))
       (clog-hypermedia:html-response "ok")))
    (let* ((application
             (make-test-application
              router :static-prefix nil :static-root nil))
           (handler (clog-hypermedia:application-handler application))
           (missing
             (funcall handler
                      (make-request-env :method :get :path "/missing")))
           (wrong-method
             (funcall handler
                      (make-request-env :method :options :path "/resource"))))
      (is (= 404 (first missing)))
      (is (= 405 (first wrong-method)))
      (is (string= "GET, POST"
                   (clack-response-header wrong-method :allow))))))

(test application/can-be-started-by-clackup
  (let ((router (clog-hypermedia:make-router))
        (server nil))
    (clog-hypermedia:add-route
     router :get "/"
     (lambda (context)
       (declare (ignore context))
       (clog-hypermedia:html-response "ready")))
    (let ((application
            (make-test-application
             router :static-prefix nil :static-root nil)))
      (unwind-protect
           (progn
             (setf server
                   (clack:clackup
                    (clog-hypermedia:as-clack-app application)
                    :server :hunchentoot
                    :address "127.0.0.1"
                    :port 0
                    :debug nil
                    :silent t
                    :use-thread t
                    :use-default-middlewares nil))
             (is (not (null server))))
        (when server
          (is-true (clack:stop server)))))))
