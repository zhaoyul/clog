;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-025 action dispatch integration tests      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defclass hm-025-counter-component (clog-hypermedia:component)
  ((value :initform 0 :accessor hm-025-value)))

(defmethod clog-hypermedia:render-component
    ((component hm-025-counter-component) context)
  (spinneret:with-html-string
    (:section
     :attrs (clog-hypermedia:component-root-attributes component context)
     (:span :class "value" (format nil "~D" (hm-025-value component))))))

(defun hm-025-decode-delta (context)
  (parse-integer
   (or (clog-hypermedia:form-param context "delta" nil)
       (error "missing delta"))
   :junk-allowed nil))

(defun hm-025-deny (component context)
  (declare (ignore component context))
  nil)

(clog-hypermedia:defaction
    (hm-025-counter-component :hm-025-increment
     :external-name "hm-025-increment"
     :parameter-decoder #'hm-025-decode-delta)
    (component delta)
  (incf (hm-025-value component) delta)
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (hm-025-counter-component :hm-025-current
     :external-name "hm-025-current"
     :parameter-decoder #'hm-025-decode-delta
     :requires-current t)
    (component delta)
  (incf (hm-025-value component) delta)
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (hm-025-counter-component :hm-025-denied
     :external-name "hm-025-denied"
     :parameter-decoder #'hm-025-decode-delta
     :authorize #'hm-025-deny)
    (component delta)
  (incf (hm-025-value component) delta)
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (hm-025-counter-component :hm-025-explode
     :external-name "hm-025-explode")
    (component context)
  (declare (ignore component context))
  (error "secret-hm025-handler-value"))

(defun hm-025-make-fixture ()
  "Return APP, HANDLER, session->component map and STORE."
  (let* ((router (clog-hypermedia:make-router))
         (store (clog-hypermedia:make-memory-component-store))
         (components (make-hash-table :test #'equal)))
    (clog-hypermedia:add-route
     router :get "/hm025/bootstrap"
     (lambda (context)
       (let* ((session-id (clog-hypermedia:request-session-id context))
              (component (gethash session-id components)))
         (unless component
           (setf component
                 (make-instance 'hm-025-counter-component
                                :scope :session
                                :owner-session-id session-id))
           (clog-hypermedia:mount-component component)
           (clog-hypermedia:store-component store session-id component)
           (setf (gethash (copy-seq session-id) components) component))
         (clog-hypermedia:html-response
          (format nil "~A|~A|~D"
                  (clog-hypermedia:csrf-token-for context)
                  (clog-hypermedia:component-id component)
                  (clog-hypermedia:component-revision component))))))
    (let* ((app
             (clog-hypermedia:make-hypermedia-application
              :name "hm-025"
              :router router
              :component-store store
              :configuration
              (make-test-configuration
               :assets-mode :none
               :strict-csp-p nil
               :static-prefix nil
               :static-root nil)))
           (handler (clog-hypermedia:application-handler app)))
      (values app handler components store))))

(defun hm-025-bootstrap (handler &optional cookie)
  "Return CSRF token, component id, revision, cookie and response."
  (let* ((response
           (funcall handler
                    (make-request-env
                     :method :get
                     :path "/hm025/bootstrap"
                     :headers
                     (when cookie (list (cons "cookie" cookie))))))
         (body (response-body-text response))
         (first-bar (position #\| body))
         (second-bar (position #\| body :start (1+ first-bar))))
    (values (subseq body 0 first-bar)
            (subseq body (1+ first-bar) second-bar)
            (parse-integer (subseq body (1+ second-bar)))
            (or cookie (cookie-pair-from-response response))
            response)))

(defun hm-025-post
    (handler cookie token component-id action
     &key (delta 1) (revision 0) (method :post) (hx-p t))
  "Submit one action form through the complete Clack middleware pipeline."
  (let ((body
          (format nil
                  "_csrf_token=~A&_clog_revision=~D&delta=~D"
                  token revision delta)))
    (funcall
     handler
     (make-request-env
      :method method
      :path (format nil "/_clog/action/~A/~A" component-id action)
      :headers
      (append (list (cons "cookie" cookie))
              (when hx-p (list (cons "hx-request" "true"))))
      :content-type "application/x-www-form-urlencoded"
      :body body))))

(test action-dispatch/full-pipeline/commits-and-renders-current-fragment
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let* ((session-response
               (hm-025-post handler cookie token component-id
                           "hm-025-increment" :delta 3))
             (component
               (loop for value being the hash-values of components
                     do (return value))))
        (is (= 200 (first session-response)))
        (is (= 3 (hm-025-value component)))
        (is (= 1 (clog-hypermedia:component-revision component)))
        (is (search "data-clog-revision=\"1\""
                    (response-body-text session-response)))
        (is (search ">3</span>" (response-body-text session-response)))))))

(test action-dispatch/security/forged-inputs-do-not-mutate
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let ((component
              (loop for value being the hash-values of components
                    do (return value))))
        (let ((bad-csrf
                (hm-025-post handler cookie "forged" component-id
                            "hm-025-increment")))
          (is (= 403 (first bad-csrf))))
        (let ((unknown-action
                (hm-025-post handler cookie token component-id
                            "hm-025-unknown")))
          (is (= 404 (first unknown-action))))
        (let ((forged-id
                (hm-025-post
                 handler cookie token
                 "clog-c-ffffffffffffffffffffffffffffffff"
                 "hm-025-increment")))
          (is (= 200 (first forged-id)))
          (is (string= "true"
                       (clack-response-header forged-id :hx-refresh))))
        (let ((wrong-method
                (hm-025-post handler cookie token component-id
                            "hm-025-increment" :method :get)))
          (is (= 405 (first wrong-method))))
        (is (= 0 (hm-025-value component)))
        (is (= 0 (clog-hypermedia:component-revision component)))))))

(test action-dispatch/authorization/denial-is-403-and-does-not-mutate
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let* ((component
               (loop for value being the hash-values of components
                     do (return value)))
             (response
               (hm-025-post handler cookie token component-id
                           "hm-025-denied" :delta 8)))
        (is (= 403 (first response)))
        (is (= 0 (hm-025-value component)))
        (is (= 0 (clog-hypermedia:component-revision component)))))))

(test action-dispatch/revision/stale-request-renders-fresh-state-without-mutation
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let ((component
              (loop for value being the hash-values of components
                    do (return value))))
        (let ((first
                (hm-025-post handler cookie token component-id
                            "hm-025-current" :delta 2 :revision 0)))
          (is (= 200 (first first)))
          (is (= 2 (hm-025-value component)))
          (is (= 1 (clog-hypermedia:component-revision component))))
        (let ((stale
                (hm-025-post handler cookie token component-id
                            "hm-025-current" :delta 99 :revision 0)))
          (is (= 200 (first stale)))
          (is (string= "clog:stale-component"
                       (clack-response-header stale :hx-trigger)))
          (is (= 2 (hm-025-value component)))
          (is (= 1 (clog-hypermedia:component-revision component)))
          (is (search "data-clog-revision=\"1\""
                      (response-body-text stale))))))))

(test action-dispatch/concurrency/no-lost-updates-and-monotonic-revision
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let* ((component
               (loop for value being the hash-values of components
                     do (return value)))
             (threads
               (loop repeat 20
                     collect
                     (bordeaux-threads:make-thread
                      (lambda ()
                        (let ((response
                                (hm-025-post
                                 handler cookie token component-id
                                 "hm-025-increment" :delta 1)))
                          (unless (= 200 (first response))
                            (error "concurrent action failed"))))))))
        (dolist (thread threads)
          (bordeaux-threads:join-thread thread))
        (is (= 20 (hm-025-value component)))
        (is (= 20 (clog-hypermedia:component-revision component)))))))

(test action-dispatch/security/cross-session-id-is-indistinguishable-from-expired
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app components store))
    (multiple-value-bind (token-a component-a revision-a cookie-a)
        (hm-025-bootstrap handler)
      (declare (ignore token-a revision-a cookie-a))
      (multiple-value-bind (token-b component-b revision-b cookie-b)
          (hm-025-bootstrap handler)
        (declare (ignore component-b revision-b))
        (let ((response
                (hm-025-post handler cookie-b token-b component-a
                            "hm-025-increment")))
          (is (= 200 (first response)))
          (is (string= "true"
                       (clack-response-header response :hx-refresh)))
          (is (string= "component-expired"
                       (clack-response-header response :x-clog-reason))))))))

(test action-dispatch/production/handler-condition-is-redacted-with-request-id
  (multiple-value-bind (app handler components store)
      (hm-025-make-fixture)
    (declare (ignore app store))
    (multiple-value-bind (token component-id revision cookie)
        (hm-025-bootstrap handler)
      (declare (ignore revision))
      (let* ((component
               (loop for value being the hash-values of components
                     do (return value)))
             (response
               (hm-025-post handler cookie token component-id
                           "hm-025-explode"))
             (body (response-body-text response)))
        (is (= 500 (first response)))
        (is (search "request-test" body))
        (is-false (search "secret-hm025-handler-value" body))
        (is (= 0 (hm-025-value component)))
        (is (= 0 (clog-hypermedia:component-revision component)))))))
