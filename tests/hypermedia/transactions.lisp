;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-034 UI transaction integration tests     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defclass hm-034-action-component (clog-hypermedia:component)
  ((value :initform 0 :accessor hm-034-action-value)
   (targets :initarg :targets :initform nil :reader hm-034-action-targets)))

(defmethod clog-hypermedia:render-component
    ((component hm-034-action-component) context)
  (spinneret:with-html-string
    (:section
     :attrs (clog-hypermedia:component-root-attributes component context)
     (:span :class "hm034-value"
            (format nil "~D" (hm-034-action-value component))))))

(defclass hm-034-failing-component (hm-034-state-component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-034-failing-component) context)
  (declare (ignore component context))
  (error "secret-hm034-render-value"))

(defun hm-034-decode-delta (context)
  (parse-integer
   (or (clog-hypermedia:form-param context "delta" nil)
       "1")
   :junk-allowed nil))

(clog-hypermedia:defaction
    (hm-034-action-component :hm-034-update
     :external-name "hm-034-update"
     :parameter-decoder #'hm-034-decode-delta)
    (component delta)
  (incf (hm-034-action-value component) delta)
  (dolist (target (hm-034-action-targets component))
    (clog-hypermedia:invalidate-component target))
  (clog-hypermedia:render-self))

(defun hm-034-make-failing-component (number)
  (let ((component
          (make-instance 'hm-034-failing-component
                         :id (hm-034-id number)
                         :scope :application
                         :label "must-not-leak")))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-034-make-action-fixture
    (targets &key (current-id (hm-034-id #x34f0)) current-parent-id)
  "Return APP, HANDLER, session component map and STORE for HM-034 actions."
  (let* ((router (clog-hypermedia:make-router))
         (store (clog-hypermedia:make-memory-component-store))
         (components (make-hash-table :test #'equal)))
    (clog-hypermedia:add-route
     router :get "/hm034/bootstrap"
     (lambda (context)
       (let* ((session-id (clog-hypermedia:request-session-id context))
              (component (gethash session-id components)))
         (unless component
           (setf component
                 (make-instance 'hm-034-action-component
                                :id current-id
                                :scope :session
                                :parent-id current-parent-id
                                :owner-session-id session-id
                                :targets targets))
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
              :name "hm-034"
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

(defun hm-034-bootstrap (handler &optional cookie)
  "Return CSRF token, component id, revision and cookie for one action session."
  (let* ((response
           (funcall handler
                    (make-request-env
                     :method :get
                     :path "/hm034/bootstrap"
                     :headers
                     (when cookie (list (cons "cookie" cookie))))))
         (body (response-body-text response))
         (first-bar (position #\| body))
         (second-bar (position #\| body :start (1+ first-bar))))
    (values (subseq body 0 first-bar)
            (subseq body (1+ first-bar) second-bar)
            (parse-integer (subseq body (1+ second-bar)))
            (or cookie (cookie-pair-from-response response)))))

(defun hm-034-post
    (handler cookie token component-id &key (delta 1) (revision 0))
  "POST the HM-034 action through the complete application middleware stack."
  (funcall
   handler
   (make-request-env
    :method :post
    :path (format nil "/_clog/action/~A/hm-034-update" component-id)
    :headers (list (cons "cookie" cookie)
                   (cons "hx-request" "true"))
    :content-type "application/x-www-form-urlencoded"
    :body (format nil
                  "_csrf_token=~A&_clog_revision=~D&delta=~D"
                  token revision delta))))

(defun hm-034-current-component (components)
  (loop for component being the hash-values of components
        do (return component)))

(defun hm-034-count-substring (needle haystack)
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(test transactions/reduction/three-unrelated-components-return-three-stable-partials
  (let ((c (hm-034-state-component #x3413 :label "gamma"))
        (a (hm-034-state-component #x3411 :label "alpha"))
        (b (hm-034-state-component #x3412 :label "beta")))
    (multiple-value-bind (app handler components store)
        (hm-034-make-action-fixture (list c a b))
      (declare (ignore app store))
      (multiple-value-bind (token component-id revision cookie)
          (hm-034-bootstrap handler)
        (let* ((response
                 (hm-034-post handler cookie token component-id
                              :revision revision))
               (body (response-body-text response))
               (current (hm-034-current-component components))
               (position-a (search (clog-hypermedia:component-id a) body))
               (position-b (search (clog-hypermedia:component-id b) body))
               (position-c (search (clog-hypermedia:component-id c) body)))
          (is (= 200 (first response)))
          (is (= 3 (hm-034-count-substring "<hx-partial" body)))
          (is (and position-a position-b position-c
                   (< position-a position-b position-c))
              "Reduced partials must be stable by dependency depth then id.")
          (is (= 1 (clog-hypermedia:component-revision current)))
          (is (= 1 (clog-hypermedia:component-revision a)))
          (is (= 1 (clog-hypermedia:component-revision b)))
          (is (= 1 (clog-hypermedia:component-revision c))))))))

(test transactions/reduction/dirty-parent-suppresses-dirty-child
  (let* ((parent (hm-034-state-component #x3420 :label "parent"))
         (child
           (hm-034-state-component
            #x3421
            :parent-id (clog-hypermedia:component-id parent)
            :label "child")))
    (multiple-value-bind (app handler components store)
        (hm-034-make-action-fixture (list child parent))
      (declare (ignore app components store))
      (multiple-value-bind (token component-id revision cookie)
          (hm-034-bootstrap handler)
        (let* ((response
                 (hm-034-post handler cookie token component-id
                              :revision revision))
               (body (response-body-text response)))
          (is (= 200 (first response)))
          (is (= 1 (hm-034-count-substring "<hx-partial" body)))
          (is (search (clog-hypermedia:component-id parent) body))
          (is-false (search (clog-hypermedia:component-id child) body))
          ;; Suppression is a render optimization, not a lost state commit.
          (is (= 1 (clog-hypermedia:component-revision parent)))
          (is (= 1 (clog-hypermedia:component-revision child))))))))

(test transactions/reduction/primary-parent-suppresses-dirty-descendant
  (let* ((current-id (hm-034-id #x3430))
         (child
           (hm-034-state-component
            #x3431 :parent-id current-id :label "child")))
    (multiple-value-bind (app handler components store)
        (hm-034-make-action-fixture (list child) :current-id current-id)
      (declare (ignore app store))
      (multiple-value-bind (token component-id revision cookie)
          (hm-034-bootstrap handler)
        (let* ((response
                 (hm-034-post handler cookie token component-id
                              :revision revision))
               (body (response-body-text response))
               (current (hm-034-current-component components)))
          (is (= 200 (first response)))
          (is (= 0 (hm-034-count-substring "<hx-partial" body)))
          (is (search current-id body))
          (is (= 1 (clog-hypermedia:component-revision current)))
          (is (= 1 (clog-hypermedia:component-revision child))))))))

(test transactions/reduction/dirty-parent-can-cover-the-primary-child
  (let* ((parent (hm-034-state-component #x3440 :label "parent"))
         (current-id (hm-034-id #x3441)))
    (multiple-value-bind (app handler components store)
        (hm-034-make-action-fixture
         (list parent)
         :current-id current-id
         :current-parent-id (clog-hypermedia:component-id parent))
      (declare (ignore app store))
      (multiple-value-bind (token component-id revision cookie)
          (hm-034-bootstrap handler)
        (let* ((response
                 (hm-034-post handler cookie token component-id
                              :revision revision))
               (body (response-body-text response))
               (current (hm-034-current-component components)))
          (is (= 200 (first response)))
          (is (= 1 (hm-034-count-substring "<hx-partial" body)))
          (is (search (clog-hypermedia:component-id parent) body))
          (is-false (search current-id body)
                    "A dirty ancestor partial makes the primary child main swap redundant.")
          (is (= 1 (clog-hypermedia:component-revision current)))
          (is (= 1 (clog-hypermedia:component-revision parent))))))))

(test transactions/concurrency/reverse-invalidation-order-does-not-deadlock-or-lose-revisions
  (let* ((a (hm-034-state-component #x3460))
         (b (hm-034-state-component #x3461))
         (thread-count 6)
         (transactions-per-thread 12)
         (errors nil)
         (errors-lock (bordeaux-threads:make-lock "hm034-errors"))
         (threads
           (loop for thread-index below thread-count
                 collect
                 (let ((captured-index thread-index))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (loop for iteration below transactions-per-thread
                            do (handler-case
                                   (clog-hypermedia:with-ui-transaction
                                       ((hm-034-request
                                         (format nil "concurrent-~D-~D"
                                                 captured-index iteration)))
                                     (if (evenp (+ captured-index iteration))
                                         (progn
                                           (clog-hypermedia:invalidate-component a)
                                           (clog-hypermedia:invalidate-component b))
                                         (progn
                                           (clog-hypermedia:invalidate-component b)
                                           (clog-hypermedia:invalidate-component a))))
                                 (error (condition)
                                   (bordeaux-threads:with-lock-held (errors-lock)
                                     (push condition errors))))))
                    :name "hm034-reverse-invalidation")))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (is (null errors))
    (let ((expected (* thread-count transactions-per-thread)))
      (is (= expected (clog-hypermedia:component-revision a)))
      (is (= expected (clog-hypermedia:component-revision b))))))

(test transactions/render-failure/state-commits-before-response-rendering
  (let ((failing (hm-034-make-failing-component #x3470)))
    (multiple-value-bind (app handler components store)
        (hm-034-make-action-fixture (list failing)
                                    :current-id (hm-034-id #x3471))
      (declare (ignore app store))
      (multiple-value-bind (token component-id revision cookie)
          (hm-034-bootstrap handler)
        (let* ((response
                 (hm-034-post handler cookie token component-id
                              :revision revision))
               (body (response-body-text response))
               (current (hm-034-current-component components)))
          (is (= 500 (first response)))
          (is (= 1 (hm-034-action-value current)))
          (is (= 1 (clog-hypermedia:component-revision current)))
          (is (= 1 (clog-hypermedia:component-revision failing)))
          (is (search "Action failed" body))
          (is-false (search "secret-hm034-render-value" body))
          (is-false (search "<hx-partial" body))
          (is-false (search (clog-hypermedia:component-id current) body)
                    "No committed fragment is exposed when later rendering fails."))))))
