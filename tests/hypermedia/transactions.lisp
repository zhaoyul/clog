;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-034 transaction tests                    ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(define-condition hm-034-render-failure (error) ())

(defclass hm-034-failing-component (hm-034-component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-034-failing-component) context)
  (declare (ignore component context))
  (error 'hm-034-render-failure))

(defun hm-034-failing-component (id)
  (let ((component
          (make-instance 'hm-034-failing-component
                         :id id
                         :scope :application)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-034-application ()
  (clog-hypermedia:make-hypermedia-application
   :name "hm-034"
   :configuration
   (make-test-configuration
    :assets-mode :none
    :strict-csp-p nil
    :static-prefix nil
    :static-root nil)))

(defun hm-034-request ()
  (hm-022-request
   :request-id "request-hm034"
   :session-id "session-hm034"))

(test transactions/nested-flushes-only-at-outer-commit
  (let* ((a (hm-034-component +hm-034-a+))
         (b (hm-034-component +hm-034-b+))
         (inside-a nil)
         (inside-b nil)
         (nested-return nil)
         (result
           (clog-hypermedia:with-ui-transaction (nil)
             (clog-hypermedia:invalidate-component a)
             (setf nested-return
                   (clog-hypermedia:with-ui-transaction (nil)
                     (clog-hypermedia:invalidate-component a)
                     (clog-hypermedia:invalidate-component b)
                     (setf inside-a
                           (clog-hypermedia:component-revision a)
                           inside-b
                           (clog-hypermedia:component-revision b))
                     :nested-body-value))
             ;; Inner exit must not commit or flush.
             (is (= 0 (clog-hypermedia:component-revision a)))
             (is (= 0 (clog-hypermedia:component-revision b))))))
    (is (eq :nested-body-value nested-return))
    (is (= 0 inside-a))
    (is (= 0 inside-b))
    (is (= 1 (clog-hypermedia:component-revision a)))
    (is (= 1 (clog-hypermedia:component-revision b)))
    (is (equal (list +hm-034-a+ +hm-034-b+)
               (hm-034-component-ids
                (clog-hypermedia:dirty-set-components result))))))

(test transactions/body-failure-does-not-commit-invalidation-revisions
  (let ((a (hm-034-component +hm-034-a+))
        (b (hm-034-component +hm-034-b+)))
    (signals error
      (clog-hypermedia:with-ui-transaction (nil)
        (clog-hypermedia:invalidate-component a)
        (clog-hypermedia:invalidate-component b)
        (error "abort transaction body")))
    (is (= 0 (clog-hypermedia:component-revision a)))
    (is (= 0 (clog-hypermedia:component-revision b)))))

(test transactions/opposite-input-order-has-stable-commit-order-and-no-rollback
  (let* ((a (hm-034-component +hm-034-a+))
         (b (hm-034-component +hm-034-b+))
         (iterations 8)
         (worker
           (lambda (reverse-p)
             (dotimes (index iterations)
               (declare (ignore index))
               (clog-hypermedia:with-ui-transaction (nil)
                 (if reverse-p
                     (progn
                       (clog-hypermedia:invalidate-component b)
                       (clog-hypermedia:invalidate-component a))
                     (progn
                       (clog-hypermedia:invalidate-component a)
                       (clog-hypermedia:invalidate-component b)))))))
         (threads
           (loop for index below 6
                 for reverse-p = (oddp index)
                 collect
                 (let ((worker-order reverse-p)
                       (worker-index index))
                   (bordeaux-threads:make-thread
                    (lambda () (funcall worker worker-order))
                    :name (format nil "hm034-worker-~D" worker-index))))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    ;; Six workers times eight transactions. Each transaction commits each
    ;; component exactly once despite opposite invalidation input order.
    (is (= 48 (clog-hypermedia:component-revision a)))
    (is (= 48 (clog-hypermedia:component-revision b)))
    ;; A final reversed input transaction still returns stable ID order.
    (let ((result
            (clog-hypermedia:with-ui-transaction (nil)
              (clog-hypermedia:invalidate-component b)
              (clog-hypermedia:invalidate-component a))))
      (is (equal (list +hm-034-a+ +hm-034-b+)
                 (hm-034-component-ids
                  (clog-hypermedia:dirty-set-components result))))
      (is (= 49 (clog-hypermedia:component-revision a)))
      (is (= 49 (clog-hypermedia:component-revision b))))))

(test transactions/render-occurs-after-commit-and-failure-does-not-half-respond
  (let* ((component
           (hm-034-failing-component +hm-034-a+))
         (dirty
           (clog-hypermedia:with-ui-transaction (component)
             (clog-hypermedia:invalidate-component component)))
         (result
           (clog-hypermedia:dirty-set->action-result dirty))
         (application (hm-034-application)))
    ;; Domain/UI revision is committed before any rendering is attempted.
    (is (= 1 (clog-hypermedia:component-revision component)))
    (is (equal (list (cons +hm-034-a+ 1))
               (clog-hypermedia:dirty-set-revisions dirty)))
    ;; Rendering failure propagates before a framework response exists. The
    ;; committed revision remains authoritative; HM-034 does not fake rollback.
    (signals hm-034-render-failure
      (clog-hypermedia:action-result->response
       result application component (hm-034-request)))
    (is (= 1 (clog-hypermedia:component-revision component)))))

(test transactions/latest-committed-revision-is-used-by-follow-up-render
  (let* ((component
           (hm-034-component +hm-034-a+ :text "latest"))
         (first
           (clog-hypermedia:with-ui-transaction (component)
             (clog-hypermedia:invalidate-component component)))
         (second
           (clog-hypermedia:with-ui-transaction (component)
             (clog-hypermedia:invalidate-component component)))
         (application (hm-034-application))
         (response
           (clog-hypermedia:action-result->response
            (clog-hypermedia:dirty-set->action-result second)
            application component (hm-034-request))))
    (is (equal (list (cons +hm-034-a+ 1))
               (clog-hypermedia:dirty-set-revisions first)))
    (is (equal (list (cons +hm-034-a+ 2))
               (clog-hypermedia:dirty-set-revisions second)))
    (is (= 2 (clog-hypermedia:component-revision component)))
    (is (search "data-clog-revision=\"2\""
                (clog-hypermedia:response-body response)))))
