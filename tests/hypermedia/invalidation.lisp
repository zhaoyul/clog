;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-034 invalidation tests                   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defclass hm-034-state-component (clog-hypermedia:component)
  ((label :initarg :label :reader hm-034-label)))

(defmethod clog-hypermedia:render-component
    ((component hm-034-state-component) context)
  (declare (ignore context))
  (spinneret:with-html-string
    (:section :id (clog-hypermedia:component-id component)
              (:span (hm-034-label component)))))

(defun hm-034-id (number)
  "Return one deterministic valid component id for HM-034 fixtures."
  (format nil "clog-c-~32,'0x" number))

(defun hm-034-state-component
    (number &key parent-id (label (format nil "component-~D" number)))
  "Create and mount one application-scoped transaction fixture."
  (let ((component
          (make-instance 'hm-034-state-component
                         :id (hm-034-id number)
                         :scope :application
                         :parent-id parent-id
                         :label label)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-034-request (&optional (suffix "default"))
  "Return a deterministic immutable request context for transaction tests."
  (hm-022-request
   :request-id (format nil "request-hm034-~A" suffix)
   :session-id (format nil "session-hm034-~A" suffix)))

(test invalidation/manual/outside-transaction-commits-once
  (let ((component (hm-034-state-component #x3401)))
    (is (= 0 (clog-hypermedia:component-revision component)))
    (is (eq component (clog-hypermedia:invalidate-component component)))
    (is (= 1 (clog-hypermedia:component-revision component)))
    (is-true (clog-component:component-dirty-p component))))

(test invalidation/transaction/defers-and-deduplicates-until-outer-flush
  (let ((component (hm-034-state-component #x3402))
        (inside-revision nil))
    (is (eq :done
            (clog-hypermedia:with-ui-transaction ((hm-034-request "dedupe"))
              (clog-hypermedia:invalidate-component component)
              (clog-hypermedia:invalidate-component component)
              (setf inside-revision
                    (clog-hypermedia:component-revision component))
              :done)))
    (is (= 0 inside-revision)
        "Invalidation must not commit while the outer transaction is open.")
    (is (= 1 (clog-hypermedia:component-revision component))
        "Duplicate invalidations in one transaction commit one revision.")))

(test invalidation/transaction/nested-transactions-share-the-outer-dirty-set
  (let ((a (hm-034-state-component #x3403))
        (b (hm-034-state-component #x3404))
        (after-inner-a nil)
        (after-inner-b nil))
    (is (eq :outer-result
            (clog-hypermedia:with-ui-transaction ((hm-034-request "nested"))
              (clog-hypermedia:invalidate-component a)
              (is (eq :inner-result
                      (clog-hypermedia:with-ui-transaction
                          ((hm-034-request "nested"))
                        (clog-hypermedia:invalidate-component b)
                        (clog-hypermedia:invalidate-component a)
                        :inner-result)))
              (setf after-inner-a (clog-hypermedia:component-revision a)
                    after-inner-b (clog-hypermedia:component-revision b))
              :outer-result)))
    (is (= 0 after-inner-a))
    (is (= 0 after-inner-b)
        "An inner transaction must not flush independently.")
    (is (= 1 (clog-hypermedia:component-revision a)))
    (is (= 1 (clog-hypermedia:component-revision b)))))

(test invalidation/transaction/unmounted-before-flush-is-ignored
  (let ((component (hm-034-state-component #x3405)))
    (clog-hypermedia:with-ui-transaction ((hm-034-request "unmounted"))
      (clog-hypermedia:invalidate-component component)
      (clog-hypermedia:unmount-component component))
    (is (eq :unmounted
            (clog-hypermedia:component-lifecycle-state component)))
    (is (= 0 (clog-hypermedia:component-revision component)))
    (is-false (clog-component:component-dirty-p component))))

(test invalidation/transaction/context-mismatch-fails-closed
  (let ((component (hm-034-state-component #x3406)))
    (signals clog-hypermedia:ui-transaction-error
      (clog-hypermedia:with-ui-transaction ((hm-034-request "outer"))
        (clog-hypermedia:invalidate-component component)
        (clog-hypermedia:with-ui-transaction ((hm-034-request "other"))
          (clog-hypermedia:invalidate-component component))))
    ;; The rejected outer transaction must not leak its dirty set into state.
    (is (= 0 (clog-hypermedia:component-revision component)))))
