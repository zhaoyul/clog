;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-034 dirty invalidation tests             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defparameter +hm-034-parent+
  "clog-c-00000000000000000000000000003401")
(defparameter +hm-034-child+
  "clog-c-00000000000000000000000000003402")
(defparameter +hm-034-a+
  "clog-c-00000000000000000000000000003411")
(defparameter +hm-034-b+
  "clog-c-00000000000000000000000000003412")
(defparameter +hm-034-c+
  "clog-c-00000000000000000000000000003413")

(defclass hm-034-component (clog-hypermedia:component)
  ((text :initarg :text :initform "component" :reader hm-034-text)))

(defmethod clog-hypermedia:render-component
    ((component hm-034-component) context)
  (spinneret:with-html-string
    (:section :attrs (clog-hypermedia:component-root-attributes component context)
              (hm-034-text component))))

(defun hm-034-component (id &key parent-id (text "component"))
  (let ((component
          (make-instance 'hm-034-component
                         :id id
                         :scope :application
                         :parent-id parent-id
                         :text text)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-034-component-ids (components)
  (mapcar #'clog-hypermedia:component-id components))

(test invalidation/requires-transaction
  (let ((component (hm-034-component +hm-034-a+)))
    (signals clog-hypermedia:ui-transaction-error
      (clog-hypermedia:invalidate-component component))
    (is (= 0 (clog-hypermedia:component-revision component)))))

(test invalidation/parent-dirty-suppresses-child-representation
  (let* ((parent (hm-034-component +hm-034-parent+ :text "parent"))
         (child (hm-034-component +hm-034-child+
                                  :parent-id +hm-034-parent+
                                  :text "child"))
         (result
           (clog-hypermedia:with-ui-transaction (parent)
             (clog-hypermedia:invalidate-component child)
             (clog-hypermedia:invalidate-component parent))))
    (is (clog-hypermedia:dirty-set-p result))
    ;; Both state-bearing components commit exactly once.
    (is (= 1 (clog-hypermedia:component-revision parent)))
    (is (= 1 (clog-hypermedia:component-revision child)))
    ;; Representation reduction keeps only the dirty ancestor.
    (is (equal (list +hm-034-parent+)
               (hm-034-component-ids
                (clog-hypermedia:dirty-set-components result))))
    (is (equal (list (cons +hm-034-parent+ 1))
               (clog-hypermedia:dirty-set-revisions result)))
    ;; HTTP action integration treats the dirty context component as primary.
    (let ((action-result
            (clog-hypermedia:dirty-set->action-result result)))
      (is (eq parent
              (clog-hypermedia:action-result-primary-component action-result)))
      (is (null
           (clog-hypermedia:action-result-invalidated-components action-result))))))

(test invalidation/three-unrelated-components-remain-three-stable-partials
  (let* ((a (hm-034-component +hm-034-a+ :text "a"))
         (b (hm-034-component +hm-034-b+ :text "b"))
         (c (hm-034-component +hm-034-c+ :text "c"))
         (result
           (clog-hypermedia:with-ui-transaction (nil)
             ;; Intentionally reverse the stable identifier order.
             (clog-hypermedia:invalidate-component c)
             (clog-hypermedia:invalidate-component a)
             (clog-hypermedia:invalidate-component b))))
    (is (equal (list +hm-034-a+ +hm-034-b+ +hm-034-c+)
               (hm-034-component-ids
                (clog-hypermedia:dirty-set-components result))))
    (is (equal (list (cons +hm-034-a+ 1)
                     (cons +hm-034-b+ 1)
                     (cons +hm-034-c+ 1))
               (clog-hypermedia:dirty-set-revisions result)))
    (let ((action-result
            (clog-hypermedia:dirty-set->action-result result)))
      (is (null
           (clog-hypermedia:action-result-primary-component action-result)))
      (is (equal (list +hm-034-a+ +hm-034-b+ +hm-034-c+)
                 (hm-034-component-ids
                  (clog-hypermedia:action-result-invalidated-components
                   action-result)))))))

(test invalidation/duplicate-invalidation-commits-one-revision
  (let ((component (hm-034-component +hm-034-a+)))
    (let ((first
            (clog-hypermedia:with-ui-transaction (component)
              (clog-hypermedia:invalidate-component component)
              (clog-hypermedia:invalidate-component component)
              (clog-hypermedia:invalidate-component component))))
      (is (= 1 (clog-hypermedia:component-revision component)))
      (is (equal (list (cons +hm-034-a+ 1))
                 (clog-hypermedia:dirty-set-revisions first))))
    (let ((second
            (clog-hypermedia:with-ui-transaction (component)
              (clog-hypermedia:invalidate-component component))))
      (is (= 2 (clog-hypermedia:component-revision component)))
      (is (equal (list (cons +hm-034-a+ 2))
                 (clog-hypermedia:dirty-set-revisions second))))))

(test invalidation/unmounted-component-is-removed-before-commit
  (let ((component (hm-034-component +hm-034-a+)))
    (let ((result
            (clog-hypermedia:with-ui-transaction (nil)
              (clog-hypermedia:invalidate-component component)
              (clog-hypermedia:unmount-component component))))
      (is (clog-hypermedia:dirty-set-empty-p result))
      (is (null (clog-hypermedia:dirty-set-components result)))
      (is (= 0 (clog-hypermedia:component-revision component))))))

(test invalidation/dirty-set-accessors-are-defensive
  (let* ((a (hm-034-component +hm-034-a+))
         (b (hm-034-component +hm-034-b+))
         (context (list :request "hm034"))
         (result
           (clog-hypermedia:with-ui-transaction (context)
             (clog-hypermedia:invalidate-component b)
             (clog-hypermedia:invalidate-component a))))
    (is (equal context (clog-hypermedia:dirty-set-context result)))
    (let ((components (clog-hypermedia:dirty-set-components result))
          (revisions (clog-hypermedia:dirty-set-revisions result)))
      (setf (first components) nil)
      (setf (cdar revisions) 999)
      (is (equal (list +hm-034-a+ +hm-034-b+)
                 (hm-034-component-ids
                  (clog-hypermedia:dirty-set-components result))))
      (is (equal (list (cons +hm-034-a+ 1)
                       (cons +hm-034-b+ 1))
                 (clog-hypermedia:dirty-set-revisions result))))))
