;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-035 component-tree tests                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defun hm-035-id (number)
  "Return one deterministic valid component id for tree fixtures."
  (string-downcase (format nil "clog-c-~32,'0x" number)))

(defun hm-035-component
    (number &key (scope :application) owner-session-id parent-id)
  "Create and mount one base component suitable for composition tests."
  (let ((component
          (make-instance 'clog-hypermedia:component
                         :id (hm-035-id number)
                         :scope scope
                         :owner-session-id owner-session-id
                         :parent-id parent-id)))
    (clog-hypermedia:mount-component component)
    component))

(test component-tree/composition/add-remove-is-stable-and-revisioned
  (let ((parent (hm-035-component #x3501))
        (child-b (hm-035-component #x3503))
        (child-a (hm-035-component #x3502)))
    ;; Attach in reverse ID order; the public snapshot remains deterministic.
    (is (eq child-b (clog-hypermedia:add-child parent child-b)))
    (is (eq child-a (clog-hypermedia:add-child parent child-a)))
    (is (equal (list (clog-hypermedia:component-id child-a)
                     (clog-hypermedia:component-id child-b))
               (mapcar #'clog-hypermedia:component-id
                       (clog-hypermedia:component-children parent))))
    (is-true (clog-hypermedia:ancestor-p parent child-a))
    (is-true (clog-hypermedia:ancestor-p parent child-b))
    (is-false (clog-hypermedia:ancestor-p child-a parent))
    ;; Topology is observable UI state: parent and child revisions advance.
    (is (= 2 (clog-hypermedia:component-revision parent)))
    (is (= 1 (clog-hypermedia:component-revision child-a)))
    (is (= 1 (clog-hypermedia:component-revision child-b)))
    ;; Re-adding the exact relation is idempotent and does not create revision noise.
    (is (eq child-a (clog-hypermedia:add-child parent child-a)))
    (is (= 2 (clog-hypermedia:component-revision parent)))
    (is (= 1 (clog-hypermedia:component-revision child-a)))
    ;; Public snapshots are defensive.
    (let ((snapshot (clog-hypermedia:component-children parent)))
      (setf (first snapshot) parent)
      (is (eq child-a (first (clog-hypermedia:component-children parent)))))
    (is (eq child-a (clog-hypermedia:remove-child parent child-a)))
    (is-false (clog-hypermedia:ancestor-p parent child-a))
    (is (= 3 (clog-hypermedia:component-revision parent)))
    (is (= 2 (clog-hypermedia:component-revision child-a)))
    ;; Removing a relation that no longer exists is idempotent.
    (is (eq child-a (clog-hypermedia:remove-child parent child-a)))
    (is (= 3 (clog-hypermedia:component-revision parent)))
    (is (= 2 (clog-hypermedia:component-revision child-a)))))

(test component-tree/security/cross-session-and-reparent-are-rejected
  (let* ((parent-a
           (hm-035-component #x3510
                             :scope :session
                             :owner-session-id "session-a"))
         (parent-b
           (hm-035-component #x3511
                             :scope :session
                             :owner-session-id "session-a"))
         (foreign
           (hm-035-component #x3512
                             :scope :session
                             :owner-session-id "session-b"))
         (child
           (hm-035-component #x3513
                             :scope :session
                             :owner-session-id "session-a")))
    (signals clog-hypermedia:component-error
      (clog-hypermedia:add-child parent-a foreign))
    (is (null (clog-hypermedia:component-children parent-a)))
    (clog-hypermedia:add-child parent-a child)
    (signals clog-hypermedia:component-error
      (clog-hypermedia:add-child parent-b child))
    (is (equal (list child) (clog-hypermedia:component-children parent-a)))
    (is (null (clog-hypermedia:component-children parent-b)))))

(test component-tree/security/cycle-is-rejected-without-topology-damage
  (let ((parent (hm-035-component #x3520))
        (child (hm-035-component #x3521))
        (grandchild (hm-035-component #x3522)))
    (clog-hypermedia:add-child parent child)
    (clog-hypermedia:add-child child grandchild)
    (signals clog-hypermedia:component-error
      (clog-hypermedia:add-child grandchild parent))
    (is-true (clog-hypermedia:ancestor-p parent grandchild))
    (is-true (clog-hypermedia:ancestor-p child grandchild))
    (is-false (clog-hypermedia:ancestor-p grandchild parent))
    (is (equal (list child) (clog-hypermedia:component-children parent)))
    (is (equal (list grandchild) (clog-hypermedia:component-children child)))
    (is (null (clog-hypermedia:component-children grandchild)))))

(test component-tree/lifecycle/parent-unmount-cascades-and-clears-relations
  (let ((parent (hm-035-component #x3530))
        (child (hm-035-component #x3531))
        (grandchild (hm-035-component #x3532)))
    (clog-hypermedia:add-child parent child)
    (clog-hypermedia:add-child child grandchild)
    (multiple-value-bind (returned status)
        (clog-hypermedia:unmount-component parent)
      (is (eq parent returned))
      (is (eq :unmounted status)))
    (dolist (component (list parent child grandchild))
      (is (eq :unmounted
              (clog-hypermedia:component-lifecycle-state component)))
      (is (null (clog-hypermedia:component-children component))))
    (is-false (clog-hypermedia:ancestor-p parent child))
    (is-false (clog-hypermedia:ancestor-p parent grandchild))
    (multiple-value-bind (returned status)
        (clog-hypermedia:unmount-component parent)
      (is (eq parent returned))
      (is (eq :already-unmounted status)))))

(test component-tree/store/delete-parent-removes-cascaded-descendants
  (let* ((session-id "session-hm035")
         (store (clog-hypermedia:make-memory-component-store))
         (parent
           (hm-035-component #x3540
                             :scope :session
                             :owner-session-id session-id))
         (child
           (hm-035-component #x3541
                             :scope :session
                             :owner-session-id session-id))
         (grandchild
           (hm-035-component #x3542
                             :scope :session
                             :owner-session-id session-id)))
    (clog-hypermedia:add-child parent child)
    (clog-hypermedia:add-child child grandchild)
    (dolist (component (list parent child grandchild))
      (clog-hypermedia:store-component store session-id component))
    (is (= 3 (getf (clog-hypermedia:component-store-stats store)
                   :component-count)))
    (multiple-value-bind (deleted status)
        (clog-hypermedia:delete-component
         store session-id (clog-hypermedia:component-id parent))
      (is (eq parent deleted))
      (is (eq :deleted status)))
    ;; Cascade cleanup is part of DELETE-COMPONENT, not a lazy side effect of
    ;; the next enumeration/load. The registry must be clean immediately.
    (is (= 0 (getf (clog-hypermedia:component-store-stats store)
                   :component-count)))
    (is (null (clog-hypermedia:enumerate-components store session-id)))
    (is (= 0 (getf (clog-hypermedia:component-store-stats store)
                   :component-count)))
    (dolist (component (list parent child grandchild))
      (is (eq :unmounted
              (clog-hypermedia:component-lifecycle-state component))))))

(defclass hm-035-render-component (clog-hypermedia:component)
  ((text :initarg :text :reader hm-035-render-text)
   (child :initarg :child :initform nil :reader hm-035-render-child-ref)))

(defmethod clog-hypermedia:render-component
    ((component hm-035-render-component) context)
  (spinneret:with-html-string
    (:section
     :attrs (clog-hypermedia:component-root-attributes component context)
     (:span :class "hm035-text" (hm-035-render-text component))
     (:span :class "hm035-context"
            :data-same-context
            (if (eq context (clog-hypermedia:current-render-context)) "yes" "no")
            :data-current-component
            (if (eq component (clog-hypermedia:current-render-component)) "yes" "no"))
     (when (hm-035-render-child-ref component)
       (clog-hypermedia:render-child
        (hm-035-render-child-ref component))))))

(defun hm-035-render-component
    (number text &key child)
  (let ((component
          (make-instance 'hm-035-render-component
                         :id (hm-035-id number)
                         :scope :application
                         :text text
                         :child child)))
    (clog-hypermedia:mount-component component)
    component))

(test component-tree/render/render-child-reuses-current-render-context
  (let* ((child
           (hm-035-render-component #x3551 "child <escaped>"))
         (parent
           (hm-035-render-component #x3550 "parent" :child child))
         (context
           (clog-hypermedia:make-render-context :mode :test)))
    (clog-hypermedia:add-child parent child)
    (let ((html (clog-hypermedia:render parent context)))
      (is (search (clog-hypermedia:component-id parent) html))
      (is (search (clog-hypermedia:component-id child) html))
      (is (search "child &lt;escaped&gt;" html))
      (is (= 2 (hm-034-count-substring "data-same-context=\"yes\"" html)))
      (is (= 2 (hm-034-count-substring "data-current-component=\"yes\"" html))))
    (signals clog-hypermedia:invalid-render-context
      (clog-hypermedia:render-child child))))
