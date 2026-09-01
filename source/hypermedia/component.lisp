;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime component identity and lifecycle             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-component
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:export #:component-error
           #:component-error-reason
           #:invalid-component-definition
           #:invalid-component-definition-reason
           #:component-lifecycle-error
           #:component-lifecycle-error-component-id
           #:component-lifecycle-error-operation
           #:component-lifecycle-error-state
           #:component-lifecycle-error-reason
           #:component-not-mounted
           #:render-method-missing
           #:render-method-missing-component-id
           #:render-method-missing-class
           #:component
           #:component-id
           #:component-key
           #:component-scope
           #:component-parent-id
           #:component-owner-session-id
           #:component-revision
           #:component-dirty-p
           #:component-lifecycle-state
           #:component-last-access
           #:component-lock
           #:component-children
           #:add-child
           #:remove-child
           #:ancestor-p
           #:mount-component
           #:unmount-component
           #:mounted-p
           #:touch-component))

(defpackage #:clog-hypermedia
  (:import-from #:clog-component
                #:component-error
                #:component-error-reason
                #:invalid-component-definition
                #:invalid-component-definition-reason
                #:component-lifecycle-error
                #:component-lifecycle-error-component-id
                #:component-lifecycle-error-operation
                #:component-lifecycle-error-state
                #:component-lifecycle-error-reason
                #:component-not-mounted
                #:render-method-missing
                #:render-method-missing-component-id
                #:render-method-missing-class
                #:component
                #:component-id
                #:component-revision
                #:component-lifecycle-state
                #:component-children
                #:add-child
                #:remove-child
                #:ancestor-p
                #:mount-component
                #:unmount-component
                #:mounted-p
                #:touch-component)
  (:export #:component-error
           #:component-error-reason
           #:invalid-component-definition
           #:invalid-component-definition-reason
           #:component-lifecycle-error
           #:component-lifecycle-error-component-id
           #:component-lifecycle-error-operation
           #:component-lifecycle-error-state
           #:component-lifecycle-error-reason
           #:component-not-mounted
           #:render-method-missing
           #:render-method-missing-component-id
           #:render-method-missing-class
           #:component
           #:component-id
           #:component-revision
           #:component-lifecycle-state
           #:component-children
           #:add-child
           #:remove-child
           #:ancestor-p
           #:mount-component
           #:unmount-component
           #:mounted-p
           #:touch-component))

(in-package #:clog-component)

(define-condition component-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader component-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Hypermedia component operation failed~@[ (~A)~]."
             (component-error-reason condition))))
  (:documentation
   "Base condition for component identity, lifecycle, composition and protocol failures."))

(define-condition invalid-component-definition (component-error)
  ((definition-reason
    :initarg :definition-reason
    :reader invalid-component-definition-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid component definition (~A)."
             (invalid-component-definition-reason condition))))
  (:documentation
   "Signaled when a component is constructed or mounted with invalid identity, scope or ownership metadata."))

(define-condition component-lifecycle-error (component-error)
  ((component-id
    :initarg :component-id
    :reader component-lifecycle-error-component-id)
   (operation
    :initarg :operation
    :reader component-lifecycle-error-operation)
   (state
    :initarg :state
    :reader component-lifecycle-error-state)
   (lifecycle-reason
    :initarg :lifecycle-reason
    :reader component-lifecycle-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Component lifecycle operation ~A rejected in state ~A (~A)."
             (component-lifecycle-error-operation condition)
             (component-lifecycle-error-state condition)
             (component-lifecycle-error-reason condition))))
  (:documentation
   "Signaled when a lifecycle transition is not permitted by the created/mounted/unmounted state machine."))

(define-condition component-not-mounted (component-lifecycle-error)
  ()
  (:documentation
   "Signaled when an operation requiring a mounted component is attempted on a created or unmounted component."))

(define-condition render-method-missing (component-error)
  ((component-id
    :initarg :component-id
    :reader render-method-missing-component-id)
   (class
    :initarg :class
    :reader render-method-missing-class))
  (:report
   (lambda (condition stream)
     (format stream "No render-component method is defined for component class ~A."
             (render-method-missing-class condition))))
  (:documentation
   "Signaled by the base render protocol when a concrete component does not implement RENDER-COMPONENT."))

(defparameter +component-id-prefix+ "clog-c-"
  "Stable prefix for CLOG 3 stateful component DOM identities.")

(defparameter +component-id-random-limit+ (ash 1 128)
  "Exclusive upper bound used to obtain 128 random component-id bits.")

(defparameter *component-id-random-state* (make-random-state t)
  "Process-local random state used only by the default component-id generator.")

(defparameter *component-id-generator-lock*
  (bordeaux-threads:make-lock "clog-component-id-generator")
  "Short-lived lock protecting the process-local component ID random state.")

(defun default-component-id-generator ()
  "Return a new opaque CLOG component ID containing 128 random bits."
  (let ((value
          (bordeaux-threads:with-lock-held (*component-id-generator-lock*)
            (random +component-id-random-limit+
                    *component-id-random-state*))))
    (concatenate 'string
                 +component-id-prefix+
                 (string-downcase (format nil "~32,'0x" value)))))

(defparameter *component-id-generator* #'default-component-id-generator
  "Dynamically bindable component ID generator used by tests and production construction.")

(defun lowercase-hex-character-p (character)
  (or (char<= #\0 character #\9)
      (char<= #\a character #\f)))

(defun valid-component-id-p (value)
  (and (stringp value)
       (= (length value) 39)
       (string= +component-id-prefix+ value :end2 7)
       (loop for index from 7 below 39
             always (lowercase-hex-character-p (char value index)))))

(defun opaque-owner-string-p (value)
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun copy-opaque-value (value)
  (if (stringp value) (copy-seq value) value))

(defun copy-hash-table-shallow (table)
  "Return a shallow defensive copy of TABLE."
  (let ((copy (make-hash-table :test (hash-table-test table)
                               :size (max 1 (hash-table-count table)))))
    (maphash (lambda (key value)
               (setf (gethash (copy-opaque-value key) copy)
                     (copy-opaque-value value)))
             table)
    copy))

(defclass component ()
  ((id
    :initarg :id
    :initform (funcall *component-id-generator*)
    :reader %component-id)
   (key
    :initarg :key
    :initform nil
    :reader %component-key)
   (scope
    :initarg :scope
    :initform :session
    :reader %component-scope)
   (parent-id
    :initarg :parent-id
    :initform nil
    :accessor %component-parent-id)
   (parent
    :initform nil
    :accessor %component-parent)
   (owner-session-id
    :initarg :owner-session-id
    :initform nil
    :reader %component-owner-session-id)
   (children
    :initform nil
    :accessor %component-children)
   (revision
    :initform 0
    :accessor %component-revision)
   (dirty-p
    :initform t
    :accessor %component-dirty-p)
   (lifecycle-state
    :initform :created
    :accessor %component-lifecycle-state)
   (last-access
    :initform (get-universal-time)
    :accessor %component-last-access)
   (lock
    :initform (bordeaux-threads:make-lock "clog-component")
    :reader component-lock)
   (metadata
    :initform (make-hash-table :test #'equal)
    :reader %component-metadata))
  (:documentation
   "Base server-side UI component with stable identity, copy-on-write child topology and per-instance synchronization."))

(defmethod initialize-instance :after ((instance component) &key)
  "Validate and defensively freeze caller-provided component metadata."
  (unless (valid-component-id-p (%component-id instance))
    (error 'invalid-component-definition
           :reason :invalid-component-id
           :definition-reason :invalid-component-id))
  (unless (member (%component-scope instance)
                  '(:request :session :application :persistent)
                  :test #'eq)
    (error 'invalid-component-definition
           :reason :invalid-component-scope
           :definition-reason :invalid-component-scope))
  (let ((parent-id (%component-parent-id instance)))
    (unless (or (null parent-id) (valid-component-id-p parent-id))
      (error 'invalid-component-definition
             :reason :invalid-component-parent
             :definition-reason :invalid-component-parent)))
  (let ((owner (%component-owner-session-id instance)))
    (unless (or (null owner) (opaque-owner-string-p owner))
      (error 'invalid-component-definition
             :reason :invalid-component-owner
             :definition-reason :invalid-component-owner)))
  (setf (slot-value instance 'id) (copy-seq (%component-id instance))
        (slot-value instance 'key) (copy-opaque-value (%component-key instance)))
  (let ((parent-id (%component-parent-id instance)))
    (when parent-id
      (setf (%component-parent-id instance) (copy-seq parent-id))))
  (let ((owner (%component-owner-session-id instance)))
    (when owner
      (setf (slot-value instance 'owner-session-id) (copy-seq owner)))))

(defun component-id (instance)
  (check-type instance component)
  (copy-seq (%component-id instance)))

(defun component-key (instance)
  (check-type instance component)
  (copy-opaque-value (%component-key instance)))

(defun component-scope (instance)
  (check-type instance component)
  (%component-scope instance))

(defun component-parent-id (instance)
  (check-type instance component)
  (let ((value (%component-parent-id instance)))
    (and value (copy-seq value))))

(defun component-owner-session-id (instance)
  (check-type instance component)
  (let ((value (%component-owner-session-id instance)))
    (and value (copy-seq value))))

(defun component-revision (instance)
  (check-type instance component)
  (%component-revision instance))

(defun component-dirty-p (instance)
  (check-type instance component)
  (%component-dirty-p instance))

(defun component-lifecycle-state (instance)
  (check-type instance component)
  (%component-lifecycle-state instance))

(defun component-last-access (instance)
  (check-type instance component)
  (%component-last-access instance))

(defun mounted-p (instance)
  (check-type instance component)
  (eq :mounted (%component-lifecycle-state instance)))

(defun component-children (instance)
  "Return a fresh stable list of INSTANCE's exact direct child capabilities."
  (check-type instance component)
  (copy-list (%component-children instance)))

(defun direct-child-p (parent child)
  "Return true when CHILD is the exact currently attached direct child of PARENT."
  (and (typep parent 'component)
       (typep child 'component)
       (eq (%component-parent child) parent)
       (let ((parent-id (%component-parent-id child)))
         (and parent-id (string= parent-id (%component-id parent))))
       (member child (%component-children parent) :test #'eq)
       t))

(defun ancestor-p (ancestor descendant)
  "Return true when ANCESTOR is in DESCENDANT's attached parent chain."
  (check-type ancestor component)
  (check-type descendant component)
  (let ((cursor (%component-parent descendant))
        (seen (make-hash-table :test #'eq)))
    (loop while cursor
          do (when (eq cursor ancestor) (return t))
             (when (gethash cursor seen)
               (error 'component-error :reason :component-parent-cycle))
             (setf (gethash cursor seen) t
                   cursor (%component-parent cursor))
          finally (return nil))))

(defun ensure-session-owner-for-mount (instance)
  (when (and (eq :session (%component-scope instance))
             (null (%component-owner-session-id instance)))
    (error 'invalid-component-definition
           :reason :missing-component-owner
           :definition-reason :missing-component-owner))
  t)

(defun lifecycle-error (instance operation reason &optional condition-type)
  (error (or condition-type 'component-lifecycle-error)
         :reason reason
         :component-id (component-id instance)
         :operation operation
         :state (%component-lifecycle-state instance)
         :lifecycle-reason reason))

(defun composition-error (reason)
  "Signal a bounded composition failure without exposing tree or session values."
  (error 'component-error :reason reason))

(defun topology-commit-no-lock (instance)
  "Record one tree topology commit while INSTANCE's lock is already held."
  (incf (%component-revision instance))
  (setf (%component-dirty-p instance) t
        (%component-last-access instance) (get-universal-time))
  (%component-revision instance))

(defun stable-unique-components (components)
  "Return exact COMPONENTS sorted by component id, rejecting ID aliases."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (instance components)
      (check-type instance component)
      (let* ((id (%component-id instance))
             (existing (gethash id table)))
        (when (and existing (not (eq existing instance)))
          (composition-error :component-identity-conflict))
        (setf (gethash id table) instance)))
    (let ((result nil))
      (maphash (lambda (id instance)
                 (declare (ignore id))
                 (push instance result))
               table)
      (sort result #'string< :key #'%component-id))))

(defun call-with-stable-component-locks (components thunk)
  "Acquire exact COMPONENTS once in stable ID order, then call THUNK."
  (labels ((acquire (remaining)
             (if (null remaining)
                 (funcall thunk)
                 (bordeaux-threads:with-lock-held
                     ((component-lock (first remaining)))
                   (acquire (rest remaining))))))
    (acquire (stable-unique-components components))))

(defun parent-chain-snapshot (instance)
  "Return INSTANCE through its attached parents, rejecting an existing cycle."
  (let ((chain nil)
        (cursor instance)
        (seen (make-hash-table :test #'eq)))
    (loop while cursor
          do (when (gethash cursor seen)
               (composition-error :component-parent-cycle))
             (setf (gethash cursor seen) t)
             (push cursor chain)
             (setf cursor (%component-parent cursor)))
    (nreverse chain)))

(defun same-component-chain-p (left right)
  (and (= (length left) (length right))
       (every #'eq left right)))

(defun ensure-composition-mounted (instance operation)
  (unless (eq :mounted (%component-lifecycle-state instance))
    (lifecycle-error instance operation :component-not-mounted 'component-not-mounted)))

(defun session-composition-compatible-p (parent child)
  "Return true when PARENT/CHILD can share one ownership namespace."
  (let ((parent-session-p (eq :session (%component-scope parent)))
        (child-session-p (eq :session (%component-scope child))))
    (cond
      ((or parent-session-p child-session-p)
       (and parent-session-p
            child-session-p
            (%component-owner-session-id parent)
            (%component-owner-session-id child)
            (string= (%component-owner-session-id parent)
                     (%component-owner-session-id child))))
      (t t))))

(defun child-with-id (parent child-id)
  "Return the exact child with CHILD-ID in PARENT's immutable snapshot, or NIL."
  (find child-id (%component-children parent)
        :key #'%component-id :test #'string=))

(defun sorted-child-snapshot-with (children child)
  "Return a new sorted child snapshot containing exact CHILD once."
  (sort (cons child (copy-list children)) #'string< :key #'%component-id))

(defun child-snapshot-without (children child)
  "Return a fresh child snapshot without exact CHILD."
  (remove child children :test #'eq))

(defun add-child (parent child)
  "Attach mounted CHILD to mounted PARENT using stable component lock ordering."
  (check-type parent component)
  (check-type child component)
  (when (eq parent child)
    (composition-error :component-cycle))
  (loop
    for chain = (parent-chain-snapshot parent)
    for retry = nil
    for result = nil
    do (call-with-stable-component-locks
        (cons child chain)
        (lambda ()
          (if (not (same-component-chain-p chain (parent-chain-snapshot parent)))
              (setf retry t)
              (progn
                (ensure-composition-mounted parent :add-child)
                (ensure-composition-mounted child :add-child)
                (unless (session-composition-compatible-p parent child)
                  (composition-error :cross-session-child))
                (when (member child chain :test #'eq)
                  (composition-error :component-cycle))
                (let* ((parent-id (%component-id parent))
                       (child-parent (%component-parent child))
                       (declared-parent-id (%component-parent-id child))
                       (existing (child-with-id parent (%component-id child))))
                  (when (and existing (not (eq existing child)))
                    (composition-error :component-identity-conflict))
                  (cond
                    ((and (eq child-parent parent)
                          existing
                          (or (null declared-parent-id)
                              (string= declared-parent-id parent-id)))
                     (when (null declared-parent-id)
                       (setf (%component-parent-id child) (copy-seq parent-id)))
                     (setf result child))
                    ((or child-parent
                         (and declared-parent-id
                              (not (string= declared-parent-id parent-id)))
                         existing)
                     (composition-error :child-already-owned))
                    (t
                     (setf (%component-children parent)
                           (sorted-child-snapshot-with
                            (%component-children parent) child)
                           (%component-parent child) parent
                           (%component-parent-id child) (copy-seq parent-id))
                     (topology-commit-no-lock parent)
                     (topology-commit-no-lock child)
                     (setf result child))))))))
       (unless retry (return result))))

(defun remove-child (parent child)
  "Detach exact CHILD from PARENT while leaving CHILD mounted."
  (check-type parent component)
  (check-type child component)
  (call-with-stable-component-locks
   (list parent child)
   (lambda ()
     (let* ((existing (child-with-id parent (%component-id child)))
            (child-parent (%component-parent child))
            (attached-p (and (eq child-parent parent) (eq existing child))))
       (when (and existing (not (eq existing child)))
         (composition-error :component-identity-conflict))
       (cond
         (attached-p
          (ensure-composition-mounted parent :remove-child)
          (ensure-composition-mounted child :remove-child)
          (setf (%component-children parent)
                (child-snapshot-without (%component-children parent) child)
                (%component-parent child) nil
                (%component-parent-id child) nil)
          (topology-commit-no-lock parent)
          (topology-commit-no-lock child)
          child)
         ((and child-parent (not (eq child-parent parent)))
          (composition-error :child-owned-by-other-parent))
         ((or existing
              (and (%component-parent-id child)
                   (string= (%component-parent-id child) (%component-id parent))))
          (composition-error :component-topology-inconsistent))
         (t child))))))

(defun mount-component (instance)
  "Transition INSTANCE from :CREATED to :MOUNTED and return INSTANCE plus status."
  (check-type instance component)
  (bordeaux-threads:with-lock-held ((component-lock instance))
    (ecase (%component-lifecycle-state instance)
      (:created
       (ensure-session-owner-for-mount instance)
       (setf (%component-lifecycle-state instance) :mounted
             (%component-last-access instance) (get-universal-time))
       (values instance :mounted))
      (:mounted
       (values instance :already-mounted))
      (:unmounted
       (lifecycle-error instance :mount :terminal-unmounted-state)))))

(defun subtree-snapshot (root)
  "Return a lock-free copy-on-write topology snapshot rooted at ROOT."
  (let ((entries nil)
        (seen (make-hash-table :test #'equal)))
    (labels ((visit (node)
               (let* ((id (%component-id node))
                      (existing (gethash id seen)))
                 (when existing
                   (if (eq existing node)
                       (composition-error :component-cycle)
                       (composition-error :component-identity-conflict)))
                 (setf (gethash id seen) node)
                 (let ((children (copy-list (%component-children node))))
                   (push (cons node children) entries)
                   (dolist (child children)
                     (visit child))))))
      (visit root))
    (nreverse entries)))

(defun same-child-snapshot-p (left right)
  (and (= (length left) (length right))
       (every #'eq left right)))

(defun subtree-snapshot-valid-p (entries external-parent root)
  "Return true while ENTRIES still describe the exact attached subtree."
  (and (eq (%component-parent root) external-parent)
       (every
        (lambda (entry)
          (let ((parent (car entry))
                (children (cdr entry)))
            (and (same-child-snapshot-p children (%component-children parent))
                 (every
                  (lambda (child)
                    (and (eq (%component-parent child) parent)
                         (%component-parent-id child)
                         (string= (%component-parent-id child)
                                  (%component-id parent))))
                  children))))
        entries)))

(defun snapshot-postorder (root entries)
  "Return subtree nodes child-first according to the validated snapshot."
  (let ((result nil))
    (labels ((visit (node)
               (let ((entry (find node entries :key #'car :test #'eq)))
                 (unless entry
                   (composition-error :component-topology-inconsistent))
                 (dolist (child (cdr entry))
                   (visit child))
                 (push node result))))
      (visit root))
    (nreverse result)))

(defun unmount-component (instance)
  "Unmount INSTANCE and all attached descendants using stable subtree locking."
  (check-type instance component)
  (loop
    for entries = (subtree-snapshot instance)
    for external-parent = (%component-parent instance)
    for nodes = (mapcar #'car entries)
    for retry = nil
    for outcome = nil
    do (call-with-stable-component-locks
        (if external-parent (cons external-parent nodes) nodes)
        (lambda ()
          (if (not (subtree-snapshot-valid-p entries external-parent instance))
              (setf retry t)
              (ecase (%component-lifecycle-state instance)
                (:created
                 (lifecycle-error instance :unmount :never-mounted))
                (:unmounted
                 (setf outcome (list instance :already-unmounted)))
                (:mounted
                 (dolist (node nodes)
                   (when (eq :created (%component-lifecycle-state node))
                     (composition-error :created-child-in-mounted-tree)))
                 (when external-parent
                   (let ((existing
                           (child-with-id external-parent (%component-id instance))))
                     (when (and existing (not (eq existing instance)))
                       (composition-error :component-identity-conflict))
                     (when (eq existing instance)
                       (setf (%component-children external-parent)
                             (child-snapshot-without
                              (%component-children external-parent) instance))
                       (when (mounted-p external-parent)
                         (topology-commit-no-lock external-parent)))))
                 (let ((now (get-universal-time)))
                   (dolist (node (snapshot-postorder instance entries))
                     (when (eq :mounted (%component-lifecycle-state node))
                       (setf (%component-lifecycle-state node) :unmounted
                             (%component-dirty-p node) nil
                             (%component-last-access node) now))
                     (setf (%component-children node) nil
                           (%component-parent node) nil
                           (%component-parent-id node) nil)))
                 (setf outcome (list instance :unmounted)))))))
       (unless retry
         (return (values-list outcome)))))

(defun touch-component (instance)
  "Atomically record one committed state change on mounted INSTANCE."
  (check-type instance component)
  (bordeaux-threads:with-lock-held ((component-lock instance))
    (unless (eq :mounted (%component-lifecycle-state instance))
      (lifecycle-error instance
                       :touch
                       :component-not-mounted
                       'component-not-mounted))
    (incf (%component-revision instance))
    (setf (%component-dirty-p instance) t
          (%component-last-access instance) (get-universal-time))
    (%component-revision instance)))

(setf (documentation 'component-error-reason 'function)
      "Return the bounded reason keyword carried by a COMPONENT-ERROR.")
(setf (documentation 'invalid-component-definition-reason 'function)
      "Return the bounded reason keyword describing an invalid component definition.")
(setf (documentation 'component-lifecycle-error-component-id 'function)
      "Return the stable component ID captured by a lifecycle error.")
(setf (documentation 'component-lifecycle-error-operation 'function)
      "Return the lifecycle operation keyword that was rejected.")
(setf (documentation 'component-lifecycle-error-state 'function)
      "Return the lifecycle state observed when an operation was rejected.")
(setf (documentation 'component-lifecycle-error-reason 'function)
      "Return the bounded lifecycle rejection reason keyword.")
(setf (documentation 'render-method-missing-component-id 'function)
      "Return the component ID for which no render method was available.")
(setf (documentation 'render-method-missing-class 'function)
      "Return the class name for which no render method was available.")
(setf (documentation 'component-lock 'function)
      "Return INSTANCE's per-component Bordeaux Threads lock. Multi-component operations use stable component-id ordering.")
