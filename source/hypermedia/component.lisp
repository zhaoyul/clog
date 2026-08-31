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
   "Base condition for component identity, lifecycle and protocol failures."))

(define-condition invalid-component-definition (component-error)
  ((definition-reason
    :initarg :definition-reason
    :reader invalid-component-definition-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid component definition (~A)."
             (invalid-component-definition-reason condition))))
  (:documentation
   "Signaled when a component is constructed or mounted with invalid identity, scope or ownership metadata.

The condition intentionally stores only a bounded reason keyword. Session IDs
and other owner values are not retained in the condition."))

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
   "Signaled when a lifecycle transition is not permitted by the frozen created/mounted/unmounted state machine."))

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
  "Lock protecting the process-local random-state object used for ID creation.

This lock is never used for component mutation, rendering or network I/O. It
therefore cannot become an application-wide component/render lock.")

(defun default-component-id-generator ()
  "Return a new UUID-style CLOG component ID containing 128 random bits.

The generator consumes one value from a process-local random state while
holding only the short-lived ID-generator lock. The resulting string has the
frozen `clog-c-` plus 32 lowercase hexadecimal format. Component IDs are opaque
identifiers, never authorization credentials."
  (let ((value
          (bordeaux-threads:with-lock-held (*component-id-generator-lock*)
            (random +component-id-random-limit+
                    *component-id-random-state*))))
    (concatenate 'string
                 +component-id-prefix+
                 (string-downcase (format nil "~32,'0x" value)))))

(defparameter *component-id-generator* #'default-component-id-generator
  "Dynamically bindable component ID generator.

Production code uses DEFAULT-COMPONENT-ID-GENERATOR. Tests may dynamically bind
this special variable to a deterministic zero-argument function. Applications
must not derive IDs from session IDs, user IDs or business keys.")

(defun lowercase-hex-character-p (character)
  "Return true when CHARACTER is a lowercase hexadecimal digit."
  (or (char<= #\0 character #\9)
      (char<= #\a character #\f)))

(defun valid-component-id-p (value)
  "Return true when VALUE follows the frozen CLOG component ID format."
  (and (stringp value)
       (= (length value) 39)
       (string= +component-id-prefix+ value :end2 7)
       (loop for index from 7 below 39
             always (lowercase-hex-character-p (char value index)))))

(defun opaque-owner-string-p (value)
  "Return true for a non-empty owner/session identifier without control bytes."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun copy-opaque-value (value)
  "Copy mutable string VALUE while preserving other opaque values."
  (if (stringp value) (copy-seq value) value))

(defun copy-hash-table-shallow (table)
  "Return a shallow copy of TABLE without exposing its mutable registry object."
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
    :reader %component-parent-id)
   (owner-session-id
    :initarg :owner-session-id
    :initform nil
    :reader %component-owner-session-id)
   (children
    :initform (make-hash-table :test #'equal)
    :reader %component-children)
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
   "Base server-side UI component with stable identity and per-instance synchronization.

A component starts in :CREATED with revision zero and dirty state true. A
successful MOUNT-COMPONENT transition makes it :MOUNTED. UNMOUNT-COMPONENT is
terminal for HM-020 and changes it to :UNMOUNTED. TOUCH-COMPONENT records one
committed state change by atomically increasing revision, marking the component
dirty and refreshing its last-access timestamp.

Each instance owns exactly one Bordeaux Threads lock. HM-020 lifecycle helpers
never acquire two component locks at once and never perform rendering or network
I/O while holding this lock."))

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
  (setf (slot-value instance 'id) (copy-seq (%component-id instance)))
  (setf (slot-value instance 'key) (copy-opaque-value (%component-key instance)))
  (let ((parent-id (%component-parent-id instance)))
    (when parent-id
      (setf (slot-value instance 'parent-id) (copy-seq parent-id))))
  (let ((owner (%component-owner-session-id instance)))
    (when owner
      (setf (slot-value instance 'owner-session-id) (copy-seq owner)))))

(defun component-id (instance)
  "Return a fresh copy of INSTANCE's stable `clog-c-...` identifier.

This function does not modify component state and does not acquire the component
lock because the identity slot is immutable after initialization."
  (check-type instance component)
  (copy-seq (%component-id instance)))

(defun component-key (instance)
  "Return INSTANCE's optional application key, defensively copying string keys."
  (check-type instance component)
  (copy-opaque-value (%component-key instance)))

(defun component-scope (instance)
  "Return INSTANCE's lifecycle scope keyword.

HM-020 recognizes :REQUEST, :SESSION, :APPLICATION and :PERSISTENT. The latter
is a declared scope only; persistence itself is intentionally deferred."
  (check-type instance component)
  (%component-scope instance))

(defun component-parent-id (instance)
  "Return a fresh copy of INSTANCE's optional parent component ID."
  (check-type instance component)
  (let ((value (%component-parent-id instance)))
    (and value (copy-seq value))))

(defun component-owner-session-id (instance)
  "Return a fresh copy of INSTANCE's owner session ID, or NIL for public scopes.

The value is an ownership namespace input for the later component store. It is
not included in the component ID and is never treated as a browser-visible
credential."
  (check-type instance component)
  (let ((value (%component-owner-session-id instance)))
    (and value (copy-seq value))))

(defun component-revision (instance)
  "Return INSTANCE's current committed non-negative revision snapshot.

Writes occur only under COMPONENT-LOCK through TOUCH-COMPONENT in HM-020. Code
requiring a multi-slot atomic snapshot may explicitly hold COMPONENT-LOCK before
reading internal state; this reader itself never recursively acquires the lock."
  (check-type instance component)
  (%component-revision instance))

(defun component-dirty-p (instance)
  "Return true when INSTANCE has state requiring a future representation refresh."
  (check-type instance component)
  (%component-dirty-p instance))

(defun component-lifecycle-state (instance)
  "Return one of :CREATED, :MOUNTED or :UNMOUNTED for INSTANCE."
  (check-type instance component)
  (%component-lifecycle-state instance))

(defun component-last-access (instance)
  "Return INSTANCE's latest lifecycle/touch universal-time snapshot."
  (check-type instance component)
  (%component-last-access instance))

(defun mounted-p (instance)
  "Return true only while INSTANCE is in the :MOUNTED lifecycle state.

This is a lock-free snapshot read. Lifecycle transitions themselves are
serialized by INSTANCE's per-component lock."
  (check-type instance component)
  (eq :mounted (%component-lifecycle-state instance)))

(defun ensure-session-owner-for-mount (instance)
  "Reject a session-scoped INSTANCE without an opaque owner session ID."
  (when (and (eq :session (%component-scope instance))
             (null (%component-owner-session-id instance)))
    (error 'invalid-component-definition
           :reason :missing-component-owner
           :definition-reason :missing-component-owner))
  t)

(defun lifecycle-error (instance operation reason &optional condition-type)
  "Signal a typed lifecycle condition for INSTANCE without exposing owner data."
  (error (or condition-type 'component-lifecycle-error)
         :reason reason
         :component-id (component-id instance)
         :operation operation
         :state (%component-lifecycle-state instance)
         :lifecycle-reason reason))

(defun mount-component (instance)
  "Transition INSTANCE from :CREATED to :MOUNTED and return two values.

The primary value is INSTANCE. The secondary value is :MOUNTED for the first
successful transition or :ALREADY-MOUNTED for an idempotent repeated call.
Session-scoped components must already carry OWNER-SESSION-ID. Attempting to
remount a terminal :UNMOUNTED component signals COMPONENT-LIFECYCLE-ERROR.

Only INSTANCE's own lock is acquired, and no user hook, registry operation,
rendering or network I/O occurs while it is held. AFTER-MOUNT orchestration is
therefore left to the later store/application layer, matching the frozen
mount -> register -> after-mount lifecycle order."
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

(defun unmount-component (instance)
  "Transition mounted INSTANCE to terminal :UNMOUNTED and return two values.

The secondary value is :UNMOUNTED for the first transition and
:ALREADY-UNMOUNTED for a repeated idempotent call. Unmounting a never-mounted
:CREATED component signals COMPONENT-LIFECYCLE-ERROR. HM-020 does not yet own a
registry or child tree, so BEFORE-UNMOUNT and recursive cleanup are orchestrated
by the later store/composition tasks.

Only INSTANCE's own component lock is acquired."
  (check-type instance component)
  (bordeaux-threads:with-lock-held ((component-lock instance))
    (ecase (%component-lifecycle-state instance)
      (:created
       (lifecycle-error instance :unmount :never-mounted))
      (:mounted
       (setf (%component-lifecycle-state instance) :unmounted
             (%component-dirty-p instance) nil
             (%component-last-access instance) (get-universal-time))
       (values instance :unmounted))
      (:unmounted
       (values instance :already-unmounted)))))

(defun touch-component (instance)
  "Atomically record one committed state change on mounted INSTANCE.

Under INSTANCE's per-component lock this function increments the non-negative
revision exactly once, marks the component dirty and refreshes last-access.
The new revision is returned. Calling it before mount or after unmount signals
COMPONENT-NOT-MOUNTED.

HM-020 never nests component locks. Later multi-component transactions must use
the architecture's lexical COMPONENT-ID lock order rather than hash iteration."
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
      "Return INSTANCE's per-component Bordeaux Threads lock. Never acquire multiple component locks without stable component-id ordering.")
