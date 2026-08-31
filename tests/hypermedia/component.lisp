;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime component core tests                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defclass hm-020-probe-component (clog-hypermedia:component)
  ((value
    :initform 0
    :accessor hm-020-probe-value)))

(defmethod clog-hypermedia:handle-action
    ((instance hm-020-probe-component) (action (eql :increment)) request-context)
  (declare (ignore request-context))
  (incf (hm-020-probe-value instance)))

(defmethod clog-hypermedia:render-component
    ((instance hm-020-probe-component) context)
  (declare (ignore context))
  (format nil "<div id=\"~A\">~D</div>"
          (clog-hypermedia:component-id instance)
          (hm-020-probe-value instance)))

(defun hm-020-valid-component-id-p (value)
  "Return true when VALUE matches the frozen component ID grammar."
  (and (stringp value)
       (= 39 (length value))
       (string= "clog-c-" value :end2 7)
       (loop for index from 7 below 39
             for character = (char value index)
             always (or (char<= #\0 character #\9)
                        (char<= #\a character #\f)))))

(defun hm-020-make-component (&key (owner "hm020-session") id scope)
  "Construct a probe component with explicit session ownership by default."
  (apply #'make-instance
         'hm-020-probe-component
         (append (when id (list :id id))
                 (when scope (list :scope scope))
                 (when owner (list :owner-session-id owner)))))

(test component/id/format-random-collision-and-session-opacity
  (let ((ids (make-hash-table :test #'equal))
        (session-a "session-alpha-private")
        (session-b "session-beta-private"))
    (loop repeat 2048
          for owner = (if (oddp (hash-table-count ids)) session-a session-b)
          for instance = (hm-020-make-component :owner owner)
          for id = (clog-hypermedia:component-id instance)
          do (is-true (hm-020-valid-component-id-p id))
             (is-false (search owner id :test #'char=))
             (setf (gethash id ids) t))
    (is (= 2048 (hash-table-count ids))
        "A 2K property sample across two owner sessions must not collide.")))

(test component/id/deterministic-generator-is-test-injectable
  (let* ((counter 0)
         (clog-component::*component-id-generator*
           (lambda ()
             (string-downcase
              (format nil "clog-c-~32,'0x" (incf counter))))))
    (let ((first (hm-020-make-component :owner "deterministic-a"))
          (second (hm-020-make-component :owner "deterministic-b")))
      (is (string= "clog-c-00000000000000000000000000000001"
                   (clog-hypermedia:component-id first)))
      (is (string= "clog-c-00000000000000000000000000000002"
                   (clog-hypermedia:component-id second))))))

(test component/id/definition-validation-and-defensive-ownership
  (signals clog-hypermedia:invalid-component-definition
    (hm-020-make-component
     :id "component-1"
     :owner "owner"))
  (signals clog-hypermedia:invalid-component-definition
    (hm-020-make-component
     :owner "owner"
     :scope :unknown))
  (let* ((owner (copy-seq "session-owner"))
         (instance (hm-020-make-component :owner owner)))
    (setf (char owner 0) #\X)
    (is (string= "session-owner"
                 (clog-component:component-owner-session-id instance)))
    (let ((returned (clog-component:component-owner-session-id instance)))
      (setf (char returned 0) #\Y)
      (is (string= "session-owner"
                   (clog-component:component-owner-session-id instance))))))

(test component/lifecycle/state-machine-and-repeated-call-contract
  (let ((instance (hm-020-make-component)))
    (is (eq :created
            (clog-hypermedia:component-lifecycle-state instance)))
    (is-false (clog-hypermedia:mounted-p instance))
    (is (= 0 (clog-hypermedia:component-revision instance)))
    (signals clog-hypermedia:component-not-mounted
      (clog-hypermedia:touch-component instance))
    (multiple-value-bind (mounted result)
        (clog-hypermedia:mount-component instance)
      (is (eq instance mounted))
      (is (eq :mounted result)))
    (is-true (clog-hypermedia:mounted-p instance))
    (is (eq :mounted
            (clog-hypermedia:component-lifecycle-state instance)))
    (multiple-value-bind (mounted result)
        (clog-hypermedia:mount-component instance)
      (is (eq instance mounted))
      (is (eq :already-mounted result)))
    (is (= 1 (clog-hypermedia:handle-action instance :increment nil)))
    (multiple-value-bind (unmounted result)
        (clog-hypermedia:unmount-component instance)
      (is (eq instance unmounted))
      (is (eq :unmounted result)))
    (is-false (clog-hypermedia:mounted-p instance))
    (is (eq :unmounted
            (clog-hypermedia:component-lifecycle-state instance)))
    (multiple-value-bind (unmounted result)
        (clog-hypermedia:unmount-component instance)
      (is (eq instance unmounted))
      (is (eq :already-unmounted result)))
    (signals clog-hypermedia:component-not-mounted
      (clog-hypermedia:handle-action instance :increment nil))
    (signals clog-hypermedia:component-not-mounted
      (clog-hypermedia:touch-component instance))
    (signals clog-hypermedia:component-lifecycle-error
      (clog-hypermedia:mount-component instance)))
  (let ((never-mounted (hm-020-make-component)))
    (signals clog-hypermedia:component-lifecycle-error
      (clog-hypermedia:unmount-component never-mounted)))
  (let ((missing-owner
          (make-instance 'hm-020-probe-component :scope :session)))
    (signals clog-hypermedia:invalid-component-definition
      (clog-hypermedia:mount-component missing-owner)))
  (let ((application-scope
          (make-instance 'hm-020-probe-component :scope :application)))
    (is (eq :mounted
            (nth-value 1
              (clog-hypermedia:mount-component application-scope))))))

(test component/protocol/defaults-and-render-method-contract
  (let ((base
          (make-instance 'clog-hypermedia:component
                         :owner-session-id "base-session"))
        (probe (hm-020-make-component)))
    (is-true (clog-hypermedia:authorize-action-p probe :anything nil))
    (is (null (clog-hypermedia:validate-action probe :anything nil)))
    (is (null (clog-hypermedia:component-title probe nil)))
    (is (null (clog-hypermedia:component-assets probe nil)))
    (is (null (clog-hypermedia:after-mount probe nil)))
    (is (null (clog-hypermedia:before-unmount probe nil)))
    (signals clog-hypermedia:render-method-missing
      (clog-hypermedia:render-component base nil))))

(test component/revision/monotonic-committed-touch
  (let ((instance (hm-020-make-component)))
    (clog-hypermedia:mount-component instance)
    (let ((revisions
            (loop repeat 100
                  collect (clog-hypermedia:touch-component instance))))
      (is (equal (loop for revision from 1 to 100 collect revision)
                 revisions))
      (is (= 100 (clog-hypermedia:component-revision instance)))
      (is-true (clog-component:component-dirty-p instance)))
    (is-false
     (fboundp '(setf clog-hypermedia:component-revision))
     "The public revision API must not expose a rollback-capable SETF writer.")))

(test component/concurrency/touch-is-per-component-and-lossless
  (let* ((instance (hm-020-make-component))
         (other (hm-020-make-component :owner "other-session"))
         (thread-count 8)
         (touches-per-thread 250))
    (is-false (eq (clog-component:component-lock instance)
                  (clog-component:component-lock other))
              "Each component must own a distinct mutation lock.")
    (clog-hypermedia:mount-component instance)
    (let ((threads
            (loop repeat thread-count
                  collect
                  (bordeaux-threads:make-thread
                   (lambda ()
                     (loop repeat touches-per-thread
                           do (clog-hypermedia:touch-component instance)))
                   :name "hm020-concurrent-touch"))))
      (dolist (thread threads)
        (bordeaux-threads:join-thread thread)))
    (is (= (* thread-count touches-per-thread)
           (clog-hypermedia:component-revision instance))
        "Concurrent component touches must not lose committed revisions.")))
