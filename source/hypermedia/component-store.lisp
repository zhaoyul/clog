;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime session-scoped component store              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-component
  (:export
   #:component-store-error #:component-store-error-reason
   #:component-not-found #:component-store-ownership-error
   #:component-store-conflict
   #:component-store-limit-exceeded #:component-store-limit-exceeded-limit
   #:component-store-operation-unsupported
   #:component-store #:component-store-p
   #:memory-component-store #:make-memory-component-store
   #:component-store-max-components-per-session
   #:component-store-component-ttl-seconds
   #:ensure-component-registry #:store-component #:load-component
   #:delete-component #:delete-session-components #:enumerate-components
   #:touch-stored-component #:sweep-component-store #:component-store-stats
   #:register-component #:find-component #:remove-component #:sweep-components))

(defpackage #:clog-hypermedia
  (:import-from #:clog-component
   #:component-store-error #:component-store-error-reason
   #:component-not-found #:component-store-ownership-error
   #:component-store-conflict
   #:component-store-limit-exceeded #:component-store-limit-exceeded-limit
   #:component-store-operation-unsupported
   #:component-store #:component-store-p
   #:memory-component-store #:make-memory-component-store
   #:component-store-max-components-per-session
   #:component-store-component-ttl-seconds
   #:ensure-component-registry #:store-component #:load-component
   #:delete-component #:delete-session-components #:enumerate-components
   #:touch-stored-component #:sweep-component-store #:component-store-stats
   #:register-component #:find-component #:remove-component #:sweep-components)
  (:export
   #:component-store-error #:component-store-error-reason
   #:component-not-found #:component-store-ownership-error
   #:component-store-conflict
   #:component-store-limit-exceeded #:component-store-limit-exceeded-limit
   #:component-store-operation-unsupported
   #:component-store #:component-store-p
   #:memory-component-store #:make-memory-component-store
   #:component-store-max-components-per-session
   #:component-store-component-ttl-seconds
   #:ensure-component-registry #:store-component #:load-component
   #:delete-component #:delete-session-components #:enumerate-components
   #:touch-stored-component #:sweep-component-store #:component-store-stats
   #:register-component #:find-component #:remove-component #:sweep-components))

(in-package #:clog-component)

(define-condition component-store-error (clog-http:clog-hypermedia-error)
  ((reason :initarg :reason :initform nil :reader component-store-error-reason))
  (:report (lambda (condition stream)
             (format stream "Component store operation failed~@[ (~A)~]."
                     (component-store-error-reason condition))))
  (:documentation "Base condition for component-store failures."))

(define-condition component-not-found (component-store-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Component was not found in the current session." stream)))
  (:documentation
   "A session-local lookup failed. The condition intentionally reveals no other namespace."))

(define-condition component-store-ownership-error (component-store-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "Component ownership is incompatible with this session store."
                           stream))))

(define-condition component-store-conflict (component-store-error) ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (write-string "A different component already uses this session-local ID."
                           stream))))

(define-condition component-store-limit-exceeded (component-store-error)
  ((limit :initarg :limit :reader component-store-limit-exceeded-limit))
  (:report (lambda (condition stream)
             (format stream "The session component limit of ~D has been reached."
                     (component-store-limit-exceeded-limit condition)))))

(define-condition component-store-operation-unsupported (component-store-error) ())

(defclass component-store () ()
  (:documentation "Abstract protocol root for Hypermedia component stores."))

(defclass component-registry ()
  ((components :initform (make-hash-table :test #'equal)
               :reader registry-components)
   (access-times :initform (make-hash-table :test #'equal)
                 :reader registry-access-times)
   (lock :initform (bordeaux-threads:make-lock "clog-component-registry")
         :reader registry-lock)
   (created-at :initarg :created-at :reader registry-created-at)
   (last-access :initarg :last-access :accessor registry-last-access)
   (active-p :initform t :accessor registry-active-p)))

(defclass memory-component-store (component-store)
  ((registries :initform (make-hash-table :test #'equal)
               :reader store-registries)
   (lock :initform (bordeaux-threads:make-lock "clog-component-store")
         :reader store-lock)
   (clock :initarg :clock :reader store-clock)
   (max-components-per-session
    :initarg :max-components-per-session
    :reader component-store-max-components-per-session)
   (component-ttl-seconds
    :initarg :component-ttl-seconds
    :reader component-store-component-ttl-seconds)
   (created-at :initarg :created-at :reader store-created-at))
  (:documentation
   "Thread-safe process-local store. The top lock protects only the registry map;
registry locks protect membership and TTL metadata. Rendering, user handlers,
network writes and component unmount hooks never run while those locks are held."))

(defun component-store-p (value)
  "Return true when VALUE is a COMPONENT-STORE."
  (typep value 'component-store))

(defun safe-session-id-p (value)
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 4096)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun safe-component-id-p (value)
  (and (stringp value)
       (= (length value) 39)
       (string= "clog-c-" value :end2 7)
       (loop for index from 7 below 39
             for character = (char value index)
             always (or (char<= #\0 character #\9)
                        (char<= #\a character #\f)))))

(defun checked-session-id (value)
  (unless (safe-session-id-p value)
    (error 'component-store-error :reason :invalid-session-id))
  (copy-seq value))

(defun checked-component-id (value)
  (unless (safe-component-id-p value)
    (error 'component-store-error :reason :invalid-component-id))
  (copy-seq value))

(defun checked-positive-integer (value reason)
  (unless (and (integerp value) (plusp value))
    (error 'component-store-error :reason reason))
  value)

(defun call-store-clock (clock)
  (handler-case
      (let ((value (funcall clock)))
        (unless (and (realp value) (not (minusp value)))
          (error 'component-store-error :reason :invalid-clock-value))
        value)
    (component-store-error (condition) (error condition))
    (error () (error 'component-store-error :reason :clock-failed))))

(defun store-time (store)
  (call-store-clock (store-clock store)))

(defun make-memory-component-store
    (&key (clock #'get-universal-time) configuration
          (max-components-per-session nil max-supplied-p)
          (component-ttl-seconds nil ttl-supplied-p))
  "Create an in-memory store with explicit limits and an injectable clock.

CONFIGURATION supplies defaults unless an explicit limit is present. No thread
is started; SWEEP-COMPONENT-STORE is invoked by an owned lifecycle or job."
  (unless (functionp clock)
    (error 'component-store-error :reason :invalid-clock))
  (when configuration
    (check-type configuration clog-hypermedia:hypermedia-configuration))
  (let ((maximum
          (cond (max-supplied-p max-components-per-session)
                (configuration
                 (clog-hypermedia:configuration-max-components-per-session
                  configuration))
                (t 2048)))
        (ttl
          (cond (ttl-supplied-p component-ttl-seconds)
                (configuration
                 (clog-hypermedia:configuration-component-ttl-seconds
                  configuration))
                (t 1800))))
    (checked-positive-integer maximum :invalid-component-limit)
    (checked-positive-integer ttl :invalid-component-ttl)
    (make-instance 'memory-component-store
                   :clock clock
                   :max-components-per-session maximum
                   :component-ttl-seconds ttl
                   :created-at (call-store-clock clock))))

(defgeneric ensure-component-registry (store session-id)
  (:documentation "Return or atomically create SESSION-ID's private registry."))
(defgeneric store-component (store session-id component)
  (:documentation "Store a mounted session-owned component and return it plus status."))
(defgeneric load-component (store session-id component-id)
  (:documentation "Load from one session namespace and refresh TTL, or return NIL."))
(defgeneric delete-component (store session-id component-id)
  (:documentation "Remove one component and unmount it after store locks are released."))
(defgeneric delete-session-components (store session-id)
  (:documentation "Detach, drain and unmount a complete session registry."))
(defgeneric enumerate-components (store session-id)
  (:documentation "Return a component-ID-sorted session-local snapshot."))
(defgeneric touch-stored-component (store session-id component-id)
  (:documentation "Refresh store TTL without changing component revision."))
(defgeneric sweep-component-store (store configuration)
  (:documentation "Synchronously remove expired entries using CONFIGURATION TTL."))
(defgeneric component-store-stats (store)
  (:documentation "Return aggregate counts and limits without session identifiers."))

(defun unsupported (operation)
  (error 'component-store-operation-unsupported :reason operation))

(defmethod ensure-component-registry ((store component-store) session-id)
  (declare (ignore store session-id)) (unsupported :ensure-component-registry))
(defmethod store-component ((store component-store) session-id component)
  (declare (ignore store session-id component)) (unsupported :store-component))
(defmethod load-component ((store component-store) session-id component-id)
  (declare (ignore store session-id component-id)) (unsupported :load-component))
(defmethod delete-component ((store component-store) session-id component-id)
  (declare (ignore store session-id component-id)) (unsupported :delete-component))
(defmethod delete-session-components ((store component-store) session-id)
  (declare (ignore store session-id)) (unsupported :delete-session-components))
(defmethod enumerate-components ((store component-store) session-id)
  (declare (ignore store session-id)) (unsupported :enumerate-components))
(defmethod touch-stored-component ((store component-store) session-id component-id)
  (declare (ignore store session-id component-id)) (unsupported :touch-stored-component))
(defmethod sweep-component-store ((store component-store) configuration)
  (declare (ignore store configuration)) (unsupported :sweep-component-store))
(defmethod component-store-stats ((store component-store))
  (declare (ignore store)) (unsupported :component-store-stats))

(defun make-registry (store)
  (let ((now (store-time store)))
    (make-instance 'component-registry :created-at now :last-access now)))

(defun lookup-registry (store session-id)
  (bordeaux-threads:with-lock-held ((store-lock store))
    (gethash session-id (store-registries store))))

(defmethod ensure-component-registry ((store memory-component-store) session-id)
  (let ((session-id (checked-session-id session-id)))
    (or (lookup-registry store session-id)
        (let ((candidate (make-registry store)))
          (bordeaux-threads:with-lock-held ((store-lock store))
            (or (gethash session-id (store-registries store))
                (setf (gethash (copy-seq session-id)
                               (store-registries store))
                      candidate)))))))

(defun owned-by-session-p (component session-id)
  (let ((owner (component-owner-session-id component)))
    (and (eq :session (component-scope component))
         owner
         (string= owner session-id))))

(defun require-owner (component session-id)
  (unless (owned-by-session-p component session-id)
    (error 'component-store-ownership-error :reason :session-owner-mismatch)))

(defun usable-p (component session-id)
  (and (typep component 'component)
       (mounted-p component)
       (owned-by-session-p component session-id)))

(defun record-access (registry component-id now)
  (setf (gethash component-id (registry-access-times registry)) now
        (registry-last-access registry) now))

(defmethod store-component
    ((store memory-component-store) session-id (component component))
  (let ((session-id (checked-session-id session-id)))
    (require-owner component session-id)
    (unless (mounted-p component)
      (error 'component-not-mounted
             :reason :store-requires-mounted-component
             :component-id (component-id component)
             :operation :store
             :state (component-lifecycle-state component)
             :lifecycle-reason :store-requires-mounted-component))
    (let ((component-id (component-id component)))
      (loop
        for registry = (ensure-component-registry store session-id)
        for now = (store-time store)
        do (multiple-value-bind (stored status)
               (bordeaux-threads:with-lock-held ((registry-lock registry))
                 (cond
                   ((not (registry-active-p registry)) (values nil :retry))
                   ((eq component
                        (gethash component-id (registry-components registry)))
                    (record-access registry component-id now)
                    (values component :already-stored))
                   ((gethash component-id (registry-components registry))
                    (error 'component-store-conflict
                           :reason :duplicate-component-id))
                   ((>= (hash-table-count (registry-components registry))
                        (component-store-max-components-per-session store))
                    (error 'component-store-limit-exceeded
                           :reason :session-component-limit
                           :limit (component-store-max-components-per-session store)))
                   (t
                    (setf (gethash (copy-seq component-id)
                                   (registry-components registry))
                          component)
                    (record-access registry component-id now)
                    (values component :stored))))
             (unless (eq status :retry)
               (return (values stored status))))))))

(defmethod load-component ((store memory-component-store) session-id component-id)
  (let ((session-id (checked-session-id session-id))
        (component-id (checked-component-id component-id))
        (inactive (list :inactive)))
    (loop
      for registry = (lookup-registry store session-id)
      for now = (store-time store)
      do (unless registry (return nil))
         (let ((result inactive))
           (bordeaux-threads:with-lock-held ((registry-lock registry))
             (when (registry-active-p registry)
               (let ((component
                       (gethash component-id (registry-components registry))))
                 (if (usable-p component session-id)
                     (progn (record-access registry component-id now)
                            (setf result component))
                     (progn
                       (when component
                         (remhash component-id (registry-components registry))
                         (remhash component-id (registry-access-times registry)))
                       (setf result nil))))))
           (unless (eq result inactive) (return result))))))

(defmethod touch-stored-component
    ((store memory-component-store) session-id component-id)
  (load-component store session-id component-id))

(defun sorted-component-list (components)
  (sort components #'string< :key #'component-id))

(defmethod enumerate-components ((store memory-component-store) session-id)
  (let ((session-id (checked-session-id session-id))
        (inactive (list :inactive)))
    (loop
      for registry = (lookup-registry store session-id)
      do (unless registry (return nil))
         (let ((result inactive))
           (bordeaux-threads:with-lock-held ((registry-lock registry))
             (when (registry-active-p registry)
               (let ((usable nil) (discard nil))
                 (maphash (lambda (id component)
                            (if (usable-p component session-id)
                                (push component usable)
                                (push id discard)))
                          (registry-components registry))
                 (dolist (id discard)
                   (remhash id (registry-components registry))
                   (remhash id (registry-access-times registry)))
                 (setf result (sorted-component-list usable)))))
           (unless (eq result inactive) (return result))))))

(defun unmount-outside-store-locks (components)
  (dolist (component components)
    (when (mounted-p component) (unmount-component component)))
  components)

(defun prune-unusable-registry-components (registry session-id)
  "Immediately remove unusable entries after an out-of-lock lifecycle cascade."
  (bordeaux-threads:with-lock-held ((registry-lock registry))
    (when (registry-active-p registry)
      (let ((discard nil))
        (maphash (lambda (id component)
                   (unless (usable-p component session-id)
                     (push id discard)))
                 (registry-components registry))
        (dolist (id discard)
          (remhash id (registry-components registry))
          (remhash id (registry-access-times registry)))
        (length discard)))))

(defmethod delete-component ((store memory-component-store) session-id component-id)
  (let ((session-id (checked-session-id session-id))
        (component-id (checked-component-id component-id))
        (inactive (list :inactive)))
    (loop
      for registry = (lookup-registry store session-id)
      for now = (store-time store)
      do (unless registry (return (values nil :not-found)))
         (let ((result inactive))
           (bordeaux-threads:with-lock-held ((registry-lock registry))
             (when (registry-active-p registry)
               (let ((component
                       (gethash component-id (registry-components registry))))
                 (when component
                   (remhash component-id (registry-components registry))
                   (remhash component-id (registry-access-times registry)))
                 (setf (registry-last-access registry) now
                       result (and (usable-p component session-id) component)))))
           (unless (eq result inactive)
             (if result
                 (progn
                   (unmount-outside-store-locks (list result))
                   ;; UNMOUNT-COMPONENT may cascade through children that are
                   ;; separately present in this same session registry. Re-enter
                   ;; the registry only after all component locks are released and
                   ;; remove those now-unusable descendants immediately.
                   (prune-unusable-registry-components registry session-id)
                   (return (values result :deleted)))
                 (return (values nil :not-found))))))))

(defun detach-registry (store session-id)
  (bordeaux-threads:with-lock-held ((store-lock store))
    (let ((registry (gethash session-id (store-registries store))))
      (when registry (remhash session-id (store-registries store)))
      registry)))

(defun drain-registry (registry)
  (bordeaux-threads:with-lock-held ((registry-lock registry))
    (setf (registry-active-p registry) nil)
    (let ((components nil))
      (maphash (lambda (id component)
                 (declare (ignore id))
                 (push component components))
               (registry-components registry))
      (clrhash (registry-components registry))
      (clrhash (registry-access-times registry))
      (sorted-component-list components))))

(defmethod delete-session-components ((store memory-component-store) session-id)
  (let* ((session-id (checked-session-id session-id))
         (registry (detach-registry store session-id))
         (components (and registry (drain-registry registry))))
    (unmount-outside-store-locks components)))

(defun registry-map-snapshot (store)
  (bordeaux-threads:with-lock-held ((store-lock store))
    (let ((snapshot nil))
      (maphash (lambda (session-id registry)
                 (push (cons (copy-seq session-id) registry) snapshot))
               (store-registries store))
      snapshot)))

(defun expired-p (now last-access ttl)
  (>= (- now last-access) ttl))

(defun sweep-one-registry (registry session-id now ttl)
  (let ((removed nil) (empty-expired-p nil))
    (bordeaux-threads:with-lock-held ((registry-lock registry))
      (when (registry-active-p registry)
        (let ((expired-ids nil))
          (maphash
           (lambda (id component)
             (let ((last-access
                     (gethash id (registry-access-times registry)
                              (registry-created-at registry))))
               (when (or (not (usable-p component session-id))
                         (expired-p now last-access ttl))
                 (push id expired-ids))))
           (registry-components registry))
          (dolist (id expired-ids)
            (let ((component (gethash id (registry-components registry))))
              (when component (push component removed)))
            (remhash id (registry-components registry))
            (remhash id (registry-access-times registry)))
          (setf empty-expired-p
                (and (zerop (hash-table-count (registry-components registry)))
                     (expired-p now (registry-last-access registry) ttl))))))
    (values removed empty-expired-p)))

(defun retire-empty-registry (store session-id registry now ttl)
  (bordeaux-threads:with-lock-held ((store-lock store))
    (when (eq registry (gethash session-id (store-registries store)))
      (bordeaux-threads:with-lock-held ((registry-lock registry))
        (when (and (registry-active-p registry)
                   (zerop (hash-table-count (registry-components registry)))
                   (expired-p now (registry-last-access registry) ttl))
          (setf (registry-active-p registry) nil)
          (remhash session-id (store-registries store))
          t)))))

(defun sweep-with-ttl (store ttl)
  (let ((now (store-time store)) (removed nil) (registry-count 0))
    (dolist (entry (registry-map-snapshot store))
      (multiple-value-bind (expired empty-expired-p)
          (sweep-one-registry (cdr entry) (car entry) now ttl)
        (setf removed (nconc removed expired))
        (when (and empty-expired-p
                   (retire-empty-registry store (car entry) (cdr entry) now ttl))
          (incf registry-count))))
    (unmount-outside-store-locks removed)
    (list :swept-at now
          :components-removed (length removed)
          :registries-removed registry-count)))

(defmethod sweep-component-store
    ((store memory-component-store) configuration)
  (check-type configuration clog-hypermedia:hypermedia-configuration)
  (sweep-with-ttl
   store (clog-hypermedia:configuration-component-ttl-seconds configuration)))

(defmethod component-store-stats ((store memory-component-store))
  (let ((registry-count 0) (component-count 0))
    (dolist (entry (registry-map-snapshot store))
      (bordeaux-threads:with-lock-held ((registry-lock (cdr entry)))
        (when (registry-active-p (cdr entry))
          (incf registry-count)
          (incf component-count
                (hash-table-count (registry-components (cdr entry)))))))
    (list :registry-count registry-count
          :component-count component-count
          :max-components-per-session
          (component-store-max-components-per-session store)
          :component-ttl-seconds
          (component-store-component-ttl-seconds store)
          :created-at (store-created-at store))))

(defun register-component (store session-id component)
  "Alias for STORE-COMPONENT."
  (store-component store session-id component))

(defun find-component (store session-id component-id)
  "Load from the current session or signal COMPONENT-NOT-FOUND without probing others."
  (or (load-component store session-id component-id)
      (error 'component-not-found :reason :not-found)))

(defun remove-component (store session-id component-id)
  "Alias for DELETE-COMPONENT."
  (delete-component store session-id component-id))

(defun sweep-components (store configuration)
  "Alias for SWEEP-COMPONENT-STORE."
  (sweep-component-store store configuration))
