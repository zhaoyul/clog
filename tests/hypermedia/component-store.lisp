;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime component-store tests                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defclass hm-021-probe-component (clog-hypermedia:component)
  ((store
    :initarg :store
    :initform nil
    :reader hm-021-probe-store)
   (session-id
    :initarg :session-id
    :initform nil
    :reader hm-021-probe-session-id)))

(defmethod clog-hypermedia:render-component
    ((instance hm-021-probe-component) context)
  (declare (ignore context))
  (let ((store (hm-021-probe-store instance))
        (session-id (hm-021-probe-session-id instance)))
    (when (and store session-id)
      (clog-hypermedia:enumerate-components store session-id))
    (format nil "<div id=\"~A\"></div>"
            (clog-hypermedia:component-id instance))))

(defun hm-021-make-component
    (owner &key id store (session-id owner))
  "Create and mount a session-scoped HM-021 probe component."
  (let ((instance
          (apply #'make-instance
                 'hm-021-probe-component
                 :owner-session-id owner
                 :store store
                 :session-id session-id
                 (when id (list :id id)))))
    (clog-hypermedia:mount-component instance)
    instance))

(defun hm-021-stat (stats key)
  "Return KEY from a bounded component-store STATS plist."
  (getf stats key))

(defun hm-021-hash-values (table)
  "Return a snapshot of TABLE values."
  (let ((values nil))
    (maphash (lambda (key value)
               (declare (ignore key))
               (push value values))
             table)
    values))

(test component-store/configuration/no-background-thread-and-defaults
  (let* ((threads-before (copy-list (bordeaux-threads:all-threads)))
         (configuration
           (clog-hypermedia:make-hypermedia-configuration
            :component-ttl-seconds 23
            :max-components-per-session 3))
         (store
           (clog-hypermedia:make-memory-component-store
            :configuration configuration)))
    (is-true (clog-hypermedia:component-store-p store))
    (is (= 23
           (clog-hypermedia:component-store-component-ttl-seconds store)))
    (is (= 3
           (clog-hypermedia:component-store-max-components-per-session store)))
    (is (null (set-difference (bordeaux-threads:all-threads)
                              threads-before
                              :test #'eq))
        "Constructing a memory store must not start a maintenance thread.")))

(test component-store/ownership/session-namespace-isolation
  (let* ((store
           (clog-hypermedia:make-memory-component-store
            :max-components-per-session 8
            :component-ttl-seconds 60))
         (shared-id "clog-c-000000000000000000000000000000aa")
         (session-a "hm021-session-a")
         (session-b "hm021-session-b")
         (component-a
           (hm-021-make-component session-a :id shared-id))
         (component-b
           (hm-021-make-component session-b :id shared-id)))
    (is (eq component-a
            (clog-hypermedia:store-component
             store session-a component-a)))
    (is (eq component-b
            (clog-hypermedia:store-component
             store session-b component-b)))
    (is (eq component-a
            (clog-hypermedia:load-component
             store session-a shared-id)))
    (is (eq component-b
            (clog-hypermedia:load-component
             store session-b shared-id)))
    (is (equal (list component-a)
               (clog-hypermedia:enumerate-components store session-a)))
    (is (equal (list component-b)
               (clog-hypermedia:enumerate-components store session-b)))
    (is (null
         (clog-hypermedia:load-component
          store "hm021-session-c" shared-id)))
    (signals clog-hypermedia:component-not-found
      (clog-hypermedia:find-component
       store "hm021-session-c" shared-id))
    (signals clog-hypermedia:component-store-ownership-error
      (clog-hypermedia:store-component
       store session-b component-a))))

(test component-store/put/idempotence-conflict-and-limit
  (let* ((store
           (clog-hypermedia:make-memory-component-store
            :max-components-per-session 1
            :component-ttl-seconds 60))
         (session-id "hm021-limited-session")
         (component
           (hm-021-make-component session-id))
         (duplicate-id
           (hm-021-make-component
            session-id
            :id (clog-hypermedia:component-id component)))
         (second (hm-021-make-component session-id)))
    (multiple-value-bind (stored status)
        (clog-hypermedia:register-component store session-id component)
      (is (eq component stored))
      (is (eq :stored status)))
    (multiple-value-bind (stored status)
        (clog-hypermedia:register-component store session-id component)
      (is (eq component stored))
      (is (eq :already-stored status)))
    (signals clog-hypermedia:component-store-conflict
      (clog-hypermedia:register-component
       store session-id duplicate-id))
    (signals clog-hypermedia:component-store-limit-exceeded
      (clog-hypermedia:register-component store session-id second))))

(test component-store/ttl/fake-clock-keeps-active-and-sweeps-stale
  (let* ((now 0)
         (clock (lambda () now))
         (configuration
           (clog-hypermedia:make-hypermedia-configuration
            :component-ttl-seconds 10
            :max-components-per-session 8))
         (store
           (clog-hypermedia:make-memory-component-store
            :clock clock
            :configuration configuration))
         (session-id "hm021-ttl-session")
         (active (hm-021-make-component session-id))
         (stale (hm-021-make-component session-id)))
    (clog-hypermedia:store-component store session-id active)
    (clog-hypermedia:store-component store session-id stale)
    (setf now 9)
    (is (eq active
            (clog-hypermedia:touch-stored-component
             store session-id
             (clog-hypermedia:component-id active))))
    (setf now 10)
    (let ((sweep
            (clog-hypermedia:sweep-component-store store configuration)))
      (is (= 1 (getf sweep :components-removed)))
      (is (= 0 (getf sweep :registries-removed))))
    (is-true (clog-hypermedia:mounted-p active))
    (is-false (clog-hypermedia:mounted-p stale))
    (is (eq active
            (clog-hypermedia:load-component
             store session-id
             (clog-hypermedia:component-id active))))
    (is (null
         (clog-hypermedia:load-component
          store session-id
          (clog-hypermedia:component-id stale))))
    (setf now 20)
    (let ((sweep
            (clog-hypermedia:sweep-components store configuration)))
      (is (= 1 (getf sweep :components-removed)))
      (is (= 1 (getf sweep :registries-removed))))
    (is-false (clog-hypermedia:mounted-p active))
    (let ((stats (clog-hypermedia:component-store-stats store)))
      (is (= 0 (hm-021-stat stats :registry-count)))
      (is (= 0 (hm-021-stat stats :component-count))))))

(test component-store/locks/user-render-runs-after-store-locks-are-released
  (let* ((store
           (clog-hypermedia:make-memory-component-store
            :max-components-per-session 4
            :component-ttl-seconds 60))
         (session-id "hm021-render-session")
         (component
           (hm-021-make-component
            session-id :store store :session-id session-id)))
    (clog-hypermedia:store-component store session-id component)
    (let ((loaded
            (clog-hypermedia:load-component
             store session-id
             (clog-hypermedia:component-id component))))
      (is (eq component loaded))
      (is (string=
           (format nil "<div id=\"~A\"></div>"
                   (clog-hypermedia:component-id component))
           (clog-hypermedia:render-component loaded nil))))))

(test component-store/concurrency/concurrent-put-get-remove
  (let* ((store
           (clog-hypermedia:make-memory-component-store
            :max-components-per-session 1024
            :component-ttl-seconds 600))
         (session-id "hm021-concurrent-session")
         (thread-count 8)
         (components-per-thread 40)
         (errors nil)
         (errors-lock
           (bordeaux-threads:make-lock "hm021-test-errors")))
    (labels ((record-error (condition)
               (bordeaux-threads:with-lock-held (errors-lock)
                 (push condition errors)))
             (worker ()
               (handler-case
                   (loop repeat components-per-thread
                         for component = (hm-021-make-component session-id)
                         for component-id =
                           (clog-hypermedia:component-id component)
                         do (clog-hypermedia:store-component
                             store session-id component)
                            (unless
                                (eq component
                                    (clog-hypermedia:load-component
                                     store session-id component-id))
                              (error "Concurrent lookup returned the wrong component."))
                            (multiple-value-bind (removed status)
                                (clog-hypermedia:delete-component
                                 store session-id component-id)
                              (unless (and (eq component removed)
                                           (eq :deleted status))
                                (error "Concurrent deletion lost its component."))))
                 (error (condition)
                   (record-error condition)))))
      (let ((threads
              (loop repeat thread-count
                    collect
                    (bordeaux-threads:make-thread
                     #'worker
                     :name "hm021-store-worker"))))
        (dolist (thread threads)
          (bordeaux-threads:join-thread thread))))
    (is (null errors)
        "Concurrent store operations must complete without errors: ~S"
        errors)
    (is (null (clog-hypermedia:enumerate-components store session-id)))
    (let ((stats (clog-hypermedia:component-store-stats store)))
      (is (= 0 (hm-021-stat stats :component-count))))))

(test component-store/session/lightweight-marker-and-explicit-rotation
  (let* ((store
           (clog-hypermedia:make-memory-component-store
            :max-components-per-session 8
            :component-ttl-seconds 60))
         (old-session (make-hash-table :test #'equal))
         (new-session (make-hash-table :test #'equal))
         (old-context
           (clog-hypermedia:make-request-context
            (make-request-env
             :method :get
             :path "/old"
             :session old-session
             :session-id "hm021-old-session")))
         (new-context
           (clog-hypermedia:make-request-context
            (make-request-env
             :method :get
             :path "/new"
             :session new-session
             :session-id "hm021-new-session")))
         (component
           (hm-021-make-component "hm021-old-session")))
    (multiple-value-bind (registry session-id)
        (clog-hypermedia:ensure-session-component-registry
         store old-context)
      (is (not (null registry)))
      (is (string= "hm021-old-session" session-id)))
    (let ((marker
            (gethash (clog-hypermedia:component-store-session-key)
                     old-session)))
      (is (string= "v1" marker))
      (is (every (lambda (value)
                   (and (not (typep value 'clog-hypermedia:component))
                        (not (clog-hypermedia:component-store-p value))))
                 (hm-021-hash-values old-session))
          "Lack session data must contain only lightweight serializable metadata."))
    (clog-hypermedia:store-component
     store "hm021-old-session" component)
    (multiple-value-bind (new-registry new-session-id retired-count)
        (clog-hypermedia:rotate-session-component-registry
         store "hm021-old-session" new-context)
      (is (not (null new-registry)))
      (is (string= "hm021-new-session" new-session-id))
      (is (= 1 retired-count)))
    (is-false (clog-hypermedia:mounted-p component))
    (is (null
         (clog-hypermedia:load-component
          store
          "hm021-old-session"
          (clog-hypermedia:component-id component))))
    (is (string=
         "v1"
         (gethash (clog-hypermedia:component-store-session-key)
                  new-session)))
    (let ((new-component
            (hm-021-make-component "hm021-new-session")))
      (clog-hypermedia:store-component
       store "hm021-new-session" new-component)
      (is (eq new-component
              (clog-hypermedia:load-component
               store
               "hm021-new-session"
               (clog-hypermedia:component-id new-component)))))))
