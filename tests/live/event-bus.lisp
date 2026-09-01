;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime LV-041 event bus tests                             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defun lv-041-next-event (subscription &key (timeout 0))
  "Dequeue one event from SUBSCRIPTION's mailbox."
  (clog-live:dequeue (clog-live:subscription-queue subscription)
                     :timeout timeout))

(defun lv-041-drain-sequences (subscription count)
  "Return COUNT immediately available event sequence IDs."
  (loop repeat count
        collect
        (multiple-value-bind (event status)
            (lv-041-next-event subscription)
          (is (eq :item status))
          (clog-live:event-envelope-sequence event))))

(test live-event-bus/envelope/topic-local-sequences-are-monotonic-and-replay-usable
  (let ((bus (clog-live:make-event-bus)))
    ;; Sequence allocation does not depend on having a current subscriber.
    (let ((a1 (clog-live:publish-event bus "alpha" :a1))
          (b1 (clog-live:publish-event bus "beta" :b1))
          (a2 (clog-live:publish-event bus "alpha" :a2)))
      (is (typep a1 'clog-live:event-envelope))
      (is (string= "alpha" (clog-live:event-envelope-topic a1)))
      (is (= 1 (clog-live:event-envelope-sequence a1)))
      (is (eq :a1 (clog-live:event-envelope-payload a1)))
      (is (= 1 (clog-live:event-envelope-sequence b1)))
      (is (= 2 (clog-live:event-envelope-sequence a2))))
    (let ((subscription (clog-live:subscribe bus "alpha")))
      (let ((a3 (clog-live:publish-event bus "alpha" nil)))
        (is (= 3 (clog-live:event-envelope-sequence a3)))
        (multiple-value-bind (delivered status)
            (lv-041-next-event subscription)
          (is (eq :item status))
          (is (eq a3 delivered)
              "Fan-out may share the immutable envelope capability.")
          (is (null (clog-live:event-envelope-payload delivered))))))))

(test live-event-bus/fanout/each-active-subscriber-receives-one-envelope
  (let* ((bus (clog-live:make-event-bus))
         (first (clog-live:subscribe bus :orders))
         (second (clog-live:subscribe bus :orders))
         (other-topic (clog-live:subscribe bus :inventory))
         (event (clog-live:publish-event bus :orders '(:id 42))))
    (dolist (subscription (list first second))
      (multiple-value-bind (received status)
          (lv-041-next-event subscription)
        (is (eq :item status))
        (is (eq event received))
        (is (= 1 (clog-live:event-envelope-sequence received)))
        (is (eq :orders (clog-live:event-envelope-topic received)))))
    (multiple-value-bind (received status)
        (lv-041-next-event other-topic)
      (is (null received))
      (is (eq :timeout status)))))

(test live-event-bus/backpressure/slow-subscriber-does-not-block-fast-one
  (let* ((bus (clog-live:make-event-bus))
         (slow-queue
           (clog-live:make-bounded-queue
            :capacity 1 :overflow-policy :drop-newest))
         (fast-queue
           (clog-live:make-bounded-queue
            :capacity 8 :overflow-policy :drop-newest))
         (slow (clog-live:subscribe bus "news" :queue slow-queue))
         (fast (clog-live:subscribe bus "news" :queue fast-queue)))
    (clog-live:publish-event bus "news" :first)
    ;; Consume only FAST. SLOW remains full and represents a stalled client.
    (is (= 1
           (clog-live:event-envelope-sequence
            (nth-value 0 (lv-041-next-event fast)))))
    (let ((second (clog-live:publish-event bus "news" :second)))
      (multiple-value-bind (received status)
          (lv-041-next-event fast)
        (is (eq :item status))
        (is (eq second received))
        (is (= 2 (clog-live:event-envelope-sequence received)))))
    ;; SLOW retained its old event because its own drop-newest policy fired;
    ;; its backlog did not prevent FAST from receiving the newer event.
    (multiple-value-bind (received status)
        (lv-041-next-event slow)
      (is (eq :item status))
      (is (= 1 (clog-live:event-envelope-sequence received))))
    (is (eq :timeout (nth-value 1 (lv-041-next-event slow))))))

(test live-event-bus/lifecycle/unsubscribe-and-connection-close-are-idempotent
  (let* ((bus (clog-live:make-event-bus))
         (subscription (clog-live:subscribe bus "orders")))
    (is-false (clog-live:subscription-closed-p subscription))
    (is (string= "orders" (clog-live:subscription-topic subscription)))
    (multiple-value-bind (returned status)
        (clog-live:unsubscribe subscription)
      (is (eq subscription returned))
      (is (eq :closed status)))
    (is-true (clog-live:subscription-closed-p subscription))
    (is-true (clog-live:queue-closed-p
              (clog-live:subscription-queue subscription)))
    (multiple-value-bind (returned status)
        (clog-live:unsubscribe subscription)
      (is (eq subscription returned))
      (is (eq :already-closed status)))
    (multiple-value-bind (returned status)
        (clog-live:close-subscription subscription)
      (is (eq subscription returned))
      (is (eq :already-closed status)))
    (clog-live:publish-event bus "orders" :after-unsubscribe)
    (multiple-value-bind (received status)
        (lv-041-next-event subscription)
      (is (null received))
      (is (eq :closed status))))
  (let* ((bus (clog-live:make-event-bus))
         (subscription (clog-live:subscribe bus :connection)))
    (is (eq :closed
            (nth-value 1 (clog-live:close-subscription subscription))))
    (is (eq :already-closed
            (nth-value 1 (clog-live:close-subscription subscription))))))

(test live-event-bus/authorization/hook-filters-topic-subscriptions-before-registration
  (let* ((calls nil)
         (bus
           (clog-live:make-event-bus
            :authorize-subscription
            (lambda (topic context)
              (push (list topic context) calls)
              (and (equal topic "public") (eq context :allowed))))))
    (signals clog-live:subscription-not-authorized
      (clog-live:subscribe bus "secret" :authorization-context :allowed))
    (signals clog-live:subscription-not-authorized
      (clog-live:subscribe bus "public" :authorization-context :denied))
    (let ((subscription
            (clog-live:subscribe
             bus "public" :authorization-context :allowed)))
      (clog-live:publish-event bus "public" :visible)
      (multiple-value-bind (event status)
          (lv-041-next-event subscription)
        (is (eq :item status))
        (is (eq :visible (clog-live:event-envelope-payload event)))))
    (is (= 3 (length calls)))))

(defclass lv-041-blocking-subscription (clog-live:subscription) ())

(defstruct lv-041-delivery-gate
  (lock (bordeaux-threads:make-lock "lv041-delivery-gate"))
  (entered-p nil)
  (release-p nil))

(defvar *lv-041-delivery-gate* nil)

(defun lv-041-gate-flag (gate reader)
  (bordeaux-threads:with-lock-held ((lv-041-delivery-gate-lock gate))
    (funcall reader gate)))

(defun lv-041-wait-until (predicate &key (timeout 1.0))
  "Poll PREDICATE until true or TIMEOUT, returning the final boolean."
  (let ((deadline (+ (get-internal-real-time)
                     (ceiling (* timeout internal-time-units-per-second)))))
    (loop
      when (funcall predicate) do (return t)
      when (>= (get-internal-real-time) deadline) do (return nil)
      do (sleep 0.001))))

(defmethod clog-live:deliver-event
    ((subscription lv-041-blocking-subscription) envelope)
  (let ((gate *lv-041-delivery-gate*))
    (when gate
      (bordeaux-threads:with-lock-held ((lv-041-delivery-gate-lock gate))
        (setf (lv-041-delivery-gate-entered-p gate) t))
      (loop until
            (bordeaux-threads:with-lock-held ((lv-041-delivery-gate-lock gate))
              (lv-041-delivery-gate-release-p gate))
            do (sleep 0.001)))
    (call-next-method)))

(test live-event-bus/concurrency/publish-releases-bus-lock-before-delivery
  (let* ((bus (clog-live:make-event-bus))
         (slow (clog-live:subscribe bus "topic"))
         (gate (make-lv-041-delivery-gate))
         (mutator-done nil)
         (mutator-lock (bordeaux-threads:make-lock "lv041-mutator")))
    (change-class slow 'lv-041-blocking-subscription)
    (setf *lv-041-delivery-gate* gate)
    (unwind-protect
         (let ((publisher
                 (bordeaux-threads:make-thread
                  (lambda () (clog-live:publish-event bus "topic" :blocked))
                  :name "lv041-blocking-publisher")))
           (is-true
            (lv-041-wait-until
             (lambda ()
               (lv-041-gate-flag gate #'lv-041-delivery-gate-entered-p))))
           ;; Delivery is now deliberately blocked. A new subscription must still
           ;; be able to acquire the bus lock and complete before delivery resumes.
           (let ((mutator
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (let ((temporary (clog-live:subscribe bus "other")))
                        (clog-live:unsubscribe temporary))
                      (bordeaux-threads:with-lock-held (mutator-lock)
                        (setf mutator-done t)))
                    :name "lv041-bus-mutator")))
             (is-true
              (lv-041-wait-until
               (lambda ()
                 (bordeaux-threads:with-lock-held (mutator-lock)
                   mutator-done))
               :timeout 0.25))
             (bordeaux-threads:with-lock-held
                 ((lv-041-delivery-gate-lock gate))
               (setf (lv-041-delivery-gate-release-p gate) t))
             (bordeaux-threads:join-thread mutator))
           (bordeaux-threads:join-thread publisher))
      (bordeaux-threads:with-lock-held ((lv-041-delivery-gate-lock gate))
        (setf (lv-041-delivery-gate-release-p gate) t))
      (setf *lv-041-delivery-gate* nil)
      (clog-live:unsubscribe slow))))

(test live-event-bus/concurrency/subscribe-unsubscribe-race-is-safe
  (let* ((bus (clog-live:make-event-bus))
         (failure-lock (bordeaux-threads:make-lock "lv041-race-failure"))
         (failure nil)
         (publisher
           (bordeaux-threads:make-thread
            (lambda ()
              (handler-case
                  (loop for index below 800
                        do (clog-live:publish-event bus :race index))
                (error (condition)
                  (bordeaux-threads:with-lock-held (failure-lock)
                    (setf failure condition)))))
            :name "lv041-race-publisher"))
         (workers
           (loop repeat 4
                 collect
                 (bordeaux-threads:make-thread
                  (lambda ()
                    (handler-case
                        (loop repeat 100
                              for queue =
                                (clog-live:make-bounded-queue
                                 :capacity 4
                                 :overflow-policy :drop-oldest)
                              for subscription =
                                (clog-live:subscribe bus :race :queue queue)
                              do (clog-live:unsubscribe subscription)
                                 (is-true
                                  (clog-live:subscription-closed-p subscription)))
                      (error (condition)
                        (bordeaux-threads:with-lock-held (failure-lock)
                          (setf failure condition)))))
                  :name "lv041-race-subscriber"))))
    (dolist (thread workers)
      (bordeaux-threads:join-thread thread))
    (bordeaux-threads:join-thread publisher)
    (is (null failure) "Subscribe/unsubscribe race failed: ~S" failure)))

(test live-event-bus/concurrency/topic-sequence-is-lossless-under-parallel-publish
  (let* ((bus (clog-live:make-event-bus))
         (subscription
           (clog-live:subscribe
            bus :parallel
            :queue (clog-live:make-bounded-queue
                    :capacity 1024 :overflow-policy :disconnect)))
         (thread-count 8)
         (per-thread 100)
         (threads
           (loop for producer below thread-count
                 collect
                 (let ((producer producer))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (loop for index below per-thread
                            do (clog-live:publish-event
                                bus :parallel (list producer index))))
                    :name "lv041-parallel-publisher")))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (let ((sequences
            (sort (lv-041-drain-sequences
                   subscription (* thread-count per-thread))
                  #'<)))
      (is (equal (loop for sequence from 1 to (* thread-count per-thread)
                       collect sequence)
                 sequences)))
    (is-false (clog-live:subscription-closed-p subscription))))
