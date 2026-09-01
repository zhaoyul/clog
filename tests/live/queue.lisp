;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime LV-040 bounded mailbox tests                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defun lv-040-drain-now (queue count)
  "Return exactly COUNT non-blocking dequeue results from QUEUE."
  (loop repeat count
        collect
        (multiple-value-bind (value status)
            (clog-live:dequeue queue :timeout 0)
          (is (eq :item status))
          value)))

(test live-queue/configuration/defaults-and-validation
  (let ((queue (clog-live:make-bounded-queue)))
    (is (= 64 (clog-live:queue-capacity queue)))
    (is (= 0 (clog-live:queue-size queue)))
    (is (eq :resync-marker (clog-live:queue-overflow-policy queue)))
    (is (eq :resync-required (clog-live:queue-resync-marker queue)))
    (is-false (clog-live:queue-closed-p queue)))
  (dolist (capacity '(0 -1 1.5 "64"))
    (signals clog-live:invalid-queue-configuration
      (clog-live:make-bounded-queue :capacity capacity)))
  (signals clog-live:invalid-queue-configuration
    (clog-live:make-bounded-queue :overflow-policy :unknown))
  (let ((queue (clog-live:make-bounded-queue :capacity 1)))
    (signals clog-live:queue-error
      (clog-live:dequeue queue :timeout -0.01))
    (signals clog-live:queue-error
      (clog-live:dequeue queue :timeout 0 :cancellation t))))

(test live-queue/fifo/nil-is-a-valid-item-and-empty-is-status-coded
  (let ((queue
          (clog-live:make-bounded-queue
           :capacity 3 :overflow-policy :drop-newest)))
    (is (eq :enqueued (clog-live:enqueue queue :a)))
    (is (eq :enqueued (clog-live:enqueue queue nil)))
    (is (eq :enqueued (clog-live:enqueue queue :c)))
    (is (= 3 (clog-live:queue-size queue)))
    (is (equal '(:a nil :c) (lv-040-drain-now queue 3)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0)
      (is (null value))
      (is (eq :timeout status)))
    (is (= 0 (clog-live:queue-size queue)))))

(test live-queue/overflow/drop-oldest
  (let ((queue
          (clog-live:make-bounded-queue
           :capacity 2 :overflow-policy :drop-oldest)))
    (clog-live:enqueue queue :a)
    (clog-live:enqueue queue :b)
    (is (eq :dropped-oldest (clog-live:enqueue queue :c)))
    (is (= 2 (clog-live:queue-size queue)))
    (is (equal '(:b :c) (lv-040-drain-now queue 2)))))

(test live-queue/overflow/drop-newest
  (let ((queue
          (clog-live:make-bounded-queue
           :capacity 2 :overflow-policy :drop-newest)))
    (clog-live:enqueue queue :a)
    (clog-live:enqueue queue :b)
    (is (eq :dropped-newest (clog-live:enqueue queue :c)))
    (is (= 2 (clog-live:queue-size queue)))
    (is (equal '(:a :b) (lv-040-drain-now queue 2)))))

(test live-queue/overflow/disconnect-is-terminal-and-wakes-consumer-contract
  (let ((queue
          (clog-live:make-bounded-queue
           :capacity 2 :overflow-policy :disconnect)))
    (clog-live:enqueue queue :a)
    (clog-live:enqueue queue :b)
    (is (eq :disconnect (clog-live:enqueue queue :c)))
    (is-true (clog-live:queue-closed-p queue))
    (is (= 0 (clog-live:queue-size queue)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0)
      (is (null value))
      (is (eq :closed status)))
    (is (eq :closed (clog-live:enqueue queue :d)))))

(test live-queue/overflow/resync-marker-clears-stale-backlog
  (let* ((marker (list :resync-required :subscriber-7))
         (queue
           (clog-live:make-bounded-queue
            :capacity 3
            :overflow-policy :resync-marker
            :resync-marker marker)))
    (clog-live:enqueue queue :a)
    (clog-live:enqueue queue :b)
    (clog-live:enqueue queue :c)
    (is (eq :resync-marker (clog-live:enqueue queue :d)))
    (is (= 1 (clog-live:queue-size queue)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0)
      (is (eq :item status))
      (is (eq marker value)))
    (is (= 0 (clog-live:queue-size queue)))
    ;; A later event may follow the marker; upper layers decide how to resync.
    (is (eq :enqueued (clog-live:enqueue queue :newer)))
    (is (equal '(:newer) (lv-040-drain-now queue 1)))))

(test live-queue/close/is-idempotent-clears-buffer-and-wakes-all-waiters
  (let* ((queue (clog-live:make-bounded-queue :capacity 8))
         (waiter-count 8)
         (results (make-array waiter-count :initial-element :not-finished))
         (threads
           (loop for index below waiter-count
                 collect
                 (let ((index index))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (multiple-value-bind (value status)
                          (clog-live:dequeue queue)
                        (declare (ignore value))
                        (setf (aref results index) status)))
                    :name "lv040-close-waiter")))))
    ;; Give the workers an opportunity to enter the condition wait. The close
    ;; contract remains correct even if a worker reaches DEQUEUE just after close.
    (sleep 0.03)
    (multiple-value-bind (returned status)
        (clog-live:close-queue queue)
      (is (eq queue returned))
      (is (eq :closed status)))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (is (every (lambda (status) (eq :closed status)) results))
    (is-true (clog-live:queue-closed-p queue))
    (is (= 0 (clog-live:queue-size queue)))
    (is (eq :closed (clog-live:enqueue queue :late)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0)
      (is (null value))
      (is (eq :closed status)))
    (multiple-value-bind (returned status)
        (clog-live:close-queue queue)
      (is (eq queue returned))
      (is (eq :already-closed status))))
  ;; Buffered values are terminally discarded on explicit close.
  (let ((queue (clog-live:make-bounded-queue :capacity 2)))
    (clog-live:enqueue queue :stale)
    (clog-live:close-queue queue)
    (is (= 0 (clog-live:queue-size queue)))
    (is (eq :closed (nth-value 1 (clog-live:dequeue queue :timeout 0))))))

(test live-queue/cancellation/actively-wakes-multiple-blocked-dequeues
  (let* ((queue (clog-live:make-bounded-queue :capacity 8))
         (cancellation (clog-live:make-queue-cancellation))
         (waiter-count 6)
         (results (make-array waiter-count :initial-element :not-finished))
         (threads
           (loop for index below waiter-count
                 collect
                 (let ((index index))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (multiple-value-bind (value status)
                          (clog-live:dequeue queue :cancellation cancellation)
                        (declare (ignore value))
                        (setf (aref results index) status)))
                    :name "lv040-cancel-waiter")))))
    (sleep 0.03)
    (multiple-value-bind (returned status)
        (clog-live:cancel-queue-cancellation cancellation)
      (is (eq cancellation returned))
      (is (eq :cancelled status)))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (is (every (lambda (status) (eq :cancelled status)) results))
    (is-true (clog-live:queue-cancellation-cancelled-p cancellation))
    (multiple-value-bind (returned status)
        (clog-live:cancel-queue-cancellation cancellation)
      (is (eq cancellation returned))
      (is (eq :already-cancelled status)))
    ;; Cancellation belongs to the waiter, not to the mailbox itself.
    (is-false (clog-live:queue-closed-p queue))
    (is (eq :enqueued (clog-live:enqueue queue :still-usable)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :cancellation cancellation :timeout 0)
      (is (null value))
      (is (eq :cancelled status)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0)
      (is (eq :still-usable value))
      (is (eq :item status)))))

(test live-queue/timeout/empty-times-out-and-notify-beats-deadline
  (let ((queue (clog-live:make-bounded-queue :capacity 2)))
    (multiple-value-bind (value status)
        (clog-live:dequeue queue :timeout 0.02)
      (is (null value))
      (is (eq :timeout status)))
    (let ((producer
            (bordeaux-threads:make-thread
             (lambda ()
               (sleep 0.02)
               (clog-live:enqueue queue :arrived))
             :name "lv040-delayed-producer")))
      (multiple-value-bind (value status)
          (clog-live:dequeue queue :timeout 1.0)
        (is (eq :arrived value))
        (is (eq :item status)))
      (bordeaux-threads:join-thread producer))))

(test live-queue/concurrency/multi-producer-consumer-is-lossless-without-overflow
  (let* ((producer-count 4)
         (consumer-count 4)
         (items-per-producer 150)
         (total (* producer-count items-per-producer))
         (queue
           (clog-live:make-bounded-queue
            :capacity 1024 :overflow-policy :disconnect))
         (result-lock (bordeaux-threads:make-lock "lv040-result-lock"))
         (seen (make-hash-table :test #'equal))
         (remaining total)
         (failure nil))
    (labels ((record-failure (reason)
               (bordeaux-threads:with-lock-held (result-lock)
                 (unless failure (setf failure reason))))
             (consumer-loop ()
               (loop
                 (multiple-value-bind (item status)
                     (clog-live:dequeue queue :timeout 2.0)
                   (case status
                     (:item
                      (let ((done nil))
                        (bordeaux-threads:with-lock-held (result-lock)
                          (when (gethash item seen)
                            (unless failure (setf failure :duplicate-item)))
                          (setf (gethash item seen) t)
                          (decf remaining)
                          (when (minusp remaining)
                            (unless failure (setf failure :too-many-items)))
                          (setf done (zerop remaining)))
                        (when done
                          (clog-live:close-queue queue)
                          (return))))
                     (:closed (return))
                     (:timeout
                      (record-failure :consumer-timeout)
                      (clog-live:close-queue queue)
                      (return))
                     (:cancelled
                      (record-failure :unexpected-cancellation)
                      (clog-live:close-queue queue)
                      (return)))))))
      (let ((consumers
              (loop repeat consumer-count
                    collect
                    (bordeaux-threads:make-thread
                     #'consumer-loop :name "lv040-stress-consumer")))
            (producers
              (loop for producer below producer-count
                    collect
                    (let ((producer producer))
                      (bordeaux-threads:make-thread
                       (lambda ()
                         (loop for sequence below items-per-producer
                               for item = (list producer sequence)
                               for status = (clog-live:enqueue queue item)
                               unless (eq :enqueued status)
                                 do (record-failure
                                     (list :unexpected-enqueue-status status))))
                       :name "lv040-stress-producer")))))
        (dolist (thread producers)
          (bordeaux-threads:join-thread thread))
        (dolist (thread consumers)
          (bordeaux-threads:join-thread thread))))
    (is (null failure) "Concurrent mailbox failure: ~S" failure)
    (is (= 0 remaining))
    (is (= total (hash-table-count seen)))
    (is-true (clog-live:queue-closed-p queue))
    (is (= 0 (clog-live:queue-size queue)))))

(test live-queue/concurrency/capacity-never-exceeds-bound-under-overflow-stress
  (let* ((capacity 8)
         (queue
           (clog-live:make-bounded-queue
            :capacity capacity :overflow-policy :drop-oldest))
         (failure-lock (bordeaux-threads:make-lock "lv040-bound-lock"))
         (failure nil)
         (threads
           (loop for producer below 8
                 collect
                 (let ((producer producer))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (loop for sequence below 250
                            do (clog-live:enqueue queue (list producer sequence))
                               (let ((size (clog-live:queue-size queue)))
                                 (when (> size capacity)
                                   (bordeaux-threads:with-lock-held (failure-lock)
                                     (setf failure size)))))
                    :name "lv040-overflow-producer")))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (is (null failure) "Observed queue size above capacity: ~S" failure)
    (is (<= (clog-live:queue-size queue) capacity))
    (is (= capacity (clog-live:queue-size queue)))))
