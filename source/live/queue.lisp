;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime bounded mailbox and backpressure primitives        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-live-queue)

(defparameter +queue-overflow-policies+
  '(:drop-oldest :drop-newest :disconnect :resync-marker)
  "Closed overflow policy vocabulary for LV-040 bounded mailboxes.")

(define-condition queue-error (error)
  ((reason
    :initarg :reason
    :initform nil
    :reader queue-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Live queue operation failed~@[ (~A)~]."
             (queue-error-reason condition))))
  (:documentation
   "Base condition for bounded mailbox configuration and operation errors."))

(define-condition invalid-queue-configuration (queue-error)
  ()
  (:documentation
   "Signaled when a bounded mailbox is constructed with invalid immutable configuration."))

(defclass queue-cancellation ()
  ((lock
    :initform (bt2:make-lock :name "clog-live-queue-cancellation")
    :reader cancellation-lock)
   (cancelled-p
    :initform nil
    :accessor %queue-cancellation-cancelled-p)
   (registrations
    :initform (make-hash-table :test #'eq)
    :reader cancellation-registrations))
  (:documentation
   "Reusable terminal cancellation token for one or more blocked DEQUEUE calls.

The registration table contains only live queue capabilities and waiter counts.
Cancellation owns no worker thread and never closes a queue."))

(defun make-queue-cancellation ()
  "Return a fresh uncancelled queue waiter cancellation token."
  (make-instance 'queue-cancellation))

(defun queue-cancellation-cancelled-p (cancellation)
  "Return true after CANCELLATION has entered its terminal cancelled state."
  (check-type cancellation queue-cancellation)
  (bt2:with-lock-held ((cancellation-lock cancellation))
    (%queue-cancellation-cancelled-p cancellation)))

(defun register-cancellation-waiter (cancellation queue)
  "Register one QUEUE waiter, or return NIL when already cancelled."
  (bt2:with-lock-held ((cancellation-lock cancellation))
    (when (%queue-cancellation-cancelled-p cancellation)
      (return-from register-cancellation-waiter nil))
    (incf (gethash queue (cancellation-registrations cancellation) 0))
    t))

(defun unregister-cancellation-waiter (cancellation queue)
  "Remove one QUEUE waiter registration. Safe after cancellation."
  (bt2:with-lock-held ((cancellation-lock cancellation))
    (multiple-value-bind (count present-p)
        (gethash queue (cancellation-registrations cancellation))
      (when present-p
        (if (> count 1)
            (setf (gethash queue (cancellation-registrations cancellation))
                  (1- count))
            (remhash queue (cancellation-registrations cancellation)))))
    t))

(defclass bounded-queue ()
  ((capacity
    :initarg :capacity
    :reader queue-capacity)
   (overflow-policy
    :initarg :overflow-policy
    :reader queue-overflow-policy)
   (resync-marker
    :initarg :resync-marker
    :reader queue-resync-marker)
   (buffer
    :initarg :buffer
    :reader queue-buffer)
   (head
    :initform 0
    :accessor queue-head)
   (count
    :initform 0
    :accessor queue-count)
   (closed-p
    :initform nil
    :accessor %queue-closed-p)
   (lock
    :initform (bt2:make-lock :name "clog-live-bounded-queue")
    :reader queue-lock)
   (condition-variable
    :initform (bt2:make-condition-variable :name "clog-live-bounded-queue")
    :reader queue-condition-variable))
  (:documentation
   "Thread-safe fixed-capacity FIFO mailbox with explicit overflow and terminal close semantics."))

(defun valid-capacity-p (capacity)
  (and (integerp capacity) (plusp capacity)))

(defun valid-overflow-policy-p (policy)
  (member policy +queue-overflow-policies+ :test #'eq))

(defun make-bounded-queue
    (&key (capacity 64)
          (overflow-policy :resync-marker)
          (resync-marker :resync-required))
  "Create a bounded queue without starting a background thread.

CAPACITY is a positive integer. OVERFLOW-POLICY is one of :DROP-OLDEST,
:DROP-NEWEST, :DISCONNECT or :RESYNC-MARKER. RESYNC-MARKER is an arbitrary Lisp
object stored when the corresponding overflow policy activates."
  (unless (valid-capacity-p capacity)
    (error 'invalid-queue-configuration :reason :invalid-capacity))
  (unless (valid-overflow-policy-p overflow-policy)
    (error 'invalid-queue-configuration :reason :invalid-overflow-policy))
  (make-instance 'bounded-queue
                 :capacity capacity
                 :overflow-policy overflow-policy
                 :resync-marker resync-marker
                 :buffer (make-array capacity :initial-element nil)))

(defun queue-size (queue)
  "Return QUEUE's current buffered item count."
  (check-type queue bounded-queue)
  (bt2:with-lock-held ((queue-lock queue))
    (queue-count queue)))

(defun queue-closed-p (queue)
  "Return true when QUEUE is terminally closed or disconnected."
  (check-type queue bounded-queue)
  (bt2:with-lock-held ((queue-lock queue))
    (%queue-closed-p queue)))

(defun queue-tail-index (queue)
  (mod (+ (queue-head queue) (queue-count queue))
       (queue-capacity queue)))

(defun queue-push-no-lock (queue item)
  "Append ITEM while QUEUE's lock is held and spare capacity exists."
  (setf (aref (queue-buffer queue) (queue-tail-index queue)) item)
  (incf (queue-count queue))
  item)

(defun queue-pop-no-lock (queue)
  "Remove and return the oldest item while QUEUE's lock is held."
  (let* ((index (queue-head queue))
         (value (aref (queue-buffer queue) index)))
    (setf (aref (queue-buffer queue) index) nil
          (queue-head queue) (mod (1+ index) (queue-capacity queue)))
    (decf (queue-count queue))
    value))

(defun queue-clear-no-lock (queue)
  "Discard all buffered references while QUEUE's lock is held."
  (fill (queue-buffer queue) nil)
  (setf (queue-head queue) 0
        (queue-count queue) 0)
  queue)

(defun enqueue (queue item)
  "Attempt to enqueue ITEM and return an explicit backpressure status keyword.

Normal insertion returns :ENQUEUED. On overflow the configured policy returns
:DROPPED-OLDEST, :DROPPED-NEWEST, :DISCONNECT or :RESYNC-MARKER. A terminally
closed queue returns :CLOSED. ITEM may be NIL."
  (check-type queue bounded-queue)
  (bt2:with-lock-held ((queue-lock queue))
    (cond
      ((%queue-closed-p queue)
       :closed)
      ((< (queue-count queue) (queue-capacity queue))
       (queue-push-no-lock queue item)
       (bt2:condition-notify (queue-condition-variable queue))
       :enqueued)
      (t
       (ecase (queue-overflow-policy queue)
         (:drop-oldest
          (queue-pop-no-lock queue)
          (queue-push-no-lock queue item)
          (bt2:condition-notify (queue-condition-variable queue))
          :dropped-oldest)
         (:drop-newest
          :dropped-newest)
         (:disconnect
          (queue-clear-no-lock queue)
          (setf (%queue-closed-p queue) t)
          (bt2:condition-broadcast (queue-condition-variable queue))
          :disconnect)
         (:resync-marker
          (queue-clear-no-lock queue)
          (queue-push-no-lock queue (queue-resync-marker queue))
          (bt2:condition-notify (queue-condition-variable queue))
          :resync-marker))))))

(defun close-queue (queue)
  "Terminally close QUEUE, clear buffered items and wake all blocked waiters.

Returns QUEUE plus :CLOSED for the first call and :ALREADY-CLOSED afterward."
  (check-type queue bounded-queue)
  (bt2:with-lock-held ((queue-lock queue))
    (if (%queue-closed-p queue)
        (values queue :already-closed)
        (progn
          (queue-clear-no-lock queue)
          (setf (%queue-closed-p queue) t)
          (bt2:condition-broadcast (queue-condition-variable queue))
          (values queue :closed)))))

(defun cancellation-registration-snapshot (cancellation)
  "Atomically mark CANCELLATION and return queues with currently registered waiters."
  (bt2:with-lock-held ((cancellation-lock cancellation))
    (when (%queue-cancellation-cancelled-p cancellation)
      (return-from cancellation-registration-snapshot
        (values nil :already-cancelled)))
    (setf (%queue-cancellation-cancelled-p cancellation) t)
    (let ((queues nil))
      (maphash (lambda (queue count)
                 (declare (ignore count))
                 (push queue queues))
               (cancellation-registrations cancellation))
      (values queues :cancelled))))

(defun cancel-queue-cancellation (cancellation)
  "Cancel all current/future waits using CANCELLATION and actively wake waiters.

The token lock is released before any queue lock is acquired. Each queue lock is
then briefly acquired before broadcast, sequencing cancellation against a waiter
that is registering immediately before CONDITION-WAIT and preventing a lost
wakeup. The queue itself is not closed."
  (check-type cancellation queue-cancellation)
  (multiple-value-bind (queues status)
      (cancellation-registration-snapshot cancellation)
    (when (eq status :cancelled)
      (dolist (queue queues)
        (bt2:with-lock-held ((queue-lock queue))
          (bt2:condition-broadcast (queue-condition-variable queue)))))
    (values cancellation status)))

(defun valid-timeout-p (timeout)
  (or (null timeout)
      (and (realp timeout) (not (minusp timeout)))))

(defun timeout-deadline (timeout)
  "Return an internal-real-time deadline for TIMEOUT seconds, or NIL."
  (when timeout
    (+ (get-internal-real-time)
       (ceiling (* timeout internal-time-units-per-second)))))

(defun remaining-timeout (deadline)
  "Return non-negative seconds until DEADLINE as a double float."
  (when deadline
    (max 0d0
         (/ (float (- deadline (get-internal-real-time)) 1d0)
            internal-time-units-per-second))))

(defun cancellation-cancelled-no-lock-p (cancellation)
  "Read CANCELLATION state under its own lock."
  (and cancellation
       (queue-cancellation-cancelled-p cancellation)))

(defun dequeue (queue &key timeout cancellation)
  "Return the next queue item and an explicit status.

Statuses are :ITEM, :TIMEOUT, :CANCELLED and :CLOSED. TIMEOUT is NIL for an
unbounded wait or a non-negative number of seconds. CANCELLATION is NIL or a
QUEUE-CANCELLATION token. Cancellation has priority over consuming an item, so a
cancelled waiter never steals data from another consumer. Close has highest
priority because it is the queue's terminal lifecycle state."
  (check-type queue bounded-queue)
  (unless (valid-timeout-p timeout)
    (error 'queue-error :reason :invalid-timeout))
  (unless (or (null cancellation)
              (typep cancellation 'queue-cancellation))
    (error 'queue-error :reason :invalid-cancellation))
  (let ((deadline (timeout-deadline timeout))
        (registered-p nil))
    (bt2:with-lock-held ((queue-lock queue))
      (unwind-protect
           (progn
             (when cancellation
               (unless (register-cancellation-waiter cancellation queue)
                 (return-from dequeue (values nil :cancelled)))
               (setf registered-p t))
             (loop
               (cond
                 ((%queue-closed-p queue)
                  (return (values nil :closed)))
                 ((cancellation-cancelled-no-lock-p cancellation)
                  (return (values nil :cancelled)))
                 ((plusp (queue-count queue))
                  (return (values (queue-pop-no-lock queue) :item)))
                 ((and deadline (<= (remaining-timeout deadline) 0d0))
                  (return (values nil :timeout)))
                 (t
                  (bt2:condition-wait
                   (queue-condition-variable queue)
                   (queue-lock queue)
                   :timeout (remaining-timeout deadline))))))
        (when registered-p
          (unregister-cancellation-waiter cancellation queue))))))
