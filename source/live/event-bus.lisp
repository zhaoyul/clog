;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime topic event bus and subscription lifecycle         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-live-event-bus)

(defclass topic-state ()
  ((sequence
    :initform 0
    :accessor topic-state-sequence)
   (subscriptions
    :initform nil
    :accessor topic-state-subscriptions))
  (:documentation
   "One event-bus topic's sequence counter and active subscription membership."))

(defclass event-bus ()
  ((lock
    :initform (bt2:make-lock :name "clog-live-event-bus")
    :reader event-bus-lock)
   (topics
    :initform (make-hash-table :test #'equal)
    :reader event-bus-topics)
   (authorize-subscription
    :initarg :authorize-subscription
    :reader event-bus-authorize-subscription))
  (:documentation
   "In-process topic event bus with topic-local sequence allocation and snapshot fan-out."))

(defun default-authorization-hook (topic context)
  (declare (ignore topic context))
  t)

(defun make-event-bus (&key (authorize-subscription #'default-authorization-hook))
  "Create an in-process event bus without starting a worker thread.

AUTHORIZE-SUBSCRIPTION is called as (HOOK TOPIC CONTEXT) outside the bus lock
before a subscription is registered. It must return generalized boolean."
  (unless (functionp authorize-subscription)
    (error 'event-bus-error :reason :invalid-authorization-hook))
  (make-instance 'event-bus :authorize-subscription authorize-subscription))

(defun safe-topic-string-p (value)
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 4096)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun valid-topic-p (topic)
  "Return true for the stable topic vocabulary accepted by LV-041."
  (or (safe-topic-string-p topic)
      (and (symbolp topic) topic)))

(defun checked-topic (topic)
  "Return an owned topic value or signal EVENT-BUS-ERROR."
  (unless (valid-topic-p topic)
    (error 'event-bus-error :reason :invalid-topic))
  (if (stringp topic) (copy-seq topic) topic))

(defun topic-state-no-lock (bus topic &key create-p)
  "Return TOPIC's state while BUS lock is already held."
  (or (gethash topic (event-bus-topics bus))
      (when create-p
        (setf (gethash (if (stringp topic) (copy-seq topic) topic)
                       (event-bus-topics bus))
              (make-instance 'topic-state)))))

(defun authorize-topic-subscription (bus topic context)
  "Run BUS authorization outside the bus lock and fail closed on rejection."
  (handler-case
      (unless (funcall (event-bus-authorize-subscription bus)
                       (if (stringp topic) (copy-seq topic) topic)
                       context)
        (error 'subscription-not-authorized :reason :not-authorized))
    (subscription-not-authorized (condition)
      (error condition))
    (error (condition)
      (declare (ignore condition))
      (error 'subscription-not-authorized :reason :authorization-hook-failed)))
  t)

(defun checked-subscription-queue (queue)
  "Return QUEUE when it is an open LV-040 mailbox."
  (unless (typep queue 'clog-live-queue:bounded-queue)
    (error 'event-bus-error :reason :invalid-subscription-queue))
  (when (clog-live-queue:queue-closed-p queue)
    (error 'event-bus-error :reason :subscription-queue-closed))
  queue)

(defun subscribe
    (bus topic &key queue authorization-context)
  "Create and register one authorized topic subscription.

When QUEUE is NIL, a default LV-040 bounded queue is created. The authorization
hook runs before membership changes and outside BUS's lock."
  (check-type bus event-bus)
  (let* ((topic (checked-topic topic))
         (queue (or queue (clog-live-queue:make-bounded-queue))))
    (authorize-topic-subscription bus topic authorization-context)
    (checked-subscription-queue queue)
    (let ((subscription
            (make-instance 'clog-live-protocol:subscription
                           :bus bus :topic topic :queue queue)))
      (bt2:with-lock-held ((event-bus-lock bus))
        (let ((state (topic-state-no-lock bus topic :create-p t)))
          ;; Preserve deterministic registration order for diagnostics while
          ;; fan-out itself intentionally promises no inter-subscriber order.
          (setf (topic-state-subscriptions state)
                (append (topic-state-subscriptions state)
                        (list subscription)))))
      subscription)))

(defun remove-subscription-membership (subscription)
  "Remove SUBSCRIPTION from its bus topic under only the bus lock."
  (let ((bus (clog-live-protocol::%subscription-bus subscription))
        (topic (clog-live-protocol::%subscription-topic subscription)))
    (when (typep bus 'event-bus)
      (bt2:with-lock-held ((event-bus-lock bus))
        (let ((state (topic-state-no-lock bus topic)))
          (when state
            (setf (topic-state-subscriptions state)
                  (remove subscription
                          (topic-state-subscriptions state)
                          :test #'eq)))))))
  subscription)

(defun close-subscription (subscription)
  "Terminally remove and close SUBSCRIPTION, returning it plus lifecycle status.

Membership is removed before waiting for an in-flight delivery's subscription
lock. If a publisher already captured this subscription in its snapshot,
CLOSE-SUBSCRIPTION waits for that delivery and then closes/clears the mailbox.
Therefore no event remains observable after this function returns."
  (check-type subscription clog-live-protocol:subscription)
  (remove-subscription-membership subscription)
  (bt2:with-lock-held ((clog-live-protocol::%subscription-lock subscription))
    (if (clog-live-protocol::%subscription-closed-p subscription)
        (values subscription :already-closed)
        (progn
          (setf (clog-live-protocol::%subscription-closed-p subscription) t)
          (clog-live-queue:close-queue
           (clog-live-protocol:subscription-queue subscription))
          (values subscription :closed)))))

(defun unsubscribe (subscription)
  "Idempotent alias for CLOSE-SUBSCRIPTION."
  (close-subscription subscription))

(defun subscription-snapshot-and-envelope (bus topic payload)
  "Allocate TOPIC sequence and snapshot subscribers while holding BUS lock."
  (bt2:with-lock-held ((event-bus-lock bus))
    (let* ((state (topic-state-no-lock bus topic :create-p t))
           (sequence (incf (topic-state-sequence state)))
           (envelope
             (make-instance 'clog-live-protocol:event-envelope
                            :topic topic
                            :sequence sequence
                            :payload payload)))
      (values envelope (copy-list (topic-state-subscriptions state))))))

(defmethod clog-live-protocol:deliver-event
    ((subscription clog-live-protocol:subscription)
     (envelope clog-live-protocol:event-envelope))
  "Default non-blocking delivery through SUBSCRIPTION's LV-040 mailbox."
  (bt2:with-lock-held ((clog-live-protocol::%subscription-lock subscription))
    (if (clog-live-protocol::%subscription-closed-p subscription)
        :closed
        (let ((status
                (clog-live-queue:enqueue
                 (clog-live-protocol:subscription-queue subscription)
                 envelope)))
          (when (member status '(:disconnect :closed) :test #'eq)
            (setf (clog-live-protocol::%subscription-closed-p subscription) t))
          status))))

(defun terminal-delivery-status-p (status)
  (member status '(:disconnect :closed) :test #'eq))

(defun publish-event (bus topic payload)
  "Publish PAYLOAD on TOPIC and return its immutable event envelope.

Sequence allocation and subscriber snapshotting occur while the bus lock is
held. The lock is released before DELIVER-EVENT is invoked for any subscriber,
so a slow or specialized subscriber cannot block membership changes or other
topic publishers waiting only on the bus state."
  (check-type bus event-bus)
  (let ((topic (checked-topic topic)))
    (multiple-value-bind (envelope subscriptions)
        (subscription-snapshot-and-envelope bus topic payload)
      (dolist (subscription subscriptions)
        (let ((status (clog-live-protocol:deliver-event subscription envelope)))
          (when (terminal-delivery-status-p status)
            ;; Delivery marked the subscription terminal under its own lock.
            ;; Membership cleanup happens afterward under only the bus lock.
            (remove-subscription-membership subscription))))
      envelope)))
