;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime event bus protocol and subscription values         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-live-protocol
  (:use #:cl)
  (:export #:event-bus-error
           #:event-bus-error-reason
           #:subscription-not-authorized
           #:event-envelope
           #:event-envelope-topic
           #:event-envelope-sequence
           #:event-envelope-payload
           #:subscription
           #:subscription-topic
           #:subscription-queue
           #:subscription-closed-p
           #:deliver-event))

(defpackage #:clog-live-event-bus
  (:use #:cl)
  (:import-from #:clog-live-protocol
                #:event-bus-error
                #:subscription-not-authorized
                #:event-envelope
                #:subscription
                #:deliver-event)
  (:export #:event-bus
           #:make-event-bus
           #:subscribe
           #:unsubscribe
           #:close-subscription
           #:publish-event))

(defpackage #:clog-live
  (:import-from #:clog-live-protocol
                #:event-bus-error
                #:event-bus-error-reason
                #:subscription-not-authorized
                #:event-envelope
                #:event-envelope-topic
                #:event-envelope-sequence
                #:event-envelope-payload
                #:subscription
                #:subscription-topic
                #:subscription-queue
                #:subscription-closed-p
                #:deliver-event)
  (:import-from #:clog-live-event-bus
                #:event-bus
                #:make-event-bus
                #:subscribe
                #:unsubscribe
                #:close-subscription
                #:publish-event)
  (:export #:event-bus-error
           #:event-bus-error-reason
           #:subscription-not-authorized
           #:event-envelope
           #:event-envelope-topic
           #:event-envelope-sequence
           #:event-envelope-payload
           #:subscription
           #:subscription-topic
           #:subscription-queue
           #:subscription-closed-p
           #:deliver-event
           #:event-bus
           #:make-event-bus
           #:subscribe
           #:unsubscribe
           #:close-subscription
           #:publish-event))

(in-package #:clog-live-protocol)

(define-condition event-bus-error (error)
  ((reason
    :initarg :reason
    :initform nil
    :reader event-bus-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Live event bus operation failed~@[ (~A)~]."
             (event-bus-error-reason condition))))
  (:documentation
   "Base condition for the in-process Live Runtime event bus."))

(define-condition subscription-not-authorized (event-bus-error)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (write-string "Live event subscription is not authorized." stream)))
  (:documentation
   "Signaled when the event-bus authorization hook rejects a topic subscription."))

(defun copy-topic (topic)
  "Defensively copy mutable string TOPIC values."
  (if (stringp topic) (copy-seq topic) topic))

(defclass event-envelope ()
  ((topic
    :initarg :topic
    :reader %event-envelope-topic)
   (sequence
    :initarg :sequence
    :reader event-envelope-sequence)
   (payload
    :initarg :payload
    :reader event-envelope-payload))
  (:documentation
   "Immutable-by-protocol topic event capability with a topic-local sequence ID."))

(defmethod initialize-instance :after ((envelope event-envelope) &key)
  (let ((sequence (event-envelope-sequence envelope)))
    (unless (and (integerp sequence) (plusp sequence))
      (error 'event-bus-error :reason :invalid-event-sequence)))
  (setf (slot-value envelope 'topic)
        (copy-topic (%event-envelope-topic envelope))))

(defun event-envelope-topic (envelope)
  "Return a defensive topic value from ENVELOPE."
  (check-type envelope event-envelope)
  (copy-topic (%event-envelope-topic envelope)))

(defclass subscription ()
  ((bus
    :initarg :bus
    :reader %subscription-bus)
   (topic
    :initarg :topic
    :reader %subscription-topic)
   (queue
    :initarg :queue
    :reader subscription-queue)
   (closed-p
    :initform nil
    :accessor %subscription-closed-p)
   (lock
    :initform (bt2:make-lock :name "clog-live-subscription")
    :reader %subscription-lock))
  (:documentation
   "One topic subscription capability with a private bounded outbound mailbox."))

(defmethod initialize-instance :after ((subscription subscription) &key)
  (setf (slot-value subscription 'topic)
        (copy-topic (%subscription-topic subscription))))

(defun subscription-topic (subscription)
  "Return a defensive topic value for SUBSCRIPTION."
  (check-type subscription subscription)
  (copy-topic (%subscription-topic subscription)))

(defun subscription-closed-p (subscription)
  "Return true when SUBSCRIPTION has entered its terminal closed state."
  (check-type subscription subscription)
  (bt2:with-lock-held ((%subscription-lock subscription))
    (%subscription-closed-p subscription)))

(defgeneric deliver-event (subscription envelope)
  (:documentation
   "Deliver ENVELOPE to one SUBSCRIPTION without holding the event-bus lock.

LV-041's default method uses the subscription's bounded mailbox. Specialized
subscriber types may wrap delivery for instrumentation or transport adapters,
but must preserve the no-bus-lock delivery boundary."))
