;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime package boundaries                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-live-queue
  (:use #:cl)
  (:export #:queue-error
           #:queue-error-reason
           #:invalid-queue-configuration
           #:bounded-queue
           #:make-bounded-queue
           #:queue-capacity
           #:queue-size
           #:queue-overflow-policy
           #:queue-resync-marker
           #:queue-closed-p
           #:enqueue
           #:dequeue
           #:close-queue
           #:queue-cancellation
           #:make-queue-cancellation
           #:cancel-queue-cancellation
           #:queue-cancellation-cancelled-p)
  (:documentation
   "Internal bounded mailbox and cancellation primitives shared by SSE and WebSocket subscribers."))

(defpackage #:clog-sse
  (:use #:cl)
  (:documentation
   "Internal Server-Sent Events encoding and streaming package.

SSE transport code must not mutate component state directly."))

(defpackage #:clog-ws
  (:use #:cl)
  (:documentation
   "Internal WebSocket framing and schema-validation package.

The WebSocket boundary never evaluates arbitrary Lisp forms or JavaScript."))

(defpackage #:clog-effect
  (:use #:cl)
  (:documentation
   "Internal typed client-effect model package.

Effects are represented as validated data rather than raw script strings."))

(defpackage #:clog-live
  (:use #:cl)
  (:import-from #:clog-live-queue
                #:queue-error
                #:queue-error-reason
                #:invalid-queue-configuration
                #:bounded-queue
                #:make-bounded-queue
                #:queue-capacity
                #:queue-size
                #:queue-overflow-policy
                #:queue-resync-marker
                #:queue-closed-p
                #:enqueue
                #:dequeue
                #:close-queue
                #:queue-cancellation
                #:make-queue-cancellation
                #:cancel-queue-cancellation
                #:queue-cancellation-cancelled-p)
  (:export #:queue-error
           #:queue-error-reason
           #:invalid-queue-configuration
           #:bounded-queue
           #:make-bounded-queue
           #:queue-capacity
           #:queue-size
           #:queue-overflow-policy
           #:queue-resync-marker
           #:queue-closed-p
           #:enqueue
           #:dequeue
           #:close-queue
           #:queue-cancellation
           #:make-queue-cancellation
           #:cancel-queue-cancellation
           #:queue-cancellation-cancelled-p)
  (:documentation
   "Public facade package for the experimental CLOG 3 Live Runtime.

LV-040 exposes only transport-neutral bounded mailbox primitives. Event bus,
SSE, WebSocket and typed effect APIs are exported by their later tasks."))
