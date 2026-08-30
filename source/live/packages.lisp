;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Live Runtime package boundaries                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (:documentation
   "Public facade package for the experimental CLOG 3 Live Runtime.

HM-002 intentionally exports no symbols. Event, SSE, WebSocket and effect APIs
are exported only after their implementation tasks are complete."))
