;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Hypermedia compatibility package boundary                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-compat
  (:use #:cl)
  (:documentation
   "Public facade package for Legacy Island compatibility integration.

HM-002 intentionally exports no symbols and does not modify Legacy CLOG. The
compatibility API is added only when the Legacy Island implementation exists."))
