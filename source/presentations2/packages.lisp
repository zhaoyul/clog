;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG Presentations 2 package boundary                                  ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-presentations2
  (:use #:cl)
  (:documentation
   "Public facade package for the experimental Presentations 2 bridge.

HM-002 establishes only the dependency and invalidation boundary. No binding
API is exported before its implementation task is complete."))
