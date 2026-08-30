;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime test packages                               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-hypermedia-tests
  (:use #:cl #:fiveam)
  (:shadow #:run-all-tests)
  (:export #:run-all-tests))

(in-package #:clog-hypermedia-tests)

(def-suite clog-hypermedia-tests
  :description "CLOG 3 Hypermedia Runtime regression tests.")

(in-suite clog-hypermedia-tests)
