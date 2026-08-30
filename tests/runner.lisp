;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime test runner                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(define-condition hypermedia-test-failure (error)
  ((failed-tests
    :initarg :failed-tests
    :initform nil
    :reader failed-tests))
  (:report
   (lambda (condition stream)
     (format stream
             "CLOG Hypermedia tests failed~@[ (~{~A~^, ~})~]."
             (failed-tests condition))))
  (:documentation
   "Signaled when one or more CLOG Hypermedia FiveAM checks fail."))

(defun ensure-test-results (results)
  "Return RESULTS or fail when the suite executed no FiveAM checks."
  (unless results
    (error 'hypermedia-test-failure :failed-tests '(:no-tests-executed)))
  results)

(defun ensure-test-success (success failed-tests)
  "Return true for a successful test run or signal HYPERMEDIA-TEST-FAILURE.

Signaling a condition, rather than terminating the Lisp image directly, keeps
the debugger available during interactive development. A non-interactive Lisp
process still exits with a non-zero status because the condition is unhandled."
  (unless success
    (error 'hypermedia-test-failure :failed-tests failed-tests))
  t)

(defun run-all-tests ()
  "Run the complete CLOG Hypermedia FiveAM suite.

Return true when every check passes. Signal HYPERMEDIA-TEST-FAILURE when a
check fails, allowing ASDF and non-interactive Lisp processes to report a
non-zero exit status while preserving normal condition handling in a REPL."
  (let ((results (ensure-test-results
                  (run 'clog-hypermedia-tests))))
    (explain! results)
    (multiple-value-bind (success failed-tests skipped-tests)
        (results-status results)
      (declare (ignore skipped-tests))
      (ensure-test-success success failed-tests))))
