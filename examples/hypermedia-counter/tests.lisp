;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Counter HM-027 tests                                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-hypermedia-counter-tests
  (:use #:cl #:fiveam)
  (:import-from #:clog-hypermedia-counter
                #:counter-component
                #:counter-value
                #:make-counter-application
                #:counter-server-port
                #:counter-server-running-p
                #:counter-server-url
                #:start-counter
                #:stop-counter)
  (:export #:run-tests))

(in-package #:clog-hypermedia-counter-tests)

(def-suite counter-tests)
(in-suite counter-tests)

(defun make-get-env (path)
  "Return a minimal Clack GET environment for Counter integration tests."
  (let ((headers (make-hash-table :test #'equal)))
    (setf (gethash "accept" headers) "text/html")
    (list :request-method :get
          :script-name ""
          :path-info path
          :server-name "localhost"
          :server-port 5000
          :server-protocol :http/1.1
          :request-uri path
          :url-scheme :http
          :query-string ""
          :headers headers)))

(defun response-body-text (response)
  "Flatten string chunks in a normal Clack RESPONSE for deterministic assertions."
  (labels ((emit (value stream)
             (cond
               ((null value) nil)
               ((stringp value) (write-string value stream))
               ((consp value) (dolist (entry value) (emit entry stream)))
               (t nil))))
    (with-output-to-string (stream)
      (emit (cddr response) stream))))

(defun count-substring (needle haystack)
  "Count non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(test counter/actions/increment-decrement-reset
  (let ((component
          (make-instance 'counter-component
                         :scope :session
                         :owner-session-id "counter-test-session")))
    (clog-hypermedia:mount-component component)
    (unwind-protect
         (progn
           (is (= 0 (counter-value component)))
           (clog-hypermedia:handle-action component :increment nil)
           (is (= 1 (counter-value component)))
           (clog-hypermedia:handle-action component :decrement nil)
           (is (= 0 (counter-value component)))
           (clog-hypermedia:handle-action component :increment nil)
           (clog-hypermedia:handle-action component :increment nil)
           (is (= 2 (counter-value component)))
           (clog-hypermedia:handle-action component :reset nil)
           (is (= 0 (counter-value component))))
      (when (clog-hypermedia:mounted-p component)
        (clog-hypermedia:unmount-component component)))))

(test counter/page/vendored-htmx-and-progressive-forms
  (let* ((application (make-counter-application))
         (configuration
           (clog-hypermedia:application-configuration application))
         (response
           (funcall (clog-hypermedia:as-clack-app application)
                    (make-get-env "/counter")))
         (body (response-body-text response)))
    (is (= 200 (first response)))
    (is (eq :vendored
            (clog-hypermedia:configuration-assets-mode configuration)))
    (is (clog-hypermedia:configuration-strict-csp-p configuration))
    (is (search "/_clog/static/vendor/htmx/4.0.0/htmx.min.js" body))
    (is (search "/_clog/static/vendor/htmx/4.0.0/hx-csp.min.js" body))
    (is (= 3 (count-substring "hx-post=\"/_clog/action/" body)))
    (is (= 3 (count-substring "hx-swap=\"outerMorph\"" body)))
    (is (= 3 (count-substring "hx-target=\"#clog-c-" body)))
    (is (= 3 (count-substring "method=\"post\"" body)))
    (is (search "name=\"_csrf_token\"" body))
    (is (search "data-clog-component=\"true\"" body))
    (is-false (search "https://" body))))

(test counter/assets/vendored-file-is-served-locally
  (let* ((application (make-counter-application))
         (response
           (funcall
            (clog-hypermedia:as-clack-app application)
            (make-get-env "/_clog/static/vendor/htmx/4.0.0/htmx.min.js"))))
    (is (= 200 (first response)))))

(test counter/server/explicit-start-stop-and-random-port
  (let ((server nil))
    (unwind-protect
         (progn
           (setf server (start-counter :host "127.0.0.1" :port 0))
           (is (counter-server-running-p server))
           (is (<= 1 (counter-server-port server) 65535))
           (is (search "http://127.0.0.1:"
                       (counter-server-url server)))
           (stop-counter server)
           (is-false (counter-server-running-p server))
           ;; Shutdown is deliberately idempotent for REPL cleanup paths.
           (stop-counter server)
           (is-false (counter-server-running-p server)))
      (when (and server (counter-server-running-p server))
        (stop-counter server)))))

(defun run-tests ()
  "Run the HM-027 Counter FiveAM suite and signal on any failure."
  (let ((results (run 'counter-tests)))
    (unless results
      (error "HM-027 Counter tests executed no checks."))
    (explain! results)
    (multiple-value-bind (success failed-tests skipped-tests)
        (results-status results)
      (declare (ignore skipped-tests))
      (unless success
        (error "HM-027 Counter tests failed: ~S" failed-tests))
      t)))
