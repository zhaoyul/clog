;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HTMX adapter tests                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun parsed-hx-trigger-events (response)
  "Parse RESPONSE's HX-Trigger JSON object as an ordered alist for assertions."
  (let ((value (clog-hypermedia:response-header response :hx-trigger nil)))
    (and value (yason:parse value :object-as :alist))))

(test htmx/request-adapter-uses-normalized-request-context
  (let* ((env (make-request-env
               :method :post
               :path "/machines/42"
               :headers '(("hX-rEqUeSt" . "true")
                          ("Hx-TaRgEt" . "div#machine-card")
                          ("HX-Trigger" . "button#refresh")
                          ("hx-current-URL" . "http://localhost/machines/42"))))
         (context (clog-hypermedia:make-request-context env)))
    (is-true (clog-hypermedia:htmx-request-p context))
    (is (string= "div#machine-card"
                 (clog-hypermedia:hx-target context)))
    (is (string= "button#refresh"
                 (clog-hypermedia:hx-trigger context)))
    (is (string= "http://localhost/machines/42"
                 (clog-hypermedia:hx-current-url context)))
    ;; Adapter accessors must preserve request-context defensive-copy semantics.
    (let ((target (clog-hypermedia:hx-target context)))
      (setf (char target 0) #\X))
    (is (string= "div#machine-card"
                 (clog-hypermedia:hx-target context)))))

(test htmx/response-trigger-json-merges-and-escapes
  (let* ((response (clog-hypermedia:html-response "ok"))
         (message (format nil "quote ~S~Cline~C~Ctail"
                          "value" #\Newline #\Return #\Newline)))
    (is (eq response
            (clog-hypermedia:set-hx-trigger response "message" message)))
    (is (eq response
            (clog-hypermedia:set-hx-trigger response "refresh" 1)))
    ;; Repeating an event replaces its value without emitting a duplicate JSON key.
    (clog-hypermedia:set-hx-trigger response "refresh" 2)
    (clog-hypermedia:set-hx-trigger response "done")
    (let* ((header (clog-hypermedia:response-header response :hx-trigger))
           (events (parsed-hx-trigger-events response)))
      (is (equal '("message" "refresh" "done")
                 (mapcar #'car events)))
      (is (string= message (cdr (assoc "message" events :test #'string=))))
      (is (= 2 (cdr (assoc "refresh" events :test #'string=))))
      (is-true (cdr (assoc "done" events :test #'string=)))
      ;; JSON escaping must prevent payload newlines from becoming header injection.
      (is (null (find #\Return header)))
      (is (null (find #\Newline header)))
      (is (= 1 (length (clog-hypermedia:response-header-values
                        response :hx-trigger))))
      ;; HM-011 remains the final response-header validation boundary.
      (is (= 200
             (first (clog-hypermedia:response->clack-response response)))))))

(test htmx/response-url-and-refresh-headers-are-typed
  (dolist (entry `((,#'clog-hypermedia:set-hx-location . :hx-location)
                   (,#'clog-hypermedia:set-hx-redirect . :hx-redirect)
                   (,#'clog-hypermedia:set-hx-push-url . :hx-push-url)
                   (,#'clog-hypermedia:set-hx-replace-url . :hx-replace-url)))
    (let ((response (clog-hypermedia:html-response "ok")))
      (is (eq response (funcall (car entry) response "/dashboard?tab=live")))
      (is (string= "/dashboard?tab=live"
                   (clog-hypermedia:response-header response (cdr entry))))
      ;; A second call replaces the same HX header rather than duplicating it.
      (funcall (car entry) response "/dashboard/final")
      (is (equal '("/dashboard/final")
                 (clog-hypermedia:response-header-values response (cdr entry))))))
  (let ((response (clog-hypermedia:html-response
                   "ok" :headers '(:hx-refresh "false"))))
    (is (eq response (clog-hypermedia:set-hx-refresh response)))
    (is (equal '("true")
               (clog-hypermedia:response-header-values response :hx-refresh)))))

(test htmx/response-adapter-rejects-open-redirects-and-header-injection
  (let ((url-setters (list #'clog-hypermedia:set-hx-location
                           #'clog-hypermedia:set-hx-redirect
                           #'clog-hypermedia:set-hx-push-url
                           #'clog-hypermedia:set-hx-replace-url)))
    (dolist (setter url-setters)
      (dolist (url '("//evil.example/path"
                     "https://evil.example/path"
                     "javascript:alert(1)"
                     "/\\evil.example/path"))
        (signals clog-hypermedia:invalid-redirect-url
          (funcall setter (clog-hypermedia:html-response "ok") url)))
      (signals clog-hypermedia:invalid-redirect-url
        (funcall setter
                 (clog-hypermedia:html-response "ok")
                 (format nil "/safe~C~CX-Injected: yes"
                         #\Return #\Newline)))))
  ;; Event names are protocol metadata, so control characters are rejected.
  (signals clog-hypermedia:invalid-response-header
    (clog-hypermedia:set-hx-trigger
     (clog-hypermedia:html-response "ok")
     (format nil "saved~CX-Injected" #\Newline)
     "payload")))
