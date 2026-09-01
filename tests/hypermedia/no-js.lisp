;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-026 no-JavaScript fallback tests          ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defclass hm-026-counter-component (clog-hypermedia:component)
  ((value :initform 0 :accessor hm-026-value)))

(defmethod clog-hypermedia:render-component
    ((component hm-026-counter-component) context)
  (spinneret:with-html-string
    (:section
     :attrs (clog-hypermedia:component-root-attributes component context)
     (:span :class "value" (format nil "~D" (hm-026-value component))))))

(defun hm-026-decode-delta (context)
  (parse-integer
   (or (clog-hypermedia:form-param context "delta" nil)
       (error "missing delta"))
   :junk-allowed nil))

(defun hm-026-decode-required-number (context)
  (parse-integer
   (or (clog-hypermedia:form-param context "required" nil)
       (error "missing required"))
   :junk-allowed nil))

(clog-hypermedia:defaction
    (hm-026-counter-component :hm-026-increment
     :external-name "hm-026-increment"
     :parameter-decoder #'hm-026-decode-delta)
    (component delta)
  (incf (hm-026-value component) delta)
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (hm-026-counter-component :hm-026-invalid
     :external-name "hm-026-invalid"
     :parameter-decoder #'hm-026-decode-required-number)
    (component required)
  (incf (hm-026-value component) required)
  (clog-action::make-action-result))

(defun hm-026-make-fixture ()
  "Return HANDLER and session->component map for progressive fallback tests."
  (let* ((router (clog-hypermedia:make-router))
         (store (clog-hypermedia:make-memory-component-store))
         (components (make-hash-table :test #'equal)))
    (clog-hypermedia:add-route
     router :get "/counter"
     (lambda (context)
       (let* ((session-id (clog-hypermedia:request-session-id context))
              (component (gethash session-id components)))
         (unless component
           (setf component
                 (make-instance 'hm-026-counter-component
                                :scope :session
                                :owner-session-id session-id))
           (clog-hypermedia:mount-component component)
           (clog-hypermedia:store-component store session-id component)
           (setf (gethash (copy-seq session-id) components) component))
         (let ((flash (clog-htmx::consume-no-js-flash context))
               (validation (clog-htmx::consume-no-js-validation context)))
           (clog-hypermedia:html-response
            (format nil "~A|~A|~D|~A|~A|~A"
                    (clog-hypermedia:csrf-token-for context)
                    (clog-hypermedia:component-id component)
                    (clog-hypermedia:component-revision component)
                    (hm-026-value component)
                    (or flash "")
                    (or validation "")))))))
    (let* ((app
             (clog-hypermedia:make-hypermedia-application
              :name "hm-026"
              :router router
              :component-store store
              :configuration
              (make-test-configuration
               :assets-mode :none
               :strict-csp-p nil
               :static-prefix nil
               :static-root nil)))
           (handler (clog-hypermedia:application-handler app)))
      (values handler components))))

(defun hm-026-bootstrap (handler &optional cookie)
  "Return token, component id, revision, value, flash, validation and cookie."
  (let* ((response
           (funcall handler
                    (make-request-env
                     :method :get :path "/counter"
                     :headers (when cookie (list (cons "cookie" cookie))))))
         (parts (split-sequence:split-sequence #\| (response-body-text response))))
    (values (nth 0 parts)
            (nth 1 parts)
            (parse-integer (nth 2 parts))
            (parse-integer (nth 3 parts))
            (nth 4 parts)
            (nth 5 parts)
            (or cookie (cookie-pair-from-response response))
            response)))

(defun hm-026-post
    (handler cookie token component-id action
     &key (delta 1) (revision 0) (return-to "/counter") hx-p omit-required-p)
  "Submit one action with or without HX-Request."
  (let ((body
          (format nil
                  "_csrf_token=~A&_clog_revision=~D&_clog_return_to=~A&delta=~D~A"
                  token revision return-to delta
                  (if omit-required-p "" "&required=1"))))
    (funcall
     handler
     (make-request-env
      :method :post
      :path (format nil "/_clog/action/~A/~A" component-id action)
      :headers
      (append (list (cons "cookie" cookie))
              (when hx-p (list (cons "hx-request" "true"))))
      :content-type "application/x-www-form-urlencoded"
      :body body))))

(test no-js/prg/success-redirects-and-refresh-does-not-repost
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (declare (ignore components))
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore value flash validation))
      (let ((post
              (hm-026-post handler cookie token component-id
                           "hm-026-increment"
                           :delta 3 :revision revision :hx-p nil)))
        (is (= 303 (first post)))
        (is (string= "/counter" (clack-response-header post :location))))
      (multiple-value-bind (token-2 component-id-2 revision-2 value-2 flash-2 validation-2)
          (hm-026-bootstrap handler cookie)
        (declare (ignore token-2 component-id-2 revision-2 validation-2))
        (is (= 3 value-2))
        (is (string= "Action completed." flash-2)))
      ;; Browser refresh after PRG is another GET, so state is unchanged and
      ;; the one-shot flash has already been consumed.
      (multiple-value-bind (token-3 component-id-3 revision-3 value-3 flash-3 validation-3)
          (hm-026-bootstrap handler cookie)
        (declare (ignore token-3 component-id-3 revision-3 validation-3))
        (is (= 3 value-3))
        (is (string= "" flash-3))))))

(test no-js/htmx/preserves-fragment-response
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (declare (ignore components))
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore value flash validation))
      (let ((response
              (hm-026-post handler cookie token component-id
                           "hm-026-increment"
                           :delta 2 :revision revision :hx-p t)))
        (is (= 200 (first response)))
        (is (null (clack-response-header response :location)))
        (is (search "data-clog-revision=\"1\""
                    (response-body-text response)))
        (is (search ">2</span>" (response-body-text response)))))))

(test no-js/validation/stores-minimal-state-and-redirects
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (declare (ignore components))
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore value flash validation))
      (let ((response
              (hm-026-post handler cookie token component-id
                           "hm-026-invalid"
                           :revision revision :hx-p nil :omit-required-p t)))
        (is (= 303 (first response)))
        (is (string= "/counter" (clack-response-header response :location))))
      (multiple-value-bind (token-2 component-id-2 revision-2 value-2 flash-2 validation-2)
          (hm-026-bootstrap handler cookie)
        (declare (ignore token-2 component-id-2 revision-2 flash-2))
        (is (= 0 value-2))
        (is (string= "Action validation failed." validation-2)))
      (multiple-value-bind (token-3 component-id-3 revision-3 value-3 flash-3 validation-3)
          (hm-026-bootstrap handler cookie)
        (declare (ignore token-3 component-id-3 revision-3 value-3 flash-3))
        (is (string= "" validation-3))))))

(test no-js/security/unsafe-return-to-fails-closed
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (declare (ignore components))
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore value flash validation))
      (dolist (target '("https://evil.example/" "//evil.example/" "\\evil.example"))
        (let ((response
                (hm-026-post handler cookie token component-id
                             "hm-026-increment"
                             :delta 1 :revision revision
                             :return-to target :hx-p nil)))
          (is (= 303 (first response)))
          (is (string= "/" (clack-response-header response :location))))))))

(test no-js/errors/non-htmx-action-error-is-complete-redacted-page
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (declare (ignore components))
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore revision value flash validation))
      (let* ((response
               (hm-026-post handler cookie token component-id
                            "unknown-action" :hx-p nil))
             (body (response-body-text response)))
        (is (= 404 (first response)))
        (is (search "<html" body))
        (is (search "Not Found" body))
        (is-false (search "unknown-action" body))))))

(test no-js/security/csrf-denial-still-blocks-mutation
  (multiple-value-bind (handler components)
      (hm-026-make-fixture)
    (multiple-value-bind (token component-id revision value flash validation cookie)
        (hm-026-bootstrap handler)
      (declare (ignore token revision value flash validation))
      (let ((response
              (hm-026-post handler cookie "forged" component-id
                           "hm-026-increment" :hx-p nil)))
        (is (= 403 (first response))))
      (let ((component
              (loop for value being the hash-values of components
                    do (return value))))
        (is (= 0 (hm-026-value component)))
        (is (= 0 (clog-hypermedia:component-revision component)))))))
