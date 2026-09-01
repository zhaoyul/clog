;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HM-033 action-result contract tests         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)
(in-suite clog-hypermedia-tests)

(defparameter +hm-033-component-a+
  "clog-c-00000000000000000000000000003301")
(defparameter +hm-033-component-b+
  "clog-c-00000000000000000000000000003302")
(defparameter +hm-033-component-c+
  "clog-c-00000000000000000000000000003303")

(defclass hm-033-result-component (clog-hypermedia:component)
  ((text :initarg :text :reader hm-033-text)))

(defmethod clog-hypermedia:render-component
    ((component hm-033-result-component) context)
  (spinneret:with-html-string
    (:section :attrs (clog-hypermedia:component-root-attributes
                      component context)
              (:span (hm-033-text component)))))

(defun hm-033-component (id text)
  (let ((component
          (make-instance 'hm-033-result-component
                         :id id
                         :scope :application
                         :text text)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-033-application ()
  (clog-hypermedia:make-hypermedia-application
   :name "hm-033"
   :configuration
   (make-test-configuration
    :assets-mode :none
    :strict-csp-p nil
    :static-prefix nil
    :static-root nil)))

(defun hm-033-request ()
  (hm-022-request
   :request-id "request-hm033"
   :session-id "session-hm033"))

(defun hm-033-map (result application current-component)
  (clog-hypermedia:action-result->response
   result application current-component (hm-033-request)))

(defun hm-033-count-substring (needle haystack)
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(test action-result/model/public-contract-and-defensive-values
  (let* ((component
           (hm-033-component +hm-033-component-a+ "alpha"))
         (headers (list :x-test "owned"))
         (result
           (clog-hypermedia:make-action-result
            :primary-component component
            :response-headers headers
            :status 201)))
    (is (clog-hypermedia:action-result-p result))
    (is (eq component
            (clog-hypermedia:action-result-primary-component result)))
    (is (= 201 (clog-hypermedia:action-result-status result)))
    (is (equal '(:x-test "owned")
               (clog-hypermedia:action-result-response-headers result)))
    (setf (second headers) "mutated")
    (is (equal '(:x-test "owned")
               (clog-hypermedia:action-result-response-headers result)))
    (let ((returned
            (clog-hypermedia:action-result-response-headers result)))
      (setf (second returned) "changed")
      (is (equal '(:x-test "owned")
                 (clog-hypermedia:action-result-response-headers result))))))

(test action-result/mapping/render-self-is-current-fragment
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current <safe>"))
         (response
           (hm-033-map (clog-hypermedia:render-self)
                       application current))
         (body (clog-hypermedia:response-body response)))
    (is (= 200 (clog-hypermedia:response-status response)))
    (is (eq :html (clog-hypermedia:response-kind response)))
    (is (search +hm-033-component-a+ body))
    (is (search "current &lt;safe&gt;" body))
    (is-false (search "<hx-partial" body))))

(test action-result/mapping/render-components-is-pure-multi-partial
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (a (hm-033-component +hm-033-component-a+ "alpha"))
         (b (hm-033-component +hm-033-component-b+ "beta"))
         (c (hm-033-component +hm-033-component-c+ "gamma"))
         (response
           (hm-033-map
            (clog-hypermedia:render-components a b c)
            application current))
         (body (clog-hypermedia:response-body response)))
    (is (= 200 (clog-hypermedia:response-status response)))
    (is (= 3 (hm-033-count-substring "<hx-partial" body)))
    (is (= 3 (hm-033-count-substring "</hx-partial>" body)))
    (is (search +hm-033-component-a+ body))
    (is (search +hm-033-component-b+ body))
    (is (search +hm-033-component-c+ body))))

(test action-result/mapping/current-fragment-can-carry-extra-partials
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (b (hm-033-component +hm-033-component-b+ "beta"))
         (c (hm-033-component +hm-033-component-c+ "gamma"))
         (result
           (clog-hypermedia:make-action-result
            :primary-component :current
            :invalidated-components (list b c)))
         (response (hm-033-map result application current))
         (body (clog-hypermedia:response-body response)))
    (is (= 200 (clog-hypermedia:response-status response)))
    (is (search (format nil "id=\"~A\"" +hm-033-component-a+) body))
    (is (= 2 (hm-033-count-substring "<hx-partial" body)))
    (is (search +hm-033-component-b+ body))
    (is (search +hm-033-component-c+ body))))

(test action-result/mapping/no-render-is-explicit-204
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (response
           (hm-033-map (clog-hypermedia:no-render)
                       application current)))
    (is (= 204 (clog-hypermedia:response-status response)))
    (is (eq :empty (clog-hypermedia:response-kind response)))
    (is (null (clog-hypermedia:response-body response)))))

(test action-result/mapping/history-and-redirect-use-typed-htmx-headers
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (push-response
           (hm-033-map (clog-hypermedia:push-url "/machines/42")
                       application current))
         (replace-response
           (hm-033-map (clog-hypermedia:replace-url "/machines/43")
                       application current))
         (redirect-response
           (hm-033-map (clog-hypermedia:redirect-to "/done")
                       application current)))
    (is (string= "/machines/42"
                 (clog-hypermedia:response-header
                  push-response :hx-push-url)))
    (is (search +hm-033-component-a+
                (clog-hypermedia:response-body push-response)))
    (is (string= "/machines/43"
                 (clog-hypermedia:response-header
                  replace-response :hx-replace-url)))
    (is (search +hm-033-component-a+
                (clog-hypermedia:response-body replace-response)))
    (is (= 200 (clog-hypermedia:response-status redirect-response)))
    (is (string= "/done"
                 (clog-hypermedia:response-header
                  redirect-response :hx-redirect)))
    (is (string= "" (clog-hypermedia:response-body redirect-response)))
    (is-false (search +hm-033-component-a+
                      (clog-hypermedia:response-body redirect-response)))))

(test action-result/mapping/toast-and-effects-merge-into-hx-trigger-json
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (base
           (clog-hypermedia:make-action-result
            :primary-component :current
            :flash "Saved"))
         (result
           (clog-hypermedia:with-effect "focus:machine-name" base))
         (response (hm-033-map result application current))
         (header
           (clog-hypermedia:response-header response :hx-trigger))
         (events (yason:parse header :object-as :alist)))
    (is (= 1 (hm-033-count-substring "HX-Trigger" "HX-Trigger")))
    (is (stringp header))
    (is (assoc "clog:toast" events :test #'string=))
    (is (string= "Saved"
                 (cdr (assoc "clog:toast" events :test #'string=))))
    (is (assoc "clog:effects" events :test #'string=))
    (is (search "focus:machine-name" header))))

(test action-result/mapping/explicit-response-headers-are-preserved
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current"))
         (response
           (hm-033-map
            (clog-hypermedia:make-action-result
             :response-headers '(:x-action-result "hm033"))
            application current)))
    (is (string= "hm033"
                 (clog-hypermedia:response-header
                  response :x-action-result)))))

(test action-result/security/invalid-combinations-fail-closed
  (let ((other
          (hm-033-component +hm-033-component-b+ "other")))
    ;; Redirect is a body-less navigation variant.
    (signals clog-hypermedia:invalid-action-result
      (clog-hypermedia:make-action-result :redirect-url "/done"))
    (signals clog-hypermedia:invalid-action-result
      (clog-hypermedia:make-action-result
       :primary-component nil
       :invalidated-components (list other)
       :redirect-url "/done"))
    ;; One response cannot apply competing history mutations.
    (signals clog-hypermedia:invalid-action-result
      (clog-hypermedia:make-action-result
       :push-url "/a" :replace-url "/b"))
    ;; 204 is incompatible with a fragment body.
    (signals clog-hypermedia:invalid-action-result
      (clog-hypermedia:make-action-result :status 204))
    ;; Mapper-owned HTMX/framing headers cannot be smuggled through raw headers.
    (dolist (headers '((:hx-redirect "/raw")
                       (:hx-trigger "raw")
                       (:content-length "999")
                       (:location "/raw")))
      (signals clog-hypermedia:invalid-action-result
        (clog-hypermedia:make-action-result
         :response-headers headers)))))

(test action-result/security/redirect-history-urls-use-same-origin-validation
  (let* ((application (hm-033-application))
         (current
           (hm-033-component +hm-033-component-a+ "current")))
    (dolist (url '("https://evil.example/steal"
                   "//evil.example/steal"
                   "\\evil.example\\steal"))
      (signals clog-hypermedia:invalid-redirect-url
        (hm-033-map (clog-hypermedia:redirect-to url)
                    application current)))
    (signals clog-hypermedia:invalid-redirect-url
      (hm-033-map (clog-hypermedia:push-url "https://evil.example/a")
                  application current))
    (signals clog-hypermedia:invalid-redirect-url
      (hm-033-map (clog-hypermedia:replace-url "//evil.example/a")
                  application current))))

(test action-result/model/with-effect-does-not-mutate-input-result
  (let* ((base (clog-hypermedia:no-render))
         (extended
           (clog-hypermedia:with-effect "toast:queued" base)))
    (is (null (clog-hypermedia:action-result-effects base)))
    (is (equal '("toast:queued")
               (clog-hypermedia:action-result-effects extended)))
    (let ((returned (clog-hypermedia:action-result-effects extended)))
      (setf (first returned) "mutated")
      (is (equal '("toast:queued")
                 (clog-hypermedia:action-result-effects extended))))))
