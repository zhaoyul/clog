;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime component root contract tests               ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +hm-023-component-id+
  "clog-c-00000000000000000000000000002301")

(defparameter +hm-023-other-component-id+
  "clog-c-00000000000000000000000000002302")

(defparameter *hm-023-script-execution-count* 0
  "Sentinel proving fragment validation never executes script contents.")

(defclass hm-023-helper-component (clog-hypermedia:component)
  ((text
    :initarg :text
    :initform "root contract"
    :reader hm-023-helper-text)))

(defmethod clog-hypermedia:render-component
    ((component hm-023-helper-component) context)
  (spinneret:with-html-string
    (:machine-panel
     :attrs (clog-hypermedia:component-root-attributes
             component
             context
             :class "hm-023"
             :attrs '(:aria-live "polite" :data-kind "fixture"))
     (:span :id "hm-023-child" (hm-023-helper-text component)))))

(defclass hm-023-literal-component (clog-hypermedia:component)
  ((fragment
    :initarg :fragment
    :reader hm-023-literal-fragment)))

(defmethod clog-hypermedia:render-component
    ((component hm-023-literal-component) context)
  (declare (ignore context))
  (hm-023-literal-fragment component))

(defun hm-023-mounted-component (class &rest initargs)
  "Create and mount an application-scoped HM-023 fixture component."
  (let ((component
          (apply #'make-instance
                 class
                 :scope :application
                 initargs)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-023-request (&key (request-id "request-hm023"))
  "Return a normalized request fixture for root-contract correlation."
  (clog-hypermedia:make-request-context
   (make-request-env :method :get :path "/hm-023")
   :request-id request-id
   :csp-nonce "nonce-hm023"))

(defun hm-023-application (&key development-p)
  "Return a deterministic application with DEVELOPMENT-P configured."
  (clog-hypermedia:make-hypermedia-application
   :name "hm-023"
   :configuration
   (make-test-configuration
    :development-p development-p
    :assets-mode :none
    :strict-csp-p nil
    :static-prefix nil
    :static-root nil)))

(defun hm-023-context
    (&key (development-p nil) (mode :fragment) application-p)
  "Return a render context for explicit or automatic root validation."
  (clog-hypermedia:make-render-context
   :request (unless (eq mode :test) (hm-023-request))
   :application
   (when (or application-p (not (eq mode :test)))
     (hm-023-application :development-p development-p))
   :mode mode))

(defun hm-023-root-html
    (component &key
                 (tag "section")
                 (id (clog-hypermedia:component-dom-id component))
                 (marker "true")
                 (revision
                   (format nil "~D"
                           (clog-hypermedia:component-revision component)))
                 (body "<span>valid</span>"))
  "Return a deterministic explicit root-contract fixture."
  (format nil
          "<~A id=\"~A\" data-clog-component=\"~A\" data-clog-revision=\"~A\">~A</~A>"
          tag id marker revision body tag))

(defun hm-023-captured-root-violation (thunk)
  "Invoke THUNK and return its root-contract condition, or NIL."
  (handler-case
      (progn (funcall thunk) nil)
    (clog-hypermedia:render-contract-violation (condition)
      condition)))

(defun hm-023-assert-root-violation
    (expected-kind component context html)
  "Assert that validating HTML fails with EXPECTED-KIND."
  (let ((condition
          (hm-023-captured-root-violation
           (lambda ()
             (clog-hypermedia:validate-component-root
              component context html)))))
    (is (typep condition 'clog-hypermedia:render-contract-violation))
    (is (typep condition 'clog-hypermedia:component-error))
    (is (eq expected-kind
            (and condition
                 (clog-hypermedia:render-contract-violation-kind
                  condition))))
    condition))

(test root-contract/helper/produces-stable-protected-attributes
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context :mode :test))
         (class (copy-seq "hm-023"))
         (label (copy-seq "设备状态"))
         (attrs (list :aria-label label :data-kind "fixture"))
         (attributes
           (clog-hypermedia:component-root-attributes
            component context :class class :attrs attrs)))
    (setf (char class 0) #\X
          (char label 0) #\X)
    (is (equal
         `(:id ,+hm-023-component-id+
           :data-clog-component "true"
           :data-clog-revision "0"
           :class "hm-023"
           :aria-label "设备状态"
           :data-kind "fixture")
         attributes))
    (let ((first (clog-hypermedia:component-dom-id component))
          (second (clog-hypermedia:component-dom-id component)))
      (setf (char first 7) #\f)
      (is (string= +hm-023-component-id+ second)))
    (let ((html (clog-hypermedia:render component context)))
      (is (search (format nil "id=\"~A\"" +hm-023-component-id+) html))
      (is (search
           "data-clog-component=\"true\""
           html))
      (is (search "data-clog-revision=\"0\"" html))
      (is (search "<machine-panel" html))
      (is (search "<span id=\"hm-023-child\">root contract</span>" html)))
    (dolist (invalid
             (list '(:id "replacement")
                   '(:data-clog-component "replacement")
                   '(:data-clog-revision "7")
                   '(:aria-label "first" :aria-label "second")))
      (signals clog-hypermedia:render-contract-violation
        (clog-hypermedia:component-root-attributes
         component context :attrs invalid)))
    (signals clog-hypermedia:render-contract-violation
      (clog-hypermedia:component-root-attributes
       component context :class "first" :attrs '(:class "second")))
    (signals clog-hypermedia:render-contract-violation
      (clog-hypermedia:component-root-attributes
       component context :attrs '(:aria-label)))))

(test root-contract/validator/accepts-custom-element-and-benign-envelope
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context))
         (html
           (format nil
                   " ~%<!-- before --><machine-card id=\"~A\" data-clog-component=\"true\" data-clog-revision=\"0\"><span id=\"inside\">设备 🧪</span></machine-card><!-- after -->~%"
                   +hm-023-component-id+)))
    (is (string=
         html
         (clog-hypermedia:validate-component-root
          component context html)))))

(test root-contract/validator/rejects-invalid-fragment-corpus
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context))
         (valid (hm-023-root-html component)))
    (hm-023-assert-root-violation
     :missing-root-element component context "")
    (hm-023-assert-root-violation
     :non-whitespace-root-text
     component context
     (concatenate 'string "outside" valid))
    (hm-023-assert-root-violation
     :multiple-root-elements
     component context
     (concatenate 'string valid valid))
    (hm-023-assert-root-violation
     :missing-root-id
     component context
     (format nil
             "<section data-clog-component=\"true\" data-clog-revision=\"0\"></section>"))
    (hm-023-assert-root-violation
     :root-id-mismatch
     component context
     (hm-023-root-html component :id +hm-023-other-component-id+))
    (hm-023-assert-root-violation
     :missing-component-marker
     component context
     (format nil
             "<section id=\"~A\" data-clog-revision=\"0\"></section>"
             +hm-023-component-id+))
    (hm-023-assert-root-violation
     :component-marker-mismatch
     component context
     (hm-023-root-html component :marker +hm-023-other-component-id+))
    (hm-023-assert-root-violation
     :missing-root-revision
     component context
     (format nil
             "<section id=\"~A\" data-clog-component=\"true\"></section>"
             +hm-023-component-id+))
    (hm-023-assert-root-violation
     :root-revision-mismatch
     component context
     (hm-023-root-html component :revision "99"))
    (hm-023-assert-root-violation
     :duplicate-element-id
     component context
     (hm-023-root-html
      component
      :body
      (format nil "<span id=\"~A\">duplicate</span>"
              +hm-023-component-id+)))))

(test root-contract/render-around/enables-only-test-and-development-parse
  (let* ((valid
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (invalid
           (hm-023-mounted-component
            'hm-023-literal-component
            :id +hm-023-other-component-id+
            :fragment "<section>legacy fragment</section>"))
         (production (hm-023-context :development-p nil))
         (development (hm-023-context :development-p t))
         (test-context (hm-023-context :mode :test)))
    ;; Production avoids the complete DOM parse, but helper-generated metadata
    ;; remains present and can be validated explicitly.
    (is (string=
         "<section>legacy fragment</section>"
         (clog-hypermedia:render invalid production)))
    (let ((html (clog-hypermedia:render valid production)))
      (is (string=
           html
           (clog-hypermedia:validate-component-root
            valid production html))))
    (let ((condition
            (hm-023-captured-root-violation
             (lambda ()
               (clog-hypermedia:render invalid development)))))
      (is (eq :missing-root-id
              (clog-hypermedia:render-contract-violation-kind condition)))
      (is (string=
           +hm-023-other-component-id+
           (clog-hypermedia:rendering-error-component-id condition)))
      (is (string=
           "request-hm023"
           (clog-hypermedia:rendering-error-request-id condition))))
    (is (eq :missing-root-id
            (clog-hypermedia:render-contract-violation-kind
             (hm-023-captured-root-violation
              (lambda ()
                (clog-hypermedia:render invalid test-context))))))))

(test root-contract/script/is-parsed-as-inert-text
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context))
         (script
           "window.__clog_hm023 = (window.__clog_hm023 || 0) + 1; const fake = '<div id=\"not-a-root\"></div>'; ")
         (html
           (hm-023-root-html
            component
            :tag "machine-panel"
            :body (format nil "<script>~A</script><span>safe</span>" script)))
         (before *hm-023-script-execution-count*))
    (is (string=
         html
         (clog-hypermedia:validate-component-root
          component context html)))
    (is (= before *hm-023-script-execution-count*))
    (is (search script html))))

(test root-contract/revision/current-revision-is-exact
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context))
         (revision-zero (hm-023-root-html component)))
    (is (string=
         revision-zero
         (clog-hypermedia:validate-component-root
          component context revision-zero)))
    (clog-hypermedia:touch-component component)
    (hm-023-assert-root-violation
     :root-revision-mismatch component context revision-zero)
    (let ((attributes
            (clog-hypermedia:component-root-attributes component context)))
      (is (string= "1" (getf attributes :data-clog-revision))))))

(test root-contract/condition/report-is-redacted
  (let* ((component
           (hm-023-mounted-component
            'hm-023-helper-component
            :id +hm-023-component-id+))
         (context (hm-023-context))
         (secret "secret-fragment-value")
         (condition
           (hm-023-assert-root-violation
            :root-id-mismatch
            component context
            (hm-023-root-html component :id secret)))
         (report (princ-to-string condition)))
    (is-false (search secret report))
    (is-false (search "/hm-023" report))
    (is (search "ROOT-ID-MISMATCH" report :test #'char-equal))))
