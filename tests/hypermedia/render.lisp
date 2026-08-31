;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime render-context and Spinneret tests           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +hm-022-component-id+
  "clog-c-00000000000000000000000000002201")

(defparameter +hm-022-extra-component-id+
  "clog-c-00000000000000000000000000002202")

(defclass hm-022-render-component (clog-hypermedia:component)
  ((text
    :initarg :text
    :reader hm-022-render-text)
   (trusted
    :initarg :trusted
    :initform nil
    :reader hm-022-render-trusted)))

(defmethod clog-hypermedia:render-component
    ((component hm-022-render-component) context)
  (declare (ignore context))
  (spinneret:with-html-string
    (:section
     :class "hm-022"
     :title (hm-022-render-text component)
     (:p (hm-022-render-text component))
     (when (hm-022-render-trusted component)
       (:div :class "trusted"
             (hm-022-render-trusted component))))))

(defmethod clog-hypermedia:component-title
    ((component hm-022-render-component) context)
  (declare (ignore component context))
  "HM-022 Component")

(defclass hm-022-context-component (clog-hypermedia:component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-022-context-component) context)
  (declare (ignore context))
  (spinneret:with-html-string
    (:div
     :data-component
     (clog-hypermedia:component-id
      (clog-hypermedia:current-render-component))
     :data-mode
     (string-downcase
      (symbol-name (clog-hypermedia:current-render-mode)))
     :data-locale
     (princ-to-string (clog-hypermedia:current-render-locale))
     :data-nonce
     (clog-hypermedia:current-render-csp-nonce)
     :data-request
     (clog-hypermedia:request-id
      (clog-hypermedia:current-render-request))
     "bound")))

(defclass hm-022-revision-mutator (clog-hypermedia:component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-022-revision-mutator) context)
  (declare (ignore context))
  (clog-hypermedia:touch-component component)
  (spinneret:with-html-string (:div "impure")))

(defclass hm-022-registry-mutator (clog-hypermedia:component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-022-registry-mutator) context)
  (let* ((request (clog-hypermedia:render-context-request context))
         (application (clog-hypermedia:render-context-application context))
         (session-id (clog-hypermedia:request-session-id request))
         (store (clog-hypermedia:application-component-store application))
         (extra
           (make-instance
            'hm-022-render-component
            :id +hm-022-extra-component-id+
            :owner-session-id session-id
            :text "extra")))
    (declare (ignore component))
    (clog-hypermedia:mount-component extra)
    (clog-hypermedia:store-component store session-id extra)
    (spinneret:with-html-string (:div "registry-mutated"))))

(defclass hm-022-error-component (clog-hypermedia:component) ())

(defmethod clog-hypermedia:render-component
    ((component hm-022-error-component) context)
  (declare (ignore component context))
  (error "sensitive renderer detail"))

(defun hm-022-request
    (&key
       (request-id "request-hm022")
       (nonce "nonce-hm022")
       (session-id "session-hm022")
       (session (make-hash-table :test #'equal)))
  "Create a normalized request fixture with stable render correlation data."
  (clog-hypermedia:make-request-context
   (make-request-env
    :method :get
    :path "/hm-022"
    :session session
    :session-id session-id)
   :request-id request-id
   :csp-nonce nonce))

(defun hm-022-application (&key component-store)
  "Create a deterministic test application suitable for isolated rendering."
  (clog-hypermedia:make-hypermedia-application
   :name "hm-022"
   :component-store component-store
   :configuration
   (make-test-configuration
    :assets-mode :none
    :strict-csp-p nil
    :static-prefix nil
    :static-root nil)))

(defun hm-022-mounted-application-component
    (class &rest initargs)
  "Construct and mount an application-scoped test component."
  (let ((component
          (apply #'make-instance
                 class
                 :scope :application
                 initargs)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-022-mounted-session-component
    (class session-id &rest initargs)
  "Construct and mount a session-owned test component."
  (let ((component
          (apply #'make-instance
                 class
                 :owner-session-id session-id
                 initargs)))
    (clog-hypermedia:mount-component component)
    component))

(test render/context/is-immutable-validated-and-request-derived
  (let* ((request (hm-022-request))
         (application (hm-022-application))
         (target (copy-seq "#machine-card"))
         (locale (copy-seq "zh-CN"))
         (asset
           (clog-hypermedia:make-asset
            :type :style
            :url "/styles/hm022.css"
            :key :hm022-style))
         (assets (list asset))
         (context
           (clog-hypermedia:make-render-context
            :request request
            :application application
            :mode :fragment
            :target target
            :locale locale
            :assets assets
            :primary-component-id +hm-022-component-id+)))
    (setf (char target 1) #\X)
    (setf (char locale 0) #\X)
    (setf (car assets) nil)
    (is (eq request
            (clog-hypermedia:render-context-request context)))
    (is (eq application
            (clog-hypermedia:render-context-application context)))
    (is (eq :fragment
            (clog-hypermedia:render-context-mode context)))
    (is (string= "#machine-card"
                 (clog-hypermedia:render-context-target context)))
    (is (string= "zh-CN"
                 (clog-hypermedia:render-context-locale context)))
    (is (string= "nonce-hm022"
                 (clog-hypermedia:render-context-csp-nonce context)))
    (is (string= +hm-022-component-id+
                 (clog-hypermedia:render-context-primary-component-id
                  context)))
    (is (eq asset
            (first (clog-hypermedia:render-context-assets context))))
    (let ((returned-target
            (clog-hypermedia:render-context-target context))
          (returned-assets
            (clog-hypermedia:render-context-assets context)))
      (setf (char returned-target 1) #\Y)
      (setf (car returned-assets) nil)
      (is (string= "#machine-card"
                   (clog-hypermedia:render-context-target context)))
      (is (eq asset
              (first
               (clog-hypermedia:render-context-assets context))))))
  (signals clog-hypermedia:invalid-render-context
    (clog-hypermedia:make-render-context :mode :unknown))
  (signals clog-hypermedia:invalid-render-context
    (clog-hypermedia:make-render-context :mode :fragment))
  (signals clog-hypermedia:invalid-render-context
    (clog-hypermedia:make-render-context
     :mode :test
     :primary-component-id "not-a-component-id"))
  (signals clog-hypermedia:invalid-render-context
    (clog-hypermedia:make-render-context
     :request (hm-022-request)
     :mode :fragment
     :csp-nonce "different-nonce")))

(test render/text/default-escaping-and-explicit-trusted-html
  (let ((context
          (clog-hypermedia:make-render-context :mode :test))
        (plain "<button title=\"x\">A & B 'quoted'</button>")
        (trusted
          (clog-hypermedia:make-trusted-html
           "<strong data-kind=\"trusted\">Ready</strong>")))
    (let ((escaped (clog-hypermedia:render plain context)))
      (is (search "&lt;button" escaped))
      (is (search "&quot;x&quot;" escaped))
      (is (search "A &amp; B" escaped))
      (is (search "&#39;quoted&#39;" escaped))
      (is-false (search "<button" escaped)))
    (is (string=
         "<strong data-kind=\"trusted\">Ready</strong>"
         (clog-hypermedia:render trusted context)))
    (let ((copy (clog-hypermedia:trusted-html-string trusted)))
      (setf (char copy 1) #\X)
      (is (string=
           "<strong data-kind=\"trusted\">Ready</strong>"
           (clog-hypermedia:trusted-html-string trusted))))))

(test render/component/snapshot-determinism-fragment-and-utf8
  (let* ((component
           (hm-022-mounted-application-component
            'hm-022-render-component
            :id +hm-022-component-id+
            :text "设备 λ 🧪"))
         (context
           (clog-hypermedia:make-render-context
            :request (hm-022-request)
            :application (hm-022-application)
            :mode :fragment
            :locale "zh-CN"
            :primary-component-id +hm-022-component-id+))
         (first (clog-hypermedia:render component context))
         (second (clog-hypermedia:render component context)))
    (is (string= first second))
    (is (string=
         "<section class=\"hm-022\" title=\"设备 λ 🧪\"><p>设备 λ 🧪</p></section>"
         first))
    (is (search "设备 λ 🧪" first))
    (is-false (search "<html" first :test #'char-equal))
    (is-false (search "<head" first :test #'char-equal))
    (is-false (search "<body" first :test #'char-equal))
    (is (= 0 (clog-hypermedia:component-revision component)))))

(test render/component/dynamic-bindings-are-complete-and-scoped
  (let* ((request (hm-022-request))
         (component
           (hm-022-mounted-application-component
            'hm-022-context-component
            :id "clog-c-00000000000000000000000000002203"))
         (context
           (clog-hypermedia:make-render-context
            :request request
            :application (hm-022-application)
            :mode :partial
            :locale "zh-CN"
            :csp-nonce "nonce-hm022"))
         (html (clog-hypermedia:render component context)))
    (is (search
         "data-component=\"clog-c-00000000000000000000000000002203\""
         html))
    (is (search "data-mode=\"partial\"" html))
    (is (search "data-locale=\"zh-CN\"" html))
    (is (search "data-nonce=\"nonce-hm022\"" html))
    (is (search "data-request=\"request-hm022\"" html))
    (is (null (clog-hypermedia:current-render-context)))
    (is (null (clog-hypermedia:current-render-component)))
    (is (null (clog-hypermedia:current-render-request)))
    (is (null (clog-hypermedia:current-render-mode)))))

(test render/purity/revision-mutation-is-rejected
  (let* ((component
           (hm-022-mounted-application-component
            'hm-022-revision-mutator
            :id "clog-c-00000000000000000000000000002204"))
         (context
           (clog-hypermedia:make-render-context
            :request (hm-022-request)
            :application (hm-022-application)
            :mode :fragment)))
    (handler-case
        (progn
          (clog-hypermedia:render component context)
          (fail "Impure renderer should have signaled."))
      (clog-hypermedia:render-purity-violation (condition)
        (is (eq :component-revision
                (clog-hypermedia:render-purity-violation-kind condition)))
        (is (string=
             (clog-hypermedia:component-id component)
             (clog-hypermedia:rendering-error-component-id condition)))
        (is (string=
             "request-hm022"
             (clog-hypermedia:rendering-error-request-id condition)))))
    (is (= 1 (clog-hypermedia:component-revision component)))))

(test render/purity/session-registry-mutation-is-rejected
  (let* ((session-id "session-hm022-registry")
         (store (clog-hypermedia:make-memory-component-store))
         (request (hm-022-request :session-id session-id))
         (application (hm-022-application :component-store store))
         (component
           (hm-022-mounted-session-component
            'hm-022-registry-mutator
            session-id
            :id "clog-c-00000000000000000000000000002205")))
    (clog-hypermedia:store-component store session-id component)
    (let ((context
            (clog-hypermedia:make-render-context
             :request request
             :application application
             :mode :fragment)))
      (handler-case
          (progn
            (clog-hypermedia:render component context)
            (fail "Registry mutation should have signaled."))
        (clog-hypermedia:render-purity-violation (condition)
          (is (eq :component-registry
                  (clog-hypermedia:render-purity-violation-kind condition)))))
      (is (= 2
             (length
              (clog-hypermedia:enumerate-components
               store session-id)))))))

(test render/errors/carry-bounded-component-and-request-context
  (let* ((component
           (hm-022-mounted-application-component
            'hm-022-error-component
            :id "clog-c-00000000000000000000000000002206"))
         (context
           (clog-hypermedia:make-render-context
            :request (hm-022-request :request-id "request-contextual")
            :application (hm-022-application)
            :mode :fragment)))
    (handler-case
        (progn
          (clog-hypermedia:render component context)
          (fail "Renderer error should have been contextualized."))
      (clog-hypermedia:rendering-error (condition)
        (is (eq :component-render-failed
                (clog-hypermedia:rendering-error-reason condition)))
        (is (string=
             (clog-hypermedia:component-id component)
             (clog-hypermedia:rendering-error-component-id condition)))
        (is (string=
             "request-contextual"
             (clog-hypermedia:rendering-error-request-id condition)))
        (is (typep (clog-hypermedia:rendering-error-cause condition)
                   'simple-error))
        (let ((report (princ-to-string condition)))
          (is-false (search "sensitive renderer detail" report))
          (is-false (search "session-hm022" report)))))))

(test render/page/component-path-uses-page-shell-without-breaking-legacy
  (let ((router (clog-hypermedia:make-router))
        (application nil)
        (component
          (hm-022-mounted-application-component
           'hm-022-render-component
           :id "clog-c-00000000000000000000000000002207"
           :text "页面内容")))
    (clog-hypermedia:add-route
     router
     :get
     "/hm-022"
     (lambda (request)
       ;; Complete page rendering normally runs inside the application's Lack
       ;; CSRF middleware dynamic scope. Set a stable token in that live session
       ;; so the page snapshot proves the production path instead of relying on
       ;; private Lack specials in an isolated unit call.
       (setf (gethash "_csrf_token"
                      (clog-hypermedia:request-session request))
             "csrf-hm022")
       (clog-hypermedia:render-page
        component
        (clog-hypermedia:make-render-context
         :request request
         :application application
         :mode :page
         :locale "zh-CN"
         :primary-component-id
         (clog-hypermedia:component-id component)))))
    (setf application
          (clog-hypermedia:make-hypermedia-application
           :name "hm-022-page"
           :router router
           :configuration
           (make-test-configuration
            :assets-mode :none
            :strict-csp-p nil
            :static-prefix nil
            :static-root nil)))
    (let* ((response
             (funcall
              (clog-hypermedia:application-handler application)
              (make-request-env :method :get :path "/hm-022")))
           (body (response-body-text response)))
      (is (= 200 (first response)))
      (is (search "<!doctype html>" body :test #'char-equal))
      (is (search "<title>HM-022 Component</title>" body))
      (is (search
           "<section class=\"hm-022\" title=\"页面内容\"><p>页面内容</p></section>"
           body))
      (is (search
           "<meta name=\"csrf-token\" content=\"csrf-hm022\">"
           body)))))

(test render/protocol/base-missing-method-is-contextualized
  (let* ((component
           (make-instance
            'clog-hypermedia:component
            :scope :application
            :id "clog-c-00000000000000000000000000002208"))
         (context
           (clog-hypermedia:make-render-context
            :request (hm-022-request)
            :application (hm-022-application)
            :mode :fragment)))
    (clog-hypermedia:mount-component component)
    (handler-case
        (progn
          (clog-hypermedia:render component context)
          (fail "Missing render method should have been contextualized."))
      (clog-hypermedia:rendering-error (condition)
        (is (eq :component-render-failed
                (clog-hypermedia:rendering-error-reason condition)))
        (is (typep (clog-hypermedia:rendering-error-cause condition)
                   'clog-hypermedia:render-method-missing))))))
