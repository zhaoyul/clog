;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HTMX multi-target partial tests             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +hm-032-component-a+
  "clog-c-00000000000000000000000000003201")
(defparameter +hm-032-component-b+
  "clog-c-00000000000000000000000000003202")
(defparameter +hm-032-component-c+
  "clog-c-00000000000000000000000000003203")

(defclass hm-032-partial-component (clog-hypermedia:component)
  ((text :initarg :text :reader hm-032-text)))

(defmethod clog-hypermedia:render-component
    ((component hm-032-partial-component) context)
  (spinneret:with-html-string
    (:section :attrs (clog-hypermedia:component-root-attributes
                      component context)
              (:span (hm-032-text component)))))

(defun hm-032-mounted-application-component (id text)
  (let ((component
          (make-instance 'hm-032-partial-component
                         :id id
                         :scope :application
                         :text text)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-032-store-session-component (store session-id id text)
  (let ((component
          (make-instance 'hm-032-partial-component
                         :id id
                         :owner-session-id session-id
                         :text text)))
    (clog-hypermedia:mount-component component)
    (clog-hypermedia:store-component store session-id component)
    component))

(defun hm-032-context (&key component-store (session-id "session-hm032"))
  (let* ((request (hm-022-request
                   :request-id "request-hm032"
                   :session-id session-id))
         (application
           (hm-022-application :component-store component-store)))
    (clog-hypermedia:make-render-context
     :request request
     :application application
     :mode :fragment)))

(defun hm-032-count-substring (needle haystack)
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(test partials/model/defaults-and-defensive-metadata
  (let* ((component
           (hm-032-mounted-application-component
            +hm-032-component-a+ "alpha"))
         (target (copy-seq "#machine-card"))
         (partial
           (clog-hypermedia:make-partial
            component :target target :swap "innerMorph")))
    (is (clog-hypermedia:partial-p partial))
    (is (eq component (clog-hypermedia:partial-component partial)))
    (is (string= +hm-032-component-a+
                 (clog-hypermedia:partial-component-id partial)))
    (is (= 0 (clog-hypermedia:partial-revision partial)))
    (is (string= "#machine-card"
                 (clog-hypermedia:partial-target partial)))
    (is (string= "innerMorph"
                 (clog-hypermedia:partial-swap partial)))
    (setf (char target 1) #\X)
    (is (string= "#machine-card"
                 (clog-hypermedia:partial-target partial)))
    (let ((returned-target (clog-hypermedia:partial-target partial)))
      (setf (char returned-target 1) #\Y)
      (is (string= "#machine-card"
                   (clog-hypermedia:partial-target partial))))
    (let ((default (clog-hypermedia:make-partial component)))
      (is (string= (format nil "#~A" +hm-032-component-a+)
                   (clog-hypermedia:partial-target default)))
      (is (string= "outerMorph"
                   (clog-hypermedia:partial-swap default))))))

(test partials/render/multi-target-pure-response-and-escaping
  (let* ((a (hm-032-mounted-application-component
             +hm-032-component-a+ "alpha <script>alert(1)</script>"))
         (b (hm-032-mounted-application-component
             +hm-032-component-b+ "beta & gamma"))
         (c (hm-032-mounted-application-component
             +hm-032-component-c+ "设备 λ"))
         (context (hm-032-context))
         (response
           (clog-hypermedia:render-partials
            (list a
                  (clog-hypermedia:make-partial
                   b :target "#secondary" :swap "innerMorph")
                  (clog-hypermedia:make-partial
                   c :target "#status" :swap "textContent"))
            context))
         (body (clog-hypermedia:response-body response)))
    (is (= 200 (clog-hypermedia:response-status response)))
    (is (eq :html (clog-hypermedia:response-kind response)))
    (is (= 3 (hm-032-count-substring "<hx-partial" body)))
    (is (= 3 (hm-032-count-substring "</hx-partial>" body)))
    (is (search (format nil "hx-target=\"#~A\"" +hm-032-component-a+) body))
    (is (search "hx-target=\"#secondary\"" body))
    (is (search "hx-swap=\"innerMorph\"" body))
    (is (search "hx-target=\"#status\"" body))
    (is (search "hx-swap=\"textContent\"" body))
    (is (search "&lt;script&gt;alert(1)&lt;/script&gt;" body))
    (is-false (search "<script>" body :test #'char-equal))
    (is (search "beta &amp; gamma" body))
    (is (search "设备 λ" body))
    (is-false (search "<html" body :test #'char-equal))
    (is-false (search "<head" body :test #'char-equal))
    (is-false (search "<body" body :test #'char-equal))))

(test partials/security/selector-and-swap-validation
  (let ((component
          (hm-032-mounted-application-component
           +hm-032-component-a+ "safe")))
    (dolist (target '("#safe\" onclick=\"alert(1)"
                      "#safe>script"
                      "#safe\nother"
                      "javascript:alert(1)"
                      ""))
      (signals error
        (clog-hypermedia:make-partial component :target target)))
    (dolist (swap '("outerHTML"
                    "beforeend"
                    "delete"
                    "outerMorph onclick=alert(1)"
                    ""))
      (signals error
        (clog-hypermedia:make-partial component :swap swap)))
    (dolist (swap '("outerMorph" "innerMorph" "textContent"))
      (is (string= swap
                   (clog-hypermedia:partial-swap
                    (clog-hypermedia:make-partial component :swap swap)))))))

(test partials/security/session-visibility-requires-current-exact-object
  (let* ((store (clog-hypermedia:make-memory-component-store))
         (session-a "session-hm032-a")
         (session-b "session-hm032-b")
         (component
           (hm-032-store-session-component
            store session-a +hm-032-component-a+ "private"))
         (allowed-context
           (hm-032-context :component-store store :session-id session-a))
         (other-context
           (hm-032-context :component-store store :session-id session-b)))
    (is (search "<hx-partial"
                (clog-hypermedia:render-partial
                 component allowed-context)))
    (signals error
      (clog-hypermedia:render-partial component other-context))
    ;; A forged object with the same opaque ID is not the registered capability.
    (let ((forged
            (make-instance 'hm-032-partial-component
                           :id +hm-032-component-a+
                           :owner-session-id session-a
                           :text "forged")))
      (clog-hypermedia:mount-component forged)
      (signals error
        (clog-hypermedia:render-partial forged allowed-context)))))

(test partials/reduction/latest-component-and-duplicate-target-are-deterministic
  (let* ((a (hm-032-mounted-application-component
             +hm-032-component-a+ "old"))
         (b (hm-032-mounted-application-component
             +hm-032-component-b+ "winner"))
         (context (hm-032-context))
         (early (clog-hypermedia:make-partial
                 a :target "#same" :swap "innerMorph")))
    (clog-hypermedia:touch-component a)
    (let* ((latest (clog-hypermedia:make-partial
                    a :target "#same" :swap "outerMorph"))
           (duplicate-target
             (clog-hypermedia:make-partial
              b :target "#same" :swap "textContent"))
           (same-component-response
             (clog-hypermedia:render-partials
              (list early latest) context))
           (same-component-body
             (clog-hypermedia:response-body same-component-response))
           (duplicate-target-response
             (clog-hypermedia:render-partials
              (list early duplicate-target) context))
           (duplicate-target-body
             (clog-hypermedia:response-body duplicate-target-response)))
      (is (= 1 (hm-032-count-substring "<hx-partial" same-component-body)))
      (is (search "data-clog-revision=\"1\"" same-component-body))
      (is (search "hx-swap=\"outerMorph\"" same-component-body))
      (is (= 1 (hm-032-count-substring "<hx-partial" duplicate-target-body)))
      (is (search +hm-032-component-b+ duplicate-target-body))
      (is (search "winner" duplicate-target-body))
      (is-false (search +hm-032-component-a+ duplicate-target-body)))))

(test partials/response/empty-set-is-explicit-no-op
  (let ((response
          (clog-hypermedia:render-partials nil (hm-032-context))))
    (is (= 204 (clog-hypermedia:response-status response)))
    (is (eq :empty (clog-hypermedia:response-kind response)))
    (is (null (clog-hypermedia:response-body response)))))
