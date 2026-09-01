;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HTMX attribute DSL/helper tests             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +hm-031-component-id+
  "clog-c-00000000000000000000000000003101")

(clog-hypermedia:defaction
    (hm-024-action-component :hm-031-save
     :external-name "save"
     :allowed-methods '(:post)
     :documentation "HM-031 descriptor-name fixture.")
    (component payload)
  (declare (ignore component payload))
  nil)

(defun hm-031-component ()
  "Return a mounted deterministic component for action-attribute snapshots."
  (let ((component
          (make-instance 'hm-024-action-component
                         :id +hm-031-component-id+
                         :scope :application)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-031-context (&key (nonce "nonce-hm031"))
  "Return a strict-CSP render context with a custom action prefix."
  (let* ((request (hm-022-request :request-id "request-hm031" :nonce nonce))
         (configuration
           (make-test-configuration
            :assets-mode :none
            :static-prefix nil
            :static-root nil
            :action-prefix "/hm031/action"
            :default-swap "outerMorph"
            :strict-csp-p t))
         (application
           (clog-hypermedia:make-hypermedia-application
            :name "hm-031"
            :configuration configuration)))
    (clog-hypermedia:make-render-context
     :request request
     :application application
     :mode :fragment
     :primary-component-id +hm-031-component-id+)))

(test htmx-attrs/action-snapshot-and-segment-url-encoding
  (let ((attrs
          (clog-hypermedia:action-attrs
           "/_clog/action/"
           "component id/with slash"
           "save/now"
           :target "#machine-card"
           :swap "outerMorph"
           :nonce "nonce-031")))
    (is (equal
         '(:action
           "/_clog/action/component%20id%2Fwith%20slash/save%2Fnow"
           :method "post"
           :hx-post
           "/_clog/action/component%20id%2Fwith%20slash/save%2Fnow"
           :hx-target "#machine-card"
           :hx-swap "outerMorph"
           :hx-nonce "nonce-031")
         attrs))
    ;; The returned plist is directly consumable by Spinneret :attrs.
    (let ((html
            (spinneret:with-html-string
              (:form :attrs attrs
                     (:button :type "submit" "Save")))))
      (is (search "action=\"/_clog/action/component%20id%2Fwith%20slash/save%2Fnow\""
                  html))
      (is (search "hx-post=\"/_clog/action/component%20id%2Fwith%20slash/save%2Fnow\""
                  html))
      (is (search "hx-swap=\"outerMorph\"" html)))))

(test htmx-attrs/hx-vals-is-json-data-never-raw-javascript
  (let* ((message (format nil "quote ~S <tag>~Cnext" "value" #\Newline))
         (attrs
           (clog-hypermedia:hx-attrs
            :post "/safe/action"
            :target "#result"
            :swap "outerMorph"
            :vals `(("message" . ,message)
                    ("count" . 2))
            :nonce "nonce-json"))
         (json (getf attrs :hx-vals))
         (parsed (yason:parse json :object-as :alist)))
    (is (string= "/safe/action" (getf attrs :hx-post)))
    (is (string= message (cdr (assoc "message" parsed :test #'string=))))
    (is (= 2 (cdr (assoc "count" parsed :test #'string=))))
    (is-false (or (and (>= (length json) 3)
                       (string-equal "js:" json :end2 3))
                  (and (>= (length json) 11)
                       (string-equal "javascript:" json :end2 11))))
    ;; Raw expression strings are not a second, hidden API surface.
    (signals error
      (clog-hypermedia:hx-attrs
       :post "/safe/action"
       :vals "js:{dangerous: window.location}"))
    (signals error
      (clog-hypermedia:hx-attrs
       :post "/safe/action"
       :vals "javascript:{dangerous: true}"))))

(test htmx-attrs/merge-policy-is-deterministic-and-fails-closed
  (is (equal
       '(:class "alpha beta gamma"
         :data-kind "machine"
         :aria-label "Save")
       (clog-hypermedia:merge-html-attrs
        '(:class "alpha beta" :data-kind "machine")
        '(:class "beta gamma" :aria-label "Save"))))
  ;; All non-token duplicates are rejected rather than silently selecting one.
  (signals error
    (clog-hypermedia:merge-html-attrs
     '(:hx-post "/first")
     '(:hx-post "/second")))
  (signals error
    (clog-hypermedia:merge-html-attrs
     '(:action "/first")
     '(:action "/second")))
  (signals error
    (clog-hypermedia:merge-html-attrs
     '(:id "first")
     '(:id "second")))
  ;; Generic merging must not re-introduce the forbidden raw-JavaScript hx-vals form.
  (signals error
    (clog-hypermedia:merge-html-attrs
     '(:class "safe")
     '(:hx-vals "js:{dangerous: true}"))))

(test htmx-attrs/component-action-attributes-use-descriptor-config-and-csp-context
  (let* ((component (hm-031-component))
         (context (hm-031-context))
         (attrs
           (clog-hypermedia:component-action-attributes
            component :hm-031-save context)))
    ;; The URL uses the descriptor's explicit external name "save", not the
    ;; Lisp symbol name HM-031-SAVE.
    (is (equal
         `(:action ,(format nil "/hm031/action/~A/save" +hm-031-component-id+)
           :method "post"
           :hx-post ,(format nil "/hm031/action/~A/save" +hm-031-component-id+)
           :hx-target ,(format nil "#~A" +hm-031-component-id+)
           :hx-swap "outerMorph"
           :hx-nonce "nonce-hm031")
         attrs))
    (is (equal
         `(:action ,(format nil "/hm031/action/~A/save" +hm-031-component-id+)
           :method "post"
           :hx-post ,(format nil "/hm031/action/~A/save" +hm-031-component-id+)
           :hx-target "#custom-target"
           :hx-swap "innerMorph"
           :hx-nonce "nonce-hm031")
         (clog-hypermedia:component-action-attributes
          component :hm-031-save context
          :target "#custom-target"
          :swap "innerMorph")))
    ;; Unknown Lisp action symbols fail before emitting a guessed URL.
    (signals error
      (clog-hypermedia:component-action-attributes
       component :hm-031-missing context))))
