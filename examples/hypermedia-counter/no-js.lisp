;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Counter no-JavaScript progressive slice              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-hypermedia-counter-no-js
  (:use #:cl)
  (:export #:make-counter-application
           #:counter-component
           #:counter-value))

(in-package #:clog-hypermedia-counter-no-js)

(defclass counter-component (clog-hypermedia:component)
  ((value :initform 0 :accessor counter-value)))

(defun action-url (component action context)
  "Return the local action URL for COMPONENT and ACTION in render CONTEXT."
  (let* ((application (clog-hypermedia:render-context-application context))
         (prefix
           (clog-hypermedia:configuration-action-prefix
            (clog-hypermedia:application-configuration application))))
    (format nil "~A/~A/~A"
            (string-right-trim "/" prefix)
            (clog-hypermedia:component-id component)
            action)))

(defun render-action-form (component context action label)
  "Render one semantic POST form usable with JavaScript disabled."
  (let* ((request (clog-hypermedia:render-context-request context))
         (csrf (clog-hypermedia:csrf-token-for request))
         (revision (clog-hypermedia:component-revision component)))
    (spinneret:with-html-string
      (:form :method "post" :action (action-url component action context)
       (:input :type "hidden" :name "_csrf_token" :value csrf)
       (:input :type "hidden" :name "_clog_revision"
               :value (format nil "~D" revision))
       (:input :type "hidden" :name "_clog_return_to" :value "/counter")
       (:button :type "submit" label)))))

(defmethod clog-hypermedia:render-component
    ((component counter-component) context)
  (let ((increment (render-action-form component context "increment" "+1"))
        (decrement (render-action-form component context "decrement" "-1"))
        (reset (render-action-form component context "reset" "Reset")))
    (spinneret:with-html-string
      (:section
       :attrs (clog-hypermedia:component-root-attributes
               component context :class "counter")
       (:h1 "Counter")
       (:p :id "counter-value" (format nil "~D" (counter-value component)))
       (spinneret:html (clog-hypermedia:make-trusted-html increment))
       (spinneret:html (clog-hypermedia:make-trusted-html decrement))
       (spinneret:html (clog-hypermedia:make-trusted-html reset))))))

(defmethod clog-hypermedia:component-title
    ((component counter-component) context)
  (declare (ignore component context))
  "Hypermedia Counter")

(clog-hypermedia:defaction
    (counter-component :increment
     :external-name "increment"
     :requires-current t)
    (component context)
  (declare (ignore context))
  (incf (counter-value component))
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (counter-component :decrement
     :external-name "decrement"
     :requires-current t)
    (component context)
  (declare (ignore context))
  (decf (counter-value component))
  (clog-action::make-action-result))

(clog-hypermedia:defaction
    (counter-component :reset
     :external-name "reset"
     :requires-current t)
    (component context)
  (declare (ignore context))
  (setf (counter-value component) 0)
  (clog-action::make-action-result))

(defun find-or-create-counter (store session-id components)
  "Return the mounted counter for SESSION-ID, replacing stale local handles."
  (let ((component (gethash session-id components)))
    (unless (and component (clog-hypermedia:mounted-p component))
      (setf component
            (make-instance 'counter-component
                           :scope :session
                           :owner-session-id session-id))
      (clog-hypermedia:mount-component component)
      (clog-hypermedia:store-component store session-id component)
      (setf (gethash (copy-seq session-id) components) component))
    component))

(defun render-counter-page (application component context)
  "Render a complete page and consume one-shot progressive metadata."
  (let* ((flash (clog-htmx::consume-no-js-flash context))
         (validation (clog-htmx::consume-no-js-validation context))
         (fragment-state
           (clog-hypermedia:make-render-context
            :request context
            :application application
            :mode :fragment
            :primary-component-id (clog-hypermedia:component-id component)))
         (fragment (clog-hypermedia:render component fragment-state))
         (root
           (spinneret:with-html-string
             (:main
              (when flash (:p :id "flash-message" flash))
              (when validation (:p :id "validation-message" validation))
              (spinneret:html (clog-hypermedia:make-trusted-html fragment))))))
    (let ((response
            (clog-hypermedia:render-page
             application context root
             :title "Hypermedia Counter"
             :language "en")))
      ;; Clack/Hunchentoot consumes normal response bodies as chunk lists.
      ;; Keep HM-011's response abstraction unchanged and adapt this browser
      ;; example at the edge so the HM-026 E2E exercises a real server.
      (clog-hypermedia:html-response
       (list (clog-hypermedia:response-body response))
       :status (clog-hypermedia:response-status response)
       :headers (clog-hypermedia:response-headers response)))))

(defun make-counter-application ()
  "Create the focused HM-026 no-JavaScript Counter application."
  (let* ((router (clog-hypermedia:make-router))
         (store (clog-hypermedia:make-memory-component-store))
         (components (make-hash-table :test #'equal))
         (application nil))
    (clog-hypermedia:add-route
     router :get "/"
     (lambda (context)
       (declare (ignore context))
       (clog-hypermedia:redirect-response "/counter" :status 303)))
    (clog-hypermedia:add-route
     router :get "/counter"
     (lambda (context)
       (multiple-value-bind (registry session-id)
           (clog-hypermedia:ensure-session-component-registry store context)
         (declare (ignore registry))
         (let ((component
                 (find-or-create-counter store session-id components)))
           (render-counter-page application component context)))))
    (setf application
          (clog-hypermedia:make-hypermedia-application
           :name "counter-no-js"
           :router router
           :component-store store
           :configuration
           (clog-hypermedia:make-hypermedia-configuration
            :assets-mode :none
            :strict-csp-p nil
            :static-prefix nil
            :static-root nil)))
    application))
