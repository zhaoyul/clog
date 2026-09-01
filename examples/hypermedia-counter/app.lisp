;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Counter vertical MVP                                ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-counter-no-js)

(defun render-action-form (component context action label)
  "Render one progressively enhanced semantic action form.

The ordinary METHOD/ACTION contract remains sufficient when JavaScript is
unavailable. With the vendored HTMX runtime active the same form posts through
HTMX, targets the stable component root and uses the application's configured
swap policy, which is outerMorph for this example."
  (let* ((request (clog-hypermedia:render-context-request context))
         (application (clog-hypermedia:render-context-application context))
         (configuration
           (clog-hypermedia:application-configuration application))
         (url (action-url component action context))
         (target (format nil "#~A" (clog-hypermedia:component-dom-id component)))
         (csrf (clog-hypermedia:csrf-token-for request))
         (revision (clog-hypermedia:component-revision component)))
    (spinneret:with-html-string
      (:form :method "post"
       :action url
       :hx-post url
       :hx-target target
       :hx-swap (clog-hypermedia:configuration-default-swap configuration)
       (:input :type "hidden" :name "_csrf_token" :value csrf)
       (:input :type "hidden" :name "_clog_revision"
               :value (format nil "~D" revision))
       (:input :type "hidden" :name "_clog_return_to" :value "/counter")
       (:button :type "submit" label)))))

(defun make-counter-application ()
  "Create the HM-027 Counter application with offline vendored HTMX assets."
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
           :name "hypermedia-counter"
           :router router
           :component-store store
           :configuration
           (clog-hypermedia:make-hypermedia-configuration
            :assets-mode :vendored
            :strict-csp-p t)))
    application))

(defclass counter-server ()
  ((application
    :initarg :application
    :reader counter-server-application)
   (handler
    :initarg :handler
    :accessor %counter-server-handler)
   (host
    :initarg :host
    :reader counter-server-host)
   (port
    :initarg :port
    :reader counter-server-port)
   (running-p
    :initarg :running-p
    :initform t
    :reader counter-server-running-p))
  (:documentation
   "Explicit Clack server handle for the HM-027 Counter reference application."))

(defun validate-counter-host (host)
  "Return HOST after rejecting empty or control-character server addresses."
  (unless (and (stringp host)
               (plusp (length host))
               (every (lambda (character)
                        (let ((code (char-code character)))
                          (and (>= code 32) (/= code 127))))
                      host))
    (error 'type-error :datum host :expected-type 'string))
  (copy-seq host))

(defun resolve-counter-port (host port)
  "Return an explicit TCP PORT, choosing an ephemeral local port for NIL or 0."
  (cond
    ((or (null port) (eql port 0))
     (clog-connection:random-port :host host))
    ((and (integerp port) (<= 1 port 65535)) port)
    (t
     (error 'type-error
            :datum port
            :expected-type '(integer 0 65535)))))

(defun counter-server-url (server)
  "Return the local HTTP URL for SERVER's configured host and port."
  (check-type server counter-server)
  (let ((host (counter-server-host server)))
    (format nil "http://~A:~D"
            (if (find #\: host)
                (format nil "[~A]" host)
                host)
            (counter-server-port server))))

(defun start-counter (&key
                        (host "127.0.0.1")
                        (port 0)
                        (server :hunchentoot)
                        (application (make-counter-application)))
  "Start the HM-027 Counter and return an explicit COUNTER-SERVER handle.

PORT may be NIL or 0 to request a currently available local port. The returned
handle owns the Clack server object and must be passed to STOP-COUNTER. No global
Counter server variable is created, so tests and REPL sessions can own lifecycle
explicitly."
  (check-type application clog-hypermedia:hypermedia-application)
  (let* ((host (validate-counter-host host))
         (port (resolve-counter-port host port))
         (handler
           (clack:clackup
            (clog-hypermedia:as-clack-app application)
            :debug nil
            :server server
            :address host
            :port port)))
    (make-instance 'counter-server
                   :application application
                   :handler handler
                   :host host
                   :port port
                   :running-p t)))

(defun stop-counter (server)
  "Stop SERVER exactly once and return SERVER.

Repeated calls are idempotent. CLACK:STOP is the sole owner of the underlying
server shutdown; the example creates no additional worker thread or subscription."
  (check-type server counter-server)
  (when (counter-server-running-p server)
    (clack:stop (%counter-server-handler server))
    (setf (%counter-server-handler server) nil
          (slot-value server 'running-p) nil))
  server)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(counter-server
            counter-server-application
            counter-server-host
            counter-server-port
            counter-server-running-p
            counter-server-url
            start-counter
            stop-counter)
          '#:clog-hypermedia-counter-no-js))

(defpackage #:clog-hypermedia-counter
  (:use #:cl)
  (:import-from #:clog-hypermedia-counter-no-js
                #:counter-component
                #:counter-value
                #:make-counter-application
                #:counter-server
                #:counter-server-application
                #:counter-server-host
                #:counter-server-port
                #:counter-server-running-p
                #:counter-server-url
                #:start-counter
                #:stop-counter)
  (:export #:counter-component
           #:counter-value
           #:make-counter-application
           #:counter-server
           #:counter-server-application
           #:counter-server-host
           #:counter-server-port
           #:counter-server-running-p
           #:counter-server-url
           #:start-counter
           #:stop-counter))
