;;;; CLOG - Common Lisp Omnificent GUI
;;;; CLOG System Definition
;;;; Copyright (C) 2022 David Botton

(asdf:defsystem clog
  :name "CLOG"
  :version "2.1"
  :maintainer "David Botton <david@botton.com>"
  :author "David Botton <david@botton.com>"
  :licence "BSD"
  :description "Common Lisp Omnificent GUI"
  :long-description "CLOG - The Common Lisp Omnificent GUI is a complete cross-platform development environment for GUI / web applications."
  :depends-on (:clack
               :websocket-driver
               :hunchentoot
               :parse-float
               :trivial-mimes
               :trivial-rfc-1123
               :http-body
               :circular-streams
               :closer-mop
               :mgl-pax
               :cl-template
               :atomics
               :sqlite
               :cl-dbi
               :cl-pass
               :cl-isaac
               :spinneret
               :local-time)
  :components ((:file "source/clog-connection")
               (:file "source/clog-base" :depends-on ("source/clog-connection"))
               (:file "source/clog-element" :depends-on ("source/clog-base"))
               (:file "source/clog-html-element" :depends-on ("source/clog-element"))
               (:file "source/clog-form-element" :depends-on ("source/clog-html-element"))
               (:file "source/clog-document" :depends-on ("source/clog-element"))
               (:file "source/clog-window" :depends-on ("source/clog-document"))
               (:file "source/clog-body" :depends-on ("source/clog-document"))
               (:file "source/clog-connection-websockets" :depends-on ("source/clog-body"))
               (:file "source/clog-webgl" :depends-on ("source/clog-element"))
               (:file "source/clog-auth" :depends-on ("source/clog-body"))
               (:file "source/clog-web" :depends-on ("source/clog-body"))
               (:file "source/clog-web-dbi" :depends-on ("source/clog-web"))
               (:file "source/clog-gui" :depends-on ("source/clog-form-element"))
               (:file "source/clog-tools" :depends-on ("source/clog-gui"))))

(asdf:defsystem clog/hypermedia
  :name "CLOG Hypermedia Runtime"
  :version "0.1.0"
  :maintainer "David Botton <david@botton.com>"
  :author "CLOG contributors"
  :licence "BSD"
  :description "Secondary HTTP/HTMX runtime built beside the legacy CLOG WebSocket stack."
  :long-description "A fail-closed HTTP and HTMX runtime for CLOG 3. It shares static assets and public data contracts with legacy CLOG while owning an independent request lifecycle."
  :depends-on (:clog
               :lack-request
               :lack-response
               :lack-middleware-session
               :lack-middleware-csrf)
  :serial t
  :components ((:file "source/hypermedia/packages")
               (:file "source/hypermedia/conditions")
               (:file "source/hypermedia/request")
               (:file "source/hypermedia/response")
               (:file "source/hypermedia/router")))

(asdf:defsystem clog/hypermedia-tests
  :name "CLOG Hypermedia Runtime Tests"
  :version "0.1.0"
  :author "CLOG contributors"
  :licence "BSD"
  :description "Regression and package-boundary tests for the secondary hypermedia runtime."
  :depends-on (:clog/hypermedia
               :fiveam
               :plump)
  :serial t
  :components ((:file "tests/package-boundaries")
               (:module "tests/hypermedia"
                :serial t
                :components ((:file "request")
                             (:file "response")
                             (:file "router"))))
  :perform (asdf:test-op (operation component)
             (declare (ignore operation component))
             (unless (uiop:symbol-call :clog-hypermedia-tests :run-tests)
               (error "CLOG hypermedia test suite failed."))))
