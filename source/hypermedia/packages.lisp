;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime package boundaries                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-http
  (:use #:cl)
  (:documentation
   "Internal Clack request and response adapter package for CLOG Hypermedia.

HM-002 establishes the package boundary only. No operational API is exported
until the HTTP kernel tasks provide its implementation."))

(defpackage #:clog-router
  (:use #:cl)
  (:documentation
   "Internal route compilation, dispatch and URL generation package.

This package owns routing behavior and must not acquire HTML rendering
responsibilities."))

(defpackage #:clog-component
  (:use #:cl)
  (:documentation
   "Internal component lifecycle and registry protocol package.

The component core remains independent of HTMX and browser transport details."))

(defpackage #:clog-render
  (:use #:cl)
  (:documentation
   "Internal render-context and HTML result package.

Rendering is intentionally separated from HTTP sockets and server lifecycle."))

(defpackage #:clog-htmx
  (:use #:cl)
  (:documentation
   "Internal HTMX metadata, attribute, partial and morph-policy package.

The package boundary does not contain application or domain logic."))

(defpackage #:clog-session
  (:use #:cl)
  (:documentation
   "Internal Lack session adapter and component-store boundary.

Session transport concerns remain separate from authorization business rules."))

(defpackage #:clog-hypermedia
  (:use #:cl)
  (:documentation
   "Public facade package for the experimental CLOG 3 Hypermedia Runtime.

HM-002 intentionally exports no symbols. Public names are introduced only by
the tasks that implement and test their complete contracts."))
