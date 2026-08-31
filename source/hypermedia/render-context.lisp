;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime immutable render context                     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-render
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:export #:rendering-error
           #:rendering-error-reason
           #:rendering-error-component-id
           #:rendering-error-request-id
           #:rendering-error-cause
           #:invalid-render-context
           #:invalid-render-result
           #:render-purity-violation
           #:render-purity-violation-kind
           #:render-context
           #:make-render-context
           #:render-context-request
           #:render-context-application
           #:render-context-mode
           #:render-context-target
           #:render-context-locale
           #:render-context-assets
           #:render-context-csp-nonce
           #:render-context-primary-component-id
           #:trusted-html
           #:trusted-html-p
           #:make-trusted-html
           #:trusted-html-string
           #:current-render-context
           #:current-render-component
           #:current-render-request
           #:current-render-mode
           #:current-render-locale
           #:current-render-csp-nonce))

(defpackage #:clog-hypermedia
  (:import-from #:clog-render
                #:rendering-error
                #:rendering-error-reason
                #:rendering-error-component-id
                #:rendering-error-request-id
                #:rendering-error-cause
                #:invalid-render-context
                #:invalid-render-result
                #:render-purity-violation
                #:render-purity-violation-kind
                #:render-context
                #:make-render-context
                #:render-context-request
                #:render-context-application
                #:render-context-mode
                #:render-context-target
                #:render-context-locale
                #:render-context-assets
                #:render-context-csp-nonce
                #:render-context-primary-component-id
                #:trusted-html
                #:trusted-html-p
                #:make-trusted-html
                #:trusted-html-string
                #:current-render-context
                #:current-render-component
                #:current-render-request
                #:current-render-mode
                #:current-render-locale
                #:current-render-csp-nonce)
  (:export #:rendering-error
           #:rendering-error-reason
           #:rendering-error-component-id
           #:rendering-error-request-id
           #:rendering-error-cause
           #:invalid-render-context
           #:invalid-render-result
           #:render-purity-violation
           #:render-purity-violation-kind
           #:render-context
           #:make-render-context
           #:render-context-request
           #:render-context-application
           #:render-context-mode
           #:render-context-target
           #:render-context-locale
           #:render-context-assets
           #:render-context-csp-nonce
           #:render-context-primary-component-id
           #:trusted-html
           #:trusted-html-p
           #:make-trusted-html
           #:trusted-html-string
           #:current-render-context
           #:current-render-component
           #:current-render-request
           #:current-render-mode
           #:current-render-locale
           #:current-render-csp-nonce))

(in-package #:clog-render)

(defparameter +render-modes+
  '(:page :fragment :partial :sse :websocket :test)
  "Closed render-mode vocabulary used by immutable render contexts.")

(defconstant +maximum-render-string-characters+ 16777216
  "Fail-closed ceiling for one rendered/trusted string before HTTP encoding.")

(defun render-scalar-character-p (character)
  "Return true when CHARACTER is a Unicode scalar accepted by the renderer."
  (let ((code (char-code character)))
    (and (<= code #x10ffff)
         (not (<= #xd800 code #xdfff)))))

(defun render-control-character-p (character)
  "Return true for forbidden controls while preserving normal HTML whitespace."
  (let ((code (char-code character)))
    (and (or (< code 32) (= code 127))
         (not (member character '(#\Tab #\Newline #\Return)
                      :test #'char=)))))

(defun safe-render-string-p (value &key allow-empty)
  "Return true for a bounded Unicode render string without forbidden controls."
  (and (stringp value)
       (<= (length value) +maximum-render-string-characters+)
       (or allow-empty (plusp (length value)))
       (every (lambda (character)
                (and (render-scalar-character-p character)
                     (not (render-control-character-p character))))
              value)))

(defun copy-render-value (value)
  "Defensively copy mutable context values while preserving opaque identities."
  (typecase value
    (string (copy-seq value))
    (cons (cons (copy-render-value (car value))
                (copy-render-value (cdr value))))
    (vector (copy-seq value))
    (t value)))

(defun valid-render-component-id-p (value)
  "Return true when VALUE follows the frozen CLOG component ID grammar."
  (and (stringp value)
       (= (length value) 39)
       (string= "clog-c-" value :end2 7)
       (loop for index from 7 below 39
             for character = (char value index)
             always (or (char<= #\0 character #\9)
                        (char<= #\a character #\f)))))

(defun render-request-id-from (request)
  "Return REQUEST's bounded request ID, or NIL when unavailable."
  (and (typep request 'clog-http:request-context)
       (clog-http:request-id request)))

(define-condition rendering-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader rendering-error-reason)
   (component-id
    :initarg :component-id
    :initform nil
    :reader %rendering-error-component-id)
   (request-id
    :initarg :request-id
    :initform nil
    :reader %rendering-error-request-id)
   (cause
    :initarg :cause
    :initform nil
    :reader rendering-error-cause))
  (:report
   (lambda (condition stream)
     (format stream "Component rendering failed~@[ (~A)~]."
             (rendering-error-reason condition))))
  (:documentation
   "Base renderer condition carrying bounded component/request correlation metadata.

The default report intentionally omits the underlying condition, rendered data,
request path, session identity and component state. RENDERING-ERROR-CAUSE is for
controlled development diagnostics only."))

(define-condition invalid-render-context (rendering-error)
  ()
  (:documentation "Signaled when immutable render-context construction fails."))

(define-condition invalid-render-result (rendering-error)
  ()
  (:documentation
   "Signaled when a component renderer returns neither an HTML string nor TRUSTED-HTML."))

(define-condition render-purity-violation (rendering-error)
  ((kind
    :initarg :kind
    :reader render-purity-violation-kind))
  (:report
   (lambda (condition stream)
     (format stream "Component renderer violated the pure-render contract (~A)."
             (render-purity-violation-kind condition))))
  (:documentation
   "Signaled when rendering changes the component revision or session registry."))

(defun rendering-error-component-id (condition)
  "Return a defensive copy of CONDITION's component ID, or NIL."
  (let ((value (%rendering-error-component-id condition)))
    (and value (copy-seq value))))

(defun rendering-error-request-id (condition)
  "Return a defensive copy of CONDITION's request ID, or NIL."
  (let ((value (%rendering-error-request-id condition)))
    (and value (copy-seq value))))

(defclass render-context ()
  ((request
    :initarg :request
    :reader %render-context-request)
   (application
    :initarg :application
    :reader %render-context-application)
   (mode
    :initarg :mode
    :reader %render-context-mode)
   (target
    :initarg :target
    :reader %render-context-target)
   (locale
    :initarg :locale
    :reader %render-context-locale)
   (assets
    :initarg :assets
    :reader %render-context-assets)
   (csp-nonce
    :initarg :csp-nonce
    :reader %render-context-csp-nonce)
   (primary-component-id
    :initarg :primary-component-id
    :reader %render-context-primary-component-id))
  (:documentation
   "Immutable request-scoped input to deterministic component rendering.

The context contains identities owned by their respective layers plus defensive
copies of mutable strings and asset lists. It exposes no SETF writers and starts
no thread. Renderers may read it but must not mutate component or registry state."))

(defun invalid-context (reason &key request component-id cause)
  "Signal INVALID-RENDER-CONTEXT with bounded correlation metadata."
  (error 'invalid-render-context
         :reason reason
         :component-id component-id
         :request-id (render-request-id-from request)
         :cause cause))

(defun validate-render-request (request mode)
  "Return REQUEST after validating the mode-specific request contract."
  (unless (or (null request) (typep request 'clog-http:request-context))
    (invalid-context :invalid-request-context :cause nil))
  (when (and (not (eq mode :test)) (null request))
    (invalid-context :request-context-required))
  request)

(defun validate-render-application (application mode request)
  "Return APPLICATION after validating the mode-specific application contract."
  (unless (or (null application)
              (typep application 'clog-hypermedia:hypermedia-application))
    (invalid-context :invalid-application :request request))
  (when (and (eq mode :page) (null application))
    (invalid-context :application-required-for-page :request request))
  application)

(defun validate-render-mode (mode)
  "Return MODE when it belongs to the closed render-mode vocabulary."
  (unless (member mode +render-modes+ :test #'eq)
    (invalid-context :invalid-render-mode))
  mode)

(defun validate-render-target (target request)
  "Return a defensive optional HTMX target string."
  (unless (or (null target)
              (and (safe-render-string-p target)
                   (<= (length target) 4096)))
    (invalid-context :invalid-render-target :request request))
  (and target (copy-seq target)))

(defun validate-render-locale (locale request)
  "Return a defensive locale designator accepted by the rendering boundary."
  (unless (or (keywordp locale)
              (and (safe-render-string-p locale)
                   (<= (length locale) 128)))
    (invalid-context :invalid-render-locale :request request))
  (if (stringp locale) (copy-seq locale) locale))

(defun validate-render-assets (assets request)
  "Return a fresh proper list of validated immutable asset descriptors."
  (unless (proper-asset-list-p assets)
    (invalid-context :malformed-render-assets :request request))
  (dolist (descriptor assets)
    (unless (asset-p descriptor)
      (invalid-context :invalid-render-asset :request request)))
  (copy-list assets))

(defun validate-render-nonce-value (nonce request)
  "Return a defensive optional CSP nonce."
  (unless (or (null nonce) (safe-asset-token-p nonce))
    (invalid-context :invalid-render-csp-nonce :request request))
  (and nonce (copy-seq nonce)))

(defun validate-primary-component-id (component-id request)
  "Return a defensive optional primary component ID."
  (unless (or (null component-id)
              (valid-render-component-id-p component-id))
    (invalid-context :invalid-primary-component-id :request request))
  (and component-id (copy-seq component-id)))

(defun make-render-context
    (&key request
          application
          (mode :fragment)
          (target nil target-supplied-p)
          (locale "zh-CN")
          (assets nil)
          (csp-nonce nil nonce-supplied-p)
          primary-component-id)
  "Create an immutable RENDER-CONTEXT.

REQUEST is required except in :TEST mode. APPLICATION is required for :PAGE.
TARGET and CSP-NONCE default to the normalized request metadata. Supplying a
nonce that differs from the request nonce signals INVALID-RENDER-CONTEXT, so
render code cannot mint an independent CSP capability. All mutable inputs are
copied. The function performs no component/store mutation and acquires no lock."
  (let* ((mode (validate-render-mode mode))
         (request (validate-render-request request mode))
         (application
           (validate-render-application application mode request))
         (request-target
           (and request (clog-http:htmx-request-target request)))
         (request-nonce
           (and request (clog-http:request-csp-nonce request)))
         (target
           (validate-render-target
            (if target-supplied-p target request-target)
            request))
         (nonce-source
           (if nonce-supplied-p csp-nonce request-nonce)))
    (when (and nonce-supplied-p request
               (not (equal csp-nonce request-nonce)))
      (invalid-context :csp-nonce-does-not-match-request :request request))
    (make-instance
     'render-context
     :request request
     :application application
     :mode mode
     :target target
     :locale (validate-render-locale locale request)
     :assets (validate-render-assets assets request)
     :csp-nonce (validate-render-nonce-value nonce-source request)
     :primary-component-id
     (validate-primary-component-id primary-component-id request))))

(defun render-context-request (context)
  "Return CONTEXT's immutable request-context identity, or NIL in isolated tests."
  (check-type context render-context)
  (%render-context-request context))

(defun render-context-application (context)
  "Return CONTEXT's Hypermedia application identity, or NIL."
  (check-type context render-context)
  (%render-context-application context))

(defun render-context-mode (context)
  "Return CONTEXT's closed rendering mode keyword."
  (check-type context render-context)
  (%render-context-mode context))

(defun render-context-target (context)
  "Return a defensive copy of CONTEXT's optional HTMX target."
  (check-type context render-context)
  (copy-render-value (%render-context-target context)))

(defun render-context-locale (context)
  "Return CONTEXT's locale, defensively copying string locales."
  (check-type context render-context)
  (copy-render-value (%render-context-locale context)))

(defun render-context-assets (context)
  "Return a fresh list containing CONTEXT's immutable asset descriptors."
  (check-type context render-context)
  (copy-list (%render-context-assets context)))

(defun render-context-csp-nonce (context)
  "Return a defensive copy of CONTEXT's request-owned CSP nonce, or NIL."
  (check-type context render-context)
  (copy-render-value (%render-context-csp-nonce context)))

(defun render-context-primary-component-id (context)
  "Return a defensive copy of CONTEXT's primary component ID, or NIL."
  (check-type context render-context)
  (copy-render-value (%render-context-primary-component-id context)))

(defstruct (trusted-html
             (:constructor %make-trusted-html (string))
             (:conc-name %trusted-html-)
             (:copier nil))
  "Explicit wrapper for HTML already rendered or audited by trusted server code.

This is a trust marker, not a sanitizer. User input, query/form values and
database text must remain ordinary strings so Spinneret escapes them."
  (string "" :type string :read-only t))

(defun make-trusted-html (string)
  "Copy STRING into an explicit TRUSTED-HTML value.

STRING may contain normal HTML whitespace but not forbidden control characters
or non-scalar Unicode code points. The constructor does not sanitize markup."
  (unless (safe-render-string-p string :allow-empty t)
    (error 'invalid-render-result
           :reason :invalid-trusted-html
           :component-id nil
           :request-id nil
           :cause nil))
  (%make-trusted-html (copy-seq string)))

(defun trusted-html-string (value)
  "Return a defensive copy of VALUE's trusted markup string."
  (check-type value trusted-html)
  (copy-seq (%trusted-html-string value)))

(defmethod spinneret:html ((value trusted-html))
  "Write trusted markup to Spinneret's current stream without escaping it."
  (write-string (%trusted-html-string value) spinneret:*html*)
  (values))

(defvar *current-render-context* nil)
(defvar *current-render-component* nil)
(defvar *current-render-request* nil)
(defvar *current-render-mode* nil)
(defvar *current-render-locale* nil)
(defvar *current-render-csp-nonce* nil)

(defmacro with-current-render-bindings ((component context) &body body)
  "Evaluate BODY with the current component/request render metadata bound."
  `(let ((*current-render-context* ,context)
         (*current-render-component* ,component)
         (*current-render-request* (render-context-request ,context))
         (*current-render-mode* (render-context-mode ,context))
         (*current-render-locale* (render-context-locale ,context))
         (*current-render-csp-nonce* (render-context-csp-nonce ,context)))
     ,@body))

(defun current-render-context ()
  "Return the dynamically active render context, or NIL outside rendering."
  *current-render-context*)

(defun current-render-component ()
  "Return the dynamically active component, or NIL outside component rendering."
  *current-render-component*)

(defun current-render-request ()
  "Return the dynamically active request context, or NIL."
  *current-render-request*)

(defun current-render-mode ()
  "Return the dynamically active render mode, or NIL."
  *current-render-mode*)

(defun current-render-locale ()
  "Return the dynamically active locale, defensively copying string values."
  (copy-render-value *current-render-locale*))

(defun current-render-csp-nonce ()
  "Return the dynamically active request CSP nonce, defensively copied."
  (copy-render-value *current-render-csp-nonce*))
