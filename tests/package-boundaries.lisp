;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime package-boundary tests                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +secondary-system-contracts+
  '(("clog/hypermedia"
     ("clog"
      "spinneret"
      "yason"
      "lack-middleware-session"
      "lack-middleware-csrf"
      "lack-util"
      "lack-middleware-static")
     ("packages"
      "conditions"
      "component"
      "protocol"
      "request"
      "response"
      "router"
      "configuration"
      "component-store"
      "session"
      "assets"
      "application"
      "render")
     ("CLOG-HTTP"
      "CLOG-ROUTER"
      "CLOG-COMPONENT"
      "CLOG-RENDER"
      "CLOG-HTMX"
      "CLOG-SESSION"
      "CLOG-HYPERMEDIA"))
    ("clog/live"
     ("clog/hypermedia" "websocket-driver")
     ("packages")
     ("CLOG-SSE" "CLOG-WS" "CLOG-EFFECT" "CLOG-LIVE"))
    ("clog/presentations2"
     ("clog/hypermedia")
     ("packages")
     ("CLOG-PRESENTATIONS2"))
    ("clog/compat"
     ("clog/hypermedia" "clog/live")
     ("packages")
     ("CLOG-COMPAT")))
  "Current secondary ASDF system, dependency, component and package contracts.")

(defparameter +http-api-export-snapshot+
  '("CLOG-HYPERMEDIA-ERROR"
    "FORM-PARAM"
    "FORM-PARAM-VALUES"
    "HTML-RESPONSE"
    "HTMX-FULL-REQUEST-P"
    "HTMX-PARTIAL-REQUEST-P"
    "HTMX-REQUEST-P"
    "HTMX-REQUEST-SOURCE"
    "HTMX-REQUEST-TARGET"
    "HTMX-REQUEST-TRIGGER"
    "HTMX-REQUEST-TYPE"
    "INVALID-REDIRECT-URL"
    "INVALID-REDIRECT-URL-REASON"
    "INVALID-REDIRECT-URL-VALUE"
    "INVALID-RESPONSE-BODY"
    "INVALID-RESPONSE-BODY-VALUE"
    "INVALID-RESPONSE-HEADER"
    "INVALID-RESPONSE-HEADER-NAME"
    "INVALID-RESPONSE-HEADER-VALUE"
    "INVALID-RESPONSE-KIND"
    "INVALID-RESPONSE-KIND-VALUE"
    "INVALID-RESPONSE-STATUS"
    "INVALID-RESPONSE-STATUS-VALUE"
    "MAKE-REQUEST-CONTEXT"
    "MAKE-RESPONSE"
    "NO-CONTENT-RESPONSE"
    "NORMALIZE-RESPONSE"
    "PATH-PARAM"
    "QUERY-PARAM"
    "QUERY-PARAM-VALUES"
    "REDIRECT-RESPONSE"
    "REQUEST-BODY-PARSE-CAUSE"
    "REQUEST-BODY-PARSE-CONTENT-TYPE"
    "REQUEST-BODY-PARSE-ERROR"
    "REQUEST-BODY-TOO-LARGE"
    "REQUEST-BODY-TOO-LARGE-LENGTH"
    "REQUEST-BODY-TOO-LARGE-LIMIT"
    "REQUEST-CONTEXT"
    "REQUEST-CSP-NONCE"
    "REQUEST-CURRENT-URL"
    "REQUEST-ENV"
    "REQUEST-ERROR"
    "REQUEST-ERROR-REASON"
    "REQUEST-HEADER"
    "REQUEST-ID"
    "REQUEST-METHOD"
    "REQUEST-OBJECT"
    "REQUEST-PATH"
    "REQUEST-PATH-PARAMS"
    "REQUEST-ROUTE"
    "REQUEST-SESSION"
    "REQUEST-SESSION-ID"
    "REQUEST-USER"
    "RESPONSE"
    "RESPONSE->CLACK-RESPONSE"
    "RESPONSE-BODY"
    "RESPONSE-CONTENT-LENGTH-ACTUAL"
    "RESPONSE-CONTENT-LENGTH-EXPECTED"
    "RESPONSE-CONTENT-LENGTH-MISMATCH"
    "RESPONSE-ERROR"
    "RESPONSE-ERROR-REASON"
    "RESPONSE-HEADER"
    "RESPONSE-HEADER-VALUES"
    "RESPONSE-HEADERS"
    "RESPONSE-KIND"
    "RESPONSE-STATUS")
  "Exact internal HTTP adapter surface after HM-010 and HM-011.")

(defparameter +router-api-export-snapshot+
  '("ADD-ROUTE"
    "DISPATCH-ROUTE"
    "FIND-ROUTE"
    "MAKE-ROUTER"
    "METHOD-NOT-ALLOWED"
    "METHOD-NOT-ALLOWED-ALLOWED-METHODS"
    "METHOD-NOT-ALLOWED-METHOD"
    "METHOD-NOT-ALLOWED-PATH"
    "PATH-DECODING-ERROR"
    "PATH-DECODING-ERROR-CAUSE"
    "PATH-DECODING-ERROR-PARAMETER"
    "PATH-DECODING-ERROR-PATH"
    "ROUTE"
    "ROUTE-CONFLICT"
    "ROUTE-CONFLICT-EXISTING-TEMPLATE"
    "ROUTE-CONFLICT-METHOD"
    "ROUTE-CONFLICT-TEMPLATE"
    "ROUTE-DEFINITION-ERROR"
    "ROUTE-DEFINITION-VALUE"
    "ROUTE-EXACT-P"
    "ROUTE-HANDLER"
    "ROUTE-HANDLER-ERROR"
    "ROUTE-HANDLER-ERROR-CAUSE"
    "ROUTE-HANDLER-ERROR-ROUTE"
    "ROUTE-METADATA"
    "ROUTE-METHOD"
    "ROUTE-MIDDLEWARE"
    "ROUTE-NAME"
    "ROUTE-NOT-FOUND"
    "ROUTE-NOT-FOUND-METHOD"
    "ROUTE-NOT-FOUND-PATH"
    "ROUTE-P"
    "ROUTE-PARAMETER-NAMES"
    "ROUTE-PATH"
    "ROUTER"
    "ROUTER-P"
    "ROUTING-ERROR"
    "ROUTING-ERROR-REASON")
  "Exact internal deterministic router surface after HM-012.")

(defparameter +application-api-export-snapshot+
  '("APPLICATION-COMPONENT-STORE"
    "APPLICATION-CONFIGURATION"
    "APPLICATION-EVENT-BUS"
    "APPLICATION-HANDLER"
    "APPLICATION-LAYOUT"
    "APPLICATION-NAME"
    "APPLICATION-ROUTER"
    "AS-CLACK-APP"
    "CONFIGURATION-ACCESS-LOG-HOOK"
    "CONFIGURATION-ACTION-PREFIX"
    "CONFIGURATION-ASSETS-MODE"
    "CONFIGURATION-AUTHENTICATION-HOOK"
    "CONFIGURATION-COMPONENT-TTL-SECONDS"
    "CONFIGURATION-CSP-NONCE-GENERATOR"
    "CONFIGURATION-CSRF-FORM-TOKEN"
    "CONFIGURATION-CSRF-ONE-TIME-P"
    "CONFIGURATION-CSRF-SESSION-KEY"
    "CONFIGURATION-DEFAULT-SWAP"
    "CONFIGURATION-DEVELOPMENT-CONDITION-HOOK"
    "CONFIGURATION-DEVELOPMENT-P"
    "CONFIGURATION-HTMX-VERSION"
    "CONFIGURATION-MAX-COMPONENTS-PER-SESSION"
    "CONFIGURATION-MAX-EFFECT-HEADER-BYTES"
    "CONFIGURATION-MAX-PARTIALS-PER-RESPONSE"
    "CONFIGURATION-REQUEST-BODY-LIMIT-BYTES"
    "CONFIGURATION-REQUEST-ID-GENERATOR"
    "CONFIGURATION-SESSION-KEEP-EMPTY-P"
    "CONFIGURATION-SESSION-STATE"
    "CONFIGURATION-SESSION-STORE"
    "CONFIGURATION-SSE-PREFIX"
    "CONFIGURATION-STATIC-PREFIX"
    "CONFIGURATION-STATIC-ROOT"
    "CONFIGURATION-STRICT-CSP-P"
    "CONFIGURATION-WS-PREFIX"
    "CSRF-TOKEN-FOR"
    "HYPERMEDIA-APPLICATION"
    "HYPERMEDIA-APPLICATION-ERROR"
    "HYPERMEDIA-APPLICATION-ERROR-REASON"
    "HYPERMEDIA-CONFIGURATION"
    "HYPERMEDIA-CONFIGURATION-ERROR"
    "HYPERMEDIA-CONFIGURATION-ERROR-REASON"
    "MAKE-HYPERMEDIA-APP"
    "MAKE-HYPERMEDIA-APPLICATION"
    "MAKE-HYPERMEDIA-CONFIGURATION")
  "Exact public application and middleware surface after HM-013.")

(defparameter +rendering-api-export-snapshot+
  '("ASSET"
    "ASSET-DEFER-P"
    "ASSET-ERROR"
    "ASSET-ERROR-REASON"
    "ASSET-INTEGRITY"
    "ASSET-KEY"
    "ASSET-MODULE-P"
    "ASSET-NONCE-REQUIRED-P"
    "ASSET-P"
    "ASSET-TYPE"
    "ASSET-URL"
    "MAKE-ASSET"
    "RENDER-ASSETS"
    "RENDER-PAGE")
  "Exact public deterministic asset and page-shell surface after HM-014.")

(defparameter +component-public-api-export-snapshot+
  '("AFTER-MOUNT"
    "AUTHORIZE-ACTION-P"
    "BEFORE-UNMOUNT"
    "COMPONENT"
    "COMPONENT-ASSETS"
    "COMPONENT-ERROR"
    "COMPONENT-ERROR-REASON"
    "COMPONENT-ID"
    "COMPONENT-LIFECYCLE-ERROR"
    "COMPONENT-LIFECYCLE-ERROR-COMPONENT-ID"
    "COMPONENT-LIFECYCLE-ERROR-OPERATION"
    "COMPONENT-LIFECYCLE-ERROR-REASON"
    "COMPONENT-LIFECYCLE-ERROR-STATE"
    "COMPONENT-LIFECYCLE-STATE"
    "COMPONENT-NOT-MOUNTED"
    "COMPONENT-REVISION"
    "COMPONENT-TITLE"
    "HANDLE-ACTION"
    "INVALID-COMPONENT-DEFINITION"
    "INVALID-COMPONENT-DEFINITION-REASON"
    "MOUNT-COMPONENT"
    "MOUNTED-P"
    "RENDER-COMPONENT"
    "RENDER-METHOD-MISSING"
    "RENDER-METHOD-MISSING-CLASS"
    "RENDER-METHOD-MISSING-COMPONENT-ID"
    "TOUCH-COMPONENT"
    "UNMOUNT-COMPONENT"
    "VALIDATE-ACTION")
  "Exact public component lifecycle and protocol surface after HM-020.")

(defparameter +component-internal-core-export-snapshot+
  '("AFTER-MOUNT"
    "AUTHORIZE-ACTION-P"
    "BEFORE-UNMOUNT"
    "COMPONENT"
    "COMPONENT-ASSETS"
    "COMPONENT-DIRTY-P"
    "COMPONENT-ERROR"
    "COMPONENT-ERROR-REASON"
    "COMPONENT-ID"
    "COMPONENT-KEY"
    "COMPONENT-LAST-ACCESS"
    "COMPONENT-LIFECYCLE-ERROR"
    "COMPONENT-LIFECYCLE-ERROR-COMPONENT-ID"
    "COMPONENT-LIFECYCLE-ERROR-OPERATION"
    "COMPONENT-LIFECYCLE-ERROR-REASON"
    "COMPONENT-LIFECYCLE-ERROR-STATE"
    "COMPONENT-LIFECYCLE-STATE"
    "COMPONENT-LOCK"
    "COMPONENT-NOT-MOUNTED"
    "COMPONENT-OWNER-SESSION-ID"
    "COMPONENT-PARENT-ID"
    "COMPONENT-REVISION"
    "COMPONENT-SCOPE"
    "COMPONENT-TITLE"
    "HANDLE-ACTION"
    "INVALID-COMPONENT-DEFINITION"
    "INVALID-COMPONENT-DEFINITION-REASON"
    "MOUNT-COMPONENT"
    "MOUNTED-P"
    "RENDER-COMPONENT"
    "RENDER-METHOD-MISSING"
    "RENDER-METHOD-MISSING-CLASS"
    "RENDER-METHOD-MISSING-COMPONENT-ID"
    "TOUCH-COMPONENT"
    "UNMOUNT-COMPONENT"
    "VALIDATE-ACTION")
  "Exact internal component lifecycle surface after HM-020.")

(defparameter +component-store-api-export-snapshot+
  '("COMPONENT-NOT-FOUND"
    "COMPONENT-STORE"
    "COMPONENT-STORE-COMPONENT-TTL-SECONDS"
    "COMPONENT-STORE-CONFLICT"
    "COMPONENT-STORE-ERROR"
    "COMPONENT-STORE-ERROR-REASON"
    "COMPONENT-STORE-LIMIT-EXCEEDED"
    "COMPONENT-STORE-LIMIT-EXCEEDED-LIMIT"
    "COMPONENT-STORE-MAX-COMPONENTS-PER-SESSION"
    "COMPONENT-STORE-OPERATION-UNSUPPORTED"
    "COMPONENT-STORE-OWNERSHIP-ERROR"
    "COMPONENT-STORE-P"
    "COMPONENT-STORE-STATS"
    "DELETE-COMPONENT"
    "DELETE-SESSION-COMPONENTS"
    "ENSURE-COMPONENT-REGISTRY"
    "ENUMERATE-COMPONENTS"
    "FIND-COMPONENT"
    "LOAD-COMPONENT"
    "MAKE-MEMORY-COMPONENT-STORE"
    "MEMORY-COMPONENT-STORE"
    "REGISTER-COMPONENT"
    "REMOVE-COMPONENT"
    "STORE-COMPONENT"
    "SWEEP-COMPONENT-STORE"
    "SWEEP-COMPONENTS"
    "TOUCH-STORED-COMPONENT")
  "Exact session-scoped component-store surface after HM-021.")

(defparameter +session-integration-public-api-export-snapshot+
  '("COMPONENT-STORE-SESSION-KEY"
    "ENSURE-SESSION-COMPONENT-REGISTRY"
    "ROTATE-SESSION-COMPONENT-REGISTRY")
  "Public Lack-session integration helpers added by HM-021.")

(defparameter +component-internal-api-export-snapshot+
  (sort (append (copy-list +component-internal-core-export-snapshot+)
                (copy-list +component-store-api-export-snapshot+))
        #'string<)
  "Exact internal component core and store surface after HM-021.")

(defparameter +hypermedia-api-export-snapshot+
  (sort (append (copy-list +http-api-export-snapshot+)
                (copy-list +router-api-export-snapshot+)
                (copy-list +application-api-export-snapshot+)
                (copy-list +rendering-api-export-snapshot+)
                (copy-list +component-public-api-export-snapshot+)
                (copy-list +component-store-api-export-snapshot+)
                (copy-list +session-integration-public-api-export-snapshot+))
        #'string<)
  "Exact combined public Hypermedia facade surface after HM-021.")

(defparameter +session-api-export-snapshot+
  '("COMPONENT-STORE-SESSION-KEY"
    "CSRF-TOKEN-FOR"
    "ENSURE-SESSION-COMPONENT-REGISTRY"
    "MAKE-CSRF-MIDDLEWARE"
    "MAKE-REQUEST-BODY-LIMIT-MIDDLEWARE"
    "MAKE-SESSION-MIDDLEWARE"
    "ROTATE-SESSION-COMPONENT-REGISTRY")
  "Exact internal Lack adapter surface after HM-021.")

(defparameter +render-api-export-snapshot+
  '("ASSET"
    "ASSET-DEFER-P"
    "ASSET-ERROR"
    "ASSET-ERROR-REASON"
    "ASSET-INTEGRITY"
    "ASSET-KEY"
    "ASSET-MODULE-P"
    "ASSET-NONCE-REQUIRED-P"
    "ASSET-P"
    "ASSET-TYPE"
    "ASSET-URL"
    "MAKE-ASSET"
    "MAKE-STATIC-ASSET-MIDDLEWARE"
    "RENDER-ASSETS"
    "RENDER-PAGE"
    "STATIC-ASSET-ERROR"
    "STATIC-ASSET-ERROR-REASON"
    "STATIC-ASSET-ERROR-STATUS")
  "Exact internal static and HTML asset surface after HM-014.")

(defparameter +public-facade-export-contracts+
  `(("CLOG-HYPERMEDIA" ,+hypermedia-api-export-snapshot+)
    ("CLOG-LIVE" nil)
    ("CLOG-PRESENTATIONS2" nil)
    ("CLOG-COMPAT" nil))
  "Facade export snapshots after HM-021.")

(defparameter +internal-export-contracts+
  `(("CLOG-HTTP" ,+http-api-export-snapshot+)
    ("CLOG-ROUTER" ,+router-api-export-snapshot+)
    ("CLOG-COMPONENT" ,+component-internal-api-export-snapshot+)
    ("CLOG-RENDER" ,+render-api-export-snapshot+)
    ("CLOG-HTMX" nil)
    ("CLOG-SESSION" ,+session-api-export-snapshot+)
    ("CLOG-SSE" nil)
    ("CLOG-WS" nil)
    ("CLOG-EFFECT" nil))
  "Internal package export snapshots after HM-021.")

(defparameter +internal-package-names+
  (mapcar #'first +internal-export-contracts+)
  "Internal package names protected from accidental facade leakage.")

(defun package-external-symbol-names (package-designator)
  "Return sorted names of symbols exported by PACKAGE-DESIGNATOR."
  (let ((names nil))
    (do-external-symbols (symbol (find-package package-designator))
      (push (symbol-name symbol) names))
    (sort names #'string<)))

(defun package-use-names (package-designator)
  "Return sorted names of packages used by PACKAGE-DESIGNATOR."
  (let ((package (find-package package-designator)))
    (when package
      (sort (mapcar #'package-name (package-use-list package)) #'string<))))

(defun system-dependency-names (system)
  "Return normalized direct ASDF dependency names for SYSTEM."
  (mapcar (lambda (dependency)
            (string-downcase
             (etypecase dependency
               (string dependency)
               (symbol (symbol-name dependency)))))
          (asdf:system-depends-on system)))

(defun newly-created-threads (threads-before)
  "Return threads present now that were absent from THREADS-BEFORE."
  (set-difference (bordeaux-threads:all-threads)
                  threads-before
                  :test #'eq))

(test package-boundaries/asdf/secondary-systems-match-contract
  (dolist (contract +secondary-system-contracts+)
    (destructuring-bind (system-name dependencies components package-names) contract
      (declare (ignore package-names))
      (let ((system (asdf:find-system system-name nil)))
        (is (not (null system))
            "ASDF must define secondary system ~A." system-name)
        (is (and system
                 (equal components (component-names system)))
            "~A components drifted from the task contract: ~S."
            system-name
            (and system (component-names system)))
        (is (and system
                 (equal dependencies (system-dependency-names system)))
            "~A direct dependencies drifted from the architecture contract: ~S."
            system-name
            (and system (system-dependency-names system)))))))

(test package-boundaries/asdf/secondary-systems-load-without-runtime-start
  (dolist (contract +secondary-system-contracts+)
    (destructuring-bind (system-name dependencies components package-names) contract
      (declare (ignore dependencies components))
      (let ((threads-before (copy-list (bordeaux-threads:all-threads))))
        (is (progn (asdf:load-system system-name) t)
            "ASDF must load secondary system ~A." system-name)
        (is-false (clog:is-running-p))
        (is (null (newly-created-threads threads-before))
            "Loading ~A must not create a background thread."
            system-name)
        (dolist (package-name package-names)
          (is (not (null (find-package package-name)))
              "Loading ~A must define package ~A."
              system-name
              package-name))))))

(test package-boundaries/packages/use-only-common-lisp
  (dolist (package-name
           (append (mapcar #'first +public-facade-export-contracts+)
                   +internal-package-names+))
    (is (equal '("COMMON-LISP") (package-use-names package-name))
        "~A must not broadly USE implementation packages, got ~S."
        package-name
        (package-use-names package-name))))

(test package-boundaries/public-api/export-snapshot
  (dolist (contract +public-facade-export-contracts+)
    (destructuring-bind (package-name expected) contract
      (is (equal expected (package-external-symbol-names package-name))
          "Facade ~A exported API drifted: ~S."
          package-name
          (package-external-symbol-names package-name)))))

(test package-boundaries/internal-api/export-snapshot
  (dolist (contract +internal-export-contracts+)
    (destructuring-bind (package-name expected) contract
      (is (equal expected (package-external-symbol-names package-name))
          "Internal package ~A exported API drifted: ~S."
          package-name
          (package-external-symbol-names package-name)))))

(test package-boundaries/internal-api/non-public-symbols-do-not-leak
  (let ((internal-packages (mapcar #'find-package +internal-package-names+)))
    (dolist (facade-name (mapcar #'first +public-facade-export-contracts+))
      (let ((leaked-symbols nil)
            (facade (find-package facade-name)))
        (do-symbols (symbol facade)
          (multiple-value-bind (found status)
              (find-symbol (symbol-name symbol) facade)
            (when (and (eq found symbol)
                       (member (symbol-package symbol) internal-packages :test #'eq)
                       (not (eq status :external)))
              (pushnew (symbol-name symbol) leaked-symbols :test #'string=))))
        (is (null leaked-symbols)
            "Facade ~A inherited/imported non-public internal symbols ~S."
            facade-name
            (sort leaked-symbols #'string<))))))
