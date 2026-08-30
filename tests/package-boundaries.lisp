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
      "lack-middleware-csrf")
     ("packages" "conditions" "request")
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

(defparameter +request-api-export-snapshot+
  '("CLOG-HYPERMEDIA-ERROR"
    "FORM-PARAM"
    "FORM-PARAM-VALUES"
    "HTMX-FULL-REQUEST-P"
    "HTMX-PARTIAL-REQUEST-P"
    "HTMX-REQUEST-P"
    "HTMX-REQUEST-SOURCE"
    "HTMX-REQUEST-TARGET"
    "HTMX-REQUEST-TRIGGER"
    "HTMX-REQUEST-TYPE"
    "MAKE-REQUEST-CONTEXT"
    "PATH-PARAM"
    "QUERY-PARAM"
    "QUERY-PARAM-VALUES"
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
    "REQUEST-USER")
  "Exact public surface introduced by HM-010.")

(defparameter +public-facade-export-contracts+
  `(("CLOG-HYPERMEDIA" ,+request-api-export-snapshot+)
    ("CLOG-LIVE" nil)
    ("CLOG-PRESENTATIONS2" nil)
    ("CLOG-COMPAT" nil))
  "Facade export snapshots after HM-010.")

(defparameter +internal-export-contracts+
  `(("CLOG-HTTP" ,+request-api-export-snapshot+)
    ("CLOG-ROUTER" nil)
    ("CLOG-COMPONENT" nil)
    ("CLOG-RENDER" nil)
    ("CLOG-HTMX" nil)
    ("CLOG-SESSION" nil)
    ("CLOG-SSE" nil)
    ("CLOG-WS" nil)
    ("CLOG-EFFECT" nil))
  "Internal package export snapshots after HM-010.")

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
