;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime package-boundary tests                       ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +hm-002-secondary-system-contracts+
  '(("clog/hypermedia"
     ("clog"
      "spinneret"
      "yason"
      "lack-middleware-session"
      "lack-middleware-csrf")
     ("CLOG-HTTP"
      "CLOG-ROUTER"
      "CLOG-COMPONENT"
      "CLOG-RENDER"
      "CLOG-HTMX"
      "CLOG-SESSION"
      "CLOG-HYPERMEDIA"))
    ("clog/live"
     ("clog/hypermedia"
      "websocket-driver")
     ("CLOG-SSE"
      "CLOG-WS"
      "CLOG-EFFECT"
      "CLOG-LIVE"))
    ("clog/presentations2"
     ("clog/hypermedia")
     ("CLOG-PRESENTATIONS2"))
    ("clog/compat"
     ("clog/hypermedia"
      "clog/live")
     ("CLOG-COMPAT")))
  "Secondary ASDF systems, dependencies and packages introduced by HM-002.")

(defparameter +hm-002-public-facade-packages+
  '("CLOG-HYPERMEDIA"
    "CLOG-LIVE"
    "CLOG-PRESENTATIONS2"
    "CLOG-COMPAT")
  "Public facade packages whose HM-002 export snapshot is intentionally empty.")

(defparameter +hm-002-internal-packages+
  '("CLOG-HTTP"
    "CLOG-ROUTER"
    "CLOG-COMPONENT"
    "CLOG-RENDER"
    "CLOG-HTMX"
    "CLOG-SESSION"
    "CLOG-SSE"
    "CLOG-WS"
    "CLOG-EFFECT")
  "Internal packages that must not leak transport implementation through a facade.")

(defun package-external-symbol-names (package-designator)
  "Return sorted names of symbols exported by PACKAGE-DESIGNATOR."
  (let ((package (find-package package-designator))
        (names nil))
    (when package
      (do-symbols (symbol package)
        (multiple-value-bind (found status)
            (find-symbol (symbol-name symbol) package)
          (when (and (eq found symbol)
                     (eq status :external))
            (pushnew (symbol-name symbol) names :test #'string=)))))
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
  (dolist (contract +hm-002-secondary-system-contracts+)
    (destructuring-bind (system-name dependencies package-names) contract
      (declare (ignore package-names))
      (let ((system (asdf:find-system system-name nil)))
        (is (not (null system))
            "ASDF must define secondary system ~A." system-name)
        (is (and system
                 (equal '("packages") (component-names system)))
            "~A must load only its package skeleton in HM-002, got ~S."
            system-name
            (and system (component-names system)))
        (is (and system
                 (equal dependencies (system-dependency-names system)))
            "~A direct dependencies drifted from the architecture contract: ~S."
            system-name
            (and system (system-dependency-names system)))))))

(test package-boundaries/asdf/secondary-systems-load-without-runtime-start
  (dolist (contract +hm-002-secondary-system-contracts+)
    (destructuring-bind (system-name dependencies package-names) contract
      (declare (ignore dependencies))
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
           (append +hm-002-public-facade-packages+
                   +hm-002-internal-packages+))
    (is (equal '("COMMON-LISP") (package-use-names package-name))
        "~A must not broadly USE implementation packages, got ~S."
        package-name
        (package-use-names package-name))))

(test package-boundaries/public-api/export-snapshot-is-empty
  (dolist (package-name +hm-002-public-facade-packages+)
    (is (null (package-external-symbol-names package-name))
        "~A must not advertise unimplemented public APIs, exported ~S."
        package-name
        (package-external-symbol-names package-name))))

(test package-boundaries/internal-api/export-snapshot-is-empty
  (dolist (package-name +hm-002-internal-packages+)
    (is (null (package-external-symbol-names package-name))
        "Internal package ~A must not export an implementation API in HM-002, got ~S."
        package-name
        (package-external-symbol-names package-name))))

(test package-boundaries/internal-api/transport-symbols-do-not-leak
  (let ((internal-packages
          (mapcar #'find-package +hm-002-internal-packages+)))
    (dolist (facade-name +hm-002-public-facade-packages+)
      (let ((leaked-symbols nil))
        (do-symbols (symbol (find-package facade-name))
          (when (member (symbol-package symbol) internal-packages :test #'eq)
            (pushnew (symbol-name symbol) leaked-symbols :test #'string=)))
        (is (null leaked-symbols)
            "Facade ~A inherited or imported internal transport symbols ~S."
            facade-name
            (sort leaked-symbols #'string<))))))
