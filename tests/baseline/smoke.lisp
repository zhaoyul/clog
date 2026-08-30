;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime baseline smoke tests                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defparameter +legacy-source-component-order+
  '("asdf-ext"
    "clog-connection"
    "clog-connection-websockets"
    "clog"
    "clog-system"
    "clog-utilities"
    "clog-base"
    "clog-element"
    "clog-jquery"
    "clog-body"
    "clog-document"
    "clog-window"
    "clog-location"
    "clog-navigator"
    "clog-style"
    "clog-element-common"
    "clog-form"
    "clog-multimedia"
    "clog-canvas"
    "clog-webgl"
    "clog-panel"
    "clog-tree"
    "clog-presentations"
    "clog-data"
    "clog-dbi"
    "clog-auth"
    "clog-web"
    "clog-web-dbi"
    "clog-web-themes"
    "clog-gui"
    "clog-helpers")
  "Frozen source component order for the Legacy #:clog ASDF system.")

(defun component-names (component)
  "Return the lower-case names of COMPONENT's direct ASDF children."
  (mapcar (lambda (child)
            (string-downcase (string (asdf:component-name child))))
          (asdf:component-children component)))

(defun find-direct-child (component name)
  "Find a direct ASDF child of COMPONENT whose name equals NAME."
  (find name
        (asdf:component-children component)
        :test #'string-equal
        :key #'asdf:component-name))

(defun make-baseline-legacy-handler ()
  "Construct a minimal Legacy CLOG on-new-window handler without starting IO."
  (lambda (body)
    (clog:create-div body :content "CLOG baseline smoke")))

(test baseline/runner/signals-a-condition-on-failure
  (is-true (ensure-test-results '(intentional-test-result)))
  (is-true (ensure-test-success t nil))
  (signals hypermedia-test-failure
    (ensure-test-results nil))
  (signals hypermedia-test-failure
    (ensure-test-success nil '(intentional-test-fixture))))

(test baseline/asdf/clog-system-is-loaded
  (is (asdf:find-system :clog nil))
  (is (member :clog *features*)))

(test baseline/asdf/legacy-component-order-is-frozen
  (let* ((system (asdf:find-system :clog))
         (source-module (find-direct-child system "source")))
    (is (equal '("static-files" "source")
               (component-names system)))
    (is (not (null source-module)))
    (is (equal +legacy-source-component-order+
               (component-names source-module)))))

(test baseline/runtime/test-system-loading-is-passive
  (is-false (clog:is-running-p)))

(test baseline/public-api/core-exports-remain-external
  (dolist (name '("INITIALIZE"
                  "SHUTDOWN"
                  "IS-RUNNING-P"
                  "CLOG-OBJ"
                  "CLOG-ELEMENT"
                  "CREATE-DIV"
                  "CREATE-BUTTON"
                  "SET-ON-CLICK"
                  "HTML-ID"
                  "PARENT"))
    (multiple-value-bind (symbol status)
        (find-symbol name :clog)
      (is (not (null symbol)) "CLOG must still define ~A." name)
      (is (eq :external status)
          "CLOG::~A must remain exported, but its status was ~S."
          name status))))

(test baseline/public-api/core-callables-and-classes-remain-defined
  (dolist (name '("INITIALIZE"
                  "SHUTDOWN"
                  "IS-RUNNING-P"
                  "CREATE-DIV"
                  "CREATE-BUTTON"
                  "SET-ON-CLICK"
                  "HTML-ID"
                  "PARENT"))
    (let ((symbol (find-symbol name :clog)))
      (is (and symbol (fboundp symbol))
          "CLOG:~A must remain callable."
          name)))
  (dolist (name '("CLOG-OBJ" "CLOG-ELEMENT"))
    (let ((symbol (find-symbol name :clog)))
      (is (and symbol (find-class symbol nil))
          "CLOG:~A must remain a defined class."
          name))))

(test baseline/legacy/minimal-app-construction
  (let ((root (make-instance 'clog:clog-obj
                             :connection-id "baseline-connection"
                             :html-id "baseline-root"))
        (handler (make-baseline-legacy-handler)))
    (is (typep root 'clog:clog-obj))
    (is (string= "baseline-root" (clog:html-id root)))
    (is (null (clog:parent root)))
    (is (functionp handler))))
