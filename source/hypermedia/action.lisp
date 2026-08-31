;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime static component action registry             ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-action
  (:use #:cl)
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:import-from #:clog-component
                #:component
                #:handle-action
                #:authorize-action-p)
  (:export #:action-error
           #:action-error-reason
           #:invalid-action-definition
           #:invalid-action-definition-reason
           #:action-registration-conflict
           #:action-registration-conflict-component-class
           #:action-registration-conflict-external-name
           #:action-replacement-not-allowed
           #:action-not-found
           #:action-not-found-component-class
           #:action-not-found-external-name
           #:action-method-not-allowed
           #:action-method-not-allowed-method
           #:action-method-not-allowed-allowed-methods
           #:action-descriptor
           #:action-descriptor-p
           #:make-action-descriptor
           #:action-descriptor-symbol
           #:action-descriptor-external-name
           #:action-descriptor-component-class
           #:action-descriptor-allowed-methods
           #:action-descriptor-parameter-decoder
           #:action-descriptor-authorize-function
           #:action-descriptor-authorization-policy
           #:action-descriptor-handler
           #:action-descriptor-requires-current-p
           #:action-descriptor-documentation
           #:action-registry
           #:action-registry-p
           #:make-action-registry
           #:action-registry-development-p
           #:action-registry-count
           #:*action-registry*
           #:register-action
           #:find-action
           #:find-action-descriptor
           #:list-actions
           #:action-method-allowed-p
           #:defaction))

(defpackage #:clog-hypermedia
  (:import-from #:clog-action
                #:action-error
                #:action-error-reason
                #:invalid-action-definition
                #:invalid-action-definition-reason
                #:action-registration-conflict
                #:action-registration-conflict-component-class
                #:action-registration-conflict-external-name
                #:action-replacement-not-allowed
                #:action-not-found
                #:action-not-found-component-class
                #:action-not-found-external-name
                #:action-method-not-allowed
                #:action-method-not-allowed-method
                #:action-method-not-allowed-allowed-methods
                #:action-descriptor
                #:action-descriptor-p
                #:make-action-descriptor
                #:action-descriptor-symbol
                #:action-descriptor-external-name
                #:action-descriptor-component-class
                #:action-descriptor-allowed-methods
                #:action-descriptor-parameter-decoder
                #:action-descriptor-authorize-function
                #:action-descriptor-authorization-policy
                #:action-descriptor-handler
                #:action-descriptor-requires-current-p
                #:action-descriptor-documentation
                #:action-registry
                #:action-registry-p
                #:make-action-registry
                #:action-registry-development-p
                #:action-registry-count
                #:*action-registry*
                #:register-action
                #:find-action
                #:find-action-descriptor
                #:list-actions
                #:action-method-allowed-p
                #:defaction)
  (:export #:action-error
           #:action-error-reason
           #:invalid-action-definition
           #:invalid-action-definition-reason
           #:action-registration-conflict
           #:action-registration-conflict-component-class
           #:action-registration-conflict-external-name
           #:action-replacement-not-allowed
           #:action-not-found
           #:action-not-found-component-class
           #:action-not-found-external-name
           #:action-method-not-allowed
           #:action-method-not-allowed-method
           #:action-method-not-allowed-allowed-methods
           #:action-descriptor
           #:action-descriptor-p
           #:make-action-descriptor
           #:action-descriptor-symbol
           #:action-descriptor-external-name
           #:action-descriptor-component-class
           #:action-descriptor-allowed-methods
           #:action-descriptor-parameter-decoder
           #:action-descriptor-authorize-function
           #:action-descriptor-authorization-policy
           #:action-descriptor-handler
           #:action-descriptor-requires-current-p
           #:action-descriptor-documentation
           #:action-registry
           #:action-registry-p
           #:make-action-registry
           #:action-registry-development-p
           #:action-registry-count
           #:*action-registry*
           #:register-action
           #:find-action
           #:find-action-descriptor
           #:list-actions
           #:action-method-allowed-p
           #:defaction))

(in-package #:clog-action)

(define-condition action-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader action-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Hypermedia action operation failed~@[ (~A)~]."
             (action-error-reason condition))))
  (:documentation
   "Base condition for static action definition, registration and lookup failures."))

(define-condition invalid-action-definition (action-error)
  ((definition-reason
    :initarg :definition-reason
    :reader invalid-action-definition-reason))
  (:report
   (lambda (condition stream)
     (format stream "Invalid static action definition (~A)."
             (invalid-action-definition-reason condition))))
  (:documentation
   "Signaled when an action descriptor, registry option or DEFACTION declaration is invalid.

Only a bounded reason keyword is printed. Function objects, handler bodies and
untrusted lookup strings are not included in the normal condition report."))

(define-condition action-registration-conflict (action-error)
  ((component-class
    :initarg :component-class
    :reader action-registration-conflict-component-class)
   (external-name
    :initarg :external-name
    :reader %action-registration-conflict-external-name))
  (:report
   (lambda (condition stream)
     (format stream "Action ~S is already registered for component class ~S."
             (%action-registration-conflict-external-name condition)
             (action-registration-conflict-component-class condition))))
  (:documentation
   "Signaled when a registry already contains the same component-class/external-name key."))

(define-condition action-replacement-not-allowed
    (action-registration-conflict)
  ()
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "Static action replacement is disabled for this registry.")))
  (:documentation
   "Signaled when explicit replacement is requested outside a development registry."))

(define-condition action-not-found (action-error)
  ((component-class
    :initarg :component-class
    :reader action-not-found-component-class)
   (external-name
    :initarg :external-name
    :initform nil
    :reader %action-not-found-external-name))
  (:report
   (lambda (condition stream)
     (declare (ignore condition))
     (format stream "No registered component action matched the request.")))
  (:documentation
   "Signaled on an explicit failing lookup without printing the runtime action string."))

(define-condition action-method-not-allowed (action-error)
  ((method
    :initarg :method
    :reader action-method-not-allowed-method)
   (allowed-methods
    :initarg :allowed-methods
    :reader %action-method-not-allowed-allowed-methods))
  (:report
   (lambda (condition stream)
     (format stream "HTTP method ~S is not allowed for this component action."
             (action-method-not-allowed-method condition))))
  (:documentation
   "Signaled when a caller explicitly requires method validation and the method is absent."))

(defun action-registration-conflict-external-name (condition)
  "Return a fresh copy of the conflicting static external action name."
  (copy-seq (%action-registration-conflict-external-name condition)))

(defun action-not-found-external-name (condition)
  "Return a fresh copy of the safe lookup name, or NIL when the input was invalid."
  (let ((value (%action-not-found-external-name condition)))
    (and value (copy-seq value))))

(defun action-method-not-allowed-allowed-methods (condition)
  "Return a fresh deterministic list of methods accepted by the action descriptor."
  (copy-list (%action-method-not-allowed-allowed-methods condition)))

(setf (documentation 'action-error-reason 'function)
      "Return the bounded reason keyword carried by an ACTION-ERROR.")
(setf (documentation 'invalid-action-definition-reason 'function)
      "Return the bounded definition reason from INVALID-ACTION-DEFINITION.")
(setf (documentation 'action-registration-conflict-component-class 'function)
      "Return the component class name involved in an action registration conflict.")
(setf (documentation 'action-not-found-component-class 'function)
      "Return the exact component class name used by a failed action lookup.")
(setf (documentation 'action-method-not-allowed-method 'function)
      "Return the normalized HTTP method rejected by ACTION-METHOD-NOT-ALLOWED.")

(defparameter +maximum-action-name-length+ 128
  "Maximum number of characters accepted for an external action name.")

(defparameter +action-method-order+
  '(:get :head :post :put :patch :delete :options)
  "Canonical ordering and vocabulary for action descriptor HTTP methods.")

(defun proper-list-length (value)
  "Return VALUE's finite proper-list length, or NIL for dotted/circular values."
  (handler-case
      (list-length value)
    (type-error () nil)))

(defun external-action-name-character-p (character)
  "Return true when CHARACTER belongs to the canonical URL-segment-safe grammar."
  (or (char<= #\a character #\z)
      (char<= #\0 character #\9)
      (find character "-._~" :test #'char=)))

(defun external-action-name-p (value)
  "Return true when VALUE is a bounded canonical lower-case external action name."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) +maximum-action-name-length+)
       (every #'external-action-name-character-p value)))

(defun canonical-action-name-from-symbol (symbol)
  "Return the canonical lower-case external name derived from trusted SYMBOL."
  (unless (and (symbolp symbol) symbol)
    (error 'invalid-action-definition
           :reason :invalid-action-symbol
           :definition-reason :invalid-action-symbol))
  (let ((name (string-downcase (symbol-name symbol))))
    (unless (external-action-name-p name)
      (error 'invalid-action-definition
             :reason :invalid-external-action-name
             :definition-reason :invalid-external-action-name))
    name))

(defun canonical-component-class-name (designator)
  "Return the exact named COMPONENT subclass represented by DESIGNATOR.

DESIGNATOR may be a component instance, a class object or a class-name symbol.
The function performs no registration and acquires no component or registry
lock. INVALID-ACTION-DEFINITION is signaled for anonymous, missing or unrelated
classes."
  (let* ((name
           (cond
             ((typep designator 'clog-component:component)
              (class-name (class-of designator)))
             ((typep designator 'class)
              (class-name designator))
             ((symbolp designator)
              designator)
             (t nil)))
         (class (and name (find-class name nil))))
    (unless (and (symbolp name) class)
      (error 'invalid-action-definition
             :reason :invalid-component-class
             :definition-reason :invalid-component-class))
    (multiple-value-bind (subtype-p known-p)
        (subtypep name 'clog-component:component)
      (unless (and known-p subtype-p)
        (error 'invalid-action-definition
               :reason :component-class-required
               :definition-reason :component-class-required)))
    name))

(defun normalize-action-methods (methods)
  "Return METHODS as a fresh, duplicate-free list in canonical HTTP order."
  (let ((length (proper-list-length methods)))
    (unless (and length (plusp length))
      (error 'invalid-action-definition
             :reason :invalid-allowed-methods
             :definition-reason :invalid-allowed-methods)))
  (let ((seen (make-hash-table :test #'eq)))
    (dolist (method methods)
      (unless (member method +action-method-order+ :test #'eq)
        (error 'invalid-action-definition
               :reason :invalid-action-method
               :definition-reason :invalid-action-method))
      (when (gethash method seen)
        (error 'invalid-action-definition
               :reason :duplicate-action-method
               :definition-reason :duplicate-action-method))
      (setf (gethash method seen) t))
    (loop for method in +action-method-order+
          when (gethash method seen)
            collect method)))

(defun function-designator-function (value reason)
  "Resolve trusted function designator VALUE or signal INVALID-ACTION-DEFINITION."
  (cond
    ((functionp value) value)
    ((and (symbolp value) (fboundp value))
     (fdefinition value))
    (t
     (error 'invalid-action-definition
            :reason reason
            :definition-reason reason))))

(defun valid-authorization-policy-p (value)
  "Return true for the small enumerable action authorization policy vocabulary."
  (member value
          '(:component-generic :custom-function)
          :test #'eq))

(defstruct (action-descriptor
             (:constructor %make-action-descriptor
                 (symbol external-name component-class allowed-methods
                  parameter-decoder authorize-function authorization-policy
                  handler requires-current-p documentation))
             (:copier nil)
             (:conc-name %action-descriptor-))
  "Immutable public description of one statically registered component action.

The descriptor maps a trusted internal action SYMBOL and exact component class
to a bounded external string. ALLOWED-METHODS and AUTHORIZATION-POLICY are
enumerable metadata. PARAMETER-DECODER, AUTHORIZE-FUNCTION and HANDLER are
validated callables. Public accessors defensively copy mutable values."
  symbol
  external-name
  component-class
  allowed-methods
  parameter-decoder
  authorize-function
  authorization-policy
  handler
  requires-current-p
  documentation)

(setf (documentation 'action-descriptor-p 'function)
      "Return true when VALUE is an ACTION-DESCRIPTOR instance.")

(defun make-action-descriptor
    (&key symbol
          external-name
          component-class
          (allowed-methods '(:post))
          (parameter-decoder #'identity)
          authorize-function
          authorization-policy
          handler
          (requires-current-p nil)
          documentation)
  "Create and return an immutable validated ACTION-DESCRIPTOR.

SYMBOL is a trusted, non-NIL Lisp symbol. EXTERNAL-NAME defaults to the
lower-case symbol name and otherwise must already use the bounded canonical
grammar. COMPONENT-CLASS is an exact named CLOG-COMPONENT:COMPONENT subclass.
ALLOWED-METHODS defaults to (:POST), so GET is never enabled implicitly.
PARAMETER-DECODER receives one request value. AUTHORIZE-FUNCTION receives a
component and request context. HANDLER receives a component and decoded input.
When authorization or handler functions are omitted, generic component protocol
calls are captured without converting any runtime string into a symbol.

The function copies mutable metadata and does not mutate any registry. It
signals INVALID-ACTION-DEFINITION for malformed metadata and acquires no lock.
Example:

  (make-action-descriptor
   :symbol :increment
   :component-class 'counter)
"
  (unless (and (symbolp symbol) symbol)
    (error 'invalid-action-definition
           :reason :invalid-action-symbol
           :definition-reason :invalid-action-symbol))
  (let* ((name
           (if external-name
               (progn
                 (unless (external-action-name-p external-name)
                   (error 'invalid-action-definition
                          :reason :invalid-external-action-name
                          :definition-reason :invalid-external-action-name))
                 (copy-seq external-name))
               (canonical-action-name-from-symbol symbol)))
         (class-name (canonical-component-class-name component-class))
         (methods (normalize-action-methods allowed-methods))
         (decoder
           (function-designator-function
            parameter-decoder
            :invalid-parameter-decoder))
         (authorization
           (if authorize-function
               (function-designator-function
                authorize-function
                :invalid-authorize-function)
               (lambda (component request-context)
                 (clog-component:authorize-action-p
                  component symbol request-context))))
         (policy
           (or authorization-policy
               (if authorize-function
                   :custom-function
                   :component-generic)))
         (resolved-handler
           (if handler
               (function-designator-function
                handler
                :invalid-action-handler)
               (lambda (component action-input)
                 (clog-component:handle-action
                  component symbol action-input)))))
    (unless (valid-authorization-policy-p policy)
      (error 'invalid-action-definition
             :reason :invalid-authorization-policy
             :definition-reason :invalid-authorization-policy))
    (when (or (and (eq policy :custom-function)
                   (null authorize-function))
              (and (eq policy :component-generic)
                   authorize-function))
      (error 'invalid-action-definition
             :reason :authorization-policy-function-mismatch
             :definition-reason :authorization-policy-function-mismatch))
    (unless (or (null documentation) (stringp documentation))
      (error 'invalid-action-definition
             :reason :invalid-action-documentation
             :definition-reason :invalid-action-documentation))
    (unless (or (null requires-current-p) (eq requires-current-p t))
      (error 'invalid-action-definition
             :reason :invalid-requires-current-flag
             :definition-reason :invalid-requires-current-flag))
    (%make-action-descriptor
     symbol
     name
     class-name
     methods
     decoder
     authorization
     policy
     resolved-handler
     requires-current-p
     (and documentation (copy-seq documentation)))))

(defun action-descriptor-symbol (descriptor)
  "Return DESCRIPTOR's trusted internal action symbol."
  (check-type descriptor action-descriptor)
  (%action-descriptor-symbol descriptor))

(defun action-descriptor-external-name (descriptor)
  "Return a fresh copy of DESCRIPTOR's canonical external string name."
  (check-type descriptor action-descriptor)
  (copy-seq (%action-descriptor-external-name descriptor)))

(defun action-descriptor-component-class (descriptor)
  "Return DESCRIPTOR's exact named component class symbol."
  (check-type descriptor action-descriptor)
  (%action-descriptor-component-class descriptor))

(defun action-descriptor-allowed-methods (descriptor)
  "Return a fresh canonical list of HTTP methods explicitly allowed by DESCRIPTOR."
  (check-type descriptor action-descriptor)
  (copy-list (%action-descriptor-allowed-methods descriptor)))

(defun action-descriptor-parameter-decoder (descriptor)
  "Return DESCRIPTOR's one-argument parameter decoder function."
  (check-type descriptor action-descriptor)
  (%action-descriptor-parameter-decoder descriptor))

(defun action-descriptor-authorize-function (descriptor)
  "Return DESCRIPTOR's two-argument component/request authorization function."
  (check-type descriptor action-descriptor)
  (%action-descriptor-authorize-function descriptor))

(defun action-descriptor-authorization-policy (descriptor)
  "Return DESCRIPTOR's enumerable authorization policy keyword."
  (check-type descriptor action-descriptor)
  (%action-descriptor-authorization-policy descriptor))

(defun action-descriptor-handler (descriptor)
  "Return DESCRIPTOR's two-argument component/decoded-input handler function."
  (check-type descriptor action-descriptor)
  (%action-descriptor-handler descriptor))

(defun action-descriptor-requires-current-p (descriptor)
  "Return true when DESCRIPTOR requires the browser revision to be current."
  (check-type descriptor action-descriptor)
  (%action-descriptor-requires-current-p descriptor))

(defun action-descriptor-documentation (descriptor)
  "Return a fresh copy of DESCRIPTOR's documentation string, or NIL."
  (check-type descriptor action-descriptor)
  (let ((value (%action-descriptor-documentation descriptor)))
    (and value (copy-seq value))))

(defstruct (action-registry
             (:constructor %make-action-registry
                 (table lock development-p))
             (:copier nil)
             (:conc-name %action-registry-))
  "Thread-safe registry keyed by exact component class and external action string.

The registry lock protects only hash-table structure. Registration and lookup
never acquire a component lock, execute handlers, decode parameters or perform
I/O. Returned descriptors are immutable through the public API."
  table
  lock
  development-p)

(setf (documentation 'action-registry-p 'function)
      "Return true when VALUE is an ACTION-REGISTRY instance.")

(defun make-action-registry (&key (development-p nil))
  "Create an empty ACTION-REGISTRY and return it.

DEVELOPMENT-P must be boolean. A development registry permits explicit
REGISTER-ACTION replacement for controlled hot reload. Production registries
reject replacement. The function starts no thread and performs no I/O."
  (unless (or (null development-p) (eq development-p t))
    (error 'invalid-action-definition
           :reason :invalid-registry-development-flag
           :definition-reason :invalid-registry-development-flag))
  (%make-action-registry
   (make-hash-table :test #'equal)
   (bordeaux-threads:make-lock "clog-action-registry")
   development-p))

(defparameter *action-registry* (make-action-registry)
  "Dynamically bindable process registry for statically declared actions.

The default registry rejects replacement. Tests and development loaders may
bind this special variable to a fresh registry created with
MAKE-ACTION-REGISTRY. Request code should use FIND-ACTION and must not mutate
the registry while rendering a component.")

(defun action-registry-development-p (registry)
  "Return true when REGISTRY permits explicit development replacement."
  (check-type registry action-registry)
  (%action-registry-development-p registry))

(defun action-registry-key (descriptor)
  "Return a fresh immutable-style hash key for DESCRIPTOR."
  (cons (%action-descriptor-component-class descriptor)
        (copy-seq (%action-descriptor-external-name descriptor))))

(defun register-action
    (descriptor &key (registry *action-registry*) (replace nil))
  "Register DESCRIPTOR in REGISTRY and return DESCRIPTOR plus a status keyword.

The status is :REGISTERED for a new key or :REPLACED for an explicit replacement
inside a development registry. Duplicate keys signal
ACTION-REGISTRATION-CONFLICT. REPLACE in a production registry signals
ACTION-REPLACEMENT-NOT-ALLOWED. REPLACE must be boolean.

Only REGISTRY's structural lock is acquired. No component lock is acquired and
no user function is called while the registry lock is held. Example:

  (register-action descriptor :registry registry)
"
  (check-type descriptor action-descriptor)
  (check-type registry action-registry)
  (unless (or (null replace) (eq replace t))
    (error 'invalid-action-definition
           :reason :invalid-replace-flag
           :definition-reason :invalid-replace-flag))
  (let ((key (action-registry-key descriptor)))
    (bordeaux-threads:with-lock-held ((%action-registry-lock registry))
      (let ((existing (gethash key (%action-registry-table registry))))
        (cond
          ((null existing)
           (setf (gethash key (%action-registry-table registry)) descriptor)
           (values descriptor :registered))
          ((and replace (%action-registry-development-p registry))
           (setf (gethash key (%action-registry-table registry)) descriptor)
           (values descriptor :replaced))
          (replace
           (error 'action-replacement-not-allowed
                  :reason :action-replacement-not-allowed
                  :component-class (%action-descriptor-component-class descriptor)
                  :external-name
                  (copy-seq (%action-descriptor-external-name descriptor))))
          (t
           (error 'action-registration-conflict
                  :reason :duplicate-action-registration
                  :component-class (%action-descriptor-component-class descriptor)
                  :external-name
                  (copy-seq (%action-descriptor-external-name descriptor)))))))))

(defun safe-action-lookup-name (value)
  "Return a defensive copy of canonical lookup VALUE, or NIL when invalid."
  (and (external-action-name-p value) (copy-seq value)))

(defun find-action
    (component-or-class external-name
     &key (registry *action-registry*) (errorp nil))
  "Find and return an exact action descriptor, or NIL when no key matches.

COMPONENT-OR-CLASS is a component instance, class object or named component
class. EXTERNAL-NAME must be a bounded canonical string. Invalid or unknown
runtime strings are treated as misses and never converted into Lisp symbols.
When ERRORP is true, ACTION-NOT-FOUND is signaled instead of returning NIL.

The function acquires only REGISTRY's structural lock, performs no component
mutation and returns an immutable descriptor reference. Example:

  (find-action component action-name)
"
  (check-type registry action-registry)
  (unless (or (null errorp) (eq errorp t))
    (error 'invalid-action-definition
           :reason :invalid-errorp-flag
           :definition-reason :invalid-errorp-flag))
  (let* ((class-name (canonical-component-class-name component-or-class))
         (lookup-name (safe-action-lookup-name external-name))
         (descriptor
           (and lookup-name
                (bordeaux-threads:with-lock-held
                    ((%action-registry-lock registry))
                  (gethash (cons class-name lookup-name)
                           (%action-registry-table registry))))))
    (cond
      (descriptor descriptor)
      (errorp
       (error 'action-not-found
              :reason :action-not-found
              :component-class class-name
              :external-name lookup-name))
      (t nil))))

(defun find-action-descriptor
    (component-or-class external-name
     &key (registry *action-registry*) (errorp nil))
  "Compatibility name for FIND-ACTION with identical exact-string semantics."
  (find-action component-or-class external-name
               :registry registry
               :errorp errorp))

(defun action-registry-count (&optional (registry *action-registry*))
  "Return the number of descriptors currently registered in REGISTRY."
  (check-type registry action-registry)
  (bordeaux-threads:with-lock-held ((%action-registry-lock registry))
    (hash-table-count (%action-registry-table registry))))

(defun descriptor-sort-key (descriptor)
  "Return a deterministic printable sort key for DESCRIPTOR."
  (let* ((class-name (%action-descriptor-component-class descriptor))
         (package (symbol-package class-name)))
    (format nil "~A::~A/~A"
            (if package (package-name package) "")
            (symbol-name class-name)
            (%action-descriptor-external-name descriptor))))

(defun list-actions (&key component-class (registry *action-registry*))
  "Return a fresh deterministic list of descriptors from REGISTRY.

When COMPONENT-CLASS is supplied, only exact registrations for that class are
returned. The registry lock is held only while descriptor references are copied;
sorting and user inspection occur after release. The function performs no
component mutation or I/O."
  (check-type registry action-registry)
  (let ((class-name
          (and component-class
               (canonical-component-class-name component-class)))
        (descriptors nil))
    (bordeaux-threads:with-lock-held ((%action-registry-lock registry))
      (maphash
       (lambda (key descriptor)
         (when (or (null class-name) (eq class-name (car key)))
           (push descriptor descriptors)))
       (%action-registry-table registry)))
    (sort descriptors #'string< :key #'descriptor-sort-key)))

(defun action-method-allowed-p (descriptor method &key (errorp nil))
  "Return true when METHOD is explicitly allowed by DESCRIPTOR.

METHOD must already be a keyword from the bounded action method vocabulary; no
string-to-symbol conversion is attempted. The default descriptor method list is
(:POST), therefore GET is false unless explicitly declared. When ERRORP is true,
ACTION-METHOD-NOT-ALLOWED is signaled for a missing or invalid method."
  (check-type descriptor action-descriptor)
  (unless (or (null errorp) (eq errorp t))
    (error 'invalid-action-definition
           :reason :invalid-errorp-flag
           :definition-reason :invalid-errorp-flag))
  (unless (member method +action-method-order+ :test #'eq)
    (if errorp
        (error 'invalid-action-definition
               :reason :invalid-action-method
               :definition-reason :invalid-action-method)
        (return-from action-method-allowed-p nil)))
  (if (member method
              (%action-descriptor-allowed-methods descriptor)
              :test #'eq)
      t
      (if errorp
          (error 'action-method-not-allowed
                 :reason :action-method-not-allowed
                 :method method
                 :allowed-methods
                 (copy-list (%action-descriptor-allowed-methods descriptor)))
          nil)))

(defun macro-literal-action-methods (value)
  "Return a trusted literal method list from VALUE or signal a definition error.

DEFACTION accepts either (:POST :DELETE) or the explicitly quoted spelling
'(:POST :DELETE). Arbitrary computed forms are rejected at macro expansion so
registry method policy remains static and enumerable."
  (let ((literal
          (if (and (consp value)
                   (eq (first value) 'quote)
                   (null (cddr value)))
              (second value)
              value)))
    (normalize-action-methods literal)))

(defun macro-literal-boolean-p (value)
  "Return true when VALUE is the literal boolean NIL or T."
  (or (null value) (eq value t)))

(defmacro defaction
    ((component-class action
      &key external-name
           (allowed-methods '(:post))
           (parameter-decoder '(function identity))
           authorize
           (requires-current nil)
           documentation
           (replace nil))
     (component-variable request-variable)
     &body body)
  "Define and statically register one component action.

COMPONENT-CLASS and ACTION are trusted source symbols. ACTION normally uses a
keyword such as :INCREMENT. EXTERNAL-NAME defaults to its canonical lower-case
name. ALLOWED-METHODS is a literal method list and defaults to (:POST).
PARAMETER-DECODER is stored in the descriptor and is applied by the later action
dispatcher before the generated handler is called. AUTHORIZE is a function form;
when omitted, the descriptor delegates to AUTHORIZE-ACTION-P. The generated EQL
HANDLE-ACTION method receives the decoded input in REQUEST-VARIABLE.

The expansion contains no generated package-local helper symbol or unstable
GENSYM. Registration occurs at load/execution time before the method is replaced,
so a duplicate production declaration fails closed. REPLACE succeeds only when
*ACTION-REGISTRY* is dynamically bound to a development registry. The macro
hides no network I/O and introduces no additional lock around the user body.

Example:

  (defaction (counter :increment :authorize #'counter-visible-p)
      (component request)
    (declare (ignore request))
    (incf (counter-value component)))
"
  (unless (symbolp component-class)
    (error 'invalid-action-definition
           :reason :invalid-component-class
           :definition-reason :invalid-component-class))
  (unless (and (symbolp action) action)
    (error 'invalid-action-definition
           :reason :invalid-action-symbol
           :definition-reason :invalid-action-symbol))
  (unless (and (symbolp component-variable)
               (symbolp request-variable)
               (not (eq component-variable request-variable)))
    (error 'invalid-action-definition
           :reason :invalid-action-lambda-list
           :definition-reason :invalid-action-lambda-list))
  (unless (or (null external-name) (stringp external-name))
    (error 'invalid-action-definition
           :reason :invalid-external-action-name
           :definition-reason :invalid-external-action-name))
  (unless (macro-literal-boolean-p requires-current)
    (error 'invalid-action-definition
           :reason :non-literal-requires-current
           :definition-reason :non-literal-requires-current))
  (unless (macro-literal-boolean-p replace)
    (error 'invalid-action-definition
           :reason :non-literal-replace
           :definition-reason :non-literal-replace))
  (unless (or (null documentation) (stringp documentation))
    (error 'invalid-action-definition
           :reason :invalid-action-documentation
           :definition-reason :invalid-action-documentation))
  (let* ((resolved-name
           (or external-name (canonical-action-name-from-symbol action)))
         (resolved-methods (macro-literal-action-methods allowed-methods))
         (authorization-form authorize)
         (authorization-policy
           (if authorize :custom-function :component-generic))
         (registration-form
           `(clog-action:register-action
             (clog-action:make-action-descriptor
              :symbol ',action
              :external-name ,resolved-name
              :component-class ',component-class
              :allowed-methods ',resolved-methods
              :parameter-decoder ,parameter-decoder
              :authorize-function ,authorization-form
              :authorization-policy ,authorization-policy
              :handler
              (lambda (clog-action::component clog-action::action-input)
                (clog-component:handle-action
                 clog-action::component
                 ',action
                 clog-action::action-input))
              :requires-current-p ,requires-current
              :documentation ,documentation)
             :replace ,replace)))
    `(progn
       ,(list 'cl:eval-when '(:load-toplevel :execute) registration-form)
       (defmethod clog-component:handle-action
           ((,component-variable ,component-class)
            (clog-action::resolved-action (eql ',action))
            ,request-variable)
         ,@(when documentation (list documentation))
         (declare (ignore clog-action::resolved-action))
         ,@body)
       ',action)))
