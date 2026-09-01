;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime UI transactions and dirty reduction         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-invalidation
  (:use #:common-lisp)
  (:import-from #:clog-http
                #:clog-hypermedia-error)
  (:export
   #:ui-transaction-error
   #:ui-transaction-error-reason
   #:dirty-set
   #:dirty-set-p
   #:dirty-set-context
   #:dirty-set-components
   #:dirty-set-revisions
   #:dirty-set-empty-p
   #:invalidate-component
   #:with-ui-transaction
   #:dirty-set->action-result))

(defpackage #:clog-hypermedia
  (:import-from #:clog-invalidation
                #:ui-transaction-error
                #:ui-transaction-error-reason
                #:dirty-set
                #:dirty-set-p
                #:dirty-set-context
                #:dirty-set-components
                #:dirty-set-revisions
                #:dirty-set-empty-p
                #:invalidate-component
                #:with-ui-transaction
                #:dirty-set->action-result)
  (:export
   #:ui-transaction-error
   #:ui-transaction-error-reason
   #:dirty-set
   #:dirty-set-p
   #:dirty-set-context
   #:dirty-set-components
   #:dirty-set-revisions
   #:dirty-set-empty-p
   #:invalidate-component
   #:with-ui-transaction
   #:dirty-set->action-result))

(in-package #:clog-invalidation)

(define-condition ui-transaction-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader ui-transaction-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "UI transaction operation failed~@[ (~A)~]."
             (ui-transaction-error-reason condition))))
  (:documentation
   "Redacted HM-034 transaction/dirty-reduction failure.

Only a bounded reason keyword is retained. Component state, rendered HTML,
session identifiers and application data are never printed by this condition."))

(defun transaction-error (reason)
  "Signal UI-TRANSACTION-ERROR carrying bounded REASON."
  (error 'ui-transaction-error :reason reason))

(defstruct (ui-transaction
             (:constructor %make-ui-transaction (context))
             (:copier nil)
             (:conc-name %ui-transaction-))
  "Dynamically scoped collector for one outer HM-034 UI transaction."
  context
  (dirty-components (make-hash-table :test #'equal)))

(defparameter *ui-transaction* nil
  "Current dynamically scoped outer UI transaction, or NIL outside one.")

(defstruct (dirty-set
             (:constructor %make-dirty-set (context components revisions))
             (:copier nil)
             (:conc-name %dirty-set-))
  "Committed immutable-by-API HM-034 representation snapshot.

COMPONENTS is the minimal stable-id-ordered representation set after ancestor
reduction. REVISIONS is an alist of owned component-id strings to the committed
revision represented by that set. CONTEXT is deliberately opaque and is carried
without interpretation so HTTP and future live transports can decide how to
map the committed set after all transaction locks have been released."
  context
  components
  revisions)

(defun dirty-set-context (dirty-set)
  "Return DIRTY-SET's opaque transaction context."
  (check-type dirty-set dirty-set)
  (%dirty-set-context dirty-set))

(defun dirty-set-components (dirty-set)
  "Return a fresh list of the reduced committed components in DIRTY-SET."
  (check-type dirty-set dirty-set)
  (copy-list (%dirty-set-components dirty-set)))

(defun dirty-set-revisions (dirty-set)
  "Return a defensive copy of DIRTY-SET's `(component-id . revision)` alist."
  (check-type dirty-set dirty-set)
  (mapcar (lambda (entry)
            (cons (copy-seq (car entry)) (cdr entry)))
          (%dirty-set-revisions dirty-set)))

(defun dirty-set-empty-p (dirty-set)
  "Return true when DIRTY-SET has no committed representation work."
  (check-type dirty-set dirty-set)
  (null (%dirty-set-components dirty-set)))

(defun record-invalidated-component (transaction component)
  "Record exact COMPONENT capability once in TRANSACTION."
  (let* ((id (clog-component:component-id component))
         (table (%ui-transaction-dirty-components transaction))
         (existing (gethash id table)))
    (cond
      ((null existing)
       (setf (gethash (copy-seq id) table) component))
      ((eq existing component)
       nil)
      (t
       (transaction-error :component-id-conflict))))
  component)

(defun invalidate-component (component)
  "Register COMPONENT as dirty in the current UI transaction.

Invalidation is intentionally declarative: it does not increment a component
revision or render anything. Duplicate invalidations of the same exact object
are idempotent. Calling this function outside WITH-UI-TRANSACTION fails closed."
  (check-type component clog-component:component)
  (unless *ui-transaction*
    (transaction-error :transaction-required))
  (unless (clog-component:mounted-p component)
    (transaction-error :component-not-mounted))
  (record-invalidated-component *ui-transaction* component))

(defun transaction-components (transaction)
  "Return TRANSACTION's unique components in stable component-id order."
  (let ((components nil))
    (maphash (lambda (id component)
               (declare (ignore id))
               (push component components))
             (%ui-transaction-dirty-components transaction))
    (sort components
          #'string<
          :key #'clog-component:component-id)))

(defun call-with-component-locks (components thunk)
  "Call THUNK while holding COMPONENTS' locks in their already-sorted order."
  (labels ((acquire (remaining)
             (if (endp remaining)
                 (funcall thunk)
                 (bordeaux-threads:with-lock-held
                     ((clog-component:component-lock (first remaining)))
                   (acquire (rest remaining))))))
    (acquire components)))

(defun commit-component-locked (component)
  "Commit one revision for COMPONENT while its lock is already held.

This is deliberately local to HM-034 rather than calling TOUCH-COMPONENT,
because TOUCH-COMPONENT owns lock acquisition itself. Multi-component
transactions acquire every component lock first in lexical component-id order,
then mutate only these bounded lifecycle slots before releasing all locks."
  (incf (clog-component::%component-revision component))
  (setf (clog-component::%component-dirty-p component) t
        (clog-component::%component-last-access component)
        (get-universal-time))
  (clog-component::%component-revision component))

(defun commit-components-under-locks (components)
  "Return `(component . revision)` entries committed under stable ordered locks."
  (call-with-component-locks
   components
   (lambda ()
     (loop for component in components
           when (eq :mounted
                    (clog-component::%component-lifecycle-state component))
             collect (cons component (commit-component-locked component))))))

(defun committed-entry-table (entries)
  "Index committed ENTRIES by immutable component id."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (entry entries table)
      (setf (gethash (clog-component:component-id (car entry)) table)
            entry))))

(defun represented-by-dirty-ancestor-p (component committed-table)
  "Return true when COMPONENT's known dirty parent suppresses its representation.

HM-034 reduces over the committed dirty graph available to this transaction.
A direct dirty parent is sufficient to suppress the child. Chained suppression
naturally follows when each intermediate ancestor is also present in that graph.
The function performs no store lookup and therefore cannot introduce global or
application locks after the component commit section."
  (let ((parent-id (clog-component:component-parent-id component)))
    (and parent-id
         (not (null (gethash parent-id committed-table))))))

(defun reduce-committed-entries (entries)
  "Return the minimal stable-id-ordered representation subset of ENTRIES."
  (let ((table (committed-entry-table entries)))
    (remove-if
     (lambda (entry)
       (represented-by-dirty-ancestor-p (car entry) table))
     entries)))

(defun entries->dirty-set (context entries)
  "Construct a defensive DIRTY-SET from already reduced committed ENTRIES."
  (let ((components (mapcar #'car entries))
        (revisions
          (mapcar (lambda (entry)
                    (cons (clog-component:component-id (car entry))
                          (cdr entry)))
                  entries)))
    (%make-dirty-set context components revisions)))

(defun commit-ui-transaction (transaction)
  "Commit TRANSACTION and return its reduced DIRTY-SET.

The only multi-component critical section is the revision/dirty/last-access
mutation. Components are locked in lexical component-id order, unmounted
components are skipped after lock acquisition, and all locks are released
before ancestor reduction, action-result conversion, rendering, JSON/header
encoding or any network operation can occur."
  (let* ((components (transaction-components transaction))
         (committed (commit-components-under-locks components))
         (reduced (reduce-committed-entries committed)))
    (entries->dirty-set (%ui-transaction-context transaction) reduced)))

(defun call-with-ui-transaction (context thunk)
  "Execute THUNK under HM-034 transaction semantics.

Nested calls share the outer collector and return THUNK's values directly; they
never commit or flush independently. The outer call commits only after THUNK
returns normally and returns the committed DIRTY-SET as its primary value,
followed by THUNK's original values as secondary values. If THUNK signals, the
commit path is never entered and invalidation revisions remain unchanged."
  (check-type thunk function)
  (if *ui-transaction*
      (funcall thunk)
      (let ((*ui-transaction* (%make-ui-transaction context)))
        (let ((body-values (multiple-value-list (funcall thunk))))
          (let ((dirty-set (commit-ui-transaction *ui-transaction*)))
            (values-list (cons dirty-set body-values)))))))

(defmacro with-ui-transaction ((context) &body body)
  "Execute BODY in a nestable UI transaction carrying opaque CONTEXT."
  `(call-with-ui-transaction ,context (lambda () ,@body)))

(defun dirty-set->action-result (dirty-set)
  "Convert committed DIRTY-SET into the HM-033 declarative ACTION-RESULT.

No component is rendered and no HTTP header/body is encoded here. If the opaque
transaction context is itself one of the reduced dirty component capabilities,
that component becomes the primary fragment and the remaining components become
additional partials. Otherwise the entire reduced set is emitted as pure
multi-target partial intent. An empty set maps to NO-RENDER."
  (check-type dirty-set dirty-set)
  (let* ((components (%dirty-set-components dirty-set))
         (context (%dirty-set-context dirty-set)))
    (cond
      ((null components)
       (clog-action:no-render))
      ((and (typep context 'clog-component:component)
            (member context components :test #'eq))
       (clog-action:make-action-result
        :primary-component context
        :invalidated-components
        (remove context components :test #'eq)))
      (t
       (clog-action:make-action-result
        :primary-component nil
        :invalidated-components components)))))
