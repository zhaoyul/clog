;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime transactional component invalidation        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-invalidation
  (:use #:common-lisp)
  (:export
   #:ui-transaction-error
   #:ui-transaction-error-reason
   #:with-ui-transaction
   #:invalidate-component))

(defpackage #:clog-hypermedia
  (:import-from #:clog-invalidation
                #:ui-transaction-error
                #:ui-transaction-error-reason
                #:with-ui-transaction
                #:invalidate-component)
  (:export
   #:ui-transaction-error
   #:ui-transaction-error-reason
   #:with-ui-transaction
   #:invalidate-component))

(in-package #:clog-invalidation)

(define-condition ui-transaction-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :initform nil
    :reader ui-transaction-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "UI transaction failed~@[ (~A)~]."
             (ui-transaction-error-reason condition))))
  (:documentation
   "Redacted HM-034 transaction failure. Reports carry only a bounded reason
keyword and never include request, session, component or rendered values."))

(defun transaction-error (reason)
  "Signal UI-TRANSACTION-ERROR with symbolic REASON."
  (error 'ui-transaction-error :reason reason))

(defstruct (ui-transaction
             (:constructor %make-ui-transaction (context dirty-table))
             (:copier nil)
             (:conc-name %ui-transaction-))
  "Request-scoped dirty-set accumulator owned by the outermost transaction."
  context
  dirty-table)

(defvar *ui-transaction* nil
  "Dynamically bound outer UI transaction, or NIL outside transactional work.")

(defun same-transaction-context-p (left right)
  "Require nested transactions to share the exact immutable request context."
  (eq left right))

(defun record-dirty-component (transaction component)
  "Record exact COMPONENT identity once in TRANSACTION and return COMPONENT."
  (check-type transaction ui-transaction)
  (check-type component clog-component:component)
  (let* ((id (clog-component:component-id component))
         (table (%ui-transaction-dirty-table transaction))
         (existing (gethash id table)))
    (when (and existing (not (eq existing component)))
      (transaction-error :component-identity-conflict))
    (setf (gethash id table) component)
    component))

(defun invalidate-component (component)
  "Mark COMPONENT dirty, deferring revision commit inside a UI transaction.

Outside a transaction this is a single-component mutation and therefore reuses
TOUCH-COMPONENT. Inside a transaction only the exact component capability is
recorded. The outermost transaction later commits each dirty component once."
  (check-type component clog-component:component)
  (if *ui-transaction*
      (record-dirty-component *ui-transaction* component)
      (progn
        (clog-component:touch-component component)
        component)))

(defun transaction-dirty-components (transaction)
  "Return TRANSACTION's exact dirty components in stable component-id order."
  (let ((components nil))
    (maphash (lambda (id component)
               (declare (ignore id))
               (push component components))
             (%ui-transaction-dirty-table transaction))
    (sort components #'string< :key #'clog-component:component-id)))

(defun exclude-current-component (components excluded-component)
  "Return COMPONENTS without EXCLUDED-COMPONENT, rejecting ID aliases."
  (if (null excluded-component)
      components
      (let ((excluded-id (clog-component:component-id excluded-component)))
        (loop for component in components
              for id = (clog-component:component-id component)
              unless (string= id excluded-id)
                collect component
              else do (unless (eq component excluded-component)
                        (transaction-error :component-identity-conflict))))))

(defun call-with-component-locks (components thunk)
  "Acquire COMPONENTS in their already stable order, then call THUNK once."
  (labels ((acquire (remaining)
             (if (null remaining)
                 (funcall thunk)
                 (bordeaux-threads:with-lock-held
                     ((clog-component:component-lock (first remaining)))
                   (acquire (rest remaining))))))
    (acquire components)))

(defun commit-dirty-components (transaction &key excluded-component)
  "Commit all mounted dirty components exactly once and return committed list.

Locks are acquired only after the transaction body has returned and in stable
lexicographic component-id order. All selected locks are acquired before any
revision is changed. Components unmounted before commit are ignored."
  (let* ((ordered
           (exclude-current-component
            (transaction-dirty-components transaction)
            excluded-component))
         (committed nil))
    (call-with-component-locks
     ordered
     (lambda ()
       ;; Validate the full selected set while all relevant locks are held so a
       ;; later commit cannot fail halfway because of lifecycle state changes.
       (let ((mounted
               (remove-if-not #'clog-component:mounted-p ordered)))
         (dolist (component mounted)
           (unless (and (integerp (clog-component:component-revision component))
                        (not (minusp (clog-component:component-revision component))))
             (transaction-error :invalid-component-revision)))
         (dolist (component mounted)
           (clog-hypermedia::hm-025-commit-component-change component)
           (push component committed)))))
    (nreverse committed)))

(defun call-with-ui-transaction
    (context thunk &key excluded-component after-flush)
  "Execute THUNK in one request transaction and flush only at the outer edge.

Nested calls must use the exact same CONTEXT and simply contribute to the outer
dirty set. For an outer call, THUNK must complete normally before state is
flushed. AFTER-FLUSH, when supplied by the framework action dispatcher, receives
three arguments: the list of THUNK return values, the committed dirty component
list, and the transaction object. No component lock is held during AFTER-FLUSH."
  (check-type context clog-http:request-context)
  (unless (functionp thunk)
    (transaction-error :invalid-transaction-thunk))
  (if *ui-transaction*
      (progn
        (unless (same-transaction-context-p
                 context (%ui-transaction-context *ui-transaction*))
          (transaction-error :nested-context-mismatch))
        (when (or excluded-component after-flush)
          (transaction-error :nested-framework-control))
        (funcall thunk))
      (let* ((transaction
               (%make-ui-transaction context (make-hash-table :test #'equal)))
             (body-values
               (let ((*ui-transaction* transaction))
                 (multiple-value-list (funcall thunk))))
             (committed
               (commit-dirty-components
                transaction :excluded-component excluded-component)))
        (if after-flush
            (funcall after-flush body-values committed transaction)
            (values-list body-values)))))

(defmacro with-ui-transaction ((context) &body body)
  "Run BODY in a request-scoped UI transaction and return BODY's values.

Only the outermost transaction commits the accumulated dirty set. An error or
non-local exit from BODY occurs before flush, so no deferred dirty revision is
committed by this transaction."
  `(call-with-ui-transaction ,context (lambda () ,@body)))

(defun candidate-component-table (components &optional primary)
  "Return component-id -> exact component table for reduction candidates."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (component components)
      (setf (gethash (clog-component:component-id component) table) component))
    (when primary
      (setf (gethash (clog-component:component-id primary) table) primary))
    table))

(defun component-ancestor-p (ancestor descendant table)
  "Return true when ANCESTOR is reachable through DESCENDANT parent ids in TABLE."
  (let ((ancestor-id (clog-component:component-id ancestor))
        (seen (make-hash-table :test #'equal))
        (cursor descendant))
    (loop
      for parent-id = (clog-component:component-parent-id cursor)
      while parent-id
      do (when (string= ancestor-id parent-id)
           (return t))
         (when (gethash parent-id seen)
           (transaction-error :component-parent-cycle))
         (setf (gethash parent-id seen) t)
         (let ((parent (gethash parent-id table)))
           (unless parent
             (return nil))
           (setf cursor parent))
      finally (return nil))))

(defun component-dependency-depth (component table)
  "Return COMPONENT's deterministic depth through candidate parent links."
  (let ((depth 0)
        (seen (make-hash-table :test #'equal))
        (cursor component))
    (loop
      for parent-id = (clog-component:component-parent-id cursor)
      while parent-id
      for parent = (gethash parent-id table)
      while parent
      do (when (gethash parent-id seen)
           (transaction-error :component-parent-cycle))
         (setf (gethash parent-id seen) t)
         (incf depth)
         (setf cursor parent)
      finally (return depth))))

(defun stable-component-order (components &optional primary)
  "Return COMPONENTS ordered by candidate dependency depth, then component id."
  (let ((table (candidate-component-table components primary)))
    (stable-sort
     (copy-list components)
     (lambda (left right)
       (let ((left-depth (component-dependency-depth left table))
             (right-depth (component-dependency-depth right table)))
         (if (= left-depth right-depth)
             (string< (clog-component:component-id left)
                      (clog-component:component-id right))
             (< left-depth right-depth)))))))

(defun reduce-dirty-components (components &optional primary)
  "Remove dirty descendants already covered by a dirty ancestor or PRIMARY."
  (let* ((ordered (stable-component-order components primary))
         (table (candidate-component-table ordered primary)))
    (loop for component in ordered
          unless (or (and primary
                          (component-ancestor-p primary component table))
                     (some (lambda (candidate)
                             (and (not (eq candidate component))
                                  (component-ancestor-p
                                   candidate component table)))
                           ordered))
            collect component)))

(defun action-result-primary-component-object (result current-component)
  "Return RESULT's concrete primary component, or NIL."
  (let ((primary (clog-action:action-result-primary-component result)))
    (cond
      ((null primary) nil)
      ((eq primary :current) current-component)
      ((eq primary current-component) current-component)
      (t (transaction-error :foreign-primary-component)))))

(defun dirty-ancestor-of-primary (dirty-components primary)
  "Return the first stable dirty ancestor covering PRIMARY, or NIL."
  (when primary
    (let ((table (candidate-component-table dirty-components primary)))
      (find-if
       (lambda (component)
         (component-ancestor-p component primary table))
       (stable-component-order dirty-components primary)))))

(defun copy-action-result-with-dirty-components
    (result current-component committed-components)
  "Merge committed dirty components into RESULT after deterministic reduction.

State commit and render reduction are intentionally separate: every committed
dirty component receives its revision even when an ancestor makes its HTML
partial redundant. Redirect and explicit 204 results commit state but keep their
body-less HTTP contract."
  (clog-action::validate-action-result result)
  (when (or (clog-action:action-result-redirect-url result)
            (= 204 (clog-action:action-result-status result)))
    (return-from copy-action-result-with-dirty-components result))
  (let* ((primary
           (action-result-primary-component-object result current-component))
         (reduced (reduce-dirty-components committed-components primary))
         (covering-ancestor (dirty-ancestor-of-primary reduced primary))
         (resolved-primary
           (if covering-ancestor nil
               (clog-action:action-result-primary-component result)))
         (existing (clog-action:action-result-invalidated-components result))
         (additional
           (if covering-ancestor
               reduced
               (remove-if
                (lambda (component)
                  (and primary
                       (component-ancestor-p
                        primary component
                        (candidate-component-table reduced primary))))
                reduced))))
    (clog-action:make-action-result
     :primary-component resolved-primary
     :invalidated-components (append existing additional)
     :effects (clog-action:action-result-effects result)
     :response-headers (clog-action:action-result-response-headers result)
     :push-url (clog-action:action-result-push-url result)
     :replace-url (clog-action:action-result-replace-url result)
     :redirect-url (clog-action:action-result-redirect-url result)
     :flash (clog-action:action-result-flash result)
     :status (clog-action:action-result-status result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HM-034 late action-dispatch transaction bridge                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia)

(defun hm-025-dispatch-known-component
    (application component descriptor context decoded-input expected-revision)
  "HM-034 bridge: commit state transactionally, then render with no locks held."
  (clog-invalidation::call-with-ui-transaction
   context
   (lambda ()
     (bordeaux-threads:with-lock-held ((clog-component:component-lock component))
       (unless (clog-component:mounted-p component)
         (return-from hm-025-dispatch-known-component
           (hm-025-component-expired-response context)))
       (when (and (clog-action:action-descriptor-requires-current-p descriptor)
                  (/= expected-revision
                      (clog-component:component-revision component)))
         (return-from hm-025-dispatch-known-component
           (hm-025-stale-response application component context)))
       (clog-component:validate-action
        component (clog-action:action-descriptor-symbol descriptor) context)
       (let* ((clog-action::*action-dispatch-lock-owner* component)
              (result
                (funcall (clog-action:action-descriptor-handler descriptor)
                         component decoded-input)))
         (unless (clog-action::valid-hm-025-action-result-p result component)
           (error 'clog-action:invalid-action-result
                  :reason :invalid-action-result))
         ;; The current action component is already protected by this lock and
         ;; commits exactly once regardless of whether user code invalidated it.
         (hm-025-commit-component-change component)
         result)))
   :excluded-component component
   :after-flush
   (lambda (body-values committed transaction)
     (declare (ignore transaction))
     (let ((result (first body-values)))
       (hm-025-response-from-result
        (clog-invalidation::copy-action-result-with-dirty-components
         result component committed)
        application component context)))))
