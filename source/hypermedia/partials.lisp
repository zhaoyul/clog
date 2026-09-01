;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime HTMX multi-target partial responses         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-partials
  (:use #:common-lisp)
  (:export
   #:partial #:partial-p #:make-partial
   #:partial-component #:partial-component-id #:partial-revision
   #:partial-target #:partial-swap
   #:render-partial #:render-partials))

(defpackage #:clog-hypermedia
  (:import-from #:clog-partials
                #:partial #:partial-p #:make-partial
                #:partial-component #:partial-component-id #:partial-revision
                #:partial-target #:partial-swap
                #:render-partial #:render-partials)
  (:export
   #:partial #:partial-p #:make-partial
   #:partial-component #:partial-component-id #:partial-revision
   #:partial-target #:partial-swap
   #:render-partial #:render-partials))

(in-package #:clog-partials)

(define-condition partial-error (clog-http:clog-hypermedia-error)
  ((reason :initarg :reason :reader partial-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "Partial response validation failed (~A)."
             (partial-error-reason condition))))
  (:documentation
   "Redacted HM-032 validation failure. The report never includes a component,
selector, rendered body or session identifier."))

(defun invalid-partial (reason)
  "Signal a redacted PARTIAL-ERROR with bounded REASON."
  (error 'partial-error :reason reason))

(defparameter +partial-swap-styles+
  '("outerMorph" "innerMorph" "textContent")
  "Closed HM-032 swap vocabulary for multi-target responses.")

(defun partial-selector-token-character-p (character)
  "Return true for one character in HM-032's simple selector vocabulary."
  (or (alphanumericp character)
      (char= character #\-)
      (char= character #\_)
      (char= character #\.)))

(defun validate-partial-target (target)
  "Return an owned safe simple ID/class TARGET selector.

HM-032 intentionally accepts a narrow selector grammar: an ID or class selector
beginning with `#` or `.`, followed by alphanumeric, hyphen, underscore or dot
characters. This is enough for stable component roots and simple class targets
without exposing arbitrary CSS/HTML syntax at this response boundary."
  (unless (and (stringp target)
               (<= 2 (length target) 4096)
               (member (char target 0) '(#\# #\.) :test #'char=)
               (every #'partial-selector-token-character-p
                      (subseq target 1)))
    (invalid-partial :invalid-target-selector))
  (copy-seq target))

(defun validate-partial-swap (swap)
  "Return an owned SWAP from the HM-032 closed vocabulary."
  (unless (and (stringp swap)
               (member swap +partial-swap-styles+ :test #'string=))
    (invalid-partial :invalid-swap-style))
  (copy-seq swap))

(defstruct (partial
             (:constructor %make-partial
                 (component component-id revision target swap))
             (:conc-name %partial-)
             (:predicate partial-p))
  "Immutable-by-API descriptor for one HTMX 4 multi-target update.

Mutable strings are owned by the descriptor and public readers return copies.
COMPONENT is retained as the rendering capability; COMPONENT-ID and REVISION
snapshot the committed identity/version used for deterministic reduction."
  component
  component-id
  revision
  target
  swap)

(defun partial-component (partial)
  "Return PARTIAL's component capability."
  (check-type partial partial)
  (%partial-component partial))

(defun partial-component-id (partial)
  "Return an owned copy of PARTIAL's component id snapshot."
  (check-type partial partial)
  (copy-seq (%partial-component-id partial)))

(defun partial-revision (partial)
  "Return PARTIAL's committed revision snapshot."
  (check-type partial partial)
  (%partial-revision partial))

(defun partial-target (partial)
  "Return an owned copy of PARTIAL's validated target selector."
  (check-type partial partial)
  (copy-seq (%partial-target partial)))

(defun partial-swap (partial)
  "Return an owned copy of PARTIAL's validated swap style."
  (check-type partial partial)
  (copy-seq (%partial-swap partial)))

(defun make-partial
    (component &key (target nil target-supplied-p)
                    (swap "outerMorph"))
  "Create a validated PARTIAL descriptor for COMPONENT.

TARGET defaults to COMPONENT's stable root selector. SWAP defaults to
`outerMorph` and is limited to `outerMorph`, `innerMorph` or `textContent`.
The current committed component revision is snapshotted for deterministic
same-component reduction. No session authorization or rendering occurs here."
  (check-type component clog-component:component)
  (unless (clog-component:mounted-p component)
    (invalid-partial :component-not-mounted))
  (let* ((component-id (clog-component:component-id component))
         (target (if target-supplied-p
                     target
                     (format nil "#~A" (clog-render:component-dom-id component))))
         (revision (clog-component:component-revision component)))
    (unless (and (integerp revision) (not (minusp revision)))
      (invalid-partial :invalid-component-revision))
    (%make-partial component
                   (copy-seq component-id)
                   revision
                   (validate-partial-target target)
                   (validate-partial-swap swap))))

(defun ensure-partial-context (context)
  "Return CONTEXT after checking the HTTP partial-response requirements."
  (check-type context clog-render:render-context)
  (unless (clog-render:render-context-request context)
    (invalid-partial :request-context-required))
  (unless (clog-render:render-context-application context)
    (invalid-partial :application-required))
  context)

(defun ensure-partial-descriptor-current (partial)
  "Revalidate mutable descriptor internals and return PARTIAL.

Although the public API exposes defensive readers, a Common Lisp caller can
still reach implementation slots deliberately. Revalidation at the sink keeps
that from turning descriptor mutation into an attribute or identity bypass."
  (check-type partial partial)
  (let* ((component (%partial-component partial))
         (actual-id
           (and (typep component 'clog-component:component)
                (clog-component:component-id component)))
         (actual-revision
           (and actual-id (clog-component:component-revision component))))
    (unless (and actual-id
                 (stringp (%partial-component-id partial))
                 (string= actual-id (%partial-component-id partial)))
      (invalid-partial :component-identity-mismatch))
    (unless (and (integerp (%partial-revision partial))
                 (not (minusp (%partial-revision partial)))
                 (integerp actual-revision)
                 (<= (%partial-revision partial) actual-revision))
      (invalid-partial :invalid-revision-snapshot))
    (validate-partial-target (%partial-target partial))
    (validate-partial-swap (%partial-swap partial))
    (unless (clog-component:mounted-p component)
      (invalid-partial :component-not-mounted))
    partial))

(defun ensure-session-component-visible (component context)
  "Require COMPONENT to be the exact object registered for CONTEXT's session."
  (let* ((request (clog-render:render-context-request context))
         (application (clog-render:render-context-application context))
         (store (clog-hypermedia:application-component-store application))
         (session-id (clog-http:request-session-id request))
         (owner (clog-component:component-owner-session-id component)))
    (unless (and store
                 (clog-component:component-store-p store)
                 (stringp session-id)
                 (stringp owner)
                 (string= session-id owner))
      (invalid-partial :component-not-visible))
    (handler-case
        (let ((registered
                (clog-component:load-component
                 store session-id (clog-component:component-id component))))
          (unless (eq registered component)
            (invalid-partial :component-not-visible)))
      (partial-error (condition) (error condition))
      (error () (invalid-partial :component-not-visible)))))

(defun ensure-partial-component-visible (partial context)
  "Require PARTIAL's component to be authorized for this render context."
  (ensure-partial-context context)
  (ensure-partial-descriptor-current partial)
  (let ((component (%partial-component partial)))
    (case (clog-component:component-scope component)
      (:application t)
      (:session (ensure-session-component-visible component context))
      (otherwise (invalid-partial :unsupported-component-scope))))
  partial)

(defun normalize-partial-input (value)
  "Convert VALUE into a PARTIAL descriptor without rendering it."
  (cond
    ((partial-p value)
     (ensure-partial-descriptor-current value))
    ((typep value 'clog-component:component)
     (make-partial value))
    (t
     (invalid-partial :invalid-partial-input))))

(defun html-escape-attribute-to-stream (value stream)
  "Write VALUE escaped for a double-quoted HTML attribute."
  (loop for character across value
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (#\" (write-string "&quot;" stream))
             (#\' (write-string "&#39;" stream))
             (otherwise (write-char character stream)))))

(defun render-partial-descriptor (partial context)
  "Render one already validated PARTIAL into a pure `<hx-partial>` wrapper."
  (ensure-partial-component-visible partial context)
  (let ((html
          (clog-render:render (%partial-component partial) context)))
    (with-output-to-string (stream)
      (write-string "<hx-partial hx-target=\"" stream)
      (html-escape-attribute-to-stream (%partial-target partial) stream)
      (write-string "\" hx-swap=\"" stream)
      (html-escape-attribute-to-stream (%partial-swap partial) stream)
      (write-string "\">" stream)
      (write-string html stream)
      (write-string "</hx-partial>" stream))))

(defun render-partial
    (subject context &key (target nil target-supplied-p)
                          (swap nil swap-supplied-p))
  "Render SUBJECT as one HTMX 4 `<hx-partial>` string.

SUBJECT may be a component or an existing PARTIAL descriptor. TARGET/SWAP
options are accepted only for a component so descriptor metadata cannot be
silently shadowed at the sink."
  (let ((partial
          (cond
            ((partial-p subject)
             (when (or target-supplied-p swap-supplied-p)
               (invalid-partial :descriptor-options-conflict))
             subject)
            ((typep subject 'clog-component:component)
             (cond
               ((and target-supplied-p swap-supplied-p)
                (make-partial subject :target target :swap swap))
               (target-supplied-p
                (make-partial subject :target target))
               (swap-supplied-p
                (make-partial subject :swap swap))
               (t
                (make-partial subject))))
            (t
             (invalid-partial :invalid-partial-input)))))
    (render-partial-descriptor partial context)))

(defun indexed-partial (index partial)
  "Represent a reduction candidate as INDEX . PARTIAL."
  (cons index partial))

(defun newer-component-candidate-p (left-indexed right-indexed)
  "Return true when RIGHT should replace LEFT for the same component id."
  (let ((left (cdr left-indexed))
        (right (cdr right-indexed)))
    (or (> (%partial-revision right) (%partial-revision left))
        (and (= (%partial-revision right) (%partial-revision left))
             (> (car right-indexed) (car left-indexed))))))

(defun reduce-partials-by-component (indexed)
  "Keep the latest committed descriptor for each component identity."
  (let ((winners (make-hash-table :test #'equal)))
    (dolist (candidate indexed)
      (let* ((partial (cdr candidate))
             (id (%partial-component-id partial))
             (existing (gethash id winners)))
        (when (or (null existing)
                  (newer-component-candidate-p existing candidate))
          (setf (gethash id winners) candidate))))
    (let ((result nil))
      (maphash (lambda (id winner)
                 (declare (ignore id))
                 (push winner result))
               winners)
      (sort result #'< :key #'car))))

(defun reduce-partials-by-target (indexed)
  "Keep one final operation per target, with the later candidate winning."
  (let ((winners (make-hash-table :test #'equal)))
    (dolist (candidate indexed)
      (setf (gethash (%partial-target (cdr candidate)) winners) candidate))
    (let ((result nil))
      (maphash (lambda (target winner)
                 (declare (ignore target))
                 (push winner result))
               winners)
      (sort result #'< :key #'car))))

(defun finite-proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (and (listp value)
       (handler-case
           (integerp (list-length value))
         (type-error () nil))))

(defun normalize-and-authorize-partials (subjects context)
  "Normalize and authorize every input before any duplicate is discarded."
  (unless (finite-proper-list-p subjects)
    (invalid-partial :malformed-partial-list))
  (loop for subject in subjects
        for index from 0
        for partial = (normalize-partial-input subject)
        do (ensure-partial-component-visible partial context)
        collect (indexed-partial index partial)))

(defun render-partials (subjects context)
  "Return a pure multi-target HTMX response for SUBJECTS.

Each input is authorized before reduction. Same-component duplicates retain the
highest committed revision, with the later descriptor breaking equal-revision
ties. Duplicate target selectors then retain the final operation. The resulting
wrappers are ordered by their winning source position. An empty input returns
an explicit HTTP 204 no-content response; a non-empty result contains only
`<hx-partial>` wrappers and no implicit main content."
  (ensure-partial-context context)
  (if (null subjects)
      (clog-http:no-content-response)
      (let* ((authorized
               (normalize-and-authorize-partials subjects context))
             (by-component (reduce-partials-by-component authorized))
             (by-target (reduce-partials-by-target by-component))
             (body
               (with-output-to-string (stream)
                 (dolist (candidate by-target)
                   (write-string
                    (render-partial-descriptor (cdr candidate) context)
                    stream)))))
        (clog-http:html-response body))))
