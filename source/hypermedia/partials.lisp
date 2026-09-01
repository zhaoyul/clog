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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; HM-033 typed action-result contract and response mapping              ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-action
  (:export
   #:invalid-action-result
   #:action-result #:action-result-p #:make-action-result
   #:action-result-primary-component
   #:action-result-invalidated-components
   #:action-result-effects #:action-result-response-headers
   #:action-result-push-url #:action-result-replace-url
   #:action-result-redirect-url #:action-result-flash #:action-result-status
   #:render-self #:render-components #:no-render
   #:redirect-to #:push-url #:replace-url #:with-effect))

(defpackage #:clog-hypermedia
  (:import-from #:clog-action
                #:invalid-action-result
                #:action-result #:action-result-p #:make-action-result
                #:action-result-primary-component
                #:action-result-invalidated-components
                #:action-result-effects #:action-result-response-headers
                #:action-result-push-url #:action-result-replace-url
                #:action-result-redirect-url #:action-result-flash
                #:action-result-status
                #:render-self #:render-components #:no-render
                #:redirect-to #:push-url #:replace-url #:with-effect)
  (:export
   #:invalid-action-result
   #:action-result #:action-result-p #:make-action-result
   #:action-result-primary-component
   #:action-result-invalidated-components
   #:action-result-effects #:action-result-response-headers
   #:action-result-push-url #:action-result-replace-url
   #:action-result-redirect-url #:action-result-flash #:action-result-status
   #:render-self #:render-components #:no-render
   #:redirect-to #:push-url #:replace-url #:with-effect
   #:action-result->response))

(in-package #:clog-action)

(defparameter +action-result-reserved-response-headers+
  '(:content-type :content-length :location
    :hx-trigger :hx-redirect :hx-location :hx-refresh
    :hx-push-url :hx-replace-url)
  "Response headers owned by the HM-033 mapper rather than business actions.")

(defun action-result-proper-list-p (value)
  "Return true when VALUE is a finite proper list."
  (and (listp value)
       (handler-case
           (integerp (list-length value))
         (type-error () nil))))

(defun copy-action-result-value (value)
  "Return a defensive copy for one mutable action-result metadata value."
  (cond
    ((stringp value) (copy-seq value))
    ((consp value) (copy-tree value))
    ((vectorp value) (copy-seq value))
    (t value)))

(defun copy-action-result-list (values)
  "Return a fresh action-result list with mutable metadata values copied."
  (and values (mapcar #'copy-action-result-value values)))

(defun validate-action-result-header-list (headers)
  "Validate business response HEADERS and return a defensively owned copy."
  (unless (action-result-proper-list-p headers)
    (error 'invalid-action-result :reason :malformed-response-headers))
  (unless (evenp (length headers))
    (error 'invalid-action-result :reason :malformed-response-headers))
  (loop for (name value) on headers by #'cddr
        do (unless (keywordp name)
             (error 'invalid-action-result :reason :invalid-response-header-name))
           (when (member name +action-result-reserved-response-headers+ :test #'eq)
             (error 'invalid-action-result :reason :reserved-response-header)))
  (clog-http:make-response :headers headers :body "" :kind :html)
  (loop for (name value) on headers by #'cddr
        append (list name (copy-action-result-value value))))

(defun validate-action-result-primary (primary)
  "Return PRIMARY when it is a supported action-result primary designator."
  (unless (or (null primary)
              (eq primary :current)
              (typep primary 'clog-component:component))
    (error 'invalid-action-result :reason :invalid-primary-component))
  primary)

(defun validate-action-result-partials (subjects)
  "Return a fresh list of component/partial SUBJECTS accepted by HM-033."
  (unless (action-result-proper-list-p subjects)
    (error 'invalid-action-result :reason :malformed-invalidated-components))
  (dolist (subject subjects)
    (unless (or (typep subject 'clog-component:component)
                (clog-partials:partial-p subject))
      (error 'invalid-action-result :reason :invalid-partial-subject)))
  (copy-list subjects))

(defun validate-action-result-effects (effects)
  "Return a fresh finite effect list without interpreting future typed effects."
  (unless (action-result-proper-list-p effects)
    (error 'invalid-action-result :reason :malformed-effects))
  (copy-action-result-list effects))

(defun validate-action-result-url-metadata (url reason)
  "Return an owned URL string for later same-origin sink validation."
  (when url
    (unless (and (stringp url) (plusp (length url)) (<= (length url) 4096))
      (error 'invalid-action-result :reason reason))
    (copy-seq url)))

(defun validate-action-result-flash (flash)
  "Return an owned bounded flash string, or NIL."
  (when flash
    (unless (and (stringp flash) (plusp (length flash)) (<= (length flash) 1024))
      (error 'invalid-action-result :reason :invalid-flash))
    (copy-seq flash)))

(defun validate-action-result-status (status)
  "Return a successful HTTP STATUS accepted by the action-result contract."
  (unless (and (integerp status) (<= 200 status 299))
    (error 'invalid-action-result :reason :invalid-action-result-status))
  status)

(defun validate-action-result-combination
    (primary partials push-url replace-url redirect-url status)
  "Reject mutually exclusive action-result body/navigation combinations."
  (when (and push-url replace-url)
    (error 'invalid-action-result :reason :competing-history-mutations))
  (when (and redirect-url
             (or primary partials push-url replace-url))
    (error 'invalid-action-result :reason :redirect-with-fragment-or-history))
  (when (and (= status 204) (or primary partials redirect-url))
    (error 'invalid-action-result :reason :no-content-with-body))
  t)

(defun validate-action-result (result)
  "Revalidate RESULT at the response sink and return RESULT."
  (unless (action-result-p result)
    (error 'invalid-action-result :reason :action-result-required))
  (let* ((primary
           (validate-action-result-primary (%action-result-primary-component result)))
         (partials
           (validate-action-result-partials
            (%action-result-invalidated-components result)))
         (effects
           (validate-action-result-effects (%action-result-effects result)))
         (headers
           (validate-action-result-header-list
            (%action-result-response-headers result)))
         (push-url
           (validate-action-result-url-metadata
            (%action-result-push-url result) :invalid-push-url))
         (replace-url
           (validate-action-result-url-metadata
            (%action-result-replace-url result) :invalid-replace-url))
         (redirect-url
           (validate-action-result-url-metadata
            (%action-result-redirect-url result) :invalid-redirect-url))
         (flash (validate-action-result-flash (%action-result-flash result)))
         (status (validate-action-result-status (%action-result-status result))))
    (declare (ignore effects headers flash))
    (validate-action-result-combination
     primary partials push-url replace-url redirect-url status)
    result))

(defun make-action-result
    (&key (primary-component :current)
          invalidated-components effects response-headers
          push-url replace-url redirect-url flash (status 200))
  "Create a validated defensive HM-033 ACTION-RESULT value."
  (let* ((primary (validate-action-result-primary primary-component))
         (partials (validate-action-result-partials invalidated-components))
         (effects (validate-action-result-effects effects))
         (headers (validate-action-result-header-list response-headers))
         (push-url (validate-action-result-url-metadata push-url :invalid-push-url))
         (replace-url
           (validate-action-result-url-metadata replace-url :invalid-replace-url))
         (redirect-url
           (validate-action-result-url-metadata redirect-url :invalid-redirect-url))
         (flash (validate-action-result-flash flash))
         (status (validate-action-result-status status)))
    (validate-action-result-combination
     primary partials push-url replace-url redirect-url status)
    (%make-action-result
     :primary-component primary
     :invalidated-components partials
     :effects effects
     :response-headers headers
     :push-url push-url
     :replace-url replace-url
     :redirect-url redirect-url
     :flash flash
     :status status)))

(defun action-result-primary-component (result)
  "Return RESULT's primary component designator."
  (check-type result action-result)
  (%action-result-primary-component result))

(defun action-result-invalidated-components (result)
  "Return a fresh list of RESULT's additional partial subjects."
  (check-type result action-result)
  (copy-list (%action-result-invalidated-components result)))

(defun action-result-effects (result)
  "Return a defensive copy of RESULT's browser effect declarations."
  (check-type result action-result)
  (copy-action-result-list (%action-result-effects result)))

(defun action-result-response-headers (result)
  "Return a defensive copy of RESULT's business response headers."
  (check-type result action-result)
  (loop for (name value) on (%action-result-response-headers result) by #'cddr
        append (list name (copy-action-result-value value))))

(defun action-result-push-url (result)
  "Return an owned copy of RESULT's push URL, or NIL."
  (check-type result action-result)
  (let ((value (%action-result-push-url result)))
    (and value (copy-seq value))))

(defun action-result-replace-url (result)
  "Return an owned copy of RESULT's replace URL, or NIL."
  (check-type result action-result)
  (let ((value (%action-result-replace-url result)))
    (and value (copy-seq value))))

(defun action-result-redirect-url (result)
  "Return an owned copy of RESULT's HTMX redirect URL, or NIL."
  (check-type result action-result)
  (let ((value (%action-result-redirect-url result)))
    (and value (copy-seq value))))

(defun action-result-flash (result)
  "Return an owned copy of RESULT's toast/flash message, or NIL."
  (check-type result action-result)
  (let ((value (%action-result-flash result)))
    (and value (copy-seq value))))

(defun action-result-status (result)
  "Return RESULT's HTTP status."
  (check-type result action-result)
  (%action-result-status result))

(defun render-self ()
  "Declare that the current action component should be rendered."
  (make-action-result))

(defun no-render ()
  "Declare successful action completion with no response body."
  (make-action-result :primary-component nil :status 204))

(defun render-components (&rest components)
  "Declare a pure multi-target partial response for COMPONENTS."
  (if components
      (make-action-result
       :primary-component nil
       :invalidated-components components)
      (no-render)))

(defun redirect-to (url)
  "Declare an HTMX same-origin redirect with no fragment body."
  (make-action-result :primary-component nil :redirect-url url))

(defun push-url (url)
  "Render the current component and push URL into browser history."
  (make-action-result :push-url url))

(defun replace-url (url)
  "Render the current component and replace the browser history URL."
  (make-action-result :replace-url url))

(defun with-effect (effect result)
  "Return a new RESULT with EFFECT appended without mutating RESULT."
  (validate-action-result result)
  (make-action-result
   :primary-component (%action-result-primary-component result)
   :invalidated-components (%action-result-invalidated-components result)
   :effects (append (%action-result-effects result) (list effect))
   :response-headers (%action-result-response-headers result)
   :push-url (%action-result-push-url result)
   :replace-url (%action-result-replace-url result)
   :redirect-url (%action-result-redirect-url result)
   :flash (%action-result-flash result)
   :status (%action-result-status result)))

(defun valid-hm-025-action-result-p (result component)
  "Accept structurally valid HM-033 results compatible with CURRENT COMPONENT."
  (and (action-result-p result)
       (handler-case
           (progn
             (validate-action-result result)
             (let ((primary (%action-result-primary-component result)))
               (or (null primary)
                   (eq primary :current)
                   (eq primary component))))
         (invalid-action-result () nil)
         (error () nil))))

(in-package #:clog-hypermedia)

(defun hm-033-render-context (application current-component request-context)
  "Create the fragment render context shared by primary and partial mapping."
  (check-type application hypermedia-application)
  (check-type current-component clog-component:component)
  (check-type request-context clog-http:request-context)
  (clog-render:make-render-context
   :request request-context
   :application application
   :mode :fragment
   :primary-component-id (clog-component:component-id current-component)))

(defun hm-033-resolve-primary-component (result current-component)
  "Resolve RESULT's primary designator without permitting a foreign primary."
  (let ((primary (clog-action::%action-result-primary-component result)))
    (cond
      ((null primary) nil)
      ((eq primary :current) current-component)
      ((eq primary current-component) current-component)
      (t
       (error 'clog-action:invalid-action-result
              :reason :foreign-primary-component)))))

(defun hm-033-subject-component-id (subject)
  "Return SUBJECT's component id for primary/partial duplicate suppression."
  (cond
    ((typep subject 'clog-component:component)
     (clog-component:component-id subject))
    ((clog-partials:partial-p subject)
     (clog-partials:partial-component-id subject))
    (t nil)))

(defun hm-033-filter-primary-from-partials (subjects primary)
  "Return SUBJECTS without entries that target PRIMARY's component identity."
  (if (null primary)
      (copy-list subjects)
      (let ((primary-id (clog-component:component-id primary)))
        (remove-if
         (lambda (subject)
           (let ((id (hm-033-subject-component-id subject)))
             (and id (string= id primary-id))))
         subjects))))

(defun hm-033-render-action-body
    (result application current-component request-context)
  "Return RESULT's deterministic fragment/partial response body string."
  (let* ((render-context
           (hm-033-render-context application current-component request-context))
         (primary
           (hm-033-resolve-primary-component result current-component))
         (subjects
           (hm-033-filter-primary-from-partials
            (clog-action::%action-result-invalidated-components result)
            primary))
         (primary-body
           (and primary (clog-render:render primary render-context)))
         (partial-body
           (when subjects
             (clog-http:response-body
              (clog-partials:render-partials subjects render-context)))))
    (cond
      ((and primary-body partial-body)
       (concatenate 'string primary-body partial-body))
      (primary-body primary-body)
      (partial-body partial-body)
      (t ""))))

(defun hm-033-base-action-response
    (result application current-component request-context)
  "Build RESULT's body/status/custom-header response before HTMX metadata."
  (let ((status (clog-action::%action-result-status result))
        (headers (clog-action::%action-result-response-headers result)))
    (if (= status 204)
        (clog-http:no-content-response :status status :headers headers)
        (clog-http:html-response
         (hm-033-render-action-body
          result application current-component request-context)
         :status status
         :headers headers))))

(defun hm-033-apply-action-result-headers (response result)
  "Apply navigation, toast and effect metadata through typed HTMX adapters."
  (let ((push-url (clog-action::%action-result-push-url result))
        (replace-url (clog-action::%action-result-replace-url result))
        (redirect-url (clog-action::%action-result-redirect-url result))
        (flash (clog-action::%action-result-flash result))
        (effects (clog-action::%action-result-effects result)))
    (when push-url
      (clog-htmx:set-hx-push-url response push-url))
    (when replace-url
      (clog-htmx:set-hx-replace-url response replace-url))
    (when redirect-url
      (clog-htmx:set-hx-redirect response redirect-url))
    (when flash
      (clog-htmx:set-hx-trigger response "clog:toast" flash))
    (when effects
      (clog-htmx:set-hx-trigger response "clog:effects" effects)))
  response)

(defun action-result->response
    (result application current-component request-context)
  "Map declarative ACTION-RESULT to one validated framework RESPONSE."
  (clog-action::validate-action-result result)
  (check-type application hypermedia-application)
  (check-type current-component clog-component:component)
  (check-type request-context clog-http:request-context)
  (hm-033-apply-action-result-headers
   (hm-033-base-action-response
    result application current-component request-context)
   result))

(defun hm-025-response-from-result (result application component context)
  "HM-033 bridge: map the expanded typed result through the unified response sink."
  (action-result->response result application component context))
