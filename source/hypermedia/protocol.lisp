;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime component extension protocol                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-component
  (:export #:render-component
           #:handle-action
           #:authorize-action-p
           #:validate-action
           #:component-title
           #:component-assets
           #:after-mount
           #:before-unmount))

(defpackage #:clog-hypermedia
  (:import-from #:clog-component
                #:render-component
                #:handle-action
                #:authorize-action-p
                #:validate-action
                #:component-title
                #:component-assets
                #:after-mount
                #:before-unmount)
  (:export #:render-component
           #:handle-action
           #:authorize-action-p
           #:validate-action
           #:component-title
           #:component-assets
           #:after-mount
           #:before-unmount))

(in-package #:clog-component)

(defgeneric render-component (component context)
  (:documentation
   "Render COMPONENT using CONTEXT and return its representation.

Concrete component classes must define this method. The base COMPONENT method
signals RENDER-METHOD-MISSING rather than returning placeholder markup. HM-020
does not define the render-context type or acquire locks around rendering; the
renderer task owns that boundary."))

(defgeneric handle-action (component action request-context)
  (:documentation
   "Handle the already-resolved ACTION for COMPONENT using REQUEST-CONTEXT.

Concrete actions are introduced by the later static action-registry task. The
HM-020 :AROUND method acquires only COMPONENT's own lock, verifies the component
is mounted, and invokes the concrete method while that lock is held. This makes
a concurrent unmount wait until the action method returns and prevents an
action from starting after terminal unmount. Action methods must not call
TOUCH-COMPONENT recursively or acquire unrelated component locks in arbitrary
order; the later dispatcher records the committed revision after the handler."))

(defgeneric authorize-action-p (component action request-context)
  (:documentation
   "Return true when COMPONENT permits ACTION for REQUEST-CONTEXT.

The base method returns true only as a protocol default. The later action layer
must perform authorization for every invocation and may specialize this generic
or attach an explicit action-descriptor authorization function."))

(defgeneric validate-action (component action request-context)
  (:documentation
   "Validate ACTION input for COMPONENT and REQUEST-CONTEXT.

The base method returns NIL and does not modify component state. Specialized
methods may signal typed validation conditions before HANDLE-ACTION mutates
state."))

(defgeneric component-title (component context)
  (:documentation
   "Return COMPONENT's optional page/section title for CONTEXT, or NIL.

The base method returns NIL and is pure."))

(defgeneric component-assets (component context)
  (:documentation
   "Return COMPONENT's declared local asset descriptors for CONTEXT, or NIL.

The base method returns NIL. Component core remains transport independent and
does not inspect HTMX metadata."))

(defgeneric after-mount (component context)
  (:documentation
   "Run COMPONENT's post-registration lifecycle hook with CONTEXT.

The base method is a no-op. MOUNT-COMPONENT deliberately does not call this
hook because the frozen lifecycle order requires component-store registration
between the mount transition and AFTER-MOUNT. The later store/application layer
owns that orchestration and must call this hook at most once."))

(defgeneric before-unmount (component context)
  (:documentation
   "Run COMPONENT's pre-removal lifecycle hook with CONTEXT.

The base method is a no-op. The later registry/composition layer owns recursive
child cleanup and the rule that hook failure must not prevent registry removal."))

(defmethod render-component ((instance component) context)
  "Signal RENDER-METHOD-MISSING for an unspecialized base COMPONENT."
  (declare (ignore context))
  (error 'render-method-missing
         :reason :render-method-missing
         :component-id (component-id instance)
         :class (class-name (class-of instance))))

(defmethod handle-action :around ((instance component) action request-context)
  "Serialize direct action handling and reject inactive components."
  (declare (ignore action request-context))
  (bordeaux-threads:with-lock-held ((component-lock instance))
    (unless (eq :mounted (%component-lifecycle-state instance))
      (lifecycle-error instance
                       :handle-action
                       :component-not-mounted
                       'component-not-mounted))
    (call-next-method)))

(defmethod authorize-action-p ((instance component) action request-context)
  "Default protocol authorization policy for a component class."
  (declare (ignore instance action request-context))
  t)

(defmethod validate-action ((instance component) action request-context)
  "Default protocol validation performs no state change and returns NIL."
  (declare (ignore instance action request-context))
  nil)

(defmethod component-title ((instance component) context)
  "Default component title is NIL."
  (declare (ignore instance context))
  nil)

(defmethod component-assets ((instance component) context)
  "Default component asset contribution is empty."
  (declare (ignore instance context))
  nil)

(defmethod after-mount ((instance component) context)
  "Default post-mount hook is a no-op."
  (declare (ignore instance context))
  nil)

(defmethod before-unmount ((instance component) context)
  "Default pre-unmount hook is a no-op."
  (declare (ignore instance context))
  nil)
