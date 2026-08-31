;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime static action registry tests                 ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defclass hm-024-action-component (clog-hypermedia:component)
  ((value
    :initform 0
    :accessor hm-024-action-value)
   (authorized-p
    :initarg :authorized-p
    :initform t
    :accessor hm-024-action-authorized-p)))

(defclass hm-024-other-action-component (clog-hypermedia:component)
  ())

(defun hm-024-decode-delta (value)
  "Decode a strict decimal integer fixture value."
  (parse-integer value :junk-allowed nil))

(defun hm-024-always-authorized-p (component request-context)
  "Return true for macro-expansion and descriptor fixtures."
  (declare (ignore component request-context))
  t)

(defmethod clog-hypermedia:authorize-action-p
    ((component hm-024-action-component)
     (action (eql :hm-024-increment))
     request-context)
  (declare (ignore request-context))
  (hm-024-action-authorized-p component))

(clog-hypermedia:defaction
    (hm-024-action-component :hm-024-increment
     :external-name "hm-024-increment"
     :allowed-methods '(:delete :post)
     :parameter-decoder #'hm-024-decode-delta
     :requires-current t
     :documentation "Increment the HM-024 fixture by a decoded delta.")
    (component delta)
  (incf (hm-024-action-value component) delta))

(defun hm-024-make-component (&key (authorized-p t))
  "Return a mounted application-scoped action fixture component."
  (let ((component
          (make-instance 'hm-024-action-component
                         :scope :application
                         :authorized-p authorized-p)))
    (clog-hypermedia:mount-component component)
    component))

(defun hm-024-make-descriptor
    (external-name
     &key
       (symbol :hm-024-direct)
       (component-class 'hm-024-action-component)
       (allowed-methods '(:post))
       (parameter-decoder #'identity)
       authorize-function
       authorization-policy
       handler
       (requires-current-p nil)
       documentation)
  "Return a validated descriptor suitable for isolated registry tests."
  (clog-hypermedia:make-action-descriptor
   :symbol symbol
   :external-name external-name
   :component-class component-class
   :allowed-methods allowed-methods
   :parameter-decoder parameter-decoder
   :authorize-function authorize-function
   :authorization-policy authorization-policy
   :handler handler
   :requires-current-p requires-current-p
   :documentation documentation))

(defun hm-024-all-symbol-count ()
  "Return the number of distinct symbols reachable from all registered packages."
  (let ((symbols (make-hash-table :test #'eq)))
    (dolist (package (list-all-packages))
      (do-symbols (symbol package)
        (setf (gethash symbol symbols) t)))
    (hash-table-count symbols)))

(defun hm-024-uninterned-symbols (form)
  "Return distinct uninterned symbols recursively present in FORM."
  (let ((result nil))
    (labels ((walk (value)
               (cond
                 ((symbolp value)
                  (when (null (symbol-package value))
                    (pushnew value result :test #'eq)))
                 ((consp value)
                  (walk (car value))
                  (walk (cdr value))))))
      (walk form))
    result))

(test action-registry/descriptor/static-metadata-and-protocol-functions
  (let* ((component (hm-024-make-component))
         (descriptor
           (clog-hypermedia:find-action
            component
            "hm-024-increment")))
    (is (clog-hypermedia:action-descriptor-p descriptor))
    (is (eq :hm-024-increment
            (clog-hypermedia:action-descriptor-symbol descriptor)))
    (is (string= "hm-024-increment"
                 (clog-hypermedia:action-descriptor-external-name descriptor)))
    (is (eq 'hm-024-action-component
            (clog-hypermedia:action-descriptor-component-class descriptor)))
    (is (equal '(:post :delete)
               (clog-hypermedia:action-descriptor-allowed-methods descriptor)))
    (is (eq :component-generic
            (clog-hypermedia:action-descriptor-authorization-policy descriptor)))
    (is-true
     (clog-hypermedia:action-descriptor-requires-current-p descriptor))
    (is (string=
         "Increment the HM-024 fixture by a decoded delta."
         (clog-hypermedia:action-descriptor-documentation descriptor)))
    (is (= 7
           (funcall
            (clog-hypermedia:action-descriptor-parameter-decoder descriptor)
            "7")))
    (is-true
     (funcall
      (clog-hypermedia:action-descriptor-authorize-function descriptor)
      component
      :request-context))
    (is (= 7
           (funcall
            (clog-hypermedia:action-descriptor-handler descriptor)
            component
            7)))
    (is (= 7 (hm-024-action-value component)))
    (setf (hm-024-action-authorized-p component) nil)
    (is-false
     (funcall
      (clog-hypermedia:action-descriptor-authorize-function descriptor)
      component
      :request-context))))

(test action-registry/descriptor/defensive-copy-and-validation
  (let* ((external-name (copy-seq "hm-024-copy"))
         (documentation (copy-seq "copied documentation"))
         (methods (list :delete :post))
         (descriptor
           (hm-024-make-descriptor
            external-name
            :allowed-methods methods
            :documentation documentation)))
    (setf (char external-name 0) #\X
          (char documentation 0) #\X
          (first methods) :get)
    (is (string= "hm-024-copy"
                 (clog-hypermedia:action-descriptor-external-name descriptor)))
    (is (string= "copied documentation"
                 (clog-hypermedia:action-descriptor-documentation descriptor)))
    (is (equal '(:post :delete)
               (clog-hypermedia:action-descriptor-allowed-methods descriptor)))
    (let ((returned-name
            (clog-hypermedia:action-descriptor-external-name descriptor))
          (returned-documentation
            (clog-hypermedia:action-descriptor-documentation descriptor))
          (returned-methods
            (clog-hypermedia:action-descriptor-allowed-methods descriptor)))
      (setf (char returned-name 0) #\Y
            (char returned-documentation 0) #\Y
            (first returned-methods) :get)
      (is (string= "hm-024-copy"
                   (clog-hypermedia:action-descriptor-external-name descriptor)))
      (is (string= "copied documentation"
                   (clog-hypermedia:action-descriptor-documentation descriptor)))
      (is (equal '(:post :delete)
                 (clog-hypermedia:action-descriptor-allowed-methods descriptor)))))
  (signals clog-hypermedia:invalid-action-definition
    (clog-hypermedia:make-action-descriptor
     :symbol nil
     :component-class 'hm-024-action-component))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor "Upper-Case"))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor "hm-024-empty-methods" :allowed-methods nil))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-duplicate-methods"
     :allowed-methods '(:post :post)))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-invalid-method"
     :allowed-methods '(:trace)))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-invalid-class"
     :component-class 'string))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-invalid-decoder"
     :parameter-decoder 42))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-policy-without-function"
     :authorization-policy :custom-function))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-function-with-generic-policy"
     :authorize-function #'hm-024-always-authorized-p
     :authorization-policy :component-generic))
  (signals clog-hypermedia:invalid-action-definition
    (hm-024-make-descriptor
     "hm-024-invalid-current"
     :requires-current-p :yes))
  (signals clog-hypermedia:invalid-action-definition
    (clog-hypermedia:make-action-registry :development-p :yes)))

(test action-registry/lookup/unknown-runtime-names-create-no-symbols
  (let* ((registry (clog-hypermedia:make-action-registry))
         (names
           (loop for index below 2000
                 collect
                 (format nil "hm-024-unknown-~4,'0d" index))))
    ;; Warm class/type lookup and the symbol-count helper before the measured loop.
    (is (null
         (clog-hypermedia:find-action
          'hm-024-action-component
          "hm-024-warm-up"
          :registry registry)))
    (hm-024-all-symbol-count)
    (let ((before (hm-024-all-symbol-count)))
      (dolist (name names)
        (is (null
             (clog-hypermedia:find-action
              'hm-024-action-component
              name
              :registry registry))))
      (is (= before (hm-024-all-symbol-count))
          "Unknown runtime action strings must not create Lisp symbols."))
    (is (null
         (clog-hypermedia:find-action
          'hm-024-action-component
          "INVALID"
          :registry registry)))
    (let ((condition
            (handler-case
                (progn
                  (clog-hypermedia:find-action
                   'hm-024-action-component
                   "hm-024-private-request-value"
                   :registry registry
                   :errorp t)
                  nil)
              (clog-hypermedia:action-not-found (value)
                value))))
      (is (typep condition 'clog-hypermedia:action-not-found))
      (is (string=
           "hm-024-private-request-value"
           (clog-hypermedia:action-not-found-external-name condition)))
      (is-false
       (search "hm-024-private-request-value"
               (princ-to-string condition))))))

(test action-registry/registration/duplicates-and-development-replacement
  (let* ((production (clog-hypermedia:make-action-registry))
         (first (hm-024-make-descriptor "hm-024-replace"))
         (second
           (hm-024-make-descriptor
            "hm-024-replace"
            :symbol :hm-024-replacement)))
    (multiple-value-bind (registered status)
        (clog-hypermedia:register-action first :registry production)
      (is (eq first registered))
      (is (eq :registered status)))
    (is (= 1 (clog-hypermedia:action-registry-count production)))
    (signals clog-hypermedia:action-registration-conflict
      (clog-hypermedia:register-action first :registry production))
    (signals clog-hypermedia:action-replacement-not-allowed
      (clog-hypermedia:register-action
       second
       :registry production
       :replace t))
    (signals clog-hypermedia:invalid-action-definition
      (clog-hypermedia:register-action
       second
       :registry production
       :replace :yes)))
  (let* ((development
           (clog-hypermedia:make-action-registry :development-p t))
         (first (hm-024-make-descriptor "hm-024-replace"))
         (second
           (hm-024-make-descriptor
            "hm-024-replace"
            :symbol :hm-024-replacement)))
    (is-true
     (clog-hypermedia:action-registry-development-p development))
    (clog-hypermedia:register-action first :registry development)
    (multiple-value-bind (registered status)
        (clog-hypermedia:register-action
         second
         :registry development
         :replace t)
      (is (eq second registered))
      (is (eq :replaced status)))
    (is (= 1 (clog-hypermedia:action-registry-count development)))
    (is (eq second
            (clog-hypermedia:find-action
             'hm-024-action-component
             "hm-024-replace"
             :registry development)))))

(test action-registry/registration/concurrent-unique-registrations-are-lossless
  (let* ((registry (clog-hypermedia:make-action-registry))
         (thread-count 4)
         (descriptors-per-thread 25)
         (descriptor-groups
           (loop for thread-index below thread-count
                 collect
                 (loop for descriptor-index below descriptors-per-thread
                       collect
                       (hm-024-make-descriptor
                        (format nil
                                "hm-024-thread-~D-action-~D"
                                thread-index
                                descriptor-index)))))
         (threads
           (loop for group in descriptor-groups
                 collect
                 (let ((captured-group group))
                   (bordeaux-threads:make-thread
                    (lambda ()
                      (dolist (descriptor captured-group)
                        (clog-hypermedia:register-action
                         descriptor
                         :registry registry)))
                    :name "hm-024-action-register")))))
    (dolist (thread threads)
      (bordeaux-threads:join-thread thread))
    (is (= (* thread-count descriptors-per-thread)
           (clog-hypermedia:action-registry-count registry)))))

(test action-registry/method-policy/default-post-and-enumeration
  (let ((descriptor (hm-024-make-descriptor "hm-024-method-default")))
    (is (equal '(:post)
               (clog-hypermedia:action-descriptor-allowed-methods descriptor)))
    (is-true
     (clog-hypermedia:action-method-allowed-p descriptor :post))
    (is-false
     (clog-hypermedia:action-method-allowed-p descriptor :get))
    (is-false
     (clog-hypermedia:action-method-allowed-p descriptor "GET"))
    (signals clog-hypermedia:invalid-action-definition
      (clog-hypermedia:action-method-allowed-p
       descriptor
       "GET"
       :errorp t))
    (let ((condition
            (handler-case
                (progn
                  (clog-hypermedia:action-method-allowed-p
                   descriptor
                   :get
                   :errorp t)
                  nil)
              (clog-hypermedia:action-method-not-allowed (value)
                value))))
      (is (typep condition
                 'clog-hypermedia:action-method-not-allowed))
      (is (eq :get
              (clog-hypermedia:action-method-not-allowed-method condition)))
      (is (equal '(:post)
                 (clog-hypermedia:action-method-not-allowed-allowed-methods
                  condition)))))
  (let ((descriptor
          (hm-024-make-descriptor
           "hm-024-method-explicit"
           :allowed-methods '(:delete :get :post))))
    (is (equal '(:get :post :delete)
               (clog-hypermedia:action-descriptor-allowed-methods descriptor)))
    (is-true
     (clog-hypermedia:action-method-allowed-p descriptor :get))
    (is-true
     (clog-hypermedia:action-method-allowed-p descriptor :delete))
    (is-false
     (clog-hypermedia:action-method-allowed-p descriptor :patch))))

(test action-registry/list/deterministic-and-class-filtered
  (let* ((registry (clog-hypermedia:make-action-registry))
         (zeta (hm-024-make-descriptor "zeta"))
         (alpha (hm-024-make-descriptor "alpha"))
         (other
           (hm-024-make-descriptor
            "middle"
            :component-class 'hm-024-other-action-component)))
    (dolist (descriptor (list zeta other alpha))
      (clog-hypermedia:register-action descriptor :registry registry))
    (is (equal
         '("alpha" "zeta" "middle")
         (mapcar #'clog-hypermedia:action-descriptor-external-name
                 (clog-hypermedia:list-actions :registry registry))))
    (is (equal
         '("alpha" "zeta")
         (mapcar
          #'clog-hypermedia:action-descriptor-external-name
          (clog-hypermedia:list-actions
           :registry registry
           :component-class 'hm-024-action-component))))))

(test action-registry/defaction/macroexpansion-is-deterministic-and-package-stable
  (let* ((form
           '(clog-hypermedia:defaction
                (hm-024-action-component :hm-024-snapshot
                 :external-name "hm-024-snapshot"
                 :allowed-methods (:delete :post)
                 :parameter-decoder #'identity
                 :authorize #'hm-024-always-authorized-p
                 :requires-current t
                 :documentation "HM-024 snapshot")
                (component request)
              (list component request)))
         (first-expansion (macroexpand-1 form))
         (second-expansion (macroexpand-1 form))
         (expected
           '(progn
             (eval-when (:load-toplevel :execute)
               (clog-action:register-action
                (clog-action:make-action-descriptor
                 :symbol ':hm-024-snapshot
                 :external-name "hm-024-snapshot"
                 :component-class 'hm-024-action-component
                 :allowed-methods '(:post :delete)
                 :parameter-decoder #'identity
                 :authorize-function #'hm-024-always-authorized-p
                 :authorization-policy :custom-function
                 :handler
                 (lambda (clog-action::component clog-action::action-input)
                   (clog-component:handle-action
                    clog-action::component
                    ':hm-024-snapshot
                    clog-action::action-input))
                 :requires-current-p t
                 :documentation "HM-024 snapshot")
                :replace nil))
             (defmethod clog-component:handle-action
                 ((component hm-024-action-component)
                  (clog-action::resolved-action
                   (eql ':hm-024-snapshot))
                  request)
               "HM-024 snapshot"
               (declare (ignore clog-action::resolved-action))
               (list component request))
             ':hm-024-snapshot)))
    (is (equal first-expansion second-expansion)
        "DEFACTION must not generate unstable GENSYM identities.")
    (is (equal expected first-expansion)
        "DEFACTION expansion drifted from the static registry snapshot:~%~S"
        first-expansion)
    (is (null (hm-024-uninterned-symbols first-expansion))
        "DEFACTION must not capture uninterned package-sensitive symbols."))
  (signals clog-hypermedia:invalid-action-definition
    (macroexpand-1
     '(clog-hypermedia:defaction
          (hm-024-action-component :hm-024-invalid
           :allowed-methods (:post :trace))
          (component request)
        (list component request))))
  (signals clog-hypermedia:invalid-action-definition
    (macroexpand-1
     '(clog-hypermedia:defaction
          (hm-024-action-component :hm-024-invalid
           :requires-current dynamic-value)
          (component request)
        (list component request)))))
