;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Immutable request-context tests                                        ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun case-variants (string)
  "Return every alphabetic upper/lower-case variant of STRING."
  (labels ((walk (index prefix)
             (if (= index (length string))
                 (list (coerce (reverse prefix) 'string))
                 (let ((character (char string index)))
                   (if (alpha-char-p character)
                       (nconc (walk (1+ index)
                                    (cons (char-downcase character) prefix))
                              (walk (1+ index)
                                    (cons (char-upcase character) prefix)))
                       (walk (1+ index) (cons character prefix)))))))
    (remove-duplicates (walk 0 nil) :test #'string=)))

(test request/header-case-property-and-get-query-model
  (let* ((env (make-request-env
               :method :get
               :path "/machines"
               :query-string "tag=one&tag=two&q=hello"
               :headers '(("HX-Request" . "true")
                          ("hX-rEqUeSt-TyPe" . "partial")
                          ("HX-Target" . "machine-card")
                          ("hx-trigger" . "refresh-button")
                          ("HX-Current-URL" . "http://localhost/machines"))))
         (context (clog-hypermedia:make-request-context env)))
    (is (eq :get (clog-hypermedia:request-method context)))
    (is (string= "/machines" (clog-hypermedia:request-path context)))
    (is (string= "one" (clog-hypermedia:query-param context "tag")))
    (is (equal '("one" "two")
               (clog-hypermedia:query-param-values context "tag")))
    (is (string= "hello" (clog-hypermedia:query-param context "q")))
    (is (null (clog-hypermedia:query-param context "missing")))
    (is (string= "fallback"
                 (clog-hypermedia:query-param context "missing" "fallback")))
    (dolist (variant (case-variants "hx-target"))
      (is (string= "machine-card"
                   (clog-hypermedia:request-header context variant))))
    (is (null (clog-hypermedia:request-header context "X-Missing")))
    (is-true (clog-hypermedia:htmx-request-p context))
    (is-true (clog-hypermedia:htmx-partial-request-p context))
    (is-false (clog-hypermedia:htmx-full-request-p context))
    (is (eq :partial (clog-hypermedia:htmx-request-type context)))
    (is (string= "machine-card"
                 (clog-hypermedia:htmx-request-target context)))
    (is (string= "refresh-button"
                 (clog-hypermedia:htmx-request-trigger context)))
    (is (string= "http://localhost/machines"
                 (clog-hypermedia:request-current-url context)))))

(test request/urlencoded-body-preserves-duplicates
  (let* ((env (make-request-env
               :method :post
               :body "name=alpha&tag=one&tag=two"
               :content-type "application/x-www-form-urlencoded"
               :headers '(("HX-Request" . "true"))))
         (context (clog-hypermedia:make-request-context
                   env :body-limit-bytes 1024)))
    (is (string= "alpha" (clog-hypermedia:form-param context "name")))
    (is (equal '("one" "two")
               (clog-hypermedia:form-param-values context "tag")))
    (is (string= "fallback"
                 (clog-hypermedia:form-param context "missing" "fallback")))))

(test request/multipart-body-preserves-duplicates
  (let* ((body (multipart-body '("name" . "alpha")
                               '("tag" . "one")
                               '("tag" . "two")))
         (env (make-request-env
               :method :post
               :body body
               :content-type "multipart/form-data; boundary=clog-boundary"))
         (context (clog-hypermedia:make-request-context
                   env :body-limit-bytes 4096)))
    (is (string= "alpha" (clog-hypermedia:form-param context "name")))
    (is (equal '("one" "two")
               (clog-hypermedia:form-param-values context "tag")))))

(test request/empty-body-is-modeled-without-parser-error
  (let* ((env (make-request-env
               :method :post
               :content-length 0
               :content-type "application/x-www-form-urlencoded"))
         (context (clog-hypermedia:make-request-context env)))
    (is (null (clog-hypermedia:form-param context "name")))
    (is (null (clog-hypermedia:form-param-values context "name")))))

(test request/body-size-limit-fails-closed
  (let ((env (make-request-env
              :method :post
              :body "name=alpha"
              :content-type "application/x-www-form-urlencoded")))
    (signals clog-hypermedia:request-body-too-large
      (clog-hypermedia:make-request-context env :body-limit-bytes 4))))

(test request/chunked-body-limit-is-enforced-while-reading
  (let ((env (make-request-env
              :method :post
              :body "name=alpha"
              :transfer-encoding "chunked"
              :content-type "application/x-www-form-urlencoded")))
    (remf env :content-length)
    (let ((context (clog-hypermedia:make-request-context
                    env :body-limit-bytes 4)))
      (signals clog-hypermedia:request-body-too-large
        (clog-hypermedia:form-param context "name")))))

(test request/malformed-content-type-signals-typed-condition
  (let* ((body (multipart-body '("name" . "alpha")))
         (env (make-request-env
               :method :post
               :body body
               :content-type "multipart/form-data; boundary"))
         (context (clog-hypermedia:make-request-context
                   env :body-limit-bytes 4096)))
    (signals clog-hypermedia:request-body-parse-error
      (clog-hypermedia:form-param context "name"))))

(test request/non-form-body-is-not-parsed-by-form-accessors
  (let* ((env (make-request-env
               :method :post
               :body "{ definitely-not-json }"
               :content-type "application/json"))
         (context (clog-hypermedia:make-request-context
                   env :body-limit-bytes 1024)))
    (is (null (clog-hypermedia:form-param context "anything")))))

(test request/source-env-remains-structurally-unchanged
  (let* ((env (make-request-env
               :method :post
               :path "/immutable"
               :query-string "q=one&q=two"
               :headers '(("X-Fixture" . "original"))
               :body "name=alpha"
               :content-type "application/x-www-form-urlencoded"))
         (original-headers (getf env :headers))
         (original-length (length env))
         (context (clog-hypermedia:make-request-context env)))
    ;; Force both query and body parsing paths.
    (is (equal '("one" "two")
               (clog-hypermedia:query-param-values context "q")))
    (is (string= "alpha" (clog-hypermedia:form-param context "name")))
    (is (= original-length (length env)))
    (is (null (getf env :query-parameters)))
    (is (null (getf env :body-parameters)))
    (is (string= "original" (gethash "X-Fixture" original-headers)))
    ;; Mutating a returned snapshot cannot mutate the caller's env or headers.
    (let ((snapshot (clog-hypermedia:request-env context)))
      (is (null (getf snapshot :raw-body)))
      (setf (getf snapshot :path-info) "/changed")
      (setf (gethash "x-fixture" (getf snapshot :headers)) "changed"))
    (is (string= "/immutable" (getf env :path-info)))
    (is (string= "original" (gethash "X-Fixture" original-headers)))))

(test request/lack-session-and-routing-metadata-are-extracted
  (let* ((session (make-hash-table :test 'equal))
         (route (list :route :machine-show))
         (env (make-request-env
               :path "/machines/42"
               :session session
               :session-id "session-42"))
         (context (clog-hypermedia:make-request-context
                   env
                   :request-id "request-7"
                   :route route
                   :path-params '((:machine-id . "42"))
                   :user 'fixture-user
                   :csp-nonce "nonce-7")))
    (is (eq session (clog-hypermedia:request-session context)))
    (is (string= "session-42" (clog-hypermedia:request-session-id context)))
    (is (string= "request-7" (clog-hypermedia:request-id context)))
    (is (eq route (clog-hypermedia:request-route context)))
    (is (string= "42" (clog-hypermedia:path-param context :machine-id)))
    (is (eq 'fixture-user (clog-hypermedia:request-user context)))
    (is (string= "nonce-7" (clog-hypermedia:request-csp-nonce context)))))

(test request/string-method-normalization-does-not-require-intern
  (let ((context (clog-hypermedia:make-request-context
                  (make-request-env :method "PATCH"))))
    (is (eq :patch (clog-hypermedia:request-method context)))))


(test request/context-construction-does-not-consume-body
  (let* ((env (make-request-env
               :method :post
               :body "name=alpha"
               :content-type "application/x-www-form-urlencoded"))
         (stream (getf env :raw-body))
         (context (clog-hypermedia:make-request-context env)))
    (is (= 0 (fixture-stream-position stream)))
    (is (string= "alpha" (clog-hypermedia:form-param context "name")))
    (is (> (fixture-stream-position stream) 0))))

(test request/unsupported-methods-fail-without-symbol-interning
  (signals clog-hypermedia:request-error
    (clog-hypermedia:make-request-context
     (make-request-env :method :definitely-not-http)))
  (signals clog-hypermedia:request-error
    (clog-hypermedia:make-request-context
     (make-request-env :method "DEFINITELY-NOT-HTTP"))))

(test request/preparsed-form-parameters-are-defensively-copied
  (let* ((env (make-request-env :method :post))
         (parameters (list (cons "tag" "one") (cons "tag" "two"))))
    (setf (getf env :body-parameters) parameters)
    (let ((context (clog-hypermedia:make-request-context env)))
      (setf (cdr (first parameters)) "changed")
      (is (equal '("one" "two")
                 (clog-hypermedia:form-param-values context "tag"))))))

(test request/public-metadata-accessors-do-not-expose-mutable-internals
  (let* ((context (clog-hypermedia:make-request-context
                   (make-request-env
                    :path "/immutable"
                    :headers '(("HX-Target" . "target-1")))
                   :request-id "request-1"
                   :path-params '((:item-id . "42"))
                   :csp-nonce "nonce-1"))
         (path (clog-hypermedia:request-path context))
         (request-id (clog-hypermedia:request-id context))
         (target (clog-hypermedia:htmx-request-target context))
         (params (clog-hypermedia:request-path-params context)))
    (setf (char path 1) #\X)
    (setf (char request-id 0) #\X)
    (setf (char target 0) #\X)
    (setf (cdr (first params)) "changed")
    (is (string= "/immutable" (clog-hypermedia:request-path context)))
    (is (string= "request-1" (clog-hypermedia:request-id context)))
    (is (string= "target-1" (clog-hypermedia:htmx-request-target context)))
    (is (string= "42" (clog-hypermedia:path-param context :item-id)))
    (is (string= "nonce-1" (clog-hypermedia:request-csp-nonce context)))))

(test request/missing-accept-header-does-not-change-public-header-model
  (let ((env (make-request-env :path "/no-accept")))
    (remhash "accept" (getf env :headers))
    (let ((context (clog-hypermedia:make-request-context env)))
      (is (string= "/no-accept" (clog-hypermedia:request-path context)))
      (is (null (clog-hypermedia:request-header context "Accept"))))))
