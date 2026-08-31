;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic asset and page-shell tests      ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(in-suite clog-hypermedia-tests)

(defun hm-014-count-substring (needle haystack)
  "Return the number of non-overlapping NEEDLE occurrences in HAYSTACK."
  (loop with count = 0
        with start = 0
        for position = (search needle haystack :start2 start)
        while position
        do (incf count)
           (setf start (+ position (length needle)))
        finally (return count)))

(defun hm-014-position-before-p (left right text)
  "Return true when LEFT occurs before RIGHT in TEXT."
  (let ((left-position (search left text))
        (right-position (search right text)))
    (and left-position
         right-position
         (< left-position right-position))))

(defun hm-014-request-context (&optional nonce)
  "Create a minimal request context carrying optional CSP NONCE."
  (clog-hypermedia:make-request-context
   (make-request-env :method :get :path "/asset-render-test")
   :csp-nonce nonce))

(test assets/descriptor/is-defensive-and-local
  (let* ((url (copy-seq "/assets/application.js"))
         (key (copy-seq "application-script"))
         (descriptor
           (clog-hypermedia:make-asset
            :type :script
            :url url
            :defer-p t
            :nonce-required-p t
            :key key)))
    (setf (char url 1) #\X)
    (setf (char key 0) #\X)
    (is (string= "/assets/application.js"
                 (clog-hypermedia:asset-url descriptor)))
    (is (string= "application-script"
                 (clog-hypermedia:asset-key descriptor)))
    (let ((returned-url (clog-hypermedia:asset-url descriptor))
          (returned-key (clog-hypermedia:asset-key descriptor)))
      (setf (char returned-url 1) #\Y)
      (setf (char returned-key 0) #\Y)
      (is (string= "/assets/application.js"
                   (clog-hypermedia:asset-url descriptor)))
      (is (string= "application-script"
                   (clog-hypermedia:asset-key descriptor))))
    (is (eq :script (clog-hypermedia:asset-type descriptor)))
    (is-true (clog-hypermedia:asset-defer-p descriptor))
    (is-true (clog-hypermedia:asset-nonce-required-p descriptor)))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:make-asset
     :type :script
     :url "https://cdn.example.invalid/application.js"
     :key :external))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:make-asset
     :type :style
     :url "/assets/application.css"
     :defer-p t
     :key :style-with-script-option))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:make-asset
     :type :script
     :url "//cdn.example.invalid/application.js"
     :key :protocol-relative)))

(test assets/rendering/order-deduplication-and-conflict-detection
  (let* ((style
           (clog-hypermedia:make-asset
            :type :style
            :url "/assets/application.css"
            :key :application-style))
         (core
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/htmx.js"
            :defer-p t
            :nonce-required-p t
            :key :htmx-core))
         (duplicate-core
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/htmx.js"
            :defer-p t
            :nonce-required-p t
            :key :htmx-core))
         (alias-core
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/htmx.js"
            :defer-p t
            :nonce-required-p t
            :key :htmx-alias))
         (duplicate-alias-core
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/htmx.js"
            :defer-p t
            :nonce-required-p t
            :key :htmx-alias))
         (application
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/application.js"
            :defer-p t
            :key :application-script))
         (markup
           (clog-hypermedia:render-assets
            (list style
                  core
                  duplicate-core
                  alias-core
                  duplicate-alias-core
                  application)
            (hm-014-request-context "nonce-assets"))))
    (is (= 1 (hm-014-count-substring "/assets/htmx.js" markup)))
    (is (hm-014-position-before-p
         "/assets/application.css" "/assets/htmx.js" markup))
    (is (hm-014-position-before-p
         "/assets/htmx.js" "/assets/application.js" markup))
    (is (search "<link rel=\"stylesheet\"" markup))
    (is (search "<script src=\"/assets/htmx.js\" defer nonce=\"nonce-assets\">"
                markup)))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:render-assets
     (list
      (clog-hypermedia:make-asset
       :type :script :url "/assets/a.js" :defer-p t :key :same)
      (clog-hypermedia:make-asset
       :type :script :url "/assets/b.js" :defer-p t :key :same))))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:render-assets
     (list
      (clog-hypermedia:make-asset
       :type :script :url "/assets/same.js" :defer-p t :key :first)
      (clog-hypermedia:make-asset
       :type :script :url "/assets/same.js" :module-p t :key :second)))))

(test assets/rendering/nonce-propagates-only-when-declared
  (let* ((required
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/required.js"
            :defer-p t
            :nonce-required-p t
            :key :required))
         (ordinary
           (clog-hypermedia:make-asset
            :type :script
            :url "/assets/ordinary.js"
            :defer-p t
            :key :ordinary))
         (markup
           (clog-hypermedia:render-assets
            (list required ordinary)
            (hm-014-request-context "request-nonce"))))
    (is (= 1 (hm-014-count-substring "nonce=\"request-nonce\"" markup)))
    (is (search
         "<script src=\"/assets/required.js\" defer nonce=\"request-nonce\"></script>"
         markup))
    (is (search
         "<script src=\"/assets/ordinary.js\" defer></script>"
         markup)))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:render-assets
     (list
      (clog-hypermedia:make-asset
       :type :script
       :url "/assets/required.js"
       :nonce-required-p t
       :key :required))))
  (signals clog-hypermedia:asset-error
    (clog-hypermedia:render-assets
     (list
      (clog-hypermedia:make-asset
       :type :script
       :url "/assets/required.js"
       :nonce-required-p t
       :key :required))
     (hm-014-request-context "unsafe nonce"))))

(test assets/page-shell/offline-snapshot-and-local-resolution
  (let ((router (clog-hypermedia:make-router))
        (application nil))
    (clog-hypermedia:add-route
     router :get "/"
     (lambda (context)
       (setf (gethash "_csrf_token"
                      (clog-hypermedia:request-session context))
             "csrf-test")
       (clog-hypermedia:render-page
        application
        context
        "<main id=\"root\">ready</main>"
        :title "Offline & Ready"
        :sse-p t
        :websocket-p t)))
    (setf application
          (clog-hypermedia:make-hypermedia-application
           :name "hm014-offline"
           :router router
           :configuration (make-test-configuration)))
    (let* ((handler (clog-hypermedia:application-handler application))
           (response
             (funcall handler
                      (make-request-env :method :get :path "/")))
           (body (response-body-text response))
           (expected
             (format nil
                     "<!doctype html>~%<html lang=\"en\">~%<head>~%<meta charset=\"utf-8\">~%<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">~%<title>Offline &amp; Ready</title>~%<meta name=\"csrf-param\" content=\"_csrf_token\">~%<meta name=\"csrf-token\" content=\"csrf-test\">~%<script src=\"/_clog/static/vendor/htmx/4.0.0/htmx.min.js\" defer nonce=\"nonce-test\"></script>~%<script src=\"/_clog/static/vendor/htmx/4.0.0/hx-csp.min.js\" defer nonce=\"nonce-test\"></script>~%<script src=\"/_clog/static/vendor/htmx/4.0.0/hx-sse.min.js\" defer nonce=\"nonce-test\"></script>~%<script src=\"/_clog/static/vendor/htmx/4.0.0/hx-ws.min.js\" defer nonce=\"nonce-test\"></script>~%</head>~%<body>~%<main id=\"root\">ready</main>~%</body>~%</html>")))
      (is (= 200 (first response)))
      (is (string= expected body))
      (is (= 4 (hm-014-count-substring "nonce=\"nonce-test\"" body)))
      (is (hm-014-position-before-p "htmx.min.js" "hx-csp.min.js" body))
      (is (hm-014-position-before-p "hx-csp.min.js" "hx-sse.min.js" body))
      (is (hm-014-position-before-p "hx-sse.min.js" "hx-ws.min.js" body))
      (is-false (search "http://" body :test #'char-equal))
      (is-false (search "https://" body :test #'char-equal))
      (is-false (search "unpkg" body :test #'char-equal))
      (is-false (search "jsdelivr" body :test #'char-equal))
      (let ((asset-response
              (funcall
               handler
               (make-request-env
                :method :get
                :path
                "/_clog/static/vendor/htmx/4.0.0/htmx.min.js"))))
        (is (= 200 (first asset-response)))
        (is (pathnamep (third asset-response)))
        (is (null (clack-response-header asset-response :set-cookie)))))))

(test assets/page-shell/optional-assets-and-none-mode
  (let ((router (clog-hypermedia:make-router))
        (application nil))
    (clog-hypermedia:add-route
     router :get "/"
     (lambda (context)
       (setf (gethash "_csrf_token"
                      (clog-hypermedia:request-session context))
             "csrf-none")
       (clog-hypermedia:render-page
        application context "<main>no framework assets</main>")))
    (setf application
          (clog-hypermedia:make-hypermedia-application
           :router router
           :configuration
           (make-test-configuration
            :assets-mode :none
            :strict-csp-p nil
            :static-prefix nil
            :static-root nil)))
    (let* ((response
             (funcall
              (clog-hypermedia:application-handler application)
              (make-request-env :method :get :path "/")))
           (body (response-body-text response)))
      (is (= 200 (first response)))
      (is (search "<main>no framework assets</main>" body))
      (is-false (search "<script" body :test #'char-equal))
      (is-false (search "htmx" body :test #'char-equal)))))
