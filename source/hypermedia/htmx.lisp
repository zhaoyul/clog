;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime progressive HTMX / no-JavaScript policy     ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-htmx
  (:export
   #:hx-target
   #:hx-trigger
   #:hx-current-url
   #:set-hx-trigger
   #:set-hx-location
   #:set-hx-redirect
   #:set-hx-refresh
   #:set-hx-push-url
   #:set-hx-replace-url))

(defpackage #:clog-hypermedia
  (:import-from #:clog-htmx
                #:hx-target
                #:hx-trigger
                #:hx-current-url
                #:set-hx-trigger
                #:set-hx-location
                #:set-hx-redirect
                #:set-hx-refresh
                #:set-hx-push-url
                #:set-hx-replace-url)
  (:export
   #:hx-target
   #:hx-trigger
   #:hx-current-url
   #:set-hx-trigger
   #:set-hx-location
   #:set-hx-redirect
   #:set-hx-refresh
   #:set-hx-push-url
   #:set-hx-replace-url))

(in-package #:clog-htmx)

(defun hx-target (context)
  "Return a defensive copy of the normalized HX-Target request header, or NIL.

CONTEXT is the immutable CLOG request context built by the HTTP kernel. Header
name case folding and ownership are therefore inherited from HM-010 instead of
being reimplemented at the HTMX boundary."
  (clog-http:htmx-request-target context))

(defun hx-trigger (context)
  "Return a defensive copy of the normalized HX-Trigger request header, or NIL."
  (clog-http:htmx-request-trigger context))

(defun hx-current-url (context)
  "Return a defensive copy of the normalized HX-Current-URL request header, or NIL."
  (clog-http:request-current-url context))

(defun response-without-header (headers name)
  "Return a fresh alternating header list with every NAME entry removed."
  (loop for (header value) on headers by #'cddr
        unless (eq header name)
          append (list header value)))

(defun validate-hx-header-value (name value)
  "Validate one HTMX response header through the HM-011 response boundary."
  (unless (stringp value)
    (error 'clog-http:invalid-response-header
           :reason :non-string-htmx-header-value
           :name name
           :value value))
  ;; MAKE-RESPONSE owns the canonical header-name/value and CRLF validation.
  (clog-http:make-response :headers (list name value) :body "" :kind :html)
  value)

(defun set-hx-response-header (response name value)
  "Replace NAME on RESPONSE with one validated string VALUE and return RESPONSE."
  (check-type response clog-http:response)
  (let ((stored-value (copy-seq (validate-hx-header-value name value))))
    (setf (clog-http:response-headers response)
          (append (response-without-header
                   (clog-http:response-headers response)
                   name)
                  (list name stored-value))))
  response)

(defun validate-hx-local-url (url)
  "Return an owned local URL after applying the HM-011 redirect safety policy.

HTMX navigation/history headers are same-origin by default. Reusing
REDIRECT-RESPONSE keeps protocol-relative paths, external origins, backslashes
and control characters governed by the same fail-closed URL policy as ordinary
HTTP redirects."
  (clog-http:redirect-response url :status 303)
  (copy-seq url))

(defun hx-event-name-valid-p (name)
  "Return true for a bounded non-empty event NAME without header controls."
  (and (stringp name)
       (plusp (length name))
       (<= (length name) 256)
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              name)))

(defun validate-hx-event-name (name)
  "Return an owned event NAME or signal INVALID-RESPONSE-HEADER."
  (unless (hx-event-name-valid-p name)
    (error 'clog-http:invalid-response-header
           :reason :invalid-hx-trigger-event-name
           :name :hx-trigger
           :value name))
  (copy-seq name))

(defun json-object-text-p (value)
  "Return true when VALUE is text whose first/last non-space characters are braces."
  (when (stringp value)
    (let ((start (position-if-not
                  (lambda (character)
                    (member character '(#\Space #\Tab #\Newline #\Return)))
                  value))
          (end (position-if-not
                (lambda (character)
                  (member character '(#\Space #\Tab #\Newline #\Return)))
                value :from-end t)))
      (and start end
           (char= (char value start) #\{)
           (char= (char value end) #\})))))

(defun valid-hx-event-alist-p (events)
  "Return true when EVENTS is a Yason object alist with string keys."
  (and (listp events)
       (every (lambda (entry)
                (and (consp entry) (stringp (car entry))))
              events)))

(defun signal-invalid-hx-trigger-json (value)
  "Signal the typed response-header condition for malformed HX-Trigger JSON."
  (error 'clog-http:invalid-response-header
         :reason :invalid-hx-trigger-json
         :name :hx-trigger
         :value value))

(defun parse-hx-trigger-events (response)
  "Return RESPONSE's current HX-Trigger object as an ordered alist.

Malformed or non-object pre-existing values fail closed. The adapter therefore
never attempts to merge trusted generated JSON with an opaque hand-written
header string."
  (let ((value (clog-http:response-header response :hx-trigger nil)))
    (if (null value)
        nil
        (handler-case
            (progn
              (unless (json-object-text-p value)
                (signal-invalid-hx-trigger-json value))
              (let ((events (yason:parse value :object-as :alist)))
                (unless (valid-hx-event-alist-p events)
                  (signal-invalid-hx-trigger-json value))
                events))
          (clog-http:invalid-response-header (condition)
            (error condition))
          (error ()
            (signal-invalid-hx-trigger-json value))))))

(defun merge-hx-trigger-event (events name payload)
  "Return EVENTS with NAME set to PAYLOAD while preserving first insertion order."
  (let ((existing (assoc name events :test #'string=)))
    (if existing
        (progn
          (setf (cdr existing) payload)
          events)
        (append events (list (cons name payload))))))

(defun encode-hx-trigger-events (events)
  "Encode ordered event alist EVENTS as compact JSON using Yason."
  (handler-case
      (with-output-to-string (stream)
        (yason:encode-alist events stream))
    (error ()
      (signal-invalid-hx-trigger-json nil))))

(defun set-hx-trigger (response event-name &optional (payload yason:true))
  "Merge EVENT-NAME and PAYLOAD into RESPONSE's JSON HX-Trigger header.

One response always carries at most one HX-Trigger header. Multiple calls merge
into one JSON object in first-insertion order; a repeated event name replaces
its prior value. Yason performs all JSON quoting and escaping."
  (check-type response clog-http:response)
  (let* ((name (validate-hx-event-name event-name))
         (events (parse-hx-trigger-events response))
         (events (merge-hx-trigger-event events name payload))
         (json (encode-hx-trigger-events events)))
    (set-hx-response-header response :hx-trigger json)))

(defun set-hx-url-header (response header-name url)
  "Set one same-origin URL-bearing HTMX HEADER-NAME on RESPONSE."
  (set-hx-response-header response header-name (validate-hx-local-url url)))

(defun set-hx-location (response url)
  "Set HX-Location to a validated same-origin local URL and return RESPONSE."
  (set-hx-url-header response :hx-location url))

(defun set-hx-redirect (response url)
  "Set HX-Redirect to a validated same-origin local URL and return RESPONSE."
  (set-hx-url-header response :hx-redirect url))

(defun set-hx-refresh (response)
  "Set the singleton HX-Refresh response header to `true` and return RESPONSE."
  (set-hx-response-header response :hx-refresh "true"))

(defun set-hx-push-url (response url)
  "Set HX-Push-Url to a validated same-origin local URL and return RESPONSE."
  (set-hx-url-header response :hx-push-url url))

(defun set-hx-replace-url (response url)
  "Set HX-Replace-Url to a validated same-origin local URL and return RESPONSE."
  (set-hx-url-header response :hx-replace-url url))

(defparameter +no-js-flash-session-key+ "_clog_flash"
  "Serializable one-shot flash-message key owned by the no-JavaScript fallback.")

(defparameter +no-js-validation-session-key+ "_clog_validation"
  "Serializable one-shot validation-message key owned by the no-JavaScript fallback.")

(defparameter +no-js-default-return-path+ "/"
  "Fail-closed local redirect target when _clog_return_to is absent or unsafe.")

(defun no-js-request-p (context)
  "Return true when CONTEXT is not an HTMX request."
  (not (clog-http:htmx-request-p context)))

(defun safe-local-return-path-p (value)
  "Return true for a bounded same-origin absolute path suitable for Location.

The accepted grammar intentionally excludes protocol-relative paths, backslashes,
control characters and fragments that begin before the path. REDIRECT-RESPONSE
performs the final response-layer validation before the header is emitted."
  (and (stringp value)
       (plusp (length value))
       (<= (length value) 4096)
       (char= (char value 0) #\/)
       (or (= (length value) 1)
           (char/= (char value 1) #\/))
       (not (find #\\ value))
       (every (lambda (character)
                (let ((code (char-code character)))
                  (and (>= code 32) (/= code 127))))
              value)))

(defun no-js-return-path (context)
  "Return the validated _clog_return_to path or the fail-closed root path."
  (let ((candidate (clog-http:form-param context "_clog_return_to" nil)))
    (if (safe-local-return-path-p candidate)
        (copy-seq candidate)
        (copy-seq +no-js-default-return-path+))))

(defun session-string-value (context key)
  "Return a defensive copy of session string KEY, or NIL."
  (let ((session (clog-http:request-session context)))
    (when (hash-table-p session)
      (let ((value (gethash key session)))
        (and (stringp value) (copy-seq value))))))

(defun store-session-string (context key value)
  "Store bounded serializable string VALUE under KEY in CONTEXT's Lack session."
  (unless (and (stringp value)
               (plusp (length value))
               (<= (length value) 1024))
    (error 'type-error :datum value :expected-type 'string))
  (let ((session (clog-http:request-session context)))
    (unless (hash-table-p session)
      (error 'clog-http:request-error :reason :missing-session))
    (setf (gethash key session) (copy-seq value))
    value))

(defun consume-session-string (context key)
  "Atomically consume and remove a one-shot string value from the Lack session."
  (let ((session (clog-http:request-session context)))
    (unless (hash-table-p session)
      (return-from consume-session-string nil))
    (let ((value (gethash key session)))
      (remhash key session)
      (and (stringp value) (copy-seq value)))))

(defun store-no-js-flash (context message)
  "Store one bounded success/status flash message for the next full page GET."
  (store-session-string context +no-js-flash-session-key+ message))

(defun store-no-js-validation (context message)
  "Store one bounded validation message for the next full page GET."
  (store-session-string context +no-js-validation-session-key+ message))

(defun consume-no-js-flash (context)
  "Return and remove the pending no-JavaScript flash message, if any."
  (consume-session-string context +no-js-flash-session-key+))

(defun consume-no-js-validation (context)
  "Return and remove the pending no-JavaScript validation message, if any."
  (consume-session-string context +no-js-validation-session-key+))

(defun html-status-title (status)
  "Return a bounded generic title for a no-JavaScript error STATUS."
  (cond
    ((= status 403) "Forbidden")
    ((= status 404) "Not Found")
    ((= status 405) "Method Not Allowed")
    ((= status 422) "Validation Failed")
    ((>= status 500) "Action Failed")
    (t "Request Failed")))

(defun no-js-error-document (status context)
  "Render a complete redacted HTML error document for a non-HTMX action request."
  (let ((title (html-status-title status))
        (request-id (or (clog-http:request-id context) "unavailable")))
    (spinneret:with-html-string
      (:doctype)
      (:html
       (:head
        (:meta :charset "utf-8")
        (:meta :name "viewport" :content "width=device-width, initial-scale=1")
        (:title title))
       (:body
        (:main
         (:h1 title)
         (:p "The action could not be completed.")
         (:p :data-request-id request-id
             (format nil "Request ID: ~A" request-id))))))))

(defun copy-non-framing-response-headers (response)
  "Return RESPONSE headers safe to preserve on a replacement full HTML page."
  (loop for (name value) on (clog-http:response-headers response) by #'cddr
        unless (member name '(:content-type :content-length :location) :test #'eq)
          append (list name value)))

(defun no-js-redirect-response (context message &key validation-p)
  "Store MESSAGE and return a validated 303 PRG redirect for CONTEXT."
  (if validation-p
      (store-no-js-validation context message)
      (store-no-js-flash context message))
  (clog-http:redirect-response (no-js-return-path context) :status 303))

(defun adapt-action-response-for-progressive-enhancement (response context)
  "Preserve HTMX RESPONSE or adapt it to the no-JavaScript PRG/full-page policy.

Successful ordinary form submissions never return the component fragment. They
store a one-shot flash marker and redirect with 303. Validation failures use the
same safe redirect after storing minimal validation metadata. Other failures are
converted to a complete redacted HTML document, while HTMX responses are returned
unchanged byte-for-byte at the response-object level."
  (check-type context clog-http:request-context)
  (unless (typep response 'clog-http:response)
    (error 'type-error :datum response :expected-type 'clog-http:response))
  (if (not (no-js-request-p context))
      response
      (let ((status (clog-http:response-status response))
            (reason (clog-http:response-header response :x-clog-reason nil)))
        (cond
          ((= status 422)
           (no-js-redirect-response
            context "Action validation failed." :validation-p t))
          ((and (<= 200 status 299)
                (stringp reason)
                (string= reason "stale-component"))
           (no-js-redirect-response context "Page changed; please retry."))
          ((and (<= 200 status 299)
                (stringp reason)
                (string= reason "component-expired"))
           (no-js-redirect-response context "Page state was refreshed."))
          ((<= 200 status 299)
           (no-js-redirect-response context "Action completed."))
          (t
           (clog-http:html-response
            (list (no-js-error-document status context))
            :status status
            :headers (copy-non-framing-response-headers response)))))))

(defun hm-026-action-route-handler (application context)
  "Run HM-025 dispatch then apply the progressive response policy."
  (adapt-action-response-for-progressive-enhancement
   (clog-hypermedia::hm-025-action-route-handler application context)
   context))

(defun hm-026-install-action-route (application)
  "Install the HM-025 POST endpoint with the HM-026 progressive response layer."
  (clog-hypermedia:add-route
   (clog-hypermedia:application-router application)
   :post
   (clog-hypermedia::hm-025-action-route-path
    (clog-hypermedia:application-configuration application))
   (lambda (context) (hm-026-action-route-handler application context))
   :name :clog-component-action
   :metadata '(:internal t :mutation t :csrf-required t :progressive t)))

(setf (symbol-function 'clog-hypermedia::hm-025-install-action-route)
      #'hm-026-install-action-route)
