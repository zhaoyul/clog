;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime response abstraction                         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-http
  (:export #:response
           #:make-response
           #:response-status
           #:response-headers
           #:response-body
           #:response-kind
           #:response-header
           #:response-header-values
           #:html-response
           #:redirect-response
           #:no-content-response
           #:response->clack-response
           #:normalize-response))

(defpackage #:clog-hypermedia
  (:import-from #:clog-http
                #:response
                #:make-response
                #:response-status
                #:response-headers
                #:response-body
                #:response-kind
                #:response-header
                #:response-header-values
                #:html-response
                #:redirect-response
                #:no-content-response
                #:response->clack-response
                #:normalize-response)
  (:export #:response
           #:make-response
           #:response-status
           #:response-headers
           #:response-body
           #:response-kind
           #:response-header
           #:response-header-values
           #:html-response
           #:redirect-response
           #:no-content-response
           #:response->clack-response
           #:normalize-response))

(in-package #:clog-http)

(defparameter +response-kinds+
  '(:html :redirect :stream :websocket :empty)
  "Frozen response-kind vocabulary for the Hypermedia HTTP adapter.")

(defparameter +singleton-response-headers+
  '(:content-type :content-length :location)
  "Headers that are ambiguous or unsafe when repeated in a single response.")

(defstruct (response
             (:constructor %make-response
                 (&key (status 200)
                       headers
                       (body "")
                       (kind :html)
                       redirect-allowed-origins)))
  "Validated HTTP response value used before encoding to the Clack protocol.

RESPONSE->CLACK-RESPONSE revalidates all mutable fields, so application code
cannot bypass header/body safety by mutating a structure after construction."
  (status 200)
  (headers nil)
  (body "")
  (kind :html)
  (redirect-allowed-origins nil))

(defun header-control-character-p (character)
  "Return true when CHARACTER is forbidden in a response header value."
  (let ((code (char-code character)))
    (or (= code 127)
        (and (< code 32)
             (char/= character #\Tab)))))

(defun safe-header-string-p (value)
  "Return true when VALUE contains no HTTP header control characters."
  (and (stringp value)
       (notany #'header-control-character-p value)))

(defun proper-list-value-p (value)
  "Return true when VALUE is a finite proper list.

Use a tortoise/hare traversal so malformed dotted and circular lists fail
closed instead of depending on an implementation or utility-library helper."
  (labels ((walk (slow fast)
             (cond
               ((null fast) t)
               ((atom fast) nil)
               ((null (cdr fast)) t)
               ((atom (cdr fast)) nil)
               (t
                (let ((next-slow (cdr slow))
                      (next-fast (cddr fast)))
                  (and (not (eq next-slow next-fast))
                       (walk next-slow next-fast)))))))
    (or (null value)
        (and (consp value)
             (walk value value)))))

(defun proper-even-list-p (value)
  "Return true when VALUE is a finite proper list with an even item count."
  (and (proper-list-value-p value)
       (evenp (length value))))

(defun validate-response-status (status)
  "Return STATUS or signal INVALID-RESPONSE-STATUS for an invalid HTTP code."
  (unless (and (integerp status)
               (<= 100 status 599))
    (error 'invalid-response-status
           :reason :invalid-status
           :value status))
  status)

(defun validate-response-kind (kind)
  "Return KIND or signal INVALID-RESPONSE-KIND when KIND is not supported."
  (unless (member kind +response-kinds+ :test #'eq)
    (error 'invalid-response-kind
           :reason :invalid-kind
           :value kind))
  kind)

(defun validate-response-header-pair (name value)
  "Validate one Clack header NAME/VALUE pair and return true.

Header names are keywords by contract. Header values are strings, except that
CONTENT-LENGTH is represented as a non-negative integer for Clack handlers."
  (unless (keywordp name)
    (error 'invalid-response-header
           :reason :invalid-header-name
           :name name
           :value value))
  (cond
    ((eq name :content-length)
     (unless (and (integerp value) (not (minusp value)))
       (error 'invalid-response-header
              :reason :invalid-content-length
              :name name
              :value value)))
    ((not (safe-header-string-p value))
     (error 'invalid-response-header
            :reason :unsafe-header-value
            :name name
            :value value)))
  t)

(defun header-occurrences (headers name)
  "Return the number of NAME entries in validated alternating HEADERS."
  (loop for tail on headers by #'cddr
        count (eq (first tail) name)))

(defun validate-response-headers (headers)
  "Validate alternating Clack HEADERS and return HEADERS.

Repeated SET-COOKIE is intentionally valid and its original order is retained.
Singleton framing headers are rejected when repeated rather than silently
merged."
  (unless (proper-even-list-p headers)
    (error 'invalid-response-header
           :reason :malformed-header-list
           :name nil
           :value nil))
  (loop for (name value) on headers by #'cddr
        do (validate-response-header-pair name value))
  (dolist (name +singleton-response-headers+)
    (when (> (header-occurrences headers name) 1)
      (error 'invalid-response-header
             :reason :duplicate-singleton-header
             :name name
             :value nil)))
  headers)

(defun copy-response-headers (headers)
  "Return a defensive copy of validated alternating HEADERS."
  (loop for (name value) on headers by #'cddr
        append (list name (if (stringp value) (copy-seq value) value))))

(defun response-header-values (response name)
  "Return defensive copies of all values for keyword header NAME in RESPONSE."
  (check-type response response)
  (unless (keywordp name)
    (error 'invalid-response-header
           :reason :invalid-header-name
           :name name
           :value nil))
  (loop for (key value) on (response-headers response) by #'cddr
        when (eq key name)
          collect (if (stringp value) (copy-seq value) value)))

(defun response-header (response name &optional default)
  "Return the first value for keyword header NAME in RESPONSE, or DEFAULT."
  (let ((values (response-header-values response name)))
    (if values (first values) default)))

(defun utf-8-character-octet-length (character)
  "Return the UTF-8 encoded byte length for CHARACTER.

Signal INVALID-RESPONSE-BODY for a character code outside the Unicode scalar
range."
  (let ((code (char-code character)))
    (cond
      ((<= code #x7f) 1)
      ((<= code #x7ff) 2)
      ((or (> code #x10ffff)
           (<= #xd800 code #xdfff))
       (error 'invalid-response-body
              :reason :invalid-unicode-scalar
              :value character))
      ((<= code #xffff) 3)
      (t 4))))

(defun utf-8-string-octet-length (string)
  "Return the number of bytes needed to encode STRING as UTF-8."
  (loop for character across string
        sum (utf-8-character-octet-length character)))

(defun octet-vector-p (value)
  "Return true when VALUE is a vector whose elements are unsigned bytes."
  (and (vectorp value)
       (not (stringp value))
       (every (lambda (item)
                (typep item '(unsigned-byte 8)))
              value)))

(defun string-chunk-list-p (value)
  "Return true when VALUE is a finite proper list containing only strings."
  (and (proper-list-value-p value)
       (every #'stringp value)))

(defun response-body-octet-length (body)
  "Return deterministic encoded byte length for BODY, or NIL when server-owned.

Pathnames and delayed responder functions deliberately return NIL so the
underlying Clack server/transport owns framing."
  (cond
    ((null body) 0)
    ((stringp body) (utf-8-string-octet-length body))
    ((string-chunk-list-p body)
     (loop for chunk in body sum (utf-8-string-octet-length chunk)))
    ((octet-vector-p body) (length body))
    ((pathnamep body) nil)
    ((functionp body) nil)
    (t
     (error 'invalid-response-body
            :reason :unsupported-body-type
            :value body))))

(defun normal-clack-body-p (body)
  "Return true when BODY can be emitted in a normal Clack response triple."
  (or (null body)
      (stringp body)
      (string-chunk-list-p body)
      (octet-vector-p body)
      (pathnamep body)))

(defun validate-response-body-for-kind (kind body)
  "Validate BODY against KIND and return BODY."
  (case kind
    (:empty
     (unless (null body)
       (error 'invalid-response-body
              :reason :non-empty-empty-response
              :value body)))
    ((:stream :websocket)
     (unless (or (pathnamep body) (functionp body))
       (error 'invalid-response-body
              :reason :invalid-delayed-or-stream-body
              :value body)))
    ((:html :redirect)
     (unless (normal-clack-body-p body)
       (error 'invalid-response-body
              :reason :invalid-normal-body
              :value body))))
  body)

(defun header-present-p (headers name)
  "Return true when alternating HEADERS contains NAME."
  (loop for tail on headers by #'cddr
        thereis (eq (first tail) name)))

(defun append-response-header (headers name value)
  "Return HEADERS with NAME and VALUE appended in Clack order."
  (append headers (list name value)))

(defun ensure-html-content-type (headers kind)
  "Add the default UTF-8 HTML Content-Type when KIND is :HTML and none exists."
  (if (and (eq kind :html)
           (not (header-present-p headers :content-type)))
      (append-response-header headers :content-type "text/html; charset=utf-8")
      headers))

(defun ensure-content-length (headers body)
  "Validate or add deterministic Content-Length for BODY.

Server-owned pathname and delayed bodies have unknown length here and therefore
leave Content-Length untouched."
  (let ((expected (response-body-octet-length body)))
    (if (null expected)
        headers
        (let ((actual (loop for (name value) on headers by #'cddr
                            when (eq name :content-length)
                              do (return value))))
          (cond
            ((null actual)
             (append-response-header headers :content-length expected))
            ((/= actual expected)
             (error 'response-content-length-mismatch
                    :reason :content-length-mismatch
                    :expected expected
                    :actual actual))
            (t headers))))))

(defun unsafe-redirect-character-p (character)
  "Return true for redirect characters that browsers/proxies can reinterpret unsafely."
  (or (char= character #\\)
      (header-control-character-p character)))

(defun safe-relative-redirect-p (url)
  "Return true when URL is an application-local absolute-path reference."
  (and (stringp url)
       (plusp (length url))
       (char= (char url 0) #\/)
       (or (= (length url) 1)
           (char/= (char url 1) #\/))
       (notany #'unsafe-redirect-character-p url)))

(defun parsed-http-origin (url &key origin-only-p)
  "Return (scheme host port) for a valid HTTP(S) URL, otherwise NIL.

When ORIGIN-ONLY-P is true, URL must not contain userinfo, query or fragment,
and its path must be empty or `/`."
  (handler-case
      (let* ((uri (quri:uri url))
             (scheme (quri:uri-scheme uri))
             (host (quri:uri-host uri))
             (path (quri:uri-path uri)))
        (when (and scheme
                   host
                   (member scheme '("http" "https") :test #'string-equal)
                   (null (quri:uri-userinfo uri))
                   (or (not origin-only-p)
                       (and (null (quri:uri-query uri))
                            (null (quri:uri-fragment uri))
                            (or (null path)
                                (string= path "")
                                (string= path "/")))))
          (list (string-downcase scheme)
                (string-downcase host)
                (quri:uri-port uri))))
    (error () nil)))

(defun normalize-redirect-origin-allowlist (allowed-origins)
  "Validate ALLOWED-ORIGINS as exact HTTP(S) origins and return deep copies.

The returned list owns its strings, preventing a caller from mutating a source
origin string after REDIRECT-RESPONSE construction and thereby changing the
stored redirect policy."
  (unless (proper-list-value-p allowed-origins)
    (error 'invalid-redirect-url
           :reason :invalid-origin-allowlist
           :value allowed-origins))
  (loop for origin in allowed-origins
        unless (and (stringp origin)
                    (notany #'unsafe-redirect-character-p origin)
                    (parsed-http-origin origin :origin-only-p t))
          do (error 'invalid-redirect-url
                    :reason :invalid-origin-allowlist
                    :value origin)
        collect (copy-seq origin)))

(defun allowlisted-external-redirect-p (url allowed-origins)
  "Return true when absolute URL has exactly an origin from ALLOWED-ORIGINS."
  (let ((target-origin (parsed-http-origin url)))
    (and target-origin
         (some (lambda (allowed-origin)
                 (let ((origin (parsed-http-origin allowed-origin
                                                   :origin-only-p t)))
                   (and origin (equal origin target-origin))))
               allowed-origins))))

(defun validate-redirect-url (url allowed-origins)
  "Return URL when redirect policy permits it, otherwise signal INVALID-REDIRECT-URL."
  (unless (and (stringp url)
               (notany #'unsafe-redirect-character-p url))
    (error 'invalid-redirect-url
           :reason :invalid-or-control-character
           :value url))
  (cond
    ((safe-relative-redirect-p url) url)
    ((and allowed-origins
          (allowlisted-external-redirect-p url allowed-origins))
     url)
    (t
     (error 'invalid-redirect-url
            :reason :origin-not-allowed
            :value url))))

(defun validate-redirect-response (response headers)
  "Validate redirect LOCATION in RESPONSE against its stored origin allowlist."
  (let ((locations (loop for (name value) on headers by #'cddr
                         when (eq name :location)
                           collect value)))
    (unless (= 1 (length locations))
      (error 'invalid-response-header
             :reason :redirect-location-required
             :name :location
             :value nil))
    (validate-redirect-url (first locations)
                           (response-redirect-allowed-origins response))))

(defun validate-response-object (response)
  "Validate all mutable fields of RESPONSE immediately before Clack encoding."
  (validate-response-status (response-status response))
  (validate-response-kind (response-kind response))
  (validate-response-headers (response-headers response))
  (validate-response-body-for-kind (response-kind response)
                                   (response-body response))
  (when (eq (response-kind response) :redirect)
    (validate-redirect-response response (response-headers response)))
  response)

(defun make-response (&key (status 200) headers (body "") (kind :html))
  "Create a validated RESPONSE.

STATUS must be an HTTP status integer from 100 through 599. HEADERS is a Clack
alternating keyword/value list; repeated :SET-COOKIE entries are preserved.
BODY may be NIL, a string, a list of string chunks, an octet vector, or a
pathname for a normal response. A delayed response function is accepted only
with KIND :STREAM or :WEBSOCKET. This function does not acquire locks or mutate
external state and can signal RESPONSE-ERROR subclasses for invalid input."
  (let ((response (%make-response :status status
                                  :headers (copy-response-headers
                                            (validate-response-headers headers))
                                  :body body
                                  :kind kind)))
    (validate-response-object response)
    response))

(defun html-response (body &key (status 200) headers)
  "Create an HTML RESPONSE with default UTF-8 Content-Type.

The default Content-Type is materialized during Clack encoding unless HEADERS
already supplies one. BODY is not escaped here; HTML escaping belongs to the
renderer boundary."
  (make-response :status status :headers headers :body body :kind :html))

(defun redirect-response (url &key (status 303) headers allowed-origins)
  "Create a redirect RESPONSE to URL.

Application-local paths beginning with `/` are accepted except `//...` and
backslash/control-character forms. Absolute HTTP(S) URLs require exact origin
membership in ALLOWED-ORIGINS. Every allowlist entry must itself be an exact
origin and is defensively copied. STATUS must be one of 301, 302, 303, 307 or
308. Repeated or caller-supplied Location headers are rejected to keep one
unambiguous redirect target."
  (unless (member status '(301 302 303 307 308))
    (error 'invalid-response-status
           :reason :invalid-redirect-status
           :value status))
  (validate-response-headers headers)
  (when (header-present-p headers :location)
    (error 'invalid-response-header
           :reason :caller-location-conflicts-with-redirect-url
           :name :location
           :value nil))
  (let ((allowed-origins
          (normalize-redirect-origin-allowlist allowed-origins)))
    (validate-redirect-url url allowed-origins)
    (let ((response (%make-response
                     :status status
                     :headers (copy-response-headers
                               (append-response-header headers :location url))
                     :body nil
                     :kind :redirect
                     :redirect-allowed-origins allowed-origins)))
      (validate-response-object response)
      response)))

(defun no-content-response (&key (status 204) headers)
  "Create an empty RESPONSE, normally using HTTP status 204."
  (make-response :status status :headers headers :body nil :kind :empty))

(defun delayed-response-p (response)
  "Return true when RESPONSE delegates response production to a Clack responder function."
  (and (member (response-kind response) '(:stream :websocket) :test #'eq)
       (functionp (response-body response))))

(defun response->clack-response (response)
  "Encode RESPONSE as a Clack normal or delayed response.

Normal responses become `(status headers body)`. Deterministically sized string,
string-chunk, octet and NIL bodies receive an exact UTF-8 byte Content-Length
unless one matching value already exists. Pathname bodies omit automatic
Content-Length so the server owns file framing. Delayed :STREAM/:WEBSOCKET
responses return their responder function directly and must not carry status or
headers that would otherwise be silently discarded."
  (check-type response response)
  (validate-response-object response)
  (if (delayed-response-p response)
      (progn
        (unless (and (= 200 (response-status response))
                     (null (response-headers response)))
          (error 'invalid-response-body
                 :reason :delayed-response-owns-status-and-headers
                 :value (response-body response)))
        (response-body response))
      (let* ((headers (copy-response-headers (response-headers response)))
             (headers (ensure-html-content-type headers (response-kind response)))
             (headers (ensure-content-length headers (response-body response))))
        (validate-response-headers headers)
        (list (response-status response)
              headers
              (response-body response)))))

(defun normalize-response (value)
  "Normalize VALUE into the Clack response protocol.

RESPONSE objects are validated and encoded. Strings become HTML responses,
pathnames become server-owned stream responses, functions are treated as Clack
delayed responder functions, and NIL becomes a 204 empty response. Raw Clack
triples are intentionally not accepted so handler code converges on the typed
response abstraction."
  (cond
    ((response-p value) (response->clack-response value))
    ((stringp value) (response->clack-response (html-response value)))
    ((pathnamep value)
     (response->clack-response
      (make-response :status 200 :headers nil :body value :kind :stream)))
    ((functionp value) value)
    ((null value) (response->clack-response (no-content-response)))
    (t
     (error 'invalid-response-body
            :reason :cannot-normalize-response
            :value value))))

(setf (documentation 'response-status 'function)
      "Return the mutable HTTP status slot of RESPONSE; encoding revalidates it.")
(setf (documentation 'response-headers 'function)
      "Return the mutable alternating Clack header list of RESPONSE; encoding revalidates it.")
(setf (documentation 'response-body 'function)
      "Return the response body object carried by RESPONSE.")
(setf (documentation 'response-kind 'function)
      "Return the response kind keyword carried by RESPONSE.")
