;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime deterministic assets and static mount         ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-render
  (:export #:asset-error
           #:asset-error-reason
           #:asset
           #:asset-p
           #:make-asset
           #:asset-type
           #:asset-url
           #:asset-integrity
           #:asset-module-p
           #:asset-defer-p
           #:asset-nonce-required-p
           #:asset-key
           #:render-assets
           #:static-asset-error
           #:static-asset-error-reason
           #:static-asset-error-status
           #:make-static-asset-middleware))

(defpackage #:clog-hypermedia
  (:import-from #:clog-render
                #:asset-error
                #:asset-error-reason
                #:asset
                #:asset-p
                #:make-asset
                #:asset-type
                #:asset-url
                #:asset-integrity
                #:asset-module-p
                #:asset-defer-p
                #:asset-nonce-required-p
                #:asset-key
                #:render-assets)
  (:export #:asset-error
           #:asset-error-reason
           #:asset
           #:asset-p
           #:make-asset
           #:asset-type
           #:asset-url
           #:asset-integrity
           #:asset-module-p
           #:asset-defer-p
           #:asset-nonce-required-p
           #:asset-key
           #:render-assets))

(in-package #:clog-render)

(define-condition asset-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :reader asset-error-reason))
  (:report
   (lambda (condition stream)
     (format stream "HTML asset definition rejected (~A)."
             (asset-error-reason condition))))
  (:documentation
   "Signaled when an asset descriptor, asset list or rendering nonce is invalid."))

(defstruct (asset
             (:constructor %make-asset
                 (&key type
                       url
                       integrity
                       module-p
                       defer-p
                       nonce-required-p
                       key))
             (:conc-name %asset-)
             (:copier nil))
  "Immutable-by-public-API CSS or JavaScript asset descriptor."
  type
  url
  integrity
  (module-p nil)
  (defer-p nil)
  (nonce-required-p nil)
  key)

(defun asset-control-character-p (character)
  "Return true for ASCII controls and DEL in an HTML asset field."
  (let ((code (char-code character)))
    (or (= code 127) (< code 32))))

(defun safe-asset-string-p (value &key allow-empty)
  "Return true for a bounded HTML attribute string without control characters."
  (and (stringp value)
       (or allow-empty (plusp (length value)))
       (notany #'asset-control-character-p value)))

(defun local-asset-url-p (value)
  "Return true when VALUE is a same-origin absolute path suitable for an asset."
  (and (safe-asset-string-p value)
       (char= (char value 0) #\/)
       (or (= (length value) 1)
           (char/= (char value 1) #\/))
       (not (search "//" value))
       (not (position #\\ value))
       (not (position #\: value))
       (not (position #\# value))))

(defun safe-asset-key-p (value)
  "Return true when VALUE can deterministically identify one asset."
  (or (and (symbolp value) value)
      (safe-asset-string-p value)))

(defun safe-asset-token-p (value)
  "Return true for a nonce token accepted by the frozen application pipeline."
  (and (stringp value)
       (plusp (length value))
       (every (lambda (character)
                (or (alphanumericp character)
                    (find character "-_." :test #'char=)))
              value)))

(defun validate-asset-type (type)
  "Return TYPE or signal ASSET-ERROR when it is not :SCRIPT or :STYLE."
  (unless (member type '(:script :style) :test #'eq)
    (error 'asset-error :reason :invalid-type))
  type)

(defun validate-asset-url (url)
  "Return a defensive copy of local URL or signal ASSET-ERROR."
  (unless (local-asset-url-p url)
    (error 'asset-error :reason :non-local-or-unsafe-url))
  (copy-seq url))

(defun validate-asset-integrity (integrity)
  "Return a defensive copy of optional SRI INTEGRITY metadata."
  (unless (or (null integrity)
              (safe-asset-string-p integrity))
    (error 'asset-error :reason :invalid-integrity))
  (and integrity (copy-seq integrity)))

(defun validate-asset-key (key)
  "Return a defensive asset KEY or signal ASSET-ERROR."
  (unless (safe-asset-key-p key)
    (error 'asset-error :reason :invalid-key))
  (if (stringp key) (copy-seq key) key))

(defun validate-asset-boolean (value reason)
  "Return boolean VALUE or signal ASSET-ERROR with REASON."
  (unless (typep value 'boolean)
    (error 'asset-error :reason reason))
  value)

(defun validate-asset-kind-options
    (type module-p defer-p nonce-required-p)
  "Reject script-only options on a style descriptor."
  (when (and (eq type :style)
             (or module-p defer-p nonce-required-p))
    (error 'asset-error :reason :script-option-on-style))
  t)

(defun make-asset
    (&key type
          url
          integrity
          (module-p nil)
          (defer-p nil)
          (nonce-required-p nil)
          key)
  "Create a validated immutable asset descriptor.

URLs must be same-origin absolute paths. TYPE is :SCRIPT or :STYLE. KEY is a
non-NIL symbol or non-empty string and drives deterministic de-duplication.
Caller-owned strings are copied before storage."
  (let* ((type (validate-asset-type type))
         (url (validate-asset-url url))
         (integrity (validate-asset-integrity integrity))
         (module-p
           (validate-asset-boolean module-p :invalid-module-flag))
         (defer-p
           (validate-asset-boolean defer-p :invalid-defer-flag))
         (nonce-required-p
           (validate-asset-boolean
            nonce-required-p :invalid-nonce-required-flag))
         (key (validate-asset-key key)))
    (validate-asset-kind-options
     type module-p defer-p nonce-required-p)
    (%make-asset :type type
                 :url url
                 :integrity integrity
                 :module-p module-p
                 :defer-p defer-p
                 :nonce-required-p nonce-required-p
                 :key key)))

(defun asset-type (descriptor)
  "Return DESCRIPTOR's immutable asset type."
  (check-type descriptor asset)
  (%asset-type descriptor))

(defun asset-url (descriptor)
  "Return a defensive copy of DESCRIPTOR's local URL."
  (check-type descriptor asset)
  (copy-seq (%asset-url descriptor)))

(defun asset-integrity (descriptor)
  "Return a defensive copy of DESCRIPTOR's optional SRI metadata."
  (check-type descriptor asset)
  (let ((value (%asset-integrity descriptor)))
    (and value (copy-seq value))))

(defun asset-module-p (descriptor)
  "Return true when DESCRIPTOR is an ECMAScript module."
  (check-type descriptor asset)
  (%asset-module-p descriptor))

(defun asset-defer-p (descriptor)
  "Return true when DESCRIPTOR must use the defer attribute."
  (check-type descriptor asset)
  (%asset-defer-p descriptor))

(defun asset-nonce-required-p (descriptor)
  "Return true when DESCRIPTOR requires the current request CSP nonce."
  (check-type descriptor asset)
  (%asset-nonce-required-p descriptor))

(defun asset-key (descriptor)
  "Return a defensive copy of DESCRIPTOR's de-duplication key."
  (check-type descriptor asset)
  (let ((value (%asset-key descriptor)))
    (if (stringp value) (copy-seq value) value)))

(defun proper-asset-list-p (value)
  "Return true when VALUE is a finite proper list."
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
        (and (consp value) (walk value value)))))

(defun same-asset-definition-p (left right &key ignore-key)
  "Return true when LEFT and RIGHT describe the same emitted HTML asset."
  (and (eq (%asset-type left) (%asset-type right))
       (string= (%asset-url left) (%asset-url right))
       (equal (%asset-integrity left) (%asset-integrity right))
       (eql (%asset-module-p left) (%asset-module-p right))
       (eql (%asset-defer-p left) (%asset-defer-p right))
       (eql (%asset-nonce-required-p left)
            (%asset-nonce-required-p right))
       (or ignore-key
           (equal (%asset-key left) (%asset-key right)))))

(defun deduplicate-assets (assets)
  "Return ASSETS in first-seen order, rejecting ambiguous duplicates.

Identical descriptors sharing a key or URL collapse to their first occurrence.
A reused key or URL with different loading semantics signals ASSET-ERROR rather
than letting hash-table iteration or caller order silently select a winner."
  (unless (proper-asset-list-p assets)
    (error 'asset-error :reason :malformed-asset-list))
  (let ((by-key (make-hash-table :test #'equal))
        (by-url (make-hash-table :test #'equal))
        (result nil))
    (dolist (descriptor assets (nreverse result))
      (unless (asset-p descriptor)
        (error 'asset-error :reason :non-asset-list-member))
      (let* ((key (%asset-key descriptor))
             (url (%asset-url descriptor))
             (key-match (gethash key by-key))
             (url-match (gethash url by-url)))
        (cond
          (key-match
           (unless (same-asset-definition-p
                    key-match descriptor :ignore-key t)
             (error 'asset-error :reason :conflicting-key)))
          (url-match
           (unless (same-asset-definition-p
                    url-match descriptor :ignore-key t)
             (error 'asset-error :reason :conflicting-url))
           (setf (gethash key by-key) url-match))
          (t
           (setf (gethash key by-key) descriptor)
           (setf (gethash url by-url) descriptor)
           (push descriptor result)))))))

(defun write-html-escaped (value stream)
  "Write VALUE escaped for a double-quoted HTML attribute or text node."
  (loop for character across value
        do (case character
             (#\& (write-string "&amp;" stream))
             (#\< (write-string "&lt;" stream))
             (#\> (write-string "&gt;" stream))
             (#\" (write-string "&quot;" stream))
             (#\' (write-string "&#39;" stream))
             (otherwise (write-char character stream)))))

(defun validate-render-nonce (nonce)
  "Return NONCE when safe for an HTML/CSP token, otherwise signal ASSET-ERROR."
  (unless (safe-asset-token-p nonce)
    (error 'asset-error :reason :missing-or-invalid-nonce))
  nonce)

(defun render-style-asset (descriptor stream)
  "Write one deterministic stylesheet link for DESCRIPTOR."
  (write-string "<link rel=\"stylesheet\" href=\"" stream)
  (write-html-escaped (%asset-url descriptor) stream)
  (write-char #\" stream)
  (let ((integrity (%asset-integrity descriptor)))
    (when integrity
      (write-string " integrity=\"" stream)
      (write-html-escaped integrity stream)
      (write-string "\" crossorigin=\"anonymous\"" stream)))
  (write-char #\> stream))

(defun render-script-asset (descriptor stream nonce)
  "Write one deterministic script tag for DESCRIPTOR."
  (write-string "<script src=\"" stream)
  (write-html-escaped (%asset-url descriptor) stream)
  (write-char #\" stream)
  (when (%asset-module-p descriptor)
    (write-string " type=\"module\"" stream))
  (when (%asset-defer-p descriptor)
    (write-string " defer" stream))
  (when (%asset-nonce-required-p descriptor)
    (write-string " nonce=\"" stream)
    (write-html-escaped (validate-render-nonce nonce) stream)
    (write-char #\" stream))
  (let ((integrity (%asset-integrity descriptor)))
    (when integrity
      (write-string " integrity=\"" stream)
      (write-html-escaped integrity stream)
      (write-string "\" crossorigin=\"anonymous\"" stream)))
  (write-string "></script>" stream))

(defun render-assets (assets &optional context)
  "Render validated ASSETS once each in stable first-seen order.

When CONTEXT is non-NIL it must be a request context, and its CSP nonce is
attached only to script descriptors that declare NONCE-REQUIRED-P. No public
raw-nonce argument exists, so page code cannot substitute a nonce outside the
request boundary. The function emits no inline script and never accepts an
external or protocol-relative URL."
  (when context
    (check-type context clog-http:request-context))
  (let ((assets (deduplicate-assets assets))
        (nonce (and context (clog-http:request-csp-nonce context))))
    (with-output-to-string (stream)
      (loop for descriptor in assets
            for first-p = t then nil
            do (unless first-p (terpri stream))
               (ecase (%asset-type descriptor)
                 (:style (render-style-asset descriptor stream))
                 (:script (render-script-asset descriptor stream nonce)))))))

(define-condition static-asset-error (clog-http:clog-hypermedia-error)
  ((reason
    :initarg :reason
    :reader static-asset-error-reason)
   (status
    :initarg :status
    :initform 400
    :reader static-asset-error-status))
  (:report
   (lambda (condition stream)
     (format stream "Static asset request rejected (~A)."
             (static-asset-error-reason condition))))
  (:documentation
   "Signaled when a static asset path is malformed or escapes its configured root."))

(defun string-prefix-p (prefix string)
  "Return true when STRING begins with PREFIX."
  (and (stringp prefix)
       (stringp string)
       (<= (length prefix) (length string))
       (string= prefix string :end2 (length prefix))))

(defun static-path-control-character-p (character)
  "Return true for control characters and DEL in a static request path."
  (let ((code (char-code character)))
    (or (= code 127) (< code 32))))

(defun hex-digit-character-p (character)
  "Return true when CHARACTER is an ASCII hexadecimal digit."
  (or (and (char<= #\0 character #\9))
      (and (char<= #\a character #\f))
      (and (char<= #\A character #\F))))

(defun decode-static-segment (raw)
  "Strictly percent-decode one static path segment while preserving plus."
  (handler-case
      (with-output-to-string (stream)
        (let ((index 0)
              (length (length raw)))
          (loop while (< index length)
                for character = (char raw index)
                do (if (char= character #\%)
                       (let ((start index))
                         (loop while (and (< index length)
                                          (char= (char raw index) #\%))
                               do (unless (and (< (+ index 2) length)
                                               (hex-digit-character-p
                                                (char raw (1+ index)))
                                               (hex-digit-character-p
                                                (char raw (+ index 2))))
                                    (error "Malformed percent encoding."))
                                  (incf index 3))
                         (write-string
                          (quri:url-decode raw
                                           :encoding :utf-8
                                           :start start
                                           :end index
                                           :lenient nil)
                          stream))
                       (progn
                         (write-char character stream)
                         (incf index))))))
    (error ()
      (error 'static-asset-error
             :reason :invalid-percent-or-utf8-encoding
             :status 400))))

(defun split-static-relative-path (relative)
  "Split RELATIVE on slash while preserving empty segments."
  (let ((segments nil)
        (start 0)
        (length (length relative)))
    (loop
      for slash = (position #\/ relative :start start)
      do (if slash
             (progn
               (push (subseq relative start slash) segments)
               (setf start (1+ slash)))
             (progn
               (push (subseq relative start length) segments)
               (return (nreverse segments)))))))

(defun safe-static-segment-p (segment)
  "Return true when decoded SEGMENT cannot alter filesystem path structure."
  (and (plusp (length segment))
       (not (member segment '("." "..") :test #'string=))
       (notany #'static-path-control-character-p segment)
       (not (position #\/ segment))
       (not (position #\\ segment))
       (not (position #\: segment))
       (not (position #\* segment))
       (not (position #\? segment))))

(defun join-static-segments (segments)
  "Join decoded static path SEGMENTS using forward slashes."
  (format nil "~{~A~^/~}" segments))

(defun decode-and-validate-static-relative-path (relative)
  "Return a safe decoded relative path or signal STATIC-ASSET-ERROR."
  (when (or (position #\# relative)
            (position #\? relative)
            (position #\\ relative)
            (some #'static-path-control-character-p relative))
    (error 'static-asset-error :reason :unsafe-path-character :status 400))
  (when (zerop (length relative))
    (return-from decode-and-validate-static-relative-path ""))
  (let ((segments
          (mapcar #'decode-static-segment
                  (split-static-relative-path relative))))
    (unless (every #'safe-static-segment-p segments)
      (error 'static-asset-error :reason :unsafe-path-segment :status 400))
    (join-static-segments segments)))

(defun pathname-directory-prefix-p (root candidate)
  "Return true when CANDIDATE's canonical directory begins with ROOT."
  (and (equalp (pathname-host root) (pathname-host candidate))
       (equalp (pathname-device root) (pathname-device candidate))
       (let ((root-directory (pathname-directory root))
             (candidate-directory (pathname-directory candidate)))
         (and (<= (length root-directory) (length candidate-directory))
              (equalp root-directory
                      (subseq candidate-directory
                              0
                              (length root-directory)))))))

(defun assert-static-candidate-contained (root relative)
  "Reject an existing candidate whose canonical target lies outside ROOT."
  (let* ((candidate (merge-pathnames (pathname relative) root))
         (existing (probe-file candidate)))
    (when existing
      (let ((canonical (truename existing)))
        (unless (pathname-directory-prefix-p root canonical)
          (error 'static-asset-error
                 :reason :static-root-escape
                 :status 400))))
    candidate))

(defun static-request-method-p (method)
  "Return true when METHOD can retrieve a static asset."
  (member method '(:get :head) :test #'eq))

(defun make-static-asset-middleware (prefix root)
  "Return middleware serving ROOT only below explicit URL PREFIX.

The mount decodes each path segment strictly, rejects dot segments, encoded
separators, control characters and filesystem wildcard syntax, and verifies
existing canonical targets remain under ROOT. Requests outside PREFIX or with
non-retrieval methods continue to the next application."
  (if (or (null prefix) (null root))
      (lambda (app) app)
      (let ((prefix (copy-seq prefix))
            (root (truename (uiop:ensure-directory-pathname root))))
        (lambda (app)
          (check-type app function)
          (lambda (env)
            (let ((path (getf env :path-info))
                  (method (getf env :request-method)))
              (if (and (stringp path)
                       (string-prefix-p prefix path)
                       (static-request-method-p method))
                  (let* ((relative
                           (subseq path (length prefix)))
                         (decoded
                           (decode-and-validate-static-relative-path relative))
                         (static-env (copy-list env)))
                    (assert-static-candidate-contained root decoded)
                    (setf (getf env :clog.static-request-p) t)
                    (setf (getf static-env :clog.static-request-p) t)
                    (setf (getf static-env :path-info)
                          (concatenate 'string "/" decoded))
                    (lack.middleware.static:call-app-file root static-env))
                  (funcall app env))))))))
