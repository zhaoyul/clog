;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; CLOG 3 Hypermedia Runtime static asset mount                            ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:clog-render
  (:export #:static-asset-error
           #:static-asset-error-reason
           #:static-asset-error-status
           #:make-static-asset-middleware))

(in-package #:clog-render)

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
