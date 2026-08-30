;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Request-context test fixtures                                           ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package #:clog-hypermedia-tests)

(defclass fixture-octet-input-stream
    (trivial-gray-streams:fundamental-binary-input-stream
     trivial-gray-streams:trivial-gray-stream-mixin)
  ((octets :initarg :octets :reader fixture-stream-octets)
   (position :initform 0 :accessor fixture-stream-position)))

(defmethod stream-element-type
    ((stream fixture-octet-input-stream))
  (declare (ignore stream))
  '(unsigned-byte 8))

(defmethod trivial-gray-streams:stream-read-byte
    ((stream fixture-octet-input-stream))
  (let ((position (fixture-stream-position stream))
        (octets (fixture-stream-octets stream)))
    (if (>= position (length octets))
        :eof
        (prog1 (aref octets position)
          (incf (fixture-stream-position stream))))))

(defmethod trivial-gray-streams:stream-read-sequence
    ((stream fixture-octet-input-stream) sequence start end &key)
  (loop for index from start below end
        for octet = (trivial-gray-streams:stream-read-byte stream)
        until (eq octet :eof)
        do (setf (elt sequence index) octet)
        finally (return index)))

(defun ascii-octets (string)
  "Encode fixture STRING as ASCII octets."
  (let ((octets (make-array (length string)
                            :element-type '(unsigned-byte 8))))
    (loop for character across string
          for index from 0
          for code = (char-code character)
          do (assert (< code 128))
             (setf (aref octets index) code))
    octets))

(defun make-fixture-body-stream (body)
  "Return a fresh binary input stream for ASCII fixture BODY."
  (make-instance 'fixture-octet-input-stream :octets (ascii-octets body)))

(defun make-request-env (&key
                           (method :get)
                           (path "/test")
                           (query-string "")
                           headers
                           body
                           content-type
                           content-length
                           transfer-encoding
                           session
                           session-id)
  "Build a minimal Clack env for request-context unit tests."
  (let ((header-table (make-hash-table :test 'equal)))
    (dolist (header headers)
      (setf (gethash (car header) header-table) (cdr header)))
    (setf (gethash "accept" header-table) "*/*")
    (when transfer-encoding
      (setf (gethash "transfer-encoding" header-table) transfer-encoding))
    (let ((env (list :request-method method
                     :script-name ""
                     :path-info path
                     :server-name "localhost"
                     :server-port 5000
                     :server-protocol :http/1.1
                     :request-uri path
                     :url-scheme :http
                     :query-string query-string
                     :headers header-table)))
      (when body
        (setf (getf env :raw-body) (make-fixture-body-stream body))
        (setf (getf env :content-length)
              (or content-length (length (ascii-octets body)))))
      (when (and (null body) content-length)
        (setf (getf env :content-length) content-length))
      (when content-type
        (setf (getf env :content-type) content-type))
      (when session
        (setf (getf env :lack.session) session))
      (when session-id
        (setf (getf env :lack.session.options)
              (list :id session-id
                    :new-session nil
                    :change-id nil
                    :expire nil)))
      env)))

(defun multipart-body (&rest fields)
  "Return a small multipart/form-data body for FIELD name/value conses."
  (with-output-to-string (stream)
    (dolist (field fields)
      (format stream "--clog-boundary~C~C" #\Return #\Newline)
      (format stream
              "Content-Disposition: form-data; name=~S~C~C~C~C"
              (car field)
              #\Return #\Newline #\Return #\Newline)
      (format stream "~A~C~C" (cdr field) #\Return #\Newline))
    (format stream "--clog-boundary--~C~C" #\Return #\Newline)))
