;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defstruct (request (:conc-name :rq-) (:copier nil))
  ;; Request line (or "initial line").
  (method nil :type symbol)
  (url #.(coerce "" 'base-string) :type base-string)
  (http-version nil :type symbol)
   
  (header-fields nil :type list)

  (message-body #.(coerce "" 'base-string) :type base-string))


(maybe-inline request-parse-http-version)
(defun request-parse-http-version (str)
  (declare (base-string str)
           #.optimizations)
  (if (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
        (string= #.(coerce "HTTP/1.1" 'simple-base-string) str))
      :http-1.1
      :http-1.0))


(maybe-inline request-parse-initial-line)
(defun request-parse-initial-line (line request)
  "Parse LINE (\"initial line\") and destructively modify REQUEST."
  (declare (base-string line)
           (request request)
           #.optimizations)
  (let ((method_url_version (cl-ppcre:split #\Space line :sharedp t :limit 3)))
    (declare (dynamic-extent method_url_version))
    (setf (rq-method request) (let ((method-str (first method_url_version)))
                                (cond ((string= method-str "GET") :get) ;; TODO: Convert to a single test if we're only to support GET and POST?
                                      ((string= method-str "POST") :post)
                                      (t (error "REQUEST-PARSE-INITIAL-LINE: HTTP method not implemented yet: ~S" method-str))))
          (rq-url request) (second method_url_version)
          (rq-http-version request) (request-parse-http-version (third method_url_version)))))


(maybe-inline request-parse-header-field)
(defun request-parse-header-field (line)
  (declare (base-string line)
           #.optimizations)
  (let ((key_value (cl-ppcre:split ":\\s*" line :limit 2 :sharedp t)))
    (setf (car key_value) (nstring-upcase (car key_value))
          (cdr key_value) (second key_value))
    key_value))


(maybe-inline request-parse-header-fields)
(defun request-parse-header-fields (lines request)
  "Parse LINES (HTTP request headers) and destructively modify REQUEST."
  (declare (list lines)
           (request request)
           #.optimizations)
  (loop :for line :in lines
     :do (push (request-parse-header-field line)
               (rq-header-fields request))))


(maybe-inline request-parse-get-query)
(defun request-parse-get-query (request)
  "Returns the QUERY part of an URL like ?id=1&value=2 in this form:
\((\"id\" \"1\") (\"value\" \"2\"))"
  (declare (request request)
           #.optimizations)
  (let ((url (rq-url request)))
    (when-let (query-string-position (position #\? url :test #'char=))
      (parse-query-str (subseq url (1+ query-string-position))))))
      

(maybe-inline request-parse-post-query)
(defun request-parse-post-query (request)
  "Returns the MESSAGE-BODY of a REQUEST in the same form as REQUEST-GET-QUERY does.
NOTE: This might modify the MESSAGE-BODY slot in REQUEST!"
  (declare (request request)
           #.optimizations)
  (parse-query-str (rq-message-body request)))


(maybe-inline request-path)
(defun request-path (request)
  (declare (request request)
           #.optimizations)
  (let* ((url (rq-url request))
         (query-string-position (or (position #\? url :test #'char=) (length url))))
    (make-array query-string-position :element-type 'base-char :displaced-to url)))
