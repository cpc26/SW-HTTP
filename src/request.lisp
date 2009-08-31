;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(maybe-inline request-parse-http-version)
(defn request-parse-http-version ((member :http-1.0 :http-1.1) ((str base-string)))
  (declare  #.optimizations)
  (if (muffle-compiler-note
        (string= #.(coerce "HTTP/1.1" 'simple-base-string) str))
      :http-1.1
      :http-1.0))


(maybe-inline request-parse-initial-line)
(defn request-parse-initial-line (((line base-string) (request request)))
  "Parse LINE (\"initial line\") and destructively modify REQUEST."
  (declare #.optimizations)
  (let ((method_url_version (cl-ppcre:split #\Space line :sharedp t :limit 3)))
    (declare (dynamic-extent method_url_version))
    (setf (rq-method request) (let ((method-str (first method_url_version)))
                                ;; TODO: Convert to a single test if we're only to support GET and POST?
                                (cond ((string= method-str "GET") :get)
                                      ((string= method-str "POST") :post)
                                      (t (error "REQUEST-PARSE-INITIAL-LINE: HTTP method not implemented yet: ~S"
                                                method-str))))
          (rq-url request) (second method_url_version)
          (rq-http-version request) (request-parse-http-version (third method_url_version)))
    (values)))


(maybe-inline request-parse-header-field)
(defn request-parse-header-field (cons ((line base-string)))
  (declare #.optimizations)
  (let ((key_value (cl-ppcre:split ":\\s*" line :limit 2 :sharedp t)))
    (setf (car key_value) (nstring-upcase (car key_value))
          (cdr key_value) (second key_value))
    key_value))


(maybe-inline request-parse-header-fields)
(defn request-parse-header-fields (((lines list) (request request)))
  "Parse LINES (HTTP request headers) and destructively modify REQUEST."
  (declare #.optimizations)
  (loop :for line :in lines
     :do (push (request-parse-header-field line)
               (rq-header-fields request)))
  (values))


(maybe-inline request-parse-get-query)
(defn request-parse-get-query (list ((request request)))
  "Returns the QUERY part of an URL like ?id=1&value=2 in this form:
\((\"id\" \"1\") (\"value\" \"2\"))"
  (declare  #.optimizations)
  (let ((url (rq-url request)))
    (when-let (query-string-position (position #\? url :test #'char=))
      (parse-query-str (subseq url (1+ query-string-position))))))


(maybe-inline request-parse-post-query)
(defn request-parse-post-query (list ((request request)))
  "Returns the MESSAGE-BODY of a REQUEST in the same form as REQUEST-GET-QUERY does.
NOTE: This might modify the MESSAGE-BODY slot in REQUEST!"
  (declare #.optimizations)
  (parse-query-str (rq-message-body request)))


(maybe-inline request-path)
(defn request-path (base-string ((request request)))
  (declare #.optimizations)
  (let* ((url (rq-url request))
         (query-string-position (or (position #\? url :test #'char=) (length url))))
    (make-array query-string-position :element-type 'base-char :displaced-to url)))
