;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defmacro with-swh-context (connection (&key
                                        parse-get-parameters-p get-parameters-url-decode-p
                                        parse-post-parameters-p post-parameters-url-decode-p
                                        parse-cookies-p)
                            &body body)
  "Setup the context needed to deal with stuff in a \"nice way\"."
  (let ((context-bindings nil))
    (when parse-get-parameters-p
      (push '(*get-parameters* (get-parameters *connection*)) context-bindings))
    (when parse-post-parameters-p
      (push '(*post-parameters* (post-parameters *connection*)) context-bindings))
    (when parse-cookies-p
      (push '(*cookies* (cookies *connection*)) context-bindings))

    `(let* ((*connection* ,connection)
            ,@context-bindings)
       (declare (connection *connection*))
       ,(when get-parameters-url-decode-p
         `(dolist (get-parameter *get-parameters*)
            (setf (cdr get-parameter)
                  (url-decode (cdr get-parameter)))))
       ,(when post-parameters-url-decode-p
         `(dolist (post-parameter *post-parameters*)
            (setf (cdr post-parameter)
                  (url-decode (cdr post-parameter)))))
       (with-sw-handlers
         ,@body))))


;; TODO: This needs more work; the :EXPIRE keyarg in particular.
#|(declaim (inline set-cookie))|#
#|(defun set-cookie (name value &key expire path domain secure-p (response (cn-outgoing-response *connection*)))
  (declare #.optimizations
           (response response))
  (push (format nil #.(catstr "Set-Cookie: "
                              "~A=~A"
                              "~@[; expire=~A~]"
                              "~@[; path=~A~]"
                              "~@[; domain=~A~]"
                              "~@[; secure~*~]")
                name value expire path domain secure-p)
        (rs-header-fields response))
  (values))|#


(declaim (inline cookies))
(defun cookies (&optional (connection *connection*))
  (declare #.optimizations
           (connection connection))
  (loop :for key_value :in (cl-ppcre:split ";\\s*" (get-header :cookie connection) :sharedp t)
     :collect (cons<-list (cl-ppcre:split #\= key_value :sharedp t :limit 2))))


(declaim (inline get-cookie))
(defun get-cookie (name &optional (cookies *cookies*))
  (declare #.optimizations
           (string name)
           (list cookies))
  (muffle-compiler-note
    (cdr (assoc name cookies :test #'string=))))


(declaim (inline request))
(defun request (&optional (connection *connection*))
  "Returns the internal structure or representation of a HTTP request.
Normally you do not need to use this."
  (declare #.optimizations
           (connection connection))
  (cn-current-request connection))


#|(declaim (inline response))|#
#|(defun response (&optional (connection *connection*))
  "Returns the internal representation of a HTTP response.
Normally you do not need to use this."
  (declare #.optimizations
           (connection connection))
  (cn-outgoing-response connection))|#


(declaim (inline get-parameters))
(defun get-parameters (&optional (connection *connection*))
  (declare #.optimizations
           (connection connection))
  "This will return the \"query\" part of the URL.
Note that it will not url-decode the strings.
For performance, call this once then store the result for later; give it to
calls to GET-PARAMETER."
  (request-parse-get-query (request connection)))


(declaim (inline post-parameters))
(defun post-parameters (&optional (connection *connection*))
  "This will return the message-body of a POST-request in the same form as in a
call to GET-PARAMETERS.
Note that it will not url-decode the strings.
For performance, call this once then store the result for later; give it to
calls to POST-PARAMETER."
  (declare #.optimizations
           (connection connection))
  (request-parse-post-query (request connection)))


(declaim (inline get-parameter))
(defun get-parameter (key &optional (get-parameters *get-parameters*))
  "If the URL is \"http://domain.com/app?id=1&empty=&value=2\" then:
 (get-parameter \"id\") => \"1\"
 (get-parameter \"empty\") => NIL
 (get-parameter \"value\") => \"2\"

GET-PARAMETERS is the result of a call to GET-PARAMETERS."
  (declare #.optimizations
           (string key)
           (list get-parameters))
  ;; TODO: I get a optimization notice from SBCL here; I guess I should declare the type of GET-PARAMETERS more clearly.
  (cdr (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
         (find key get-parameters :key #'first :test #'string=))))


(declaim (inline post-parameter))
(defun post-parameter (key &optional (post-parameters *post-parameters*))
  "This works the same way as GET-PARAMETER, only for HTTP requests of type POST.
POST-PARAMETERS is the result of POST-PARAMETERS."
  (declare #.optimizations
           (string key)
           (list post-parameters)
           (inline get-parameter))
  (get-parameter key post-parameters))


(declaim (inline path))
(defun path (&optional (connection *connection*))
  "The part of the URL after what is returned by (HOST) and before the ? character.
\"http://domain.com/hello?id=1234\" would return \"/hello\"."
  (declare #.optimizations
           (connection connection))
  (request-path (request connection)))


(declaim (inline url))
(defun url (&optional (connection *connection*))
  (declare #.optimizations
           (connection connection))
  ;; TODO: What about https?
  (catstr "http://" (host) (rq-url (request connection))))


(declaim (inline headers-in))
(defun headers-in (&optional (connection *connection*))
  "Returns HTTP header-fields of request as an alist.
The key-part is an uppercase string."
  (declare #.optimizations
           (connection connection))
  (rq-header-fields (request connection)))


(declaim (inline get-header))
(defun get-header (header-key &optional (connection *connection*))
  "Returns header from HTTP request denoted by keyword HEADER-KEY as a string
or keyword.
Returns NIL if no header with HEADER-KEY was found."
  (declare #.optimizations
           ((or symbol string) header-key)
           (connection connection))
  (%get-header (etypecase header-key
                 (string (string-upcase header-key))
                 (symbol (string header-key)))
               (rq-header-fields (request connection))))


(declaim (inline host))
(defun host (&optional (connection *connection*))
  "HTTP \"Host\" header-field.
\"http://domain.com/hello?id=1234\" would return \"domain.com\"."
  (declare #.optimizations
           (connection connection))
  (get-header :host connection))


(declaim (inline host-no-port))
(defun host-no-port (&optional (connection *connection*))
  "Same as HOST except this will return \"localhost\" where HOST would
return \"localhost:6001\" where URL was \"http://localhost:6001/\""
  (declare #.optimizations
           (inline host)
           (connection connection))
  (first (cl-ppcre:split #\: (host connection) :limit 2 :sharedp t)))


(declaim (inline subdomain))
(defun subdomain (&optional (connection *connection*))
  "Given `sw.nostdal.org' this returns `sw'.
Given `nostdal.org' this returns NIL."
  (declare #.optimizations
           (connection connection))
  (let ((host (host connection)))
    (declare (base-string host))
    (if-let (pos (position #\. host :from-end t :end (position #\. host :from-end t)))
      (mk-array-view host :end pos)
      nil)))


(declaim (inline http-method))
(defun http-method (&optional (connection *connection*))
  "Returns :POST or :GET."
  (declare #.optimizations
           (connection connection))
  (rq-method (request connection)))


(declaim (inline http-version))
(defun http-version (&optional (connection *connection*))
  "Returns NIL (not known, yet), :HTTP-1.0 or :HTTP-1.1."
  (declare #.optimizations
           (connection connection))
  (rq-http-version (request connection)))
(export 'http-version)


(declaim (inline http-url))
(defun http-url (&optional (connection *connection*))
  (declare (connection connection)
           #.optimizations)
  (rq-url (request connection)))


(declaim (inline done-generating-response))
(defun done-generating-response (&optional (connection *connection*))
  (declare (connection connection)
           (inline connection-done-generating-response)
           #.optimizations)
  (connection-done-generating-response connection))


(maybe-inline browser-type)
(defun browser-type (&optional (connection *connection*))
  "Returns NIL if the browser type used is unknown.
The user should parse the result of the \"user-agent\" header himself then."
  (declare #.optimizations
           (connection connection))
  (let ((user-agent (the string (get-header "user-agent" connection))))
    (or (when (search "KHTML" user-agent) :khtml)
        (when (search "Gecko" user-agent) :gecko)
        (when (search "MSIE" user-agent) :msie)
        (when (or (search "Presto" user-agent)
                  (search "Opera" user-agent)) :presto)
        nil)))


(declaim (inline close-connection-p))
(defun close-connection-p (&optional (connection *connection*))
  (declare (connection connection)
           #.optimizations)
  (cn-close-p connection))


(declaim (inline (setf close-connection-p)))
(defun (setf close-connection-p) (pred &optional (connection *connection*))
  "If PRED is given a T value the server will close the connection/socket to
the client when it is done sending the respons. Some proxies require for the
back-end servers to do this."
  (declare (connection connection)
           #.optimizations)
  (setf (cn-close-p connection) pred))


(declaim (inline response-add-chunk))
(defun response-add-chunk (chunk &optional (response (cn-response *connection*)))
  (declare (octets chunk))
  (queue-push (rs-chunks response) chunk))
(export 'response-add-chunk)
