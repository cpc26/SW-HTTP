;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(define-constant +crlf-octets+ (sb-ext:string-to-octets +crlf+) :test #'equalp)

(define-constant +http-blank-line+ (concatenate 'string +crlf+ +crlf+) :test #'string-equal)
(define-constant +http-blank-line-octets+ (sb-ext:string-to-octets +http-blank-line+) :test #'equalp)


(declaim (inline cons<-list))
(defun cons<-list (list)
  (declare #.optimizations
           (cons list))
  (setf (cdr list) (second list))
  list)


(defmacro with-sw-handlers (&body body)
  `(handler-bind (((or socket-error iolib.syscalls:epipe)
                   (lambda (c)
                     (warn "(WITH-SW-HANDLERS) Got condition: ~A. Will close and clean up this connection and all relevant objects and data and continue to listen for new connections." c)
                     (invoke-restart 'continue-listening)))
                  (error (lambda (c) (maybe-debug (cn-server *connection*) c))))
     ,@body))


(defmacro loop-until-eagain (&body body)
  "Repeats code in BODY until any networking or socket code in BODY would block.
Returns NIL when code starts blocking."
  `(handler-case (loop ,@body)
     (iolib.syscalls:ewouldblock ()
       nil)))


(defmacro maybe-inline (function)
  (if +inline-functions-p+
      `(declaim (inline ,function))
      nil))


;; TODO: Don't know about the connection type and it's associated (inline) functions.
;; Is SBCL smart enough to recompile this later?
(maybe-inline read-until-eagain)
(defun read-until-eagain (buffer connection)
  (declare ((vector (unsigned-byte 8)) buffer)
           (type connection connection))
  (let ((request-buffer (request-buffer-of (cn-server connection))))
    (loop-until-eagain
       (multiple-value-bind (%not-used num-bytes-read)
           (receive-from (cn-socket connection) :buffer request-buffer :end +request-buffer-size+)
         (declare (type fixnum num-bytes-read)
                  (ignore %not-used))
         (when (zerop num-bytes-read) ;; Peer closed connection.
           (invoke-restart 'continue-listening))
         (let* ((current-pos-of-buffer (fill-pointer buffer))
                (new-pos-of-buffer (+ current-pos-of-buffer num-bytes-read)))
           (declare (sb-ext:muffle-conditions sb-ext:compiler-note)) ;; Messages about BUFFER not being simple.
           (if (minusp (- (the fixnum (array-total-size buffer)) new-pos-of-buffer))
               ;; TODO: It might make sense adding som extra at the end here.
               (adjust-array buffer new-pos-of-buffer :fill-pointer new-pos-of-buffer)
               (setf (fill-pointer buffer) new-pos-of-buffer))
           (replace buffer request-buffer :start1 current-pos-of-buffer :end2 num-bytes-read))))))


(maybe-inline parse-query-str)
(defun parse-query-str (query-str &optional (start 0))
  (declare (base-string query-str)
           (fixnum start)
           #.optimizations)
  (let ((key_values (cl-ppcre:split #\& query-str :sharedp t :start start)))
    (declare (dynamic-extent key_values))
    (loop :for key_value :in key_values
       :collect (let ((key_value (cl-ppcre:split #\= key_value :sharedp t :limit 2)))
                  (setf (cdr key_value) (or (second key_value) ""))
                  key_value))))


;; Dumb, simple and fast(?).
(maybe-inline octets-to-simple-base-string)
(defun octets-to-simple-base-string (octets &key (end (length octets)))
  "Returns a string of type SIMPLE-BASE-STRING."
  (declare ((vector (unsigned-byte 8)) octets)
           (fixnum end)
           #.optimizations)
  (let ((str (make-string end :element-type 'base-char)))
    (loop :for i fixnum :below end
       :do (setf (schar str i) (code-char
                                (locally (declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                                  (aref octets i)))))
    str))


(defmacro with-simple-base-string-from-octets (octets string-symbol &body body)
  (with-gensyms (i end)
    (once-only (octets)
      `(let* ((,end (length ,octets))
              (,string-symbol (make-string ,end :element-type 'base-char)))
         (declare (dynamic-extent ,string-symbol))
         (loop :for ,i fixnum :below ,end
            :do (setf (schar ,string-symbol ,i) (code-char (aref ,octets ,i))))
         ,@body))))


(declaim (inline %get-header))
(defun %get-header (key request-header-fields)
  (declare (string key)
           (list request-header-fields)
           #.optimizations)
  (cdr (assoc key request-header-fields :test #'string=)))







;;; Based on something from pkhuong on #lisp ( here: http://paste.lisp.org/display/67320 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
(defun opaque-identity (x) x)

(defun ub8-to-bc (ub8-vector)
  (declare (type (simple-array (unsigned-byte 8) 1) ub8-vector))
  (princ (type-of ub8-vector)) (terpri)
  (setf (sb-kernel:%raw-bits ub8-vector 0) sb-vm:simple-base-string-widetag)
  (when (zerop (sb-kernel:%raw-bits ub8-vector 1))
    (error "Must pass a null-terminated vector. Received an empty vector"))
  (decf (sb-kernel:%raw-bits ub8-vector 1) (sb-vm:fixnumize 1))
  (values (sb-ext:truly-the simple-base-string
                            (opaque-identity ub8-vector))))
|#
