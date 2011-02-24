;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defn incoming-request-reset (null ((incoming-request incoming-request)))
  (declare #.optimizations)
  ;; We don't mess with the buffer here as it might contain data for the next request already.
  (setf (irq-status incoming-request) :handle-request-line-and-header-fields
        (irq-complete-request incoming-request) (make-request))
  (values))


(defn incoming-request-move-tail-of-buffer-to-beginning (null ((incoming-request incoming-request)
                                                               (tail-start fixnum) (tail-end fixnum)))
  "Move any \"tail data\" to beginning of buffer and set the
CURRENT-POS-OF-BUFFER and LAST-POS-OF-BUFFER slots to \"sane\" values ready for
reading more data."
  (declare #.optimizations)
  (let ((buffer (irq-buffer incoming-request))
        (tail-size (- tail-end tail-start)))
    (when (plusp tail-size)
      (muffle-compiler-note
        (replace buffer buffer :start2 tail-start :end2 tail-end)))
    (setf (fill-pointer buffer) tail-size
          (irq-last-pos-of-buffer incoming-request) 0))
  (values))


(defun incoming-request-read-until-blank-line (connection)
    "Returns NIL if there is more to read,
or position of blank line if a +BLANK-LINE-OCTETS+ sequence has been found."
    (declare (connection connection)
             #.optimizations)
    (let* ((incoming-request (cn-incoming-request connection))
           (buffer (irq-buffer incoming-request)))
      (read-until-eagain buffer connection)
      ;; "Look back" as we go along.
      (prog1 (muffle-compiler-note
               (search +http-blank-line-octets+ buffer
                       ;; Backstep a bit, as we might have a split (IO-block) when we did the last search.
                       :start2 (max 0 (- (irq-last-pos-of-buffer incoming-request)
                                         (length +http-blank-line-octets+)))
                       :end2 (fill-pointer buffer)))
        (when (< +request-headers-max-size+
                 (the fixnum (setf (irq-last-pos-of-buffer incoming-request) (fill-pointer buffer))))
          (error "INCOMING-REQUEST-READ-UNTIL-BLANK-LINE: Read more than +request-headers-max-size+ octets (~A)."
                 (fill-pointer buffer))))))


(defun incoming-request-handle-line-and-header-fields (connection)
  "Returns NIL if there is more to read."
  (declare (connection connection)
           #.optimizations)
  (when-let ((blank-line-pos (incoming-request-read-until-blank-line connection)))
    (locally (declare (fixnum blank-line-pos))
      (let* ((incoming-request (cn-incoming-request connection))
             (complete-request (irq-complete-request incoming-request)))
        ;; TODO: This split thingy isn't correct, but let's see if we can get away with it anyway.
        (let ((request-lines
               (cl-ppcre:split +crlf+
                               (octets-to-simple-base-string (irq-buffer incoming-request)
                                                             :end  (truly-the fixnum (+ blank-line-pos
                                                                                        (length +crlf+))))
                               :sharedp t)))
          (declare (dynamic-extent request-lines))
          (request-parse-initial-line (first request-lines) complete-request)
          (request-parse-header-fields (rest request-lines) complete-request)
          (incoming-request-move-tail-of-buffer-to-beginning
           incoming-request
           (truly-the fixnum (+ blank-line-pos
                                (length +http-blank-line-octets+)))
           (fill-pointer (irq-buffer incoming-request)))
          complete-request)))))


(defun incoming-request-prepare-to-read-message-body (incoming-request)
  (declare (incoming-request incoming-request)
           #.optimizations)
  ;; TODO: Should probably place this in utils.lisp and make user-api.lisp use it too.
  (let* ((request (irq-complete-request incoming-request))
         (request-header-fields (rq-header-fields request))
         (content-length (parse-integer (%get-header "CONTENT-LENGTH" request-header-fields)))
         ;;(content-type (%get-header "CONTENT-TYPE" request-header-fields)))
         )
    #|
    (unless (string= #.(coerce "application/x-www-form-urlencoded" 'simple-base-string)
                     content-type)
      (warn "INCOMING-REQUEST-PREPARE-TO-READ-MESSAGE-BODY: Content-type ~A not implemented yet." content-type))
    |#
    ;; NOTE: This is pretty non-obvious, but since we won't be needing this slot anymore we store content-length here.
    ;; See INCOMING-REQUEST-HANDLE-MESSAGE-BODY where it is used.
    (setf (irq-last-pos-of-buffer incoming-request) content-length)))


(defun incoming-request-handle-message-body (connection)
  "Returns NIL if there is more to read."
  (declare (connection connection)
           #.optimizations)
  (let* ((incoming-request (cn-incoming-request connection))
         (complete-request (irq-complete-request incoming-request))
         ;; See the `NOTE' in INCOMING-REQUEST-PREPARE-TO-READ-MESSAGE-BODY.
         (content-length (irq-last-pos-of-buffer incoming-request))
         (buffer (irq-buffer incoming-request)))
    (read-until-eagain buffer connection)
    (let ((remaining (- content-length (fill-pointer buffer))))
      (if (>= 0 remaining)
          (progn
            (setf (rq-message-body complete-request) (octets-to-simple-base-string buffer))
            (incoming-request-move-tail-of-buffer-to-beginning (cn-incoming-request connection)
                                                               content-length
                                                               (truly-the fixnum (+ (fill-pointer buffer)
                                                                                    (abs remaining))))
            complete-request)
          nil))))


(defun request-handle (connection)
  "..or \"handle the request\".
Returns NIL if there is more to read, or an instance of REQUEST if we have a complete request."
  (declare (connection connection)
           #.optimizations)
  (let ((incoming-request (cn-incoming-request connection)))
    (ecase (irq-status incoming-request)
      (:handle-request-line-and-header-fields
       (when-let (request (incoming-request-handle-line-and-header-fields connection))
         (if (eq :post (rq-method request))
             (progn
               (setf (irq-status incoming-request) :handle-request-message-body)
               (incoming-request-prepare-to-read-message-body incoming-request)
               ;; Keep going; we might have read in some of the message body already.
               (request-handle connection))
             (progn
               (incoming-request-reset incoming-request)
               request))))

      (:handle-request-message-body
       (when-let (request (incoming-request-handle-message-body connection))
         (incoming-request-reset incoming-request)
         request)))))
