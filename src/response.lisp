;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defstruct (response (:conc-name :rs-) (:copier nil))
  (chunks (mk-queue) :type queue)
  (chunk-buffer-pos 0 :type fixnum))


(defn mk-response-status-code (octets ((status-code fixnum)))
  (declare #.optimizations)
  (ecase status-code
    (200 #.(sb-ext:string-to-octets (catstr "HTTP/1.1 200 OK" +crlf+)))
    (404 #.(sb-ext:string-to-octets (catstr "HTTP/1.1 404 Not Found" +crlf+)))))
(export 'mk-response-status-code)


(defn mk-response-header-field (octets ((header-field string)))
  (declare #.optimizations)
  (sb-ext:string-to-octets (format nil "~A~A" header-field +crlf+)
                           :external-format :ascii))
(export 'mk-response-header-field)


(defn mk-response-message-body (octets ((message-body string)))
  (declare #.optimizations)
  (sb-ext:string-to-octets (format nil "Content-Length: ~D~A~A~A"
                                   (length message-body) +crlf+ +crlf+
                                   message-body)
                           :external-format :utf-8))
(export 'mk-response-message-body)


(maybe-inline response-handle)
(defn response-handle ((member nil t) ((connection connection)))
  "..or \"handle the response\".
Returns NIL if there is more to send."
  (declare #.optimizations)
  (let* ((response (cn-response connection))
         (chunks (rs-chunks response))
         (socket (cn-socket connection)))
    (loop-until-eagain
       (loop :for chunk = (queue-peek chunks)
          :while chunk
          ;; TODO: Alot of TRULY-THE's here; this is a bit unsafe.
          :do (when (zerop (- (length (truly-the octets chunk))
                              ;; TODO: Ugly. Should be replaced by a simple INCF, but INCF and/or SETF vs. a struct
                              ;; slot doesn't handle types 100% atm.
                              (with1 (truly-the fixnum (+ (rs-chunk-buffer-pos response)
                                                          (truly-the fixnum (send-to socket chunk
                                                                                     :start (rs-chunk-buffer-pos
                                                                                             response)))))
                                (setf (rs-chunk-buffer-pos response) it))))
                (setf (rs-chunk-buffer-pos response) 0)
                (queue-pop chunks))
          :finally (return-from response-handle t)))))
