;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(deftype buffer () '(vector (unsigned-byte 8)))


(defun mk-buffer (&optional (size +response-buffer-size+))
  (make-array size :element-type '(unsigned-byte 8) :fill-pointer 0))


(defun buffer-append-octets (buffer octets)
  (declare ((vector (unsigned-byte 8)) buffer)
           ((simple-array (unsigned-byte 8) (*)) octets)
           #.optimizations)
  (let* ((current-pos (fill-pointer buffer))
         (new-pos (truly-the fixnum (+ current-pos (length octets)))))
    (if (minusp (- (array-total-size buffer) new-pos))
        ;; TODO: It would probably make sense adding some extra at the end here.
        (adjust-array buffer new-pos :fill-pointer new-pos)
        (setf (fill-pointer buffer) new-pos))
    (muffle-compiler-note
      (replace buffer octets :start1 current-pos))))


(declaim (inline combine-buffers))
(defun combine-buffers (&rest chunks)
  "\"Finalizes\" chunks into one big chunk (without a fill-pointer)."
  (declare #.optimizations)
  (apply #'concatenate 'buffer
         chunks))
(export 'combine-buffers)
