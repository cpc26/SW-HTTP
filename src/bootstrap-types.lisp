;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defclass server (event-base)
  ((running-p :reader running-p-of
              :type (member t nil)
              :initform nil)

   (default-mime-type :accessor default-mime-type-of
                      :initarg :default-mime-type
                      :initform "text/plain")

   (port :reader port-of :initarg :port
         :type fixnum
         :initform (error ":PORT needed."))

   (socket :reader socket-of)

   (application-finder-fn :accessor application-finder-fn-of :initarg :application-finder-fn
                          :initform  (progn
                                       (warn ":APPLICATION-FINDER-FN not supplied; will always return DUMMY-APPLICATION.")
                                       'dummy-application))
   (connections :reader connections-of
                :type hash-table
                :initform (make-hash-table))

   (request-buffer :reader request-buffer-of
                   :type (simple-array (unsigned-byte 8) (#.+request-buffer-size+))
                   :initform (make-array +request-buffer-size+ :element-type '(unsigned-byte 8)))

   (thread :reader thread-of
           :type sb-thread:thread)))



(defstruct (request (:conc-name :rq-) (:copier nil))
  ;; Request line (or "initial line").
  (method nil :type (member nil :get :post))
  (url #.(coerce "" 'base-string) :type base-string)
  (http-version nil :type (member nil :http-1.0 :http-1.1))
  (header-fields nil :type list)
  (message-body #.(coerce "" 'base-string) :type base-string))



(defstruct (incoming-request (:conc-name :irq-) (:copier nil))
  (complete-request (make-request) :type request)

  (buffer (make-array +request-buffer-size+ :element-type '(unsigned-byte 8) :fill-pointer 0)
          :type (vector (unsigned-byte 8) #.+request-buffer-size+))

  (last-pos-of-buffer 0 :type fixnum)
  (status :handle-request-line-and-header-fields :type symbol))



(defstruct (connection (:conc-name :cn-) (:copier nil))
  (mutex (bt:make-lock) :type sb-thread:mutex)

  (socket (error ":SOCKET is needed.") :type iolib.sockets:socket)
  (server (error ":SERVER is needed.") :type server)

  (incoming-request (make-incoming-request) :type incoming-request)

  (incoming-request-queue (mk-queue) :type queue)
  (current-request nil :type (or null request))
  (sending-p nil :type (member t nil))

  (response (make-response) :type response)

  (close-p nil :type (member t nil)))
