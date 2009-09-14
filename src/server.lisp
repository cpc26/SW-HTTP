;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(defmethod maybe-debug ((server server) condition)
  (invoke-debugger condition))


(maybe-inline server-find-application)
(defun server-find-application (server connection)
  (declare (server server)
           (type connection connection)
           #.optimizations)
  (funcall (the function (application-finder-fn-of server)) server connection))


(maybe-inline server-add-connection)
(defun server-add-connection (server connection)
  (declare (server server)
           (type connection connection)
           #.optimizations)
  (setf (gethash (cn-socket connection)
                 (connections-of server))
        connection))


(maybe-inline server-remove-connection)
(defun server-remove-connection (server connection)
  (declare (server server)
           (type connection connection)
           #.optimizations)
  (remhash (cn-socket connection)
           (connections-of server)))


(maybe-inline handle-socket-event)
(defun handle-socket-event (server)
  (declare (server server)
           #.optimizations)
  (let ((client-socket (accept-connection (socket-of server))))
    (setf (fd-non-blocking client-socket) t)
    (let ((connection (make-connection :server server :socket client-socket)))
      (server-add-connection server connection)
      (connection-start-reading connection))))


(defmethod start-listening ((server server) &optional (as-thread-p t))
  (unless (running-p-of server)
    (with-slots (socket running-p) server
      (setf socket (make-socket :local-host +any-host+
                                :connect :passive
                                :type :stream
                                :address-family :ipv4
                                :reuse-address t
                                :local-port (port-of server))
            (fd-non-blocking socket) t
            running-p t)
      (set-io-handler server (fd-of socket) :read
                      (lambda (fd event errorp)
                        (declare (ignore fd event errorp))
                        (handle-socket-event server)))
      (set-error-handler server (fd-of socket)
                         (lambda (fd event)
                           (declare (ignore fd event))
                           (iolib.sockets::signal-socket-error (socket-option (socket-of server) :error)))))
    (flet ((do-it ()
             (unwind-protect
                  (loop :until (eq :real-exit
                                   (with-simple-restart (continue-listening "(START-LISTENING): Skip whatever went wrong now and listen for another/more IO events.")
                                     (event-dispatch server)
                                     :real-exit)))
               (stop server))))
      (declare (inline do-it))
      (if as-thread-p
          (setf (slot-value server 'thread) (bt:make-thread #'do-it :name (princ-to-string server)))
          (do-it)))))
(export 'start-listening)


(defmethod stop ((server server))
  (when (running-p-of server)
    (remove-fd-handlers server (fd-of (socket-of server)))
    (close (socket-of server))
    (maphash (lambda (%not-used connection)
               (declare (ignore %not-used))
               (connection-destroy connection))
             (connections-of server))
    (when (slot-boundp server 'thread)
      (bt:destroy-thread (thread-of server))
      (slot-makunbound server 'thread))
    (setf (slot-value server 'running-p) nil)))
(export 'stop)