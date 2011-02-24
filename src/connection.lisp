;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(define-variable *connection*
    :value nil
    :type (or null connection))


(defn cn-outgoing-response-busy-p (t ((connection connection)))
  "Anything currently writing or reading to/from OUTGOING-RESPONSE slot in CONNECTION?"
  (declare #.optimizations)
  (or (cn-sending-p connection)
      (cn-current-request connection)))


(defun connection-add-request-to-queue (connection request)
  (declare (connection connection)
           (type request request)
           #.optimizations)
  (queue-push (cn-incoming-request-queue connection) request))


(defun connection-extract-next-request (connection)
  (declare (connection connection)
           #.optimizations)
  (setf (cn-current-request connection)
        (queue-pop (cn-incoming-request-queue connection))))


(defun connection-handle-request-queue (connection)
  (declare (connection connection)
           #.optimizations)
  (bt:acquire-lock (cn-mutex connection))
  (if (and (not (cn-outgoing-response-busy-p connection))
           (connection-extract-next-request connection))
      (progn
        (bt:release-lock (cn-mutex connection))
        (server-find-application (cn-server connection) connection))
      (bt:release-lock (cn-mutex connection))))


(defun connection-done-generating-response (connection)
  (declare (connection connection)
           #.optimizations)
  (with-sw-handlers
    (if (socket-connected-p (cn-socket connection))
        (bt:with-lock-held ((cn-mutex connection))
          (setf (cn-current-request connection) nil)
          (connection-start-sending connection))
        (warn "SW-HTTP, CONNECTION-DONE-GENERATING-RESPONSE: No longer connected."))))


(defun connection-done-sending-response (connection)
  (declare (connection connection)
           #.optimizations)
  (connection-stop-sending connection)
  (connection-handle-request-queue connection))


(defun connection-done-reading-request (connection request)
  (declare (connection connection)
           (type request request)
           #.optimizations)
  (connection-add-request-to-queue connection request)
  (connection-handle-request-queue connection))


(defun connection-start-sending (connection)
  (declare (connection connection)
           #.optimizations)
  (setf (cn-sending-p connection) t)
  (set-io-handler (cn-server connection) (fd-of (cn-socket connection)) :write
                  (lambda (fd event errorp)
                    (declare (ignore fd))
                    (connection-handle connection (cn-server connection) event errorp))))


(defun connection-stop-sending (connection)
  (declare (connection connection)
           #.optimizations)
  (when-let (event (cn-sending-p connection))
    (remove-fd-handlers (cn-server connection) (fd-of (cn-socket connection)) :write t)
    (setf (cn-sending-p connection) nil)))


(defun connection-start-reading (connection)
  (declare (connection connection)
           #.optimizations)
  (set-io-handler (cn-server connection) (fd-of (cn-socket connection)) :read
                  (lambda (fd event errorp)
                    (declare (ignore fd))
                    (connection-handle connection (cn-server connection) event errorp))))


(defun connection-destroy (connection)
  (declare (connection connection)
           #.optimizations)
  (remove-fd-handlers (cn-server connection) (fd-of (cn-socket connection)))
  (close (cn-socket connection))
  (server-remove-connection (cn-server connection) connection))


(defun connection-handle (connection server event errorp)
  "..or \"handle the connection\"."
  (declare (connection connection)
           (server server)
           (symbol event)
           (ignore server errorp)
           #.optimizations)
  (let ((*connection* connection))
    (unwind-protect-case ()
        (with-sw-handlers
          (ecase event
            (:read (when-let (request (request-handle connection))
                     (connection-done-reading-request connection request)))

            (:write (when (response-handle connection)
                      ;; Lighttpd 1.4.x's mod_proxy does HTTP/1.0 and needs for the connection to close. :(
                      ;; 1.5.x does not require this though.
                      (if (cn-close-p connection)
                          (connection-destroy connection)
                          (connection-done-sending-response connection))))))
      (:abort
       (connection-destroy connection)))))
