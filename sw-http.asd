;;;; http://nostdal.org/ ;;;;

(defsystem sw-http
  :description "SW-HTTP - an EPOLL based server for Common Lisp"
  :long-description "Tusen på tusen av nydelige løpenoter og hvilenoter"
  :author "Lars Rune Nøstdal <larsnostdal@gmail.com>"
  :version "v0.1"
  :license "AGPLv3"

  :depends-on (:iolib
               :alexandria
               :cl-utilities
               :cl-ppcre
               :aromyxo)

  :serial t
  :components
  ((:module src
    :serial t
    :components
    ((:file "package")
     (:file "config")
     (:file "specials")
     (:file "bootstrap-types")
     (:file "utils")
     (:file "buffer")
     (:file "server")
     (:file "connection")
     (:file "request")
     (:file "request-incoming")
     (:file "response")
     #|(:file "mime-types")|#
     (:file "user-api")
     ))))
