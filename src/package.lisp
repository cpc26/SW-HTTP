;;;; http://nostdal.org/ ;;;;

(defpackage #:sw-http
  (:use #:cl
        #:cl-utilities #:alexandria
        #:aromyxo
        #:iolib)

  (:shadowing-import-from #:alexandria
    #:copy-array
    #:compose
    #:with-unique-names
    #:once-only
    #:with-gensyms
    )

  (:nicknames #:swh)

  (:shadow #:remove))


(in-package #:sw-http)


(export
 '(with-swh-context
   maybe-debug
   generate-simple-response
   set-message-body
   set-status-code
   add-header
   set-cookie
   cookies
   get-cookie
   request
   response
   message-body
   get-parameters
   post-parameters
   get-parameter
   post-parameter
   path
   url
   headers-in
   get-header
   host
   host-no-port
   subdomain
   http-method
   http-version
   http-url
   browser-type
   done-generating-response
   close-connection-p
   continue-listening
   connection
   ))

