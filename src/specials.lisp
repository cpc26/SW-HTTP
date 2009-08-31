;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(define-variable *post-parameters*
    :value nil
    :type list)

(define-variable *get-parameters*
    :value nil
    :type list)

(define-variable *cookies*
    :value nil
    :type list)
