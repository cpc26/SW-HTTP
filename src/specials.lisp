;;;; http://nostdal.org/ ;;;;

(in-package #:sw-http)


(define-variable *post-parameters*
    :kind :var
    :value nil
    :type list
    :always-boundp t)

(define-variable *get-parameters*
    :kind :var
    :value nil
    :type list
    :always-boundp t)

(define-variable *cookies*
    :kind :var
    :value nil
    :type list
    :always-boundp t)
