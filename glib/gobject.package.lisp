(defpackage :gobject
  (:use :cl :glib :cffi :tg :bind :anaphora :bordeaux-threads :iter)
  (:export #:g-object
           #:register-object-type
           #:g-object-call-constructor
           #:register-flags-type
           #:register-enum-type
           #:g-type-from-object
           #:g-type-name
           #:g-type-from-name
           #:g-signal-connect
           #:define-g-object-class
           #:g-initially-unowned
           #:define-g-enum
           #:*lisp-name-package*
           #:define-g-boxed-class
           #:define-g-flags
           #:fixed-array
           #:g-boxed-inline
           #:g-boxed-ptr 
           #:boxed-c-structure-name
           #:define-g-interface
           #:release
           #:using
           #:using*
           #:define-g-boxed-ref
           #:g-boxed-ref
           #:allocate-stable-pointer
           #:free-stable-pointer
           #:get-stable-pointer-value
           #:with-stable-pointer
           #:release*))

(in-package :gobject)

(load-foreign-library "libgobject-2.0.so")

(defvar *gobject-debug* nil)

(defun debugf (&rest args)
  (when *gobject-debug*
    (apply 'format t args)))