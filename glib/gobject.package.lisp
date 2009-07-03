(defpackage :gobject
  (:use :cl :glib :cffi :tg :bordeaux-threads :iter :closer-mop)
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
           #:release*
           #:disown-boxed-ref
           #:pointer
           #:g-type-interface
           #:g-value
           #:register-object-type-implementation
           #:ensure-g-type
           #:define-vtable
           #:g-type
           #:set-g-value
           #:parse-gvalue
           #:emit-signal
           #:g-value-unset
           #:g-value-zero
           #:g-value-take-boxed
           #:g-value-init
           #:g-class-property-definition
           #:g-class-property-definition-name
           #:g-class-property-definition-type
           #:g-class-property-definition-readable
           #:g-class-property-definition-writable
           #:g-class-property-definition-constructor
           #:g-class-property-definition-constructor-only
           #:g-class-property-definition-owner-type
           #:g-type-class-ref
           #:g-object-class
           #:gobject-class
           #:g-param-spec
           #:type-instance
           #:parse-g-param-spec
           #:g-type-class-unref
           #:registered-object-type-by-name
           #:g-type-children
           #:g-signal-lookup))

(in-package :gobject)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library gobject
    (:unix (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
    (t "libgobject-2.0")))

(use-foreign-library gobject)

(defvar *gobject-debug* nil)

(defun debugf (&rest args)
  (when *gobject-debug*
    (apply 'format t args)))