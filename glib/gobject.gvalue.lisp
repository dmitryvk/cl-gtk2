(in-package :gobject)

(defcfun g-value-init (:pointer g-value)
  (value (:pointer g-value))
  (type g-type))

(defun g-value-zero (g-value)
  (loop
     for i from 0 below (foreign-type-size 'g-value)
     do (setf (mem-ref g-value :uchar i) 0)))

(defcfun g-value-copy :void
  (src-value (:pointer g-value))
  (dst-value (:pointer g-value)))

(defcfun g-value-reset (:pointer g-value)
  (value (:pointer g-value)))

(defcfun g-value-unset (:pointer g-value)
  (value (:pointer g-value)))

(defcfun g-value-set-instance :void
  (value (:pointer g-value))
  (instance :pointer))

(defcfun g-strdup-value-contents :string
  (value (:pointer g-value)))

