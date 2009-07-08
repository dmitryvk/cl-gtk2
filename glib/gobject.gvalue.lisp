(in-package :gobject)

(defcfun g-value-init (:pointer g-value)
  "Initializes the GValue @code{value} with the default value of @code{type}

@arg[value]{a C pointer to the GValue structure}
@arg[type]{an integer specifying the GType}"
  (value (:pointer g-value))
  (type g-type))

(defun g-value-zero (g-value)
  "Initializes the GValue in \"unset\" state.

@arg[g-value]{a C pointer to the GValue structure}"
  (loop
     for i from 0 below (foreign-type-size 'g-value)
     do (setf (mem-ref g-value :uchar i) 0)))

(defcfun g-value-copy :void
  (src-value (:pointer g-value))
  (dst-value (:pointer g-value)))

(defcfun g-value-reset (:pointer g-value)
  (value (:pointer g-value)))

(defcfun g-value-unset (:pointer g-value)
  "Clears the current value in @code{value} and \"unsets\" the type, releasing all resources associated with this GValue. An unset value is the same as an unitialized GValue.

@arg[value]{a C pointer to the GValue structure}"
  (value (:pointer g-value)))

(defcfun g-value-set-instance :void
  (value (:pointer g-value))
  (instance :pointer))

(defcfun g-strdup-value-contents :string
  (value (:pointer g-value)))

