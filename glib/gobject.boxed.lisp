(in-package :gobject)

(defcfun g-boxed-copy :pointer
  (boxed-type g-type)
  (src-boxed :pointer))

(defcfun g-boxed-free :void
  (boxed-type g-type)
  (boxed :pointer))

(defcfun g-boxed-type-register-static g-type
  (name :string)
  (copy-fn :pointer)
  (free-fn :pointer))

(defcfun g-pointer-type-register-static g-type
  (name :string))