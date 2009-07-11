(in-package :gobject)

(defcfun g-boxed-copy :pointer
  (boxed-type g-type-designator)
  (src-boxed :pointer))

(defcfun g-boxed-free :void
  (boxed-type g-type-designator)
  (boxed :pointer))

(defcfun g-boxed-type-register-static g-type-designator
  (name :string)
  (copy-fn :pointer)
  (free-fn :pointer))

(defcfun g-pointer-type-register-static g-type-designator
  (name :string))