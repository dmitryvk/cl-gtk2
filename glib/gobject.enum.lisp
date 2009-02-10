(in-package :gobject)

(defcfun g-enum-register-static g-type
  (name :string)
  (static-values (:pointer g-enum-value)))

(defcfun g-flags-register-static g-type
  (name :string)
  (static-values (:pointer g-flags-value)))