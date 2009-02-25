(in-package :glib)

(defctype quark-value-type :uint32)

(defcfun g-quark-from-string quark-value-type
  (string :string))

(defcfun g-quark-to-string :string
  (quark quark-value-type))

(define-foreign-type quark-type ()
  ()
  (:actual-type quark-value-type)
  (:simple-parser g-quark))

(defmethod translate-to-foreign (value (type quark-type))
  (g-quark-from-string value))

(defmethod translate-from-foreign (value (type quark-type))
  (g-quark-to-string value))