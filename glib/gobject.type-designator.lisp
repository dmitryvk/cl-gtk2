(in-package :gobject.ffi)

(defctype g-type gsize)

(define-foreign-type g-type-designator ()
  ((mangled-p :initarg :mangled-p
              :reader g-type-designator-mangled-p
              :initform nil
              :documentation "Whether the type designator is mangled with G_SIGNAL_TYPE_STATIC_SCOPE flag"))
  (:documentation "Values of this CFFI foreign type identify the GType. GType is designated by a its name (a string) or a numeric identifier. Functions accept GType designators as a string or integer and return them as a string. Functions @fun{g-type-name} and @fun{g-type-from-name} are used to convert between name and numeric identifier.

Numeric identifier of GType may be different between different program runs. But string identifier of GType does not change.")
  (:actual-type g-type)
  (:simple-parser g-type-designator))

(defun unmangle-g-type (g-type)
  (logxor g-type (ldb (byte 1 0) g-type)));;subtract the G_SIGNAL_TYPE_STATIC_SCOPE

(defmethod translate-from-foreign (value (type g-type-designator))
  (g-type-name (if (g-type-designator-mangled-p type)
                   (unmangle-g-type value)
                   value)))

(defmethod translate-to-foreign (value (type g-type-designator))
  (etypecase value
    (string (g-type-from-name value))
    (integer value)
    (null 0)))

(defun g-type-numeric (g-type-designator)
  (etypecase g-type-designator
    (string (g-type-from-name g-type-designator))
    (integer g-type-designator)
    (null 0)))

(defun g-type-string (g-type-designator)
  (etypecase g-type-designator
    (string (g-type-name g-type-designator))
    (integer (g-type-name g-type-designator))
    (null nil)))

(defcfun (g-type-name "g_type_name") :string
  "Returns the name of a GType.@see{g-type-from-name}

Example:
@pre{
\(g-type-from-name \"GtkLabel\")
=> 7151952
\(g-type-name 7151952)
=> \"GtkLabel\"
}
@arg[type]{GType designator (see @class{g-type-designator})}
@return{a string}"
  (type g-type-designator))

(defcfun (g-type-from-name "g_type_from_name") g-type
  "Returns the numeric identifier of a GType by its name. @see{g-type-name}

Example:
@pre{
\(g-type-from-name \"GtkLabel\")
=> 7151952
\(g-type-name 7151952)
=> \"GtkLabel\"
}
@arg[name]{a string - name of GType}
@return{an integer}"
  (name :string))

(defun g-type= (type-1 type-2)
  (= (g-type-numeric type-1)
     (g-type-numeric type-2)))

(defun g-type/= (type-1 type-2)
  (/= (g-type-numeric type-1)
      (g-type-numeric type-2)))
