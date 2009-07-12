(in-package :gobject.ffi)

(defctype g-type gsize)

(define-foreign-type g-type-designator ()
  ()
  (:documentation "Values of this CFFI foreign type identify the GType. GType is designated by a its name (a string) or a numeric identifier. Functions accept GType designators as a string or integer and return them as a string. Functions @fun{g-type-name} and @fun{g-type-from-name} are used to convert between name and numeric identifier.

Numeric identifier of GType may be different between different program runs. But string identifier of GType does not change.")
  (:actual-type g-type)
  (:simple-parser g-type-designator))

(defmethod translate-from-foreign (value (type g-type-designator))
  (g-type-name value))

(defmethod translate-to-foreign (value (type g-type-designator))
  (etypecase value
    (string (g-type-from-name value))
    (integer value)
    (null 0)))

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