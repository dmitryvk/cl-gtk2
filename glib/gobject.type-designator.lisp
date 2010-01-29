(in-package :gobject.ffi)

(defctype g-type gsize)

(defstruct gtype name %id)

(defvar *name-to-gtype* (make-hash-table :test 'equal))
(defvar *id-to-gtype* (make-hash-table))
(defvar *gtype-lock* (bt:make-lock "gtype lock"))

(defun invalidate-gtypes ()
  (bt:with-lock-held (*gtype-lock*)
    (clrhash *id-to-gtype*)
    (iter (for (name gtype) in-hashtable *name-to-gtype*)
          (setf (gtype-%id gtype) nil))))

(at-finalize () (invalidate-gtypes))

(defcfun (%g-type-from-name "g_type_from_name") g-type
  (name :string))

(defcfun (%g-type-name "g_type_name") (:string :free-from-foreign nil)
  (type g-type))

(defun gtype-from-name (name)
  (declare (optimize (safety 0) (speed 3)))
  (when (null name) (return-from gtype-from-name nil))
  (bt:with-lock-held (*gtype-lock*)
    (let ((type (gethash name *name-to-gtype*)))
      (when type
        (when (null (gtype-%id type))
          (let ((n (%g-type-from-name name)))
            (if (zerop n)
                (warn "GType ~A is not known to GObject" name)
                (progn
                  (setf (gtype-%id type) n
                        (gethash n *id-to-gtype*) type)))))
        (return-from gtype-from-name type)))
    (let ((n (%g-type-from-name name)))
      (when (zerop n)
        (warn "GType ~A is not known to GObject" name)
        (setf n nil))
      (let ((type (make-gtype :name (copy-seq name) :%id n)))
        (setf (gethash n *id-to-gtype*) type
              (gethash name *name-to-gtype*) type)
        (return-from gtype-from-name type)))))

(defun gtype-from-id (id)
  (declare (optimize (safety 0) (speed 3)))
  (when (zerop id) (return-from gtype-from-id nil))
  (bt:with-lock-held (*gtype-lock*)
    (let ((type (gethash id *id-to-gtype*)))
      (when type
        (return-from gtype-from-id type)))
    (let ((name (%g-type-name id)))
      (unless name
        (error "GType with ~A is not known to GObject" id))
      (let ((type (gethash name *name-to-gtype*)))
        (when type
          (setf (gtype-%id type) id
                (gethash id *id-to-gtype*) type)
          (return-from gtype-from-id type))
        (let ((type (make-gtype :name name :%id id)))
          (setf (gethash id *id-to-gtype*) type
                (gethash name *name-to-gtype*) type)
          (return-from gtype-from-id type))))))

(defun gtype-id (gtype)
  (when (null gtype) (return-from gtype-id 0))
  (when (gtype-%id gtype) (return-from gtype-id (gtype-%id gtype)))
  (bt:with-lock-held (*gtype-lock*)
    (let ((n (%g-type-from-name (gtype-name gtype))))
      (when (zerop n)
        (warn "GType ~A is not known to GObject" (gtype-name gtype))
        (return-from gtype-id 0))
      (setf (gtype-%id gtype) n
            (gethash n *id-to-gtype*) gtype)
      n)))

(defun %gtype (thing)
  (etypecase thing
    (null nil)
    (gtype thing)
    (string (gtype-from-name thing))
    (integer (gtype-from-id thing))))

(defun gtype (thing)
  (%gtype thing))

(define-compiler-macro gtype (&whole whole thing)
  (if (constantp thing)
      `(load-time-value (%gtype ,thing))
      whole))

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
