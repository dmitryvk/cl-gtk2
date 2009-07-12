(in-package :gobject)

(defstruct g-class-property-definition
  "Structure describing property of a GObject class.

See accessor functions:
@itemize{
@item{@fun{g-class-property-definition-name}}
@item{@fun{g-class-property-definition-type}}
@item{@fun{g-class-property-definition-readable}}
@item{@fun{g-class-property-definition-writable}}
@item{@fun{g-class-property-definition-constructor}}
@item{@fun{g-class-property-definition-constructor-only}}
@item{@fun{g-class-property-definition-owner-type}}
}
"
  name
  type
  readable
  writable
  constructor
  constructor-only
  owner-type)

(defmethod print-object ((instance g-class-property-definition) stream)
  (if *print-readably*
      (call-next-method)
      (print-unreadable-object (instance stream)
        (format stream
                "PROPERTY ~A ~A.~A (flags:~@[~* readable~]~@[~* writable~]~@[~* constructor~]~@[~* constructor-only~])"
                (g-class-property-definition-type instance)
                (g-class-property-definition-owner-type instance)
                (g-class-property-definition-name instance)
                (g-class-property-definition-readable instance)
                (g-class-property-definition-writable instance)
                (g-class-property-definition-constructor instance)
                (g-class-property-definition-constructor-only instance)))))

(setf (documentation 'g-class-property-definition-name 'function)
      "Name of GObject class property. See @class{g-class-property-definition}.
@return{a string}")

(setf (documentation 'g-class-property-definition-type 'function)
      "Type of GObject class property. See @class{g-class-property-definition}.
@return{a GType (integer)}")

(setf (documentation 'g-class-property-definition-readable 'function)
      "Whether the GObject class property is readable. See @class{g-class-property-definition}.
@return{a boolean}")

(setf (documentation 'g-class-property-definition-writable 'function)
      "Whether the GObject class property is writable. See @class{g-class-property-definition}.
@return{a boolean}")

(setf (documentation 'g-class-property-definition-constructor 'function)
      "Whether the GObject class property can be set at object construction time. See @class{g-class-property-definition}.
@return{a boolean}")

(setf (documentation 'g-class-property-definition-constructor-only 'function)
      "Whether the GObject class property can only be set at object construction time. See @class{g-class-property-definition}.
@return{a boolean}")

(setf (documentation 'g-class-property-definition-owner-type 'function)
      "The GType on which this GObject class property was defined. See @class{g-class-property-definition}.
@return{a GType (integer)}")

(defun parse-g-param-spec (param)
  (let ((flags (foreign-slot-value param 'g-param-spec :flags)))
    (make-g-class-property-definition
     :name (foreign-slot-value param 'g-param-spec :name)
     :type (foreign-slot-value param 'g-param-spec :value-type)
     :readable (not (null (member :readable flags)))
     :writable (not (null (member :writable flags)))
     :constructor (not (null (member :construct flags)))
     :constructor-only (not (null (member :construct-only flags)))
     :owner-type (foreign-slot-value param 'g-param-spec :owner-type))))

(defmacro with-unwind ((var expr unwind-function) &body body)
  `(let ((,var ,expr))
     (unwind-protect (progn ,@body)
       (,unwind-function ,var))))

(defun class-properties (g-type)
  "@return{list of properties of GObject class @code{g-type}. Each property is described by an object of type @class{g-class-property-definition}.}
@arg[g-type]{an integer or a string specifying the GType}"
  (assert (g-type-is-a g-type +g-type-object+))
  (with-unwind (g-class (g-type-class-ref g-type) g-type-class-unref)
    (with-foreign-object (n-properties :uint)
      (with-unwind (params (g-object-class-list-properties g-class n-properties) g-free)
        (loop
           for i from 0 below (mem-ref n-properties :uint)
           for param = (mem-aref params :pointer i)
           collect (parse-g-param-spec param))))))

(defun class-property-info (g-type property-name)
  (with-unwind (g-class (g-type-class-ref g-type) g-type-class-unref)
    (let* ((param-spec (g-object-class-find-property g-class property-name)))
      (when param-spec (parse-g-param-spec param-spec)))))

(defun interface-properties (g-type)
  "@return{list of properties of GObject interface @code{g-type}. Each property is described by an object of type @class{g-class-property-definition}.}
@arg[g-type]{an integer or a string specifying the GType}"
  (assert (g-type-is-a g-type +g-type-interface+))
  (with-unwind (g-iface (g-type-default-interface-ref g-type) g-type-default-interface-unref)
    (with-foreign-object (n-properties :uint)
      (with-unwind (params (g-object-interface-list-properties g-iface n-properties) g-free)
        (loop
           for i from 0 below (mem-ref n-properties :uint)
           for param = (mem-aref params :pointer i)
           collect (parse-g-param-spec param))))))
