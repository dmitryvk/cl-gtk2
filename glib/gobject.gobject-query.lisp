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
  (let ((flags (foreign-slot-value param 'g-param-spec 'flags)))
    (make-g-class-property-definition
     :name (foreign-slot-value param 'g-param-spec
                               'name)
     :type (foreign-slot-value param 'g-param-spec
                               'value-type)
     :readable (not (null (member :readable flags)))
     :writable (not (null (member :writable flags)))
     :constructor (not (null (member :construct flags)))
     :constructor-only (not (null (member :construct-only flags)))
     :owner-type (foreign-slot-value param 'g-param-spec
                                     'owner-type))))

(defun class-properties (g-type)
  "@return{list of properties of GObject class @code{g-type}. Each property is described by an object of type @class{g-class-property-definition}.}
@arg[g-type]{an integer or a string specifying the GType}"
  (setf g-type (ensure-g-type g-type))
  (let ((g-class (g-type-class-ref g-type)))
    (unwind-protect
         (with-foreign-object (n-properties :uint)
           (let ((params (g-object-class-list-properties g-class n-properties)))
             (unwind-protect
                  (loop
                     for i from 0 below (mem-ref n-properties :uint)
                     for param = (mem-aref params :pointer i)
                     collect (parse-g-param-spec param))
               (g-free params))))
      (g-type-class-unref g-class))))

(defun class-parent (type)
  (g-type-parent (ensure-g-type type)))

(defun interface-properties (g-type)
"@return{list of properties of GObject interface @code{g-type}. Each property is described by an object of type @class{g-class-property-definition}.}
@arg[g-type]{an integer or a string specifying the GType}"
  (setf g-type (ensure-g-type g-type))
  (let ((g-iface (g-type-default-interface-ref g-type)))
    (unwind-protect
         (with-foreign-object (n-properties :uint)
           (let ((params (g-object-interface-list-properties g-iface n-properties)))
             (unwind-protect
                  (loop
                     for i from 0 below (mem-ref n-properties :uint)
                     for param = (mem-aref params :pointer i)
                     for flags = (foreign-slot-value param 'g-param-spec 'flags)
                     collect (make-g-class-property-definition
                              :name (foreign-slot-value param 'g-param-spec
                                                        'name)
                              :type (foreign-slot-value param 'g-param-spec
                                                        'value-type)
                              :readable (not (null (member :readable flags)))
                              :writable (not (null (member :writable flags)))
                              :constructor (not (null (member :construct flags)))
                              :constructor-only (not (null (member :construct-only flags)))
                              :owner-type (foreign-slot-value param 'g-param-spec
                                                              'owner-type)))
               (g-free params))))
      (g-type-default-interface-unref g-iface))))

(defstruct enum-item
  "A structure describing a single enumeration item.

See accessor functions:
@itemize{
@item{@fun{enum-item-name}}
@item{@fun{enum-item-value}}
@item{@fun{enum-item-nick}}
}"
  name value nick)

(setf (documentation 'enum-item-name 'function)
      "The C name of enum item, e.g. \"GTK_WINDOW_TOPLEVEL\".
@return{a string}")

(setf (documentation 'enum-item-value 'function)
      "The numeric value of enum item.
@return{an integer}")

(setf (documentation 'enum-item-nick 'function)
      "The \"nickname\" of enum item. Nickname is a short name of enum item. E.g., \"toplevel\".
@return{a string}")

(defun get-enum-items (type)
  "Gets the list of enum items that belong to GEnum type @code{type}
@arg[type]{a string or an integer specifying GEnum type}
@return{a list of @class{enum-item} objects}"
  (let ((g-class (g-type-class-ref (ensure-g-type type))))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-enum-class 'n-values)
            with values = (foreign-slot-value g-class 'g-enum-class 'values)
            for i from 0 below n
            for enum-value = (mem-aref values 'g-enum-value i)
            collect (make-enum-item
                     :name (foreign-slot-value enum-value 'g-enum-value
                                               'name)
                     :value (foreign-slot-value enum-value 'g-enum-value
                                                'value)
                     :nick (foreign-slot-value enum-value 'g-enum-value
                                               'nick)))
      (g-type-class-unref g-class))))

(defstruct flags-item
  "A structure describing a single flags item.

See accessor functions:
@itemize{
@item{@fun{flags-item-name}}
@item{@fun{flags-item-value}}
@item{@fun{flags-item-nick}}
}"
  name value nick)

(setf (documentation 'flags-item-name 'function)
      "The C name of flags item, e.g. \"GDK_PROPERTY_CHANGE_MASK\".
@return{a string}")

(setf (documentation 'flags-item-value 'function)
      "The numeric value of flags item.
@return{an integer}")

(setf (documentation 'flags-item-nick 'function)
      "The \"nickname\" of flags item. Nickname is a short name of flags item. E.g., \"property-change-mask\".
@return{a string}")

(defun get-flags-items (type)
  "Gets the list of flags items that belong to GFlags type @code{type}
@arg[type]{a string or an integer specifying GFlags type}
@return{a list of @class{flags-item} objects}"
  (let ((g-class (g-type-class-ref (ensure-g-type type))))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-flags-class 'n-values)
            with values = (foreign-slot-value g-class 'g-flags-class 'values)
            for i from 0 below n
            for flags-value = (mem-aref values 'g-flags-value i)
            collect (make-flags-item
                     :name (foreign-slot-value flags-value 'g-flags-value
                                               'name)
                     :value (foreign-slot-value flags-value 'g-flags-value
                                                'value)
                     :nick (foreign-slot-value flags-value 'g-flags-value
                                               'nick)))
      (g-type-class-unref g-class))))
