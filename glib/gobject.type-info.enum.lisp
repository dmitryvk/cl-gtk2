(in-package :gobject)

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
  (assert (g-type-is-a type +g-type-enum+))
  (let ((g-class (g-type-class-ref type)))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-enum-class :n-values)
            with values = (foreign-slot-value g-class 'g-enum-class :values)
            for i from 0 below n
            for enum-value = (mem-aref values 'g-enum-value i)
            collect (make-enum-item
                     :name (foreign-slot-value enum-value 'g-enum-value
                                               :name)
                     :value (foreign-slot-value enum-value 'g-enum-value
                                                :value)
                     :nick (foreign-slot-value enum-value 'g-enum-value
                                               :nick)))
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
  (assert (g-type-is-a type +g-type-flags+))
  (let ((g-class (g-type-class-ref type)))
    (unwind-protect
         (loop
            with n = (foreign-slot-value g-class 'g-flags-class :n-values)
            with values = (foreign-slot-value g-class 'g-flags-class :values)
            for i from 0 below n
            for flags-value = (mem-aref values 'g-flags-value i)
            collect (make-flags-item
                     :name (foreign-slot-value flags-value 'g-flags-value
                                               :name)
                     :value (foreign-slot-value flags-value 'g-flags-value
                                                :value)
                     :nick (foreign-slot-value flags-value 'g-flags-value
                                               :nick)))
      (g-type-class-unref g-class))))
