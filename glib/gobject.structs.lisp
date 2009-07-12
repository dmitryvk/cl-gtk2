(in-package :gobject)

(defun g-type-from-object (object)
  "Returns the GType of an @code{object}

@arg[object]{C pointer to an object}
@return{GType designator (see @class{g-type-designator})}"
  (g-type-from-instance object))

(defun g-type-from-class (g-class)
  (g-type-name (foreign-slot-value g-class 'g-type-class :type)))

(defun g-type-from-instance (type-instance)
  (g-type-from-class (foreign-slot-value type-instance 'g-type-instance :class)))

(defun g-type-from-interface (type-interface)
  (g-type-name (foreign-slot-value type-interface 'g-type-interface :type)))

