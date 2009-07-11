(in-package :gobject)

(define-foreign-type g-type-designator ()
  ()
  (:documentation "Values of this type identify the GType. GType is designated by a its name (a string) or a numeric identifier. Functions accept GType designators as a string or integer and return them as a string. Functions @fun{g-type-name} and @fun{g-type-from-name} are used to convert between name and numeric identifier.")
  (:actual-type g-type)
  (:simple-parser g-type-designator))

(defmethod translate-from-foreign (value (type g-type-designator))
  (g-type-name value))

(defmethod translate-to-foreign (value (type g-type-designator))
  (etypecase value
    (string (g-type-from-name value))
    (integer value)
    (null +g-type-invalid+)))

(defcfun (g-type-fundamental "g_type_fundamental") g-type-designator
  "Returns the fundamental type which is the ancestor of @code{type}.
@arg[type]{GType designator (see @class{g-type-fundamental})}
@return{GType designator}"
  (type g-type-designator))

(defcfun (%g-type-init "g_type_init") :void)

(at-init () (%g-type-init))

(defcfun (g-type-name "g_type_name") :string
  "Returns the name of a GType.

@arg[type]{GType designator (see @class{g-type-designator})}
@return{a string}"
  (type g-type-designator))

(defcfun (g-type-from-name "g_type_from_name") g-type
  "Returns the numeric identifier of a GType by its name

@arg[name]{a string - name of GType}
@return{an integer}"
  (name :string))

(defcfun g-type-parent g-type-designator
  "Returns the parent of a GType

@arg[type]{GType designator (see @class{g-type-designator})}
@return{GType designator}"
  (type g-type-designator))

(defcfun g-type-depth :uint
  "Returns the length of the ancestry of @code{type}. This includes the @code{type} itself, so that e.g. a fundamental type has depth 1.
@arg[type]{GType designator (see @class{g-type-designator})}
@return{an integer}"
  (type g-type-designator))

(defcfun g-type-next-base g-type-designator
  "Determines the type that is derived directly from @code{root-type} which is also a base class of @code{leaf-type}.
@arg[leaf-type]{GType designator (see @class{g-type-designator})}
@arg[root-type]{GType designator}
@return{GType designator}"
  (leaf-type g-type-designator)
  (root-type g-type-designator))

(defcfun g-type-is-a :boolean
  "If @code{is-a-type} is a derivable type, check whether type is a descendant of @code{is-a-type}. If @code{is-a-type} is an interface, check whether type conforms to it.
@arg[type]{GType designator (see @class{g-type-designator})}
@arg[is-a-type]{GType designator}
@return{boolean}"
  (type g-type-designator)
  (is-a-type g-type-designator))

(defcfun g-type-class-ref (:pointer g-type-class)
  (type g-type-designator))

(defcfun g-type-class-unref :void
  (class (:pointer g-type-class)))

(defcfun g-type-class-add-private :void
  (class (:pointer g-type-class))
  (private-size gsize))

(defcfun g-type-default-interface-ref :pointer
  (type g-type-designator))

(defcfun g-type-default-interface-unref :void
  (interface :pointer))

(defcfun (%g-type-children "g_type_children") (:pointer g-type)
  (type g-type-designator)
  (n-children (:pointer :uint)))

(defun g-type-children (g-type)
  "Returns the list of types inherited from @code{g-type}.

@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-children :uint)
    (let ((g-types-ptr (%g-type-children g-type n-children)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-children :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interfaces "g_type_interfaces") (:pointer g-type)
  (type g-type-designator)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (g-type)
  "Returns the list of interfaces the @code{g-type} conforms to.

@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-interfaces :uint)
    (let ((g-types-ptr (%g-type-interfaces g-type n-interfaces)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interfaces :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interface-prerequisites "g_type_interface_prerequisites") (:pointer g-type)
  (type g-type-designator)
  (n-interface-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (g-type)
  "Returns the prerequisites of an interface type. Prerequisite is a type that must be a superclass of an implementing class or an interface that the object must also implement.
@arg[g-type]{GType designator (see @class{g-type-designator})}
@return{list of GType designators}"
  (with-foreign-object (n-interface-prerequisites :uint)
    (let ((g-types-ptr (%g-type-interface-prerequisites g-type n-interface-prerequisites)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interface-prerequisites :uint)
             collect (mem-aref g-types-ptr 'g-type-designator i))
        (g-free g-types-ptr)))))

(defcfun g-type-register-static g-type-designator
  (parent-type g-type-designator)
  (type-name :string)
  (info (:pointer g-type-info))
  (flags g-type-flags))

(defcfun g-type-register-static-simple g-type-designator
  (parent-type g-type-designator)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags g-type-flags))

(defcfun g-type-add-interface-static :void
  (instance-type g-type-designator)
  (interface-type g-type-designator)
  (info (:pointer g-interface-info)))

(defcfun g-type-interface-add-prerequisite :void
  (interface-type g-type-designator)
  (prerequisite-type g-type-designator))

(defun g-type-from-object (object)
  "Returns the GType of an @code{object}

@arg[object]{C pointer to an object}
@return{GType designator (see @class{g-type-designator})}"
  (g-type-from-instance object))

(defun g-type-from-class (g-class)
  (g-type-name (foreign-slot-value g-class 'g-type-class 'type)))

(defun g-type-from-instance (type-instance)
  (g-type-from-class (foreign-slot-value type-instance 'g-type-instance 'class)))

(defun g-type-from-interface (type-interface)
  (g-type-name (foreign-slot-value type-interface 'g-type-interface 'type)))

(defcfun g-strv-get-type g-type-designator)

(at-init nil (g-strv-get-type))

(defcfun g-closure-get-type g-type-designator)

(at-init nil (g-closure-get-type))

(defcfun g-type-query :void
  (type g-type-designator)
  (query (:pointer g-type-query)))