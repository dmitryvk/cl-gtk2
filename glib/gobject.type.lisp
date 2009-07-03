(in-package :gobject)

(defcfun (g-type-fundamental "g_type_fundamental") g-type
  (type-id g-type))

(defcfun (%g-type-init "g_type_init") :void)

(at-init (%g-type-init))

(defcfun (g-type-name "g_type_name") :string
  (type g-type))

(defcfun (g-type-from-name "g_type_from_name") g-type
  (name :string))

(defcfun g-type-parent g-type
  (type g-type))

(defcfun g-type-depth :uint
  (type g-type))

(defcfun g-type-next-base g-type
  (leaf-type g-type)
  (root-type g-type))

(defcfun g-type-is-a :boolean
  (type g-type)
  (is-a-type g-type))

(defcfun g-type-class-ref (:pointer g-type-class)
  (type g-type))

(defcfun g-type-class-unref :void
  (class (:pointer g-type-class)))

(defcfun g-type-class-add-private :void
  (class (:pointer g-type-class))
  (private-size gsize))

(defcfun g-type-default-interface-ref :pointer
  (type g-type))

(defcfun g-type-default-interface-unref :void
  (interface :pointer))

(defcfun (%g-type-children "g_type_children") (:pointer g-type)
  (type g-type)
  (n-children (:pointer :uint)))

(defun g-type-children (g-type)
  (setf g-type (ensure-g-type g-type))
  (with-foreign-object (n-children :uint)
    (let ((g-types-ptr (%g-type-children g-type n-children)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-children :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interfaces "g_type_interfaces") (:pointer g-type)
  (type g-type)
  (n-interfaces (:pointer :uint)))

(defun g-type-interfaces (g-type)
  (setf g-type (ensure-g-type g-type))
  (with-foreign-object (n-interfaces :uint)
    (let ((g-types-ptr (%g-type-interfaces g-type n-interfaces)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interfaces :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(defcfun (%g-type-interface-prerequisites "g_type_interface_prerequisites") (:pointer g-type)
  (type g-type)
  (n-interface-prerequisites (:pointer :uint)))

(defun g-type-interface-prerequisites (g-type)
  (with-foreign-object (n-interface-prerequisites :uint)
    (let ((g-types-ptr (%g-type-interface-prerequisites g-type n-interface-prerequisites)))
      (prog1
          (loop
             for i from 0 below (mem-ref n-interface-prerequisites :uint)
             collect (mem-aref g-types-ptr 'g-type i))
        (g-free g-types-ptr)))))

(defcfun g-type-register-static g-type
  (parent-type g-type)
  (type-name :string)
  (info (:pointer g-type-info))
  (flags g-type-flags))

(defcfun g-type-register-static-simple g-type
  (parent-type g-type)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags g-type-flags))

(defcfun g-type-add-interface-static :void
  (instance-type g-type)
  (interface-type g-type)
  (info (:pointer g-interface-info)))

(defcfun g-type-interface-add-prerequisite :void
  (interface-type g-type)
  (prerequisite-type g-type))

(defun g-type-from-object (object)
  (g-type-from-instance object))

(defun g-type-from-class (g-class)
  (foreign-slot-value g-class 'g-type-class 'type))

(defun g-type-from-instance (type-instance)
  (g-type-from-class (foreign-slot-value type-instance 'g-type-instance 'class)))

(defun g-type-from-interface (type-interface)
  (foreign-slot-value type-interface 'g-type-interface 'type))

(defcfun g-strv-get-type g-type)

(g-strv-get-type)

(defcfun g-closure-get-type g-type)

(g-closure-get-type)

(defcfun g-type-query :void
  (type g-type)
  (query (:pointer g-type-query)))