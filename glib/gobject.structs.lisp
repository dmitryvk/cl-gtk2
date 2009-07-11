(in-package :gobject)

(defcstruct g-type-interface
  (type g-type)
  (instance-type g-type))

(defcstruct g-type-class
  (type g-type))

(defcstruct g-type-instance
  (class (:pointer g-type-class)))

(defcstruct g-type-info
  (class-size :uint16)
  (base-init-fn :pointer)
  (base-finalize-fn :pointer)
  (class-init-fn :pointer)
  (class-finalize-fn :pointer)
  (class-data :pointer)
  (instance-size :uint16)
  (n-preallocs :uint16)
  (instance-init-fn :pointer)
  (value-table :pointer))

(defcstruct g-type-query
  (type g-type)
  (type-name (:string :free-from-foreign nil))
  (class-size :uint)
  (instance-size :uint))

(defbitfield g-type-fundamental-flags
  :classed
  :instantiatable
  :derivable
  :deep-derivable)

(defcstruct g-type-fundamental-info
  (type-flags g-type-fundamental-flags))

(defcstruct g-interface-info
  (interface-init :pointer)
  (interface-finalize :pointer)
  (interface-data :pointer))

(defcstruct g-type-value-table
  (value-init :pointer)
  (value-free :pointer)
  (value-copy :pointer)
  (value-peek-pointer :pointer)
  (collect-format (:string :free-from-foreign nil :free-to-foreign nil))
  (collect-value :pointer)
  (lcopy-format (:string :free-from-foreign nil :free-to-foreign nil))
  (lcopy-value :pointer))

(defbitfield g-type-flags
  (:abstract #. (ash 1 4))
  :value-abstract)

(defcstruct %g-object
  (type-instance g-type-instance)
  (ref-count :uint)
  (data :pointer))

(defctype %g-initially-unowned %g-object)

(defcstruct g-object-class
  (type-class g-type-class)
  (construct-properties :pointer)
  (constructor :pointer)
  (set-property :pointer)
  (get-property :pointer)
  (dispose :pointer)
  (finalize :pointer)
  (dispatch-properties-changed :pointer)
  (notify :pointer)
  (constructed :pointer)
  (pdummy :pointer :count 7))

(defbitfield g-param-flags
  :readable
  :writable
  :construct
  :construct-only
  :lax-validation
  :static-name
  :nick
  :blurb)

(defcstruct g-param-spec
  (type-instance g-type-instance)
  (name (:string :free-from-foreign nil :free-to-foreign nil))
  (flags g-param-flags)
  (value-type g-type)
  (owner-type g-type))

(defcunion g-value-data
  (int :int)
  (uint :uint)
  (long :long)
  (ulong :ulong)
  (int64 :int64)
  (uint64 :uint64)
  (float :float)
  (double :double)
  (pointer :pointer))

(defcstruct g-value
  (type g-type)
  (data g-value-data :count 2))

(defcstruct g-object-construct-param
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcstruct g-parameter
  (name (:string :free-from-foreign nil :free-to-foreign nil))
  (value g-value))

(defcstruct g-enum-value
  (value :int)
  (name (:string :free-from-foreign nil :free-to-foreign nil))
  (nick (:string :free-from-foreign nil :free-to-foreign nil)))

(defcstruct g-enum-class
  (type-class g-type-class)
  (minimum :int)
  (maximum :int)
  (n-values :uint)
  (values (:pointer g-enum-value)))

(defcstruct g-flags-value
  (value :uint)
  (name (:string :free-from-foreign nil :free-to-foreign nil))
  (nick (:string :free-from-foreign nil :free-to-foreign nil)))

(defcstruct g-flags-class
  (type-class g-type-class)
  (mask :uint)
  (n-values :uint)
  (values (:pointer g-flags-value)))

(defcstruct g-param-spec-boolean
  (parent-instance g-param-spec)
  (default-value :boolean))

(defcstruct g-param-spec-char
  (parent-instance g-param-spec)
  (minimum :int8)
  (maximum :int8)
  (default-value :int8))

(defcstruct g-param-spec-uchar
  (parent-instance g-param-spec)
  (minimum :uint8)
  (maximum :uint8)
  (default-value :uint8))

(defcstruct g-param-spec-int
  (parent-instance g-param-spec)
  (minimum :int)
  (maximum :int)
  (default-value :int))

(defcstruct g-param-spec-uint
  (parent-instance g-param-spec)
  (minimum :uint)
  (maximum :uint)
  (default-value :uint))

(defcstruct g-param-spec-long
  (parent-instance g-param-spec)
  (minimum :long)
  (maximum :long)
  (default-value :ulong))

(defcstruct g-param-spec-ulong
  (parent-instance g-param-spec)
  (minimum :ulong)
  (maximum :ulong)
  (default-value :ulong))

(defcstruct g-param-spec-int64
  (parent-instance g-param-spec)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64))

(defcstruct g-param-spec-uint64
  (parent-instance g-param-spec)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64))

(defcstruct g-param-spec-float
  (parent-instance g-param-spec)
  (minimum :float)
  (maximum :float)
  (default-value :float)
  (epsilon :float))

(defcstruct g-param-spec-double
  (parent-instance g-param-spec)
  (minimum :double)
  (maximum :double)
  (default-value :double)
  (epsilon :double))

(defcstruct g-param-spec-enum
  (parent-instance g-param-spec)
  (enum-class (:pointer g-enum-class))
  (default-value :int))

(defcstruct g-param-spec-flags
  (parent-instance g-param-spec)
  (flags-class (:pointer g-flags-class))
  (default-value :uint))

(defcstruct g-param-spec-string
  (parent-instance g-param-spec)
  (default-value (:string :free-to-foreign nil :free-from-foreign nil))
  (cset-first (:string :free-to-foreign nil :free-from-foreign nil))
  (cset-nth (:string :free-to-foreign nil :free-from-foreign nil))
  (substitutor :char)
  (flags-for-null :uint))

(defcstruct g-param-spec-param
  (parent-instance g-param-spec))

(defcstruct g-param-spec-boxed
  (parent-instance g-param-spec))

(defcstruct g-param-spec-pointer
  (parent-instance g-param-spec))

(defcstruct g-param-spec-object
  (parent-instance g-param-spec))

(defcstruct g-param-spec-value-array
  (parent-instance g-param-spec)
  (element-spec (:pointer g-param-spec))
  (fixed-n-elements :uint))

(defcstruct g-param-spec-g-type
  (parent-instance g-param-spec)
  (types-root g-type))

(defcstruct g-param-spec-class
  (type-class g-type-class)
  (value-type g-type)
  (finalize :pointer)
  (value-set-default :pointer)
  (value-validate :pointer)
  (values-cmp :pointer))

(defcstruct g-closure
  (private-data :uint32)
  (marshal :pointer)
  (data :pointer)
  (notifiers :pointer))

(defcfun g-type-class-ref (:pointer g-type-class)
  (type g-type-designator))

(defcfun g-type-class-unref :void
  (class (:pointer g-type-class)))

(defcfun g-type-class-add-private :void
  (class (:pointer g-type-class))
  (private-size gsize))

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

(defcfun g-type-query :void
  (type g-type-designator)
  (query (:pointer g-type-query)))

(defcfun g-type-default-interface-ref :pointer
  (type g-type-designator))

(defcfun g-type-default-interface-unref :void
  (interface :pointer))