(in-package :gobject)

(defcfun g-param-spec-boolean (:pointer g-param-spec-boolean)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :boolean)
  (flags g-param-flags))

(defcfun g-value-set-boolean :void
  (g-value (:pointer g-value))
  (new-value :boolean))

(defcfun g-value-get-boolean :boolean
  (g-value (:pointer g-value)))

(defcfun g-param-spec-char (:pointer g-param-spec-char)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int8)
  (maximum :int8)
  (default-value :int8)
  (flags g-param-flags))

(defcfun g-value-set-char :void
  (g-value (:pointer g-value))
  (new-value :char))

(defcfun g-value-get-char :char
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uchar (:pointer g-param-spec-uchar)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint8)
  (maximum :uint8)
  (default-value :uint8)
  (flags g-param-flags))

(defcfun g-value-set-uchar :void
  (g-value (:pointer g-value))
  (new-value :uchar))

(defcfun g-value-get-uchar :uchar
  (g-value (:pointer g-value)))

(defcfun g-param-spec-int (:pointer g-param-spec-int)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int)
  (maximum :int)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-int :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-int :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uint (:pointer g-param-spec-uint)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint)
  (maximum :uint)
  (default-value :uint)
  (flags g-param-flags))

(defcfun g-value-set-uint :void
  (g-value (:pointer g-value))
  (new-value :uint))

(defcfun g-value-get-uint :uint
  (g-value (:pointer g-value)))

(defcfun g-param-spec-long (:pointer g-param-spec-long)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :long)
  (maximum :long)
  (default-value :long)
  (flags g-param-flags))

(defcfun g-value-set-long :void
  (g-value (:pointer g-value))
  (new-value :long))

(defcfun g-value-get-long :long
  (g-value (:pointer g-value)))

(defcfun g-param-spec-ulong (:pointer g-param-spec-ulong)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :ulong)
  (maximum :ulong)
  (default-value :ulong)
  (flags g-param-flags))

(defcfun g-value-set-ulong :void
  (g-value (:pointer g-value))
  (new-value :ulong))

(defcfun g-value-get-ulong :ulong
  (g-value (:pointer g-value)))

(defcfun g-param-spec-int64 (:pointer g-param-spec-int64)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :int64)
  (maximum :int64)
  (default-value :int64)
  (flags g-param-flags))

(defcfun g-value-set-int64 :void
  (g-value (:pointer g-value))
  (new-value :int64))

(defcfun g-value-get-int64 :int64
  (g-value (:pointer g-value)))

(defcfun g-param-spec-uint64 (:pointer g-param-spec-uint64)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :uint64)
  (maximum :uint64)
  (default-value :uint64)
  (flags g-param-flags))

(defcfun g-value-set-uint64 :void
  (g-value (:pointer g-value))
  (new-value :uint64))

(defcfun g-value-get-uint64 :uint64
  (g-value (:pointer g-value)))

(defcfun g-param-spec-float (:pointer g-param-spec-float)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :float)
  (maximum :float)
  (default-value :float)
  (flags g-param-flags))

(defcfun g-value-set-float :void
  (g-value (:pointer g-value))
  (new-value :float))

(defcfun g-value-get-float :float
  (g-value (:pointer g-value)))

(defcfun g-param-spec-double (:pointer g-param-spec-double)
  (name :string)
  (nick :string)
  (blurb :string)
  (minimum :double)
  (maximum :double)
  (default-value :double)
  (flags g-param-flags))

(defcfun g-value-set-double :void
  (g-value (:pointer g-value))
  (new-value :double))

(defcfun g-value-get-double :double
  (g-value (:pointer g-value)))

(defcfun g-param-spec-enum (:pointer g-param-spec-enum)
  (name :string)
  (nick :string)
  (blurb :string)
  (enum-type g-type)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-enum :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-enum :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-flags (:pointer g-param-spec-flags)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags-type g-type)
  (default-value :int)
  (flags g-param-flags))

(defcfun g-value-set-flags :void
  (g-value (:pointer g-value))
  (new-value :int))

(defcfun g-value-get-flags :int
  (g-value (:pointer g-value)))

(defcfun g-param-spec-string (:pointer g-param-spec-string)
  (name :string)
  (nick :string)
  (blurb :string)
  (default-value :string)
  (flags g-param-flags))

(defcfun g-value-set-string :void
  (g-value (:pointer g-value))
  (new-value :string))

(defcfun g-value-get-string (:string :free-from-foreign nil)
  (g-value (:pointer g-value)))

(defcfun g-param-spec-param (:pointer g-param-spec-param)
  (name :string)
  (nick :string)
  (blurb :string)
  (param-type g-type)
  (flags g-param-flags))

(defcfun g-value-set-param :void
  (g-value (:pointer g-value))
  (new-value (:pointer g-param-spec)))

(defcfun g-value-get-param (:pointer g-param-spec)
  (g-value (:pointer g-value)))

(defcfun g-param-spec-boxed (:pointer g-param-spec-boxed)
  (name :string)
  (nick :string)
  (blurb :string)
  (boxed-type g-type)
  (flags g-param-flags))

(defcfun g-value-set-boxed :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-take-boxed :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-boxed :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-pointer (:pointer g-param-spec-pointer)
  (name :string)
  (nick :string)
  (blurb :string)
  (flags g-param-flags))

(defcfun g-value-set-pointer :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-pointer :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-object (:pointer g-param-spec-object)
  (name :string)
  (nick :string)
  (blurb :string)
  (object-type g-type)
  (flags g-param-flags))

(defcfun g-value-set-object :void
  (g-value (:pointer g-value))
  (new-value :pointer))

(defcfun g-value-get-object :pointer
  (g-value (:pointer g-value)))

(defcfun g-param-spec-value-array (:pointer g-param-spec-value-array)
  (name :string)
  (nick :string)
  (blurb :string)
  (element-spec (:pointer g-param-spec))
  (flags g-param-flags))

(defcfun (g-param-spec-g-type "g_param_spec_gtype") (:pointer g-param-spec-g-type)
  (name :string)
  (nick :string)
  (blurb :string)
  (types-root g-type)
  (flags g-param-flags))

(defcfun (g-value-set-g-type "g_value_set_gtype") :void
  (g-value (:pointer g-value))
  (new-value g-type))

(defcfun (g-value-get-g-type "g_value_get_gtype") g-type
  (g-value (:pointer g-value)))

(defcfun g-param-spec-ref-sink (:pointer g-param-spec)
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-unref :void
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-value-set-default :void
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-value-defaults :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-value-validate :boolean
  (param-spec (:pointer g-param-spec))
  (value (:pointer g-value)))

(defcfun g-param-spec-get-name :string
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-get-nick :string
  (param-spec (:pointer g-param-spec)))

(defcfun g-param-spec-get-blurb :string
  (param-spec (:pointer g-param-spec)))