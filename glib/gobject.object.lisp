(in-package :gobject)

(defcfun g-object-class-install-property :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (param-spec (:pointer g-param-spec)))

(defcfun g-object-class-find-property (:pointer g-param-spec)
  (class (:pointer g-object-class))
  (property-name :string))

(defcfun g-object-class-list-properties (:pointer (:pointer g-param-spec))
  (class (:pointer g-object-class))
  (n-properties (:pointer :uint)))

(defcfun g-object-class-override-property :void
  (class (:pointer g-object-class))
  (property-id :uint)
  (name :string))

(defcfun g-object-interface-install-property :void
  (interface :pointer)
  (param-spec (:pointer g-param-spec)))

(defcfun g-object-interface-find-property (:pointer g-param-spec)
  (interface :pointer)
  (property-name :string))

(defcfun g-object-interface-list-properties (:pointer g-param-spec)
  (interface :pointer)
  (n-properties (:pointer :uint)))

(defcfun g-object-newv :pointer
  (object-type g-type)
  (n-parameter :uint)
  (parameters (:pointer g-parameter)))

(defcfun g-object-ref :pointer
  (object :pointer))

(defcfun g-object-unref :void
  (object :pointer))

(defcfun g-object-ref-sink :pointer
  (object :pointer))

(defcfun g-object-is-floating :boolean
  (object :pointer))

(defcfun g-object-force-floating :void
  (object :pointer))

(defcfun g-object-weak-ref :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(defcfun g-object-weak-unref :void
  (object :pointer)
  (notify :pointer)
  (data :pointer))

(defcfun g-object-add-toggle-ref :void
  (object :pointer)
  (notifty :pointer)
  (data :pointer))

(defcfun g-object-remove-toggle-ref :void
  (object :pointer)
  (notifty :pointer)
  (data :pointer))

(defcfun g-object-notify :void
  (object :pointer)
  (property-name :string))

(defcfun g-object-freeze-notify :void
  (object :pointer))

(defcfun g-object-thaw-notify :void
  (object :pointer))

(defcfun g-object-get-data :pointer
  (object :pointer)
  (key :string))

(defcfun g-object-set-data :void
  (object :pointer)
  (key :string)
  (new-value :pointer))

(defcfun g-object-set-data-full :void
  (object :pointer)
  (key :string)
  (data :pointer)
  (destory :pointer))

(defcfun g-object-steal-data :pointer
  (object :pointer)
  (key :string))

(defcfun g-object-set-property :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))

(defcfun g-object-get-property :void
  (object :pointer)
  (property-name :string)
  (value (:pointer g-value)))