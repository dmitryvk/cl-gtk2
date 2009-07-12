(defsystem :cl-gtk2-glib
  :name :cl-gtk2-glib
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "glib")
               (:file "glib.glist")
               (:file "glib.gstrv")
               (:file "glib.string")
               (:file "glib.quark")

               (:file "gobject.init")
               (:file "gobject.ffi.package")
               (:file "gobject.type-designator")
               (:file "gobject.ffi")

               (:file "gobject.type-info")
               (:file "gobject.type-info.object")
               (:file "gobject.type-info.enum")
               (:file "gobject.type-info.signals")
               
               (:file "gobject.package")
               (:file "gobject.gvalue")
               (:file "gobject.foreign")
               (:file "gobject.stable-pointer")
               (:file "gobject.closure")
               (:file "gobject.object.low")
               (:file "gobject.foreign-gobject")
               (:file "gobject.foreign-gboxed")
               
               (:file "gobject.meta")
               (:file "gobject.generating")
               (:file "gobject.object-defs")
               (:file "gobject.foreign-gobject-subclassing")

               #+sbcl (:file "sbcl"))
  :depends-on (:cffi :trivial-garbage :iterate :bordeaux-threads :iterate :closer-mop))