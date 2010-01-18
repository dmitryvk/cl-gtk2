(defsystem :cl-gtk2-glib
  :name :cl-gtk2-glib
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "glib")
               (:file "glib.glist")
               (:file "glib.gstrv")
               (:file "glib.string")
               (:file "glib.quark")
               (:file "glib.gerror")

               (:file "gobject.init")
               (:file "gobject.ffi.package")
               (:file "gobject.type-designator")
               (:file "gobject.ffi")

               (:file "gobject.package")               
               
               (:file "gobject.type-info")
               (:file "gobject.type-info.object")
               (:file "gobject.type-info.enum")
               (:file "gobject.type-info.signals")
               
               (:file "gobject.gvalue")
               (:file "gobject.foreign")
               (:file "gobject.stable-pointer")
               (:file "gobject.object.low")
               (:file "gobject.object.high")
               (:file "gobject.signals")
               
               (:file "gobject.meta")
               (:file "gobject.generating")
               (:file "gobject.object-defs")
               (:file "gobject.cffi-callbacks")
               (:file "gobject.foreign-gobject-subclassing")
               
               (:file "gobject.boxed")
               (:file "gobject.object-function"))
  :depends-on (:cffi :trivial-garbage :iterate :bordeaux-threads :iterate :closer-mop))