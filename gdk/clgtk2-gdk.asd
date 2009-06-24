(defsystem :clgtk2-gdk
  :name :clgtk2-gdk
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gdk.package")
               (:file "gdk.objects")
               (:file "gdk.functions"))
  :depends-on (:clgtk2-glib :cffi))