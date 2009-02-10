(defsystem :gdk
  :name "gdk"
  :serial t
  :components ((:file "gdk.package")
               (:file "gdk.objects")
               (:file "gdk.functions"))
  :depends-on (:glib :cffi))