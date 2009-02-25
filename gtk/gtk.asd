(defsystem :gtk
  :name "gtk"
  :serial t
  :components ((:file "gtk.package")
               (:file "gtk.generated-classes")
               (:file "gtk.objects")
               (:file "gtk.main_loop_events")
               (:file "gtk.functions")
               (:file "gtk.base-classes")
               (:file "gtk.dialog")
               (:file "gtk.window")
               (:file "gtk.image")
               (:file "gtk.label")
               (:file "gtk.progress-bar")
               (:file "gtk.status-bar")
               (:file "gtk.status-icon")
               (:file "gtk.scale-button")
               (:file "gtk.entry")
               (:file "gtk.spin-button")
               (:file "gtk.text")
               (:file "gtk.tree-model")
               
               (:file "gtk.dialog.example")
               
               (:file "gtk.demo"))
  :depends-on (:glib :cffi :gdk :anaphora))