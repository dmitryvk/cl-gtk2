(defsystem :cl-gtk2-cairo
  :name :cl-gtk2-cairo
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "cairo.package")
               (:file "cairo")
               (:file "cairo.demo"))
  :depends-on (:cl-gtk2-glib :cffi :cl-gtk2-gdk :cl-gtk2-gtk :iterate :cl-cairo2))