(defsystem :cl-gtk2-pango
  :name :cl-gtk2-pango
  :version "0.1.1"
  :author "Kalyanov Dmitry <Kalyanov.Dmitry@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "pango.package")
               (:file "pango.init")
               (:file "pango"))
  :depends-on (:cl-gtk2-glib :iterate))