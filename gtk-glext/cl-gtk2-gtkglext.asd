(defsystem :cl-gtk2-gtkglext
  :name :cl-gtk2-gtkglext
  :version "0.1.1"
  :author "Vitaly Mayatskikh <v.mayatskih@gmail.com>"
  :license "LLGPL"
  :serial t
  :components ((:file "gtkglext.package")
	       (:file "gtkglext")
	       (:file "gtkglext-drawing-area")
               (:file "demo"))
  :depends-on (:cl-gtk2-glib :cffi :cl-gtk2-gtk :cl-gtk2-gdk :cl-opengl :cl-glu :cl-glut))
