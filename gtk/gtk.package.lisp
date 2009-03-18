(defpackage :gtk
  (:use :cl :cffi :gobject :gdk :glib :metabang-bind :anaphora)
  (:export #:register-object-type
           #:gtk-main
           #:gtk-main-quit
           #:gtk-widget-queue-draw
           #:gtk-widget-show-all
           #:gtk-widget-create-pango-layout
           #:dialog-run
           #:object-destroy
           #:text-buffer-insert
           #:define-child-property
           #:container-class-child-properties
           #:generate-child-properties))

(defpackage :gtk-examples
  (:use :cl :gtk :gdk :gobject)
  (:export #:test-dialog))

(in-package :gtk)

(load-foreign-library "libgtk-x11-2.0.so")

#+sbcl (when (and (find-package "SB-EXT")
                  (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
         (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")) :traps nil))