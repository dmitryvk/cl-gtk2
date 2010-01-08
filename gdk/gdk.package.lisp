(defpackage :gdk
  (:use :cl :gobject :cffi :pango :iter)
  (:export #:gdk-window-events
           #:gdk-atom-as-string))

(in-package :gdk)

(glib:at-init ()
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (define-foreign-library gdk
     (:unix (:or "libgdk-x11-2.0.so.0" "libgdk-x11-2.0.so"))
     (:windows "libgdk-win32-2.0-0.dll")
     (t "libgdk-2.0"))
   (define-foreign-library gdk-pixbuf
     (:unix (:or "libgdk_pixbuf-2.0.so.0" "libgdk_pixbuf-2.0.so"))
     (:windows (:or "libgdk_pixbuf-win32-2.0-0" "libgdk_pixbuf-2.0-0.dll"))
     (t "libgdk_pixbuf-2.0")))

 (use-foreign-library gdk)
 (use-foreign-library gdk-pixbuf))