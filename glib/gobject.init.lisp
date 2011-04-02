(defpackage :cl-gtk2-init
  (:use :cl :glib))

(in-package :cl-gtk2-init)

(at-init ()
 (eval-when (:compile-toplevel :load-toplevel :execute)
   (cffi:define-foreign-library gobject
     ((:and :unix (:not :darwin)) (:or "libgobject-2.0.so.0" "libgobject-2.0.so"))
     (:darwin (:or "libgobject-2.0.0.dylib" "libgobject-2.0.dylib"))
     (:windows "libgobject-2.0-0.dll")
     (t "libgobject-2.0")))

 (cffi:use-foreign-library gobject))

