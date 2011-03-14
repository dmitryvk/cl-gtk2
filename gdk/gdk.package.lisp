(defpackage :gdk
  (:use :cl :gobject :cffi :pango :iter)
  (:export #:gdk-window-events
           #:gdk-atom-as-string))

(in-package :gdk)

(glib:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gdk
      ((:and :unix (:not :darwin)) (:or "libgdk-x11-2.0.so.0" "libgdk-x11-2.0.so"))
      (:darwin (:or "libgdk-x11-2.0.0.dylib" "libgdk-x11-2.0.dylib"))
      (:windows "libgdk-win32-2.0-0.dll")
      (t "libgdk-2.0"))
    (define-foreign-library gdk-pixbuf
      ((:and :unix (:not :darwin)) (:or "libgdk_pixbuf-2.0.so.0" "libgdk_pixbuf-2.0.so"))
      (:darwin (:or "libgdk_pixbuf-2.0.0.dylib" "libgdk_pixbuf-2.0.dylib"))
      (:windows (:or "libgdk_pixbuf-win32-2.0-0" "libgdk_pixbuf-2.0-0.dll"))
      (t "libgdk_pixbuf-2.0"))

    (define-foreign-library gtk
      ((:and :unix (:not :darwin)) (:or "libgtk-x11-2.0.so.0" "libgtk-x11-2.0.so"))
      (:darwin (:or "libgtk-x11-2.0.0.dylib" "libgtk-x11-2.0.dylib"))
      (:windows (:or "libgtk-2.0-0.dll" "libgtk-win32-2.0-0.dll"))
      (t "libgtk-2.0")))

  (use-foreign-library gdk)
  (use-foreign-library gdk-pixbuf)
  (use-foreign-library gtk))

(defcvar (*gtk-major-version* "gtk_major_version" :read-only t :library gtk) :uint)
(defcvar (*gtk-minor-version* "gtk_minor_version" :read-only t :library gtk) :uint)
(defcvar (*gtk-micro-version* "gtk_micro_version" :read-only t :library gtk) :uint)
(defcvar (*gtk-binary-age* "gtk_binary_age" :read-only t :library gtk) :uint)
(defcvar (*gtk-interface-age* "gtk_interface_age" :read-only t :library gtk) :uint)

(glib:push-library-version-features gtk *gtk-major-version* *gtk-minor-version*
  2 2
  2 4
  2 6
  2 8
  2 10
  2 12
  2 14
  2 16
  2 18)

(glib:require-library-version "Gtk+" 2 16 *gtk-major-version* *gtk-minor-version*)
