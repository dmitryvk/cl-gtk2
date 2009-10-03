(defpackage :gdk
  (:use :cl :gobject :cffi)
  (:export #:gdk-window-events
           #:gdk-gc-set-rgb-fg-color
           #:gdk-drawable-get-size
           #:gdk-draw-line
           #:gdk-gc-new
           #:drawable-get-size
           #:gdk-draw-layout
           #:gdk-atom-as-string
           #:gdk-window-events))

(in-package :gdk)

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
(use-foreign-library gdk-pixbuf)