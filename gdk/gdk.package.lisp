(defpackage :gdk
  (:use :cl :gobject :cffi)
  (:export #:gdk-window-events
           #:gdk-gc-set-rgb-fg-color
           #:gdk-drawable-get-size
           #:gdk-draw-line
           #:gdk-gc-new
           #:drawable-get-size
           #:gdk-draw-layout
           ))

(in-package :gdk)

(load-foreign-library "libgdk-x11-2.0.so")