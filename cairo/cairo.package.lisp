(defpackage #:cl-gtk2-cairo
  (:use #:cl #:gdk #:cffi #:gobject)
  (:export #:gdk-context
           #:create-gdk-context
           #:with-gdk-context))
