(defpackage #:cl-gtk2-cairo
  (:use #:cl #:gdk #:cffi #:gobject)
  (:export #:gdk-context
           #:create-gdk-context
           #:with-gdk-context
           #:gdk-cairo-set-source-pixbuf
           #:gdk-cairo-set-source-pixmap
           #:gdk-cairo-region
           #:gdk-cairo-reset-clip))
