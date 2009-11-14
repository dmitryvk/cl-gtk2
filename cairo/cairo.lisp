(in-package #:cl-gtk2-cairo)

(defcfun gdk-cairo-create :pointer (drawable (g-object drawable)))

(defclass gdk-context (cl-cairo2:context)
  ())
                          
(defun create-gdk-context (gdk-drawable)
  "creates an context to draw on a GTK widget, more precisely on the
associated gdk-window.  This should only be called from within the
expose event.  In cells-gtk, use (gtk-adds-widget-window gtk-pointer)
to obtain the gdk-window. 'gtk-pointer' is the pointer parameter
passed to the expose event handler."
  (make-instance 'gdk-context
                 :pointer (gdk-cairo-create gdk-drawable)))

(defmethod cl-cairo2:destroy ((self gdk-context))
  (cl-cairo2::cairo_destroy (slot-value self 'cl-cairo2:pointer)))

(defmacro with-gdk-context ((context gdk-drawable) &body body)
  "Executes body while context is bound to a valid cairo context for
gdk-window.  This should only be called from within an expose event
handler.  In cells-gtk, use (gtk-adds-widget-window gtk-pointer) to
obtain the gdk-window. 'gtk-pointer' is the pointer parameter passed
to the expose event handler."
  (cl-utilities:with-gensyms (context-pointer)
    `(let ((,context (create-gdk-context ,gdk-drawable)))
       (cl-cairo2::with-context-pointer (,context ,context-pointer)
         ,@body)
       (cl-cairo2:destroy ,context))))

(defcfun gdk_cairo_set_source_pixbuf :void
  (cr :pointer)
  (pixbuf (g-object pixbuf))
  (pixbuf-x :int)
  (pixbuf-y :int))

(defun gdk-cairo-set-source-pixbuf (pixbuf pixbuf-x pixbuf-y &optional (context cl-cairo2:*context*))
  (gdk_cairo_set_source_pixbuf (slot-value context 'cl-cairo2:pointer)
                               pixbuf pixbuf-x pixbuf-y))

(defcfun gdk_cairo_set_source_pixmap :void
  (cr :pointer)
  (pixmap (g-object pixmap))
  (pixmap-x :double)
  (pixmap-y :double))

(defun gdk-cairo-set-source-pixmap (pixmap pixmap-x pixmap-y &optional (context cl-cairo2:*context*))
  (gdk_cairo_set_source_pixmap (slot-value context 'cl-cairo2:pointer)
                               pixmap pixmap-x pixmap-y))

(defcfun gdk_cairo_region :void
  (cr :pointer)
  (region (g-boxed-foreign region)))

(defun gdk-cairo-region (region &optional (context cl-cairo2:*context*))
  (gdk_cairo_region (slot-value context 'cl-cairo2:pointer) region))

(defcfun gdk_cairo_reset_clip :void
  (cr :pointer)
  (drawable (g-object drawable)))

(defun gdk-cairo-reset-clip (drawable &optional (context cl-cairo2:*context*))
  (gdk_cairo_reset_clip (slot-value context 'cl-cairo2:pointer) drawable))