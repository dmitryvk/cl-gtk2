(in-package :gtk)

(defcfun gtk-widget-show-all :void
  (widget (g-object widget)))

(defcfun gtk-widget-queue-draw :void
  (widget (g-object widget)))

(defcfun gtk-widget-create-pango-layout (g-object gdk::pango-layout)
  (widget (g-object widget))
  (text :string))

(defcfun (object-destroy "gtk_object_destroy") :void
  (object (g-object gtk-object)))

(defcfun gtk-text-buffer-insert :void
  (buffer (g-object text-buffer))
  (iter :pointer)
  (text :string)
  (len :int))

(defun text-buffer-insert (buffer iter text)
  (declare (ignore iter))
  (gtk-text-buffer-insert buffer (null-pointer) text (length text)))