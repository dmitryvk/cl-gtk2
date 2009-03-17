(in-package :gtk)

(defcfun gtk-widget-show-all :void
  (widget (g-object widget)))

(defcfun gtk-widget-queue-draw :void
  (widget (g-object widget)))

(defcfun gtk-widget-create-pango-layout (g-object gdk::pango-layout)
  (widget (g-object widget))
  (text :string))

(defcfun gtk-box-pack-start :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-start (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-start box child expand fill padding))

(defcfun (container-add "gtk_container_add") :void
  (container (g-object container))
  (widget (g-object widget)))

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