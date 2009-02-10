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

(define-g-flags "GtkAttachOptions" attach-options () :expand :shrink :fill)

(defcfun gtk-table-attach :void
  (table (g-object table))
  (child (g-object widget))
  (left-attach :uint)
  (right-attach :uint)
  (top-attach :uint)
  (bottom-attach :uint)
  (x-options attach-options)
  (y-options attach-options)
  (x-padding :uint)
  (y-padding :uint))

(defun table-attach (table widget left right top bottom &key (x-options '(:expand :fill)) (y-options '(:expand :fill)) (x-padding 0) (y-padding 0))
  (gtk-table-attach table widget left right top bottom x-options y-options x-padding y-padding))