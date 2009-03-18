(in-package :gtk)

(defcfun gtk-box-pack-start :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-start (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-start box child expand fill padding))

(export 'box-pack-start)

(defcfun gtk-box-pack-end :void
  (box (g-object box))
  (child (g-object widget))
  (expand :boolean)
  (fill :boolean)
  (padding :uint))

(defun box-pack-end (box child &key (expand t) (fill t) (padding 0))
  (gtk-box-pack-end box child expand fill padding))

(export 'box-pack-end)

(defcfun (box-reorder-child "gtk_box_reorder_child") :void
  (box g-object)
  (child g-object)
  (position :int))

(export 'box-reorder-child)