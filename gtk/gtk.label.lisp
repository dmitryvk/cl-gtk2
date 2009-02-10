(in-package :gtk)

(defcfun gtk-label-get-layout-offsets :void
  (label (g-object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun label-layout-offsets (label)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-label-get-layout-offsets label x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'label-layout-offsets)

(defcfun (label-select-region "gtk_label_select_region") :void
  (label (g-object label))
  (start-offset :int)
  (end-offset :int))

(export 'label-select-region)

(defcfun (label-layout "gtk_label_get_layout") g-object ;(g-object pango-layout)
  (label (g-object label)))

(export 'label-layout)