(in-package :gtk)

(defcfun (%gtk-label-get-layout-offsets "gtk_label_get_layout_offsets") :void
  (label (g-object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-label-get-layout-offsets (label)
  (with-foreign-objects ((x :int) (y :int))
    (%gtk-label-get-layout-offsets label x y)
    (list (mem-ref x :int) (mem-ref y :int))))

(defcfun (label-select-region "gtk_label_select_region") :void
  (label (g-object label))
  (start-offset :int)
  (end-offset :int))

(export 'label-select-region)

(defcfun (%gtk-label-get-selection-bounds "gtk_label_get_selection_bounds") :boolean
  (label (g-object label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun gtk-label-get-selection-bounds (label)
  (with-foreign-objects ((start :int) (end :int))
    (when (%gtk-label-get-selection-bounds label start end)
      (list (mem-ref start :int) (mem-ref end :int)))))
