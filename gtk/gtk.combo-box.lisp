(in-package :gtk)

(defcfun gtk-combo-box-get-active-iter :boolean
  (combo-box g-object)
  (iter (g-boxed-foreign tree-iter)))

(defun combo-box-get-active-iter (combo-box)
  (let ((i (make-instance 'tree-iter)))
    (when (gtk-combo-box-get-active-iter combo-box i)
      i)))

(defcfun (combo-box-active-text "gtk_combo_box_get_active_text") (:string :free-from-foreign t)
  (combo-box g-object))

(export 'combo-box-active-text)

(defcfun (combo-box-popup "gtk_combo_box_popup") :void
  (combo-box g-object))

(export 'combo-box-popup)

(defcfun (combo-box-popdown "gtk_combo_box_popdown") :void
  (combo-box g-object))

(export 'combo-box-popdown)

(defcfun (combo-box-get-popup-accessible "gtk_combo_box_get_popup_accessible") g-object
  (combo-box g-object))

(export 'combo-box-get-popup-accessible)

(defcfun gtk-combo-box-set-row-separator-func :void
  (combo-box g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun combo-box-set-row-separator-func (combo-box func)
  (gtk-combo-box-set-row-separator-func combo-box
                                        (callback gtk-tree-view-row-separator-func-callback)
                                        (allocate-stable-pointer func)
                                        (callback stable-pointer-free-destroy-notify-callback)))


