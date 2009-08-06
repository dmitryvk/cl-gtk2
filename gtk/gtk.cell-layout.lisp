(in-package :gtk)

; TODO: GtkCellLayout vtable

(defcfun gtk-cell-layout-pack-start :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun cell-layout-pack-start (cell-layout cell &key (expand t))
  (gtk-cell-layout-pack-start cell-layout cell expand))

(export 'cell-layout-pack-start)

(defcfun gtk-cell-layout-pack-end :void
  (cell-layout g-object)
  (cell g-object)
  (expand :boolean))

(defun cell-layout-pack-end (cell-layout cell &key (expand t))
  (gtk-cell-layout-pack-end cell-layout cell expand))

(export 'cell-layout-pack-end)

(defcfun (cell-layout-cells "gtk_cell_layout_get_cells") (glist g-object :free-from-foreign t)
  (cell-layout g-object))

(export 'cell-layout-cells)

(defcfun (cell-layout-reorder "gtk_cell_layout_reorder") :void
  (cell-layout g-object)
  (cell g-object)
  (positin :int))

(export 'cell-layout-reorder)

(defcfun (cell-layout-clear "gtk_cell_layout_clear") :void
  (cell-layout g-object))

(export 'cell-layout-clear)

(defcfun (cell-layout-add-attribute "gtk_cell_layout_add_attribute") :void
  (cell-layout g-object)
  (cell g-object)
  (attribute (:string :free-to-foreign t))
  (column :int))

(export 'cell-layout-add-attribute)

(defcallback gtk-cell-layout-cell-data-func-callback :void
    ((cell-layout g-object) (cell g-object) (tree-model g-object) (iter (g-boxed-foreign tree-iter)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               cell-layout cell tree-model iter)
    (return () nil)))

(defcfun gtk-cell-layout-set-cell-data-func :void
  (cell-layout g-object)
  (cell g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun cell-layout-set-cell-data-func (cell-layout cell func)
  (gtk-cell-layout-set-cell-data-func cell-layout
                                      cell
                                      (callback gtk-cell-layout-cell-data-func-callback)
                                      (allocate-stable-pointer func)
                                      (callback stable-pointer-free-destroy-notify-callback)))

(export 'cell-layout-set-cell-data-func)

(defcfun (cell-layout-clear-attributes "gtk_cell_layout_clear_attributes") :void
  (cell-layout g-object)
  (cell g-object))

(export 'cell-layout-clear-attributes)