(in-package :gtk)

(defcfun gtk-tree-view-column-pack-start :void
  (tree-column (g-object tree-view-column))
  (cell (g-object cell-renderer))
  (expand :boolean))

(defun tree-view-column-pack-start (tree-column cell &key (expand t))
  (gtk-tree-view-column-pack-start tree-column cell expand))

(export 'tree-view-column-pack-start)

(defcfun gtk-tree-view-column-pack-end :void
  (tree-column (g-object tree-view-column))
  (cell (g-object cell-renderer))
  (expand :boolean))

(defun tree-view-column-pack-end (tree-column cell &key (expand t))
  (gtk-tree-view-column-pack-end tree-column cell expand))

(export 'tree-view-column-pack-end)

(defcfun (tree-view-column-clear "gtk_tree_view_column_clear") :void
  (tree-column (g-object tree-view-column)))

(export 'tree-view-column-clear)

(defcfun (tree-view-column-add-attribute "gtk_tree_view_column_add_attribute") :void
  (tree-column (g-object tree-view-column))
  (cell-renderer (g-object cell-renderer))
  (attribute :string)
  (column :int))

(export 'tree-view-column-add-attribute)

(defcallback gtk-tree-cell-data-func-cb :void
    ((tree-column (g-object tree-column))
     (cell (g-object cell-renderer))
     (tree-model (g-object tree-model))
     (iter (g-boxed-foreign tree-iter))
     (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
        (funcall fn tree-column cell tree-model iter)
      (return-from-tree-cell-data-function () nil))))

(defcallback gtk-tree-cell-data-func-destroy-cb :void ((data :pointer))
  (free-stable-pointer data))

(defcfun gtk-tree-view-column-set-cell-data-func :void
  (tree-column (g-object tree-column))
  (cell-renderer (g-object cell-renderer))
  (func :pointer)
  (func-data :pointer)
  (destroy-notify :pointer))

(defun tree-view-column-set-cell-data-function (tree-column cell-renderer function)
  (gtk-tree-view-column-set-cell-data-func
   tree-column
   cell-renderer
   (callback gtk-tree-cell-data-func-cb)
   (allocate-stable-pointer function)
   (callback gtk-tree-cell-data-func-destroy-cb)))

(defcfun (tree-view-column-clear-attributes "gtk_tree_view_column_clear_attributes") :void
  (tree-column (g-object tree-column))
  (cell-renderer (g-object cell-renderer)))

(export 'tree-view-column-clear-attributes)

(defcfun (tree-view-column-cell-set-cell-data "gtk_tree_view_column_cell_set_cell_data") :void
  (tree-column (g-object tree-view-column))
  (tree-model (g-object tree-model))
  (iter (g-boxed-foreign tree-iter))
  (is-expander :boolean)
  (is-expanded :boolean))

(export 'tree-view-column-cell-set-data)

(defcfun gtk-tree-view-column-cell-get-size :void
  (tree-column (g-object tree-view-column))
  (cell-area (g-boxed-foreign rectangle))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun tree-view-column-cell-size (tree-column cell-area)
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int))
    (gtk-tree-view-column-cell-get-size tree-column cell-area x y width height)
    (values (mem-ref x :int) (mem-ref y :int) (mem-ref width :int) (mem-ref height :int))))

(export 'tree-view-column-cell-size)

(defcfun gtk-tree-view-column-cell-get-position :boolean
  (tree-column (g-object tree-view-column))
  (cell-renderer (g-object cell-renderer))
  (start-pos (:pointer :int))
  (width (:pointer :int)))

(defun tree-view-column-cell-position (tree-column cell-renderer)
  (with-foreign-objects ((start :int) (width :int))
    (when (gtk-tree-view-column-cell-get-position tree-column cell-renderer start width)
      (list (mem-ref start :int) (mem-ref width :int)))))


(defcfun (tree-view-column-focus-cell "gtk_tree_view_column_focus_cell") :void
  (tree-column (g-object tree-view-column))
  (cell-renderer (g-object cell-renderer)))

(export 'tree-view-column-focus-cell)

(defcfun (tree-view-column-queue-resize "gtk_tree_view_column_queue_resize") :void
  (tree-column (g-object tree-view-column)))

(export 'tree-view-column-queue-resize)
