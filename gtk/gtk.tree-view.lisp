(in-package :gtk)

(defcfun (tree-view-columns-autosize "gtk_tree_view_columns_autosize") :void
  (tree-view g-object))

(export 'tree-view-columns-autosize)

(defcfun (tree-view-append-column "gtk_tree_view_append_column") :int
  (tree-view (g-object gtk:tree-view))
  (column (g-object gtk:tree-view-column)))

(export 'tree-view-append-column)

(defcfun (tree-view-remove-column "gtk_tree_view_remove_column") :int
  (tree-view (g-object gtk:tree-view))
  (column (g-object gtk:tree-view-column)))

(export 'tree-view-remove-column)

(defcfun (tree-view-insert-column "gtk_tree_view_insert_column") :int
  (tree-view g-object)
  (column g-object)
  (position :int))

(export 'tree-view-insert-column)

(defcfun (tree-view-get-column "gtk_tree_view_get_column") g-object
  (tree-view g-object)
  (position :int))

(export 'tree-view-get-column)

(defcfun (tree-view-columns "gtk_tree_view_get_columns") (glist g-object)
  (tree-view g-object))

(export 'tree-view-columns)

(defcfun (tree-view-move-column-after "gtk_tree_view_move_column_after") :void
  (tree-view g-object)
  (column g-object)
  (base-column g-object))

(export 'tree-view-move-column-after)

(defcallback gtk-tree-view-column-drop-func-callback :boolean
    ((tree-view g-object) (column g-object) (prev-column g-object) (next-column g-object) (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn tree-view column prev-column next-column)))

(defcfun gtk-tree-view-set-column-drag-function :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-column-drag-fuction (tree-view function)
  (gtk-tree-view-set-column-drag-function tree-view
                                          (callback gtk-tree-view-column-drop-func-callback)
                                          (allocate-stable-pointer function)
                                          (callback stable-pointer-free-destroy-notify-callback)))

(defcfun (tree-view-scroll-to-point "gtk_tree_view_scroll_to_point") :void
  (tree-view g-object)
  (tree-x :int)
  (tree-y :int))

(export 'tree-view-scroll-to-point)

(defcfun gtk-tree-view-scroll-to-cell :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (column g-object)
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun tree-view-scroll-to-cell (tree-view path column &optional (row-align 0.5 row-align-supplied-p) (col-align 0.5 col-align-supplied-p))
  (gtk-tree-view-scroll-to-cell tree-view path column (or row-align-supplied-p col-align-supplied-p) row-align col-align))

(export 'tree-view-scroll-to-cell)

(defcfun gtk-tree-view-set-cursor :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (focus-column g-object)
  (start-editing :boolean))

(defun tree-view-set-cursor (tree-view path &key focus-column start-editing)
  (gtk-tree-view-set-cursor tree-view path focus-column start-editing))

(export 'tree-view-set-cursor)

(defcfun gtk-tree-view-set-cursor-on-cell :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (focus-column g-object)
  (focus-cell g-object)
  (start-editing :boolean))

(defun tree-view-set-cursor-on-cell (tree-view path &key focus-column focus-cell start-editing)
  (gtk-tree-view-set-cursor-on-cell tree-view path focus-column focus-cell start-editing))

(export 'tree-view-set-cursor-on-cell)

(defcfun gtk-tree-view-get-cursor :void
  (tree-view g-object)
  (path :pointer)
  (focus-column :pointer))

(defun tree-view-get-cursor (tree-view)
  (with-foreign-objects ((path :pointer) (focus-column :pointer))
    (gtk-tree-view-get-cursor tree-view path focus-column)
    (values (mem-ref path '(g-boxed-foreign tree-path :return))
            (mem-ref focus-column 'g-object))))

(export 'tree-view-get-cursor)

(defcfun (tree-view-expand-all "gtk_tree_view_expand_all") :void
  (tree-view g-object))

(export 'tree-view-expand-all)

(defcfun (tree-view-collapse-all "gtk_tree_view_collapse_all") :void
  (tree-view g-object))

(export 'tree-view-collapse-all)

(defcfun (tree-view-expand-to-path "gtk_tree_view_expand_to_path") :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-view-expand-to-path)

(defcfun (tree-view-expand-row "gtk_tree_view_expand_row") :boolean
  (tree-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-view-expand-row)

(defcfun (tree-view-collapse-row "gtk_tree_view_collapse_row") :boolean
  (tree-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-view-collapse-row)

(defcallback gtk-tree-view-mapping-func-callback :void
    ((tree-view g-object) (path (g-boxed-foreign tree-path)) (data :pointer))
  (funcall (get-stable-pointer-value data)
           tree-view path))

(defcfun gtk-tree-view-map-expanded-rows :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer))

(defun tree-view-map-expanded-rows (tree-view function)
  (with-stable-pointer (ptr function)
    (gtk-tree-view-map-expanded-rows tree-view (callback gtk-tree-view-mapping-func-callback) ptr)))

(export 'tree-view-map-expanded-rows)

(defcfun (tree-view-row-expanded-p "gtk_tree_view_row_expanded") :boolean
  (tree-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-view-row-expanded-p)

(defcfun gtk-tree-view-get-path-at-pos :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (column :pointer)
  (cell-x :pointer)
  (cell-y :pointer))

(defun tree-view-get-path-at-pos (tree-view x y)
  (with-foreign-objects ((path :pointer) (column :pointer) (cell-x :int) (cell-y :int))
    (when (gtk-tree-view-get-path-at-pos tree-view x y path column cell-x cell-y)
      (values (mem-ref path '(g-boxed-foreign tree-path :return))
              (mem-ref column 'g-object)
              (mem-ref cell-x :int)
              (mem-ref cell-y :int)))))

(export 'tree-view-get-path-at-pos)

(defcfun gtk-tree-view-get-cell-area :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign rectangle)))

(defun tree-view-get-cell-area (tree-view path column)
  (let ((rect (make-rectangle :x 0 :y 0 :width 0 :height 0)))
    (gtk-tree-view-get-cell-area tree-view path column rect)
    rect))

(export 'tree-view-get-cell-area)

(defcfun gtk-tree-view-get-background-area :void
  (tree-view g-object)
  (path (g-boxed-foreign tree-path))
  (column g-object)
  (rectangle (g-boxed-foreign rectangle)))

(defun tree-view-get-background-area (tree-view path column)
  (let ((rect (make-rectangle :x 0 :y 0 :width 0 :height 0)))
    (gtk-tree-view-get-background-area tree-view path column rect)
    rect))

(export 'tree-view-get-background-area)

(defcfun gtk-tree-view-get-visible-rect :void
  (tree-view g-object)
  (rectangle (g-boxed-foreign rectangle)))

(defun tree-view-get-visible-rect (tree-view)
  (let ((rect (make-rectangle :x 0 :y 0 :width 0 :height 0)))
    (gtk-tree-view-get-visible-rect tree-view rect)
    rect))

(export 'tree-view-get-visible-rect)

(defcfun gtk-tree-view-get-visible-range :boolean
  (tree-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun tree-view-get-visible-range (tree-view)
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (gtk-tree-view-get-visible-range tree-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign tree-path :return))
              (mem-ref end-path '(g-boxed-foreign tree-path :return))))))

(export 'tree-view-get-visible-range)

(defcfun gtk-tree-view-convert-bin-window-to-tree-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-bin-window-to-tree-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-bin-window-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-bin-window-to-tree-coords)

(defcfun gtk-tree-view-convert-bin-window-to-widget-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-bin-window-to-widget-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-bin-window-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-bin-window-to-widget-coords)

(defcfun gtk-tree-view-convert-tree-to-bin-window-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-tree-to-bin-window-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-tree-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-tree-to-bin-window-coords)

(defcfun gtk-tree-view-convert-tree-to-widget-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-tree-to-widget-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-tree-to-widget-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-tree-to-widget-coords)

(defcfun gtk-tree-view-convert-widget-to-bin-window-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-widget-to-bin-window-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-widget-to-bin-window-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-widget-to-bin-window-coords)

(defcfun gtk-tree-view-convert-widget-to-tree-coords :void
  (tree-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun tree-view-convert-widget-to-tree-coords (tree-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-tree-view-convert-widget-to-tree-coords tree-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'tree-view-convert-widget-to-tree-coords)

; TODO: gtk_tree_view_enable_model_drag_dest

; TODO: gtk_tree_view_enable_model_drag_source

; TODO: gtk_tree_view_unset_rows_drag_source

; TODO: gtk_tree_view_unset_rows_drag_dest

; TOOD: gtk_tree_view_set_drag_dest_row

; TOOD: gtk_tree_view_get_drag_dest_row

; TOOD: gtk_tree_view_get_dest_row_at_pos

; TOOD: gtk_tree_view_create_drag_icon

(defcallback gtk-tree-view-search-equal-func-callback :boolean
    ((model g-object) (column :int) (key (:string :free-from-foreign nil)) (iter (g-boxed-foreign tree-iter)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               model column key iter)
    (return-true () t)
    (return-false () t)))

(defcfun gtk-tree-view-set-search-equal-func :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-search-equal-func (tree-view func)
  (gtk-tree-view-set-search-equal-func tree-view
                                       (callback gtk-tree-view-search-equal-func-callback)
                                       (allocate-stable-pointer func)
                                       (callback stable-pointer-free-destroy-notify-callback)))

(defcallback gtk-tree-view-search-position-func :void
    ((tree-view g-object) (search-dialog g-object) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               tree-view
               search-dialog)
    (return () nil)))

(defcfun gtk-tree-view-set-search-position-func :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-search-position-func (tree-view func)
  (if func
      (gtk-tree-view-set-search-position-func tree-view
                                              (callback gtk-tree-view-set-search-position-func)
                                              (allocate-stable-pointer func)
                                              (callback stable-pointer-free-destroy-notify-callback))
      (gtk-tree-view-set-search-position-func tree-view
                                              (null-pointer)
                                              (null-pointer)
                                              (null-pointer))))

; TODO: gtk_tree_view_set_destroy_count_func

(defcallback gtk-tree-view-row-separator-func-callback :boolean
    ((tree-model g-object) (iter (g-boxed-foreign tree-iter)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               tree-model
               iter)
    (return-true () t)
    (return-false () nil)))

(defcfun gtk-tree-view-set-row-separator-func :void
  (tree-view g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-view-set-row-separartor-func (tree-view func)
  (if func
      (gtk-tree-view-set-row-separator-func tree-view
                                            (callback gtk-tree-view-row-separator-func-callback)
                                            (allocate-stable-pointer func)
                                            (callback stable-pointer-free-destroy-notify-callback))
      (gtk-tree-view-set-row-separator-func tree-view (null-pointer) (null-pointer) (null-pointer))))

(defcfun (tree-view-rubber-banding-active "gtk_tree_view_is_rubber_banding_active") :boolean
  (tree-view g-object))

(export 'tree-view-rubber-banding-active)

(defcfun (tree-view-set-tooltip-row "gtk_tree_view_set_tooltip_row") :void
  (tree-view g-object)
  (tooltip g-object)
  (tree-path (g-boxed-foreign tree-path)))

(export 'tree-view-set-tooltip-row)

(defcfun (tree-view-set-tooltip-cell "gtk_tree_view_set_tooltip_cell") :void
  (tree-view g-object)
  (tooltip g-object)
  (path (g-boxed-foreign tree-path))
  (column g-object)
  (cell g-object))

(export 'tree-view-set-tooltip-cell)

(defcfun gtk-tree-view-get-tooltip-context :boolean
  (tree-view g-object)
  (x :int)
  (y :int)
  (keyboard-tip :boolean)
  (model :pointer)
  (path :pointer)
  (iter :pointer))

(defun tree-view-get-tooltip-context (tree-view)
  (with-foreign-objects ((x :int) (y :int) (keyboard-tip :boolean) (model :pointer) (path :pointer) (iter :pointer))
    (when (gtk-tree-view-get-tooltip-context tree-view x y keyboard-tip model path iter)
      (values (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref keyboard-tip :boolean)
              (mem-ref model 'g-object)
              (mem-ref path '(g-boxed-foreign tree-path :return))
              (mem-ref iter '(g-boxed-foreign tree-iter :return))))))

(export 'tree-view-get-tooltip-context)

; TODO: GtkTreeView drag-and-drop

(defcfun gtk-cell-view-get-size-of-row :boolean
  (cell-view (g-object cell-view))
  (path (g-boxed-foreign tree-path))
  (requisition (g-boxed-foreign requisition)))

(defun cell-view-get-size-of-row (cell-view path)
  (let ((requisition (make-requisition)))
    (gtk-cell-view-get-size-of-row cell-view path requisition)
    requisition))

(export 'cell-view-get-size-of-row)
