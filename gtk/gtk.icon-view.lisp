(in-package :gtk)

(defcfun (icon-view-get-path-at-pos "gtk_icon_view_get_path_at_pos") (g-boxed-foreign tree-path :return)
  (icon-view g-object)
  (x :int)
  (y :int))

(export 'icon-view-get-path-at-pos)

(defcfun gtk-icon-view-get-item-at-pos :boolean
  (icon-view g-object)
  (x :int)
  (y :int)
  (path :pointer)
  (cell :pointer))

(defun icon-view-get-item-at-pos (icon-view x y)
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (gtk-icon-view-get-item-at-pos icon-view x y path cell)
      (values (mem-ref path '(g-boxed-foreign tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'icon-view-get-item-at-pos)

(defcfun gtk-icon-view-convert-widget-to-bin-window-coords :void
  (icon-view g-object)
  (x :int)
  (y :int)
  (rx :pointer)
  (ry :pointer))

(defun icon-view-convert-widget-to-bin-window-coords (icon-view x y)
  (with-foreign-objects ((rx :int) (ry :int))
    (gtk-icon-view-convert-widget-to-bin-window-coords icon-view x y rx ry)
    (values (mem-ref rx :int)
            (mem-ref ry :int))))

(export 'icon-view-conver-widget-to-bin-window-coords)

(defcfun (icon-view-set-cursor "gtk_icon_view_set_cursor") :void
  (icon-view g-object)
  (path (g-boxed-foreign tree-path))
  (cell g-object)
  (start-editing :boolean))

(export 'icon-view-set-cursor)

(defcfun gtk-icon-view-get-cursor :boolean
  (icon-view g-object)
  (path :pointer)
  (cell :pointer))

(defun icon-view-get-cursor (icon-view)
  (with-foreign-objects ((path :pointer) (cell :pointer))
    (when (gtk-icon-view-get-cursor icon-view path cell)
      (values (mem-ref path '(g-boxed-foreign tree-path :return))
              (mem-ref cell 'g-object)))))

(export 'icon-view-get-cursor)

(defcallback gtk-icon-view-foreach-func-callback :void
    ((icon-view g-object) (path (g-boxed-foreign tree-path)) (data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value data)
               icon-view
               path)
    (return () nil)))

(defcfun gtk-icon-view-selected-foreach :void
  (icon-view g-object)
  (func :pointer)
  (data :pointer))

(defun map-icon-view-selected (icon-view func)
  (with-stable-pointer (ptr func)
    (gtk-icon-view-selected-foreach icon-view
                                    (callback gtk-icon-view-foreach-func-callback)
                                    ptr)))

(export 'map-icon-view-selected)

(defcfun (icon-view-select-path "gtk_icon_view_select_path") :void
  (icon-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'icon-view-select-path)

(defcfun (icon-view-unselect-path "gtk_icon_view_unselect_path") :void
  (icon-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'icon-view-unselect-path)

(defcfun (icon-view-path-selected-p "gtk_icon_view_path_is_selected") :boolean
  (icon-view g-object)
  (path (g-boxed-foreign tree-path)))

(export 'icon-view-path-selected-p)

(defcfun (icon-view-selected-items "gtk_icon_view_get_selected_items") (glist (g-boxed-foreign tree-path) :free-from-foreign t)
  (icon-view g-object))

(export 'icon-view-selected-items)

(defcfun (icon-view-select-all "gtk_icon_view_select_all") :void
  (icon-view g-object))

(export 'icon-view-select-all)

(defcfun (icon-view-unselect-all "gtk_icon_view_unselect_all") :void
  (icon-view g-object))

(export 'icon-view-unselect-all)

(defcfun gtk-icon-view-scroll-to-path :void
  (icon-view g-object)
  (path (g-boxed-foreign tree-path))
  (use-align :boolean)
  (row-align :float)
  (col-align :float))

(defun icon-view-scroll-to-path (icon-view path &key (row-align 0.5 row-align-supplied-p) (col-align 0.5 col-align-supplied-p))
  (gtk-icon-view-scroll-to-path icon-view path (or row-align-supplied-p col-align-supplied-p) row-align col-align))

(export 'icon-view-scroll-to-path)

(defcfun gtk-icon-view-get-visible-range :boolean
  (icon-view g-object)
  (start-path :pointer)
  (end-path :pointer))

(defun icon-view-get-visible-range (icon-view)
  (with-foreign-objects ((start-path :pointer) (end-path :pointer))
    (when (gtk-icon-view-get-visible-range icon-view start-path end-path)
      (values (mem-ref start-path '(g-boxed-foreign tree-path :return))
              (mem-ref end-path '(g-boxed-foreign tree-path :return))))))

(export 'icon-view-get-visible-range)

(defcfun (icon-view-set-tooltip-item "gtk_icon_view_set_tooltip_item") :void
  (icon-view (g-object icon-view))
  (tooltip (g-object tooltip))
  (path (g-boxed-foreign tree-path)))

(export 'icon-view-set-tooltip-item)

(defcfun (icon-view-set-tooltip-cell "gtk_icon_view_set_tooltip_cell") :void
  (icon-view (g-object icon-view))
  (tooltip (g-object tooltip))
  (path (g-boxed-foreign tree-path))
  (cell-renderer (g-object cell-renderer)))

(export 'icon-view-set-tooltip-cell)

(defcfun gtk-icon-view-get-tooltip-context :boolean
  (icon-view (g-object icon-view))
  (x (:pointer :int))
  (y (:pointer :int))
  (keyboard-tip :boolean)
  (model (:pointer (g-object tree-model)))
  (path (:pointer (g-boxed-foreign tree-path)))
  (iter (g-boxed-foreign tree-iter)))

(defun icon-view-get-tooltip-context (icon-view x y keyboard-tip)
  (with-foreign-objects ((xx :int) (yy :int) (model-ptr :pointer) (path-ptr :pointer))
    (setf (mem-ref xx :int) x
          (mem-ref yy :int) y)
    (let ((iter (make-tree-iter)))
      (when (gtk-icon-view-get-tooltip-context icon-view xx yy keyboard-tip model-ptr path-ptr iter)
        (values (mem-ref xx :int) (mem-ref yy :int)
                (convert-from-foreign (mem-ref model-ptr :pointer) '(g-object tree-model))
                (convert-from-foreign (mem-ref path-ptr :pointer) '(g-boxed-foreign tree-path :return))
                iter)))))

; TODO: gtk_icon_view_enable_model_drag_source

; TODO: gtk_icon_view_enable_model_drag_dest

; TODO: gtk_icon_view_unset_model_drag_source

; TODO: gtk_icon_view_unset_model_drag_dest

; TODO: gtk_icon_view_set_drag_dest_item

; TODO: gtk_icon_view_get_drag_dest_item

; TODO: gtk_icon_view_get_dest_item_at_pos

; TODO: gtk_icon_view_create_drag_icon