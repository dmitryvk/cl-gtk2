(in-package :gtk)

(defcfun gtk-tree-selection-set-select-function :void
  (selection g-object)
  (select-function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defcallback gtk-tree-selection-select-function-callback :boolean
    ((selection g-object) (model g-object) (path (g-boxed-foreign tree-path)) (path-currently-selected :boolean) (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
        (funcall fn selection model path path-currently-selected)
      (return-true () t)
      (return-false () nil))))

(defun tree-selection-set-select-function (tree-selection fn)
  (gtk-tree-selection-set-select-function tree-selection
                                          (callback gtk-tree-selection-select-function-callback)
                                          (allocate-stable-pointer fn)
                                          (callback stable-pointer-free-destroy-notify-callback)))

(defcfun gtk-tree-selection-get-user-data :pointer (tree-selection g-object))

(defun tree-selection-get-select-function (tree-selection)
  (let ((ptr (gtk-tree-selection-get-user-data tree-selection)))
    (unless (null-pointer-p ptr)
      (get-stable-pointer-value ptr))))

(defcfun gtk-tree-selection-get-selected :boolean
  (selection g-object)
  (model :pointer)
  (iter (g-boxed-foreign tree-iter)))

(defun tree-selection-selected (tree-selection)
  (let ((iter (make-instance 'tree-iter)))
    (when (gtk-tree-selection-get-selected tree-selection (null-pointer) iter)
      iter)))

(export 'tree-selection-selected)

(defcfun gtk-tree-selection-selected-foreach :void
  (selection g-object)
  (func :pointer)
  (data :pointer))

(defcallback gtk-tree-selection-foreach-callback :void
    ((model g-object) (path (g-boxed-foreign tree-path)) (iter (g-boxed-foreign tree-iter)) (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (funcall fn model path iter)))

(defun map-tree-selection-rows (tree-selection fn)
  (with-stable-pointer (ptr fn)
    (gtk-tree-selection-selected-foreach tree-selection (callback gtk-tree-selection-foreach-callback) ptr)))

(export 'map-tree-selection-rows)

(defcfun gtk-tree-selection-get-selected-rows (glist (g-boxed-foreign tree-path) :free-from-foreign t)
  (selection g-object)
  (model :pointer))

(defun tree-selection-selected-rows (tree-selection)
  (gtk-tree-selection-get-selected-rows tree-selection (null-pointer)))

(export 'tree-selection-selected-rows)

(defcfun (tree-selection-count-selected-rows "gtk_tree_selection_count_selected_rows") :int
  (selection g-object))

(export 'tree-selection-count-selected-rows)

(defcfun (tree-selection-select-path "gtk_tree_selection_select_path") :void
  (selection g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-selection-select-path)

(defcfun (tree-selection-unselect-path "gtk_tree_selection_unselect_path") :void
  (selection g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-selection-unselect-path)

(defcfun (tree-selection-path-selected-p "gtk_tree_selection_path_is_selected") :boolean
  (selection g-object)
  (path (g-boxed-foreign tree-path)))

(export 'tree-selection-path-selected-p)

(defcfun (tree-selection-select-iter "gtk_tree_selection_select_iter") :void
  (selection g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-selection-select-iter)

(defcfun (tree-selection-unselect-iter "gtk_tree_selection_unselect_iter") :void
  (selection g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-selection-unselect-iter)

(defcfun (tree-selection-iter-selected-p "gtk_tree_selection_iter_is_selected") :boolean
  (selection g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-selection-iter-selected-p)

(defcfun (tree-selection-select-all "gtk_tree_selection_select_all") :void
  (selection g-object))

(export 'tree-selection-select-all)

(defcfun (tree-selection-unselect-all "gtk_tree_selection_unselect_all") :void
  (selection g-object))

(export 'tree-selection-unselect-all)

(defcfun (tree-selection-select-range "gtk_tree_selection_select_range") :void
  (selection g-object)
  (start-path (g-boxed-foreign tree-path))
  (end-path (g-boxed-foreign tree-path)))

(export 'tree-selection-select-range)

(defcfun (tree-selection-unselect-range "gtk_tree_selection_unselect_range") :void
  (selection g-object)
  (start-path (g-boxed-foreign tree-path))
  (end-path (g-boxed-foreign tree-path)))

(export 'tree-selection-unselect-range)