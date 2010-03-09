(in-package :gtk)

(defcallback gtk-tree-model-filter-visible-func-callback :boolean
  ((tree-model g-object) (iter (g-boxed-foreign tree-iter)) (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
        (funcall fn tree-model iter)
      (return-true () t)
      (return-false () nil))))

(defcfun gtk-tree-model-filter-set-visible-func :void
  (filter (g-object tree-model-filter))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun tree-model-filter-set-visible-function (tree-model-filter function)
  (gtk-tree-model-filter-set-visible-func
   tree-model-filter
   (callback gtk-tree-model-filter-visible-func-callback)
   (allocate-stable-pointer function)
   (callback stable-pointer-free-destroy-notify-callback)))

(export 'tree-model-filter-set-visible-function)

#|

typedef void (* GtkTreeModelFilterModifyFunc) (GtkTreeModel *model,
                                               GtkTreeIter  *iter,
                                               GValue       *value,
                                               gint          column,
                                               gpointer      data);

void          gtk_tree_model_filter_set_modify_func            (GtkTreeModelFilter           *filter,
                                                                gint                          n_columns,
                                                                GType                        *types,
                                                                GtkTreeModelFilterModifyFunc  func,
                                                                gpointer                      data,
                                                                GDestroyNotify                destroy);
|#

(defcfun (tree-model-filter-set-visible-column "gtk_tree_model_filter_set_visible_column") :void
  (filter (g-object tree-model-filter))
  (column :int))

(export 'tree-model-filter-set-visible-column)

;; conversion

(defcfun gtk-tree-model-filter-convert-child-iter-to-iter :boolean
  (filter (g-object tree-model-filter))
  (filter-iter (g-boxed-foreign tree-iter))
  (child-iter (g-boxed-foreign tree-iter)))

(defun tree-model-filter-convert-child-iter-to-iter (filter iter)
  (let ((filter-iter (make-instance 'tree-iter)))
    (when (gtk-tree-model-filter-convert-child-iter-to-iter filter filter-iter iter)
      filter-iter)))

(export 'tree-model-filter-convert-child-iter-to-iter)

(defcfun gtk-tree-model-filter-convert-iter-to-child-iter :void
  (filter (g-object tree-model-filter))
  (child-iter (g-boxed-foreign tree-iter))
  (filter-iter (g-boxed-foreign tree-iter)))

(defun tree-model-filter-convert-iter-to-child-iter (filter iter)
  (let ((child-iter (make-instance 'tree-iter)))
    (gtk-tree-model-filter-convert-iter-to-child-iter filter child-iter iter)
    child-iter))

(export 'tree-model-filter-convert-iter-to-child-iter)

(defcfun (tree-model-filter-convert-child-path-to-path "gtk_tree_model_filter_convert_child_path_to_path") (g-boxed-foreign tree-path :return)
  (filter (g-object tree-model-sort))
  (child-path (g-boxed-foreign tree-path)))

(export 'tree-model-filter-convert-child-path-to-path)

(defcfun (tree-model-filter-convert-path-to-child-path "gtk_tree_model_filter_convert_path_to_child_path") (g-boxed-foreign tree-path :return)
  (filter (g-object tree-model-sort))
  (filter-path (g-boxed-foreign tree-path)))

(export 'tree-model-filter-convert-path-to-child-path)

;; extras

(defcfun (tree-model-filter-refilter "gtk_tree_model_filter_refilter") :void
  (filter (g-object tree-model-filter)))

(export 'tree-model-filter-refilter)

(defcfun (tree-model-filter-clear-cache "gtk_tree_model_filter_clear_cache") :void
  (filter (g-object tree-model-filter)))

(export 'tree-model-filter-clear-cache)
