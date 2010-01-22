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

void          gtk_tree_model_filter_set_visible_column         (GtkTreeModelFilter           *filter,
                                                                gint                          column);

/* conversion */
gboolean      gtk_tree_model_filter_convert_child_iter_to_iter (GtkTreeModelFilter           *filter,
                                                                GtkTreeIter                  *filter_iter,
                                                                GtkTreeIter                  *child_iter);
|#

(defcfun gtk-tree-model-filter-convert-iter-to-child-iter :void
  (filter (g-object tree-model-filter))
  (child-iter (g-boxed-foreign tree-iter))
  (filter-iter (g-boxed-foreign tree-iter)))

(defun tree-model-filter-convert-iter-to-child-iter (filter iter)
  (let ((child-iter (make-instance 'tree-iter)))
    (gtk-tree-model-filter-convert-iter-to-child-iter filter child-iter iter)
    child-iter))

(export 'tree-model-filter-convert-iter-to-child-iter)

#|
GtkTreePath  *gtk_tree_model_filter_convert_child_path_to_path (GtkTreeModelFilter           *filter,
                                                                GtkTreePath                  *child_path);

GtkTreePath  *gtk_tree_model_filter_convert_path_to_child_path (GtkTreeModelFilter           *filter,
                                                                GtkTreePath                  *filter_path);
|#

;; extras

(defcfun gtk-tree-model-filter-refilter :void
  (filter (g-object tree-model-filter)))

(defun tree-model-filter-refilter (filter)
  (gtk-tree-model-filter-refilter filter))

(export 'tree-model-filter-refilter)

(defcfun gtk-tree-model-filter-clear-cache :void
  (filter (g-object tree-model-filter)))

(defun tree-model-filter-clear-cache (filter)
  (gtk-tree-model-filter-clear-cache filter))

(export 'tree-model-filter-clear-cache)
