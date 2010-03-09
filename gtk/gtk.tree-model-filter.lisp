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

(defcfun gtk-tree-model-filter-set-visible-column :void
  (filter (g-object tree-model-filter))
  (column :int))

(defun tree-model-filter-set-visible-column (filter column)
  (gtk-tree-model-filter-set-visible-column filter column))

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

(defcfun gtk-tree-model-filter-convert-child-path-to-path (g-boxed-foreign tree-path :return)
  (filter (g-object tree-model-sort))
  (child-path (g-boxed-foreign tree-path)))

(defun tree-model-filter-convert-child-path-to-path (filter child-path)
  (gtk-tree-model-filter-convert-child-path-to-path))

(export 'tree-model-filter-convert-child-path-to-path)

(defcfun gtk-tree-model-filter-convert-path-to-child-path (g-boxed-foreign tree-path :return)
  (filter (g-object tree-model-sort))
  (filter-path (g-boxed-foreign tree-path)))

(defun tree-model-filter-convert-path-to-child-path (filter child-path)
  (gtk-tree-model-filter-convert-path-to-child-path))

(export 'tree-model-filter-convert-path-to-child-path)

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
