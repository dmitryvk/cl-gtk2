(in-package :gtk)

(defcfun gtk-tree-store-set-column-types :void
  (tree-store (g-object tree-store))
  (n-columns :int)
  (types :pointer))

(defun call-tree-store-set-column-types (tree-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type-designator n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type-designator i) type))
      (gtk-tree-store-set-column-types tree-store n types-ar))))

(defmethod initialize-instance :after ((store tree-store) &rest initargs &key (column-types nil column-types-supplied-p) &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-supplied-p
    (call-tree-store-set-column-types store column-types)))

(defcfun (%gtk-tree-store-set-value "gtk_tree_store_set_value") :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-tree-store-set-value (tree-store iter column value)
  (with-foreign-object (v 'g-value)
    (set-g-value v value (tree-model-column-type tree-store column) :zero-g-value t)
    (%gtk-tree-store-set-value tree-store iter column v)
    (g-value-unset v)
    (values)))

(defun tree-store-value (tree-store iter column)
  (tree-model-value tree-store iter column))

(defun (setf tree-store-value) (new-value tree-store iter column)
  (gtk-tree-store-set-value tree-store iter column new-value)
  new-value)

(export 'tree-store-value)

;; not implemented
;; void                gtk_tree_store_set_valuesv          (GtkTreeStore *tree_store,
;;                                                          GtkTreeIter *iter,
;;                                                          gint *columns,
;;                                                          GValue *values,
;;                                                          gint n_values);

(defcfun (tree-store-remove "gtk_tree_store_remove") :boolean
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-store-remove)

(defcfun gtk-tree-store-insert :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter))
  (position :int))

(defun tree-store-insert (tree-store parent position)
  (let ((iter (make-tree-iter)))
    (gtk-tree-store-insert tree-store iter parent position)
    iter))

(defcfun gtk-tree-store-insert-before :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter))
  (sibling (g-boxed-foreign tree-iter)))

(defun tree-store-insert-before (tree-store parent sibling)
  (let ((iter (make-tree-iter)))
    (gtk-tree-store-insert-before tree-store iter parent sibling)
    iter))

(defcfun gtk-tree-store-insert-after :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter))
  (sibling (g-boxed-foreign tree-iter)))

(defun tree-store-insert-after (tree-store parent sibling)
  (let ((iter (make-tree-iter)))
    (gtk-tree-store-insert-after tree-store iter parent sibling)
    iter))

(export '(tree-store-insert tree-store-insert-before tree-store-insert-after))

(defcfun gtk-tree-store-insert-with-valuesv :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(defun tree-store-insert-with-values (tree-store parent position &rest values)
  (let ((n (length values))
        (iter (make-tree-iter)))
    (with-foreign-objects ((v-ar 'g-value n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (tree-model-column-type tree-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aref v-ar 'g-value i) value type :zero-g-value t))
      (gtk-tree-store-insert-with-valuesv tree-store iter parent position columns-ar v-ar n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aref v-ar 'g-value i)))
      iter)))

(export 'tree-store-insert-with-values)

(defcfun gtk-tree-store-prepend :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter)))

(defun tree-store-prepend (tree-store parent)
  (let ((iter (make-tree-iter)))
    (gtk-tree-store-prepend tree-store iter parent)
    iter))

(defcfun gtk-tree-store-append :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter)))

(defun tree-store-append (tree-store parent)
  (let ((iter (make-tree-iter)))
    (gtk-tree-store-append tree-store iter parent)
    iter))

(export '(tree-store-prepend tree-store-append))

(defcfun (tree-store-is-ancestor "gtk_tree_store_is_ancestor") :boolean
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (descendant (g-boxed-foreign tree-iter)))

(export 'tree-store-is-ancestor)

(defcfun (tree-store-iter-depth "gtk_tree_store_iter_depth") :int
  (tree-store (g-object tree-store))
  (tree-iter (g-boxed-foreign tree-iter)))

(export 'tree-store-iter-depth)

(defcfun (tree-store-clear "gtk_tree_store_clear") :void
  (tree-store (g-object tree-store)))

(export 'tree-store-clear)

(defcfun (tree-store-iter-is-valid "gtk_tree_store_iter_is_valid") :boolean
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-store-iter-is-valid)

;; not implemented
;; void                gtk_tree_store_reorder              (GtkTreeStore *tree_store,
;;                                                          GtkTreeIter *parent,
;;                                                          gint *new_order);


(defcfun (tree-store-swap "gtk_tree_store_swap") :void
  (tree-store (g-object tree-store))
  (a (g-boxed-foreign tree-iter))
  (b (g-boxed-foreign tree-iter)))

(export 'tree-store-swap)

(defcfun (tree-store-move-before "gtk_tree_store_move_before") :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (position (g-boxed-foreign tree-iter)))

(export 'tree-store-move-before)

(defcfun (tree-store-move-after "gtk_tree_store_move_after") :void
  (tree-store (g-object tree-store))
  (iter (g-boxed-foreign tree-iter))
  (position (g-boxed-foreign tree-iter)))

(export 'tree-store-move-after)
