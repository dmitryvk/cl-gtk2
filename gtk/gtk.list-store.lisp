(in-package :gtk)

(defcfun gtk-list-store-set-column-types :void
  (list-store (g-object list-store))
  (n-columns :int)
  (types :pointer))

(defun call-list-store-set-column-types (list-store column-types)
  (let ((n (length column-types)))
    (with-foreign-object (types-ar 'g-type-designator n)
      (iter (for i from 0 below n)
            (for type in column-types)
            (setf (mem-aref types-ar 'g-type-designator i) type))
      (gtk-list-store-set-column-types list-store n types-ar))))

(defmethod initialize-instance :after ((store list-store) &rest initargs &key (column-types nil column-types-supplied-p) &allow-other-keys)
  (declare (ignore initargs))
  (when column-types-supplied-p
    (call-list-store-set-column-types store column-types)))

(defcfun (%gtk-list-store-set-value "gtk_list_store_set_value") :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter))
  (column :int)
  (value :pointer))

(defun gtk-list-store-set-value (list-store iter column value)
  (with-foreign-object (v 'g-value)
    (set-g-value v value (tree-model-column-type list-store column) :zero-g-value t)
    (%gtk-list-store-set-value list-store iter column v)
    (g-value-unset v)
    (values)))

(defun list-store-value (list-store iter column)
  (tree-model-value list-store iter column))

(defun (setf list-store-value) (new-value list-store iter column)
  (gtk-list-store-set-value list-store iter column new-value)
  new-value)

(export 'list-store-value)

; unimplemented
;void                gtk_list_store_set_valuesv          (GtkListStore *list_store,
;                                                         GtkTreeIter *iter,
;                                                         gint *columns,
;                                                         GValue *values,
;                                                         gint n_values);

(defcfun (list-store-remove "gtk_list_store_remove") :boolean
  (list-store (g-object list-store))
  (tree-iter (g-boxed-foreign tree-iter)))

(export 'list-store-remove)

(defcfun gtk-list-store-insert :void
  (list-store (g-object list-store))
  (tree-iter (g-boxed-foreign tree-iter))
  (position :int))

(defun list-store-insert (list-store position)
  (let ((iter (make-tree-iter)))
    (gtk-list-store-insert list-store iter position)
    iter))

(export 'list-store-insert)

(defcfun gtk-list-store-insert-before :void
  (list-store (g-object list-store))
  (tree-iter (g-boxed-foreign tree-iter))
  (sibling (g-boxed-foreign tree-iter)))

(defcfun gtk-list-store-insert-after :void
  (list-store (g-object list-store))
  (tree-iter (g-boxed-foreign tree-iter))
  (sibling (g-boxed-foreign tree-iter)))

(defun list-store-insert-before (list-store sibling)
  (let ((iter (make-tree-iter)))
    (gtk-list-store-insert-before list-store iter sibling)
    iter))

(defun list-store-insert-after (list-store sibling)
  (let ((iter (make-tree-iter)))
    (gtk-list-store-insert-after list-store iter sibling)
    iter))

(export '(list-store-insert-before list-store-insert-after))

(defcfun gtk-list-store-insert-with-valuesv :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter))
  (position :int)
  (columns :pointer)
  (values :pointer)
  (n-values :int))

(defun list-store-insert-with-values (list-store position &rest values)
  (let ((n (length values))
        (iter (make-tree-iter)))
    (with-foreign-objects ((v-ar 'g-value n)
                           (columns-ar :int n))
      (iter (for i from 0 below n)
            (for value in values)
            (for type = (tree-model-column-type list-store i))
            (setf (mem-aref columns-ar :int i) i)
            (set-g-value (mem-aref v-ar 'g-value i) value type :zero-g-value t))
      (gtk-list-store-insert-with-valuesv list-store iter position columns-ar v-ar n)
      (iter (for i from 0 below n)
            (g-value-unset (mem-aref v-ar 'g-value i)))
      iter)))

(export 'list-store-insert-with-values)

(defcfun gtk-list-store-prepend :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter)))

(defcfun gtk-list-store-append :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter)))

(defun list-store-append (list-store)
  (let ((i (make-tree-iter)))
    (gtk-list-store-append list-store i)
    i))

(defun list-store-prepend (list-store)
  (let ((i (make-tree-iter)))
    (gtk-list-store-prepend list-store i)
    i))

(export '(list-store-append list-store-prepend))

(defcfun (list-store-clear "gtk_list_store_clear") :void
  (list-store (g-object list-store)))

(defcfun (list-store-iter-is-valid "gtk_list_store_iter_is_valid") :boolean
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter)))

; not implemented yet
;(defcfun (list-store-reorder "gtk_list_store_reorder") :void
;  ())

(defcfun (list-store-swap "gtk_list_store_swap") :void
  (list-store (g-object list-store))
  (a (g-boxed-foreign tree-iter))
  (b (g-boxed-foreign tree-iter)))

(defcfun (list-store-move-before "gtk_list_store_move_before") :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter))
  (position (g-boxed-foreign tree-iter)))

(defcfun (list-store-move-after "gtk_list_store_move_after") :void
  (list-store (g-object list-store))
  (iter (g-boxed-foreign tree-iter))
  (position (g-boxed-foreign tree-iter)))

(export '(list-store-clear list-store-iter-is-valid list-store-swap list-store-move-before list-store-move-after))
