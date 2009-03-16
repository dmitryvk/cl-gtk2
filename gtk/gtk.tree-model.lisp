(in-package :gtk)

(define-vtable ("GtkTreeModel" c-gtk-tree-model)
  (:skip parent-instance g-type-interface)
  ;;some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;;methods
  (tree-model-get-flags-impl tree-model-get-flags-cb tree-model-flags (tree-model g-object))
  (tree-model-get-n-columns-impl tree-model-get-n-columns-cb :int (tree-model g-object))
  (tree-model-get-column-type-impl tree-model-get-column-type-cb g-type (tree-model g-object) (index :int))
  (tree-model-get-iter-impl tree-model-get-iter-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)) (path (g-boxed-ref tree-path)))
  (tree-model-get-path-impl tree-model-get-path-cb (g-boxed-ref tree-path) (tree-model g-object) (iter (g-boxed-ref tree-iter)))
  (tree-model-get-value-impl tree-model-get-value-cb :void (tree-model g-object) (iter (g-boxed-ref tree-iter)) (n :int) (value (:pointer g-value)))
  (tree-model-iter-next-impl tree-model-iter-next-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)))
  (tree-model-iter-children-impl tree-model-iter-children-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)) (parent (g-boxed-ref tree-iter)))
  (tree-model-iter-has-child-impl tree-model-iter-has-child-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)))
  (tree-model-iter-n-children-impl tree-model-iter-n-children-cb :int (tree-model g-object) (iter (g-boxed-ref tree-iter)))
  (tree-model-iter-nth-child-impl tree-model-iter-nth-child-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)) (parent (g-boxed-ref tree-iter)) (n :int))
  (tree-model-iter-parent-impl tree-model-iter-parent-cb :boolean (tree-model g-object) (iter (g-boxed-ref tree-iter)) (child (g-boxed-ref tree-iter)))
  (tree-model-ref-node-impl tree-model-ref-node-cb :void (tree-model g-object) (iter (g-boxed-ref tree-iter)))
  (tree-model-unref-node-impl tree-model-unref-node-cb :void (tree-model g-object) (iter (g-boxed-ref tree-iter))))

; TODO: GtkTreeSortable

; TODO: GtkTreeModelSort

; TODO: GtkTreeModelFilter

(defclass array-list-store (g-object tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-items)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-types)))

(export 'array-list-store)

(register-object-type-implementation "LispArrayListStore" array-list-store "GObject" ("GtkTreeModel") nil)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (using* ((path (make-instance 'tree-path))
                   (iter (make-instance 'tree-iter)))
    (setf (tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (1- (length (store-items store))))
    (emit-signal store "row-inserted" path iter)))

(export 'store-add-item)

(defun store-add-column (store type getter)
  (vector-push-extend (ensure-g-type type) (store-types store))
  (vector-push-extend getter (store-getters store))
  (1- (length (store-types store))))

(export 'store-add-column)

(defmethod tree-model-get-flags-impl ((model array-list-store))
  '(:list-only))

(defmethod tree-model-get-n-columns-impl ((model array-list-store))
  (length (store-types model)))

(defmethod tree-model-get-column-type-impl ((tree-model array-list-store) index)
  (aref (store-types tree-model) index))

(defmethod tree-model-get-iter-impl ((model array-list-store) iter path)
  (using* (iter path)
    (let ((indices (tree-path-indices path)))
      (when (and (= 1 (length indices))
                 (< (first indices) (length (store-items model))))
        (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (first indices))
        t))))

(defmethod tree-model-ref-node-impl ((model array-list-store) iter) (release iter))
(defmethod tree-model-unref-node-impl ((model array-list-store) iter) (release iter))

(defmethod tree-model-iter-next-impl ((model array-list-store) iter)
  (using* (iter)
    (let ((n (tree-iter-user-data iter)))
      (when (< n (1- (length (store-items model))))
        (setf (tree-iter-user-data iter) (1+ n))
        t))))

(defmethod tree-model-iter-nth-child-impl ((model array-list-store) iter parent n)
  (using* (iter parent)
    (setf (tree-iter-stamp iter) 0
          (tree-iter-user-data iter) n)
    t))

(defmethod tree-model-iter-n-children-impl ((model array-list-store) iter)
  (if (null-pointer-p iter)
      (length (store-items model))
      0))

(defmethod tree-model-get-path-impl ((model array-list-store) iter)
  (using* (iter)
    (anaphora:aprog1 (make-instance 'tree-path)
      (setf (tree-path-indices anaphora:it) (list (tree-iter-user-data iter)))
      (disown-boxed-ref anaphora:it))))

(defmethod tree-model-iter-has-child-impl ((model array-list-store) iter)
  (release iter)
  nil)

(defgeneric tree-model-item (model iter-or-path))

(defmethod tree-model-item ((model array-list-store) (iter tree-iter))
  (let ((n-row (tree-iter-user-data iter)))
    (aref (store-items model) n-row)))

(defmethod tree-model-item ((model array-list-store) (path tree-path))
  (let ((n-row (first (tree-path-indices path))))
    (aref (store-items model) n-row)))

(export 'tree-model-item)

(defmethod tree-model-get-value-impl ((model array-list-store) iter n value)
  (using (iter)
    (let ((n-row (tree-iter-user-data iter)))
      (set-g-value value
                   (funcall (aref (store-getters model) n) 
                            (aref (store-items model) n-row))
                   (aref (store-types model) n)))))

(defstruct tree-node
  store
  (columns-values (make-array 0 :adjustable t :fill-pointer t) :type vector)
  (children (make-array 0 :adjustable t :fill-pointer t) :type (vector tree-node)))

(export 'tree-node)

(defclass tree-store (g-object tree-model)
  ((root :reader tree-store-root)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t) :reader tree-store-types)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t) :reader tree-store-getters)))

(export 'tree-store)

(register-object-type-implementation "LispTreeStore" tree-store "GObject" ("GtkTreeModel") nil)

(defcfun (tree-model-flags "gtk_tree_model_get_flags") tree-model-flags
  (tree-model g-object))

(export 'tree-modelg-flags)

(defcfun (tree-model-n-columns "gtk_tree_model_get_n_columns") :int
  (tree-model g-object))

(export 'tree-model-flags)

(defcfun (tree-model-column-type "gtk_tree_model_get_column_type") g-type
  (tree-model g-object)
  (index :int))

(export 'tree-model-column-type)

(defcfun (tree-model-set-iter-to-path "gtk_tree_model_get_iter") :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter))
  (path (g-boxed-ref tree-path)))

(defun tree-model-iter-by-path (tree-model tree-path)
  (let ((iter (make-instance 'tree-iter)))
    (if (tree-model-set-iter-to-path tree-model iter tree-path)
        iter
        (progn (release iter) nil))))

(export 'tree-model-iter-by-path)

(defcfun (tree-model-set-iter-from-string "gtk_tree_model_get_iter_from_string") :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter))
  (path-string :string))

(defun tree-model-iter-from-string (tree-model path-string)
  (let ((iter (make-instance 'tree-iter)))
    (if (tree-model-set-iter-from-string tree-model iter path-string)
        iter
        (progn (release iter) nil))))

(export 'tree-model-iter-from-string)

(defcfun (tree-model-set-iter-to-first "gtk_tree_model_get_iter_first") :boolean
  (model g-object)
  (iter (g-boxed-ref tree-iter)))

(defun tree-model-iter-first (tree-model)
  (let ((iter (make-instance 'tree-iter)))
    (if (tree-model-set-iter-to-first tree-model iter)
        iter
        (progn (release iter) nil))))

(export 'tree-model-iter-first)

(defcfun (tree-model-path "gtk_tree_model_get_path") (g-boxed-ref tree-path :owner :lisp)
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-path)

(defcfun gtk-tree-model-get-value :void
  (model g-object)
  (iter (g-boxed-ref tree-iter))
  (column :int)
  (value (:pointer g-value)))

(defun tree-model-value (tree-model iter column)
  (with-foreign-object (v 'g-value)
    (g-value-zero v)
    (gtk-tree-model-get-value tree-model iter column v)
    (prog1 (parse-gvalue v)
      (g-value-unset v))))

(export 'tree-model-value)

(defcfun (tree-model-iter-next "gtk_tree_model_iter_next") :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-iter-next)

(defcfun gtk-tree-model-iter-children :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter))
  (parent (g-boxed-ref tree-iter)))

(defun tree-model-iter-first-child (tree-model parent)
  (let ((iter (make-instance 'tree-iter)))
    (if (gtk-tree-model-iter-children tree-model iter parent)
        iter
        (progn (release iter) nil))))

(export 'tree-model-iter-first-child)

(defcfun (tree-model-iter-has-child "gtk_tree_model_iter_has_child") :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-iter-has-child)

(defcfun (tree-model-iter-n-children "gtk_tree_model_iter_n_children") :int
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-iter-n-children)

(defcfun gtk-tree-model-iter-nth-child :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter))
  (parent (g-boxed-ref tree-iter))
  (n :int))

(defun tree-model-iter-nth-child (tree-model parent n)
  (let ((iter (make-instance 'tree-iter)))
    (if (gtk-tree-model-iter-nth-child tree-model iter parent n)
        iter
        (progn (release iter) n))))

(export 'tree-model-iter-nth-child)

(defcfun gtk-tree-model-iter-parent :boolean
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter))
  (parent (g-boxed-ref tree-iter)))

(defun tree-model-iter-parent (tree-model iter)
  (let ((parent (make-instance 'tree-iter)))
    (if (gtk-tree-model-iter-parent tree-model iter parent)
        parent
        (progn (release parent) nil))))

(export 'tree-model-iter-parent)

(defcfun (tree-model-iter-to-string "gtk_tree_model_get_string_from_iter") (g-string :free-from-foreign t)
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-iter-to-string)

(defcfun (tree-model-ref-node "gtk_tree_model_ref_node") :void
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-ref-node)

(defcfun (tree-model-unref-node "gtk_tree_model_unref_node") :void
  (tree-model g-object)
  (iter (g-boxed-ref tree-iter)))

(export 'tree-model-unref-node)

(defcallback gtk-tree-model-foreach-cb :boolean ((model g-object) (path (g-boxed-ref tree-path)) (iter (g-boxed-ref tree-iter)) (data :pointer))
  (let ((fn (get-stable-pointer-value data)))
    (restart-case
        (funcall fn model path iter)
      (stop-tree-model-iteration () t)
      (skip-tree-model-current () nil))))

(defcfun gtk-tree-model-foreach :void
  (model g-object)
  (func :pointer)
  (data :pointer))

(defun do-tree-model (model fn)
  (with-stable-pointer (ptr fn)
    (gtk-tree-model-foreach model (callback gtk-tree-model-foreach-cb) ptr)))

(export 'do-tree-model)

