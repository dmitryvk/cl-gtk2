(in-package :gtk)

(define-vtable ("GtkTreeModel" tree-model)
  (:skip parent-instance g-type-interface)
  ;;some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;;methods
  (get-flags (tree-model-flags (tree-model g-object)))
  (get-n-columns (:int (tree-model g-object)))
  (get-column-type (g-type-designator
                    (tree-model g-object)
                    (index :int)))
  (get-iter (:boolean
             (tree-model g-object)
             (iter (g-boxed-foreign tree-iter))
             (path (g-boxed-foreign tree-path))))
  (get-path ((g-boxed-foreign tree-path :return)
             (tree-model g-object)
             (iter (g-boxed-foreign tree-iter))))
  (get-value (:void
              (tree-model g-object)
              (iter (g-boxed-foreign tree-iter))
              (n :int)
              (value (:pointer g-value)))
             :impl-call
             ((tree-model iter n)
              (multiple-value-bind (v type) (tree-model-get-value-impl tree-model iter n)
                (set-g-value value v type))))
  (iter-next (:boolean
              (tree-model g-object)
              (iter (g-boxed-foreign tree-iter))))
  (iter-children (:boolean
                  (tree-model g-object)
                  (iter (g-boxed-foreign tree-iter))
                  (parent (g-boxed-foreign tree-iter))))
  (iter-has-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign tree-iter))))
  (iter-n-children (:int
                    (tree-model g-object)
                    (iter (g-boxed-foreign tree-iter))))
  (iter-nth-child (:boolean
                   (tree-model g-object)
                   (iter (g-boxed-foreign tree-iter))
                   (parent (g-boxed-foreign tree-iter))
                   (n :int)))
  (iter-parent (:boolean
                (tree-model g-object)
                (iter (g-boxed-foreign tree-iter))
                (child (g-boxed-foreign tree-iter))))
  (ref-node (:void
             (tree-model g-object)
             (iter (g-boxed-foreign tree-iter))))
  (unref-node (:void
               (tree-model g-object)
               (iter (g-boxed-foreign tree-iter)))))

(define-vtable ("GtkTreeSortable" tree-sortable)
  (:skip parent-instance g-type-interface)
  ;; signal
  (:skip sort-columns-changed :pointer)
  ;; methods
  (get-sort-column-id
   (:boolean (sortable (g-object tree-sortable))
             (sort-column-id (:pointer :int))
             (order (:pointer sort-type)))
   :impl-call ((sortable)
               (multiple-value-bind (sorted-p r-sort-column-id r-order) (tree-sortable-get-sort-column-id-impl sortable)
                 (unless (null-pointer-p sort-column-id)
                   (setf (mem-ref sort-column-id :int) r-sort-column-id))
                 (unless (null-pointer-p order)
                   (setf (mem-ref order 'sort-type) r-order))
                 sorted-p)))
  (set-sort-column-id (:void (sortable (g-object tree-sortable)) (sort-column-id :int) (order sort-type)))
  (set-sort-func (:void (sortable (g-object tree-sortable)) (sort-column-id :int) (func :pointer) (data :pointer) (destroy-notify :pointer)))
  (set-default-sort-func (:void (sortable (g-object tree-sortable)) (func :pointer) (data :pointer) (destroy-notify :pointer)))
  (has-default-sort-func (:boolean (sortable (g-object tree-sortable)))))

(defcfun (tree-model-sort-convert-child-path-to-path "gtk_tree_model_sort_convert_child_path_to_path") (g-boxed-foreign tree-path :return)
  (tree-model-sort (g-object tree-model-sort))
  (child-path (g-boxed-foreign tree-path)))

(export 'tree-model-sort-convert-child-path-to-path)

(defcfun gtk-tree-model-sort-convert-child-iter-to-iter :boolean
  (tree-model-sort (g-object tree-model-sort))
  (sort-iter (g-boxed-foreign tree-iter))
  (child-iter (g-boxed-foreign tree-iter)))

(defun tree-model-sort-convert-child-iter-to-iter (tree-model-sort child-iter)
  (let ((sort-iter (make-tree-iter)))
    (when (gtk-tree-model-sort-convert-child-iter-to-iter tree-model-sort sort-iter child-iter)
      sort-iter)))

(export 'tree-model-sort-convert-child-iter-to-iter)

(defcfun (tree-model-sort-convert-path-to-child-path "gtk_tree_model_sort_convert_path_to_child_path") (g-boxed-foreign tree-path :return)
  (tree-model-sort (g-object tree-model-sort))
  (sort-path (g-boxed-foreign tree-path)))

(export 'tree-model-sort-convert-path-to-child-path)

(defcfun gtk-tree-model-sort-convert-iter-to-child-iter :void
  (tree-model-sort (g-object tree-model-sort))
  (child-iter (g-boxed-foreign tree-iter))
  (sorted-iter (g-boxed-foreign tree-iter)))

(defun tree-model-sort-convert-iter-to-child-iter (tree-model-sort sorted-iter)
  (let ((child-iter (make-tree-iter)))
    (gtk-tree-model-sort-convert-iter-to-child-iter tree-model-sort child-iter sorted-iter)
    child-iter))

(export 'tree-model-sort-convert-iter-to-child-iter)

(defcfun (tree-model-sort-reset-default-sort-func "gtk_tree_model_sort_reset_default_sort_func") :void
  (tree-model-sort (g-object tree-model-sort)))

(export 'tree-model-sort-reset-default-sort-func)

(defcfun (tree-model-sort-clear-cache "gtk_tree_model_sort_clear_cache") :void
  (tree-model-sort (g-object tree-model-sort)))

(export 'tree-model-sort-clear-cached)

(defcfun (tree-model-sort-iter-is-valid "gtk_tree_model_sort_iter_is_valid") :boolean
  (tree-model-sort (g-object tree-model-sort))
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-sort-iter-is-valid)

; TODO: GtkTreeModelFilter

(defclass array-list-store (tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-items)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-types))
  (:metaclass gobject-class)
  (:g-type-name . "LispArrayListStore"))

(export 'array-list-store)

(register-object-type-implementation "LispArrayListStore" array-list-store "GObject" ("GtkTreeModel") nil)

(defun store-items-count (store)
  (length (store-items store)))

(export 'store-items-count)

(defun store-item (store index)
  (aref (store-items store) index))

(export 'store-item)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (let* ((path (make-instance 'tree-path))
         (iter (make-tree-iter)))
    (setf (tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (1- (length (store-items store))))
    (emit-signal store "row-inserted" path iter)))

(export 'store-add-item)

(defun store-remove-item (store item &key (test 'eq))
  (with-slots (items) store
    (let ((index (position item items :test test)))
      (unless index (error "No such item~%~A~%in list-store~%~A" item store))
      (setf items (delete item items :test test))
      (let ((path (make-instance 'tree-path)))
        (setf (tree-path-indices path) (list index))
        (emit-signal store "row-deleted" path)))))

(export 'store-remove-item)

(defun store-add-column (store type getter)
  (vector-push-extend type (store-types store))
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
  (let ((indices (tree-path-indices path)))
    (when (and (= 1 (length indices))
               (< (first indices) (length (store-items model))))
      (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (first indices))
      t)))

(defmethod tree-model-ref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))
(defmethod tree-model-unref-node-impl ((model array-list-store) iter)
  (declare (ignorable model iter)))

(defmethod tree-model-iter-next-impl ((model array-list-store) iter)
  (let ((n (tree-iter-user-data iter)))
    (when (< n (1- (length (store-items model))))
      (setf (tree-iter-user-data iter) (1+ n))
      t)))

(defmethod tree-model-iter-nth-child-impl ((model array-list-store) iter parent n)
  (declare (ignorable parent))
  (setf (tree-iter-stamp iter) 0
        (tree-iter-user-data iter) n)
  t)

(defmethod tree-model-iter-children-impl ((model array-list-store) iter parent)
  (declare (ignore iter parent))
  nil)

(defmethod tree-model-iter-n-children-impl ((model array-list-store) iter)
  (if (null iter)
      (length (store-items model))
      0))

(defmethod tree-model-get-path-impl ((model array-list-store) iter)
  (let ((path (make-instance 'tree-path)))
    (setf (tree-path-indices path) (list (tree-iter-user-data iter)))
    path))

(defmethod tree-model-iter-has-child-impl ((model array-list-store) iter)
  (declare (ignorable iter))
  nil)

(defgeneric tree-model-item (model iter-or-path))

(defmethod tree-model-item ((model array-list-store) (iter tree-iter))
  (let ((n-row (tree-iter-user-data iter)))
    (aref (store-items model) n-row)))

(defmethod tree-model-item ((model array-list-store) (path tree-path))
  (let ((n-row (first (tree-path-indices path))))
    (aref (store-items model) n-row)))

(export 'tree-model-item)

(defmethod tree-model-get-value-impl ((model array-list-store) iter n)
  (let ((n-row (tree-iter-user-data iter)))
    (values (funcall (aref (store-getters model) n) 
                     (aref (store-items model) n-row))
            (aref (store-types model) n))))

(defcfun (tree-model-flags "gtk_tree_model_get_flags") tree-model-flags
  (tree-model g-object))

(export 'tree-model-flags)

(defcfun (tree-model-n-columns "gtk_tree_model_get_n_columns") :int
  (tree-model g-object))

(export 'tree-model-flags)

(defcfun (tree-model-column-type "gtk_tree_model_get_column_type") g-type-designator
  (tree-model g-object)
  (index :int))

(export 'tree-model-column-type)

(defcfun (tree-model-set-iter-to-path "gtk_tree_model_get_iter") :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter))
  (path (g-boxed-foreign tree-path)))

(defun tree-model-iter-by-path (tree-model tree-path)
  (let ((iter (make-tree-iter)))
    (if (tree-model-set-iter-to-path tree-model iter tree-path)
        iter
        nil)))

(export 'tree-model-iter-by-path)

(defcfun (tree-model-set-iter-from-string "gtk_tree_model_get_iter_from_string") :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter))
  (path-string :string))

(defun tree-model-iter-from-string (tree-model path-string)
  (let ((iter (make-tree-iter)))
    (if (tree-model-set-iter-from-string tree-model iter path-string)
        iter
        nil)))

(export 'tree-model-iter-from-string)

(defcfun (tree-model-set-iter-to-first "gtk_tree_model_get_iter_first") :boolean
  (model g-object)
  (iter (g-boxed-foreign tree-iter)))

(defun tree-model-iter-first (tree-model)
  (let ((iter (make-tree-iter)))
    (if (tree-model-set-iter-to-first tree-model iter)
        iter
        nil)))

(export 'tree-model-iter-first)

(defcfun (tree-model-path "gtk_tree_model_get_path") (g-boxed-foreign tree-path :return)
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-path)

(defcfun gtk-tree-model-get-value :void
  (model g-object)
  (iter (g-boxed-foreign tree-iter))
  (column :int)
  (value (:pointer g-value)))

(defun tree-model-value (tree-model iter column)
  (with-foreign-object (v 'g-value)
    (g-value-zero v)
    (gtk-tree-model-get-value tree-model iter column v)
    (prog1 (parse-g-value v)
      (g-value-unset v))))

(export 'tree-model-value)

(defcfun (tree-model-iter-next "gtk_tree_model_iter_next") :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-iter-next)

(defcfun gtk-tree-model-iter-children :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter)))

(defun tree-model-iter-first-child (tree-model parent)
  (let ((iter (make-tree-iter)))
    (if (gtk-tree-model-iter-children tree-model iter parent)
        iter
        nil)))

(export 'tree-model-iter-first-child)

(defcfun (tree-model-iter-has-child "gtk_tree_model_iter_has_child") :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-iter-has-child)

(defcfun (tree-model-iter-n-children "gtk_tree_model_iter_n_children") :int
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-iter-n-children)

(defcfun gtk-tree-model-iter-nth-child :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter))
  (parent (g-boxed-foreign tree-iter))
  (n :int))

(defun tree-model-iter-nth-child (tree-model parent n)
  (let ((iter (make-tree-iter)))
    (if (gtk-tree-model-iter-nth-child tree-model iter parent n)
        iter
        n)))

(export 'tree-model-iter-nth-child)

(defcfun gtk-tree-model-iter-parent :boolean
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter))
  (child (g-boxed-foreign tree-iter)))

(defun tree-model-iter-parent (tree-model iter)
  (let ((parent (make-tree-iter)))
    (if (gtk-tree-model-iter-parent tree-model parent iter)
        parent
        nil)))

(export 'tree-model-iter-parent)

(defcfun (tree-model-iter-to-string "gtk_tree_model_get_string_from_iter") (g-string :free-from-foreign t)
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-iter-to-string)

(defcfun (tree-model-ref-node "gtk_tree_model_ref_node") :void
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-ref-node)

(defcfun (tree-model-unref-node "gtk_tree_model_unref_node") :void
  (tree-model g-object)
  (iter (g-boxed-foreign tree-iter)))

(export 'tree-model-unref-node)

(defcallback gtk-tree-model-foreach-cb :boolean ((model g-object) (path (g-boxed-foreign tree-path)) (iter (g-boxed-foreign tree-iter)) (data :pointer))
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

(defun array-insert-at (array element index)
  (assert (adjustable-array-p array))
  (adjust-array array (1+ (length array)) :fill-pointer t)
  (iter (for i from (1- (length array)) above index)
        (setf (aref array i)
              (aref array (1- i))))
  (setf (aref array index) element)
  array)

(defun array-remove-at (array index)
  (assert (adjustable-array-p array))
  (iter (for i from index below (1- (length array)))
        (setf (aref array i)
              (aref array (1+ i))))
  (adjust-array array (1- (length array)) :fill-pointer t)
  array)

(defstruct tree-node
  (tree nil)
  (parent nil)
  (id nil)
  (item nil)
  (children (make-array 0 :element-type 'tree-node :adjustable t :fill-pointer t)))

(defclass tree-lisp-store (tree-model)
  ((columns-getters :initform (make-array 0 :adjustable t :fill-pointer t) :reader tree-lisp-store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t) :reader tree-lisp-store-types)
   (root :initform (make-tree-node) :reader tree-lisp-store-root)
   (id-map :initform (make-hash-table) :reader tree-lisp-store-id-map)
   (next-id-value :initform 0 :accessor tree-lisp-store-next-id-value))
  (:metaclass gobject-class)
  (:g-type-name . "LispTreeStore"))

(defmethod initialize-instance :after ((object tree-lisp-store) &key &allow-other-keys)
  (setf (tree-node-tree (tree-lisp-store-root object)) object))

(register-object-type-implementation "LispTreeStore" tree-lisp-store "GObject" ("GtkTreeModel") nil)

(defun map-subtree (node fn)
  (funcall fn node)
  (iter (for child in-vector (tree-node-children node))
        (map-subtree child fn)))

(defun clear-id (node)
  (map-subtree node
               (lambda (n)
                 (when (and (tree-node-id n)
                            (tree-node-tree n))
                   (remhash (tree-node-id n)
                            (tree-lisp-store-id-map (tree-node-tree n))))
                 (setf (tree-node-id n) nil))))

(defun set-node-tree (node tree)
  (map-subtree node
               (lambda (n)
                 (setf (tree-node-tree n) tree))))

(defun tree-node-insert-at (node child index)
  (assert (null (tree-node-parent child)))
  (clear-id child)
  (setf (tree-node-parent child) node)
  (set-node-tree child (tree-node-tree node))
  (array-insert-at (tree-node-children node) child index)
  (notice-tree-node-insertion (tree-node-tree node) node child index)
  node)

(defun tree-node-child-at (node index)
  (aref (tree-node-children node) index))

(defun tree-node-remove-at (node index)
  (assert (<= 0 index (1- (length (tree-node-children node)))))
  (let ((child (tree-node-child-at node index)))
    (clear-id child)
    (setf (tree-node-parent child) nil)
    (set-node-tree child nil)
    (array-remove-at (tree-node-children node) index)
    (notice-tree-node-removal (tree-node-tree node) node child index)))

(defun tree-lisp-store-add-column (store column-type column-getter)
  (vector-push-extend column-getter (tree-lisp-store-getters store))
  (vector-push-extend column-type (tree-lisp-store-types store)))

(defmethod tree-model-get-flags-impl ((store tree-lisp-store))
  nil)

(defmethod tree-model-get-n-columns-impl ((store tree-lisp-store))
  (length (tree-lisp-store-getters store)))

(defmethod tree-model-get-column-type-impl ((store tree-lisp-store) index)
  (aref (tree-lisp-store-types store) index))

(defun get-node-by-indices (root indices)
  (if indices
      (get-node-by-indices (tree-node-child-at root (first indices)) (rest indices))
      root))

(defun get-node-by-path (tree path)
  (let ((indices (tree-path-indices path)))
    (get-node-by-indices (tree-lisp-store-root tree) indices)))

(defun get-node-path (node)
  (iter (with z = nil)
        (for parent = (tree-node-parent node))
        (while parent)
        (for index = (position node (tree-node-children parent)))
        (push index z)
        (setf node parent)
        (finally (return z))))

(defun tree-lisp-store-get-next-id (tree)
  (incf (tree-lisp-store-next-id-value tree)))

(defun tree-lisp-store-add-id-map (tree id node)
  (setf (gethash id (tree-lisp-store-id-map tree)) node))

(defun get-assigned-id (tree node)
  (or (tree-node-id node)
      (let ((id (tree-lisp-store-get-next-id tree)))
        (tree-lisp-store-add-id-map tree id node)
        (setf (tree-node-id node) id)
        id)))

(defun get-node-by-id (tree id)
  (gethash id (tree-lisp-store-id-map tree)))

(defmethod tree-model-get-iter-impl ((store tree-lisp-store) iter path)
  (let* ((node (get-node-by-path store path))
         (node-idx (get-assigned-id store node)))
    (setf (tree-iter-stamp iter) 0
          (tree-iter-user-data iter) node-idx)))

(defun get-node-by-iter (tree iter)
  (get-node-by-id tree (tree-iter-user-data iter)))

(defmethod tree-model-get-path-impl ((store tree-lisp-store) iter)
  (let* ((path (make-instance 'tree-path))
         (node (get-node-by-iter store iter))
         (indices (get-node-path node)))
    (setf (tree-path-indices path) indices)
    path))

(defmethod tree-model-get-value-impl ((store tree-lisp-store) iter n)
  (let* ((node (get-node-by-iter store iter))
         (getter (aref (tree-lisp-store-getters store) n))
         (type (aref (tree-lisp-store-types store) n)))
    (values (funcall getter (tree-node-item node))
            type)))

(defmethod tree-model-iter-next-impl ((store tree-lisp-store) iter)
  (let* ((node (get-node-by-iter store iter))
         (parent (tree-node-parent node))
         (index (position node (tree-node-children parent))))
    (when (< (1+ index) (length (tree-node-children parent)))
      (setf (tree-iter-stamp iter)
            0
            (tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at parent (1+ index))))
      t)))

(defmethod tree-model-iter-children-impl ((store tree-lisp-store) iter parent)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store))))
    (when (plusp (length (tree-node-children node)))
      (setf (tree-iter-stamp iter)
            0
            (tree-iter-user-data iter)
            (get-assigned-id store (tree-node-child-at node 0)))
      t)))

(defmethod tree-model-iter-has-child-impl ((store tree-lisp-store) iter)
  (let ((node (get-node-by-iter store iter)))
    (plusp (length (tree-node-children node)))))

(defmethod tree-model-iter-n-children-impl ((store tree-lisp-store) iter)
  (let* ((node (if iter
                   (get-node-by-iter store iter)
                   (tree-lisp-store-root store))))
    (length (tree-node-children node))))

(defmethod tree-model-iter-nth-child-impl ((store tree-lisp-store) iter parent n)
  (let* ((node (if parent
                   (get-node-by-iter store parent)
                   (tree-lisp-store-root store)))
         (requested-node (tree-node-child-at node n)))
    (setf (tree-iter-stamp iter) 0
          (tree-iter-user-data iter) (get-assigned-id store requested-node))
    t))

(defmethod tree-model-iter-parent-impl ((store tree-lisp-store) iter child)
  (let ((node (get-node-by-iter store child)))
    (when (tree-node-parent node)
      (setf (tree-iter-stamp iter) 0
            (tree-iter-user-data iter) (get-assigned-id store (tree-node-parent node))))))

(defmethod tree-model-ref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

(defmethod tree-model-unref-node-impl ((store tree-lisp-store) iter)
  (declare (ignorable iter)))

(defun notice-tree-node-insertion (tree node child index)
  (declare (ignore node index))
  (when tree
    (let* ((path (make-instance 'tree-path))
           (iter (make-tree-iter)))
      (setf (tree-path-indices path) (get-node-path child)
            (tree-iter-stamp iter) 0
            (tree-iter-user-data iter) (get-assigned-id tree child))
      (emit-signal tree "row-inserted" path iter)
      (when (plusp (length (tree-node-children child)))
        (emit-signal tree "row-has-child-toggled" path iter)))))

(defun notice-tree-node-removal (tree node child index)
  (declare (ignore child))
  (when tree
    (let ((path (make-instance 'tree-path)))
      (setf (tree-path-indices path) (nconc (get-node-path node) (list index)))
      (emit-signal tree "row-deleted" path))
    (when (zerop (length (tree-node-children node)))
      (let* ((path (make-instance 'tree-path))
             (iter (make-tree-iter)))
        (setf (tree-path-indices path) (get-node-path node)
              (tree-iter-stamp iter) 0
              (tree-iter-user-data iter) (get-assigned-id tree node))
        (emit-signal tree "row-has-child-toggled" path iter)))))
