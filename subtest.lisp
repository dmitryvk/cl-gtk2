(gobject:define-g-flags "GtkTreeModelFlags" tree-model-flags (:type-initializer "gtk_tree_model_flags_get_type")
  (:iters-persist 1) (:list-only 2))

(cffi:defcstruct tree-iter
  (stamp :int)
  (user-data :pointer)
  (user-data-2 :pointer)
  (user-data-3 :pointer))

(defun tree-iter-get-stamp (i) (cffi:foreign-slot-value (gobject::pointer i) 'tree-iter 'stamp))
(defun tree-iter-set-stamp (value i) (setf (cffi:foreign-slot-value (gobject::pointer i) 'tree-iter 'stamp) value))
(defun tree-iter-get-user-data (i) (cffi:pointer-address (cffi:foreign-slot-value (gobject::pointer i) 'tree-iter 'user-data)))
(defun tree-iter-set-user-data (value i) (setf (cffi:foreign-slot-value (gobject::pointer i) 'tree-iter 'user-data) (cffi:make-pointer value)))

(defun tree-iter-alloc () (glib::g-malloc (cffi:foreign-type-size 'tree-iter)))
(defun tree-iter-free (v) (glib::g-free v))

(gobject:define-g-boxed-ref "GtkTreeIter" tree-iter
  (:slots (stamp :reader tree-iter-get-stamp :writer tree-iter-set-stamp :accessor tree-iter-stamp)
          (user-data :reader tree-iter-get-user-data :writer tree-iter-set-user-data :accessor tree-iter-user-data))
  (:alloc-function tree-iter-alloc)
  (:free-function tree-iter-free))

(cffi:defctype tree-path :pointer)
(cffi:defcfun (%gtk-tree-path-get-depth "gtk_tree_path_get_depth") :int
  (path tree-path))

(cffi:defcfun (%gtk-tree-path-get-indices "gtk_tree_path_get_indices") (:pointer :int)
  (path tree-path))

(cffi:defcfun (%gtk-tree-path-new "gtk_tree_path_new") :pointer)

(cffi:defcfun (%gtk-tree-path-append-index "gtk_tree_path_append_index") :void
  (path :pointer)
  (index :int))

(defun tree-path-get-indices (path)
  (setf path (gobject::pointer path))
  (let ((n (%gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
       for i from 0 below n
       collect (cffi:mem-aref indices :int i))))

(defun tree-path-set-indices (indices path)
  (setf path (gobject::pointer path))
  (loop 
     repeat (%gtk-tree-path-get-depth path)
     do (cffi:foreign-funcall "gtk_tree_path_up" :pointer path :boolean))
  (loop
     for index in indices
     do(cffi:foreign-funcall "gtk_tree_path_append_index" :pointer path :int index :void)))

(cffi:defcfun gtk-tree-path-new :pointer)
(cffi:defcfun gtk-tree-path-free :void (path :pointer))

(gobject::define-g-boxed-ref "GtkTreePath" tree-path
  (:alloc-function gtk-tree-path-new)
  (:free-function gtk-tree-path-free)
  (:slots (indices :reader tree-path-get-indices :writer tree-path-set-indices :accessor tree-path-indices)))

(gobject::define-vtable ("GtkTreeModel" c-gtk-tree-model)
  (:skip parent-instance gobject::g-type-interface)
  ;;some signals
  (:skip tree-model-row-changed :pointer)
  (:skip tree-model-row-inserted :pointer)
  (:skip tree-model-row-has-child-toggled :pointer)
  (:skip tree-model-row-deleted :pointer)
  (:skip tree-model-rows-reordered :pointer)
  ;;methods
  (tree-model-get-flags-impl tree-model-get-flags-cb tree-model-flags (tree-model gobject:g-object))
  (tree-model-get-n-columns-impl tree-model-get-n-columns-cb :int (tree-model gobject:g-object))
  (tree-model-get-column-type-impl tree-model-get-column-type-cb gobject::g-type (tree-model gobject:g-object) (index :int))
  (tree-model-get-iter-impl tree-model-get-iter-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)) (path (gobject:g-boxed-ref tree-path)))
  (tree-model-get-path-impl tree-model-get-path-cb (gobject:g-boxed-ref tree-path) (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)))
  (tree-model-get-value-impl tree-model-get-value-cb :void (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)) (n :int) (value (:pointer gobject::g-value)))
  (tree-model-iter-next-impl tree-model-iter-next-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)))
  (tree-model-iter-children-impl tree-model-iter-children-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)) (parent (gobject:g-boxed-ref tree-iter)))
  (tree-model-iter-has-child-impl tree-model-iter-has-child-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)))
  (tree-model-iter-n-children-impl tree-model-iter-n-children-cb :int (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)))
  (tree-model-iter-nth-child-impl tree-model-iter-nth-child-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)) (parent (gobject:g-boxed-ref tree-iter)) (n :int))
  (tree-model-iter-parent-impl tree-model-iter-parent-cb :boolean (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)) (child (gobject:g-boxed-ref tree-iter)))
  (tree-model-ref-node-impl tree-model-ref-node-cb :void (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter)))
  (tree-model-unref-node-impl tree-model-unref-node-cb :void (tree-model gobject:g-object) (iter (gobject:g-boxed-ref tree-iter))))

(defclass array-list-store (gobject:g-object gtk:tree-model)
  ((items :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-items)
   (columns-getters :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-getters)
   (columns-types :initform (make-array 0 :adjustable t :fill-pointer t) :reader store-types)))

(gobject::register-object-type-implementation "LispArrayListStore" array-list-store "GObject" ("GtkTreeModel") nil)

(defun store-add-item (store item)
  (vector-push-extend item (store-items store))
  (gobject:using* ((path (make-instance 'tree-path))
                   (iter (make-instance 'tree-iter)))
    (setf (tree-path-indices path) (list (1- (length (store-items store)))))
    (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (1- (length (store-items store))))
    (gobject::emit-signal store "row-inserted" path iter)))

(defun store-add-column (store type getter)
  (vector-push-extend (gobject::ensure-g-type type) (store-types store))
  (vector-push-extend getter (store-getters store))
  (1- (length (store-types store))))

(defmethod tree-model-get-flags-impl ((model array-list-store))
  '(:list-only))

(defmethod tree-model-get-n-columns-impl ((model array-list-store))
  (length (store-types model)))

(defmethod tree-model-get-column-type-impl ((tree-model array-list-store) index)
  (aref (store-types tree-model) index))

(defmethod tree-model-get-iter-impl ((model array-list-store) iter path)
  (gobject:using* (iter path)
    (let ((indices (tree-path-indices path)))
      (when (= 1 (length indices))
        (setf (tree-iter-stamp iter) 0 (tree-iter-user-data iter) (first indices))
        t))))

(defmethod tree-model-ref-node-impl ((model array-list-store) iter) (gobject:release iter))
(defmethod tree-model-unref-node-impl ((model array-list-store) iter) (gobject:release iter))

(defmethod tree-model-iter-next-impl ((model array-list-store) iter)
  (gobject:using* (iter)
    (let ((n (tree-iter-user-data iter)))
      (when (< n (1- (length (store-items model))))
        (setf (tree-iter-user-data iter) (1+ n))
        t))))

(defmethod tree-model-iter-nth-child-impl ((model array-list-store) iter parent n)
  (gobject:using* (iter parent)
    (setf (tree-iter-stamp iter) 0
          (tree-iter-user-data iter) n)
    t))

(defmethod tree-model-iter-n-children-impl ((model array-list-store) iter)
  (if (cffi:null-pointer-p iter)
      (length (store-items model))
      0))

(defmethod tree-model-get-path-impl ((model array-list-store) iter)
  (gobject:using* (iter)
    (anaphora:aprog1 (make-instance 'tree-path)
      (setf (tree-path-indices anaphora:it) (list (tree-iter-user-data iter)))
      (gobject:disown-boxed-ref anaphora:it))))

(defmethod tree-model-iter-has-child-impl ((model array-list-store) iter)
  (gobject:release iter)
  nil)

(defmethod tree-model-get-value-impl ((model array-list-store) iter n value)
  (gobject:using (iter)
    (let ((n-row (tree-iter-user-data iter)))
      (gobject::set-g-value value
                            (funcall (aref (store-getters model) n) 
                                     (aref (store-items model) n-row))
                            (aref (store-types model) n)))))

(cffi:defcfun (%gtk-tree-view-append-column "gtk_tree_view_append_column") :int
  (tree-view (gobject:g-object gtk:tree-view))
  (column (gobject:g-object gtk:tree-view-column)))

(cffi:defcfun (%gtk-tree-view-column-pack-start "gtk_tree_view_column_pack_start") :void
  (tree-column (gobject:g-object gtk:tree-view-column))
  (cell (gobject:g-object gtk:cell-renderer))
  (expand :boolean))

(cffi:defcfun (%gtk-tree-view-column-add-attribute "gtk_tree_view_column_add_attribute") :void
  (tree-column (gobject:g-object gtk:tree-view-column))
  (cell-renderer (gobject:g-object gtk:cell-renderer))
  (attribute :string)
  (column-number :int))

(defstruct item title value)

(defun test-treeview ()
  (let* ((window (make-instance 'gtk:gtk-window :type :toplevel :title "Treeview"))
         (model (make-instance 'array-list-store))
         (scroll (make-instance 'gtk:scrolled-window :hscrollbar-policy :automatic :vscrollbar-policy :automatic))
         (tv (make-instance 'gtk:tree-view :headers-visible t :width-request 100 :height-request 400))
         (h-box (make-instance 'gtk:h-box))
         (v-box (make-instance 'gtk:v-box))
         (title-entry (make-instance 'gtk:entry))
         (value-entry (make-instance 'gtk:entry))
         (button (make-instance 'gtk:button :label "Add")))
    (store-add-column model "gchararray" #'item-title)
    (store-add-column model "gint" #'item-value)
    (store-add-item model (make-item :title "Monday" :value 1))
    (store-add-item model (make-item :title "Tuesday" :value 2))
    (store-add-item model (make-item :title "Wednesday" :value 3))
    (store-add-item model (make-item :title "Thursday" :value 4))
    (store-add-item model (make-item :title "Friday" :value 5))
    (store-add-item model (make-item :title "Saturday" :value 6))
    (store-add-item model (make-item :title "Sunday" :value 7))
    (setf (gtk:tree-view-model tv) model)
    (gobject:g-signal-connect window "destroy" (lambda (w) (gobject:release w) (gtk:gtk-main-quit)))
    (gobject:g-signal-connect button "clicked" (lambda (b) (gobject:release b) (store-add-item model (make-item :title (gtk:entry-text title-entry)
                                                                                                                :value (parse-integer (gtk:entry-text value-entry) 
                                                                                                                                      :junk-allowed t)))
                                                       #+nil(setf (gtk:tree-view-model tv) nil)
                                                       #+nil(setf (gtk:tree-view-model tv) model)))
    (gtk:container-add window v-box)
    (gtk:box-pack-start v-box h-box :expand nil)
    (gtk:box-pack-start h-box title-entry :expand nil)
    (gtk:box-pack-start h-box value-entry :expand nil)
    (gtk:box-pack-start h-box button :expand nil)
    (gtk:box-pack-start v-box scroll)
    (gtk:container-add scroll tv)
    (let ((column (make-instance 'gtk:tree-view-column :title "Title"))
          (renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
      (%gtk-tree-view-column-pack-start column renderer t)
      (%gtk-tree-view-column-add-attribute column renderer "text" 0)
      (%gtk-tree-view-append-column tv column))
    (let ((column (make-instance 'gtk:tree-view-column :title "Value"))
          (renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
      (%gtk-tree-view-column-pack-start column renderer t)
      (%gtk-tree-view-column-add-attribute column renderer "text" 1)
      (%gtk-tree-view-append-column tv column))
    (gtk:gtk-widget-show-all window)
    (gtk:gtk-main)))