(in-package :gtk)

(define-g-boxed-cstruct border "GtkBorder"
  (left :int :initform 0)
  (right :int :initform 0)
  (top :int :initform 0)
  (bottom :int :initform 0))

(at-init () (foreign-funcall "gtk_border_get_type" :int))
(at-init () (foreign-funcall "gtk_ui_manager_get_type" :int))

(export (boxed-related-symbols 'border))

(define-foreign-type pointer-as-integer-foreign-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser pointer-as-integer))

(defmethod translate-to-foreign (value (type pointer-as-integer-foreign-type))
  (make-pointer value))

(defmethod translate-from-foreign (value (type pointer-as-integer-foreign-type))
  (pointer-address value))

(define-g-boxed-cstruct tree-iter "GtkTreeIter"
  (stamp :int :initform 0)
  (user-data pointer-as-integer :initform 0)
  (user-data-2 pointer-as-integer :initform 0)
  (user-data-3 pointer-as-integer :initform 0))

(export 'tree-iter)
(export 'tree-iter-stamp)
(export 'tree-iter-user-data)

(defctype tree-path :pointer)

(defcfun gtk-tree-path-new :pointer)
(defcfun gtk-tree-path-free :void (path :pointer))

(define-g-boxed-opaque tree-path "GtkTreePath"
  :alloc (gtk-tree-path-new))

(defcfun (%gtk-tree-path-get-depth "gtk_tree_path_get_depth") :int
  (path tree-path))

(defcfun (%gtk-tree-path-get-indices "gtk_tree_path_get_indices") (:pointer :int)
  (path tree-path))

(defcfun (%gtk-tree-path-append-index "gtk_tree_path_append_index") :void
  (path :pointer)
  (index :int))

(defun tree-path-indices (path)
  (tree-path-get-indices path))

(defun (setf tree-path-indices) (new-value path)
  (tree-path-set-indices new-value path))

(defun tree-path-get-indices (path)
  (setf path (pointer path))
  (let ((n (%gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
       for i from 0 below n
       collect (mem-aref indices :int i))))

(defun tree-path-set-indices (indices path)
  (setf path (pointer path))
  (loop 
     repeat (%gtk-tree-path-get-depth path)
     do (foreign-funcall "gtk_tree_path_up" :pointer path :boolean))
  (loop
     for index in indices
     do(foreign-funcall "gtk_tree_path_append_index" :pointer path :int index :void)))

(export 'tree-path)
(export 'tree-path-indices)

(defcfun (tree-path-compare "gtk_tree_path_compare") :int
  (tree-path-1 (g-boxed-foreign tree-path))
  (tree-path-2 (g-boxed-foreign tree-path)))

(export 'tree-path-compare)

(defcfun (tree-path-next "gtk_tree_path_next") :void
  (tree-path (g-boxed-foreign tree-path)))

(export 'tree-path-next)

(defcfun (tree-path-prev "gtk_tree_path_prev") :void
  (tree-path (g-boxed-foreign tree-path)))

(export 'tree-path-prev)

(defcfun (tree-path-up "gtk_tree_path_up") :boolean
  (tree-path (g-boxed-foreign tree-path)))

(export 'tree-path-up)

(defcfun (tree-path-down "gtk_tree_path_down") :void
  (tree-path (g-boxed-foreign tree-path)))

(export 'tree-path-down)

(defcfun (tree-path-is-ancestor "gtk_tree_path_is_ancestor") :boolean
  (tree-path (g-boxed-foreign tree-path))
  (descendant (g-boxed-foreign tree-path)))

(export 'tree-path-is-ancestor)

(defcfun (tree-path-is-descendant "gtk_tree_path_is_descendant") :boolean
  (tree-path (g-boxed-foreign tree-path))
  (ancestor (g-boxed-foreign tree-path)))

(export 'tree-path-is-descendant)

(at-init () (gobject::type-initializer-call "gtk_tree_row_reference_get_type"))

(define-g-boxed-opaque tree-row-reference "GtkTreeRowReference" :alloc (lambda () (error "")))

(export 'tree-row-reference)

(defcfun (tree-row-reference-new "gtk_tree_row_reference_new") (g-boxed-foreign tree-row-reference :return)
  (model (g-object tree-model))
  (path (g-boxed-foreign tree-path)))

(export 'tree-row-reference)

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-model
  :reader "gtk_tree_row_reference_get_model" :type (g-object tree-model))

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-path
  :reader "gtk_tree_row_reference_get_path" :type (g-boxed-foreign tree-path :return))

(define-boxed-opaque-accessor tree-row-reference tree-row-reference-valid
  :reader "gtk_tree_row_reference_valid" :type :boolean)

(export '(tree-row-reference-model tree-row-reference-path tree-row-reference-valid))

(defcfun (adjustment-clamp-page "gtk_adjustment_clamp_page") :void
  (adjustment (g-object adjustment))
  (lower :double)
  (upper :double))

(export 'adjustment-clamp-page)

(define-g-boxed-cstruct requisition "GtkRequisition"
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'requisition))

(define-g-boxed-cstruct allocation "GtkAllocation"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'allocation))

(defcfun (h-s-v-set-color "gtk_hsv_set_color") :void
  (h-s-v (g-object h-s-v))
  (h :double)
  (s :double)
  (v :double))

(export 'h-s-v-set-color)

(defcfun gtk-hsv-get-color :void
  (h-s-v (g-object h-s-v))
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun h-s-v-get-color (h-s-v)
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (gtk-hsv-get-color h-s-v h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'h-s-v-get-color)

(defcfun (h-s-v-set-metrics "gtk_hsv_set_metrics") :void
  (h-s-v (g-object h-s-v))
  (size :int)
  (ring-width :int))

(export 'h-s-v-set-metrics)

(defcfun gtk-hsv-get-metrics :void
  (h-s-v (g-object h-s-v))
  (size (:pointer :int))
  (ring-width (:pointer :int)))

(defun h-s-v-get-metrics (h-s-v)
  (with-foreign-objects ((size :int) (ring-width :int))
    (gtk-hsv-get-metrics h-s-v size ring-width)
    (values (mem-ref size :int) (mem-ref ring-width :int))))

(export 'h-s-v-get-metrics)

(defcfun (h-s-v-is-adjusting "gtk_hsv_is_adjusting") :boolean
  (h-s-v (g-object h-s-v)))

(export 'h-s-v-is-adjusting)

(defcfun gtk-hsv-to-rgb :void
  (h :double)
  (s :double)
  (v :double)
  (r (:pointer :double))
  (g (:pointer :double))
  (b (:pointer :double)))

(defun h-s-v-to-r-g-b (h s v)
  (with-foreign-objects ((r :double) (g :double) (b :double))
    (gtk-hsv-to-rgb h s v r g b)
    (values (mem-ref r :double) (mem-ref g :double) (mem-ref b :double))))

(export 'h-s-v-to-r-g-b)

(defcfun gtk-rgb-to-hsv :void
  (r :double)
  (g :double)
  (b :double)
  (h (:pointer :double))
  (s (:pointer :double))
  (v (:pointer :double)))

(defun r-g-b-to-h-s-v (r g b)
  (with-foreign-objects ((h :double) (s :double) (v :double))
    (gtk-rgb-to-hsv r g b h s v)
    (values (mem-ref h :double) (mem-ref s :double) (mem-ref v :double))))

(export 'r-g-b-to-h-s-v)
