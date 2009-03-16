(in-package :gtk)

(define-g-boxed-class "GtkBorder" border ()
  (left :int :initform 0)
  (right :int :initform 0)
  (top :int :initform 0)
  (bottom :int :initform 0))

(defcstruct tree-iter
  (stamp :int)
  (user-data :pointer)
  (user-data-2 :pointer)
  (user-data-3 :pointer))

(defun tree-iter-get-stamp (i) (foreign-slot-value (pointer i) 'tree-iter 'stamp))
(defun tree-iter-set-stamp (value i) (setf (foreign-slot-value (pointer i) 'tree-iter 'stamp) value))
(defun tree-iter-get-user-data (i) (pointer-address (foreign-slot-value (pointer i) 'tree-iter 'user-data)))
(defun tree-iter-set-user-data (value i) (setf (foreign-slot-value (pointer i) 'tree-iter 'user-data) (make-pointer value)))

(defun tree-iter-alloc () (glib::g-malloc (foreign-type-size 'tree-iter)))
(defun tree-iter-free (v) (glib::g-free v))

(define-g-boxed-ref "GtkTreeIter" tree-iter
  (:slots (stamp :reader tree-iter-get-stamp :writer tree-iter-set-stamp :accessor tree-iter-stamp)
          (user-data :reader tree-iter-get-user-data :writer tree-iter-set-user-data :accessor tree-iter-user-data))
  (:alloc-function tree-iter-alloc)
  (:free-function tree-iter-free))

(export 'tree-iter)
(export 'tree-iter-stamp)
(export 'tree-iter-user-data)

(defctype tree-path :pointer)
(defcfun (%gtk-tree-path-get-depth "gtk_tree_path_get_depth") :int
  (path tree-path))

(defcfun (%gtk-tree-path-get-indices "gtk_tree_path_get_indices") (:pointer :int)
  (path tree-path))

(defcfun (%gtk-tree-path-new "gtk_tree_path_new") :pointer)

(defcfun (%gtk-tree-path-append-index "gtk_tree_path_append_index") :void
  (path :pointer)
  (index :int))

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

(defcfun gtk-tree-path-new :pointer)
(defcfun gtk-tree-path-free :void (path :pointer))

(define-g-boxed-ref "GtkTreePath" tree-path
  (:alloc-function gtk-tree-path-new)
  (:free-function gtk-tree-path-free)
  (:slots (indices :reader tree-path-get-indices :writer tree-path-set-indices :accessor tree-path-indices)))

(export 'tree-path)
(export 'tree-path-indices)
