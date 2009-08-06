(in-package :gtk)

(define-g-boxed-cstruct border "GtkBorder"
  (left :int :initform 0)
  (right :int :initform 0)
  (top :int :initform 0)
  (bottom :int :initform 0))

(define-g-boxed-cstruct tree-iter "GtkTreeIter"
  (stamp :int :initform 0)
  (user-data :pointer :initform (null-pointer))
  (user-data-2 :pointer :initform (null-pointer))
  (user-data-3 :pointer :initform (null-pointer)))

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
