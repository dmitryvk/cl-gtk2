(eval-when (:load-toplevel :compile-toplevel :execute)
  (asdf:oos 'asdf:load-op :gtk)
  (asdf:oos 'asdf:load-op :iterate)
  (asdf:oos 'asdf:load-op :metabang-bind)
  (use-package :cffi)
  (use-package :gobject)
  (use-package :iter)
  (use-package :bind))

(define-g-boxed-class nil g-type-info ()
  (class-size :uint16 :initform 0)
  (base-init :pointer :initform (null-pointer))
  (base-finalize :pointer :initform (null-pointer))
  (class-init :pointer :initform (null-pointer))
  (class-finalize :pointer :initform (null-pointer))
  (class-data :pointer :initform (null-pointer))
  (instance-size :uint16 :initform 0)
  (n-preallocs :uint16 :initform 0)
  (instance-init :pointer :initform (null-pointer))
  (value-type :pointer :initform (null-pointer)))

(defcfun (%g-type-register-static "g_type_register_static") gobject::g-type
  (parent-type gobject::g-type)
  (type-name :string)
  (info (g-boxed-ptr g-type-info))
  (flags gobject::g-type-flags))

(defcfun (%g-type-regiser-static-simple "g_type_register_static_simple") gobject::g-type
  (parent-type gobject::g-type)
  (type-name :string)
  (class-size :uint)
  (class-init :pointer)
  (instance-size :uint)
  (instance-init :pointer)
  (flags gobject::g-type-flags))

(define-g-boxed-class nil g-type-query ()
  (type gobject::g-type :initform 0)
  (name (:string :free-from-foreign nil :free-to-foreign nil) :initform (null-pointer))
  (class-size :uint :initform 0)
  (instance-size :uint :initform 0))

(defcfun (%g-type-query "g_type_query") :void
  (type gobject::g-type)
  (query (g-boxed-ptr g-type-query :in-out)))

(define-foreign-type g-quark ()
  ()
  (:actual-type :uint32)
  (:simple-parser g-quark))

(defcfun g-quark-from-string :uint32
  (string :string))

(defcfun g-quark-to-string (:string :free-from-foreign nil)
  (quark :uint32))

(defmethod translate-to-foreign (string (type g-quark))
  (g-quark-from-string string))

(defmethod translate-from-foreign (value (type g-quark))
  (g-quark-to-string value))

(defvar *stable-pointers-to-symbols* (make-array 0 :adjustable t :fill-pointer t))

(defun stable-pointer (symbol)
  (vector-push-extend symbol *stable-pointers-to-symbols*)
  (length *stable-pointers-to-symbols*))

(defun deref-stable-pointer (p)
  (aref *stable-pointers-to-symbols* (1- p)))

(defcfun g-type-set-qdata :void
  (type gobject::g-type)
  (quark g-quark)
  (data :pointer))

(defcfun g-type-get-qdata :pointer
  (type gobject::g-type)
  (quark g-quark))

(defun g-object-register-sub-type (name parent-type lisp-class)
  (let ((q (make-g-type-query)))
    (%g-type-query (gobject::ensure-g-type parent-type) q)
    (let ((new-type-id (%g-type-regiser-static-simple (gobject::ensure-g-type parent-type)
                                                      name
                                                      (g-type-query-class-size q)
                                                      (null-pointer)
                                                      (g-type-query-instance-size q)
                                                      (null-pointer)
                                                      nil)))
      (when (zerop new-type-id)
        (error "Type registration failed for ~A" name))
      (g-type-set-qdata new-type-id "lisp-class-name" (make-pointer (stable-pointer lisp-class)))
      (setf (get lisp-class 'g-type-name) name))))

(defun g-type-lisp-class (type)
  (let ((sp (pointer-address (g-type-get-qdata (gobject::ensure-g-type type) "lisp-class-name"))))
    (when (zerop sp)
      (error "Type ~A is not a lisp-based type" type))
    (deref-stable-pointer sp)))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun vtable-item->cstruct-item (member)
    (if (eq (first member) :skip)
        (second member)
        `(,(first member) :pointer)))

  (defun vtable->cstruct (table-name options members)
    (bind (((&key cstruct-name &allow-other-keys) options))
      `(defcstruct ,cstruct-name
         ,@(mapcar #'vtable-item->cstruct-item members))))

  (defun arg-name->name (name)
    (if (listp name)
        (second name)
        name))

  (defun arg->arg-name (arg)
    (arg-name->name (first arg)))

  (defun vtable-member->callback (table-name options member)
    (bind (((name return-type &rest args) member))
      `(defcallback ,name ,return-type ,args
         (funcall ',name ,@(mapcar #'arg->arg-name args)))))

  (defun vtable->callbacks (table-name options members)
    (mapcar (lambda (member) (vtable-member->callback table-name options member))
            (remove-if (lambda (member) (eq (first member) :skip)) members)))

  (defun vtable-member->init-member (iface-ptr-var table-name options member)
    (bind (((&key cstruct-name &allow-other-keys) options))
      `(setf (foreign-slot-value ,iface-ptr-var ',cstruct-name ',(first member))
             (callback ,(first member)))))

  (defun vtable->interface-init (table-name options members)
    (bind (((&key interface-initializer &allow-other-keys) options))
      `(defcallback ,interface-initializer :void ((iface :pointer) (data :pointer))
         (declare (ignore data))
         ,@(mapcar (lambda (member) (vtable-member->init-member 'iface table-name options member))
                   (remove-if (lambda (member) (eq (first member) :skip)) members)))))

  (defun vtable-member->generic-function (table-name options member)
    (bind (((name return-type &rest arguments) member))
      `(defgeneric ,name (,@(mapcar #'first arguments)))))

  (defun vtable->generics-def (table-name options members)
    (mapcar (lambda (member) (vtable-member->generic-function table-name options member))
            (remove-if (lambda (member) (eq (first member) :skip)) members))))

(defmacro define-vtable (name options &body members)
  `(progn
     ,(vtable->cstruct name options members)
     ,@(vtable->callbacks name options members)
     ,(vtable->interface-init name options members)
     ,@(vtable->generics-def name options members)
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'options) ',options
             (get ',name 'members) ',members))))

(define-g-flags "GtkTreeModelFlags" tree-model-flags (t)
  (:iters-persist 1) (:list-only 2))

(define-g-boxed-class "GtkTreeIter" tree-iter ()
  (stamp :int)
  (user-data :pointer)
  (user-data-2 :pointer)
  (user-data-3 :pointer))

(defctype tree-path :pointer)

(define-vtable tree-model (:interface "GtkTreeModel" :class-name gtk-tree-model :cstruct-name gtk-tree-model-iface :interface-initializer gtk-tree-model-iface-init)
  (:skip (parent-instance gobject::g-type-interface))
  ;;some signals
  (tree-model-row-changed :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-inserted :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-has-child-toggled :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-deleted :void (tree-model :pointer) (path :pointer))
  (tree-model-rows-reordered :void (tree-model :pointer) (path :pointer) (iter :pointer) (new-order :pointer))
  ;;methods
  (tree-model-get-flags tree-model-flags (tree-model g-object))
  (tree-model-get-n-columns :int (tree-model g-object))
  (tree-model-get-column-type gobject::g-type (tree-model g-object) (index :int))
  (tree-model-get-iter :boolean (tree-model g-object) (iter (:pointer tree-iter)) (path tree-path))
  (tree-model-get-path tree-path (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-get-value :void (tree-model g-object) (iter (g-boxed-ptr tree-iter)) (n :int) (value (:pointer gobject::g-value)))
  (tree-model-iter-next :boolean (tree-model g-object) (iter (:pointer tree-iter)))
  (tree-model-iter-children :boolean (tree-model g-object) (iter (:pointer tree-iter)) (parent (g-boxed-ptr tree-iter)))
  (tree-model-iter-has-child :boolean (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-iter-n-children :int (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-iter-nth-child :boolean (tree-model g-object) (iter (:pointer tree-iter)) (parent (g-boxed-ptr tree-iter)) (n :int))
  (tree-model-iter-parent :boolean (tree-model g-object) (iter (:pointer tree-iter)) (child (g-boxed-ptr tree-iter)))
  (tree-model-ref-node :void (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-unref-node :void (tree-model g-object) (iter (g-boxed-ptr tree-iter))))

(defcfun g-type-add-interface-static :void
  (instance-type gobject::g-type)
  (interface-type gobject::g-type)
  (info (:pointer gobject::g-interface-info)))

(defun add-interface (lisp-class vtable-name)
  (with-foreign-object (iface-info 'gobject::g-interface-info)
    (setf (foreign-slot-value iface-info 'gobject::g-interface-info 'gobject::interface-init) (get-callback (getf (get vtable-name 'options) :interface-initializer))
          (foreign-slot-value iface-info 'gobject::g-interface-info 'gobject::interface-finalize) (null-pointer)
          (foreign-slot-value iface-info 'gobject::g-interface-info 'gobject::interface-data) (null-pointer))
    (unless (getf (get vtable-name 'options) :interface)
      (error "Vtable ~A is not a vtable of an interface"))
    (g-type-add-interface-static (gobject::g-type-from-name (get lisp-class 'g-type-name))
                                 (gobject::g-type-from-name (getf (get vtable-name 'options) :interface))
                                 iface-info)))

(defvar *o1* nil)
(defvar *o2* nil)

(unless *o1*
  (g-object-register-sub-type "LispTreeStore" "GObject" 'lisp-tree-store)
  (setf *o1* t))
(unless *o2*
  (add-interface 'lisp-tree-store 'tree-model)
  (setf *o2* t))

(defclass tree-model (g-object) ())
(defmethod initialize-instance :before ((object tree-model) &key pointer)
  (unless pointer
    (setf (gobject::pointer object) (gobject::g-object-call-constructor (gobject::g-type-from-name "LispTreeStore") nil nil nil))))

(defmethod tree-model-get-flags ((model tree-model))
  (list :list-only))

(defmethod tree-model-get-n-columns ((model tree-model))
  1)

(defmethod tree-model-get-column-type ((model tree-model) index)
  (gobject::g-type-from-name "gchararray"))

(defcfun (%gtk-tree-path-get-depth "gtk_tree_path_get_depth") :int
  (path tree-path))

(defcfun (%gtk-tree-path-get-indices "gtk_tree_path_get_indices") (:pointer :int)
  (path tree-path))

(defcfun (%gtk-tree-path-new "gtk_tree_path_new") :pointer)

(defcfun (%gtk-tree-path-append-index "gtk_tree_path_append_index") :void
  (path :pointer)
  (index :int))

(defun tree-path-indices (path)
  (let ((n (%gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
       for i from 0 below n
       collect (mem-aref indices :int i))))

(defmethod tree-model-get-iter ((model tree-model) iter path)
  (let ((indices (tree-path-indices path)))
    (when (= 1 (length indices))
      (with-foreign-slots ((stamp user-data user-data-2 user-data-3) iter tree-iter)
        (setf stamp 0 user-data (make-pointer (first indices)) user-data-2 (null-pointer) user-data-3 (null-pointer)))
      t)))

(defmethod tree-model-ref-node ((model tree-model) iter))
(defmethod tree-model-unref-node ((model tree-model) iter))

(defmethod tree-model-iter-next ((model tree-model) iter)
  (with-foreign-slots ((stamp user-data) iter tree-iter)
    (let ((n (pointer-address user-data)))
      (when (< n 5)
        (setf user-data (make-pointer (1+ n)))
        t))))

(defmethod tree-model-iter-nth-child ((model tree-model) iter parent n)
  (with-foreign-slots ((stamp user-data user-data-2 user-data-3) iter tree-iter)
    (setf stamp 0 user-data (make-pointer n) user-data-2 (null-pointer) user-data-3 (null-pointer)))
  t)

(defmethod tree-model-iter-n-children ((model tree-model) iter)
  (if (null iter)
      5
      0))

(defmethod tree-model-get-path ((model tree-model) iter)
  (let ((path (%gtk-tree-path-new)))
    (%gtk-tree-path-append-index path (pointer-address (tree-iter-user-data iter)))
    path))

(defmethod tree-model-iter-has-child ((model tree-model) iter)
  nil)

(defmethod tree-model-get-value ((model tree-model) iter n value)
  (let ((n-row (pointer-address (tree-iter-user-data iter))))
    (gobject::set-g-value value (format nil "~A" (expt n-row 2)) (gobject::g-type-from-name "gchararray"))))

(defcfun (%gtk-tree-view-append-column "gtk_tree_view_append_column") :int
  (tree-view (g-object gtk:tree-view))
  (column (g-object gtk:tree-view-column)))

(defcfun (%gtk-tree-view-column-pack-start "gtk_tree_view_column_pack_start") :void
  (tree-column (g-object gtk:tree-view-column))
  (cell (g-object gtk:cell-renderer))
  (expand :boolean))

(defcfun (%gtk-tree-view-column-add-attribute "gtk_tree_view_column_add_attribute") :void
  (tree-column (g-object gtk:tree-view-column))
  (cell-renderer (g-object gtk:cell-renderer))
  (attribute :string)
  (column-number :int))

(defun test-treeview ()
  (let* ((window (make-instance 'gtk:gtk-window :type :toplevel :title "Treeview" :border-width 30))
         (model (make-instance 'tree-model))
         (tv (make-instance 'gtk:tree-view :model model :headers-visible t)))
    (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (gtk:gtk-main-quit)))
    (let ((column (make-instance 'gtk:tree-view-column :title "Number"))
          (renderer (make-instance 'gtk:cell-renderer-text :text "A text")))
      (%gtk-tree-view-column-pack-start column renderer t)
      (%gtk-tree-view-column-add-attribute column renderer "text" 0)
      (%gtk-tree-view-append-column tv column))
    (gtk:container-add window tv)
    (gtk:gtk-widget-show-all window)
    (gtk:gtk-main)))

(defcfun (%gtk-cell-layout-pack-start "gtk_cell_layout_pack_start") :void
  (cell-layout g-object)
  (cell (g-object gtk:cell-renderer))
  (expand :boolean))

(defcfun (%gtk-cell-layout-add-attribute "gtk_cell_layout_add_attribute") :void
  (cell-layout g-object)
  (cell (g-object gtk:cell-renderer))
  (attribute :string)
  (column :int))

(defun test-combobox ()
  (let* ((window (make-instance 'gtk:gtk-window :type :toplevel :title "cb" :border-width 30))
         (model (make-instance 'tree-model))
         (combobox (make-instance 'gtk:combo-box :model model)))
    (g-signal-connect window "destroy" (lambda (w) (declare (ignore w)) (gtk:gtk-main-quit)))
    (g-signal-connect combobox "changed" (lambda (w) (declare (ignore w)) (format t "Changed cb; active now = ~A~%" (gtk:combo-box-active combobox))))
    (let ((renderer (make-instance 'gtk:cell-renderer-text)))
      (%gtk-cell-layout-pack-start combobox renderer t)
      (%gtk-cell-layout-add-attribute combobox renderer "text" 0))
    (gtk:container-add window combobox)
    (gtk:gtk-widget-show-all window)
    (gtk:gtk-main)))

(define-vtable widget (:class "GtkWidget" :cstruct-name widget-vtable :interface-initializer gtk-tree-model-iface-init)
  (:skip (parent-instance gobject::g-type-interface))
  ;;some signals
  (tree-model-row-changed :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-inserted :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-has-child-toggled :void (tree-model :pointer) (path :pointer) (iter :pointer))
  (tree-model-row-deleted :void (tree-model :pointer) (path :pointer))
  (tree-model-rows-reordered :void (tree-model :pointer) (path :pointer) (iter :pointer) (new-order :pointer))
  ;;methods
  (tree-model-get-flags tree-model-flags (tree-model g-object))
  (tree-model-get-n-columns :int (tree-model g-object))
  (tree-model-get-column-type gobject::g-type (tree-model g-object) (index :int))
  (tree-model-get-iter :boolean (tree-model g-object) (iter (:pointer tree-iter)) (path tree-path))
  (tree-model-get-path tree-path (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-get-value :void (tree-model g-object) (iter (g-boxed-ptr tree-iter)) (n :int) (value (:pointer gobject::g-value)))
  (tree-model-iter-next :boolean (tree-model g-object) (iter (:pointer tree-iter)))
  (tree-model-iter-children :boolean (tree-model g-object) (iter (:pointer tree-iter)) (parent (g-boxed-ptr tree-iter)))
  (tree-model-iter-has-child :boolean (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-iter-n-children :int (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-iter-nth-child :boolean (tree-model g-object) (iter (:pointer tree-iter)) (parent (g-boxed-ptr tree-iter)) (n :int))
  (tree-model-iter-parent :boolean (tree-model g-object) (iter (:pointer tree-iter)) (child (g-boxed-ptr tree-iter)))
  (tree-model-ref-node :void (tree-model g-object) (iter (g-boxed-ptr tree-iter)))
  (tree-model-unref-node :void (tree-model g-object) (iter (g-boxed-ptr tree-iter))))