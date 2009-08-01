(in-package :gobject)

#+nil(define-g-boxed-class "GdkRectangle" rectangle ()
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(define-foreign-type g-boxed-foreign-type ()
  ((info :initarg :info
         :accessor g-boxed-foreign-info
         :initform (error "info must be specified"))
   (free-from-foreign :initarg :free-from-foreign
                      :initform nil
                      :accessor g-boxed-foreign-free-from-foreign)
   (free-to-foreign :initarg :free-to-foreign
                    :initform nil
                    :accessor g-boxed-foreign-free-to-foreign)
   (for-callback :initarg :for-callback
                 :initform nil
                 :accessor g-boxed-foreign-for-callback))
  (:actual-type :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct g-boxed-info
    name
    g-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun get-g-boxed-foreign-info (name)
   (get name 'g-boxed-foreign-info)))

(define-parse-method g-boxed-foreign (name &key free-from-foreign free-to-foreign for-callback)
  (let ((info (get-g-boxed-foreign-info name)))
    (assert info nil "Unknown foreign GBoxed type ~A" name)
    (make-instance 'g-boxed-foreign-type
                   :info info
                   :free-from-foreign free-from-foreign
                   :free-to-foreign free-to-foreign
                   :for-callback for-callback)))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct (g-boxed-cstruct-wrapper-info (:include g-boxed-info))
    cstruct
    slots))

(defmacro define-g-boxed-cstruct (name cstruct-name g-type-name &body slots)
  `(progn
     (defstruct ,name
       ,@(iter (for (name type &key initarg) in slots)
               (collect (list name initarg))))
     (defcstruct ,cstruct-name
       ,@(iter (for (name type &key initarg) in slots)
               (collect `(,name ,type))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'g-boxed-foreign-info)
             (make-g-boxed-cstruct-wrapper-info :name ',name
                                                :g-type ,g-type-name
                                                :cstruct ',cstruct-name
                                                :slots ',(iter (for (name type &key initarg) in slots)
                                                               (collect name)))))))

(define-g-boxed-cstruct gdk-rectangle gdk-rectangle-cstruct "GdkRectangle"
  (x :int :initarg 0)
  (y :int :initarg 0)
  (width :int :initarg 0)
  (height :int :initarg 0))

(defgeneric create-temporary-native (type proxy)
  (:documentation "Creates a native structure (or passes a pointer to copy contained in PROXY)
that contains the same data that the PROXY contains and returns a pointer to it.

This call is always paired by call to FREE-TEMPORARY-NATIVE and calls may be nested."))

(defgeneric free-temporary-native (type proxy native-ptr)
  (:documentation "Frees the native structure that was previously created
by CREATE-TEMPORARY-NATIVE for the same PROXY.

Also reads data from native structure pointer to by NATIVE-PTR
and sets the PROXY to contain the same data.

This call is always paired by call to CREATE-TEMPORARY-NATIVE and calls may be nested."))

(defgeneric create-proxy-for-native (type native-ptr)
  (:documentation "Creates a proxy that is initialized by data contained in native
structured pointed to by NATIVE-PTR.

Created proxy should not be linked to NATIVE-PTR and should have
indefinite lifetime (until garbage collector collects it). Specifically,
if proxy need a pointer to native structure, it should make a copy of
a structure.

If proxy requires finalization, finalizers should be added."))

(defgeneric create-reference-proxy (type native-ptr)
  (:documentation "Creates a reference proxy for a native structure pointed to by NATIVE-PTR.

Reference proxy's lifetime is bound to duration of a callback. When the
callback returns the reference proxy is declared invalid and operations on it are errors.

This call is always paired by call to FREE-REFERENCE-PROXY and calls will not nest."))

(defgeneric free-reference-proxy (type proxy native-ptr)
  (:documentation "Frees a reference proxy PROXY previously created by call to
CREATE-REFERENCE-PROXY. This call should ensure that all changes on PROXY are
reflected in native structure pointed to by NATIVE-PTR.

After a call to FREE-REFERENCE-PROXY, PROXY is declared invalid and using it is an error,
operations on it should signal erros.

This call is always paired by call to CREATE-REFERENCE-PROXY."))

(defmethod create-temporary-native ((type g-boxed-cstruct-wrapper-info) proxy)
  (format t "create-temporary-native~%")
  (let* ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type))
         (native-structure (foreign-alloc native-structure-type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (foreign-slot-value native-structure native-structure-type slot)
                (slot-value proxy slot)))
    native-structure))

(defmethod free-temporary-native ((type g-boxed-cstruct-wrapper-info) proxy native-structure)
  (format t "free-temporary-native~%")
  (let ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (slot-value proxy slot)
                (foreign-slot-value native-structure native-structure-type slot))))
  (foreign-free native-structure))

(defmethod create-proxy-for-native ((type g-boxed-cstruct-wrapper-info) native-structure)
  (format t "create-proxy-for-native~%")
  (let* ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type))
         (proxy (make-instance (g-boxed-info-name type))))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (slot-value proxy slot)
                (foreign-slot-value native-structure native-structure-type slot)))
    proxy))

(defmethod create-reference-proxy ((type g-boxed-cstruct-wrapper-info) native-structure)
  (format t "create-reference-proxy~%")
  (create-proxy-for-native type native-structure))

(defmethod free-reference-proxy ((type g-boxed-cstruct-wrapper-info) proxy native-structure)
  (format t "free-reference-proxy~%")
  (let ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (foreign-slot-value native-structure native-structure-type slot)
                (slot-value proxy slot)))))

(defmethod translate-to-foreign (proxy (type g-boxed-foreign-type))
  (if proxy
      (let* ((info (g-boxed-foreign-info type)))
        (values (create-temporary-native info proxy) proxy))
      (null-pointer)))

(defmethod free-translated-object (native-structure (type g-boxed-foreign-type) proxy)
  (when proxy
    (free-temporary-native (g-boxed-foreign-info type) proxy native-structure)))

(defmethod translate-from-foreign (native-structure (type g-boxed-foreign-type))
  (unless (null-pointer-p native-structure)
    (let* ((info (g-boxed-foreign-info type)))
      (cond
        ((g-boxed-foreign-for-callback type)
         (create-reference-proxy info native-structure))
        ((or (g-boxed-foreign-free-to-foreign type)
             (g-boxed-foreign-free-from-foreign type))
         (error "Feature not yet handled"))
        (t (create-proxy-for-native info native-structure))))))

(defmethod cleanup-translated-object-for-callback ((type g-boxed-foreign-type) proxy native-structure)
  (unless (null-pointer-p native-structure)
    (free-reference-proxy (g-boxed-foreign-info type) proxy native-structure)))

(defmethod has-callback-cleanup ((type g-boxed-foreign-type))
  t)

(defcallback incf-rectangle :void ((rectangle (g-boxed-foreign gdk-rectangle :for-callback t))
                                   (delta :int))
  (incf (gdk-rectangle-x rectangle) delta)
  (incf (gdk-rectangle-y rectangle) delta)
  (incf (gdk-rectangle-width rectangle) delta)
  (incf (gdk-rectangle-height rectangle) delta)
  (format t "~A~%" rectangle))

(defun do-incf-rect (r &optional (delta 1))
  (foreign-funcall-pointer (callback incf-rectangle) ()
                           (g-boxed-foreign gdk-rectangle) r
                           :int delta
                           :void)
  r)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (g-boxed-opaque-wrapper-info (:include g-boxed-info))
    alloc free))

(defclass g-boxed-opaque ()
  ((pointer :initarg :pointer
            :initform nil
            :accessor g-boxed-opaque-pointer)))

(defmethod create-temporary-native ((type g-boxed-opaque-wrapper-info) proxy)
  (declare (ignore type))
  (g-boxed-opaque-pointer proxy))

(defmethod free-temporary-native ((type g-boxed-opaque-wrapper-info) proxy native-structure)
  (declare (ignore type proxy native-structure)))

(defmethod create-reference-proxy ((type g-boxed-opaque-wrapper-info) native-structure)
  (make-instance (g-boxed-info-g-type type) :pointer native-structure))

(defmethod free-reference-proxy ((type g-boxed-opaque-wrapper-info) proxy native-structure)
  (declare (ignore type native-structure))
  (setf (g-boxed-opaque-pointer proxy) nil))

(defmethod create-proxy-for-native ((type g-boxed-opaque-wrapper-info) native-structure)
  (let* ((g-type (g-boxed-info-g-type type))
         (native-copy (g-boxed-copy g-type native-structure)))
    (flet ((finalizer () (g-boxed-free g-type native-copy)))
      (let ((proxy (make-instance (g-boxed-opaque-wrapper-info-g-type type) :pointer native-copy)))
        (tg:finalize proxy #'finalizer)
        proxy))))

(defmacro define-g-boxed-opaque (name g-type-name &key
                                 (alloc (error "Alloc must be specified")))
  (let ((native-copy (gensym "NATIVE-COPY-"))
        (instance (gensym "INSTANCE-"))
        (finalizer (gensym "FINALIZER-")))
    `(progn (defclass ,name (g-boxed-opaque) ())
            (defmethod initialize-instance :after ((,instance ,name) &key &allow-other-keys)
              (unless (g-boxed-opaque-pointer ,instance)
                (let ((,native-copy ,alloc))
                  (flet ((,finalizer () (g-boxed-free ,g-type-name ,native-copy)))
                    (setf (g-boxed-opaque-pointer ,instance) ,native-copy)
                    (finalize ,instance #',finalizer)))))
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (setf (get ',name 'g-boxed-foreign-info)
                    (make-g-boxed-opaque-wrapper-info :name ',name
                                                      :g-type ,g-type-name))))))

(define-g-boxed-opaque gtk-tree-path "GtkTreePath"
  :alloc (let* ((native-structure (gtk-tree-path-new))
                (native-copy (g-boxed-copy "GtkTreePath" native-structure)))
           (gtk-tree-path-free native-structure)
           native-copy))

(defcfun gtk-tree-path-new :pointer)

(defcfun gtk-tree-path-free :void
  (gtk-tree-path :pointer))

(defcfun gtk-tree-path-copy :pointer
  (gtk-tree-path :pointer))

(defcfun (%gtk-tree-path-get-depth "gtk_tree_path_get_depth") :int
  (path (g-boxed-foreign gtk-tree-path)))

(defcfun (%gtk-tree-path-get-indices "gtk_tree_path_get_indices") (:pointer :int)
  (path (g-boxed-foreign gtk-tree-path)))

(defcfun gtk-tree-path-append-index :void
  (path (g-boxed-foreign gtk-tree-path))
  (index :int))

(defun tree-path-get-indices (path)
  (let ((n (%gtk-tree-path-get-depth path))
        (indices (%gtk-tree-path-get-indices path)))
    (loop
       for i from 0 below n
       collect (mem-aref indices :int i))))
