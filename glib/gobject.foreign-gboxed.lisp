(in-package :gobject)

(defun ensure-list (thing)
  (if (listp thing) thing (list thing)))

(defun slot->cstruct-slot (slot)
  (destructuring-bind (name type &key &allow-other-keys) slot
    `(,name ,type)))

(defun slot->slot-name (slot)
  (destructuring-bind (name type &key &allow-other-keys) slot
    (declare (ignore type))
    name))

(defun cstruct-definition (name slots)
  `(defcstruct ,name ,@(mapcar #'slot->cstruct-slot slots)))

(defun maybe-unlist (thing)
  (if (or (not (listp thing)) (cdr thing))
      thing
      (car thing)))

(defun slot->struct-slot (slot)
  (destructuring-bind (name type &key initform &allow-other-keys) slot
    (declare (ignore type))
    (maybe-unlist `(,name ,@(when initform (list initform))))))

(defun struct-definition (name superclass slots)
  `(defstruct ,@(if superclass
                    (list `(,name (:include ,superclass)))
                    (list name))
     ,@(mapcar #'slot->struct-slot slots)))

(define-foreign-type g-boxed-pointer-type ()
  ((name :accessor g-boxed-pointer-type-name :initarg :name)
   (outp :accessor g-boxed-pointer-type-outp :initarg :outp)))

(define-parse-method g-boxed-ptr (name &optional (type :in))
  (make-instance 'g-boxed-pointer-type :name name :actual-type :pointer :outp (ecase type
                                                                                (:in nil)
                                                                                (:in-out t))))

(defmethod translate-from-foreign (value (type g-boxed-pointer-type))
  (unless (null-pointer-p value)
    (parse-g-boxed value (g-boxed-pointer-type-name type))))

(defmethod translate-to-foreign (value (type g-boxed-pointer-type))
  (let ((ptr (foreign-alloc (boxed-c-structure-name (g-boxed-pointer-type-name type)))))
    (real-unparse-g-boxed ptr value)
    (values ptr value)))

(defmethod free-translated-object (ptr (type g-boxed-pointer-type) param)
  (when (g-boxed-pointer-type-outp type)
    (let ((original-object param)
          (new-real-name (g-boxed-real-name ptr (g-boxed-pointer-type-name type))))
      (if (eq new-real-name (type-of original-object))
          (real-parse-g-boxed ptr original-object)
          (error "Type has changed!"))))
  (foreign-free ptr))

(defmethod expand-to-foreign-dyn (value var body (type g-boxed-pointer-type))
  (let ((value-var (gensym)))
    `(with-foreign-object (,var ',(boxed-c-structure-name (g-boxed-pointer-type-name type)))
       (let ((,value-var ,value))
         (real-unparse-g-boxed ,var ,value-var)
         ,@body
         ,@(when (g-boxed-pointer-type-outp type)
                 (list `(let ((new-real-name (g-boxed-real-name ,var ',(g-boxed-pointer-type-name type))))
                          (if (eq new-real-name (type-of ,value-var))
                              (real-parse-g-boxed ,var ,value-var)
                              (error "Type has changed from ~A to ~A" (type-of ,value-var) new-real-name)))))))))

(define-foreign-type g-boxed-inline-type ()
  ((name :accessor g-boxed-inline-type :initarg :name)))

(define-parse-method g-boxed-inline (name)
  (make-instance 'g-boxed-inline-type :name name :actual-type name))

(defgeneric real-parse-g-boxed (pointer object))
(defgeneric real-unparse-g-boxed (pointer object))

(defun parse-g-boxed (pointer name)
  (let* ((real-name (g-boxed-real-name pointer name))
         (object (make-instance real-name)))
    (real-parse-g-boxed pointer object)
    object))

(defun boxed-alloc (type alloc-type)
  (ecase alloc-type
    (:cffi (foreign-alloc type))
    (:boxed (let ((pointer (foreign-alloc type)))
              (prog1 (g-boxed-copy (g-type-from-name (boxed-type-gname type)) pointer)
                (foreign-free pointer))))))

(defun g-boxed->cstruct (object &key (alloc-type :cffi))
  (let ((pointer (boxed-alloc (type-of object) alloc-type)))
    (real-unparse-g-boxed pointer object)
    pointer))

(defun g-boxed-real-name (pointer name)
  (or (loop
         for (sub-name slot values) in (get name 'boxed-dispatch)
         do (debugf "Checking ~A ~A ~A against ~A.~A = ~A~%" sub-name slot values name slot (foreign-slot-value pointer name slot)) 
         when (member (foreign-slot-value pointer name slot) values :test 'equalp)
         return (g-boxed-real-name pointer sub-name))
      name))

(defun slot->slot-parser (class-name pointer-var slot)
  (destructuring-bind (slot-name slot-type &key parser &allow-other-keys) slot
    (cond
      (parser
       `(setf ,slot-name (funcall ,parser ',class-name ,pointer-var)))
      ((and (listp slot-type) (eq 'g-boxed-inline (first slot-type)))
       `(setf ,slot-name (parse-g-boxed (foreign-slot-pointer ,pointer-var ',class-name ',slot-name) ',(second slot-type))))
      (t
       `(setf ,slot-name (foreign-slot-value ,pointer-var ',class-name ',slot-name))))))

(defun parse-method-definition (name slots)
  (let ((slot-names (mapcar #'slot->slot-name slots)))
    `(defmethod real-parse-g-boxed (pointer (object ,name))
       (with-slots (,@slot-names) object
         ,@(mapcar (lambda (slot) (slot->slot-parser name 'pointer slot)) slots)))))

(defun slot->slot-unparser (class-name pointer-var slot object)
  (destructuring-bind (slot-name slot-type &key unparser &allow-other-keys) slot
    (cond
      (unparser
       `(funcall ,unparser ',class-name ,pointer-var ,object))
      ((and (listp slot-type) (eq 'g-boxed-inline (first slot-type)))
       `(real-unparse-g-boxed (foreign-slot-pointer ,pointer-var ',class-name ',slot-name) ,slot-name))
      (t
       `(setf (foreign-slot-value ,pointer-var ',class-name ',slot-name) ,slot-name)))))
  
(defun unparse-method-definition (name slots)
  (let ((slot-names (mapcar #'slot->slot-name slots)))
    `(defmethod real-unparse-g-boxed (pointer (object ,name))
       (with-slots (,@slot-names) object
         ,@(mapcar (lambda (slot) (slot->slot-unparser name 'pointer slot 'object)) slots)))))

(defun slot->export-accessor (class-name slot)
  (destructuring-bind (slot-name slot-type &key &allow-other-keys) slot
    (declare (ignore slot-type))
    (let ((accessor-name (intern (format nil "~A-~A" (symbol-name class-name) (symbol-name slot-name))
                                 (symbol-package class-name))))
      `(export ',accessor-name (symbol-package ',accessor-name)))))

(defun struct-constructor-name (name)
  (intern (format nil "MAKE-~A" (symbol-name name)) (symbol-package name)))

(defun get-g-boxed-direct-subclasses (name)
  (mapcar (lambda (spec) (destructuring-bind (name slot values) spec
                           (declare (ignore slot values))
                           name))
          (get name 'boxed-dispatch)))

(defun map-append (f &rest lists)
  (reduce #'append (apply #'mapcar f lists)))

(defun get-g-boxed-all-subclasses (name)
  (cons name
        (map-append #'get-g-boxed-all-subclasses (get-g-boxed-direct-subclasses name))))

(defun get-g-boxed-completed-c-definition (name union-name)
  `(defcunion ,union-name
     ,@(mapcar (lambda (sub-name)
                 `(,sub-name ,sub-name))
               (get-g-boxed-all-subclasses name))))

(defun g-boxed-root (name)
  (if (get name 'superclass)
      (g-boxed-root (get name 'superclass))
      name))

(defmacro update-g-boxed-root-c-class (name)
  (when (get name 'c-name)
    (get-g-boxed-completed-c-definition (g-boxed-root name) (get name 'c-name))))

(defmacro define-g-boxed-class (g-name-and-c-name name (&optional superclass-and-dispatch (export t)) &body slots)
  (destructuring-bind (&optional g-name c-name) (ensure-list g-name-and-c-name)
    (destructuring-bind (&optional superclass dispatch-slot dispatch-values) superclass-and-dispatch
      (let* ((superclass-slots (get superclass 'boxed-combined-slots))
             (combined-slots (append superclass-slots slots)))
        
        (setf c-name (or c-name (gensym "C-UNION-")))
        `(progn ,(cstruct-definition name combined-slots)
                ,(struct-definition name superclass slots)
                ,(parse-method-definition name combined-slots)
                ,(unparse-method-definition name combined-slots)
                (eval-when (:load-toplevel :compile-toplevel :execute)
                  (setf (get ',name 'boxed-slots) ',slots
                        (get ',name 'boxed-combined-slots) ',combined-slots
                        (get ',name 'superclass) ',superclass
                        (get ',name 'c-name) (or (get ',name 'c-name) ',c-name))
                  ,@(when superclass
                          (list `(pushnew '(,name ,dispatch-slot ,(ensure-list dispatch-values)) (get ',superclass 'boxed-dispatch) :test 'equalp))))
                (update-g-boxed-root-c-class ,name)
                ,@(when g-name
                        (list `(register-boxed-type ,g-name ',name)))
                ,@(when export
                        (append (list `(export ',name (symbol-package ',name))
                                      `(export ',(struct-constructor-name name) (symbol-package ',(struct-constructor-name name))))
                                (mapcar (lambda (slot) (slot->export-accessor name slot)) slots))))))))

(defun boxed-c-structure-name (name)
  (get (g-boxed-root name) 'c-name))

(defclass g-boxed-ref () ((pointer :accessor pointer :initarg :pointer)))

(defvar *g-boxed-gc-lock* (make-recursive-lock "g-boxed-gc-lock"))
(defvar *known-boxed-refs* (tg:make-weak-hash-table :test 'equal :weakness :value))
(defvar *boxed-ref-count* (make-hash-table :test 'equal))
(defvar *boxed-ref-owner* (make-hash-table :test 'equal))

(defun boxed-ref-free-function (name)
  (or (get name 'free-function)
      (error "g-boxed-ref class ~A has no free-function" name)))

(defun disown-boxed-ref (object)
  (setf (gethash (pointer-address (pointer object)) *boxed-ref-owner*) :foreign))

(defun dispose-boxed-ref (type pointer)
  (debugf "disposing g-boxed-ref ~A~%" pointer)
  
  (unless (gethash (pointer-address pointer) *boxed-ref-count*)
    (error "g-boxed-ref ~A is already disposed from lisp-side" pointer))
  (with-recursive-lock-held (*g-boxed-gc-lock*)
    (let ((object (gethash (pointer-address pointer) *known-boxed-refs*)))
      (when object
        (debugf "Removing finalization from ~A for pointer ~A~%" object pointer)
        (tg:cancel-finalization object)))
    (when (eq :lisp (gethash (pointer-address pointer) *boxed-ref-owner*))
      (funcall (boxed-ref-free-function type) pointer))
    (remhash (pointer-address pointer) *known-boxed-refs*)
    (remhash (pointer-address pointer) *boxed-ref-count*)
    (remhash (pointer-address pointer) *boxed-ref-owner*)
    (debugf "Disposed of g-boxed-ref ~A (object ~A)~%"
            pointer
            (gethash (pointer-address pointer) *known-boxed-refs*))))

(defmethod initialize-instance :after ((object g-boxed-ref) &key)
  (with-recursive-lock-held (*g-boxed-gc-lock*)
    (let ((address (pointer-address (pointer object))))
      (let ((object (gethash address *known-boxed-refs*)))
        (when object
          (tg:cancel-finalization object)))
      (setf (gethash address *known-boxed-refs*) object)
      (setf (gethash address *boxed-ref-count*) 1)
      (setf (gethash address *boxed-ref-owner*)
            (gethash address *boxed-ref-owner* :foreign)))
    (debugf "setting g-boxed-ref-count of ~A (for ptr ~A) to 1~%" object (pointer object))
    (let ((p (pointer object))
          (type (type-of object))
          (s (format nil "~A" object)))
      (tg:finalize object (lambda ()                          
                            (handler-case
                                (dispose-boxed-ref type p)
                              (error (e) (format t "Error ~A for ~A~%" e s))))))))

(defmethod release ((object g-boxed-ref))
  (debugf "releasing g-boxed-ref ~A~%" (pointer object))
  (unless (gethash (pointer-address (pointer object)) *boxed-ref-count*)
    (error "g-boxed-ref ~A is already disposed from lisp-side" (pointer object)))
  (decf (gethash (pointer-address (pointer object)) *boxed-ref-count*))
  (when (zerop (gethash (pointer-address (pointer object)) *boxed-ref-count*))
    (dispose-boxed-ref (type-of object) (pointer object))))

(define-foreign-type g-boxed-ref-type ()
  ((class-name :reader g-boxed-ref-class-name :initarg :class-name)
   (owner :reader g-boxed-ref-owner :initarg :owner :initform nil))
  (:actual-type :pointer))

(define-parse-method g-boxed-ref (class-name &key (owner :foreign))
  (unless (get class-name 'is-g-boxed-ref)
    (error "~A is not a subtype of G-BOXED-REF (~A: ~S)" class-name class-name (symbol-plist class-name)))
  (make-instance 'g-boxed-ref-type :class-name class-name :owner owner))

(defmethod translate-to-foreign (value (type g-boxed-ref-type))
  (if value
      (pointer value)
      (null-pointer)))

(defun convert-g-boxed-ref-from-pointer (pointer name type)
  (unless (null-pointer-p pointer)
    (with-recursive-lock-held (*g-boxed-gc-lock*)
      (or (let ((object (gethash (pointer-address pointer) *known-boxed-refs*)))
            (when object (debugf "Boxed-ref for ~A is found (~A)~%" pointer object))
            (when object (incf (gethash (pointer-address pointer) *boxed-ref-count*)))
            object)
          (let ((object (make-instance name :pointer pointer)))
            (setf (gethash (pointer-address pointer) *boxed-ref-owner*) (g-boxed-ref-owner type))
            (debugf "Boxed-ref for ~A is created (~A) with owner ~A~%" pointer object
                    (gethash (pointer-address pointer) *boxed-ref-owner*))
            object)))))

(defmethod translate-from-foreign (value (type g-boxed-ref-type))
  (let ((owner (or (gethash (pointer-address value) *boxed-ref-owner*) (g-boxed-ref-owner type)))) ;;This is needed to prevent changing ownership of already created
    (prog1
        (convert-g-boxed-ref-from-pointer value (g-boxed-ref-class-name type) type)
      (setf (gethash (pointer-address value) *boxed-ref-owner*) owner))))

(defun g-boxed-ref-slot->methods (class slot)
  (destructuring-bind (slot-name &key reader writer type (accessor slot-name)) slot
    `(progn ,@(when reader
                    (list `(defmethod ,accessor ((object ,class))
                             ,(if (stringp reader)
                                  `(foreign-funcall ,reader :pointer (pointer object) ,type)
                                  `(,reader object)))))
            ,@(when writer
                    (list `(defmethod (setf ,accessor) (new-value (object ,class))
                             ,(if (stringp writer)
                                  `(foreign-funcall ,writer :pointer (pointer object) ,type new-value)
                                  `(,writer new-value object))))))))

(defmacro define-g-boxed-ref (gobject-name name &rest properties)
  (let ((free-fn (second (find :free-function properties :key 'first)))
        (alloc-fn (second (find :alloc-function properties :key 'first)))
        (slots (rest (find :slots properties :key 'first))))
    (unless (and free-fn alloc-fn) (error "All of :free-function, :alloc-function must be specified"))
    `(progn (defclass ,name (g-boxed-ref) ())
            (defmethod initialize-instance :before ((object ,name) &key pointer)
              (unless (or pointer (slot-boundp object 'pointer))
                (setf (pointer object) (,alloc-fn)
                      (gethash (pointer-address (pointer object)) *boxed-ref-owner*) :lisp)))
            (setf (get ',name 'free-function) ',free-fn)
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (setf (get ',name 'is-g-boxed-ref) t))
            ,@(mapcar (lambda (slot)
                        (g-boxed-ref-slot->methods name slot))
                      slots)
            (register-boxed-type ,gobject-name ',name))))

(define-foreign-type fixed-array ()
  ((element-type :reader fixed-array-element-type :initarg :element-type :initform (error "Element type must be specified"))
   (array-size :reader fixed-array-array-size :initarg :array-size :initform (error "Array size must be specified")))
  (:actual-type :pointer))

(define-parse-method fixed-array (element-type array-size)
  (make-instance 'fixed-array :element-type element-type :array-size array-size))

(defmethod translate-from-foreign (ptr (type fixed-array))
  (when (not (null-pointer-p ptr))
    (let ((result (make-array (fixed-array-array-size type)))
          (el-type (fixed-array-element-type type)))
      (loop
         for i from 0 below (fixed-array-array-size type)
         do (setf (aref result i) (mem-aref ptr el-type i)))
      result)))

(defvar *registered-boxed-types* (make-hash-table :test 'equal))
(defvar *registered-boxed-names* (make-hash-table))
(defun register-boxed-type (name type)
  (setf (gethash name *registered-boxed-types*) type
        (gethash type *registered-boxed-names*) name))
(defun get-registered-boxed-type (name)
  (gethash name *registered-boxed-types*))

(defun boxed-type-gname (type)
  (gethash type *registered-boxed-names*))

(defun set-gvalue-boxed (gvalue value)
  (if value
      (progn
        (cond
          ((typep value 'g-boxed-ref)
           (g-value-set-boxed gvalue (pointer value)))
          (t (g-value-take-boxed gvalue (g-boxed->cstruct value :alloc-type :boxed)))))
      (g-value-set-boxed gvalue (null-pointer))))

(defun parse-gvalue-boxed (gvalue)
  (let* ((g-type (gvalue-type gvalue))
         (type-name (g-type-name g-type))
         (boxed-type (get-registered-boxed-type type-name)))
    (unless boxed-type
      (warn t "Type ~A is a not registered GBoxed~%" type-name)
      (return-from parse-gvalue-boxed nil))
    (unless (null-pointer-p (g-value-get-boxed gvalue))
      (cond
        ((subtypep boxed-type 'g-boxed-ref) (convert-g-boxed-ref-from-pointer (g-value-get-boxed gvalue) boxed-type (make-instance 'g-boxed-ref-type :class-name boxed-type :owner :foreign)))
        (t (parse-g-boxed (g-value-get-boxed gvalue) boxed-type))))))