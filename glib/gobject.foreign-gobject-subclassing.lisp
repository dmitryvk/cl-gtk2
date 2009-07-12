(in-package :gobject)

(defvar *registered-types* (make-hash-table :test 'equal))

(defstruct object-type name class parent interfaces properties)

(defvar *lisp-objects-references* (make-hash-table :test 'equal))

(defun object-toggle-pointer (data object is-last-ref)
  (declare (ignore data))
  (debugf "Toggling pointer on ~a (~A) to being ~A~%" object (gethash (pointer-address object) *lisp-objects-references*) (if is-last-ref "last ref" "not last ref"))
  (if is-last-ref
      (remhash (pointer-address object) *lisp-objects-references*)
      (setf (gethash (pointer-address object) *lisp-objects-references*) (gethash (pointer-address object) *foreign-gobjects*))))

(defun instance-init (instance class)
  (debugf "Initializing instance ~A for type ~A (creating ~A)~%" instance (g-type-name (foreign-slot-value class 'g-type-class :type)) *current-creating-object*)
  (unless (gethash (pointer-address instance) *lisp-objects-pointers*)
    (debugf "  Proceeding with initialization...")
    (setf (gethash (pointer-address instance) *lisp-objects-pointers*) t
          (gethash (pointer-address instance) *lisp-objects-references*)
          (or *current-creating-object*
              (let* ((g-type (foreign-slot-value class 'g-type-class :type))
                     (type-name (g-type-name g-type))
                     (lisp-type-info (gethash type-name *registered-types*))
                     (lisp-class (object-type-class lisp-type-info)))
                (make-instance lisp-class :pointer instance))))
    (g-object-add-toggle-ref instance (callback c-object-toggle-pointer) (null-pointer))
    (g-object-unref instance)))

(defcallback c-object-toggle-pointer :void ((data :pointer) (object :pointer) (is-last-ref :boolean))
  (object-toggle-pointer data object is-last-ref))

(defcallback c-instance-init :void ((instance :pointer) (class :pointer))
  (instance-init instance class))

(defcallback c-class-init :void ((class :pointer) (data :pointer))
  (class-init class data))

(defun minimum-foreign-integer (type &optional (signed t))
  (if signed
      (- (ash 1 (1- (* 8 (foreign-type-size type)))))
      0))

(defun maximum-foreign-integer (type &optional (signed t))
  (if signed
      (1- (ash 1 (1- (* 8 (foreign-type-size type)))))
      (1- (ash 1 (* 8 (foreign-type-size type))))))

(defun property->param-spec (property)
  (destructuring-bind (property-name property-type accessor property-get-fn property-set-fn) property
    (declare (ignore accessor))
    (let ((property-g-type (ensure-g-type property-type))
          (flags (append (when property-get-fn (list :readable))
                         (when property-set-fn (list :writable)))))
      (ev-case (g-type-fundamental property-g-type)
        (+g-type-invalid+ (error "GValue is of invalid type ~A (~A)" property-g-type (g-type-name property-g-type)))
        (+g-type-void+ nil)
        (+g-type-char+ (g-param-spec-char property-name property-name property-name (minimum-foreign-integer :char) (maximum-foreign-integer :char) 0 flags))
        (+g-type-uchar+ (g-param-spec-uchar property-name property-name property-name (minimum-foreign-integer :uchar nil) (maximum-foreign-integer :uchar nil) 0 flags))
        (+g-type-boolean+ (g-param-spec-boolean property-name property-name property-name nil flags))
        (+g-type-int+ (g-param-spec-int property-name property-name property-name (minimum-foreign-integer :int) (maximum-foreign-integer :int) 0 flags))
        (+g-type-uint+ (g-param-spec-uint property-name property-name property-name (minimum-foreign-integer :uint nil) (maximum-foreign-integer :uint nil) 0 flags))
        (+g-type-long+ (g-param-spec-long property-name property-name property-name (minimum-foreign-integer :long) (maximum-foreign-integer :long) 0 flags))
        (+g-type-ulong+ (g-param-spec-ulong property-name property-name property-name (minimum-foreign-integer :ulong nil) (maximum-foreign-integer :ulong nil) 0 flags))
        (+g-type-int64+ (g-param-spec-int64 property-name property-name property-name (minimum-foreign-integer :int64) (maximum-foreign-integer :int64) 0 flags))
        (+g-type-uint64+ (g-param-spec-uint64 property-name property-name property-name (minimum-foreign-integer :uint64 nil) (maximum-foreign-integer :uint64 t) 0 flags))
        (+g-type-enum+ (g-param-spec-enum property-name property-name property-name property-g-type (enum-item-value (first (get-enum-items property-g-type))) flags))
        (+g-type-flags+ (g-param-spec-enum property-name property-name property-name property-g-type (flags-item-value (first (get-flags-items property-g-type))) flags))
        (+g-type-float+ (g-param-spec-float property-name property-name property-name most-negative-single-float most-positive-single-float 0.0 flags))
        (+g-type-double+ (g-param-spec-double property-name property-name property-name most-negative-double-float most-positive-double-float 0.0d0 flags))
        (+g-type-string+ (g-param-spec-string property-name property-name property-name "" flags))
        (+g-type-pointer+ (g-param-spec-pointer property-name property-name property-name flags))
        (+g-type-boxed+ (g-param-spec-boxed property-name property-name property-name property-g-type flags))
                                        ;(+g-type-param+ (parse-gvalue-param gvalue))
        (+g-type-object+ (g-param-spec-object property-name property-name property-name property-g-type flags))
                                        ;(+g-type-interface+ )
        (t (error "Unknown type: ~A (~A)" property-g-type (g-type-name property-g-type)))))))

(defun install-properties (class)
  (let* ((name (g-type-name (foreign-slot-value class 'g-type-class :type)))
         (lisp-type-info (gethash name *registered-types*)))
    (iter (for property in (object-type-properties lisp-type-info))
          (for param-spec = (property->param-spec property))
          (for property-id from 123)
          (debugf "installing property ~A~%" property)
          (g-object-class-install-property class property-id param-spec))))

(defun vtable-item->cstruct-item (item)
  (if (eq :skip (first item))
      (rest item)
      (list (first item) :pointer)))

(defstruct vtable-method-info name return-type args callback-name)

(defmethod make-load-form ((object vtable-method-info) &optional environment)
  (declare (ignore environment))
  `(make-vtable-method-info :name ',(vtable-method-info-name object)
                            :return-type ',(vtable-method-info-return-type object)
                            :args ',(vtable-method-info-args object)
                            :callback-name ',(vtable-method-info-callback-name object)))

(defun vtable-methods (items)
  (iter (for item in items)
        (when (eq :skip (first item)) (next-iteration))
        (destructuring-bind (name callback-name return-type &rest args) item
          (collect (make-vtable-method-info :name name :return-type return-type :args args :callback-name callback-name)))))

(defvar *vtables* (make-hash-table :test 'equal))

(defstruct vtable-description type-name cstruct-name methods)

(defmacro define-vtable ((type-name cstruct-name) &body items)
  `(progn
     (defcstruct ,cstruct-name ,@(mapcar #'vtable-item->cstruct-item items))
     (setf (gethash ,type-name *vtables*)
           (make-vtable-description :type-name ,type-name :cstruct-name ',cstruct-name :methods (list ,@(mapcar #'make-load-form (vtable-methods items)))))
     ,@(iter (for method in (vtable-methods items))
             (collect `(defgeneric ,(vtable-method-info-name method) (,@(mapcar #'first (vtable-method-info-args method)))))
             (collect `(defcallback ,(vtable-method-info-callback-name method) ,(vtable-method-info-return-type method)
                           (,@(vtable-method-info-args method))
                         (restart-case 
                             (,(vtable-method-info-name method) ,@(mapcar #'first (vtable-method-info-args method)))
                           (return-from-interface-method-implementation (v) :interactive (lambda () (list (eval (read)))) v)))))))

(defun interface-init (iface data)
  (destructuring-bind (class-name interface-name) (prog1 (get-stable-pointer-value data) (free-stable-pointer data))
    (let* ((vtable (gethash interface-name *vtables*))
           (vtable-cstruct (vtable-description-cstruct-name vtable)))
      (debugf "interface-init for class ~A and interface ~A~%" class-name interface-name)
      (iter (for method in (vtable-description-methods vtable))
            (setf (foreign-slot-value iface vtable-cstruct (vtable-method-info-name method)) (get-callback (vtable-method-info-callback-name method)))))))

(defcallback c-interface-init :void ((iface :pointer) (data :pointer))
  (interface-init iface data))

(defun add-interface (name interface)
  (let* ((interface-info (list name interface))
         (interface-info-ptr (allocate-stable-pointer interface-info)))
    (with-foreign-object (info 'g-interface-info)
      (setf (foreign-slot-value info 'g-interface-info :interface-init) (callback c-interface-init)
            (foreign-slot-value info 'g-interface-info :interface-data) interface-info-ptr)
      (g-type-add-interface-static (g-type-from-name name) (ensure-g-type interface) info))))

(defun add-interfaces (name)
  (let* ((lisp-type-info (gethash name *registered-types*))
         (interfaces (object-type-interfaces lisp-type-info)))
    (iter (for interface in interfaces)
          (add-interface name interface))))

(defun class-init (class data)
  (declare (ignore data))
  (debugf "class-init for ~A~%" (g-type-name (g-type-from-class class)))
  (setf (foreign-slot-value class 'g-object-class :get-property)
        (callback c-object-property-get)
        (foreign-slot-value class 'g-object-class :set-property)
        (callback c-object-property-set))
  
  (install-properties class))

(defun object-property-get (object property-id g-value pspec)
  (declare (ignore property-id))
  (let* ((lisp-object (gethash (pointer-address object) *lisp-objects-references*))
         (property-name (foreign-slot-value pspec 'g-param-spec :name))
         (property-type (foreign-slot-value pspec 'g-param-spec :value-type))
         (type-name (g-type-name (foreign-slot-value pspec 'g-param-spec :owner-type)))
         (lisp-type-info (gethash type-name *registered-types*))
         (property-info (find property-name (object-type-properties lisp-type-info) :test 'string= :key 'first))
         (property-get-fn (fourth property-info)))
    (debugf "get(~A,'~A')~%" lisp-object property-name)
    (let ((value (restart-case
                     (funcall property-get-fn lisp-object)
                   (return-from-property-getter (value) :interactive (lambda () (debugf "Enter new value: ") (list (eval (read)))) value))))
      (set-g-value g-value value property-type))))

(defcallback c-object-property-get :void ((object :pointer) (property-id :uint) (value :pointer) (pspec :pointer))
  (object-property-get object property-id value pspec))

(defun object-property-set (object property-id value pspec)
  (declare (ignore property-id))
  (let* ((lisp-object (gethash (pointer-address object) *lisp-objects-references*))
         (property-name (foreign-slot-value pspec 'g-param-spec :name))
         (type-name (g-type-name (foreign-slot-value pspec 'g-param-spec :owner-type)))
         (lisp-type-info (gethash type-name *registered-types*))
         (property-info (find property-name (object-type-properties lisp-type-info) :test 'string= :key 'first))
         (property-set-fn (fifth property-info))
         (new-value (parse-gvalue value)))
    (debugf "set(~A,'~A',~A)~%" lisp-object property-name new-value)
    (restart-case
        (funcall property-set-fn new-value lisp-object)
      (return-without-error-from-property-setter () nil))))

(defcallback c-object-property-set :void ((object :pointer) (property-id :uint) (value :pointer) (pspec :pointer))
  (object-property-set object property-id value pspec))

(defmacro register-object-type-implementation (name class parent interfaces properties)
  (unless (stringp parent)
    (setf parent (g-type-name (ensure-g-type parent))))
  `(progn
     (setf (gethash ,name *registered-types*) (make-object-type :name ,name :class ',class :parent ,parent :interfaces ',interfaces :properties ',properties))
     (at-init (',class)
       (debugf "Registering GObject type implementation ~A for type ~A~%" ',class ,name)
       (with-foreign-object (query 'g-type-query)
         (g-type-query (g-type-from-name ,parent) query)
         (with-foreign-slots ((class-size instance-size) query g-type-query)
           (g-type-register-static-simple (g-type-from-name ,parent)
                                          ,name
                                          (foreign-slot-value query 'g-type-query :class-size)
                                          (callback c-class-init)
                                          (foreign-slot-value query 'g-type-query :instance-size)
                                          (callback c-instance-init) nil)))
       (add-interfaces ,name))
     (defmethod initialize-instance :before ((object ,class) &key pointer)
       (unless (or pointer (and (slot-boundp object 'gobject::pointer)
                                (gobject::pointer object)))
         (setf (gobject::pointer object) (gobject::g-object-call-constructor ,name nil nil)
               (gobject::g-object-has-reference object) t)))
     (progn
       ,@(iter (for (prop-name prop-type prop-accessor prop-reader prop-writer) in properties)
               (when prop-reader
                 (collect `(defun ,prop-accessor (object) (g-object-call-get-property object ,prop-name))))
               (when prop-writer
                 (collect `(defun (setf ,prop-accessor) (new-value object) (g-object-call-set-property object ,prop-name new-value))))))
     ,name))