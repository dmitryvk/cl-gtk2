(in-package :gobject)

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

(defstruct var-structure
  name
  parent
  slots
  discriminator-slot
  variants)

(defstruct var-structure-variant
  discriminating-values
  structure)

(defstruct var-structure-slot
  name
  type
  initform
  count)

(defmethod make-load-form ((object var-structure) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-slot) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmethod make-load-form ((object var-structure-variant) &optional env)
  (make-load-form-saving-slots object :environment env))

(defun var-struct-all-slots (struct)
  (when struct
    (append (var-struct-all-slots (var-structure-parent struct))
            (var-structure-slots struct))))

(defun all-structures (structure)
  (append (iter (for variant in (var-structure-variants structure))
                (appending (all-structures (var-structure-variant-structure variant))))
          (list structure)))

(defun parse-variant-structure-definition (name slots &optional parent)
  (iter (with result = (make-var-structure :name name
                                           :parent parent
                                           :slots nil
                                           :discriminator-slot nil
                                           :variants nil))
        (for slot in slots)
        (if (eq :variant (first slot))
            (progn
              (when (var-structure-discriminator-slot result)
                (error "Structure has more than one discriminator slot"))
              (setf (var-structure-discriminator-slot result) (second slot)
                    (var-structure-variants result) (parse-variants result (nthcdr 2 slot))))
            (push (parse-slot slot) (var-structure-slots result)))
        (finally (setf (var-structure-slots result)
                       (reverse (var-structure-slots result)))
                 (return result))))

(defun parse-slot (slot)
  (destructuring-bind (name type &key count initform) slot
    (make-var-structure-slot :name name :type type :count count :initform initform)))

(defun parse-variants (parent variants)
  (iter (for var-descr in variants)
        (for (options variant-name . slots) in variants)
        (for variant =
             (make-var-structure-variant
              :discriminating-values (ensure-list options)
              :structure (parse-variant-structure-definition variant-name slots parent)))
        (collect variant)))

(defun generated-cstruct-name (symbol)
  (or (get symbol 'generated-cstruct-name)
      (setf (get symbol 'generated-cstruct-name) (gensym (format nil "GEN-~A-CSTRUCT-" (symbol-name symbol))))))

(defun generated-cunion-name (symbol)
  (or (get symbol 'generated-cunion-name)
      (setf (get symbol 'generated-cunion-name) (gensym (format nil "GEN-~A-CSTRUCT-" (symbol-name symbol))))))

(defun generate-cstruct-1 (struct)
  `(defcstruct ,(generated-cstruct-name (var-structure-name struct))
     ,@(iter (for slot in (var-struct-all-slots struct))
             (collect `(,(var-structure-slot-name slot) ,(var-structure-slot-type slot)
                         ,@(when (var-structure-slot-count slot)
                                 (list `(:count ,(var-structure-slot-count slot)))))))))

(defun generate-c-structures (structure)
  (iter (for str in (all-structures structure))
        (collect (generate-cstruct-1 str))))

(defun generate-union-1 (struct)
  `(defcunion ,(generated-cunion-name (var-structure-name struct))
     ,@(iter (for variant in (all-structures struct))
             (unless (eq struct variant)
               (collect `(,(var-structure-name variant)
                           ,(generated-cunion-name (var-structure-name variant))))))))

(defun generate-unions (struct)
  (iter (for str in (all-structures struct))
        (collect (generate-union-1 str))))

(defun generate-structure-1 (str)
  `(defstruct ,(if (var-structure-parent str)
                   `(,(var-structure-name str) (:include ,(var-structure-name (var-structure-parent str))
                                                         (,(var-structure-discriminator-slot (var-structure-parent str))
                                                           ,(first (var-structure-variant-discriminating-values
                                                                    (find str
                                                                          (var-structure-variants
                                                                           (var-structure-parent str))
                                                                          :key #'var-structure-variant-structure))))))
                   `,(var-structure-name str))
     ,@(iter (for slot in (var-structure-slots str))
             (collect `(,(var-structure-slot-name slot)
                         ,(var-structure-slot-initform slot))))))

(defun generate-structures (str)
  (iter (for variant in (reverse (all-structures str)))
        (collect (generate-structure-1 variant))))

(defun generate-native-type-decision-procedure-1 (str proxy-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(generated-cstruct-name (var-structure-name str))
               ',(mapcar #'var-structure-slot-name (var-struct-all-slots str)))
      `(typecase ,proxy-var
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-name v-str)
                             ,(generate-native-type-decision-procedure-1 v-str proxy-var))))
         (,(var-structure-name str)
          (values ',(generated-cstruct-name (var-structure-name str))
                  ',(mapcar #'var-structure-slot-name (var-struct-all-slots str)))))))

(defun generate-proxy-type-decision-procedure-1 (str native-var)
  (if (null (var-structure-discriminator-slot str))
      `(values ',(var-structure-name str)
               ',(mapcar #'var-structure-slot-name (var-struct-all-slots str))
               ',(generated-cstruct-name (var-structure-name str)))
      `(case (foreign-slot-value ,native-var
                                 ',(generated-cstruct-name (var-structure-name str))
                                 ',(var-structure-discriminator-slot str))
         ,@(iter (for variant in (var-structure-variants str))
                 (for v-str = (var-structure-variant-structure variant))
                 (collect `(,(var-structure-variant-discriminating-values variant)
                             ,(generate-proxy-type-decision-procedure-1
                               v-str
                               native-var))))
         (t (values ',(var-structure-name str)
                    ',(mapcar #'var-structure-slot-name (var-struct-all-slots str))
                    ',(generated-cstruct-name (var-structure-name str)))))))

(defun generate-proxy-type-decision-procedure (str)
  (let ((native (gensym "NATIVE-")))
    `(lambda (,native)
       ,(generate-proxy-type-decision-procedure-1 str native))))

(defun generate-native-type-decision-procedure (str)
  (let ((proxy (gensym "PROXY-")))
    `(lambda (,proxy)
       ,(generate-native-type-decision-procedure-1 str proxy))))

(defun compile-proxy-type-decision-procedure (str)
  (compile nil (generate-proxy-type-decision-procedure str)))

(defun compile-native-type-decision-procedure (str)
  (compile nil (generate-native-type-decision-procedure str)))

(defstruct (g-boxed-variant-cstruct-info (:include g-boxed-info))
  root
  native-type-decision-procedure
  proxy-type-decision-procedure)

(defmethod make-load-form ((object g-boxed-variant-cstruct-info) &optional env)
  (make-load-form-saving-slots object :environment env))

(defmacro define-boxed-variant-cstruct (name g-type-name &body slots)
  (let* ((structure (parse-variant-structure-definition name slots)))
    `(progn ,@(generate-c-structures structure)
            ,@(generate-unions structure)
            ,@(generate-structures structure)
            (eval-when (:compile-toplevel :load-toplevel :execute)
              (setf (get ',name 'g-boxed-foreign-info)
                    (make-g-boxed-variant-cstruct-info :name ',name
                                                       :g-type ,g-type-name
                                                       :root ,structure
                                                       :native-type-decision-procedure
                                                       ,(generate-native-type-decision-procedure structure)
                                                       :proxy-type-decision-procedure
                                                       ,(generate-proxy-type-decision-procedure structure)))))))

(defun decide-native-type (info proxy)
  (funcall (g-boxed-variant-cstruct-info-native-type-decision-procedure info) proxy))

(defmethod create-temporary-native ((type g-boxed-variant-cstruct-info) proxy)
  (multiple-value-bind (actual-cstruct slots) (decide-native-type type proxy)
    (let ((native-structure (foreign-alloc
                             (generated-cstruct-name
                              (var-structure-name
                               (g-boxed-variant-cstruct-info-root type))))))
      (iter (for slot in slots)
            (setf (foreign-slot-value native-structure actual-cstruct slot)
                  (slot-value proxy slot)))
      native-structure)))

(defun decide-proxy-type (info native-structure)
  (funcall (g-boxed-variant-cstruct-info-proxy-type-decision-procedure info) native-structure))

(defmethod free-temporary-native ((type g-boxed-variant-cstruct-info) proxy native-ptr)
  (multiple-value-bind (actual-struct slots actual-cstruct) (decide-proxy-type type native-ptr)
    (unless (eq (type-of proxy) actual-struct)
      (restart-case
          (error "Expected type of boxed variant structure ~A and actual type ~A do not match"
                 (type-of proxy) actual-struct)
        (skip-parsing-values () (return-from free-temporary-native))))
    (iter (for slot in slots)
          (setf (slot-value proxy slot)
                (foreign-slot-value native-ptr actual-cstruct slot)))))

(defmethod create-proxy-for-native ((type g-boxed-variant-cstruct-info) native-ptr)
  (multiple-value-bind (actual-struct slots actual-cstruct) (decide-proxy-type type native-ptr)
    (let ((proxy (make-instance actual-struct)))
      (iter (for slot in slots)
            (setf (slot-value proxy slot)
                  (foreign-slot-value native-ptr actual-cstruct slot)))
      proxy)))

(defmethod create-reference-proxy ((type g-boxed-variant-cstruct-info) native-ptr)
  (create-proxy-for-native type native-ptr))

(defmethod free-reference-proxy ((type g-boxed-variant-cstruct-info) proxy native-ptr)
  (multiple-value-bind (actual-cstruct slots) (decide-native-type type proxy)
    (iter (for slot in slots)
          (setf (foreign-slot-value native-ptr actual-cstruct slot)
                (slot-value proxy slot)))))
