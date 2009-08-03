(in-package :gobject)

(define-foreign-type g-boxed-foreign-type ()
  ((info :initarg :info
         :accessor g-boxed-foreign-info
         :initform (error "info must be specified")))
  (:actual-type :pointer))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct g-boxed-info
    name
    g-type))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun get-g-boxed-foreign-info (name)
    (get name 'g-boxed-foreign-info)))

(defvar *g-type-name->g-boxed-foreign-info* (make-hash-table :test 'equal))

(defun get-g-boxed-foreign-info-for-gtype (g-type-designator)
  (or (gethash (g-type-string g-type-designator) *g-type-name->g-boxed-foreign-info*)
      (error "Unknown GBoxed type '~A'" (g-type-string g-type-designator))))

(define-parse-method g-boxed-foreign (name)
  (let ((info (get-g-boxed-foreign-info name)))
    (assert info nil "Unknown foreign GBoxed type ~A" name)
    (make-instance 'g-boxed-foreign-type :info info)))

(defgeneric boxed-proxy-to-native (type-info proxy))

(defgeneric boxed-read-values-from-native (type-info proxy native))

(defgeneric boxed-native-to-proxy (type-info native))

(defgeneric boxed-write-values-to-native-and-free (type-info proxy native))

(defmethod translate-to-foreign (proxy (type g-boxed-foreign-type))
  (if proxy
      (let ((boxed-type-info (g-boxed-foreign-info type)))
        (values (boxed-proxy-to-native boxed-type-info proxy) proxy))
      (null-pointer)))

(defmethod free-translated-object (native-structure (type g-boxed-foreign-type) proxy)
  (when proxy
    (let ((boxed-type-info (g-boxed-foreign-info type)))
      (boxed-read-values-from-native boxed-type-info proxy native-structure)
      (g-boxed-free (g-boxed-info-g-type boxed-type-info) native-structure))))

(defmethod translate-from-foreign (native-structure (type g-boxed-foreign-type))
  (unless (null-pointer-p native-structure)
    (let ((info (g-boxed-foreign-info type)))
      (boxed-native-to-proxy info native-structure))))

(defmethod cleanup-translated-object-for-callback ((type g-boxed-foreign-type) proxy native-structure)
  (unless (null-pointer-p native-structure)
    (let ((info (g-boxed-foreign-info type)))
      (boxed-write-values-to-native-and-free info proxy native-structure))))

(defmethod has-callback-cleanup ((type g-boxed-foreign-type))
  t)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defstruct (g-boxed-cstruct-wrapper-info (:include g-boxed-info))
    cstruct
    slots))

(defmacro define-g-boxed-cstruct (name g-type-name &body slots)
  `(progn
     (defstruct ,name
       ,@(iter (for (name type &key initarg) in slots)
               (collect (list name initarg))))
     (defcstruct ,(generated-cstruct-name name)
       ,@(iter (for (name type &key initarg) in slots)
               (collect `(,name ,type))))
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (get ',name 'g-boxed-foreign-info)
             (make-g-boxed-cstruct-wrapper-info :name ',name
                                                :g-type ,g-type-name
                                                :cstruct ',(generated-cstruct-name name)
                                                :slots ',(iter (for (name type &key initarg) in slots)
                                                               (collect name)))
             (gethash ,g-type-name *g-type-name->g-boxed-foreign-info*)
             (get ',name 'g-boxed-foreign-info)))))

(defmethod boxed-proxy-to-native ((type g-boxed-cstruct-wrapper-info) proxy)
  (let* ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type))
         (native-structure (foreign-alloc native-structure-type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (foreign-slot-value native-structure native-structure-type slot)
                (slot-value proxy slot)))
    (prog1 (g-boxed-copy (g-boxed-info-g-type type) native-structure)
      (foreign-free native-structure))))

(defmethod boxed-native-to-proxy ((type g-boxed-cstruct-wrapper-info) native-structure)
  (let* ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type))
         (proxy-structure-type (g-boxed-info-name type))
         (proxy (make-instance proxy-structure-type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (slot-value proxy slot)
                (foreign-slot-value native-structure native-structure-type slot)))
    proxy))

(defmethod boxed-read-values-from-native ((type g-boxed-cstruct-wrapper-info) proxy native-structure)
  (let ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (slot-value proxy slot)
                (foreign-slot-value native-structure native-structure-type slot)))))

(defmethod boxed-write-values-to-native-and-free ((type g-boxed-cstruct-wrapper-info) proxy native-structure)
  (let ((native-structure-type (g-boxed-cstruct-wrapper-info-cstruct type)))
    (iter (for slot in (g-boxed-cstruct-wrapper-info-slots type))
          (setf (foreign-slot-value native-structure native-structure-type slot)
                (slot-value proxy slot)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (g-boxed-opaque-wrapper-info (:include g-boxed-info))
    alloc free))

(defclass g-boxed-opaque ()
  ((pointer :initarg :pointer
            :initform nil
            :accessor g-boxed-opaque-pointer)))

(defmethod boxed-proxy-to-native ((type g-boxed-opaque-wrapper-info) proxy)
  (g-boxed-copy (g-boxed-info-g-type type) (g-boxed-opaque-pointer proxy)))

(defun make-boxed-free-finalizer (g-type pointer)
  (lambda () (g-boxed-free g-type pointer)))

(defmethod boxed-native-to-proxy ((type g-boxed-opaque-wrapper-info) native)
  (let* ((g-type (g-boxed-info-g-type type))
         (proxy (make-instance (g-boxed-info-name type) :pointer native)))
    (tg:finalize proxy (make-boxed-free-finalizer g-type native))))

(defmethod boxed-read-values-from-native ((type g-boxed-opaque-wrapper-info) proxy native)
  (g-boxed-free (g-boxed-info-g-type type) (g-boxed-opaque-pointer proxy))
  (tg:cancel-finalization proxy)
  (tg:finalize proxy (make-boxed-free-finalizer (g-boxed-info-g-type type) native)))

(defmethod boxed-write-values-to-native-and-free ((type g-boxed-opaque-wrapper-info) proxy native)
  (declare (ignore type native))
  (tg:cancel-finalization proxy))

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
                                                      :g-type ,g-type-name)
                    (gethash ,g-type-name *g-type-name->g-boxed-foreign-info*)
                    (get ',name 'g-boxed-foreign-info))))))

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

(defun ensure-list (thing)
  (if (listp thing)
      thing
      (list thing)))

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
      (setf (get symbol 'generated-cunion-name) (gensym (format nil "GEN-~A-CUNION-" (symbol-name symbol))))))

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

(defmacro define-g-boxed-variant-cstruct (name g-type-name &body slots)
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
                                                       ,(generate-proxy-type-decision-procedure structure))
                    (gethash ,g-type-name *g-type-name->g-boxed-foreign-info*)
                    (get ',name 'g-boxed-foreign-info))))))

(defun decide-native-type (info proxy)
  (funcall (g-boxed-variant-cstruct-info-native-type-decision-procedure info) proxy))

(defmethod boxed-proxy-to-native ((type g-boxed-variant-cstruct-info) proxy)
  (multiple-value-bind (actual-cstruct slots) (decide-native-type type proxy)
    (let ((native-structure (foreign-alloc
                             (generated-cstruct-name
                              (var-structure-name
                               (g-boxed-variant-cstruct-info-root type))))))
      (iter (for slot in slots)
            (setf (foreign-slot-value native-structure actual-cstruct slot)
                  (slot-value proxy slot)))
      (prog1 (g-boxed-copy (g-boxed-info-g-type type) native-structure)
        (foreign-free native-structure)))))

(defun decide-proxy-type (info native-structure)
  (funcall (g-boxed-variant-cstruct-info-proxy-type-decision-procedure info) native-structure))

(defmethod boxed-write-values-to-native-and-free ((type g-boxed-variant-cstruct-info) proxy native-ptr)
  (multiple-value-bind (actual-struct slots actual-cstruct) (decide-proxy-type type native-ptr)
    (unless (eq (type-of proxy) actual-struct)
      (restart-case
          (error "Expected type of boxed variant structure ~A and actual type ~A do not match"
                 (type-of proxy) actual-struct)
        (skip-parsing-values () (return-from boxed-write-values-to-native-and-free))))
    (iter (for slot in slots)
          (setf (slot-value proxy slot)
                (foreign-slot-value native-ptr actual-cstruct slot)))))

(defmethod boxed-native-to-proxy ((type g-boxed-variant-cstruct-info) native-ptr)
  (multiple-value-bind (actual-struct slots actual-cstruct) (decide-proxy-type type native-ptr)
    (let ((proxy (make-instance actual-struct)))
      (iter (for slot in slots)
            (setf (slot-value proxy slot)
                  (foreign-slot-value native-ptr actual-cstruct slot)))
      proxy)))

(defgeneric boxed-native-to-proxy-needs-copy-for-gvalue-get (type))

(defmethod boxed-native-to-proxy-needs-copy-for-gvalue-get ((type g-boxed-cstruct-wrapper-info))
  nil)

(defmethod boxed-native-to-proxy-needs-copy-for-gvalue-get ((type g-boxed-variant-cstruct-info))
  nil)

(defmethod boxed-native-to-proxy-needs-copy-for-gvalue-get ((type g-boxed-opaque-wrapper-info))
  t)

(defmethod parse-g-value-for-type (gvalue-ptr (type-numeric (eql +g-type-boxed+)) parse-kind)
  (declare (ignore parse-kind))
  (if (g-type= (g-value-type gvalue-ptr) (g-strv-get-type))
      (convert-from-foreign (g-value-get-boxed gvalue-ptr) '(glib:gstrv :free-from-foreign nil))
      (let* ((boxed-type (get-g-boxed-foreign-info-for-gtype type-numeric))
             (native (if (boxed-native-to-proxy-needs-copy-for-gvalue-get boxed-type)
                         (g-boxed-copy type-numeric (g-value-get-boxed gvalue-ptr))
                         (g-value-get-boxed gvalue-ptr))))
        (boxed-native-to-proxy boxed-type native))))

(defmethod set-gvalue-for-type (gvalue-ptr (type-numeric (eql +g-type-boxed+)) value)
  (if (g-type= (g-value-type gvalue-ptr) (g-strv-get-type))
      (g-value-set-boxed gvalue-ptr (convert-to-foreign value '(glib:gstrv :free-from-foreign nil)))
      (let* ((boxed-type (get-g-boxed-foreign-info-for-gtype type-numeric))
             (native (boxed-proxy-to-native boxed-type value)))
        (g-value-take-boxed gvalue-ptr native))))
