(in-package :gobject)

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

(define-boxed-variant-cstruct evt "evt"
  (type :int :initform 0)
  (time :uint :initform 0)
  (:variant type
            (0 evt-zero
               (x :double :initform 0.0d0)
               (y :double :initform 0.0d0))
            ((1 2 3) evt-multi
             (t2 :int :initform 0)
             (:variant t2
                       (1 evt-single
                          (item :uchar :initform 0))))))

(defcallback test-evt (g-boxed-foreign evt)
    ((time :int) (e1 (g-boxed-foreign evt)))
  (print time)
  (print e1)
  (incf (evt-time e1) time)
  (make-evt-multi :time time :t2 123))

(defun do-test-evt (e1 time)
  (let ((e2 (foreign-funcall-pointer (callback test-evt) () :int time (g-boxed-foreign evt) e1 (g-boxed-foreign evt))))
    (values e1 e2)))
