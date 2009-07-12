(in-package :gobject)

(defun g-type-from-object (object-ptr)
  "Returns the GType of an @code{object-ptr}

@arg[object-ptr]{C pointer to an object}
@return{GType designator (see @class{g-type-designator})}"
  (g-type-from-instance object-ptr))

(defun g-type-from-class (g-class)
  (g-type-name (foreign-slot-value g-class 'g-type-class :type)))

(defun g-type-from-instance (type-instance)
  (g-type-from-class (foreign-slot-value type-instance 'g-type-instance :class)))

(defun g-type-from-interface (type-interface)
  (g-type-name (foreign-slot-value type-interface 'g-type-interface :type)))

(define-condition property-access-error (error)
  ((property-name :initarg :property-name :reader property-access-error-property-name)
   (class-name :initarg :class-name :reader property-access-error-class-name)
   (message :initarg :message :reader property-access-error-message))
  (:report (lambda (condition stream)
             (format stream "Error accessing property '~A' on class '~A': ~A"
                     (property-access-error-property-name condition)
                     (property-access-error-class-name condition)
                     (property-access-error-message condition)))))

(define-condition property-unreadable-error (property-access-error)
  ()
  (:default-initargs :message "property is not readable"))

(define-condition property-unwritable-error (property-access-error)
  ()
  (:default-initargs :message "property is not writable"))

(defun g-object-type-property-type (object-type property-name
                               &key assert-readable assert-writable)
  (let* ((property (class-property-info object-type property-name)))
    (when (and assert-readable (not (g-class-property-definition-readable property)))
      (error 'property-unreadable-error
             :property-name property-name
             :class-name (g-type-string object-type)))
    (when (and assert-writable (not (g-class-property-definition-writable property)))
      (error 'property-unwritable-error
             :property-name property-name
             :class-name (g-type-string object-type)))
    (g-class-property-definition-type property)))

(defun g-object-property-type (object-ptr property-name &key assert-readable assert-writable)
  (g-object-type-property-type (g-type-from-object object-ptr) property-name :assert-readable assert-readable :assert-writable assert-writable))

(defun g-object-call-get-property (object-ptr property-name &optional property-type)
  (restart-case
      (unless property-type
        (setf property-type
              (g-object-type-property-type (g-type-from-object object-ptr) property-name :assert-readable t)))
    (return-nil () (return-from g-object-call-get-property nil)))
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (g-value-init value property-type)
    (g-object-get-property object-ptr property-name value)
    (unwind-protect
         (parse-g-value value)
      (g-value-unset value))))

(defun g-object-call-set-property (object-ptr property-name new-value
                                   &optional property-type)
  (unless property-type
    (setf property-type
          (g-object-type-property-type (g-type-from-object object-ptr) property-name :assert-writable t)))
  (with-foreign-object (value 'g-value)
    (set-g-value value new-value property-type :zero-g-value t)
    (unwind-protect
         (g-object-set-property object-ptr property-name value)
      (g-value-unset value))))

(defun g-object-call-constructor (object-type args-names args-values
                                  &optional args-types)
  (unless args-types
    (setf args-types
          (mapcar (lambda (name)
                    (g-object-type-property-type object-type name))
                  args-names)))
  (let ((args-count (length args-names)))
    (with-foreign-object (parameters 'g-parameter args-count)
      (loop
         for i from 0 below args-count
         for arg-name in args-names
         for arg-value in args-values
         for arg-type in args-types
         for arg-g-type = (if arg-type arg-type (g-object-type-property-type object-type arg-name))
         for parameter = (mem-aref parameters 'g-parameter i)
         do (setf (foreign-slot-value parameter 'g-parameter :name) arg-name)
         do (set-g-value (foreign-slot-value parameter 'g-parameter :value) arg-value arg-g-type :zero-g-value t))
      (unwind-protect
           (g-object-newv object-type args-count parameters)
        (loop
           for i from 0 below args-count
           for parameter = (mem-aref parameters 'g-parameter i)
           do (foreign-string-free (mem-ref (foreign-slot-pointer parameter 'g-parameter :name) :pointer))
           do (g-value-unset (foreign-slot-pointer parameter 'g-parameter :value)))))))

