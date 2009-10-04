(in-package :gobject)

(defun g-value-zero (g-value)
  "Initializes the GValue in \"unset\" state.

@arg[g-value]{a C pointer to the GValue structure}"
  (loop
     for i from 0 below (foreign-type-size 'g-value)
     do (setf (mem-ref g-value :uchar i) 0)))

(defun g-value-type (gvalue)
  (foreign-slot-value gvalue 'g-value :type))

(defmacro ev-case (keyform &body clauses)
  "Macro that is an analogue of CASE except that it evaluates keyforms"
  (let ((value (gensym)))
    `(let ((,value ,keyform))
       (cond
         ,@(loop
              for (key . forms) in clauses
              collect
                (if (eq key t)
                    `(t ,@forms)
                    `((equalp ,key ,value) ,@forms)))))))

(defgeneric parse-g-value-for-type (gvalue-ptr type-numeric parse-kind))

(defmethod parse-g-value-for-type (gvalue-ptr type-numeric parse-kind)
  (if (g-type= type-numeric (g-type-fundamental type-numeric))
      (call-next-method)
      (parse-g-value-for-type gvalue-ptr (g-type-numeric (g-type-fundamental type-numeric)) parse-kind)))

(defun parse-g-value (gvalue &key (parse-kind :get-property))
  "Parses the GValue structure and returns the corresponding Lisp object.

@arg[value]{a C pointer to the GValue structure}
@return{value contained in the GValue structure. Type of value depends on GValue type}"
  (let* ((type (g-type-numeric (g-value-type gvalue)))
         (fundamental-type (g-type-numeric (g-type-fundamental type))))
    (ev-case fundamental-type
      (+g-type-invalid+ (error "GValue is of invalid type (~A)" (g-type-name type)))
      (+g-type-void+ nil)
      (+g-type-char+ (g-value-get-char gvalue))
      (+g-type-uchar+ (g-value-get-uchar gvalue))
      (+g-type-boolean+ (g-value-get-boolean gvalue))
      (+g-type-int+ (g-value-get-int gvalue))
      (+g-type-uint+ (g-value-get-uint gvalue))
      (+g-type-long+ (g-value-get-long gvalue))
      (+g-type-ulong+ (g-value-get-ulong gvalue))
      (+g-type-int64+ (g-value-get-int64 gvalue))
      (+g-type-uint64+ (g-value-get-uint64 gvalue))
      (+g-type-enum+ (parse-g-value-enum gvalue))
      (+g-type-flags+ (parse-g-value-flags gvalue))
      (+g-type-float+ (g-value-get-float gvalue))
      (+g-type-double+ (g-value-get-double gvalue))
      (+g-type-string+ (g-value-get-string gvalue))
      (t (parse-g-value-for-type gvalue type parse-kind)))))

(defmethod parse-g-value-for-type (gvalue-ptr (type-numeric (eql +g-type-pointer+)) parse-kind)
  (declare (ignore parse-kind))
  (g-value-get-pointer gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr (type-numeric (eql +g-type-param+)) parse-kind)
  (declare (ignore parse-kind))
  (parse-g-param-spec (g-value-get-param gvalue-ptr)))

(defgeneric set-gvalue-for-type (gvalue-ptr type-numeric value))

(defmethod set-gvalue-for-type (gvalue-ptr type-numeric value)
  (if (g-type= type-numeric (g-type-fundamental type-numeric))
      (call-next-method)
      (set-gvalue-for-type gvalue-ptr (g-type-numeric (g-type-fundamental type-numeric)) value)))

(defun set-g-value (gvalue value type &key zero-g-value unset-g-value (g-value-init t))
  "Assigns the GValue structure @code{gvalue} the value @code{value} of GType @code{type}.

@arg[gvalue]{a C pointer to the GValue structure}
@arg[value]{a Lisp object that is to be assigned}
@arg[type]{a GType that is to be assigned}
@arg[zero-g-value]{a boolean specifying whether GValue should be zero-initialized before assigning. See @fun{g-value-zero}}
@arg[unset-g-value]{a boolean specifying whether GValue should be \"unset\" before assigning. See @fun{g-value-unset}. The \"true\" value should not be passed to both @code{zero-g-value} and @code{unset-g-value} arguments}
@arg[g-value-init]{a boolean specifying where GValue should be initialized}"
  (setf type (g-type-numeric type))
  (cond
    (zero-g-value (g-value-zero gvalue))
    (unset-g-value (g-value-unset gvalue)))
  (when g-value-init (g-value-init gvalue type))
  (let ((fundamental-type (ensure-g-type (g-type-fundamental type))))
    (ev-case fundamental-type
      (+g-type-invalid+ (error "Invalid type (~A)" type))
      (+g-type-void+ nil)
      (+g-type-char+ (g-value-set-char gvalue value))
      (+g-type-uchar+ (g-value-set-uchar gvalue value))
      (+g-type-boolean+ (g-value-set-boolean gvalue value))
      (+g-type-int+ (g-value-set-int gvalue value))
      (+g-type-uint+ (g-value-set-uint gvalue value))
      (+g-type-long+ (g-value-set-long gvalue value))
      (+g-type-ulong+ (g-value-set-ulong gvalue value))
      (+g-type-int64+ (g-value-set-int64 gvalue value))
      (+g-type-uint64+ (g-value-set-uint64 gvalue value))
      (+g-type-enum+ (set-gvalue-enum gvalue value))
      (+g-type-flags+ (set-gvalue-flags gvalue value))
      (+g-type-float+ (unless (realp value) (error "~A is not a real number" value)) (g-value-set-float gvalue (coerce value 'single-float)))
      (+g-type-double+ (unless (realp value) (error "~A is not a real number" value)) (g-value-set-double gvalue (coerce value 'double-float)))
      (+g-type-string+ (g-value-set-string gvalue value))
      (t (set-gvalue-for-type gvalue type value)))))

(defmethod set-gvalue-for-type (gvalue-ptr (type-numeric (eql +g-type-pointer+)) value)
  (g-value-set-pointer gvalue-ptr value))

(defmethod set-gvalue-for-type (gvalue-ptr (type-numeric (eql +g-type-param+)) value)
  (declare (ignore gvalue-ptr value))
  (error "Setting of GParam is not implemented"))

;;Enums

(defvar *registered-enum-types* (make-hash-table :test 'equal))
(defun register-enum-type (name type)
  (setf (gethash name *registered-enum-types*) type))
(defun registered-enum-type (name)
  (gethash name *registered-enum-types*))

(defun parse-g-value-enum (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (g-type-name g-type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-enum gvalue) enum-type)))

(defun set-gvalue-enum (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (g-type-name g-type))
         (enum-type (registered-enum-type type-name)))
    (unless enum-type
      (error "Enum ~A is not registered" type-name))
    (g-value-set-enum gvalue (convert-to-foreign value enum-type))))


;;Flags

(defvar *registered-flags-types* (make-hash-table :test 'equal))
(defun register-flags-type (name type)
  (setf (gethash name *registered-flags-types*) type))
(defun registered-flags-type (name)
  (gethash name *registered-flags-types*))

(defun parse-g-value-flags (gvalue)
  (let* ((g-type (g-value-type gvalue))
         (type-name (g-type-name g-type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (convert-from-foreign (g-value-get-flags gvalue) flags-type)))

(defun set-gvalue-flags (gvalue value)
  (let* ((g-type (g-value-type gvalue))
         (type-name (g-type-name g-type))
         (flags-type (registered-flags-type type-name)))
    (unless flags-type
      (error "Flags ~A is not registered" type-name))
    (g-value-set-flags gvalue (convert-to-foreign value flags-type))))
