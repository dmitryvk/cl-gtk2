(in-package :gobject)

(defclass g-object ()
  ((pointer
    :type cffi:foreign-pointer
    :initarg :pointer
    :accessor pointer
    :initform nil)
   (has-reference
    :type boolean
    :accessor g-object-has-reference
    :initform nil)))

(defvar *foreign-gobjects* (make-weak-hash-table :test 'equal :weakness :value))
(defvar *foreign-gobjects-ref-count* (make-hash-table :test 'equal))
(defvar *lisp-objects-pointers* (make-hash-table :test 'equal))
(defvar *current-creating-object* nil)

(defcstruct g-object-struct
  (type-instance g-type-instance)
  (ref-count :uint)
  (qdata :pointer))

(defun ref-count (pointer)
  (foreign-slot-value (if (pointerp pointer) pointer (pointer pointer)) 'g-object-struct 'ref-count))

(defmethod initialize-instance :around ((obj g-object) &key)
  (let ((*current-creating-object* obj))
    (call-next-method)))

(defmethod initialize-instance :after ((obj g-object) &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer)
    (error "Pointer slot is not initialized for ~A" obj))
  (let* ((pointer (pointer obj))
         (s (format nil "~A" obj)))
    (finalize obj
              (lambda ()
                (handler-case
                    (g-object-dispose-carefully pointer)
                  (error (e) (format t "Error in finalizer for ~A: ~A~%" s e))))))
  (register-g-object obj)
  (activate-gc-hooks))

(defvar *gobject-gc-hooks-lock* (make-recursive-lock "gobject-gc-hooks-lock"))
(defvar *gobject-gc-hooks* nil);;pointers to objects to be freed

(defun activate-gc-hooks ()
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (when *gobject-gc-hooks*
      (debugf "activating gc hooks for objects: ~A~%" *gobject-gc-hooks*)
      (loop
         for pointer in *gobject-gc-hooks*
         do (g-object-unref pointer))
      (setf *gobject-gc-hooks* nil))))

(defcallback g-idle-gc-hook :boolean ((data :pointer))
  (declare (ignore data))
  (activate-gc-hooks)
  nil)

(defun register-gobject-for-gc (pointer)
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (let ((locks-were-present (not (null *gobject-gc-hooks*))))
      (push pointer *gobject-gc-hooks*)
      (unless locks-were-present
        (debugf "adding idle-gc-hook to main loop~%")
        (glib::g-idle-add (callback g-idle-gc-hook) (null-pointer))))))

(defun g-object-dispose-carefully (pointer)
  (handler-case
      (register-gobject-for-gc pointer)
    (error (e) (format t "Error in dispose: ~A~%" e))))

(defcallback weak-notify-print :void ((data :pointer) (object-pointer :pointer))
  (declare (ignore data))
  (debugf "g-object has finalized ~A ~A~%" (g-type-name (g-type-from-object object-pointer)) object-pointer))

(defun erase-pointer (data object-pointer)
  (declare (ignore data))
  (remhash (pointer-address object-pointer) *lisp-objects-pointers*))

(defcallback weak-notify-erase-pointer :void ((data :pointer) (object-pointer :pointer))
  (erase-pointer data object-pointer))

(defun should-ref-sink-at-creation (object)
;;If object was not created from lisp-side, we should ref it
;;If an object is regular g-object, we should not ref-sink it
;;If an object is GInitiallyUnowned, then it is created with a floating reference, we should ref-sink it
;;A special case is GtkWindow: we should ref-sink it anyway
  (if (g-object-has-reference object)
      (let ((object-type (g-type-from-object (pointer object)))
            (initially-unowned-type (g-type-from-name "GInitiallyUnowned")))
        (g-type-is-a object-type initially-unowned-type))
      t))

(defun register-g-object (obj)
  (debugf "registered GObject ~A with gobject ref-count ~A ~A~%" (pointer obj) (ref-count obj) (if (g-object-is-floating (pointer obj)) "(floating)" ""))
  (when (should-ref-sink-at-creation obj)
    (debugf "g_object_ref_sink(~A)~%" (pointer obj))
    (g-object-ref-sink (pointer obj)))
  (g-object-weak-ref (pointer obj) (callback weak-notify-print) (null-pointer))
  (g-object-weak-ref (pointer obj) (callback weak-notify-erase-pointer) (null-pointer))
  (setf (g-object-has-reference obj) t)
  (setf (gethash (pointer-address (pointer obj)) *foreign-gobjects*)
        obj)
  (setf (gethash (pointer-address (pointer obj)) *foreign-gobjects-ref-count*) 1))

(defun g-object-dispose (pointer)
  (unless (gethash (pointer-address pointer) *foreign-gobjects-ref-count*)
    (format t "GObject ~A is already disposed, signalling error~%" pointer)
    (error "GObject ~A is already disposed" pointer))
  (debugf "g_object_unref(~A) (of type ~A, lisp-value ~A) (lisp ref-count ~A, gobject ref-count ~A)~%"
          pointer
          (g-type-name (g-type-from-object pointer))
          (gethash (pointer-address pointer) *foreign-gobjects*)
          (gethash (pointer-address pointer) *foreign-gobjects-ref-count*)
          (ref-count pointer))
  (awhen (gethash (pointer-address pointer) *foreign-gobjects*)
    (setf (pointer it) nil)
    (cancel-finalization it))
  (remhash (pointer-address pointer) *foreign-gobjects*)
  (remhash (pointer-address pointer) *foreign-gobjects-ref-count*)
  (g-object-unref pointer))

(defmethod release ((object g-object))
  (debugf "Releasing object ~A (type ~A, lisp-value ~A)~%" (pointer object) (when (pointer object) (g-type-name (g-type-from-object (pointer object)))) object)
  (unless (and (pointer object) (gethash (pointer-address (pointer object)) *foreign-gobjects-ref-count*))
    (error "Object ~A already disposed of from lisp side" object))
  (decf (gethash (pointer-address (pointer object)) *foreign-gobjects-ref-count*))
  (when (zerop (gethash (pointer-address (pointer object)) *foreign-gobjects-ref-count*))
    (g-object-dispose (pointer object)))
  (activate-gc-hooks))

(defvar *registered-object-types* (make-hash-table :test 'equal))
(defun register-object-type (name type)
  (setf (gethash name *registered-object-types*) type))
(defun get-g-object-lisp-type (g-type)
  (loop
     while (not (zerop g-type))
     for lisp-type = (gethash (g-type-name g-type) *registered-object-types*)
     when lisp-type do (return lisp-type)
     do (setf g-type (g-type-parent g-type))))

(defun make-g-object-from-pointer (pointer)
  (let* ((g-type (g-type-from-instance pointer))
         (lisp-type (get-g-object-lisp-type g-type)))
    (unless lisp-type
      (error "Type ~A is not registered with REGISTER-OBJECT-TYPE"
             (g-type-name g-type)))
    (make-instance lisp-type :pointer pointer)))

(define-foreign-type foreign-g-object-type ()
  ((sub-type :reader sub-type :initarg :sub-type :initform 'g-object))
  (:actual-type :pointer))

(define-parse-method g-object (&optional (sub-type 'g-object))
  (make-instance 'foreign-g-object-type :sub-type sub-type))

(defmethod translate-to-foreign (object (type foreign-g-object-type))
  (cond
    ((null (pointer object))
     (error "Object ~A has been disposed" object))
    ((typep object 'g-object)
     (assert (typep object (sub-type type))
             nil
             "Object ~A is not a subtype of ~A" object (sub-type type))
     (pointer object))
    ((pointerp object) object)
    (t (error "Object ~A is not translatable as GObject*" object))))

(defun get-g-object-for-pointer (pointer)
  (unless (null-pointer-p pointer)
    (aif (gethash (pointer-address pointer) *foreign-gobjects*)
         (prog1 it
           (incf (gethash (pointer-address pointer) *foreign-gobjects-ref-count*))
           (debugf "increfering object ~A~%" pointer))
         (make-g-object-from-pointer pointer))))

(defmethod translate-from-foreign (pointer (type foreign-g-object-type))
  (get-g-object-for-pointer pointer))

(register-object-type "GObject" 'g-object)

(defun ensure-g-type (type)
  (etypecase type
    (integer type)
    (string (or (g-type-from-name type)
                (error "Type ~A is invalid" type)))))

(defun ensure-object-pointer (object)
  (if (pointerp object)
      object
      (etypecase object
        (g-object (pointer object)))))

(defun g-object-type-property-type (object-type property-name
                                    &key assert-readable assert-writable)
  (let* ((object-class (g-type-class-ref object-type))
         (param-spec (g-object-class-find-property object-class property-name)))
    (unwind-protect
         (progn
           (when (null-pointer-p param-spec)
             (error "Property ~A on type ~A is not found"
                    property-name
                    (g-type-name object-type)))
           (when (and assert-readable
                      (not (member :readable
                                   (foreign-slot-value param-spec
                                                       'g-param-spec
                                                       'flags))))
             (error "Property ~A on type ~A is not readable"
                    property-name
                    (g-type-name object-type)))
           (when (and assert-writable
                      (not (member :writable
                                   (foreign-slot-value param-spec
                                                       'g-param-spec
                                                       'flags))))
             (error "Property ~A on type ~A is not writable"
                    property-name
                    (g-type-name object-type)))
           (foreign-slot-value param-spec 'g-param-spec 'value-type))
      (g-type-class-unref object-class))))

(defun g-object-property-type (object property-name
                               &key assert-readable assert-writable)
  (g-object-type-property-type (g-type-from-object (ensure-object-pointer object))
                               property-name
                               :assert-readable assert-readable
                               :assert-writable assert-writable))

(defun g-object-call-constructor (object-type args-names args-values
                                  &optional args-types)
  (setf object-type (ensure-g-type object-type))
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
         for arg-g-type = (ensure-g-type arg-type)
         for parameter = (mem-aref parameters 'g-parameter i)
         do (setf (foreign-slot-value parameter 'g-parameter 'name) arg-name)
         do (set-g-value (foreign-slot-value parameter 'g-parameter 'value)
                         arg-value arg-g-type
                         :zero-g-value t))
      (unwind-protect
           (g-object-newv object-type args-count parameters)
        (loop
           for i from 0 below args-count
           for parameter = (mem-aref parameters 'g-parameter i)
           do (foreign-free
               (mem-ref (foreign-slot-pointer parameter 'g-parameter 'name)
                        :pointer))
           do (g-value-unset
               (foreign-slot-pointer parameter 'g-parameter 'value)))))))

(defun g-object-call-get-property (object property-name &optional property-type)
  (unless property-type
    (setf property-type
          (g-object-property-type object property-name :assert-readable t)))
  (setf property-type (ensure-g-type property-type))
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (g-value-init value property-type)
    (g-object-get-property (ensure-object-pointer object)
                           property-name value)
    (unwind-protect
         (parse-gvalue value)
      (g-value-unset value))))

(defun g-object-call-set-property (object property-name new-value
                                   &optional property-type)
  (unless property-type
    (setf property-type
          (g-object-property-type object property-name :assert-writable t)))
  (setf property-type (ensure-g-type property-type))
  (with-foreign-object (value 'g-value)
    (set-g-value value new-value property-type :zero-g-value t)
    (unwind-protect
         (g-object-set-property (ensure-object-pointer object)
                                property-name value)
      (g-value-unset value))))