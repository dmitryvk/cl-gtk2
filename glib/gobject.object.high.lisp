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
    :initform nil))
  (:documentation
   "Base class for GObject classes hierarchy."))

(defvar *foreign-gobjects* (make-weak-hash-table :test 'equal :weakness :value))
(defvar *foreign-gobjects-ref-count* (make-hash-table :test 'equal))
(defvar *lisp-objects-pointers* (make-hash-table :test 'equal))
(defvar *current-creating-object* nil)

(defun ref-count (pointer)
  (foreign-slot-value (if (pointerp pointer) pointer (pointer pointer)) 'g-object-struct :ref-count))

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
        (g-idle-add (callback g-idle-gc-hook) (null-pointer))))))

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
    (debugf "GObject ~A is already disposed, signalling error~%" pointer)
    (error "GObject ~A is already disposed" pointer))
  (debugf "g_object_unref(~A) (of type ~A, lisp-value ~A) (lisp ref-count ~A, gobject ref-count ~A)~%"
          pointer
          (g-type-name (g-type-from-object pointer))
          (gethash (pointer-address pointer) *foreign-gobjects*)
          (gethash (pointer-address pointer) *foreign-gobjects-ref-count*)
          (ref-count pointer))
  (let ((object (gethash (pointer-address pointer) *foreign-gobjects*)))
    (when object
      (setf (pointer object) nil)
      (cancel-finalization object)))
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
(defun registered-object-type-by-name (name)
  (gethash name *registered-object-types*))
(defun get-g-object-lisp-type (g-type)
  (setf g-type (ensure-g-type g-type))
  (loop
     while (not (zerop g-type))
     for lisp-type = (gethash (g-type-name g-type) *registered-object-types*)
     when lisp-type do (return lisp-type)
     do (setf g-type (ensure-g-type (g-type-parent g-type)))))

(defun make-g-object-from-pointer (pointer)
  (let* ((g-type (g-type-from-instance pointer))
         (lisp-type (get-g-object-lisp-type g-type)))
    (unless lisp-type
      (error "Type ~A is not registered with REGISTER-OBJECT-TYPE"
             (g-type-name g-type)))
    (g-object-ref pointer)
    (make-instance lisp-type :pointer pointer)))

(define-foreign-type foreign-g-object-type ()
  ((sub-type :reader sub-type :initarg :sub-type :initform 'g-object))
  (:actual-type :pointer))

(define-parse-method g-object (&optional (sub-type 'g-object))
  (make-instance 'foreign-g-object-type :sub-type sub-type))

(defmethod translate-to-foreign (object (type foreign-g-object-type))
  (cond
    ((null object)
     (null-pointer))
    ((pointerp object) object)
    ((null (pointer object))
     (error "Object ~A has been disposed" object))
    ((typep object 'g-object)
     (assert (typep object (sub-type type))
             nil
             "Object ~A is not a subtype of ~A" object (sub-type type))
     (pointer object))
    (t (error "Object ~A is not translatable as GObject*" object))))

(defun get-g-object-for-pointer (pointer)
  (unless (null-pointer-p pointer)
    (let ((object (gethash (pointer-address pointer) *foreign-gobjects*)))
      (if object
          (prog1 object
            (incf (gethash (pointer-address pointer) *foreign-gobjects-ref-count*))
            (debugf "increfering object ~A~%" pointer))
          (make-g-object-from-pointer pointer)))))

(defmethod translate-from-foreign (pointer (type foreign-g-object-type))
  (get-g-object-for-pointer pointer))

(register-object-type "GObject" 'g-object)

(defun ensure-g-type (type)
  "Returns the GType value for a given type. If type is an integer, it is returned. If type is a string, GType corresponding to this type name is looked up and returned.
@arg[type]{a string or and integer}
@return{integer equal to GType of @code{type}}"
  (etypecase type
    (integer type)
    (string (or (g-type-from-name type)
                (error "Type ~A is invalid" type)))))

(defun ensure-object-pointer (object)
  (if (pointerp object)
      object
      (etypecase object
        (g-object (pointer object)))))

(defun parse-g-value-object (gvalue)
  (get-g-object-for-pointer (g-value-get-object gvalue)))

(defun set-gvalue-object (gvalue value)
  (g-value-set-object gvalue (if value (pointer value) (null-pointer))))

(defmethod parse-g-value-for-type (gvalue-ptr (type-numeric (eql +g-type-object+)) parse-kind)
  (declare (ignore parse-kind))
  (parse-g-value-object gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr (type-numeric (eql +g-type-interface+)) parse-kind)
  (declare (ignore parse-kind))
  (parse-g-value-object gvalue-ptr))

(defmethod set-gvalue-for-type (gvalue-ptr (type-numeric (eql +g-type-object+)) value)
  (set-gvalue-object gvalue-ptr value))

(defmethod set-gvalue-for-type (gvalue-ptr (type-numeric (eql +g-type-interface+)) value)
  (set-gvalue-object gvalue-ptr value))

(defun g-signal-connect (object signal handler &key after)
  "Deprecated alias for @fun{connect-signal}"
  (connect-signal object signal handler :after after))

(defun connect-signal (object signal handler &key after)
  "Connects the function to a signal for a particular object.
If @code{after} is true, then the function will be called after the default handler of the signal.

@arg[object]{an instance of @class{gobject}}
@arg[signal]{a string; names the signal}
@arg[handler]{a function; handles the signal. Number (and type) of arguments and return value type depends on the signal}
@arg[after]{a boolean}"
  (g-signal-connect-closure (ensure-object-pointer object)
                            signal
                            (create-g-closure handler)
                            after))

(defun emit-signal (object signal-name &rest args)
  "Emits the signal.
@arg[object]{an instance of @class{g-object}. Signal is emitted on this object}
@arg[signal-name]{a string specifying the signal}
@arg[args]{arguments for the signal}
@return{none}"
  (let* ((object-type (g-type-from-object (pointer object)))
         (signal-info (parse-signal-name object-type signal-name)))
    (unless signal-info
      (error "Signal ~A not found on object ~A" signal-name object))
    (let ((params-count (length (signal-info-param-types signal-info))))
      (with-foreign-object (params 'g-value (1+ params-count))
        (set-g-value (mem-aref params 'g-value 0) object object-type :zero-g-value t)
        (iter (for i from 0 below params-count)
              (for arg in args)
              (for type in (signal-info-param-types signal-info))
              (set-g-value (mem-aref params 'g-value (1+ i)) arg type :zero-g-value t))
        (prog1
            (if (g-type= (signal-info-return-type signal-info) +g-type-void+)
                (g-signal-emitv params (signal-info-id signal-info) signal-name (null-pointer))
                (with-foreign-object (return-value 'g-value)
                  (g-value-zero return-value)
                  (g-value-init return-value (signal-info-return-type signal-info))
                  (prog1 (parse-g-value return-value)
                    (g-value-unset return-value))))
          (iter (for i from 0 below (1+ params-count))
                (g-value-unset (mem-aref params 'g-value i))))))))
