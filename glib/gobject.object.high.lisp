(in-package :gobject)

(defclass g-object ()
  ((pointer
    :type (or null cffi:foreign-pointer)
    :initarg :pointer
    :accessor pointer
    :initform nil)
   (has-reference
    :type boolean
    :accessor g-object-has-reference
    :initform nil)
   (signal-handlers
    :type (array t *)
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :reader g-object-signal-handlers))
  (:documentation
   "Base class for GObject classes hierarchy."))

(defvar *foreign-gobjects-weak* (make-weak-hash-table :test 'equal :weakness :value))
(defvar *foreign-gobjects-strong* (make-hash-table :test 'equal))
(defvar *current-creating-object* nil)
(defvar *current-object-from-pointer* nil)
(defvar *currently-making-object-p* nil)

(at-finalize ()
  (clrhash *foreign-gobjects-weak*)
  (clrhash *foreign-gobjects-strong*)
  (setf *current-creating-object* nil
        *current-object-from-pointer* nil
        *currently-making-object-p* nil))

(defun ref-count (pointer)
  (foreign-slot-value (if (pointerp pointer) pointer (pointer pointer)) 'g-object-struct :ref-count))

(defmethod release ((obj g-object))
  (cancel-finalization obj)
  (let ((p (pointer obj)))
    (setf (pointer obj) nil)
    (g-object-dispose-carefully p)))

(defmethod initialize-instance :around ((obj g-object) &key)
  (when *currently-making-object-p*
    (setf *currently-making-object-p* t))
  (let ((*current-creating-object* obj))
    (log-for :subclass "initialize-instance :around; *current-creating-object* = ~A~%" obj)
    (call-next-method)))

(defmethod initialize-instance :after ((obj g-object) &key &allow-other-keys)
  (unless (slot-boundp obj 'pointer)
    (error "Pointer slot is not initialized for ~A" obj))
  (let* ((pointer (pointer obj))
         (s (format nil "~A" obj)))
    (finalize obj
              (lambda ()
                (log-for :gc "~A ~A is queued for GC (having ~A refs)~%"
                         (g-type-from-object pointer) pointer (ref-count pointer))
                (handler-case
                    (g-object-dispose-carefully pointer)
                  (error (e)
                    (log-for :gc "Error in finalizer for ~A: ~A~%" s e)
                    (format t "Error in finalizer for ~A: ~A~%" s e))))))
  (register-g-object obj)
  (activate-gc-hooks))

(defvar *gobject-gc-hooks-lock* (make-recursive-lock "gobject-gc-hooks-lock"))
(defvar *gobject-gc-hooks* nil);;pointers to objects to be freed

(defun activate-gc-hooks ()
  (with-recursive-lock-held (*gobject-gc-hooks-lock*)
    (when *gobject-gc-hooks*
      (log-for :gc "activating gc hooks for objects: ~A~%" *gobject-gc-hooks*)
      (loop
         for pointer in *gobject-gc-hooks*
         do (g-object-remove-toggle-ref pointer (callback gobject-toggle-ref-toggled) (null-pointer)))
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
        (log-for :gc "adding idle-gc-hook to main loop~%")
        (g-idle-add (callback g-idle-gc-hook) (null-pointer))))))

(defun g-object-dispose-carefully (pointer)
  (handler-case
      (register-gobject-for-gc pointer)
    (error (e)
      (log-for :gc  "Error in dispose: ~A~%" e)
      (format t "Error in dispose: ~A~%" e))))

(defun should-ref-sink-at-creation (object)
;;If object was not created from lisp-side, we should ref it
;;If an object is regular g-object, we should not ref-sink it
;;If an object is GInitiallyUnowned, then it is created with a floating reference, we should ref-sink it
;;A special case is GtkWindow: we should ref-sink it anyway
  (let ((r (cond
             ((equal *current-object-from-pointer* (pointer object))
              (log-for :gc "*cur-obj-from-ptr* ")
              t)  ;; not new objects should be ref_sunk
             ((eq object *current-creating-object*) ;; g_object_new returns objects with ref = 1, we should save _this_ ref
              (typep object 'g-initially-unowned)) ;; but GInitiallyUnowned objects should be ref_sunk
             (t t))))
    (log-for :gc "(should-ref-sink-at-creation ~A) => ~A~%" object r)
    r))

(defcallback gobject-toggle-ref-toggled :void
    ((data :pointer) (pointer :pointer) (is-last-ref :boolean))
  (declare (ignore data))
  (log-for :gc "~A is now ~A with ~A refs~%" pointer (if is-last-ref "weak pointer" "strong pointer") (ref-count pointer))
  (log-for :gc "obj: ~A~%" (or (gethash (pointer-address pointer) *foreign-gobjects-strong*)
                            (gethash (pointer-address pointer) *foreign-gobjects-weak*)))
  (if is-last-ref
      (let ((obj (gethash (pointer-address pointer) *foreign-gobjects-strong*)))
        (if obj
            (progn
              (remhash (pointer-address pointer) *foreign-gobjects-strong*)
              (setf (gethash (pointer-address pointer) *foreign-gobjects-weak*) obj))
            (progn
              (log-for :gc "GObject at ~A has no lisp-side (strong) reference" pointer)
              (warn "GObject at ~A has no lisp-side (strong) reference" pointer))))
      (let ((obj (gethash (pointer-address pointer) *foreign-gobjects-weak*)))
        (unless obj
          (log-for :gc "GObject at ~A has no lisp-side (weak) reference" pointer)
          (warn "GObject at ~A has no lisp-side (weak) reference" pointer))
        (remhash (pointer-address pointer) *foreign-gobjects-weak*)
        (setf (gethash (pointer-address pointer) *foreign-gobjects-strong*) obj))))

(defcallback gobject-weak-ref-finalized :void
    ((data :pointer) (pointer :pointer))
  (declare (ignore data))
  (log-for :gc "~A is weak-ref-finalized with ~A refs~%" pointer (ref-count pointer))
  (remhash (pointer-address pointer) *foreign-gobjects-weak*)
  (when (gethash (pointer-address pointer) *foreign-gobjects-strong*)
    (warn "GObject at ~A was weak-ref-finalized while still holding lisp-side strong reference to it" pointer)
    (log-for :gc "GObject at ~A was weak-ref-finalized while still holding lisp-side strong reference to it" pointer))
  (remhash (pointer-address pointer) *foreign-gobjects-strong*))

(defun register-g-object (obj)
  (log-for :gc "registered GObject ~A (~A) with initial ref-count ~A ~A~%"
           (pointer obj) obj
           (ref-count obj) (if (g-object-is-floating (pointer obj)) "(floating)" ""))
  (when (should-ref-sink-at-creation obj)
    (log-for :gc "g_object_ref_sink(~A)~%" (pointer obj))
    (g-object-ref-sink (pointer obj)))
  (setf (g-object-has-reference obj) t)
  (setf (gethash (pointer-address (pointer obj)) *foreign-gobjects-strong*) obj)
  (g-object-add-toggle-ref (pointer obj) (callback gobject-toggle-ref-toggled) (null-pointer))
  (g-object-unref (pointer obj)))

(defvar *registered-object-types* (make-hash-table :test 'equal))
(defun register-object-type (name type)
  (setf (gethash name *registered-object-types*) type))
(defun registered-object-type-by-name (name)
  (gethash name *registered-object-types*))
(defun get-g-object-lisp-type (g-type)
  (setf g-type (gtype g-type))
  (iter (while (not (null g-type)))
        (for lisp-type = (gethash (gtype-name g-type) *registered-object-types*))
        (when lisp-type
          (return lisp-type))
        (setf g-type (g-type-parent g-type))))

(defun make-g-object-from-pointer (pointer)
  (let* ((g-type (g-type-from-instance pointer))
         (lisp-type (get-g-object-lisp-type g-type)))
    (unless lisp-type
      (error "Type ~A is not registered with REGISTER-OBJECT-TYPE"
             (gtype-name g-type)))
    (let ((*current-object-from-pointer* pointer))
      (make-instance lisp-type :pointer pointer))))

(define-foreign-type foreign-g-object-type ()
  ((sub-type :reader sub-type :initarg :sub-type :initform 'g-object)
   (already-referenced :reader foreign-g-object-type-already-referenced :initarg :already-referenced :initform nil))
  (:actual-type :pointer))

(define-parse-method g-object (&rest args)
  (let* ((sub-type (first (remove-if #'keywordp args)))
         (flags (remove-if-not #'keywordp args))
         (already-referenced (not (null (find :already-referenced flags)))))
    (make-instance 'foreign-g-object-type :sub-type sub-type :already-referenced already-referenced)))

(defmethod translate-to-foreign (object (type foreign-g-object-type))
  (cond
    ((null object)
     (null-pointer))
    ((pointerp object) object)
    ((null (pointer object))
     (error "Object ~A has been disposed" object))
    ((typep object 'g-object)
     (when (sub-type type)
       (assert (typep object (sub-type type))
               nil
               "Object ~A is not a subtype of ~A" object (sub-type type)))
     (pointer object))
    (t (error "Object ~A is not translatable as GObject*" object))))

(defun get-g-object-for-pointer (pointer)
  (unless (null-pointer-p pointer)
    (or (gethash (pointer-address pointer) *foreign-gobjects-strong*)
        (gethash (pointer-address pointer) *foreign-gobjects-weak*)
        (progn (log-for :gc "Now creating object for ~A~%" pointer)
               (make-g-object-from-pointer pointer)))))

(defmethod translate-from-foreign (pointer (type foreign-g-object-type))
  (let ((object (get-g-object-for-pointer pointer)))
    (when (and object (foreign-g-object-type-already-referenced type))
      (g-object-unref (pointer object)))
    object))

(register-object-type "GObject" 'g-object)

(defun ensure-object-pointer (object)
  (if (pointerp object)
      object
      (etypecase object
        (g-object (pointer object)))))

(defun parse-g-value-object (gvalue)
  (get-g-object-for-pointer (g-value-get-object gvalue)))

(defun set-gvalue-object (gvalue value)
  (g-value-set-object gvalue (if value (pointer value) (null-pointer))))

(defmethod parse-g-value-for-type (gvalue-ptr (type (eql (gtype +g-type-object+))) parse-kind)
  (declare (ignore parse-kind))
  (parse-g-value-object gvalue-ptr))

(defmethod parse-g-value-for-type (gvalue-ptr (type (eql (gtype +g-type-interface+))) parse-kind)
  (declare (ignore parse-kind))
  (parse-g-value-object gvalue-ptr))

(defmethod set-gvalue-for-type (gvalue-ptr (type (eql (gtype +g-type-object+))) value)
  (set-gvalue-object gvalue-ptr value))

(defmethod set-gvalue-for-type (gvalue-ptr (type (eql (gtype +g-type-interface+))) value)
  (set-gvalue-object gvalue-ptr value))
