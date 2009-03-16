(in-package :gobject)

(defgeneric release (object))

(defmethod release ((object null)))

(defun release* (&rest objects)
  (declare (dynamic-extent objects))
  (loop
     for object in objects
     do (release object)))

(defmacro using ((var &optional (expr var)) &body body)
  `(let ((,var ,expr))
     (unwind-protect
          (progn ,@body)
       (release ,var))))

(defun using-expand (bindings body)
  (if bindings
      (destructuring-bind (var &optional (expr var)) (ensure-list (first bindings))
       `(let ((,var ,expr))
          (unwind-protect
               ,(using-expand (rest bindings) body)
            (release ,var))))
      `(progn ,@body)))

(defmacro using* ((&rest bindings) &body body)
  (using-expand bindings body))

(defvar *registered-stable-pointers* (make-array 0 :adjustable t :fill-pointer t))

(defun allocate-stable-pointer (thing)
  (let ((id (find-fresh-id)))
    (setf (aref *registered-stable-pointers* id) thing)
    (make-pointer id)))

(defun free-stable-pointer (stable-pointer)
  (setf (aref *registered-stable-pointers* (pointer-address stable-pointer)) nil))

(defun get-stable-pointer-value (stable-pointer)
  (when (<= 0 (pointer-address stable-pointer) (length *registered-stable-pointers*))
    (aref *registered-stable-pointers* (pointer-address stable-pointer))))

(defun find-fresh-id ()
  (or (position nil *registered-stable-pointers*)
      (progn (vector-push-extend nil *registered-stable-pointers*)
             (1- (length *registered-stable-pointers*)))))

(defmacro with-stable-pointer ((ptr expr) &body body)
  `(let ((,ptr (allocate-stable-pointer ,expr)))
     (unwind-protect
          (progn ,@body)
       (free-stable-pointer ,ptr))))