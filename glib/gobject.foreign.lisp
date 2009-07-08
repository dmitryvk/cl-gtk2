(in-package :gobject)

(defgeneric release (object)
  (:documentation "Manually frees the Lisp reference to the @code{object}. Probably should not be called.

@arg[object]{an instance of @class{g-object}}"))

(defmethod release ((object null)))

(defun release* (&rest objects)
  "Calls @fun{release} on all objects in @code{objects}

@arg[objects]{a list of instances of @class{g-object}}"
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
  "Allocates the stable pointer for @code{thing}. Stable pointer is an integer that can be dereferenced with @fun{get-stable-pointer-value} and freed with @fun{free-stable-pointer}. Stable pointers are used to pass references to lisp objects to foreign code.
@arg[thing]{any object}
@return{integer}"
  (let ((id (find-fresh-id)))
    (setf (aref *registered-stable-pointers* id) thing)
    (make-pointer id)))

(defun free-stable-pointer (stable-pointer)
  "Frees the stable pointer previously allocated by @fun{allocate-stable-pointer}"
  (setf (aref *registered-stable-pointers* (pointer-address stable-pointer)) nil))

(defun get-stable-pointer-value (stable-pointer)
  "Returns the objects that is referenced by stable pointer previously allocated by @fun{allocate-stable-pointer}. May be called any number of times."
  (when (<= 0 (pointer-address stable-pointer) (length *registered-stable-pointers*))
    (aref *registered-stable-pointers* (pointer-address stable-pointer))))

(defun find-fresh-id ()
  (or (position nil *registered-stable-pointers*)
      (progn (vector-push-extend nil *registered-stable-pointers*)
             (1- (length *registered-stable-pointers*)))))

(defmacro with-stable-pointer ((ptr expr) &body body)
  "Executes @code{body} with @code{ptr} bound to the stable pointer to result of evaluating @code{expr}.

@arg[ptr]{a symbol naming the variable which will hold the stable pointer value}
@arg[expr]{an expression}"
  `(let ((,ptr (allocate-stable-pointer ,expr)))
     (unwind-protect
          (progn ,@body)
       (free-stable-pointer ,ptr))))