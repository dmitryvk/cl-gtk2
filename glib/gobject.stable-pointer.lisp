(in-package :gobject)

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

(defun set-stable-pointer-value (stable-pointer value)
  "Returns the objects that is referenced by stable pointer previously allocated by @fun{allocate-stable-pointer}. May be called any number of times."
  (when (<= 0 (pointer-address stable-pointer) (length *registered-stable-pointers*))
    (setf (aref *registered-stable-pointers* (pointer-address stable-pointer)) value)))

(defun stable-pointer-value (stable-pointer)
  (get-stable-pointer-value stable-pointer))

(defun (setf stable-pointer-value) (new-value stable-pointer)
  (set-stable-pointer-value stable-pointer new-value))

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
