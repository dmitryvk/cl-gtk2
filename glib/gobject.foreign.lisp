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
