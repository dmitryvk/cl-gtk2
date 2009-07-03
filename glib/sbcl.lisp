(in-package :glib)

#+thread-support
(progn
  (defun glib-stop-thread ()
    (setf *threads-initialized-p* nil))
  (pushnew 'glib-stop-thread sb-ext:*save-hooks*))

(defun map-inherited-classes (class fn)
  (when (symbolp class) (setf class (find-class class)))
  (when class
    (funcall fn class)
    (iter (for subclass in (closer-mop:class-direct-subclasses class))
          (map-inherited-classes subclass fn))))

(pushnew 'run-initializers sb-ext:*init-hooks*)
