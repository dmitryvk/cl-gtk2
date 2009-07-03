(in-package :glib)

#+thread-support
(progn
  (defun glib-stop-thread ()
    (setf *threads-initialized-p* nil))
  (defun glib-start-thread ()
    (g-thread-init (null-pointer))
    (setf *threads-initialized-p* t))
  (pushnew 'glib-stop-thread sb-ext:*save-hooks*)
  (pushnew 'glib-start-thread sb-ext:*init-hooks*))

(defun map-inherited-classes (class fn)
  (when (symbolp class) (setf class (find-class class)))
  (when class
    (funcall fn class)
    (iter (for subclass in (closer-mop:class-direct-subclasses class))
          (map-inherited-classes subclass fn))))

(defun initialize-all-gobject-types ()
  (map-inherited-classes 'gobject::g-object
                         (lambda (class)
                           (when (typep class 'gobject::gobject-class)
                             (gobject::initialize-gobject-class-g-type class)))))

(pushnew 'initialize-all-gobject-types sb-ext:*init-hooks*)
