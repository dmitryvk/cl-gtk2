(in-package :gtk)

(defun finalize-subclasses (class)
  ;(format t "Finalizing ~A~%" class)
  (c2mop:ensure-finalized class)
  (iter (for subclass in (c2mop:class-direct-subclasses class))
        (finalize-subclasses subclass)))

(defun finalize-gtk-classes ()
  (finalize-subclasses (find-class 'gobject:g-object)))

(finalize-gtk-classes)
