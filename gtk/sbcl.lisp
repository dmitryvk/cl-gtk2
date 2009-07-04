(in-package :gtk)

#+thread-support
(progn
  (defun stop-main-thread-on-save ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (within-main-loop-and-wait (gtk-main-quit))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))
  (defun cl-gtk2-sbcl-init ()
    (gtk-init))
  (pushnew 'cl-gtk2-sbcl-init sb-ext:*init-hooks*)
  (pushnew 'stop-main-thread-on-save sb-ext:*save-hooks*))

