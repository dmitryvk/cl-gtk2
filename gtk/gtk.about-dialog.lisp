(in-package :gtk)

(defvar *about-dialog-url-func* nil)

(defcallback about-dialog-url-func-cb :void
  ((dialog (g-object about-dialog)) (link (:string :free-from-foreign nil)) (user-data :pointer))
  (declare (ignore user-data))
  (funcall *about-dialog-url-func* dialog link))

(defcallback about-dialog-url-func-destroy-cb :void
  ((data :pointer))
  (declare (ignore data))
  (setf *about-dialog-url-func* nil))

(defcfun gtk-about-dialog-set-url-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf about-dialog-global-url-hook) (new-value)
  (if new-value
      (gtk-about-dialog-set-url-hook (callback about-dialog-url-func-cb)
				     (null-pointer)
				     (callback about-dialog-url-func-destroy-cb))
      (gtk-about-dialog-set-url-hook (null-pointer)
				     (null-pointer)
				     (null-pointer)))
  (setf *about-dialog-url-func* new-value))

(export 'about-dialog-global-url-hook)

(defvar *about-dialog-email-func* nil)

(defcallback about-dialog-email-func-cb :void
  ((dialog (g-object about-dialog)) (link (:string :free-from-foreign nil)) (user-data :pointer))
  (declare (ignore user-data))
  (funcall *about-dialog-email-func* dialog link))

(defcallback about-dialog-email-func-destroy-cb :void
  ((data :pointer))
  (declare (ignore data))
  (setf *about-dialog-email-func* nil))

(defcfun gtk-about-dialog-set-email-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf about-dialog-global-email-hook) (new-value)
  (if new-value
      (gtk-about-dialog-set-email-hook (callback about-dialog-email-func-cb)
				       (null-pointer)
				       (callback about-dialog-email-func-destroy-cb))
      (gtk-about-dialog-set-email-hook (null-pointer)
				       (null-pointer)
				       (null-pointer)))
  (setf *about-dialog-email-func* new-value))

(export 'about-dialog-global-email-hook)

