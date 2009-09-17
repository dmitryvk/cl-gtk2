(in-package :gtk)

(defvar *link-button-uri-func* nil)

(defcallback link-button-uri-func-cb :void
    ((button (g-object link-button)) (link (:string :free-from-foreign nil)) (user-data :pointer))
  (declare (ignore user-data))
  (funcall *link-button-uri-func* button link))

(defcallback link-button-uri-func-destroy-cb :void
    ((data :pointer))
  (declare (ignore data))
  (setf *link-button-uri-func* nil))

(defcfun gtk-link-button-set-uri-hook :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun (setf link-button-global-uri-hook) (new-value)
  (if new-value
      (gtk-link-button-set-uri-hook (callback link-button-uri-func-cb)
                                    (null-pointer)
                                    (callback link-button-uri-func-destroy-cb))
      (gtk-link-button-set-uri-hook (null-pointer)
                                    (null-pointer)
                                    (null-pointer)))
  (setf *link-button-uri-func* new-value))

(export 'link-button-global-uri-hook)
