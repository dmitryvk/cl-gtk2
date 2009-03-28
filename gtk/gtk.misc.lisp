(in-package :gtk)

(defcallback stable-pointer-free-destroy-notify-callback :void ((data :pointer))
  (free-stable-pointer data))

(defcfun (get-clipboard "gtk_clipboard_get") g-object
  (selection gdk-atom-as-string))

(export 'get-clipboard)

(defcallback call-from-main-loop-callback :boolean
    ((data :pointer))
  (restart-case
      (progn (funcall (get-stable-pointer-value data))
             nil)
    (return-from-callback () nil)))

(defun call-from-gtk-main-loop (function &key (priority +g-priority-default-idle+))
  (g-idle-add-full priority
                   (callback call-from-main-loop-callback)
                   (allocate-stable-pointer function)
                   (callback stable-pointer-free-destroy-notify-callback)))

(export 'call-from-gtk-main-loop)

(defmacro within-main-loop (&body body)
  `(call-from-gtk-main-loop (lambda () ,@body)))

(export 'within-main-loop)