(in-package :gdk)

(defcfun gdk-threads-init :void)
(glib:at-init () (gdk-threads-init))

(defcfun gdk-threads-enter :void)
(export 'gdk-threads-enter)

(defcfun gdk-threads-leave :void)
(export 'gdk-threads-leave)

(defmacro with-gdk-threads-lock (&body body)
  `(progn
     (gdk-threads-enter)
     (unwind-protect
          (progn ,@body)
       (gdk-threads-leave))))
(export 'with-gdk-threads-lock)

;; ignored:
;; void                gdk_threads_set_lock_functions      (GCallback enter_fn,
;;                                                          GCallback leave_fn);

(defcallback source-func-callback :boolean
    ((data :pointer))
  (funcall (stable-pointer-value data)))

(defcallback stable-pointer-free-destroy-notify-callback :void ((data :pointer))
  (free-stable-pointer data))

(defcfun gdk_threads_add_idle_full :uint
  (priority :int)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gdk-threads-add-idle-full (priority function)
  (gdk_threads_add_idle_full priority
                             (callback source-func-callback)
                             (allocate-stable-pointer function)
                             (callback stable-pointer-free-destroy-notify-callback)))

(export 'gdk-threads-add-idle-full)

(defcfun gdk_threads_add_timeout_full :uint
  (priority :int)
  (interval :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gdk-threads-add-timeout-full (priority interval-msec function)
  (gdk_threads_add_timeout_full priority interval-msec
                                (callback source-func-callback)
                                (allocate-stable-pointer function)
                                (callback stable-pointer-free-destroy-notify-callback)))

(export 'gdk-threads-add-timeout-full)

(defcfun gdk_threads_add_timeout_seconds_full :uint
  (priority :int)
  (interval :uint)
  (function :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gdk-threads-add-timeout-seconds-full (priority interval-sec function)
  (gdk_threads_add_timeout_seconds_full priority interval-sec
                                        (callback source-func-callback)
                                        (allocate-stable-pointer function)
                                        (callback stable-pointer-free-destroy-notify-callback)))

(export 'gdk-threads-add-timeout-seconds-full)
