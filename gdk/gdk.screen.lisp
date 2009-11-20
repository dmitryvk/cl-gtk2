(in-package :gdk)

(defcfun gdk-screen-get-monitor-geometry :void
  (screen (g-object screen))
  (monitor-num :int)
  (dest (g-boxed-foreign rectangle)))

(defun screen-get-monitor-geometry (screen monitor-num)
  (let ((dest (make-rectangle)))
    (gdk-screen-get-monitor-geometry screen monitor-num dest)
    dest))

(export 'screen-get-monitor-geometry)

(defcfun (screen-get-monitor-at-point "gdk_screen_get_monitor_at_point") :int
  (screen (g-object screen))
  (x :int)
  (y :int))

(export 'screen-get-monitor-at-point)

(defcfun (screen-get-monitor-at-window "gdk_screen_get_monitor_at_window") :int
  (screen (g-object screen))
  (window (g-object gdk-window)))

(export 'screen-get-monitor-at-window)

(defcfun (screen-get-monitor-height-mm "gdk_screen_get_monitor_height_mm") :int
  (screen (g-object screen))
  (monitor-num :int))

(defcfun (screen-get-monitor-width-mm "gdk_screen_get_monitor_width_mm") :int
  (screen (g-object screen))
  (monitor-num :int))

(export '(screen-get-monitor-height-mm screen-get-monitor-width-mm))

(defcfun (screen-get-monitor-plug-name "gdk_screen_get_monitor_plug_name") (glib:g-string :free-from-foreign t)
  (screen (g-object screen))
  (monitor-num :int))

(export 'screen-get-monitor-plug-name)

(defcfun (screen-broadcast-client-message "gdk_screen_broadcast_client_message") :void
  (screen (g-object screen))
  (event (g-boxed-foreign event)))

(export 'screen-broadcast-client-message)

(defcfun gdk-screen-get-setting :boolean
  (screen (g-object screen))
  (name :string)
  (value :pointer))

(defun screen-get-setting (screen name)
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (when (gdk-screen-get-setting screen name value)
      (prog1 (parse-g-value value)
        (g-value-unset value)))))

(export 'screen-get-setting)

(defcfun gdk-spawn-command-line-on-screen :boolean
  (screen (g-object screen))
  (command-line :string)
  (error :pointer))

(defun spawn-command-line-on-screen (screen command-line)
  (glib:with-g-error (err)
    (gdk-spawn-command-line-on-screen screen command-line err)))

(export 'spawn-command-line-on-screen)

(defcfun gdk_spawn_on_screen :boolean
  (screen (g-object screen))
  (working-directory :string)
  (argv :pointer)
  (envp :pointer)
  (flags glib:g-spawn-flags)
  (child-setup :pointer)
  (user-data :pointer)
  (child-pid (:pointer :int))
  (g-error :pointer))

(defcfun gdk_spawn_on_screen_with_pipes :boolean
  (screen (g-object screen))
  (working-directory :string)
  (argv :pointer)
  (envp :pointer)
  (flags glib:g-spawn-flags)
  (child-setup :pointer)
  (user-data :pointer)
  (child-pid (:pointer :int))
  (std-input (:pointer :int))
  (std-output (:pointer :int))
  (std-err (:pointer :int))
  (g-error :pointer))

(defmacro with-foreign-string-array ((var strings &key (null-terminated t)) &body body)
  (let ((strings-var (gensym))
        (s (gensym))
        (i (gensym))
        (n (gensym)))
    `(let* ((,strings-var ,strings)
            (,n (length ,strings-var)))
       (with-foreign-object (,var :pointer ,(if null-terminated `(1+ ,n) `,n))
         (iter (for ,s in ,strings-var)
               (for ,i from 0)
               (setf (mem-aref ,var :pointer ,i) (foreign-string-alloc ,s))
               ,@(when null-terminated
                       (list `(finally (setf (mem-aref ,var :pointer ,n) (null-pointer))))))
         (unwind-protect (progn ,@body)
           (iter (for ,i from 0 below ,n)
                 (foreign-string-free (mem-aref ,var :pointer ,i))))))))

(defun gdk-spawn-on-screen (screen argv &key working-directory env (flags '(:search-path)) with-pipes)
  (unless working-directory (setf working-directory (null-pointer)))
  (glib:with-g-error (err)
    (with-foreign-objects ((pid :int) (stdin :int) (stdout :int) (stderr :int))
      (with-foreign-string-array (argvp argv)
        (if (null env)
            (if with-pipes
                (gdk_spawn_on_screen_with_pipes screen
                                                working-directory
                                                argvp
                                                (null-pointer)
                                                flags
                                                (null-pointer)
                                                (null-pointer)
                                                pid stdin stdout stderr err)
                (gdk_spawn_on_screen screen
                                     working-directory
                                     argvp
                                     (null-pointer)
                                     flags
                                     (null-pointer)
                                     (null-pointer)
                                     pid err))
            (with-foreign-string-array (envp env)
              (if with-pipes
                  (gdk_spawn_on_screen_with_pipes screen
                                                  working-directory
                                                  argvp envp flags
                                                  (null-pointer) (null-pointer)
                                                  pid stdin stdout stderr err)
                  (gdk_spawn_on_screen screen
                                       working-directory
                                       argvp envp
                                       flags (null-pointer) (null-pointer)
                                       pid err)))))
      (if with-pipes
          (values (mem-ref pid :int) (mem-ref stdin :int) (mem-ref stdout :int) (mem-ref stderr :int))
          (mem-ref pid :int)))))

(export 'gdk-spawn-on-screen)
