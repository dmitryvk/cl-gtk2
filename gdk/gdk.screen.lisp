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
