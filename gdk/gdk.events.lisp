(in-package :gdk)

(defconstant +gdk-current-time+ 0)

(export '+gdk-current-time+)

(defconstant +gdk-priority-redraw+ (+ glib:+g-priority-high-idle+ 20))

(export '+gdk-priority-redraw+)

(defcfun (events-pending "gdk_events_pending") :boolean)

(export 'events-pending)

(defcfun (event-peek "gdk_event_peek") (g-boxed-foreign event :return))

(export 'event-peek)

(defcfun (event-get "gdk_event_get") (g-boxed-foreign event :return))

;; ignored
;; GdkEvent*           gdk_event_get_graphics_expose       (GdkWindow *window);

(defcfun (event-put "gdk_event_put") :void
  (event (g-boxed-foreign event)))

(export 'event-put)

(defcfun (event-get-time "gdk_event_get_time") :uint32
  (event (g-boxed-foreign event)))

(export 'event-get-time)

(defcfun gdk_event_get_state :boolean
  (event (g-boxed-foreign event))
  (state (:pointer modifier-type)))

(defun event-get-state (event)
  (with-foreign-object (state 'modifier-type)
    (when (gdk_event_get_state event state)
      (mem-ref state 'modifier-type))))

(export 'event-get-state)

(defcfun gdk_event_get_axis :boolean
  (event (g-boxed-foreign event))
  (axis-use axis-use)
  (value (:pointer :double)))

(defun event-get-axis (event axis)
  (with-foreign-object (value :double)
    (when (gdk_event_get_axis event axis value)
      (mem-ref value :double))))

(export 'event-get-axis)

(defcfun gdk_event_get_coords :boolean
  (event (g-boxed-foreign event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun event-get-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (gdk_event_get_coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

(export 'event-get-coords)

(defcfun gdk_event_get_root_coords :boolean
  (event (g-boxed-foreign event))
  (x-win (:pointer :double))
  (y-win (:pointer :double)))

(defun event-get-root-coords (event)
  (with-foreign-objects ((x :double) (y :double))
    (when (gdk_event_get_root_coords event x y)
      (values (mem-ref x :double) (mem-ref y :double)))))

(export 'event-get-root-coords)

(defcfun (event-request-motions "gdk_event_request_motions") :void
  (event (g-boxed-foreign event)))

(export 'event-request-motions)

(defcallback gdk-event-func-callback :void
    ((event (g-boxed-foreign event)) (user-data :pointer))
  (restart-case
      (funcall (get-stable-pointer-value user-data) event)
    (return-from-callback () nil)))

(defcfun gdk_event_handler_set :void
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun event-handler-set (fn)
  (gdk_event_handler_set (callback gdk-event-func-callback)
                         (allocate-stable-pointer fn)
                         (callback stable-pointer-free-destroy-notify-callback)))

(export 'event-handler-set)

(defcfun (event-send-client-message "gdk_event_send_client_message") :boolean
  (event (g-boxed-foreign event))
  (window-id native-window))

(export 'event-send-client-message)

(defcfun (event-send-client-message-for-display "gdk_event_send_client_message_for_display") :boolean
  (display (g-object display))
  (event (g-boxed-foreign event))
  (window-id native-window))

(export 'event-send-client-message-for-display)

(defcfun (event-send-client-message-to-all "gdk_event_send_clientmessage_toall") :void
  (event (g-boxed-foreign event)))

(export 'event-send-client-message-to-all)

(defcallback gdk-client-message-filter-func gdk-filter-return
    ((xevent :pointer) (event :pointer) (data :pointer))
  (multiple-value-bind (return-value translated-event) (funcall (stable-pointer-value data) xevent)
    (when (eq return-value :translate)
      (gobject:copy-boxed-slots-to-foreign translated-event event))
    return-value))

(defcfun gdk_add_client_message_filter :void
  (message-type gdk-atom-as-string)
  (func :pointer)
  (data :pointer))

(defun gdk-add-client-message-filter (message-type fn)
  (gdk_add_client_message_filter message-type (callback gdk-client-message-filter-func) (allocate-stable-pointer fn)))

(export 'gdk-add-client-message-filter)

(defcfun gdk-get-show-events :boolean)

(export 'gdk-get-show-events)

(defcfun gdk-set-show-events :void
  (show-events :boolean))

(export 'gdk-set-show-events)

;; ignored:
;; void                gdk_event_set_screen                (GdkEvent *event,
;;                                                          GdkScreen *screen);
;; GdkScreen *         gdk_event_get_screen                (const GdkEvent *event);

(defcfun gdk_setting_get :boolean
  (name :string)
  (value (:pointer g-value)))

(defun gdk-get-setting (name)
  (with-foreign-object (value 'g-value)
    (g-value-zero value)
    (when (gdk_setting_get name value)
      (prog1 (parse-g-value value)
        (g-value-unset value)))))

(export 'gdk-get-setting)
