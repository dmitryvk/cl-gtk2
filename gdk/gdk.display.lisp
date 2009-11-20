(in-package :gdk)

(defcfun (display-open "gdk_display_open") (g-object display)
  (display-name :string))
(export 'display-open)

(defcfun (default-display "gdk_display_get_default") (g-object display))
(export 'default-display)

(defcfun (display-get-screen "gdk_display_get_screen") (g-object screen)
  (display (g-object display))
  (screen-num :int))
(export 'display-get-screen)

(defcfun (display-pointer-ungrab "gdk_display_pointer_ungrab") :void
  (display (g-object display))
  (time :uint32))
(export 'display-pointer-ungrab)

(defcfun (display-keyboard-ungrab "gdk_display_keyboard_ungrab") :void
  (display (g-object display))
  (time :uint32))
(export 'display-keyboard-ungrab)

(defcfun (display-pointer-is-grabbed "gdk_display_pointer_is_grabbed") :boolean
  (display (g-object display)))
(export 'display-pointer-is-grabbed)

(defcfun (display-beep "gdk_display_beep") :void
  (display (g-object display)))
(export 'display-beep)

(defcfun (display-sync "gdk_display_sync") :void
  (display (g-object display)))
(export 'display-sync)

(defcfun (display-flush "gdk_display_flush") :void
  (display (g-object display)))
(export 'display-flush)

(defcfun (display-close "gdk_display_close") :void
  (display (g-object display)))
(export 'display-close)

(defcfun (display-get-event "gdk_display_get_event") (g-boxed-foreign event :return)
  (display (g-object display)))
(export 'display-get-event)

(defcfun (display-peek-event "gdk_display_peek_event") (g-boxed-foreign event :return)
  (display (g-object display)))
(export 'display-peek-event)

(defcfun (display-put-event "gdk_display_put_event") :void
  (display (g-object display))
  (event (g-boxed-foreign event)))
(export 'display-put-event)

(defcfun gdk_display_add_client_message_filter :void
  (display (g-object display))
  (message-type gdk-atom-as-string)
  (func :pointer)
  (data :pointer))

(defun dispaly-add-client-message-filter (display message-type fn)
  (gdk_display_add_client_message_filter display message-type (callback gdk-client-message-filter-func) (allocate-stable-pointer fn)))

(export 'display-add-client-message-filter)

(defcfun (display-set-double-click-time "gdk_display_set_double_click_time") :void
  (display (g-object display))
  (msec :uint))
(export 'display-set-double-click-time)

(defcfun (display-set-double-click-distance "gdk_display_set_double_click_distance") :void
  (display (g-object display))
  (distance :uint))
(export 'display-set-double-click-distance)

(defcfun gdk-display-get-pointer :void
  (display (g-object display))
  (screen :pointer)
  (x :pointer)
  (y :pointer)
  (mask :pointer))

(defun display-get-pointer (display)
  (with-foreign-objects ((screen :pointer) (x :int) (y :int) (mask 'modifier-type))
    (gdk-display-get-pointer display screen x y mask)
    (values (mem-ref screen '(g-object screen))
            (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref mask :int))))
(export 'display-get-pointer)

(defcfun gdk-display-get-window-at-pointer (g-object gdk-window)
  (display (g-object display))
  (win-x :pointer)
  (win-y :pointer))

(defun display-get-window-at-pointer (display)
  (with-foreign-objects ((win-x :int) (win-y :int))
    (let ((win (gdk-display-get-window-at-pointer display win-x win-y)))
      (values win (mem-ref win-x :int) (mem-ref win-y :int)))))

(export 'display-get-window-at-pointer)

; ignored:
;GdkDisplayPointerHooks * gdk_display_set_pointer_hooks  (GdkDisplay *display,
;                                                         const GdkDisplayPointerHooks *new_hooks);

(defcfun (display-warp-pointer "gdk_display_warp_pointer") :void
  (display (g-object display))
  (screen (g-object screen))
  (x :int)
  (y :int))
(export 'display-warp-pointer)

(defcfun gdk-display-get-maximal-cursor-size :void
  (display (g-object display))
  (width :pointer)
  (height :pointer))

(defun display-get-maximal-cursor-size (display)
  (with-foreign-objects ((width :uint) (height :uint))
    (gdk-display-get-maximal-cursor-size display width height)
    (values (mem-ref width :uint) (mem-ref height :uint))))
(export 'display-get-maximal-cursor-size)

(defcfun (display-request-selection-notification "gdk_display_request_selection_notification") :boolean
  (display (g-object display))
  (selection gdk-atom-as-string))
(export 'display-request-selection-notification)

(defcfun gdk-display-store-clipboard :void
  (display (g-object display))
  (clipboard-window (g-object gdk-window))
  (time :uint32)
  (targets :pointer)
  (n-targets :int))

(defun display-store-clipboard (display clipboard-window time targets)
  (let ((n-targets (length targets)))
    (with-foreign-object (targets-ptr 'gdk-atom-as-string n-targets)
      (loop
         for str in targets
         for i from 0
         do (setf (mem-aref targets-ptr 'gdk-atom-as-string i) str))
      (gdk-display-store-clipboard display clipboard-window time targets-ptr n-targets))))
(export 'display-store-clipboard)

(defcfun (display-manager-get "gdk_display_manager_get") (g-object display-manager))
(export 'display-manager-get)
