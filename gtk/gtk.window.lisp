(in-package :gtk)

(defcfun (window-add-accel-group "gtk_window_add_accel_group") :void
  (window (g-object gtk-window))
  (accel-group (g-object accel-group)))

(export 'window-add-accel-group)

(defcfun (window-remove-accel-group "gtk_window_remove_accel_group") :void
  (window (g-object gtk-window))
  (accel-group (g-object accel-group)))

(export 'window-remove-accel-group)

(defcfun (window-activate-focus "gtk_window_activate_focus") :boolean
  (window (g-object gtk-window)))

(export 'window-activate-focus)

(defcfun (window-activate-default "gtk_window_activate_default") :boolean
  (window (g-object gtk-window)))

(export 'window-activate-default)

(defcfun (window-set-geometry-hints "gtk_window_set_geometry_hints") :void
  (window (g-object gtk-window))
  (geometry-widget (g-object widget))
  (geometry geometry)
  (geometry-mask window-hints))

(export 'window-set-geometry-hints)

(defcfun (window-list-toplevels "gtk_window_list_toplevels") (glist (g-object gtk-window) :free-from-foreign t))

(export 'window-list-toplevels)

(defcfun (window-add-mnemonic "gtk_window_add_mnemonic") :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object widget)))

(export 'window-add-mnemonic)

(defcfun (window-remove-mnemonic "gtk_window_remove_mnemonic") :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object widget)))

(export 'window-remove-mnemonic)

(defcfun (window-activate-mnemonic "gtk_window_mnemonic_activate") :boolean
  (window (g-object gtk-window))
  (keyval :uint)
  (modifier modifier-type))

(export 'window-activate-mnemonic)

(defcfun (window-activate-key "gtk_window_activate_key") :boolean
  (window (g-object gtk-window))
  (event (g-boxed-ptr event-key)))

(export 'window-activate-key)

(defcfun (window-propagate-key-event "gtk_window_propagate_key_event") :boolean
  (window (g-object gtk-window))
  (event (g-boxed-ptr event-key)))

(export 'window-propagate-key-event)

(defcfun (window-focus "gtk_window_get_focus") (g-object widget)
  (window (g-object gtk-window)))

(defcfun (window-set-focus "gtk_window_set_focus") :void
  (window (g-object gtk-window))
  (focus (g-object widget)))

(defun (setf window-focus) (focus window)
  (window-set-focus window focus)
  focus)

(export 'window-focus)

(defcfun (window-default-widget "gtk_window_get_default_widget") (g-object widget)
  (window (g-object gtk-window)))

(defcfun (window-set-default-widget "gtk_window_set_default") :void
  (window (g-object gtk-window))
  (default-widget (g-object widget)))

(defun (setf window-default-widget) (default-widget window)
  (window-set-default-widget window default-widget)
  default-widget)

(export 'window-default-widget)

(defcfun (present-window "gtk_window_present") :void
  (window (g-object gtk-window)))

(export 'present-window)

(defcfun (present-window-with-time "gtk_window_present_with_time") :void
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'present-window-with-time)

(defcfun (iconify-window "gtk_window_iconify") :void
  (window (g-object gtk-window)))

(export 'iconify-window)

(defcfun (deiconify-window "gtk_window_deiconify") :void
  (window (g-object gtk-window)))

(export 'deiconify-window)

(defcfun (stick-window "gtk_window_stick") :void
  (window (g-object gtk-window)))

(export 'stick-window)

(defcfun (unstick-window "gtk_window_unstick") :void
  (window (g-object gtk-window)))

(export 'unstick-window)

(defcfun (maximize-window "gtk_window_maximize") :void
  (window (g-object gtk-window)))

(export 'maximize-window)

(defcfun (unmaximize-window "gtk_window_unmaximize") :void
  (window (g-object gtk-window)))

(export 'unmaximize-window)

(defcfun (fullscreen-window "gtk_window_fullscreen") :void
  (window (g-object gtk-window)))

(export 'fullscreen-window)

(defcfun (unfullscreen-window "gtk_window_unfullscreen") :void
  (window (g-object gtk-window)))

(export 'unfullscreen-window)

(defcfun (window-set-keep-above "gtk_window_set_keep_above") :void
  (window (g-object gtk-window))
  (setting :boolean))

(export 'window-set-keep-above)

(defcfun (window-set-keep-below "gtk_window_set_keep_below") :void
  (window (g-object gtk-window))
  (setting :boolean))

(export 'window-set-keep-below)

(defcfun (window-begin-resize-drag "gtk_window_begin_resize_drag") :void
  (window (g-object gtk-window))
  (edge window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'window-begin-resize-drag)

(defcfun (window-begin-move-drag "gtk_window_begin_move_drag") :void
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'window-begin-move-drag)

(defcfun (window-set-frame-dimensions "gtk_window_set_frame_dimensions") :void
  (window (g-object gtk-window))
  (left :int)
  (top :int)
  (right :int)
  (bottom :int))

(export 'window-set-frame-dimensions)

(defcfun (window-set-has-frame "gtk_window_set_has_frame") :void
  (window (g-object gtk-window))
  (setting :boolean))

(export 'window-set-has-frame)

(defcfun (window-set-mnemonic-modifier "gtk_window_set_mnemonic_modifier") :void
  (window (g-object gtk-window))
  (modifier modifier-type))

(export 'window-set-mnemonic-modifier)

(defcfun (window-icon-list "gtk_window_get_icon_list") (glist pixbuf :free-from-foreign t)
  (window (g-object gtk-window)))

(defcfun (window-set-icon-list "gtk_window_set_icon_list") :void
  (window (g-object gtk-window))
  (icons (glist pixbuf :free-to-foreign t)))

(defun (setf window-icon-list) (icon-list window)
  (window-set-icon-list window icon-list))

(export 'window-icon-list)

(defcfun (%window-get-position "gtk_window_get_position") :void
  (window (g-object gtk-window))
  (root-x (:pointer :int))
  (root-y (:pointer :int)))

(defun window-get-position (window)
  (with-foreign-objects ((x :int)
                         (y :int))
    (%window-get-position window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'window-get-position)

(defcfun (%window-get-size "gtk_window_get_size") :void
  (window (g-object gtk-window))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun window-size (window)
  (with-foreign-objects ((width :int)
                         (height :int))
    (%window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'window-size)

(defcfun (window-group "gtk_window_get_group") (g-object window-group)
  (window (g-object gtk-window)))

(export 'window-group)

(defcfun (move-window "gtk_window_move") :void
  (window (g-object gtk-window))
  (x :int)
  (y :int))

(export 'move-window)

(defcfun (window-parse-geometry "gtk_window_parse_geometry") :boolean
  (window (g-object gtk-window))
  (geometry-string :string))

(export 'window-parse-geometry)

(defcfun (reshow-window-with-initial-size "gtk_window_reshow_with_initial_size") :void
  (window (g-object gtk-window)))

(export 'reshow-window-with-initial-size)

(defcfun (resize-window "gtk_window_resize") :void
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'resize-window)

(defcfun (default-window-icon-list "gtk_window_get_default_icon_list") (glist pixbuf))

(defcfun (set-default-window-icon-list "gtk_window_set_default_icon_list") :boolean
  (icon-list (glist pixbuf)))

(defun (setf default-window-icon-list) (icon-list)
  (set-default-window-icon-list icon-list)
  icon-list)

(export 'default-window-icon-list)

(defcfun (set-default-window-icon "gtk_window_set_default_icon") :void
  (icon (g-object pixbuf)))

(defcfun (set-default-window-icon-name "gtk_window_set_default_icon_name") :void
  (icon-name :string))

(defun (setf default-window-icon) (icon)
  (etypecase icon
    (pixbuf (set-default-window-icon icon))
    (string (set-default-window-icon-name icon))))

(export 'default-window-icon)

(defcfun (set-window-auto-startup-notification "gtk_window_set_auto_startup_notification") :void
  (setting :boolean))

(export 'set-window-auto-startup-notification)

(defcfun (window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-add-window)

(defcfun (window-group-remove-window "gtk_window_group_remove_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-remove-window)

(defcfun (window-group-list-windows "gtk_window_group_list_windows") (glist gtk-window)
  (window-group (g-object window-group)))

(export 'window-group-list-windows)