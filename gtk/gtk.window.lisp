(in-package :gtk)

(defcfun (gtk-window-add-accel-group "gtk_window_add_accel_group") :void
  (window (g-object gtk-window))
  (accel-group (g-object accel-group)))

(export 'gtk-window-add-accel-group)

(defcfun (gtk-window-remove-accel-group "gtk_window_remove_accel_group") :void
  (window (g-object gtk-window))
  (accel-group (g-object accel-group)))

(export 'gtk-window-remove-accel-group)

(defcfun (gtk-window-activate-focus "gtk_window_activate_focus") :boolean
  (window (g-object gtk-window)))

(export 'gtk-window-activate-focus)

(defcfun (gtk-window-activate-default "gtk_window_activate_default") :boolean
  (window (g-object gtk-window)))

(export 'gtk-window-activate-default)

(defcfun (gtk-window-set-geometry-hints "gtk_window_set_geometry_hints") :void
  (window (g-object gtk-window))
  (geometry-widget (g-object widget))
  (geometry (g-boxed-foreign geometry))
  (geometry-mask gdk-window-hints))

(export 'gtk-window-set-geometry-hints)

(defcfun (gtk-window-list-toplevels "gtk_window_list_toplevels") (glist (g-object gtk-window) :free-from-foreign t))

(export 'gtk-window-list-toplevels)

(defcfun (gtk-window-add-mnemonic "gtk_window_add_mnemonic") :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object widget)))

(export 'gtk-window-add-mnemonic)

(defcfun (gtk-window-remove-mnemonic "gtk_window_remove_mnemonic") :void
  (window (g-object gtk-window))
  (keyval :uint)
  (target (g-object widget)))

(export 'gtk-window-remove-mnemonic)

(defcfun (gtk-window-activate-mnemonic "gtk_window_mnemonic_activate") :boolean
  (window (g-object gtk-window))
  (keyval :uint)
  (modifier modifier-type))

(export 'gtk-window-activate-mnemonic)

(defcfun (gtk-window-activate-key "gtk_window_activate_key") :boolean
  (window (g-object gtk-window))
  (event (g-boxed-foreign event)))

(export 'gtk-window-activate-key)

(defcfun (gtk-window-propagate-key-event "gtk_window_propagate_key_event") :boolean
  (window (g-object gtk-window))
  (event (g-boxed-foreign event)))

(export 'gtk-window-propagate-key-event)

(defcfun (gtk-window-present "gtk_window_present") :void
  (window (g-object gtk-window)))

(export 'gtk-window-present)

(defcfun (gtk-window-present-with-time "gtk_window_present_with_time") :void
  (window (g-object gtk-window))
  (timestamp :uint32))

(export 'gtk-window-present-with-time)

(defcfun (gtk-window-iconify "gtk_window_iconify") :void
  (window (g-object gtk-window)))

(export 'gtk-window-iconify)

(defcfun (gtk-window-deiconify "gtk_window_deiconify") :void
  (window (g-object gtk-window)))

(export 'gtk-window-deiconify)

(defcfun (gtk-window-stick "gtk_window_stick") :void
  (window (g-object gtk-window)))

(export 'gtk-window-stick)

(defcfun (gtk-window-unstick "gtk_window_unstick") :void
  (window (g-object gtk-window)))

(export 'gtk-window-unstick)

(defcfun (gtk-window-maximize "gtk_window_maximize") :void
  (window (g-object gtk-window)))

(export 'gtk-window-maximize)

(defcfun (gtk-window-unmaximize "gtk_window_unmaximize") :void
  (window (g-object gtk-window)))

(export 'gtk-window-unmaximize)

(defcfun (gtk-window-fullscreen "gtk_window_fullscreen") :void
  (window (g-object gtk-window)))

(export 'gtk-window-fullscreen)

(defcfun (gtk-window-unfullscreen "gtk_window_unfullscreen") :void
  (window (g-object gtk-window)))

(export 'gtk-window-unfullscreen)

(defcfun (gtk-window-begin-resize-drag "gtk_window_begin_resize_drag") :void
  (window (g-object gtk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-resize-drag)

(defcfun (gtk-window-begin-move-drag "gtk_window_begin_move_drag") :void
  (window (g-object gtk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gtk-window-begin-move-drag)

(defcfun (gtk-window-set-frame-dimensions "gtk_window_set_frame_dimensions") :void
  (window (g-object gtk-window))
  (left :int)
  (top :int)
  (right :int)
  (bottom :int))

(export 'gtk-window-set-frame-dimensions)

(defcfun (%gtk-window-get-position "gtk_window_get_position") :void
  (window (g-object gtk-window))
  (root-x (:pointer :int))
  (root-y (:pointer :int)))

(defun gtk-window-get-position (window)
  (with-foreign-objects ((x :int)
                         (y :int))
    (%gtk-window-get-position window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gtk-window-get-position)

(defcfun (%gtk-window-get-size "gtk_window_get_size") :void
  (window (g-object gtk-window))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gtk-window-size (window)
  (with-foreign-objects ((width :int)
                         (height :int))
    (%gtk-window-get-size window width height)
    (values (mem-ref width :int) (mem-ref height :int))))

(export 'gtk-window-size)

(defcfun (window-move "gtk_window_move") :void
  (window (g-object gtk-window))
  (x :int)
  (y :int))

(export 'window-move)

(defcfun (gtk-window-parse-geometry "gtk_window_parse_geometry") :boolean
  (window (g-object gtk-window))
  (geometry-string :string))

(export 'gtk-window-parse-geometry)

(defcfun (gtk-window-reshow-with-initial-size "gtk_window_reshow_with_initial_size") :void
  (window (g-object gtk-window)))

(export 'gtk-window-reshow-with-initial-size)

(defcfun (gtk-window-resize "gtk_window_resize") :void
  (window (g-object gtk-window))
  (width :int)
  (height :int))

(export 'gtk-window-resize)

(defcfun (gtk-window-default-icon-list "gtk_window_get_default_icon_list") (glist (g-object pixbuf)))

(defcfun (set-default-window-icon-list "gtk_window_set_default_icon_list") :boolean
  (icon-list (glist (g-object pixbuf))))

(defun (setf gkt-window-default-icon-list) (icon-list)
  (set-default-window-icon-list icon-list)
  icon-list)

(export 'gtk-window-default-icon-list)

(defcfun (gtk-window-set-default-icon "gtk_window_set_default_icon") :void
  (icon (g-object pixbuf)))

(defcfun (gtk-window-default-icon-name "gtk_window_get_default_icon_name") (:string :free-from-foreign nil))

(defcfun (set-default-window-icon-name "gtk_window_set_default_icon_name") :void
  (icon-name :string))

(defun (setf gtk-window-default-icon) (icon)
  (gtk-window-set-default-icon icon))

(defun (setf gtk-window-default-icon-name) (name)
  (set-default-window-icon-name name))

(export 'window-default-icon)

(export 'window-default-icon-name)

(defcfun (set-window-auto-startup-notification "gtk_window_set_auto_startup_notification") :void
  (setting :boolean))

(export 'set-window-auto-startup-notification)

(defcfun (gtk-window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-add-window)

(defcfun (gtk-window-group-remove-window "gtk_window_group_remove_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'gtk-window-group-remove-window)

(defcfun (gtk-window-group-list-windows "gtk_window_group_list_windows") (glist gtk-window)
  (window-group (g-object window-group)))

(export 'gtk-window-group-list-windows)

(defcfun (gtk-window-mnemonic-activate "gtk_window_mnemonic_activate") :boolean
  (window (g-object gtk-window))
  (keyval :uint)
  (modifier modifier-type))

(export 'gtk-window-mnemonic-activate)
