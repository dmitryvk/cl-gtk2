(in-package :gtk)

(defcfun (window-group-add-window "gtk_window_group_add_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-add-window)

(defcfun (window-group-remove-window "gtk_window_group_remove_window") :void
  (window-group (g-object window-group))
  (window (g-object gtk-window)))

(export 'window-group-remove-window)
