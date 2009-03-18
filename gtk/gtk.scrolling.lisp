(in-package :gtk)

(defcfun (scrolled-window-add-with-viewport "gtk_scrolled_window_add_with_viewport") :void
  (scrolled-window g-object)
  (child g-object))

(export 'scrolled-window-add-with-viewport)