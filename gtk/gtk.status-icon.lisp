(in-package :gtk)

(defcfun (set-status-icon-tooltip "gtk_status_icon_set_tooltip") :void
  (status-icon (g-object status-icon))
  (tooltip-text :string))

(export 'set-status-icon-tooltip)