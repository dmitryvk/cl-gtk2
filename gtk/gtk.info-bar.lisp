(in-package :gtk)

(defcfun (info-bar-add-button "gtk_info_bar_add_button") (g-object widget)
  (info-bar    (g-object info-bar))
  (button-text :string)
  (response-id :int))

(export 'info-bar-add-button)

(defcfun (info-bar-add-action-widget "gtk_info_bar_add_action_widget") :void
  (info-bar    (g-object info-bar))
  (widget      (g-object widget))
  (response-id :int))

(export 'info-bar-add-action-widget)
