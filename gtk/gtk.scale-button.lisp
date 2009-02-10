(in-package :gtk)

(defcfun (scale-button-popup "gtk_scale_button_get_popup") (g-object widget)
  (scale-button (g-object scale-button)))

(defcfun (scale-button-plus-button "gtk_scale_button_get_plus_button") (g-object widget)
  (scale-button (g-object scale-button)))

(defcfun (scale-button-minus-button "gtk_scale_button_get_minus_button") (g-object widget)
  (scale-button (g-object scale-button)))

(export 'scale-button-popup)
(export 'scale-button-plus-button)
(export 'scale-button-minus-button)