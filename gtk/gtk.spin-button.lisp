(in-package :gtk)

(define-g-enum "GtkSpinType" spin-type ()
  (:step-forward 0)
  (:step-backward 1) (:page-forward 2) (:page-backward 3)
  (:home 4) (:end 5) (:user-defined 6))

(defcfun (spin-button-spin "gtk_spin_button_spin") :void
  (spin-button (g-object spin-button))
  (direction spin-type)
  (increment :double))

(export 'spin-button-spin)

(defcfun (spin-button-update "gtk_spin_button_update") :void
  (spin-button (g-object spin-button)))

(export 'spin-button-update)
