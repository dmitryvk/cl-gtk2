(in-package :gtk)

(define-g-flags "GtkTargetFlags" target-flags
    (:export t :type-initializer "gdk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

