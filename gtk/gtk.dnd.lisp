(in-package :gtk)

(define-g-flags "GtkTargetFlags" target-flags
    (:export t :type-initializer "gtk_target_flags_get_type")
  (:same-app 1)
  (:same-widget 2)
  (:other-app 4)
  (:other-widget 8))

(defcfun gtk-drag-finish :void
  (context g-object)
  (success :boolean)
  (del :boolean)
  (time :uint32))

(defun drag-finish (context success del time)
  (gtk-drag-finish context success del time))

(export 'drag-finish)

