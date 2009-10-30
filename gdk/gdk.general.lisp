(in-package :gdk)

(defcfun gdk-set-locale (:string :free-from-foreign nil))
(export 'gdk-set-locale)

(defcfun (set-sm-client-id "gdk_set_sm_client_id") :void
  (sm-client-id :string))
(export 'set-sm-client-id)

(defcfun gdk-notify-startup-complete :void)
(defcfun gdk-notify-startup-complete-with-id :void
  (startup-id :string))

(defun notify-startup-complete (&optional startup-id)
  (if startup-id
      (gdk-notify-startup-complete-with-id startup-id)
      (gdk-notify-startup-complete)))

(export 'notify-startup-complete)

(defcfun gdk-get-program-class (:string :free-from-foreign nil))
(defcfun gdk-set-program-class :void
  (program-class (:string :free-to-foreign t)))
(defun program-class () (gdk-get-program-class))
(defun (setf program-class) (new-value) (gdk-set-program-class new-value))
(export 'program-class)

(defcfun (get-display "gdk_get_display") (:string :free-from-foreign nil))
(export 'get-display)

(defcfun gdk-flush :void)
(export 'gdk-flush)

(defcfun (pointer-grab "gdk_pointer_grab") grab-status
  (window (g-object gdk-window))
  (owner-events :boolean)
  (event-mask event-mask)
  (confine-to (g-object gdk-window))
  (cursor (g-boxed-foreign cursor))
  (time :uint32))

(export 'pointer-grab)

(defcfun (pointer-ungrab "gdk_pointer_ungrab") :void
  (time :uint32))

(export 'pointer-ungrab)

(defcfun (pointer-grabbed-p "gdk_pointer_is_grabbed") :boolean)

(export 'pointer-grabbed-p)

(defcfun (keyboard-grab "gdk_keyboard_grab") grab-status
  (window (g-object gdk-window))
  (owner-events :boolean)
  (time :uint32))

(export 'keyboard-grab)

(defcfun (keyboard-ungrab "gdk_keyboard_ungrab") :void
  (time :uint32))

(export 'keyboard-ungrab)

(defcfun gdk-beep :void)

(export 'gdk-beep)

(defcfun gdk-error-trap-push :void)
(defcfun gdk-error-trap-pop :int)
(export '(gdk-error-trap-push gdk-error-trap-pop))
