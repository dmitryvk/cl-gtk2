(in-package :gtk)

; TODO: gtk_get_default_language

(defcfun gtk-events-pending :boolean)

(export 'gtk-events-pending)

(defcfun gtk-main-iteration :boolean)

(export 'gtk-main-iteration)

(defcfun gtk-main-iteration-do :boolean
  (blocking :boolean))

(export 'gtk-main-iteration-do)

; TODO: gtk_main_do_event

(defcfun (grab-add "gtk_grab_add") :void
  (widget g-object))

(export 'grab-add)

(defcfun (grab-current "gtk_grab_get_current") g-object)

(export 'grab-current)

(defcfun (grab-remove "gtk_grab_remove") :void
  (widget g-object))

(export 'grab-remove)

; TODO: gtk_quit_add_destroy

; TODO: gtk_quit_add

; TODO: gtk_quit_add_full

; TODO: gtk_quit_remove

; TODO: gtk_quit_remove_by_data

; TODO: gtk_key_snooper_install

; TODO: gtk_key_snooper_remove

(defcfun (current-event "gtk_get_current_event") (g-boxed-foreign event :return))

(export 'current-event)

(defcfun (current-event-time "gtk_get_current_event_time") :uint32)

(export 'current-event-time)

(defcfun (event-widget "gtk_get_event_widget") g-object
  (event (g-boxed-foreign event)))

(export 'event-widget)

(defcfun (propagate-event "gtk_propagate_event") :void
  (widget g-object)
  (event (g-boxed-foreign event)))

(export 'propagate-event)

