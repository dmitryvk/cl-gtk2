(in-package :gtk)

(defcfun (drag-finish "gtk_drag_finish") :void
  (context (g-object drag-context))
  (success :boolean)
  (del :boolean)
  (time :uint32))

(export 'drag-finish)

