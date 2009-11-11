(in-package :gdk)

(defcfun gdk-drop-finish :void
  (context g-object)
  (success :boolean)
  (time :uint32))

(defun drop-finish (context success time)
  (gdk-drop-finish context success time))

(export 'drop-finish)

(defcfun gdk-drag-status :void
  (context g-object)
  (action drag-action)
  (time :uint))

(defun drag-status (context action time)
  (gdk-drag-status context action time))

(export 'drag-status)

