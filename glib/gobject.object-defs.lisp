(in-package :gobject)

(defclass g-initially-unowned (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "GInitiallyUnowned")
  (:g-type-initializer . "g_initially_unowned_get_type")
  (:documentation "Base class that has initial \"floating\" reference."))
