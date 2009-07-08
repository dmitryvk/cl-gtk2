(in-package :gobject)

(defclass g-initially-unowned (g-object)
  ()
  (:metaclass gobject-class)
  (:g-type-name . "GInitiallyUnowned")
  (:documentation "Base class that has initial \"floating\" reference."))
