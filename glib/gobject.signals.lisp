(in-package :gobject)

(defcfun g-signal-connect-closure :ulong
  (instance :pointer)
  (detailed-signal :string)
  (closure (:pointer g-closure))
  (after :boolean))