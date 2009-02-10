(in-package :gtk)

(define-g-boxed-class "GtkBorder" border ()
  (left :int :initform 0)
  (right :int :initform 0)
  (top :int :initform 0)
  (bottom :int :initform 0))