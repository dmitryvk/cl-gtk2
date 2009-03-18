(in-package :gtk)

(defcstruct %gtk-object
  (parent-instance gobject::%g-initially-unowned)
  (flags :uint32))

(defun gtk-object-flags-as-integer (object)
  (foreign-slot-value (pointer object) '%gtk-object 'flags))

(defun (setf gtk-object-flags-as-integer) (new-value object)
  (setf (foreign-slot-value (pointer object) '%gtk-object 'flags) new-value))