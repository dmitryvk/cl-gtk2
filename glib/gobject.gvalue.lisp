(in-package :gobject)

(defun g-value-zero (g-value)
  "Initializes the GValue in \"unset\" state.

@arg[g-value]{a C pointer to the GValue structure}"
  (loop
     for i from 0 below (foreign-type-size 'g-value)
     do (setf (mem-ref g-value :uchar i) 0)))

