(in-package :gtk)

(defcfun (scale-get-layout "gtk_scale_get_layout") g-object
  (scale (g-object scale)))

(export 'scale-get-layout)

(defcfun gtk-scale-get-layout-offsets :void
  (scale (g-object scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun scale-get-layout-offsets (scale)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-scale-get-layout-offsets scale x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'scale-get-layout-offsets)

(defcfun (scale-add-mark "gtk_scale_add_mark") :void
  (scale (g-object scale))
  (value :double)
  (position position-type)
  (markup :string))

(export 'scale-add-mark)

(defcfun (scale-clear-marks "gtk_scale_clear_marks") :void
  (scale (g-object scale)))

(export 'scale-clear-marks)
