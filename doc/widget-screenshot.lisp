(defpackage :widget-screenshot
  (:use :cl :gtk :gdk :gobject)
  (:export :make-widget-screenshot))

(in-package :widget-screenshot)

(defun make-widget-screenshot (filename widget-creator-fn)
  (within-main-loop
    (let ((window (make-instance 'gtk-window))
          (widget (funcall widget-creator-fn)))
      (container-add window widget)
      (pushnew :structure-mask (gdk-window-events (widget-window window)))
      (connect-signal window "map-event"
                      (lambda (&rest args)
                        (declare (ignore args))
                        (let* ((pm (widget-snapshot widget))
                               (pb (pixbuf-get-from-drawable nil pm)))
                          (pixbuf-save pb filename "png"))
                        (object-destroy window)))
      (widget-show window))))
