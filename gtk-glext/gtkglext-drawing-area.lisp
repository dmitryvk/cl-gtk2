(in-package :gtkglext)

(defclass gl-drawing-area (drawing-area)
  ((on-draw :initarg :on-draw :initform nil)
   (on-init :initarg :on-init :initform nil))
  (:metaclass gobject-class)
  (:g-type-name . "GtkGLDrawingArea"))

(defun resize (widget width height)
  (with-gl-context (widget)
    (gl:viewport 0 0 width height)

    ;; set projection to account for aspect
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (glu:perspective 90 (/ width height) 0.5 20) ; 90 degrees field of view y, clip 0.5-20 z

    ;; set modelview to identity
    (gl:matrix-mode :modelview)
    (gl:load-identity)))

(defun gl-drawing-area-configure (widget event)
  (declare (ignore event))
  (multiple-value-bind (width height)
      (gdk:drawable-get-size (widget-window widget))
    #+nil(format t "configure ~Dx~D~%" width height)
    (when (widget-realized-p widget)
      (resize widget width height))))

(defun gl-drawing-area-realize (widget)
  #+nil(format t "realize~%")
  (multiple-value-bind (width height)
      (gdk:drawable-get-size (widget-window widget))
    (resize widget width height))
  (bwhen (init-fn (slot-value widget 'on-init))
	 (funcall init-fn widget))
  nil)

(defun gl-drawing-area-exposed (widget event)
  (bwhen (draw-fn (slot-value widget 'on-draw))
    (funcall draw-fn widget event))
  nil)

(register-object-type-implementation "GtkGLDrawingArea" gl-drawing-area "GtkDrawingArea" nil nil)

(defmethod initialize-instance :after ((widget gl-drawing-area) &key &allow-other-keys)
  (connect-signal widget "realize" #'gl-drawing-area-realize)
  (connect-signal widget "expose-event" #'gl-drawing-area-exposed)
  (connect-signal widget "configure-event" #'gl-drawing-area-configure)
  (connect-signal widget "parent-set" (lambda (widget event)
					(declare (ignore event))
					(at-init () (gl-init))
					(unless (gtk-widget-set-gl-capability widget
                                                                              *gl-config*
                                                                              nil
                                                                              nil
                                                                              :rgba-type)
                                          (warn "set gl capability for ~A (with ~A) failed~%" widget *gl-config*)))))
