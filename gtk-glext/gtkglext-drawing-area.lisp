(in-package :gtkglext)

(defclass gl-drawing-area (drawing-area)
  ((on-expose :initarg :on-expose :initform nil :accessor gl-drawing-area-on-expose)
   (on-init :initarg :on-init :initform nil :accessor gl-drawing-area-on-init)
   (on-resize :initarg :on-resize :initform nil :accessor gl-drawing-area-on-resize)
   (realized-p :initform nil :accessor gl-drawing-area-realized-p))
  (:metaclass gobject-class)
  (:g-type-name . "GtkGLDrawingArea"))

(defun resize (widget width height)
  (with-gl-context (widget)
    (if (gl-drawing-area-on-resize widget)
        (funcall (gl-drawing-area-on-resize widget) widget width height)
        (progn
          (gl:viewport 0 0 width height)

          ;; set projection to account for aspect
          (gl:matrix-mode :projection)
          (gl:load-identity)
          (glu:perspective 90 (/ width height) 0.5 20) ; 90 degrees field of view y, clip 0.5-20 z

          ;; set modelview to identity
          (gl:matrix-mode :modelview)
          (gl:load-identity)))))

(defun gl-drawing-area-configure (widget event)
  (declare (ignore event))
  (multiple-value-bind (width height)
      (gdk:drawable-get-size (widget-window widget))
    #+nil(format t "configure ~Dx~D~%" width height)
    (when (gl-drawing-area-realized-p widget)
      (resize widget width height))))

(defun gl-drawing-area-realize (widget)
  #+nil(format t "realize~%")
  (bwhen (init-fn (gl-drawing-area-on-init widget))
    (with-gl-context (widget)
      (funcall init-fn widget)))
  (setf (gl-drawing-area-realized-p widget) t)  
  (multiple-value-bind (width height)
      (gdk:drawable-get-size (widget-window widget))
    (resize widget width height))
  nil)

(defun gl-drawing-area-unrealize (widget)
  (setf (gl-drawing-area-realized-p widget) nil)
  nil)

(defun gl-drawing-area-exposed (widget event)
  (bwhen (draw-fn (gl-drawing-area-on-expose widget))
    (with-gl-context (widget)
      (funcall draw-fn widget event)))
  nil)

(defun gl-drawing-area-parent-set (widget event)
  (declare (ignore event))
  (unless (gtk-widget-set-gl-capability widget
                                        *gl-config*
                                        nil
                                        nil
                                        :rgba-type)
    (warn "set gl capability for ~A (with ~A) failed~%" widget *gl-config*)))

(register-object-type-implementation "GtkGLDrawingArea" gl-drawing-area "GtkDrawingArea" nil nil)

(defmethod initialize-instance :after ((widget gl-drawing-area) &key &allow-other-keys)
  (connect-signal widget "realize" #'gl-drawing-area-realize)
  (connect-signal widget "unrealize" #'gl-drawing-area-unrealize)
  (connect-signal widget "expose-event" #'gl-drawing-area-exposed)
  (connect-signal widget "configure-event" #'gl-drawing-area-configure)
  (connect-signal widget "parent-set" #'gl-drawing-area-parent-set))
