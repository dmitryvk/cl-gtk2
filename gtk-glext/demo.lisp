(defpackage :gtk-glext-demo
  (:use :cl :gtk :gtkglext :gobject :glib)
  (:export :run
           #:planet))

(in-package :gtk-glext-demo)

(defvar *theta* 30)

(defun draw (widget event)
  (declare (ignore widget event))
  (gl:clear-color 0 0 0 0)
  (gl:cull-face :back)
  (gl:depth-func :less)
  (gl:disable :dither)
  (gl:shade-model :smooth)
  (gl:light-model :light-model-local-viewer 1)
  (gl:color-material :front :ambient-and-diffuse)
  (gl:enable :light0 :lighting :cull-face :depth-test)
  (gl:load-identity)
  (gl:translate 0 0 -5)
  (gl:rotate *theta* 1 1 0)
  (gl:light :light0 :position '(0 1 1 0))
  (gl:light :light0 :diffuse '(0.2 0.4 0.6 0))
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:color 1 1 1)
  (gl:front-face :cw)
  (glut:solid-teapot 1.5)
  (gl:front-face :ccw)
  (gl:flush))

(defun run ()
  (with-main-loop
    (setf *theta* 30)
    (let ((window (make-instance 'gtk-window
				 :type :toplevel
				 :window-position :center
				 :title "Hello world!"
				 :default-width 320
				 :default-height 240))
          (v-box (make-instance 'v-box))
          (label (make-instance 'label :label "Click me!"))
	  (drawing (make-instance 'gl-drawing-area :on-expose #'draw)))
      (box-pack-start v-box drawing)
      (box-pack-start v-box label :expand nil)
      (container-add window v-box)
      (let ((source-id (gtk-main-add-timeout 100 (lambda ()
                                                  (setf *theta*
                                                        (mod (+ *theta* 0.5) 360))
                                                  (widget-queue-draw drawing)
                                                  (setf (label-label label)
                                                        (format nil "Theta = ~A" *theta*))
                                                  t))))
        (connect-signal window "delete-event" (lambda (w e)
                                                (declare (ignore w e))
                                                (g-source-remove source-id)
                                                nil)))
      (widget-show window :all t))))

(defvar *d* 0)
(defvar *y* 0)

(defun planet ()
  (with-main-loop
    (setf *d* 0 *y* 0)
    (let ((window (make-instance 'gtk-window
                                 :window-position :center
                                 :title "Planets"
                                 :default-width 500
                                 :default-height 500))
          (area (make-instance 'gl-drawing-area :on-expose #'planet-draw :on-resize #'planet-resize)))
      (container-add window area)
      (pushnew :key-press-mask (gdk:gdk-window-events (widget-window window)))
      (connect-signal window "key-press-event"
                      (lambda (w e)
                        (declare (ignore w))
                        (ignore-errors
                          (let ((c (aref (gdk:event-key-string e) 0)))
                            (case c
                              (#\d (incf *d* 10) (widget-queue-draw area))
                              (#\D (incf *d* -10) (widget-queue-draw area))
                              (#\y (incf *y* 5) (widget-queue-draw area))
                              (#\Y (incf *y* -5) (widget-queue-draw area)))))
                        nil))
      (let ((timer-id (gtk-main-add-timeout 10 (lambda ()
                                                  (incf *d* 1) (incf *y* 0.5)
                                                  (widget-queue-draw area)
                                                  t))))
        (connect-signal window "delete-event" (lambda (w e)
                                                (declare (ignore w e))
                                                (g-source-remove timer-id)
                                                nil)))
      (widget-show window))))

(defun planet-draw (w e)
  (declare (ignore w e))
  (gl:clear-color 0 0 0 0)
  (gl:shade-model :flat)
  (gl:clear :color-buffer)
  (gl:color 1 1 1)
  (gl:with-pushed-matrix
    ;; draw sun
    (gl:translate 0 0 -2)
    (gl:rotate 30 1 1 0)
    (glut:wire-sphere 1 20 16)
    ;; draw smaller planet
    (gl:rotate *y* 0 1 0)
    (gl:translate 2 0 0)
    (gl:rotate *d* 0 1 0)
    (glut:wire-sphere 0.2 10 8))
  (gl:flush))

(defun planet-resize (w width height)
  (declare (ignore w))
  (gl:viewport 0 0 width height)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (glu:perspective 60 (/ width height) 1 20)
  (gl:matrix-mode :modelview)
  (gl:load-identity)
  (glu:look-at 0 0 5 0 0 0 0 1 0))
