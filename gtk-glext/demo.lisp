(defpackage :gtk-glext-demo
  (:use :cl :gtk :gtkglext :gobject :glib)
  (:export :run))

(in-package :gtk-glext-demo)

(defvar *theta* 30)

(defun draw (widget event)
  (declare (ignore event))
  (with-gl-context (widget)
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
    (gl:flush)))

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
	  (drawing (make-instance 'gl-drawing-area :on-draw #'draw)))
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
