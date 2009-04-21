(defpackage :gtk-hello
  (:use :cl :gtk :gobject :glib)
  (:export :run))

(in-package :gtk-hello)

(defun run ()
  (let ((output *standard-output*))
    (with-main-loop
        (let ((window (make-instance 'gtk-window
                                     :type :toplevel
                                     :window-position :center
                                     :title "Hello world!"
                                     :default-width 300
                                     :default-height 100))
              (button (make-instance 'button :label "Hello, world!"))
              (counter 0))
          (g-signal-connect button "clicked"
                            (lambda (b)
                              (declare (ignore b))
                              (format output "Hello, world!~%")
                              (setf (button-label button)
                                    (format nil
                                            "Hello, world! (clicked ~D times)"
                                            (incf counter)))))
          (container-add window button)
          (widget-show window :all t)))))