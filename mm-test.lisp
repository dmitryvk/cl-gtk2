(eval-when (:load-toplevel :compile-toplevel :execute)
  (require :fiveam))

(defpackage :mm-test
  (:use :cl :gtk :glib :gobject :iter :tg :5am))

(in-package :mm-test)

(defun get-object (ptr)
  (when (cffi:pointerp ptr) (setf ptr (cffi:pointer-address ptr)))
  (or (gethash ptr gobject::*foreign-gobjects-strong*)
      (gethash ptr gobject::*foreign-gobjects-weak*)))

(defun do-gc ()
  (gc :full t)
  (gobject::activate-gc-hooks)
  (gethash 0 gobject::*foreign-gobjects-strong*)
  (gobject::activate-gc-hooks)
  (gc :full t)
  (gethash 0 gobject::*foreign-gobjects-weak*)
  (gobject::activate-gc-hooks)
  (gc :full t))

(defun object-handlers (object)
  (when object
    (remove nil (gobject::g-object-signal-handlers object))))

(defun print-refs-table (table &optional (stream *standard-output*))
  (iter (for (ptr object) in-hashtable table)
        (format stream "~A => ~A (~A refs~@[~*, floating~])~@[ handlers: ~A~]~%"
                ptr object (gobject::ref-count object)
                (gobject.ffi:g-object-is-floating (cffi:make-pointer ptr))
                (object-handlers object))))

(defun print-refs (&optional (stream *standard-output*))
  (format stream "Strong:~%")
  (print-refs-table gobject::*foreign-gobjects-strong*)
  (format stream "Weak:~%")
  (print-refs-table gobject::*foreign-gobjects-weak*))

(defun count-refs ()
  (+ (hash-table-count gobject::*foreign-gobjects-strong*)
     (hash-table-count gobject::*foreign-gobjects-weak*)))

(defun print-sps (&optional (stream *standard-output*))
  (iter (initially (format stream "Stable pointers:~%"))
        (for v in-vector gobject::*registered-stable-pointers*)
        (for i from 0)
        (when v
          (format stream "~A => ~A~%" i v))
        (finally (format stream "~%"))))

(defun print-hooks (&optional (stream *standard-output*))
  (format stream "~A~%" gobject::*gobject-gc-hooks*))

(defun delete-refs ()
  (maphash (lambda (key value)
             (declare (ignore value))
             (remhash key gobject::*foreign-gobjects-strong*))
           gobject::*foreign-gobjects-strong*)
  (maphash (lambda (key value)
             (declare (ignore value))
             (remhash key gobject::*foreign-gobjects-weak*))
           gobject::*foreign-gobjects-weak*))

(when nil (defvar *builder* (make-instance 'builder :from-string
                                  "
<interface>
  <object class=\"GtkDialog\" id=\"dialog1\">
  </object>
</interface>
")))

(setf gobject::*debug-stream* *standard-output*
      gobject::*debug-gc* t
      gobject::*debug-subclass* t)

(defclass my-button (gtk:button) () (:metaclass gobject-class))

(def-suite mm-tests)

(defun run-all-tests ()
  (run! 'mm-tests))

(in-suite mm-tests)

(defmacro with-gc-same-counting (&body body)
  (let ((count (gensym)))
    (multiple-value-bind (body gc-count)
        (if (integerp (first body))
            (values (rest body) (first body))
            (values body 1))
      `(progn
         (gc :full t)
         (gobject::activate-gc-hooks)
         (count-refs)
         (let ((,count (count-refs)))
           (funcall (lambda () ,@body))
           (iter (repeat ,gc-count)
                 (format t "gc'ing~%")
                 (gc :full t)
                 (gobject::activate-gc-hooks)
                 (count-refs))
           (is (= ,count (count-refs))))))))

(test test-1
  (with-gc-same-counting
    2
    (make-instance 'my-button)))

(test test-with-signal
  (with-gc-same-counting
    2
    (let ((b (make-instance 'my-button)))
      (connect-signal b "clicked" (lambda (bb) (declare (ignore bb)) (print b)))
      nil)))

(test test-repassing
  (with-gc-same-counting
    2
    (let ((b (make-instance 'my-button)))
      (cffi:convert-from-foreign (pointer b) 'g-object)
      nil)))

(test test-builder
  (with-gc-same-counting
    5
    (let ((b (make-instance 'builder :from-string "<interface>
  <object class=\"GtkButton\" id=\"button1\">
  </object>
</interface>")))
      (builder-get-object b "button1")
      (gc :full t)
      (gobject::activate-gc-hooks))
    nil))

(test test-builder-with-signals
  (with-gc-same-counting
    6
    (let ((b (make-instance 'builder :from-string "<interface>
  <object class=\"GtkButton\" id=\"button1\">
  </object>
</interface>")))
      (let ((btn (builder-get-object b "button1")))
        (connect-signal btn "clicked" (lambda (bb) (declare (ignore bb)) (print btn))))
      (gc :full t)
      (gobject::activate-gc-hooks))
    nil))

(defun make-builder (&optional return)
  (let* ((builder (make-instance 'gtk:builder
                                 :from-file (namestring (merge-pathnames "demo/demo1.ui" gtk-demo::*src-location*))))
         (text-view (builder-get-object builder "textview1"))
         (window (builder-get-object builder "window1")))
    (builder-connect-signals-simple
     builder
     `(("quit_cb"
        ,(lambda (&rest args)
                 (print args)
                 (object-destroy window)))))
    (when return builder)))
