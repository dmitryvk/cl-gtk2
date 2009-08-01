(in-package :gobject)

(define-boxed-variant-cstruct evt "evt"
  (type :int :initform 0)
  (time :uint :initform 0)
  (:variant type
            (0 evt-zero
               (x :double :initform 0.0d0)
               (y :double :initform 0.0d0))
            ((1 2 3) evt-multi
             (t2 :int :initform 0)
             (:variant t2
                       (1 evt-single
                          (item :uchar :initform 0))))))

(defcallback test-evt (g-boxed-foreign evt)
    ((time :int) (e1 (g-boxed-foreign evt)))
  (print time)
  (print e1)
  (when e1
    (incf (evt-time e1) time))
  (make-evt-multi :time time :t2 123))

(defun do-test-evt (e1 time)
  (let ((e2 (foreign-funcall-pointer (callback test-evt) () :int time (g-boxed-foreign evt) e1 (g-boxed-foreign evt))))
    (values e1 e2)))
