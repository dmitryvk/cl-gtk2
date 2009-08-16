(in-package :gobject)

(define-g-boxed-cstruct rectangle "GdkRectangle"
  (left :int :initform 0)
  (top :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(at-init () (eval (type-initializer-call "gdk_rectangle_get_type")))

(define-g-boxed-cstruct point nil
  (x :int :initform 0)
  (y :int :initform 0))

(defun mem-copy (source destination count)
  (iter (for i from 0 below count)
        (setf (mem-aref destination :uchar i)
              (mem-aref source :uchar i))))

(defmethod boxed-copy-fn ((type-info (eql (get 'point 'g-boxed-foreign-info))) native)
  (let ((native-copy (foreign-alloc (generated-cstruct-name (g-boxed-info-name type-info)))))
    (mem-copy native native-copy (foreign-type-size (generated-cstruct-name (g-boxed-info-name type-info))))
    native-copy))

(defmethod boxed-free-fn ((type-info (eql (get 'point 'g-boxed-foreign-info))) native)
  (foreign-free native))

(defcallback make-rect-cb (g-boxed-foreign rectangle :return)
    ((a (g-boxed-foreign point)) (b (g-boxed-foreign point)))
  (make-rectangle :left (min (point-x a) (point-x b))
                  :top (min (point-y a) (point-y b))
                  :width (abs (- (point-x a) (point-x b)))
                  :height (abs (- (point-y a) (point-y b)))))

(defun call-make-rect-cb (a b)
  (foreign-funcall-pointer (callback make-rect-cb) ()
                           (g-boxed-foreign point) a
                           (g-boxed-foreign point) b
                           (g-boxed-foreign rectangle :return)))

(define-g-boxed-cstruct vector4 nil
  (coords :double :count 4 :initform (vector 0d0 0d0 0d0 0d0)))

(define-g-boxed-cstruct segment nil
  (a point :inline t :initform (make-point))
  (b point :inline t :initform (make-point)))

(define-g-boxed-variant-cstruct var-segment nil
  (deep :boolean :initform t)
  (a point :inline t :initform (make-point))
  (b point :inline t :initform (make-point))
  (:variant deep
            (t deep-segment
               (depth point :inline t :initform (make-point)))))

(define-g-boxed-variant-cstruct event nil
  (type :int :initform 0)
  (time :int :initform 0)
  (:variant type
            (0 zero-event
               (x :int :initform 0))
            (1 one-event
               (x :double :initform 0.0d0))
            (2 three-event
               (three-type :int :initform 0)
               (:variant three-type
                         (1 three-one-event
                            (y :uchar :initform 0))
                         (2 three-two-event
                            (z :double :initform 0.0d0))
                         (3 segment-event
                            (segment segment :inline t :initform (make-segment)))))))

(defcallback copy-event-cb (g-boxed-foreign event :return)
    ((event (g-boxed-foreign event)))
  (let ((new-event (copy-event event)))
    (incf (event-time new-event) (random 100))
    new-event))

(defun call-copy-event (e)
  (foreign-funcall-pointer (callback copy-event-cb) ()
                           (g-boxed-foreign event) e
                           (g-boxed-foreign event :return)))

