(defpackage #:cl-gtk2-cairo-demo
  (:shadowing-import-from #:cl-cairo2 #:scale)
  (:use :cl #:gtk #:cl-cairo2 #:cl-gtk2-cairo #:iter)
  (:export #:demo))

(in-package #:cl-gtk2-cairo-demo)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass cairo-w (drawing-area)
    ((draw-fn :initform 'draw-clock-face :accessor cairo-w-draw-fn))
    (:metaclass gobject:gobject-class)))

(defmethod initialize-instance :after ((w cairo-w) &rest initargs)
  (declare (ignore initargs))
  (gobject:connect-signal w "configure-event" (lambda (widget event)
                                                (declare (ignore event))
                                                (widget-queue-draw widget)))
  (gobject:connect-signal w "expose-event" (lambda (widget event)
                                             (declare (ignore event))
                                             (cc-expose widget))))

(defmethod (setf cairo-w-draw-fn) :after (new-value (w cairo-w))
  (declare (ignore new-value))
  (widget-queue-draw w))

(defun cc-expose (widget)
  (multiple-value-bind (w h) (gdk:drawable-get-size (widget-window widget))
    (with-gdk-context (ctx (widget-window widget))
      (with-context (ctx)
        (funcall (cairo-w-draw-fn widget) w h)
        nil))))

(defstruct cairo-fn name fn)

(defun starts-with (str prefix)
  (string= str prefix :end1 (min (length str) (length prefix))))

(defun get-draw-fns ()
  (iter (for symbol in-package '#:cl-gtk2-cairo-demo)
        (when (and (fboundp symbol)
                   (starts-with (symbol-name symbol) "DRAW-"))
          (for doc = (or (documentation (fdefinition symbol) t) (let ((*print-case* :downcase)) (format nil "~A" symbol))))
          (collect (make-cairo-fn :name doc :fn symbol)))))

(defun demo ()
  (within-main-loop
    (let ((cb-list (make-instance 'array-list-store)))
      (store-add-column cb-list gobject:+g-type-string+ #'cairo-fn-name)
      (iter (for fn in (get-draw-fns))
            (store-add-item cb-list fn))
      (let-ui (gtk-window
               :var w
               :default-width 300
               :default-height 400
               :type :toplevel
               :title "Cairo drawing"
               (v-box
                (combo-box :var combo :model cb-list) :expand nil
                (cairo-w :var cw)))
        (let ((renderer (make-instance 'cell-renderer-text :text "A text")))
          (cell-layout-pack-start combo renderer)
          (cell-layout-add-attribute combo renderer "text" 0))
        (gobject:connect-signal combo "changed"
                                (lambda (widget)
                                  (declare (ignore widget))
                                  (let ((iter (combo-box-active-iter combo)))
                                    (when iter
                                      (setf (cairo-w-draw-fn cw)
                                            (cairo-fn-fn (tree-model-item cb-list iter)))))))
        (setf (combo-box-active-iter combo) (tree-model-iter-first cb-list))
        (widget-show w)))))

(defun draw-clock-face (w h)
  "Draw a clock face"
  (set-line-width 1)
  (translate (/ w 2) (/ h 2))
  (setf w (- w 2) h (- h 2))
  (scale (* 0.99 (/ (min w h) 2)) (* 0.99 (/ (min w h) 2)))
  (set-line-width 0.01)

  ;; Circle
  (arc 0 0 1 0 (* 2 pi))
  (set-source-rgb 1 1 1)
  (fill-preserve)
  (set-source-rgb 0 0 0)
  (stroke)
        
  ;; Ticks
  (iter (for i from 0 below 12)
        (for angle = (/ (* i pi) 6))
        (for cos = (cos angle))
        (for sin = (sin angle))
        (save)
        (if (zerop (mod i 3))
            (progn (set-line-width 0.02)
                   (move-to (* 0.8 cos) (* 0.8 sin)))
            (move-to (* 0.9 cos) (* 0.9 sin)))
        (line-to cos sin)
        (set-source-rgb 0 0 0)
        (stroke)
        (restore)))

(defun draw-line (w h)
  "Draw simple diagonal line"
  (set-line-width 1)
  (move-to 0 0)
  (line-to w h)
  (set-source-rgb 1 1 1)
  (stroke))

(defun draw-ex-1 (w h)
  "White diagonal line on a blue background"
  (set-source-rgb 0.2 0.2 1)
  (rectangle 0 0 w h)
  (fill-path)
  
  (move-to w 0)
  (line-to 0 h)
  (set-source-rgb 1 1 1)
  (set-line-width 5)
  (stroke))

(defun draw-text (w h)
  "Very simple text example"
  (declare (ignore w h))
  (move-to 0 100)
  (set-font-size 50)
  (show-text "foo. Привет мир!"))

(defparameter *lis-a* 9)
(defparameter *lis-b* 8)
(defparameter *lis-delta* (/ pi 2))
(defparameter *lis-density* 2000)
(defparameter *lis-margin* 10)

(defun draw-lissajou (w h)
  "Draw Lissajous curve"
  (rectangle 0 0 w h)
  (set-source-rgb 0.9 0.9 1)
  (fill-path)
  
  (labels ((stretch (s x)
             (+ (* (1+ x)
                   (- (/ s 2)
                      *lis-margin*))
                *lis-margin*)))
    (move-to (stretch w (sin *lis-delta*)) (stretch h 0))
    (dotimes (i *lis-density*)
      (let* ((v (/ (* i pi 2) *lis-density*))
             (x (sin (+ (* *lis-a* v) *lis-delta*)))
             (y (sin (* *lis-b* v))))
        (line-to (stretch w x) (stretch h y)))))
  (close-path)
  (set-line-width 0.5)
  (set-source-rgb 0 0 1)
  (stroke))

(defun heart (alpha)
  "Draw a heart with fixed size and the given transparency alpha.
  Heart is upside down."
  (let ((radius (sqrt 0.5)))
    (move-to 0 -2)
    (line-to 1 -1)
    (arc 0.5 -0.5 radius (deg-to-rad -45) (deg-to-rad 135))
    (arc -0.5 -0.5 radius (deg-to-rad 45) (deg-to-rad 215))
    (close-path)
    (set-source-rgba 1 0 0 alpha)
    (fill-path)))

(defvar *heart-max-angle* 40d0)

(defun draw-heart (w h)
  "Draw a lot of hearts"
  (rectangle 0 0 w h)
  (set-source-rgb 1 1 1)
  (fill-path)

  (dotimes (i 200)
    (let ((scaling (+ 5d0 (random 40d0))))
      (reset-trans-matrix)              ; reset matrix
      (translate (random w) (random h)) ; move the origin
      (scale scaling scaling)           ; scale
      (rotate (deg-to-rad (- (random (* 2 *heart-max-angle*))
                             *heart-max-angle* 180))) ; rotate
      (heart (+ 0.1 (random 0.7))))))

(defun draw-gradient (w h)
  "Draw a gradient"
  (with-linear-pattern rainbow (0 0 w h)
      `((0 (0.7 0 0.7 0))               ;rgb(a) color as list
        (1/6 ,cl-colors:+blue+)         ;color as cl-color
        (2/6 ,cl-colors:+green+)
        (3/6 ,cl-colors:+yellow+)
        (4/6 ,cl-colors:+orange+)
        (5/6 ,cl-colors:+red+)
        (1 ,cl-colors:+violetred+))
    (rectangle 0 0 w h)
    (set-source rainbow)
    (fill-path)))
