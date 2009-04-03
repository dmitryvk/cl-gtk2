(in-package :gtk)

(defun call-within-main-loop-and-wait (fn)
  (let ((lock (bt:make-lock))
        (cv (bt:make-condition-variable))
        result)
    (bt:with-lock-held (lock)
      (within-main-loop
        (setf result (multiple-value-list (funcall fn)))
        (bt:with-lock-held (lock)
          (bt:condition-notify cv)))
      (bt:condition-wait cv lock)
      (values-list result))))

(export 'call-within-main-loop-and-wait)

(defmacro within-main-loop-and-wait (&body body)
  `(call-within-main-loop-and-wait (lambda () ,@body)))

(export 'within-main-loop-and-wait)

(defstruct progress-display parent name count bar time-started current)

(export 'progress-display)
(export 'progress-display-parent)
(export 'progress-display-name)
(export 'progress-display-count)
(export 'progress-display-bar)
(export 'progress-display-time-started)
(export 'progress-display-current)

(defstruct (progress-window (:include progress-display)) window box)

(export 'progress-window)
(export 'progress-window-window)
(export 'progress-window-box)

(defun create-progress-window (name count)
  (within-main-loop-and-wait
    (let* ((window (make-instance 'gtk-window :type :toplevel :title name :window-position :center))
           (box (make-instance 'v-box))
           (bar (make-instance 'progress-bar :text name)))
      (container-add window box)
      (box-pack-start box bar :expand nil)
      (widget-show window)
      (make-progress-window :parent nil :name name :count count :bar bar :window window :box box :time-started (get-internal-real-time) :current 0))))

(defun progress-display-root (progress)
  (if (progress-display-parent progress)
      (progress-display-root (progress-display-parent progress))
      progress))

(defun create-progress-bar (parent name count)
  (assert name) (assert count)
  (if parent
      (within-main-loop-and-wait
        (let* ((root (progress-display-root parent))
               (bar (make-instance 'progress-bar :text name)))
          (box-pack-start (progress-window-box root) bar :expand nil)
          (widget-show bar)
          (make-progress-display :parent parent :name name :count count :bar bar :time-started (get-internal-real-time) :current 0)))
      (create-progress-window name count)))

(export 'create-progress-window)

(defgeneric delete-progress-bar (bar))

(export 'delete-progress-bar)

(defmethod delete-progress-bar ((bar progress-window))
  (within-main-loop-and-wait (object-destroy (progress-window-window bar))))

(defmethod delete-progress-bar ((bar progress-display))
  (let ((root (progress-display-root bar)))
    (within-main-loop-and-wait (container-remove (progress-window-box root) (progress-display-bar bar)))))

(defun update-progress-bar-text (bar &optional (lower-frac 0.0))
  (let* ((elapsed (coerce (/ (- (get-internal-real-time)
                                (progress-display-time-started bar))
                             internal-time-units-per-second)
                          'double-float))
         (process-rate (coerce (/ elapsed (+ lower-frac (progress-display-current bar))) 'double-float))
         (total-time (coerce (* (progress-display-count bar) process-rate) 'double-float)))
    (setf (progress-bar-text (progress-display-bar bar))
          (format nil "~A (~$ of ETA ~$)" (progress-display-name bar) elapsed total-time))))

(defun update-progress-bar-texts (bar &optional (lower-frac 0.0))
  (when bar
    (update-progress-bar-text bar lower-frac)
    (update-progress-bar-texts (progress-display-parent bar) (coerce (/ (progress-display-current bar) (progress-display-count bar)) 'double-float))))

(defun tick-progress-bar (bar)
  (when bar
    (within-main-loop-and-wait
      (incf (progress-bar-fraction (progress-display-bar bar))
            (coerce (/ (progress-display-count bar)) 'double-float))
      (incf (progress-display-current bar))
      (update-progress-bar-text bar))))

(export 'tick-progress-bar)

(defvar *current-progress-bar* nil)

(defmacro with-progress-bar ((name count) &body body)
  (let ((bar (gensym)))
    `(let* ((,bar (create-progress-bar *current-progress-bar* ,name ,count))
            (*current-progress-bar* ,bar))
       (unwind-protect
            (progn ,@body)
         (delete-progress-bar ,bar)))))

(export 'with-progress-bar)

(defmacro with-progress-bar-action (&body body)
  `(multiple-value-prog1 (progn ,@body)
     (tick-progress-bar *current-progress-bar*)))

(export 'with-progress-bar-action)

(defun test-progress ()
  (with-progress-bar ("Snowball" 4)
    (iter (repeat 4)
          (with-progress-bar-action
            (with-progress-bar ("Texts" 10)
              (iter (repeat 10)
                    (with-progress-bar-action (sleep 1))))))))