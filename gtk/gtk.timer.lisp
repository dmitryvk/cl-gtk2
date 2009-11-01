(in-package :gtk)

(defclass timer ()
  ((fn :initform nil :initarg :fn :accessor timer-fn)
   (interval-msec :initform 100 :initarg :interval-msec :accessor timer-interval-msec)
   (source-id :initform nil)))

(defun timer-enabled-p (timer)
  (not (null (slot-value timer 'source-id))))

(defun (setf timer-enabled-p) (new-value timer)
  (unless (eq new-value (timer-enabled-p timer))
    (if new-value
        (start-timer timer)
        (stop-timer timer))))

(defmethod (setf timer-interval-msec) :after (new-value (timer timer))
  (when (timer-enabled-p timer)
    (stop-timer timer)
    (start-timer timer)))

(defun start-timer (timer)
  (unless (slot-value timer 'source-id)
    (setf (slot-value timer 'source-id)
          (gtk-main-add-timeout (timer-interval-msec timer) (lambda () (funcall (timer-fn timer)) t)))))

(defun stop-timer (timer)
  (when (slot-value timer 'source-id)
    (glib:g-source-remove (slot-value timer 'source-id))
    (setf (slot-value timer 'source-id) nil)))

(export '(timer timer-fn timer-interval-msec timer-enabled-p timer-interval-msec start-timer stop-timer))
