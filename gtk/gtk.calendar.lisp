(in-package :gtk)

(defcfun (calendar-mark-day "gtk_calendar_mark_day") :boolean
  (calendar g-object)
  (day :uint))

(export 'calendar-mark-day)

(defcfun (calendar-unmark-day "gtk_calendar_unmark_day") :boolean
  (calendar g-object)
  (day :uint))

(export 'calendar-unmark-day)

(defcfun (calendar-clear-marks "gtk_calendar_clear_marks") :void
  (calendar g-object))

(export 'calendar-clear-marks)

(defcallback gtk-calendar-detail-func-callback (g-string :free-to-foreign nil :free-from-foreign nil)
    ((calendar g-object) (year :uint) (month :uint) (day :uint) (data :pointer))
  (restart-case
      (or (funcall (get-stable-pointer-value data)
                   calendar year month day)
          (null-pointer))
    (return-null () (null-pointer))))

(defcfun gtk-calendar-set-detail-func :void
  (calendar g-object)
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun calendar-set-detail-function (calendar function)
  (gtk-calendar-set-detail-func calendar
                                (callback gtk-calendar-detail-func-callback)
                                (allocate-stable-pointer function)
                                (callback stable-pointer-free-destroy-notify-callback)))
