(in-package :gtk)

(define-condition gtk-call-aborted (error)
  ((condition :initarg :condition :reader gtk-call-aborted-condition))
  (:report (lambda (c stream)
             (format stream "Call within main loop aborted because of error:~%~A" (gtk-call-aborted-condition c)))))

(defun call-within-main-loop-and-wait (fn)
  (let ((lock (bt:make-lock))
        (cv (bt:make-condition-variable))
        error
        result)
    (bt:with-lock-held (lock)
      (within-main-loop
        (handler-case
            (setf result (multiple-value-list (funcall fn)))
          (error (e) (setf error e)))
        (bt:with-lock-held (lock)
          (bt:condition-notify cv)))
      (bt:condition-wait cv lock)
      (if error
          (error 'gtk-call-aborted :condition error)
          (values-list result)))))

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

(defun format-duration (stream seconds colon-modifier-p at-sign-modifier-p)
  (declare (ignore colon-modifier-p at-sign-modifier-p))
  (let ((seconds (mod (truncate seconds) 60))
        (minutes (mod (truncate seconds 60) 60))
        (hours (truncate seconds 3600)))
    (format stream "~2,'0D:~2,'0D:~2,'0D" hours minutes seconds)))

(defun update-progress-bar-text (bar &optional (lower-frac 0.0))
  (let* ((elapsed (coerce (/ (- (get-internal-real-time)
                                (progress-display-time-started bar))
                             internal-time-units-per-second)
                          'double-float))
         (process-rate (coerce (/ elapsed (+ lower-frac (progress-display-current bar))) 'double-float))
         (total-time (coerce (* (progress-display-count bar) process-rate) 'double-float)))
    (setf (progress-bar-text (progress-display-bar bar))
          (format nil "~A (~/gtk::format-duration/; ETA ~/gtk::format-duration/)" (progress-display-name bar) elapsed total-time))))

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
  (with-progress-bar ("Snowball" 10)
    (loop
       repeat 10
       do (with-progress-bar-action
            (with-progress-bar ("Texts" 10)
              (loop
                 repeat 10
                 do (with-progress-bar-action (sleep 1))))))))

(defun show-message (message &key (buttons :ok) (message-type :info) (use-markup nil))
  (let ((dialog (make-instance 'message-dialog
                               :text message
                               :buttons buttons
                               :message-type message-type
                               :use-markup use-markup)))
    (prog1
        (dialog-run dialog)
      (object-destroy dialog))))

(export 'show-message)


;;; Condition Handling with `info-bar's
;;

(defun make-condition-info-bar (context condition &rest actions)
  "Make a `gtk:info-bar' that displays CONDITIONS and contains buttons
according to the specification in ACTIONS. Each element of actions is
of the form
ACTION ::= (TITLE &optional THUNK DESCRIPTION)"
  (bind (((:flet format-escape (thing))
	  (gtk:markup-escape-text (format nil "~A" thing)))
	 ((:flet make-message (context condition))
	  (format nil "<b>~A ~A</b>:~%~A: <i>~A</i>"
		  (%condition->message condition)
		  (format-escape context)
		  (format-escape (type-of condition))
		  (format-escape condition)))
	 (icon  (make-instance
		 'gtk:image
		 :stock     (%condition->stock-id condition)
		 :icon-size 6))
	 (label (make-instance
		 'gtk:label
		 :use-markup t
		 :xalign     0.0
		 :label      (make-message context condition)))
	 (box   (make-instance 'gtk:h-box))
	 (bar   (make-instance
		 'gtk:info-bar
		 :message-type (%condition->message-type condition)))
	 response-map
	 ((:flet make-button (response action))
	  (bind (((title &optional thunk description) action)
		 (button (make-instance 'gtk:button))
		 (label  (make-instance
			  'gtk:label
			  :use-markup t
			  :xalign     (if description 0 .5)
			  :label      (format
				       nil "<b>~@(~A~)</b>~:[~;~:*
<span size='x-small' font='italic'>~A</span>~]"
				       (gtk:markup-escape-text title)
				       (when description
					 (gtk:markup-escape-text description))))))
	    (gtk:container-add button label)
	    (gtk:info-bar-add-action-widget bar button response)
	    (when action
	      (push `(,response . ,thunk) response-map)))))
    ;; Add icon and label to content area.
    (gtk:box-pack-start box icon  :expand nil :fill t)
    (gtk:box-pack-start box label :expand t   :fill t)
    (gtk:container-add (gtk:info-bar-content-area bar) box)

    ;; Add buttons.
    (map nil #'make-button (iota (1+ (length actions)))
	 (or actions `(("Close" ,(lambda ())))))

    ;; Connect response handler.
    (gobject:connect-signal
     bar "response"
     #'(lambda (bar id)
	 ;; First remove BAR from its parent widget.
	 (gtk:container-remove (gtk:widget-parent bar) bar)
	 ;; Then run the selected reponse, if we can find it.
	 (let ((func (cdr (assoc id response-map))))
	   (when func (funcall func)))))

    bar))

(defun show-condition-info-bar (container context condition
				&rest actions)
  "Make a `gtk:info-bar' and show it in CONTAINER.
CONTEXT, CONDITION and ACTIONS are as described for
`make-condition-info-bar'."
  (gtk:within-main-loop
    (gtk:box-pack-end
     container
     (apply #'make-condition-info-bar context condition actions)
     :expand nil :fill t)
    (gtk:widget-show container :all t)))

(defun do-condition-info-bar-blocking (container context condition
				       &rest actions)
  "Show a `gtk:info-bar' using CONTAINER, CONTEXT CONDITION and
ACTIONS as described for `show-condition-info-bar'. However, do not
return immediately, but block until an action has been selected. If
the selection action has a thunk, the value returned by that thunk is
returned."
  (bind (choice
	 (choice-lock      (bt:make-lock "choice-lock"))
	 (choice-condition (bt:make-condition-variable
			    :name "choice-condition"))
	 ((:flet make-proxy-action (action))
	  (bind (((title &optional thunk description) action))
	    `(,title
	      ,(lambda ()
		       (bt:with-lock-held (choice-lock)
			 (setf choice (or thunk (lambda ())))
			 (bt:condition-notify choice-condition)))
	      ,@(when description
		      `(,description)))))
	 (proxy-actions (mapcar #'make-proxy-action actions)))

    (apply #'show-condition-info-bar container context condition proxy-actions)
    (bt:with-lock-held (choice-lock)
      (iter (until choice)
	    (bt:condition-wait choice-condition choice-lock)))
    (funcall choice)))

(defun do-condition-info-bar-non-blocking (container context condition
					   &rest actions)
  "Show a `gtk:info-bar' using CONTAINER, CONTEXT, CONDITION and
ACTIONS as described for `show-condition-info-bar'. Return immediately
without any values."
  (apply #'show-condition-info-bar container context condition actions))

(defun do-condition-info-bar (container context condition block?
			      &rest actions)
  "Show a `gtk:info-bar' using CONTAINER, CONTEXT, CONDITION and
ACTIONS as described for `show-condition-info-bar'. Depending on
BLOCK?, either use `do-condition-info-bar-blocking' or
`do-condition-info-bar-non-blocking'."
  (apply
   (if block?
       #'do-condition-info-bar-blocking
       #'do-condition-info-bar-non-blocking)
   container context condition actions))

(defun handle-condition-with-info-bar (container context condition)
  "Show a `gtk:info-bar' using CONTAINER, CONTEXT and CONDITION.
The info bar contains one action button for each restart applicable to
CONDITION. Pressing a button invokes the associated restart. This
function blocks until a restart has been invoked."
  (apply #'do-condition-info-bar-blocking
   container context condition
   (map 'list #'%restart->action (compute-restarts condition))))

(defun %restart->action (restart)
  "Return an action specification that contains a description of
RESTART and a thunk invoking RESTART."
  `(,(string (restart-name restart))
    ,(lambda ()
       (invoke-restart restart))
    ,(format nil "~A" restart)))

(defmacro with-errors-to-info-bar ((container context) &body body)
  "Execute BODY with a handler installed that handles all conditions
using an `gtk:info-bar' with `handle-condition-with-info-bar'. For the
meaning of CONTAINER and CONTEXT, see
`handle-condition-with-info-bar'."
  `(handler-case
       (progn ,@body)
     (error (condition)
       (handle-condition-with-info-bar
	,container ,context condition))))

(export 'make-condition-info-bar)
(export 'show-condition-info-bar)
(export 'do-condition-info-bar)
(export 'handle-condition-with-info-bar)
(export 'with-errors-to-info-bar)


;;; Utility Functions
;;

(defun %condition->message (condition)
  "Return appropriate message text for CONDITION. "
  (etypecase condition
    (error   "Error in")
    (warning "Warning in")
    (t       "Info from")))

(defun %condition->stock-id (condition)
  "Return appropriate stock id for CONDITION."
  (etypecase condition
    (error   "gtk-dialog-error")
    (warning "gtk-dialog-warning")
    (t       "gtk-dialog-info")))

(defun %condition->message-type (condition)
  "Return appropriate message type for CONDITION."
  (etypecase condition
    (error   :error)
    (warning :warning)
    (t       :info)))


;;; Example
;;

#+example
(gtk:within-main-loop
  (let ((w (make-instance 'gtk:gtk-window))
	(b (make-condition-info-bar
	    :bla (make-instance 'simple-error
				:format-control "Something went wrong")
	    '("FIX-IT" ,(lambda () (format t "fixing it~%")) "Try fixing the problem by foobling to nooble")
	    '("IGNORE" nil "Ignore the problem"))))
    (gobject:connect-signal w "unmap" #'(lambda (widget) (gtk:gtk-main-quit)))
    (gtk:container-add w b)
    (gtk:widget-show w :all t)))

#+example
(progn
  (let ((b))
    (gtk:within-main-loop-and-wait
      (let ((w (make-instance 'gtk:gtk-window)))
	(setf b (make-instance 'gtk:v-box))
	(gobject:connect-signal w "unmap" #'(lambda (widget) (gtk:gtk-main-quit)))
	(gtk:container-add w b)
	(gtk:widget-show w :all t)))
    (handle-condition-with-info-bar-blocking
     b :bla (make-instance 'simple-error
			   :format-control "Something went wrong")
     `("FIX-IT" ,(lambda () (format t "fixing it~%")) "Try fixing the problem by foobling to nooble")
     '("IGNORE" nil "Ingore the problem"))))
