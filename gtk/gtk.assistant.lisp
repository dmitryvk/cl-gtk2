(in-package :gtk)

(defcfun (assistant-nth-page "gtk_assistant_get_nth_page") (g-object widget)
  (assistant (g-object assistant))
  (page-num :int))

(export 'assistant-nth-page)

(defcfun (assistant-append-page "gtk_assistant_append_page") :int
  (assistant (g-object assistant))
  (page (g-object widget)))

(export 'assistant-append-page)

(defcfun (assistant-prepend-page "gtk_assistant_prepend_page") :int
  (assistant (g-object assistant))
  (page (g-object widget)))

(export 'assistant-prepend-page)

(defcfun (assistant-insert-page "gtk_assistant_insert_page") :int
  (assistant (g-object assistant))
  (page (g-object widget))
  (position :int))

(export 'assistant-insert-page)

(defcstruct assistant-page-func-ref
  (object :pointer)
  (fn-id :int))

(defcallback assistant-page-func-cb :int
    ((current-page :int) (data :pointer))
  (let* ((object (convert-from-foreign (foreign-slot-value data 'assistant-page-func-ref 'object) '(g-object assistant)))
         (fn-id (foreign-slot-value data 'assistant-page-func-ref 'fn-id))
         (fn (retrieve-handler-from-object object fn-id)))
    (funcall fn current-page)))

(defcallback assistant-page-func-destroy-notify-cb :void
    ((data :pointer))
  (let* ((object (convert-from-foreign (foreign-slot-value data 'assistant-page-func-ref 'object) '(g-object assistant)))
         (fn-id (foreign-slot-value data 'assistant-page-func-ref 'fn-id)))
    (delete-handler-from-object object fn-id)))

(defcfun gtk-assistant-set-forward-page-func :void
  (assistant (g-object assistant))
  (page-func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun set-assistant-forward-page-function (assistant function)
  (if function
      (let ((ref (foreign-alloc 'assistant-page-func-ref))
            (fn-id (save-handler-to-object assistant function)))
        (setf (foreign-slot-value ref 'assistant-page-func-ref 'object)
              (pointer assistant)
              (foreign-slot-value ref 'assistant-page-func-ref 'fn-id)
              fn-id)
        (gtk-assistant-set-forward-page-func assistant (callback assistant-page-func-cb)
                                             ref (callback assistant-page-func-destroy-notify-cb)))
      (gtk-assistant-set-forward-page-func assistant (null-pointer) (null-pointer) (null-pointer))))

(defcfun (assistant-add-action-widget "gtk_assistant_add_action_widget") :void
  (assistant (g-object assistant))
  (widget (g-object widget)))

(export 'assistant-add-action-widget)

(defcfun (assistant-remove-action-widget "gtk_assistant_remove_action_widget") :void
  (assistant (g-object assistant))
  (widget (g-object widget)))

(export 'assistant-remove-action-widget)

(defcfun (assistant-update-buttons-state "gtk_assistant_update_buttons_state") :void
  (assistant (g-object assistant)))

(export 'assistant-update-buttons-state)


