(in-package :gtk)

(defcfun (dialog-run "gtk_dialog_run") response-type
  (dialog (g-object dialog)))

(export 'dialog-run)

(defcfun (dialog-response "gtk_dialog_response") :void
  (dialog (g-object dialog))
  (response response-type))

(export 'dialog-response)

(defcfun (dialog-add-button "gtk_dialog_add_button") (g-object widget)
  (dialog (g-object dialog))
  (button-text :string)
  (response response-type))

(export 'dialog-add-button)

(defcfun (dialog-add-action-widget "gtk_dialog_add_action_widget") :void
  (dialog (g-object dialog))
  (child (g-object widget))
  (response response-type))

(export 'dialog-add-action-widget)

(defcfun (dialog-set-response-sensitive "gtk_dialog_set_response_sensitive") :void
  (dialog (g-object dialog))
  (response response-type)
  (setting :boolean))

(export 'dialog-set-response-sensitive)

(defcfun (dialog-response-for-widget "gtk_dialog_get_response_for_widget") :int
  (dialog (g-object dialog))
  (widget (g-object widget)))

(export 'dialog-response-for-widget)

(defcfun (dialog-alternative-button-order-on-screen "gtk_alternative_dialog_button_order") :boolean
  (screen (g-object screen)))

(export 'dialog-alternative-button-order-on-screen)

(defcfun (dialog-set-alternative-button-order-from-array "gtk_dialog_set_alternative_button_order_from_array") :void
  (dialog (g-object dialog))
  (n-params :int)
  (new-order (:pointer response-type)))

(defun set-dialog-alternative-button-order (dialog response-list)
  (with-foreign-object (new-order 'response-type (length response-list))
    (loop
       for i from 0
       for response in response-list
       do (setf (mem-aref new-order 'response-type i) response))
    (dialog-set-alternative-button-order-from-array dialog (length response-list) new-order))
  response-list)

(export 'set-dialog-alternative-button-order)

(defmacro with-gtk-message-error-handler (&body body)
  (let ((dialog (gensym))
        (e (gensym)))
    `(handler-case
         (progn ,@body)
       (error (,e) (using* ((,dialog (make-instance 'message-dialog 
                                                    :message-type :error :buttons :ok
                                                    :text (format nil "Error~%~A~%during execution of~%~A" ,e '(progn ,@body)))))
                           (dialog-run ,dialog)
                           (object-destroy ,dialog)
                           nil)))))

(export 'with-gtk-message-error-handler)
