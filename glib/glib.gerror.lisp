(in-package :glib)

(defcstruct g-error
  (:domain g-quark)
  (:code :int)
  (:message (:string :free-from-foreign nil)))

(defcfun g-error-new-literal :pointer
  (domain g-quark)
  (code :int)
  (message :string))

(defcfun g-error-free :void
  (error :pointer))

(defcfun g-error-copy :pointer
  (error :pointer))

(defcfun g-error-matches :boolean
  (error :pointer)
  (domain g-quark)
  (code :int))

(defcfun g-set-error-literal :void
  (err-ptr :pointer)
  (domain g-quark)
  (code :int)
  (message :string))

(defcfun g-propagate-error :void
  (dest-ptr :pointer)
  (src-ptr :pointer))

(defcfun g-clear-error :void
  (err-ptr :pointer))

(define-condition g-error-condition (error)
  ((domain :initarg :domain :initform nil :reader g-error-condition-domain)
   (code :initarg :code :initform nil :reader g-error-condition-code)
   (message :initarg :message :initform nil :reader g-error-condition-message))
  (:report (lambda (e stream)
             (format stream "GError was raised. Domain: ~S, code: ~S, message: ~A"
                     (g-error-condition-domain e)
                     (g-error-condition-code e)
                     (g-error-condition-message e)))))

(defun mayber-raise-g-error-condition (err)
  (unless (null-pointer-p err)
    (error 'g-error-condition
           :domain (foreign-slot-value err 'g-error :domain)
           :code (foreign-slot-value err 'g-error :code)
           :message (foreign-slot-value err 'g-error :message))))

(defmacro with-g-error ((err) &body body)
  `(with-foreign-object (,err :pointer)
     (setf (mem-ref ,err :pointer) (null-pointer))
     (unwind-protect
          (progn ,@body)
       (mayber-raise-g-error-condition (mem-ref ,err :pointer))
       (g-clear-error ,err))))

(defmacro with-catching-to-g-error ((err) &body body)
  `(handler-case
       (progn ,@body)
     (g-error-condition (e)
       (g-set-error-literal ,err
                            (g-error-condition-domain e)
                            (g-error-condition-code e)
                            (g-error-condition-message e)))))

;; void                g_prefix_error                      (GError **err,
;;                                                          const gchar *format,
;;                                                          ...);
;; void                g_propagate_prefixed_error          (GError **dest,
;;                                                          GError *src,
;;                                                          const gchar *format,
;;                                                          ...);