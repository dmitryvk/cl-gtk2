(in-package :glib)

;; A type that it almost like :string but uses g_malloc and g_free

(define-foreign-type g-string-type ()
  ((free-from-foreign :initarg :fff :reader g-string-type-fff :initform nil)
   (free-to-foreign :initarg :ftf :reader g-string-type-ftf :initform t))
  (:actual-type :pointer))

(define-parse-method g-string (&key (free-from-foreign nil) (free-to-foreign t))
  (make-instance 'g-string-type :fff free-from-foreign :ftf free-to-foreign))

(defmethod translate-to-foreign (value (type g-string-type))
  (g-strdup value))

(defmethod translate-from-foreign (value (type g-string-type))
  (prog1
      (convert-from-foreign value '(:string :free-from-foreign nil))
    (when (g-string-type-fff type)
      (g-free value))))