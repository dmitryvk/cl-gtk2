(in-package :glib)

(define-foreign-type glist-type ()
  ((type :reader glist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader glist-type-free-from-foreign :initarg :free-from-foreign :initform t)
   (free-to-foreign :reader glist-type-free-to-foreign :initarg :free-to-foreign :initform t))
  (:actual-type :pointer))

(define-parse-method glist (type &key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'glist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defcstruct g-list
  (data :pointer)
  (next :pointer)
  (prev :pointer))

(defcfun g-list-first (:pointer g-list) (list (:pointer g-list)))

(defcfun g-list-free :void (list (:pointer g-list)))

(defun g-list-next (list)
  (if (null-pointer-p list)
      (null-pointer)
      (foreign-slot-value list 'g-list 'next)))

(defmethod translate-from-foreign (pointer (type glist-type))
  (prog1
      (iter (for c initially pointer then (g-list-next c))
            (until (null-pointer-p c))
            (collect (convert-from-foreign (foreign-slot-value c 'g-list 'data) (glist-type-type type))))
    (when (glist-type-free-from-foreign type)
      (g-list-free pointer))))


(define-foreign-type gslist-type ()
  ((type :reader gslist-type-type :initarg :type :initform :pointer)
   (free-from-foreign :reader gslist-type-free-from-foreign :initarg :free-from-foreign :initform t)
   (free-to-foreign :reader gslist-type-free-to-foreign :initarg :free-to-foreign :initform t))
  (:actual-type :pointer))

(define-parse-method gslist (type &key (free-from-foreign t) (free-to-foreign t))
  (make-instance 'gslist-type
                 :type type
                 :free-from-foreign free-from-foreign
                 :free-to-foreign free-to-foreign))

(defcstruct g-slist
  (data :pointer)
  (next :pointer))

(defcfun g-slist-free :void (list (:pointer g-slist)))

(defun g-slist-next (list)
  (if (null-pointer-p list)
      (null-pointer)
      (foreign-slot-value list 'g-slist 'next)))

(defmethod translate-from-foreign (pointer (type gslist-type))
  (prog1
      (iter (for c initially pointer then (g-slist-next c))
            (until (null-pointer-p c))
            (collect (convert-from-foreign (foreign-slot-value c 'g-slist 'data) (gslist-type-type type))))
    (when (gslist-type-free-from-foreign type)
      (g-slist-free pointer))))