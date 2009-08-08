(in-package :gobject)

(defun wrap-body-with-boxed-translations (args body)
  (if (null args)
      body
      (let ((arg (first args)))
        (destructuring-bind (arg-name arg-type) arg
          (if (and (listp arg-type) (eq 'g-boxed-foreign (first arg-type)))
              (let ((var (gensym))
                    (cffi-type (cffi::parse-type arg-type)))
                `((let ((,var ,arg-name)
                        (,arg-name (translate-from-foreign ,arg-name ,cffi-type)))
                    (unwind-protect
                         (progn ,@(wrap-body-with-boxed-translations (rest args) body))
                      (cleanup-translated-object-for-callback ,cffi-type ,arg-name ,var)))))
              (wrap-body-with-boxed-translations (rest args) body))))))

(defmacro glib-defcallback (name-and-options return-type args &body body)
  (let* ((c-args (iter (for arg in args)
                       (for (name type) = arg)
                       (if (and (listp type) (eq 'g-boxed-foreign (first type)))
                           (collect `(,name :pointer))
                           (collect arg))))
         (c-body (wrap-body-with-boxed-translations args body)))
    `(defcallback ,name-and-options ,return-type ,c-args
       ,@c-body)))
