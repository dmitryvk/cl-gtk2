(in-package :glib)

(defcfun g-get-user-cache-dir :string)

(defun get-user-cache-dir ()
  (g-get-user-cache-dir))

(export 'get-user-cache-dir)

(defcfun g-get-user-data-dir :string)

(defun get-user-data-dir ()
  (g-get-user-data-dir))

(export 'get-user-data-dir)

(defcfun g-get-user-config-dir :string)

(defun get-user-config-dir ()
  (g-get-user-config-dir))

(export 'get-user-config-dir)

(defcfun g-build-filenamev (:string :free-from-foreign t)
  (args :pointer))

(defun build-filename (&rest args)
  (let* ((n (length args))
         (arr (g-malloc (* (1+ n) (foreign-type-size :pointer)))))

    (iter (for i from 0)
          (for arg in args)
          (setf (mem-aref arr :pointer i) (g-strdup arg)))
    (setf (mem-aref arr :pointer n) (null-pointer))

    (prog1
      (g-build-filenamev arr)

      (iter (for i from 0)
            (for str-ptr = (mem-aref arr :pointer i))
            (until (null-pointer-p str-ptr))
            (g-free str-ptr))
      (g-free arr))))

(export 'build-filename)

