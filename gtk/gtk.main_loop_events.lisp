(in-package :gtk)

(defcfun gtk-init-check :boolean
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

(defun gtk-init ()
  (gtk-init-check (foreign-alloc :int :initial-element 0)
                  (foreign-alloc :string :initial-contents '("/usr/bin/sbcl")))
  #+ (and sbcl (not win32))
  (sb-unix::enable-interrupt sb-unix:sigpipe #'sb-unix::sigpipe-handler)
  #+nil(with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (mem-ref argc :int) 0
          (mem-ref argv '(:pointer :string)) (foreign-alloc :string :count 1
                                                            :initial-element "/usr/bin/sbcl"))
    (unwind-protect
         (unless (gtk-init-check argc argv)
           (error "Cannot initialize Gtk+"))
      (foreign-free (mem-ref argv '(:pointer :string))))))

(at-init () (gtk-init))

(defcfun (%gtk-main "gtk_main") :void)

(defun gtk-main ()
  (with-gdk-threads-lock (%gtk-main)))

#+thread-support
(progn
  (defvar *main-thread* nil)
  (defvar *main-thread-level* nil)
  (defvar *main-thread-lock* (bt:make-lock "*main-thread* lock"))

  (at-finalize ()
    (when (and *main-thread* (bt:thread-alive-p *main-thread*))
      (bt:destroy-thread *main-thread*)
      (setf *main-thread* nil)))

  (defun ensure-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
        (setf *main-thread* nil))
      (unless *main-thread*
        (setf *main-thread* (bt:make-thread (lambda () (gtk-main)) :name "cl-gtk2 main thread")
              *main-thread-level* 0))
      (incf *main-thread-level*))
    (values))

  (defun join-gtk-main ()
    (when *main-thread*
      (bt:join-thread *main-thread*)))

  (defun leave-gtk-main ()
    (bt:with-lock-held (*main-thread-lock*)
      (decf *main-thread-level*)
      (when (zerop *main-thread-level*)
        (gtk-main-quit)))))

#-thread-support
(progn
  (defun ensure-gtk-main ()
    (gtk-main)
    (values))

  (defun leave-gtk-main ()
    (gtk-main-quit))
  
  (defun join-gtk-main ()))

(export 'ensure-gtk-main)

(export 'leave-gtk-main)

(export 'join-gtk-main)

(defcfun gtk-main-level :uint)

(defcfun gtk-main-quit :void)

(defcfun gtk-grab-add :void
  (widget g-object))

(defcfun gtk-grab-get-current g-object)

(defcfun gtk-grab-remove :void
  (widget g-object))