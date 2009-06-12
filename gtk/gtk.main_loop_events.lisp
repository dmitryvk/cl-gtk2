(in-package :gtk)

(defcfun gtk-init-check :boolean
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

(defun gtk-init ()
  (gtk-init-check (foreign-alloc :int :initial-element 0)
                  (foreign-alloc :string :initial-contents '("/usr/bin/sbcl")))
  #+nil(with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (mem-ref argc :int) 0
          (mem-ref argv '(:pointer :string)) (foreign-alloc :string :count 1
                                                            :initial-element "/usr/bin/sbcl"))
    (unwind-protect
         (unless (gtk-init-check argc argv)
           (error "Cannot initialize Gtk+"))
      (foreign-free (mem-ref argv '(:pointer :string))))))

(gtk-init)

(defcfun gtk-test-register-all-types :void)

(gtk-test-register-all-types)

(defcfun gtk-events-pending :boolean)

(defcfun gtk-main :void)

#+thread-support
(defvar *main-thread* nil)

#+thread-support
(defun ensure-gtk-main ()
  (when (and *main-thread* (not (bt:thread-alive-p *main-thread*)))
    (setf *main-thread* nil))
  (unless *main-thread*
    (setf *main-thread* (bt:make-thread (lambda () (gtk:gtk-main)) :name "cl-gtk2 main thread"))))

#+thread-support
(defun join-main-thread ()
  (when *main-thread*
    (bt:join-thread *main-thread*)))

#+thread-support
(export 'join-main-thread)

#-thread-support
(defun ensure-gtk-main ()
  (gtk-main))

(export 'ensure-gtk-main)

#+thread-support
(defun leave-gtk-main ()) ;noop on multithreading

#-thread-support
(defun leave-gtk-main ()
  (gtk-main-quit))

(export 'leave-gtk-main)

(defcfun gtk-main-level :uint)

(defcfun gtk-main-quit :void)

(defcfun gtk-grab-add :void
  (widget g-object))

(defcfun gtk-grab-get-current g-object)

(defcfun gtk-grab-remove :void
  (widget g-object))