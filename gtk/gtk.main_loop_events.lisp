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

(defcfun gtk-main-level :uint)

(defcfun gtk-main-quit :void)

(defcfun gtk-main-iteration :boolean)

(defcfun gtk-main-iteration-do :boolean
  (blocking :boolean))

(defcfun gtk-grab-add :void
  (widget g-object))

(defcfun gtk-grab-get-current g-object)

(defcfun gtk-grab-remove :void
  (widget g-object))