(in-package :gdk)

(defcfun (default-screen "gdk_screen_get_default") (g-object gdk-screen))
(export 'default-screen)

(defcfun gdk-atom-name (glib:g-string :free-from-foreign t)
  (atom gdk-atom))

(defcfun gdk-atom-intern gdk-atom
  (name :string)
  (only-if-exists :boolean))

(defcfun gdk-pixbuf-savev :boolean
  (pixbuf (g-object pixbuf))
  (filename :string)
  (type :string)
  (option-keys (:pointer (:pointer :char)))
  (option-values (:pointer (:pointer :char)))
  (error :pointer))

(defun pixbuf-save (pixbuf filename type)
  (gdk-pixbuf-savev pixbuf
                    (etypecase filename
                      (string filename)
                      (pathname (namestring filename)))
                    type
                    (null-pointer)
                    (null-pointer)
                    (null-pointer)))

(export 'pixbuf-save)

(defcfun gdk-pixbuf-new-from-file (g-object pixbuf :already-referenced)
  (filename :string)
  (error :pointer))

(defun pixbuf-new-from-file (filename)
  (glib:with-g-error (err)
    (gdk-pixbuf-new-from-file filename err)))

(export 'pixbuf-new-from-file)
