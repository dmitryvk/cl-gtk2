(in-package :gtk)

(defcfun gtk-clipboard-set-text :void
  (clipboard g-object)
  (text :string)
  (len :int))

(defun clipboard-set-text (clipboard text)
  (gtk-clipboard-set-text clipboard text -1))

(export 'clipboard-set-text)

