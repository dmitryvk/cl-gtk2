(in-package :gtk)

(defcfun gtk-clipboard-set-text :void
  (clipboard g-object)
  (text :string)
  (len :int))

(defun clipboard-set-text (clipboard text)
  (gtk-clipboard-set-text clipboard text -1))

(export 'clipboard-set-text)

(defcfun gtk-clipboard-clear :void
  (clipboard g-object))

(defun clipboard-clear (clipboard)
  (gtk-clipboard-clear clipboard))

(export 'clipboard-clear)

