(in-package :gtk)

(defcfun gtk-clipboard-set-text :void
  (clipboard (g-object clipboard))
  (text :string)
  (len :int))

(defun clipboard-set-text (clipboard text)
  (gtk-clipboard-set-text clipboard text -1))

(export 'clipboard-set-text)

(defcfun (clipboard-clear "gtk_clipboard_clear") :void
  (clipboard (g-object clipboard)))

(export 'clipboard-clear)

