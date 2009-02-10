(in-package :gtk)

(defcfun gtk-statusbar-get-context-id :uint
  (status-bar (g-object statusbar))
  (context-description :string))

(defcfun gtk-statusbar-push :uint
  (status-bar (g-object statusbar))
  (context-id :uint)
  (text :string))

(defcfun gtk-statusbar-pop :void
  (status-bar (g-object statusbar))
  (context-id :uint))

(defcfun gtk-statusbar-remove :void
  (status-bar (g-object statusbar))
  (context-id :uint)
  (message-id :uint))

(defun status-bar-push (status-bar context text)
  (gtk-statusbar-push status-bar (gtk-statusbar-get-context-id status-bar context) text))

(defun status-bar-pop (status-bar context)
  (gtk-statusbar-pop status-bar (gtk-statusbar-get-context-id status-bar context)))

(defun status-bar-remove (status-bar context message-id)
  (gtk-statusbar-remove status-bar (gtk-statusbar-get-context-id status-bar context) message-id))

(export 'status-bar-push)
(export 'status-bar-pop)
(export 'status-bar-remove)