(in-package :gtk)

(defcfun (statusbar-get-context-id "gtk_statusbar_get_context_id") :uint
  (statusbar (g-object statusbar))
  (context-description :string))

(defcfun gtk-statusbar-push :uint
  (statusbar (g-object statusbar))
  (context-id :uint)
  (text :string))

(defcfun gtk-statusbar-pop :void
  (statusbar (g-object statusbar))
  (context-id :uint))

(defcfun gtk-statusbar-remove :void
  (statusbar (g-object statusbar))
  (context-id :uint)
  (message-id :uint))

(defun statusbar-context-id (statusbar context)
  (etypecase context
    (integer context)
    (string (statusbar-get-context-id statusbar context))))

(defun statusbar-push (statusbar context text)
  (gtk-statusbar-push statusbar (statusbar-context-id statusbar context) text))

(defun statusbar-pop (statusbar context)
  (gtk-statusbar-pop statusbar (statusbar-context-id statusbar context)))

(defun statusbar-remove (statusbar context message-id)
  (gtk-statusbar-remove statusbar (statusbar-context-id statusbar context) message-id))

(export 'statusbar-push)
(export 'statusbar-pop)
(export 'statusbar-remove)
(export 'statusbar-context-id)