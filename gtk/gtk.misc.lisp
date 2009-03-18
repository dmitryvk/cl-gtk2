(in-package :gtk)

(defcallback stable-pointer-free-destroy-notify-callback :void ((data :pointer))
  (free-stable-pointer data))

(defcfun (get-clipboard "gtk_clipboard_get") g-object
  (selection gdk-atom-as-string))

(export 'get-clipboard)