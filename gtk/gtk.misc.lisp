(in-package :gtk)

(defcallback stable-pointer-free-destroy-notify-callback :void ((data :pointer))
  (free-stable-pointer data))