(in-package :gtk)

(defcfun (spinner-start "gtk_spinner_start") :void
  (spinner (g-object gtk:spinner)))

(defcfun (spinner-stop "gtk_spinner_stop") :void
  (spinner (g-object gtk:spinner)))

(export '(spinner-start spinner-stop))
