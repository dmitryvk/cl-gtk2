(in-package :gtk)

(defcfun (object-destroy "gtk_object_destroy") :void
  (object (g-object gtk-object)))
