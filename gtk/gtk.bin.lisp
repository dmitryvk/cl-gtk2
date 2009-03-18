(in-package :gtk)

(defcfun (bin-child "gtk_bin_get_child") g-object
  (bin g-object))

(export 'bin-child)