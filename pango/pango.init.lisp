(in-package :pango)

(glib:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pango
      ((:and :unix (:not :darwin)) "libpango-1.0.so.0")
      (:darwin (:or "libpango-1.0.0.dylib" "libpango-1.0.dylib"))
      (:windows "libpango-1.0-0.dll")
      (t (:default "libgpango-1.0"))))

  (use-foreign-library pango))

