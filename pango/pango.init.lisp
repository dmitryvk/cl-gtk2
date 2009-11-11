(in-package :pango)

(glib:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library pango
      (:unix "libpango-1.0.so.0")
      (:windows "libpango-1.0-0.dll")
      (t (:default "libgpango-1.0"))))

  (use-foreign-library pango))

