(in-package :gdk)

(defcstruct %gdk-cursor
  (cursor-type cursor-type))

(defun cursor-cursor-type (cursor)
  (foreign-slot-value (pointer cursor) '%gdk-cursor 'cursor-type))

(export 'cursor-cursor-type)

(defcfun (cursor-new "gdk_cursor_new") (g-boxed-foreign cursor :return)
  (cursor-type cursor-type))

(export 'cursor-new)

(defcfun (cursor-new-from-pixmap "gdk_cursor_new_from_pixmap") (g-boxed-foreign cursor :return)
  (source (g-object pixmap))
  (make (g-object pixmap))
  (fg-color (g-boxed-foreign color))
  (bg-color (g-boxed-foreign color))
  (x :int)
  (y :int))

(export 'cursor-new-from-pixmap)

(defcfun (cursor-new-from-pixbuf "gdk_cursor_new_from_pixbuf") (g-boxed-foreign cursor :return)
  (display (g-object display))
  (pixbuf (g-object pixbuf))
  (x :int)
  (y :int))

(export 'cursor-new-from-pixbuf)

(defcfun (cursor-new-from-name "gdk_cursor_new_from_name") (g-boxed-foreign cursor :return)
  (display (g-object display))
  (name :string))

(export 'cursor-new-from-name)

(defcfun (cursor-new-for-display "gdk_cursor_new_for_display") (g-boxed-foreign cursor :return)
  (display (g-object display))
  (cursor-type cursor-type))

(export 'cursor-new-for-display)

(define-boxed-opaque-accessor cursor cursor-display :type (g-object display) :reader "gdk_cursor_get_display")
(define-boxed-opaque-accessor cursor cursor-image :type (g-object pixbuf) :reader "gdk_cursor_get_image")

(export '(cursor-display cursor-image))
