(in-package :pango)

(define-g-enum "PangoWrapMode" pango-wrap-mode
    (:export t :type-initializer
             "pango_wrap_mode_get_type")
  (:word 0) (:char 1) (:word-char 2))

(export 'pango-wrap-mode)

(define-g-enum "PangoEllipsizeMode" pango-ellipsize-mode
    (:export t :type-initializer
             "pango_ellipsize_mode_get_type")
  (:none 0) (:start 1) (:middle 2) (:end 3))

(export 'pango-ellipsize-mode)

(define-g-object-class "PangoLayout" pango-layout (:type-initializer "pango_layout_get_type") ())

(export 'pango-layout)

(define-g-enum "PangoUnderline" pango-underline (:export t :type-initializer "pango_underline_get_type")
  (:none 0)
  (:single 1)
  (:double 2)
  (:low 3)
  (:error 4))

(export 'pango-underline)
