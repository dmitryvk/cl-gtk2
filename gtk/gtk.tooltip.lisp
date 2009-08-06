(in-package :gtk)

(defcfun gtk-tooltip-set-markup :void
  (tooltip g-object)
  (markup :string))

(defun (setf tooltip-markup) (new-value tooltip)
  (gtk-tooltip-set-markup tooltip new-value))

(export 'tooltip-markup)

(defcfun gtk-tooltip-set-text :void
  (tooltip g-object)
  (text :string))

(defun (setf tooltip-text) (new-value tooltip)
  (gtk-tooltip-set-text tooltip new-value))

(export 'tooltip-text)

(defcfun gtk-tooltip-set-icon :void
  (tooltip g-object)
  (pixbuf g-object))

(defun (setf tooltip-icon) (new-value tooltip)
  (gtk-tooltip-set-icon tooltip new-value))

(export 'tooltip-icon)

(defcfun (tooltip-set-icon-from-stock "gtk_tooltip_set_icon_from_stock") :void
  (tooltip g-object)
  (stock-id :string)
  (icon-size icon-size))

(export 'tooltip-set-icon-from-stock)

(defcfun (tooltip-set-icon-from-icon-name "gtk_tooltip_set_icon_from_icon_name") :void
  (tooltip g-object)
  (icon-name :string)
  (icon-size icon-size))

(export 'tooltip-set-icon-from-icon-name)

(defcfun (tooltip-set-custom "gtk_tooltip_set_custom") :void
  (tooltip g-object)
  (custom-widget g-object))

(export 'tooltip-set-custom)

(defcfun (tooltip-trigger-tooltip-query "gtk_tooltip_trigger_tooltip_query") :void
  (display g-object))

(export 'tooltip-trigger-tooltip-query)

(defcfun (tooltip-set-tip-area "gtk_tooltip_set_tip_area") :void
  (tooltip g-object)
  (rectangle (g-boxed-foreign rectangle)))

(export 'tooltip-set-tip-area)