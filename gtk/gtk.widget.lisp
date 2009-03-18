(in-package :gtk)

; TODO: GtkWidget

(defcfun gtk-widget-show-all :void
  (widget (g-object widget)))

(defcfun gtk-widget-queue-draw :void
  (widget (g-object widget)))

(defcfun gtk-widget-create-pango-layout (g-object gdk::pango-layout)
  (widget (g-object widget))
  (text :string))
