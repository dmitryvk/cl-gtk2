(in-package :gtk)

(defcfun gtk-paned-pack1 :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun paned-pack-1 (paned child &key (resize nil) (shrink t))
  (gtk-paned-pack1 paned child resize shrink))

(export 'paned-pack-1)

(defcfun gtk-paned-pack2 :void
  (paned g-object)
  (child g-object)
  (resize :boolean)
  (shrink :boolean))

(defun paned-pack-2 (paned child &key (resize t) (shrink t))
  (gtk-paned-pack2 paned child resize shrink))

(export 'paned-pack-2)

(defcfun (paned-child-1 "gtk_paned_get_child1") g-object
  (paned g-object))

(defcfun (paned-child-2 "gtk_paned_get_child2") g-object
  (paned g-object))

(export 'paned-child-1)

(export 'paned-child-2)

; TODO: GtkScale, gtk_scale_get_layout_offsets