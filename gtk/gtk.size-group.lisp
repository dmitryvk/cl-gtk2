(in-package :gtk)

(defcfun (size-group-add-widget "gtk_size_group_add_widget") :void
  (size-group g-object)
  (widget g-object))

(export 'size-group-add-widget)

(defcfun (size-group-remove-widget "gtk_size_group_remove_widget") :void
  (size-group g-object)
  (widget g-object))

(export 'size-group-remove-widget)

(defcfun (size-group-widgets "gtk_size_group_get_widgets") (gslist g-object :free-from-foreign nil)
  (size-group g-object))

(export 'size-group-widgets)