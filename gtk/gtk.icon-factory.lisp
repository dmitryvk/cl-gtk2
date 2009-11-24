(in-package :gtk)

;; icon-source

(at-init () (foreign-funcall "gtk_icon_source_get_type" :int))

(defcfun gtk-icon-source-new :pointer)

(define-g-boxed-opaque icon-source "GtkIconSource"
  :alloc (gtk-icon-source-new))

(export 'icon-source)

(define-boxed-opaque-accessor icon-source icon-source-filename
  :reader "gtk_icon_source_get_filename"
  :writer "gtk_icon_source_set_filename"
  :type (:string :free-from-foreign nil))

(export 'icon-source-filename)

(define-boxed-opaque-accessor icon-source icon-source-icon-name
  :reader "gtk_icon_source_get_icon_name"
  :writer "gtk_icon_source_set_icon_name"
  :type (:string :free-from-foreign nil))

(export 'icon-source-icon-name)

#|
GtkTextDirection    gtk_icon_source_get_direction       (const GtkIconSource *source);
gboolean            gtk_icon_source_get_direction_wildcarded
                                                        (const GtkIconSource *source);
GdkPixbuf*          gtk_icon_source_get_pixbuf          (const GtkIconSource *source);
GtkIconSize         gtk_icon_source_get_size            (const GtkIconSource *source);
gboolean            gtk_icon_source_get_size_wildcarded (const GtkIconSource *source);
GtkStateType        gtk_icon_source_get_state           (const GtkIconSource *source);
gboolean            gtk_icon_source_get_state_wildcarded
                                                        (const GtkIconSource *source);

void                gtk_icon_source_set_direction       (GtkIconSource *source,
                                                         GtkTextDirection direction);
void                gtk_icon_source_set_direction_wildcarded
                                                        (GtkIconSource *source,
                                                         gboolean setting);
void                gtk_icon_source_set_pixbuf          (GtkIconSource *source,
                                                         GdkPixbuf *pixbuf);
void                gtk_icon_source_set_size            (GtkIconSource *source,
                                                         GtkIconSize size);
void                gtk_icon_source_set_size_wildcarded (GtkIconSource *source,
                                                         gboolean setting);
void                gtk_icon_source_set_state           (GtkIconSource *source,
                                                         GtkStateType state);
void                gtk_icon_source_set_state_wildcarded
                                                        (GtkIconSource *source,
                                                         gboolean setting);
|#


;; icon-set

(at-init () (foreign-funcall "gtk_icon_set_get_type" :int))

(defcfun gtk-icon-set-new :pointer)

(define-g-boxed-opaque icon-set "GtkIconSet"
  :alloc (gtk-icon-set-new))

(export 'icon-set)

(defcfun gtk-icon-set-add-source :void
  (icon-set (g-boxed-foreign icon-set))
  (source (g-boxed-foreign icon-source)))

(defun icon-set-add-source (icon-set icon-source)
  (gtk-icon-set-add-source icon-set icon-source))

(export 'icon-set-add-source)

#|
GtkIconSet*         gtk_icon_set_new_from_pixbuf        (GdkPixbuf *pixbuf);
GdkPixbuf*          gtk_icon_set_render_icon            (GtkIconSet *icon_set,
                                                         GtkStyle *style,
                                                         GtkTextDirection direction,
                                                         GtkStateType state,
                                                         GtkIconSize size,
                                                         GtkWidget *widget,
                                                         const char *detail);
void                gtk_icon_set_get_sizes              (GtkIconSet *icon_set,
                                                         GtkIconSize **sizes,
                                                         gint *n_sizes);

gboolean            gtk_icon_size_lookup                (GtkIconSize size,
                                                         gint *width,
                                                         gint *height);
gboolean            gtk_icon_size_lookup_for_settings   (GtkSettings *settings,
                                                         GtkIconSize size,
                                                         gint *width,
                                                         gint *height);
GtkIconSize         gtk_icon_size_register              (const gchar *name,
                                                         gint width,
                                                         gint height);
void                gtk_icon_size_register_alias        (const gchar *alias,
                                                         GtkIconSize target);
GtkIconSize         gtk_icon_size_from_name             (const gchar *name);
const gchar*        gtk_icon_size_get_name              (GtkIconSize size);
|#

;; icon-factory

(defcfun gtk-icon-factory-add :void
  (factory (g-object icon-factory))
  (stock-id :string)
  (icon-set (g-boxed-foreign icon-set)))

(defun icon-factory-add (factory stock-id icon-set)
  (gtk-icon-factory-add factory stock-id icon-set))

(export 'icon-factory-add)

(defcfun gtk-icon-factory-add-default :void
  (factory (g-object icon-factory)))

(defun icon-factory-add-default (factory)
  (gtk-icon-factory-add-default factory))

(export 'icon-factory-add-default)

(defcfun gtk-icon-factory-lookup (g-boxed-foreign icon-set :return)
  (factory (g-object icon-factory))
  (stock-id :string))

(defun icon-factory-lookup (factory stock-id)
  (gtk-icon-factory-lookup factory stock-id))

(export 'icon-factory-lookup)

(defcfun gtk-icon-factory-lookup-default (g-boxed-foreign icon-set :return)
  (stock-id :string))

(defun icon-factory-lookup-default (stock-id)
  (gtk-icon-factory-lookup-default stock-id))

(export 'icon-factory-lookup-default)

(defcfun gtk-icon-factory-remove-default :void
  (factory (g-object icon-factory)))

(defun icon-factory-remove-default (factory)
  (gtk-icon-factory-remove-default factory))

