(in-package :gdk)

(defcfun (gdk-image-new "gdk_image_new") (g-object gdk-image :already-referenced)
  (type gdk-image-type)
  (visual (g-object visual))
  (width :int)
  (height :int))

(export 'gdk-image-new)

;; deprecated:
;; GdkImage*           gdk_image_new_bitmap                (GdkVisual *visual,
;;                                                          gpointer data,
;;                                                          gint width,
;;                                                          gint height);
;; GdkImage*           gdk_image_get                       (GdkDrawable *drawable,
;;                                                          gint x,
;;                                                          gint y,
;;                                                          gint width,
;;                                                          gint height);
;; GdkImage *          gdk_image_ref                       (GdkImage *image);
;; void                gdk_image_unref                     (GdkImage *image);
;; #define             gdk_image_destroy


(defcfun (gdk-image-put-pixel "gdk_image_put_pixel") :void
  (image (g-object gdk-image))
  (x :int)
  (y :int)
  (pixel :uint32))

(export 'gdk-image-put-pixel)

(defcfun (gdk-image-get-pixel "gdk_image_get_pixel") :uint32
  (image (g-object gdk-image))
  (x :int)
  (y :int))

(export 'gdk-image-get-pixel)
