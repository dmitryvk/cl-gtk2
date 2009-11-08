(in-package :gdk)

(defcfun (colormap-new "gdk_colormap_new") (g-object colormap :already-referenced)
  (visual (g-object visual))
  (allocate :boolean))

(export 'colormap-new)

;; deprecated
;; GdkColormap*        gdk_colormap_ref                    (GdkColormap *cmap);
;; void                gdk_colormap_unref                  (GdkColormap *cmap);

(defcfun (colormap-get-system "gdk_colormap_get_system") (g-object colormap))

(export 'colormap-get-system)

;; deprecated
;; gint                gdk_colormap_get_system_size        (void);
;; void                gdk_colormap_change                 (GdkColormap *colormap,
;;                                                          gint ncolors);

(defcfun gdk-colormap-alloc-colors :int
  (colormap (g-object colormap))
  (colors :pointer)
  (n-colors :int)
  (writeable :boolean)
  (best-match :boolean)
  (success (:pointer :boolean)))

(defun colormap-alloc-colors (colormap colors writeable best-match)
  (with-foreign-boxed-array (n colors-ar color colors)
    (with-foreign-object (success :boolean)
      (gdk-colormap-alloc-colors colormap colors-ar n writeable best-match success)
      (mem-ref success :boolean))))

(export 'colormap-alloc-colors)

(defcfun (colormap-alloc-color "gdk_colormap_alloc_color") :boolean
  (colormap (g-object colormap))
  (color (g-boxed-foreign color))
  (writeable :boolean)
  (best-match :boolean))

(export 'colormap-alloc-color)

(defcfun gdk-colormap-free-colors :void
  (colormap (g-object colormap))
  (colors :pointer)
  (n-colors :int))

(defun colormap-free-colors (colormap colors)
  (with-foreign-boxed-array (n colors-ptr color colors)
    (gdk-colormap-free-colors colormap colors-ptr n)))

(export 'colormap-free-colors)

(defcfun gdk-colormap-query-color :void
  (colormap (g-object colormap))
  (pixel :ulong)
  (result (g-boxed-foreign color)))

(defun colormap-query-color (colormap pixel)
  (let ((color (make-color)))
    (gdk-colormap-query-color colormap pixel color)
    color))

(export 'colormap-query-color)

;; ignored:
;; void                gdk_colors_store                    (GdkColormap *colormap,
;;                                                          GdkColor *colors,
;;                                                          gint ncolors);
;; gint                gdk_colors_alloc                    (GdkColormap *colormap,
;;                                                          gboolean contiguous,
;;                                                          gulong *planes,
;;                                                          gint nplanes,
;;                                                          gulong *pixels,
;;                                                          gint npixels);
;; void                gdk_colors_free                     (GdkColormap *colormap,
;;                                                          gulong *pixels,
;;                                                          gint npixels,
;;                                                          gulong planes);
;; gint                gdk_color_white                     (GdkColormap *colormap,
;;                                                          GdkColor *color);
;; gint                gdk_color_black                     (GdkColormap *colormap,
;;                                                          GdkColor *color);

(defcfun gdk-color-parse :boolean
  (spec :string)
  (color (g-boxed-foreign color)))

(defun color-parse (color-spec)
  (let ((color (make-color)))
    (when (gdk-color-parse color-spec color)
      color)))

(export 'color-parse)

;; ignored:
;; gint                gdk_color_alloc                     (GdkColormap *colormap,
;;                                                          GdkColor *color);
;; gint                gdk_color_change                    (GdkColormap *colormap,
;;                                                          GdkColor *color);

(defcfun (color= "gdk_color_equal") :boolean
  (color-a (g-boxed-foreign color))
  (color-b (g-boxed-foreign color)))

(export 'color=)

(defcfun (gdk-color-hash "gdk_color_hash") :uint
  (color (g-boxed-foreign color)))

(export 'gdk-color-hash)

(defcfun (color-to-string "gdk_color_to_string") (glib:g-string :free-from-foreign t)
  (color (g-boxed-foreign color)))

(export 'color-to-string)