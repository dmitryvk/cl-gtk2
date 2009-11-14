(in-package :gdk)

(defcfun gdk-drawable-get-size :void
  (drawable (g-object drawable))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun drawable-get-size (drawable)
  (with-foreign-objects ((x :int)
                         (y :int))
    (gdk-drawable-get-size drawable x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'drawable-get-size)

(defcfun (draw-point "gdk_draw_point") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int))

(export 'draw-point)

(defcfun gdk-draw-points :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (points :pointer)
  (n :int))

(defun draw-points (drawable gc points)
  (with-foreign-boxed-array (n points-ptr point points)
    (gdk-draw-points drawable gc points-ptr n)))

(export 'draw-points)

(defcfun (draw-line "gdk_draw_line") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(export 'draw-line)

(defcfun gdk-draw-lines :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (points :pointer)
  (n :int))

(defun draw-lines (drawable gc points)
  (with-foreign-boxed-array (n points-ptr point points)
    (gdk-draw-lines drawable gc points-ptr n)))

(export 'draw-lines)

(defcfun (draw-pixbuf "gdk_draw_pixbuf") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (pixbuf (g-object pixbuf))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'draw-pixbuf)

(defcfun gdk-draw-segments :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (segments :pointer)
  (n-segments :int))

(defun draw-segments (drawable gc segments)
  (with-foreign-boxed-array (n segments-ptr segment segments)
    (gdk-draw-segments drawable gc segments-ptr n)))

(export 'draw-segments)

(defcfun (draw-rectangle "gdk_draw_rectangle") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (filled :boolean)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defcfun (draw-arc "gdk_draw_arc") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (filled :boolean)
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (angle1 :int)
  (angle2 :int))

(export 'draw-arc)

(defcfun gdk-draw-polygon :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (filled :boolean)
  (points :pointer)
  (n-points :int))

(defun draw-polygon (drawable gc filled points)
  (with-foreign-boxed-array (n points-ptr point points)
    (gdk-draw-polygon drawable gc filled points-ptr n)))

(export 'draw-polygon)

(defcfun gdk-draw-trapezoids :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (trapezoids :pointer)
  (n :int))

(defun draw-trapezoids (drawable gc trapezoids)
  (with-foreign-boxed-array (n trapezoids-ptr trapezoid trapezoids)
    (gdk-draw-trapezoids drawable gc trapezoids-ptr n)))

(export 'draw-trapezoids)

(defcfun (draw-glyphs "gdk_draw_glyphs") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (font (g-object pango-font))
  (x :int)
  (y :int)
  (glyphs (g-boxed-foreign pango-glyph-string)))

(export 'draw-glyphs)

(defcfun (draw-glyphs-transformed "gdk_draw_glyphs_transformed") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (matrix (g-boxed-foreign pango-matrix))
  (font (g-object pango-font))
  (x :int)
  (y :int))

(export 'draw-glyphs-transformed)

(defcfun (draw-layout-line "gdk_draw_layout_line") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (line (g-boxed-foreign pango-layout-line)))

(export 'draw-layout-line)

(defcfun (draw-layout-line-with-colors "gdk_draw_layout_line_with_colors") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (line (g-boxed-foreign pango-layout-line))
  (foreground (g-boxed-foreign color))
  (background (g-boxed-foreign color)))

(export 'draw-layout-line-with-colors)

(defcfun (draw-layout "gdk_draw_layout") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (layout (g-object pango-layout)))

(export 'draw-layout)

(defcfun (draw-layout-with-colors "gdk_draw_layout_with_colors") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (layout (g-object pango-layout))
  (foreground (g-boxed-foreign color))
  (background (g-boxed-foreign color)))

(export 'draw-layout-with-colors)

;; ignored:
;; void                gdk_draw_string                     (GdkDrawable *drawable,
;;                                                          GdkFont *font,
;;                                                          GdkGC *gc,
;;                                                          gint x,
;;                                                          gint y,
;;                                                          const gchar *string);
;; void                gdk_draw_text                       (GdkDrawable *drawable,
;;                                                          GdkFont *font,
;;                                                          GdkGC *gc,
;;                                                          gint x,
;;                                                          gint y,
;;                                                          const gchar *text,
;;                                                          gint text_length);
;; void                gdk_draw_text_wc                    (GdkDrawable *drawable,
;;                                                          GdkFont *font,
;;                                                          GdkGC *gc,
;;                                                          gint x,
;;                                                          gint y,
;;                                                          const GdkWChar *text,
;;                                                          gint text_length);


(defcfun (draw-drawable "gdk_draw_drawable") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (src (g-object drawable))
  (x-src :int)
  (y-src :int)
  (x-dest :int)
  (y-dest :int)
  (width :int)
  (height :int))

(export 'draw-drawable)

(defcfun (draw-image "gdk_draw_image") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (image (g-object gdk-image))
  (x-src :int)
  (y-src :int)
  (x-dest :int)
  (y-dest :int)
  (width :int)
  (height :int))

(export 'draw-image)

(defcfun (drawable-get-image "gdk_drawable_get_image") (g-object gdk-image)
  (drawable (g-object drawable))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'drawable-get-image)

(defcfun (drawable-copy-to-image "gdk_drawable_copy_to_image") (g-object gdk-image)
  (drawable (g-object drawable))
  (image (g-object gdk-image))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(export 'drawable-copy-to-image)
