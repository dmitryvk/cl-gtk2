(in-package :gdk)

(define-g-object-class "GdkPangoRenderer" gdk-pango-renderer
  (:superclass pango-renderer :export t
               :interfaces nil :type-initializer
               "gdk_pango_renderer_get_type")
  ((screen gdk-pango-renderer-screen "screen"
           "GdkScreen" t nil)))

(defcfun gdk-pango-renderer-new (g-object gdk-pango-renderer :already-referenced)
  (screen (g-object screen)))

(export 'gdk-pango-renderer-new)

(defcfun gdk-pango-renderer-get-default (g-object gdk-pango-renderer)
  (screen (g-object screen)))

(export 'gdk-pango-renderer-get-default)

(defcfun gdk-pango-renderer-set-drawable :void
  (renderer (g-object gdk-pango-renderer))
  (drawable (g-object drawable)))

(export 'gdk-pango-renderer-set-drawable)

(defcfun gdk-pango-renderer-set-gc :void
  (renderer (g-object gdk-pango-renderer))
  (gc (g-object graphics-context)))

(export 'gdk-pango-renderer-set-gc)

(defcfun gdk-pango-renderer-set-stipple :void
  (renderer (g-object gdk-pango-renderer))
  (part pango-render-part)
  (stipple (g-object pixmap)))

(export 'gdk-pango-renderer-set-stipple)

(defcfun gdk-pango-renderer-set-override-color :void
  (renderer (g-object gdk-pango-renderer))
  (part pango-render-part)
  (color (g-boxed-foreign color)))

(export 'gdk-pango-renderer-set-override-color)

(defcfun gdk-pango-context-get (g-object pango-context :already-referenced))

(export 'gdk-pango-context-get)

(defcfun gdk-pango-context-get-for-screen (g-object pango-context :already-referenced)
  (screen (g-object screen)))

(export 'gdk-pango-context-get-for-screen)

;; ignored:
;; void                gdk_pango_context_set_colormap      (PangoContext *context,
;;                                                          GdkColormap *colormap);

;; TODO:
;;                     GdkPangoAttrEmbossed;
;;                     GdkPangoAttrEmbossColor;
;;                     GdkPangoAttrStipple;
;; PangoAttribute *    gdk_pango_attr_emboss_color_new     (const GdkColor *color);
;; PangoAttribute *    gdk_pango_attr_embossed_new         (gboolean embossed);
;; PangoAttribute *    gdk_pango_attr_stipple_new          (GdkBitmap *stipple);

(defcfun gdk_pango_layout_get_clip_region (g-boxed-foreign region :return)
  (layout (g-object pango-layout))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-get-clip-region (layout x-origin y-origin index-ranges)
  (let ((n (length index-ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges :int i) x)
                 (incf i))
               index-ranges))
        (gdk_pango_layout_get_clip_region layout x-origin y-origin index-ranges n-ranges)))))

(export 'gdk-pango-layout-get-clip-region)

(defcfun gdk_pango_layout_line_get_clip_region (g-boxed-foreign region :return)
  (layout-line (g-boxed-foreign pango-layout-line))
  (x-origin :int)
  (y-origin :int)
  (index-ranges (:pointer :int))
  (n-ranges :int))

(defun gdk-pango-layout-line-get-clip-region (layout-line x-origin y-origin index-ranges)
  (let ((n (length index-ranges)))
    (assert (zerop (mod n 2)))
    (let ((n-ranges (/ n 2)))
      (with-foreign-object (ranges :int n)
        (let ((i 0))
          (map nil
               (lambda (x)
                 (setf (mem-aref ranges :int i) x)
                 (incf i))
               index-ranges))
        (gdk_pango_layout_line_get_clip_region layout-line x-origin y-origin index-ranges n-ranges)))))

(export 'gdk-pango-layout-line-get-clip-region)
