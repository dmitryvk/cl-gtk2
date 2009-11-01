(in-package :gdk)

(defcfun (draw-rgb-image "gdk_draw_rgb_image") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (rgb-buf :pointer)
  (rowstride :int))

(export 'draw-rgb-image)

(defcfun (draw-rgb-image-dithalign "gdk_draw_rgb_image_dithalign") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (rgb-buf :pointer)
  (rowstride :int)
  (x-dith :int)
  (y-dith :int))

(export 'draw-rgb-image-dithalign)

(define-g-boxed-cstruct rgb-cmap nil
  (colors :uint32 :count 256 :initform (make-array 256 :element-type '(unsigned-byte 32) :initial-element 0))
  (n-colors :int :initform 0))

(defcfun (draw-indexed-image "gdk_draw_indexed_image") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (buf :pointer)
  (rowstring :int)
  (cmap (g-boxed-foreign rgb-cmap)))

(export 'draw-indexed-image)

(defcfun (draw-gray-image "gdk_draw_gray_image") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dith rgb-dither)
  (buf :pointer)
  (rowstride :int))

(export 'draw-gray-image)

(defcfun (draw-rgb-32-image "gdk_draw_rgb_32_image") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (buf :pointer)
  (rowstride :int))

(export 'draw-rgb-32-image)

(defcfun (draw-rgb-32-image-dithalign "gdk_draw_rgb_32_image_dithalign") :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (buf :pointer)
  (rowstride :int)
  (xdith :int)
  (ydith :int))

(export 'draw-rgb-32-image-dithalign)

(defcfun (rgb-find-color "gdk_rgb_find_color") :void
  (colormap (g-object colormap))
  (color (g-boxed-foreign color)))

(export 'rgb-find-color)

(defcfun (rgb-set-install "gdk_rgb_set_install") :void
  (install :boolean))

(export 'rgb-set-install)

(defcfun (rgb-set-min-colors "gdk_rgb_set_min_colors") :void
  (min-colors :int))

(export 'rgb-set-min-colors)

(defcfun (rgb-get-visual "gdk_rgb_get_visual") (g-object visual))

(export 'rgb-get-visual)

(defcfun (rgb-get-colormap "gdk_rgb_get_colormap") (g-object colormap))

(export 'rgb-get-colormap)

(defcfun (rgb-ditherable "gdk_rgb_ditherable") :boolean)

(export 'rgb-ditherable)

(defcfun (rgb-colormap-ditherable "gdk_rgb_colormap_ditherable") :boolean
  (colormap (g-object colormap)))

(export 'rgb-colormap-ditherable)

(defcfun (rgb-set-verbose "gdk_rgb_set_verbose") :void
  (verbose :boolean))

(export 'rgb-set-verbose)
