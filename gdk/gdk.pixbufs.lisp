(in-package :gdk)

(defcfun (pixbuf-render-threshold-alpha "gdk_pixbuf_render_threshold_alpha") :void
  (pixbuf (g-object pixbuf))
  (bitmap (g-object bitmap))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (alpha-threshold :int))

(export 'pixbuf-render-threshold-alpha)

(defcfun (pixbuf-render-to-drawable "gdk_pixbuf_render_to_drawable") :void
  (pixbuf (g-object pixbuf))
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (dither rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'pixbuf-render-to-drawable)

(defcfun (pixbuf-render-to-drawable-alpha "gdk_pixbuf_render_to_drawable_alpha") :void
  (pixbuf (g-object pixbuf))
  (drawable (g-object drawable))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int)
  (alpha-mode pixbuf-alpha-mode)
  (alpha-threshold :int)
  (dither rgb-dither)
  (x-dither :int)
  (y-dither :int))

(export 'pixbuf-render-to-drawable-alpha)

(defcfun gdk-pixbuf-render-pixmap-and-mask :void
  (pixbuf (g-object pixbuf))
  (pixmap-return :pointer)
  (mask-return :pointer)
  (alpha-threshold :int))

(defun pixbuf-render-pixmap-and-mask (pixbuf alpha-threshold)
  (with-foreign-objects ((pixmap-return :pointer) (mask-return :pointer))
    (gdk-pixbuf-render-pixmap-and-mask pixbuf pixmap-return mask-return alpha-threshold)
    (values (convert-from-foreign (mem-ref pixmap-return :pointer) '(g-object pixmap :already-referenced))
            (convert-from-foreign (mem-ref mask-return :pointer) '(g-object pixmap :already-referenced)))))

(export 'pixbuf-render-pixmap-and-mask)

(defcfun gdk-pixbuf-render-pixmap-and-mask-for-colormap :void
  (pixbuf (g-object pixbuf))
  (colormap (g-object colormap))
  (pixmap-return :pointer)
  (mask-return :pointer)
  (alpha-threshold :int))

(defun pixbuf-render-pixmap-and-mask-for-colormap (pixbuf colormap alpha-threshold)
  (with-foreign-objects ((pixmap-return :pointer) (mask-return :pointer))
    (gdk-pixbuf-render-pixmap-and-mask-for-colormap pixbuf colormap pixmap-return mask-return alpha-threshold)
    (values (convert-from-foreign (mem-ref pixmap-return :pointer) '(g-object pixmap :already-referenced))
            (convert-from-foreign (mem-ref mask-return :pointer) '(g-object pixmap :already-referenced)))))

(export 'pixbuf-render-pixmap-and-mask-for-colormap)

(defcfun gdk-pixbuf-get-from-drawable (g-object pixbuf :already-referenced)
  (dest (g-object pixbuf))
  (src (g-object drawable))
  (colormap :pointer)
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(defun pixbuf-get-from-drawable (pixbuf drawable &key (src-x 0) (src-y 0) (dest-x 0) (dest-y 0) (width -1) (height -1))
  (gdk-pixbuf-get-from-drawable pixbuf drawable (null-pointer) src-x src-y dest-x dest-y width height))

(export 'pixbuf-get-from-drawable)

(defcfun gdk-pixbuf-get-from-image (g-object pixbuf :already-referenced)
  (dest (g-object pixbuf))
  (src (g-object gdk-image))
  (colormap :pointer)
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(defun pixbuf-get-from-image (pixbuf image &key (src-x 0) (src-y 0) (dest-x 0) (dest-y 0) (width -1) (height -1))
  (gdk-pixbuf-get-from-image pixbuf image (null-pointer) src-x src-y dest-x dest-y width height))

(export 'pixbuf-get-from-image)
