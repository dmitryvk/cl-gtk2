(in-package :gdk)

(defcfun gdk-window-get-events event-mask
  (window (g-object gdk-window)))

(defcfun gdk-window-set-events :void
  (window (g-object gdk-window))
  (flags event-mask))

(defun gdk-window-events (window)
  (gdk-window-get-events window))

(defun (setf gdk-window-events) (new-value window)
  (gdk-window-set-events window new-value))

(defcfun gdk-gc-new (g-object graphics-context)
  (drawable (g-object drawable)))

(defcfun gdk-draw-line :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x1 :int)
  (y1 :int)
  (x2 :int)
  (y2 :int))

(defcfun gdk-gc-set-rgb-fg-color :void
  (gc (g-object graphics-context))
  (color (g-boxed-ptr color)))

(defcfun gdk-drawable-get-size :void
  (drawable (g-object drawable))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun drawable-get-size (drawable)
  (with-foreign-objects ((x :int)
                         (y :int))
    (gdk-drawable-get-size drawable x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(define-g-object-class "PangoLayout" pango-layout (:type-initializer "pango_layout_get_type") ())

(defcfun gdk-draw-layout :void
  (drawable (g-object drawable))
  (gc (g-object graphics-context))
  (x :int)
  (y :int)
  (layout (g-object pango-layout)))

(defcfun gdk-atom-name (glib:g-string :free-from-foreign t)
  (atom gdk-atom))

(defcfun gdk-atom-intern gdk-atom
  (name :string)
  (only-if-exists :boolean))

(export 'drawable-size)

(defcfun gdk-pixbuf-get-from-drawable (g-object pixbuf)
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

(defcfun gdk-pixbuf-savev :boolean
  (pixbuf (g-object pixbuf))
  (filename :string)
  (type :string)
  (option-keys (:pointer (:pointer :char)))
  (option-values (:pointer (:pointer :char)))
  (error :pointer))

(defun pixbuf-save (pixbuf filename type)
  (gdk-pixbuf-savev pixbuf
                    (etypecase filename
                      (string filename)
                      (pathname (namestring filename)))
                    type
                    (null-pointer)
                    (null-pointer)
                    (null-pointer)))

(export 'pixbuf-save)
