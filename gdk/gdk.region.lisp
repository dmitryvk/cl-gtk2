(in-package :gdk)

(defcfun gdk-rectangle-intersect :boolean
  (src-1 (g-boxed-foreign rectangle))
  (src-2 (g-boxed-foreign rectangle))
  (dest (g-boxed-foreign rectangle)))

(defun rectangle-intersect (rectangle-1 rectangle-2)
  (let ((dest (make-rectangle)))
    (when (gdk-rectangle-intersect rectangle-1 rectangle-2 dest)
      dest)))

(export 'rectangle-intersect)

(defcfun gdk-rectangle-union :boolean
  (src-1 (g-boxed-foreign rectangle))
  (src-2 (g-boxed-foreign rectangle))
  (dest (g-boxed-foreign rectangle)))

(defun rectangle-union (rectangle-1 rectangle-2)
  (let ((dest (make-rectangle)))
    (when (gdk-rectangle-union rectangle-1 rectangle-2 dest)
      dest)))

(export 'rectangle-union)

(defcfun gdk-region-polygon (g-boxed-foreign region :return)
  (points :pointer)
  (n-points :int)
  (fill-rule gdk-fill-rule))

(defun region-from-polygon (points fill-rule)
  (with-foreign-boxed-array (n pts point points)
    (gdk-region-polygon pts n fill-rule)))

(export 'region-from-polygon)

(defcfun (region-from-rectangle "gdk_region_rectangle") (g-boxed-foreign region :return)
  (rectangle (g-boxed-foreign rectangle)))

(export 'region-from-rectangle)

(defcfun gdk-region-get-clipbox :void
  (region (g-boxed-foreign region))
  (rectangle (g-boxed-foreign rectangle)))

(defun region-get-clipbox (region)
  (let ((clipbox (make-rectangle)))
    (gdk-region-get-clipbox region clipbox)
    clipbox))

(export 'region-get-clipbox)

(defcfun gdk-region-get-rectangles :void
  (region (g-boxed-foreign region))
  (rectangles :pointer)
  (n-rectangles :pointer))

(defun region-get-rectangles (region)
  (with-foreign-objects ((rectangles-ptr :pointer) (n-rectangles-ptr :int))
    (gdk-region-get-rectangles region rectangles-ptr n-rectangles-ptr)
    (let ((n (mem-ref n-rectangles-ptr :int))
          (rectangles (mem-ref rectangles-ptr :pointer)))
      (prog1
          (iter (for i from 0 below n)
                (for rect = (convert-from-foreign (inc-pointer rectangles (* i (foreign-type-size 'rectangle-cstruct)))
                                                  '(g-boxed-foreign rectangle)))
                (collect rect))
        (glib:g-free rectangles)))))

(export 'region-get-rectangles)

(defcfun (region-is-empty "gdk_region_empty") :boolean
  (region (g-boxed-foreign region)))

(export 'region-is-empty)

(defcfun (region= "gdk_region_equal") :boolean
  (region-1 (g-boxed-foreign region))
  (region-2 (g-boxed-foreign region)))

(export 'region=)

(defcfun (region-point-in "gdk_region_point_in") :boolean
  (region (g-boxed-foreign region))
  (x :int)
  (y :int))

(export 'region-point-in)

(defcfun (region-rect-in "gdk_region_rect_in") overlap-type
  (region (g-boxed-foreign region))
  (rectangle (g-boxed-foreign rectangle)))

(export 'region-rect-in)

(defcfun (region-offset "gdk_region_offset") :void
  (region (g-boxed-foreign region))
  (dx :int)
  (dy :int))

(export 'region-offset)

(defcfun (region-shrink "gdk_region_shrink") :void
  (region (g-boxed-foreign region))
  (dx :int)
  (dy :int))

(export 'region-shrink)

(defcfun (region-union-with-rect "gdk_region_union_with_rect") :void
  (region (g-boxed-foreign region))
  (rect (g-boxed-foreign rectangle)))

(export 'region-union-with-rect)

(defcfun (region-intersect "gdk_region_intersect") :void
  (target (g-boxed-foreign region))
  (source (g-boxed-foreign region)))

(export 'region-intersect)

(defcfun (region-union "gdk_region_union") :void
  (target (g-boxed-foreign region))
  (source (g-boxed-foreign region)))

(export 'region-union)

(defcfun (region-subtract "gdk_region_subtract") :void
  (target (g-boxed-foreign region))
  (source (g-boxed-foreign region)))

(export 'region-subtract)

(defcfun (region-xor "gdk_region_xor") :void
  (target (g-boxed-foreign region))
  (source (g-boxed-foreign region)))

(export 'region-xor)

(defcallback gdk-span-func-callback :void
    ((span (g-boxed-foreign span)) (data :pointer))
  (let ((fn (stable-pointer-value data)))
    (funcall fn span)))

(defcfun gdk-region-spans-intersect-foreach :void
  (region (g-boxed-foreign region))
  (spans :pointer)
  (n-spans :int)
  (sorted :boolean)
  (function :pointer)
  (data :pointer))

(defun region-spans-intersect-foreach (region spans sorted fn)
  (with-stable-pointer (ptr fn)
    (with-foreign-boxed-array (n spans-ptr span spans)
      (gdk-region-spans-intersect-foreach region spans-ptr n sorted (callback gdk-region-spans-intersect-foreach) ptr))))

(export 'region-spans-intersect-foreach)
