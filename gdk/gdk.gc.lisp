(in-package :gdk)

(define-g-boxed-cstruct gc-values nil
  (foregound color :initform (make-color) :inline t)
  (background color :initform (make-color) :inline t)
  (font (g-boxed-foreign font) :initform nil)
  (function gdk-function :initform :copy)
  (fill gdk-fill :initform :solid)
  (tile (g-object pixmap) :initform nil)
  (stipple (g-object pixmap) :initform nil)
  (clip-mask (g-object pixmap) :initform nil)
  (subwindow-mode subwindow-mode :initform :clip-by-children)
  (ts-x-origin :int :initform 0)
  (ts-y-origin :int :initform 0)
  (clip-x-origin :int :initform 0)
  (clip-y-origin :int :initform 0)
  (graphics-exposures :boolean :initform t)
  (line-width :int :initform 0)
  (line-style line-style :initform :solid)
  (cap-style cap-style :initform :butt)
  (join-style join-style :initform :miter))

(export (boxed-related-symbols 'gc-values))

(defcfun (graphics-context-new "gdk_gc_new") (g-object graphics-context :already-referenced)
  (drawable (g-object drawable)))

(export 'graphics-context-new)

(defcfun (graphics-context-new-with-values "gdk_gc_new_with_values") (g-object graphics-context :already-referenced)
  (drawable (g-object drawable))
  (values (g-boxed-foreign gc-values))
  (values-mask gc-values-mask))

(export 'graphics-context-new-with-values)

(defcfun (graphics-context-set-values "gdk_gc_set_values") :void
  (graphics-context (g-object graphics-context))
  (values (g-boxed-foreign gc-values))
  (values-mask gc-values-mask))

(export 'graphics-context-set-values)

(defcfun gdk-gc-get-values :void
  (gc (g-object graphics-context))
  (values (g-boxed-foreign gc-values)))

(defun graphics-context-get-values (graphics-context)
  (let ((values (make-gc-values)))
    (gdk-gc-get-values graphics-context values)
    values))

(export 'graphics-context-get-values)

(defcfun (graphics-context-set-ts-origin "gdk_gc_set_ts_origin") :void
  (graphics-context (g-object graphics-context))
  (x :int)
  (y :int))

(export 'graphics-context-set-ts-origin)

(defcfun (graphics-context-set-clip-origin "gdk_gc_set_clip_origin") :void
  (graphics-context (g-object graphics-context))
  (x :int)
  (y :int))

(export 'graphics-context-set-clip-origin)

(defcfun (graphics-context-set-line-attributes "gdk_gc_set_line_attributes") :void
  (graphics-context (g-object graphics-context))
  (line-width :int)
  (line-style line-style)
  (cap-style cap-style)
  (join-style join-style))

(export 'graphics-context-set-line-attributes)

(defcfun gdk-gc-set-dashes :void
  (graphics-context (g-object graphics-context))
  (dash-offset :int)
  (dash-list :pointer)
  (n :int))

(defun graphics-context-set-dashes (graphics-context dash-offset dash-list)
  (let ((n (length dash-list)))
    (with-foreign-object (dash-list-ptr :int8 n)
      (let ((i 0))
        (map nil
             (lambda (dash)
               (setf (mem-aref dash-list-ptr :int8 i) dash)
               (incf i))
             dash-list))
      (gdk-gc-set-dashes graphics-context dash-offset dash-list n))))

(export 'graphics-context-set-dashes)

(defcfun (graphics-context-copy "gdk_gc_copy") :void
  (dst-gc (g-object graphics-context))
  (src-gc (g-object graphics-context)))

(export 'graphics-context-copy)

(defcfun (graphics-context-offset "gdk_gc_offset") :void
  (graphics-context (g-object graphics-context))
  (x-offset :int)
  (y-offset :int))

(export 'graphic-context-offset)
