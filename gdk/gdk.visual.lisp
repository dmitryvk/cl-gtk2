(in-package :gdk)

(defcfun (%gdk-query-depths "gdk_query_depths") :void
  (depths (:pointer (:pointer :int)))
  (count (:pointer :int)))

(defun gdk-query-depths ()
  (with-foreign-objects ((count-r :int) (depths-r :pointer))
    (%gdk-query-depths depths-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with depths = (mem-ref depths-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref depths :int i)))))

(export 'gdk-query-depths)

(defcfun (%gdk-query-visual-types "gdk_query_visual_types") :void
  (depths (:pointer (:pointer visual-type)))
  (count (:pointer :int)))

(defun gdk-query-visual-types ()
  (with-foreign-objects ((count-r :int) (types-r 'visual-type))
    (%gdk-query-visual-types types-r count-r)
    (iter (with count = (mem-ref count-r :int))
          (with types = (mem-ref types-r :pointer))
          (for i from 0 below count)
          (collect (mem-aref types 'visual-type i)))))

(export 'gdk-query-visual-types)

(defcstruct gdk-visual-cstruct
  (parent-instance gobject.ffi::%g-object)
  (visual-type visual-type)
  (depth :int)
  (byte-order byte-order)
  (colormap-size :int)
  (bits-per-rgb :int)
  (red-mask :uint32)
  (red-shift :int)
  (red-prec :int)
  (green-mask :uint32)
  (green-shift :int)
  (green-prec :int)
  (blue-mask :uint32)
  (blue-shift :int)
  (blue-prec :int))

(defmacro def-visual-accessor (slot)
  `(defun ,(intern (format nil "~A-GET-~A" (symbol-name 'gdk-visual) (symbol-name slot))) (visual)
     (foreign-slot-value (pointer visual) 'gdk-visual-cstruct ',slot)))

(def-visual-accessor visual-type)
(def-visual-accessor depth)
(def-visual-accessor byte-order)
(def-visual-accessor colormap-size)
(def-visual-accessor bits-per-rgb)
(def-visual-accessor red-mask)
(def-visual-accessor red-shift)
(def-visual-accessor red-prec)
(def-visual-accessor green-mask)
(def-visual-accessor green-shift)
(def-visual-accessor green-prec)
(def-visual-accessor blue-mask)
(def-visual-accessor blue-shift)
(def-visual-accessor blue-prec)

(defcfun (list-visuals "gdk_list_visuals") (glib:glist (g-object visual) :free-from-foreign t))

(export 'list-visuals)

(defcfun (visual-get-best-depth "gdk_visual_get_best_depth") :int)
(export 'visual-get-best-depth)

(defcfun (visual-get-best-type "gdk_visual_get_best_type") visual-type)
(export 'visual-get-best-type)

(defcfun (visual-get-system "gdk_visual_get_system") (g-object visual))
(export 'visual-get-system)

(defcfun (visual-get-best "gdk_visual_get_best") (g-object visual))
(export 'visual-get-best)

(defcfun (visual-get-best-with-depth "gdk_visual_get_best_with_depth") (g-object visual)
  (depth :int))
(export 'visual-get-best-with-depth)

(defcfun (visual-get-best-with-both "gdk_visual_get_best_with_both") (g-object visual)
  (depth :int)
  (visual-type visual-type))
(export 'visual-get-best-with-both)

(defmethod print-object ((visual visual) stream)
  (print-unreadable-object (visual stream :type t :identity t)
    (format stream "~S at ~S bpp" (visual-visual-type visual) (visual-depth visual))))
