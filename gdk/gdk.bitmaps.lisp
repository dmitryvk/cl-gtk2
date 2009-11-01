(in-package :gdk)

(defcfun (pixmap-new "gdk_pixmap_new") (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (width :int)
  (height :int)
  (depth :int))

(export 'pixmap-new)

(defcfun (bitmap-create-from-data "gdk_bitmap_create_from_data") (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (data :pointer)
  (width :int)
  (height :int))

(export 'bitmap-create-from-data)

(defcfun (pixmap-create-from-data "gdk_pixmap_create_from_data") (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (data :pointer)
  (width :int)
  (height :int)
  (depth :int)
  (fg-color (g-boxed-foreign color))
  (bg-color (g-boxed-foreign color)))

(export 'pixmap-create-from-data)

(defcfun gdk-pixmap-create-from-xpm (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (mask :pointer)
  (transparent-color (g-boxed-foreign color))
  (filename :string))

(defcfun gdk-pixmap-colormap-create-from-xpm (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (colormap (g-object colormap))
  (mask :pointer)
  (transparent-color (g-boxed-foreign color))
  (filename :string))

(defcfun gdk-pixmap-create-from-xpm-d (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (mask :pointer)
  (transparent-color (g-boxed-foreign color))
  (data (:pointer :pointer)))

(defun gdk-pixmap-create-from-xpm-d-1 (drawable mask transparent-color data)
  (let ((n (length data)))
    (with-foreign-object (data-ptr :pointer n)
      (let ((i 0))
        (map nil
             (lambda (str)
               (setf (mem-aref data-ptr :pointer i) (cffi:foreign-string-alloc str))
               (incf i))
             data))
      (gdk-pixmap-create-from-xpm-d drawable mask transparent-color data-ptr))))

(defcfun gdk-pixmap-colormap-create-from-xpm-d (g-object pixmap :already-referenced)
  (drawable (g-object drawable))
  (colormap (g-object colormap))
  (mask :pointer)
  (transparent-color (g-boxed-foreign color))
  (data (:pointer :pointer)))

(defun gdk-pixmap-colormap-create-from-xpm-d-1 (drawable colormap mask transparent-color data)
  (let ((n (length data)))
    (with-foreign-object (data-ptr :pointer n)
      (let ((i 0))
        (map nil
             (lambda (str)
               (setf (mem-aref data-ptr :pointer i) (cffi:foreign-string-alloc str))
               (incf i))
             data))
      (gdk-pixmap-colormap-create-from-xpm-d drawable colormap mask transparent-color data-ptr))))

(defun pixmap-create-from-xpm (drawable transparent-color &key (colormap nil colormap-p) (filename nil filename-p) (xpm-data nil xpm-p))
  (unless (or filename-p xpm-p)
    (error "FILENAME or XPM-DATA must be specified"))
  (when (and filename-p xpm-p)
    (error "FILENAME and XPM-DATA may not be specified at the same time"))
  (with-foreign-object (mask-ptr :pointer)
    (let ((pixmap (if filename-p
                      (if colormap-p
                          (gdk-pixmap-colormap-create-from-xpm drawable colormap mask-ptr transparent-color filename)
                          (gdk-pixmap-create-from-xpm drawable mask-ptr transparent-color filename))
                      (if colormap-p
                          (gdk-pixmap-colormap-create-from-xpm-d-1 drawable colormap mask-ptr transparent-color xpm-data)
                          (gdk-pixmap-create-from-xpm-d-1 drawable mask-ptr transparent-color xpm-data)))))
      (values pixmap (convert-from-foreign mask-ptr '(g-object pixmap :already-referenced))))))

(export 'pixmap-create-from-xpm)
