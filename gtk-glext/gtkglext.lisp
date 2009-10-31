(in-package :gtkglext)

;; Initialization

(defcfun gtk-gl-init :void
  (argc :pointer)
  (argv :pointer))

(defun gl-init ()
  (gtk-gl-init (null-pointer) (null-pointer))
  (glut:init))

(at-init () (gl-init))
(at-finalize () (setf cl-glut::*glut-initialized-p* nil))

;; Query

(defcfun (%gdk-gl-query-extension "gdk_gl_query_extension") :boolean)
(defcfun (%gdk-gl-query-extension-for-display "gdk_gl_query_extension_for_display") :boolean
  (display (g-object display)))

(defun gdk-gl-query-extension (&optional (display nil display-provided-p))
  (if display-provided-p
      (%gdk-gl-query-extension-for-display display)
      (%gdk-gl-query-extension)))

(export 'gdk-gl-query-extension)

(defcfun (%gdk-gl-query-version "gdk_gl_query_version") :boolean
  (major (:pointer :int))
  (minor (:pointer :int)))

(defcfun (%gdk-gl-query-version-for-display "gdk_gl_query_version_for_display") :boolean
  (display (g-object display))
  (major (:pointer :int))
  (minor (:pointer :int)))

(defun gdk-gl-query-version (&optional (display nil display-provided-p))
  (with-foreign-objects ((major :int) (minor :int))
    (if display-provided-p
        (%gdk-gl-query-version-for-display display major minor)
        (%gdk-gl-query-version major minor))
    (list (mem-ref major :int) (mem-ref minor :int))))

(export 'gdk-gl-query-version)

(defcfun gdk-gl-query-gl-extension :boolean
  (extension-name :string))

(export 'gdk-gl-query-gl-extension)

;; Tokens

(define-g-enum "GdkGLConfigAttrib" gdk-gl-config-attrib (:export t :type-initializer "gdk_gl_config_attrib_get_type")
  (:use-gl 1) (:buffer-size 2) (:level 3)
  (:rgba 4) (:doublebuffer 5) (:stereo 6)
  (:aux-buffers 7) (:red-size 8) (:green-size 9)
  (:blue-size 10) (:alpha-size 11) (:depth-size 12)
  (:stencil-size 13) (:accum-red-size 14)
  (:accum-green-size 15) (:accum-blue-size 16)
  (:accum-alpha-size 17) (:config-caveat 32)
  (:x-visual-type 34) (:transparent-type 35)
  (:transparent-index-value 36)
  (:transparent-red-value 37)
  (:transparent-green-value 38)
  (:transparent-blue-value 39)
  (:transparent-alpha-value 40)
  (:drawable-type 32784) (:render-type 32785)
  (:x-renderable 32786) (:fbconfig-id 32787)
  (:max-pbuffer-width 32790)
  (:max-pbuffer-height 32791)
  (:max-pbuffer-pixels 32792) (:visual-id 32779)
  (:screen 32780) (:sample-buffers 100000)
  (:samples 100001))

(define-g-enum "GdkGLRenderType" gdk-gl-render-type (:export t :type-initializer "gdk_gl_render_type_get_type")
  (:rgba-type 32788) (:color-index-type 32789))

;; Frame buffer configuration

(define-g-flags "GdkGLConfigMode" gdk-gl-config-mode (:export t :type-initializer "gdk_gl_config_mode_get_type")
  (:rgb 0) (:rgba 0) (:index 1) (:single 0)
  (:double 2) (:stereo 4) (:alpha 8) (:depth 16)
  (:stencil 32) (:accum 64) (:multisample 128))

(define-g-object-class "GdkGLConfig" gdk-gl-config (:export t :type-initializer "gdk_gl_config_get_type")
  ((:cffi screen gdk-gl-config-screen (g-object screen) "gdk_gl_config_get_screen" nil)
   (:cffi colormap gdk-gl-config-colormap (g-object colormap) "gdk_gl_config_get_colormap" nil)
   (:cffi visual gdk-gl-config-visual (g-object visual) "gdk_gl_config_get_visual" nil)
   (:cffi depth gdk-gl-config-depth :int "gdk_gl_config_get_depth" nil)
   (:cffi layer-plane gdk-gl-config-layer-plane :int "gdk_gl_config_get_layer_plane" nil)
   (:cffi n-aux-buffers gdk-gl-config-n-aux-buffers :int "gdk_gl_config_get_n_aux_buffers" nil)
   (:cffi n-sample-buffers gdk-gl-config-n-sample-buffers :int "gdk_gl_config_get_n_sample_buffers" nil)
   (:cffi is-rgba gdk-gl-config-is-rgba :boolean "gdk_gl_config_is_rgba" nil)
   (:cffi is-double-buffered gdk-gl-config-is-double-buffered :boolean "gdk_gl_config_is_double_buffered" nil)
   (:cffi is-stereo gdk-gl-config-is-stereo :boolean "gdk_gl_config_is_stereo" nil)
   (:cffi has-alpha gdk-gl-config-has-alpha :boolean "gdk_gl_config_has_alpha" nil)
   (:cffi has-depth-buffer gdk-gl-config-has-depth-buffer :boolean "gdk_gl_config_has_depth_buffer" nil)
   (:cffi has-stencil-buffer gdk-gl-config-has-stencil-buffer :boolean "gdk_gl_config_has_stencil_buffer" nil)
   (:cffi has-accum-buffer gdk-gl-config-has-accum-buffer :boolean "gdk_gl_config_has_accum_buffer" nil)))

(defcfun (%gdk-gl-config-get-attrib "gdk_gl_config_get_attrib") :boolean
  (gl-config (g-object gdk-gl-config))
  (attribute gdk-gl-config-attrib)
  (return-value (:pointer :int)))

(defun gdk-gl-config-attrib (gl-config attribute)
  (with-foreign-object (v :int)
    (when (%gdk-gl-config-get-attrib gl-config attribute v)
      (mem-ref v :int))))

(defcfun gdk-gl-config-new-by-mode :pointer
  (mode gdk-gl-config-mode))

(defcfun gdk-gl-config-new-by-mode-for-screen :pointer
  (screen (g-object screen))
  (mode gdk-gl-config-mode))

(defcfun (%gdk-gl-config-new-for-screen "gdk_gl_config_new_for_screen") :pointer
  (screen (g-object screen))
  (attrib-list (:pointer :int)))

(defun gdk-gl-config-new-for-screen (screen attrib-plist)
  (with-foreign-object (attributes :int (+ (length attrib-plist) 2))
    (iter (for (attr value) on attrib-plist by #'cddr)
          (for i from 0 by 2)
          (setf (mem-aref attributes 'gdk-gl-config-attrib i) attr
                (mem-aref attributes :int (1+ i)) value))
    (%gdk-gl-config-new-for-screen screen attributes)))

(defmethod make-instance ((config-class (eql (find-class 'gdk-gl-config)))
                          &rest initargs
                          &key pointer screen mode attrib-plist)
  (cond
    (pointer (call-next-method))
    (mode (assert (not attrib-plist) nil "MODE and ATTRIB-LIST initargs can not be combined")
          (let ((p (if screen
                    (gdk-gl-config-new-by-mode-for-screen screen mode)
                    (gdk-gl-config-new-by-mode mode))))
            (apply #'call-next-method config-class :pointer p initargs)))
    (attrib-plist (assert screen nil "SCREEN initargs must be specified when ATTRIB-LIST is specified")
                  (let ((p (gdk-gl-config-new-for-screen screen attrib-plist)))
                    (apply #'call-next-method config-class :pointer p initargs)))
    (t (error "MODE or (MODE and SCREEN) or (SCREEN and ATTRIB-PLIST) initargs must be specified"))))

;; Render context

(define-g-object-class "GdkGLContext" gdk-gl-context  (:export t :type-initializer "gdk_gl_context_get_type")
  ((:cffi drawable gdk-gl-context-drawable (g-object gdk-gl-drawable) "gdk_gl_context_get_gl_drawable" nil)
   (:cffi gl-config gdk-gl-context-config (g-object gdk-gl-config) "gdk_gl_context_get_gl_config" nil)
   (:cffi share-list gdk-gl-context-share-list (g-object gdk-gl-context) "gdk_gl_context_get_share_list" nil)
   (:cffi is-direct gdk-gl-context-is-direct :boolean "gdk_gl_context_is_direct" nil)
   (:cffi render-type gdk-gl-context-get-render-type gdk-gl-render-type "gdk_gl_context_get_render_type" nil)))

(defcfun (gdk-gl-context-current "gdk_gl_context_get_current") (g-object gdk-gl-context))

(export 'gdk-gl-context-current)

(defcfun gdk-gl-context-new :pointer
  (gl-drawable (g-object gdk-gl-drawable))
  (share-list (g-object gdk-gl-context))
  (direct-p :boolean)
  (render-type gdk-gl-render-type))

(defmethod make-instance ((context-class (eql (find-class 'gdk-gl-context)))
                          &rest initargs
                          &key pointer gl-drawable share-list direct-p (render-type :rgba-type))
  (cond
    (pointer (call-next-method))
    (gl-drawable (let ((p (gdk-gl-context-new gl-drawable share-list direct-p render-type)))
                   (apply #'call-next-method context-class :pointer p initargs)))
    (t (error "At least GL-DRAWABLE initarg must be specified"))))

(defcfun (gdk-gl-context-copy-state "gdk_gl_context_copy") :boolean
  (dst-gl-context (g-object gdk-gl-context))
  (src-gl-context (g-object gdk-gl-context))
  (attribs-mask :int)) ;;TODO: more specific enum type

(export 'gdk-gl-context-copy-state)

;; Rendering surface

(define-g-interface "GdkGLDrawable" gdk-gl-drawable (:export t :type-initializer "gdk_gl_drawable_get_type")
  (:cffi is-double-buffered gdk-gl-drawable-is-double-buffered :boolean "gdk_gl_drawable_is_double_buffered" nil)
  (:cffi config gdk-gl-drawable-config (g-object gdk-gl-config) "gdk_gl_drawable_get_gl_config" nil)
  (:cffi size gdk-gl-drawable-size list gdk-gl-drawable-size nil))

(defcfun (%gdk-gl-drawable-get-size "gdk_gl_drawable_get_size") :void
  (gl-drawable (g-object gl-drawable))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun gdk-gl-drawable-get-size (gl-drawable)
  (with-foreign-objects ((width :int) (height :int))
    (%gdk-gl-drawable-get-size gl-drawable width height)
    (list (mem-ref width :int) (mem-ref height :int))))

(defcfun (gdk-gl-drawable-current "gdk_gl_drawable_get_current") (g-object gdk-gl-drawable))

(export 'gdk-gl-drawable-current)

(defcfun gdk-gl-drawable-swap-buffers :void
  (gl-drawable (g-object gdk-gl-drawable)))

(export 'gdk-gl-drawable-swap-buffers)

(defcfun gdk-gl-drawable-wait-gl :void
  (gl-drawable (g-object gdk-gl-drawable)))

(export 'gdk-gl-drawable-wait-gl)

(defcfun gdk-gl-drawable-wait-gdk :void
  (gl-drawable (g-object gdk-gl-drawable)))

(export 'gdk-gl-drawable-wait-gdk)

(defcfun gdk-gl-drawable-gl-begin :boolean
  (gl-drawable (g-object gdk-gl-drawable))
  (gl-context (g-object gdk-gl-context)))

(export 'gdk-gl-drawable-gl-begin)

(defcfun gdk-gl-drawable-gl-end :void
  (gl-drawable (g-object gdk-gl-drawable)))

(export 'gdk-gl-drawable-gl-end)

;; OpenGL Pixmap

(define-g-object-class "GdkGLPixmap" gdk-gl-pixmap (:superclass drawable :export t :interfaces ("GdkGLDrawable"))
  ())

(defcfun gdk-gl-pixmap-new :pointer
  (gl-config (g-object gdk-gl-config))
  (pixmap (g-object pixmap))
  (attrib-list-unused (:pointer :int)))

(defmethod make-instance ((pixmap-class (eql (find-class 'gdk-gl-pixmap))) &rest initargs &key pointer gl-config pixmap)
  (cond
    (pointer (call-next-method))
    ((and gl-config pixmap) (let ((p (gdk-gl-pixmap-new gl-config pixmap (null-pointer))))
                              (apply #'call-next-method pixmap-class :pointer p initargs)))
    (t (error "POINTER or (GL-CONFIG and PIXMAP) initargs must be specified"))))

(defcfun (%gdk-pixmap-set-gl-capability "gdk_pixmap_set_gl_capability") (g-object gdk-gl-pixmap)
  (pixmap (g-object pixmap))
  (gl-config (g-object gdk-gl-config))
  (attrib-list-unused (:pointer :int)))

(defun pixmap-set-gl-capability (pixmap gl-config)
  (%gdk-pixmap-set-gl-capability pixmap gl-config (null-pointer)))

(export 'pixmap-set-gl-capability)

(defcfun (pixmap-unset-gl-capability "gdk_pixmap_unset_gl_capability") :void
  (pixmap (g-object pixmap)))

(export 'pixmap-unset-gl-capability)

(defcfun (pixmap-is-gl-capable "gdk_pixmap_is_gl_capable") :boolean
  (pixmap (g-object pixmap)))

(export 'pixmap-is-gl-capable)

(defcfun (pixmap-gl-pixmap "gdk_pixmap_get_gl_pixmap") (g-object gdk-gl-pixmap)
  (pixmap (g-object pixmap)))

(export 'pixmap-gl-pixmap)

;; OpenGL Window

(define-g-object-class "GdkGLWindow" gdk-gl-window (:superclass drawable :export t :interfaces ("GdkGLDrawable"))
  ((:cffi window gdk-gl-window-gdk-window (g-object gdk-window) "gdk_gl_window_get_type" nil)))

(defcfun gdk-gl-window-new :pointer
  (gl-config (g-object gdk-gl-config))
  (window (g-object gdk-window))
  (attrib-list-unused (:pointer :int)))

(defmethod make-instance ((window-class (eql (find-class 'gdk-gl-window)))
                          &rest initargs
                          &key pointer gl-config window)
  (cond
    (pointer (call-next-method))
    ((and gl-config window) (let ((p (gdk-gl-window-new gl-config window (null-pointer))))
                              (apply #'call-next-method window-class :pointer p initargs)))
    (t (error "POINTER or (GL-CONFIG and WINDOW) initargs must be specified"))))

(defcfun (%gdk-window-set-gl-capability "gdk_window_set_gl_capability") (g-object gdk-gl-window)
  (window (g-object gdk-window))
  (gl-config (g-object gdk-gl-config))
  (attrib-list-unused (:pointer :int)))

(defun gdk-window-set-gl-capability (window gl-config)
  (%gdk-window-set-gl-capability window gl-config (null-pointer)))

(export 'gdk-window-set-gl-capability)

(defcfun gdk-window-unset-gl-capability :void
  (window (g-object gdk-window)))

(export 'gdk-window-unset-gl-capability)

(defcfun gdk-window-is-gl-capable :boolean
  (window (g-object gdk-window)))

(export 'gdk-window-is-gl-capable)

(defcfun (gdk-window-gl-window "gdk_window_get_gl_window") (g-object gdk-gl-window)
  (window (g-object gdk-window)))

(export 'gdk-window-gl-window)

;; Font Rendering

;; TODO: gdk_gl_font_use_pango_font

;; TODO: gdk_gl_font_use_pango_font_for_display

;; Geometric Object Rendering

(defcfun gdk-gl-draw-cube :void
  (solid-p :boolean)
  (size :double))

(export 'gdk-gl-draw-cube)

(defcfun gdk-gl-draw-sphere :void
  (solid-p :boolean)
  (radius :double)
  (slices :int)
  (stacks :int))

(export 'gdk-gl-draw-sphere)

(defcfun gdk-gl-draw-cone :void
  (solid-p :boolean)
  (base :double)
  (height :double)
  (slices :int)
  (stacks :int))

(export 'gdk-gl-draw-cone)

(defcfun gdk-gl-draw-torus :void
  (solid-p :boolean)
  (inner-radius :double)
  (outer-radius :double)
  (n-sides :int)
  (n-rings :int))

(export 'gdk-gl-draw-torus)

(defcfun gdk-gl-draw-tetrahedron :void
  (solid-p :boolean))

(export 'gdk-gl-draw-tetrahedron)

(defcfun gdk-gl-draw-octahedron :void
  (solid-p :boolean))

(export 'gdk-gl-draw-octahedron)

(defcfun gdk-gl-draw-dodecahedron :void
  (solid-p :boolean))

(export 'gdk-gl-draw-dodecahedron)

(defcfun gdk-gl-draw-icosahedron :void
  (solid-p :boolean))

(export 'gdk-gl-draw-icosahedron)

(defcfun gdk-gl-draw-teapot :void
  (solid-p :boolean)
  (scale :double))

(export 'gdk-gl-draw-teapot)

;; OpenGL-Capable Widget

(defcfun gtk-widget-set-gl-capability :boolean
  (widget (g-object widget))
  (gl-config (g-object gdk-gl-config))
  (share-list (g-object gdk-gl-config))
  (direct-p :boolean)
  (render-type gdk-gl-render-type))

(export 'gtk-widget-set-gl-capability)

(defcfun gtk-widget-is-gl-capable :boolean
  (widget (g-object widget)))

(export 'gtk-widget-is-gl-capable)

(defcfun (gtk-widget-gl-config "gtk_widget_get_gl_config") (g-object gdk-gl-config)
  (widget (g-object widget)))

(export 'gtk-widget-gl-config)

(defcfun gtk-widget-create-gl-context (g-object gdk-gl-context)
  (widget (g-object widget))
  (share-list (g-object gdk-gl-context))
  (direct-p :boolean)
  (render-type gdk-gl-render-type))

(export 'gtk-widget-create-gl-context)

(defcfun (gtk-widget-gl-context "gtk_widget_get_gl_context") (g-object gdk-gl-context)
  (widget (g-object widget)))

(export 'gtk-widget-gl-context)

(defcfun (gtk-widget-gl-window "gtk_widget_get_gl_window") (g-object gdk-gl-window)
  (widget (g-object widget)))

(export 'gtk-widget-gl-window)

(defun get-gl-config-ptr ()
  (let ((cfg (gdk-gl-config-new-by-mode '(:rgba :depth :double))))
    (if (null-pointer-p cfg)
	(let ((cfg (gdk-gl-config-new-by-mode '(:rgba :depth))))
	  (warn "No double buffered visual found.  Trying single-buffered.")
	  (if (null-pointer-p cfg)
	      (error "No OpenGL capable visual found.")
	      cfg))
	cfg)))

(defun get-gl-config ()
  (make-instance 'gdk-gl-config :pointer (get-gl-config-ptr)))

(defvar *gl-config* nil)

(at-init () (setf *gl-config* (get-gl-config)))

(defmacro with-gensyms (syms &body body)
  "Paul Graham ON LISP pg 145. Used in macros to avoid variable capture."
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
          syms)
     ,@body))

(defmacro bwhen ((bindvar boundform) &body body)
  `(let ((,bindvar ,boundform))
      (when ,bindvar
        ,@body)))

(defmacro with-gl-context ((widget &key (swap-buffers-p t)) &rest body)
  (with-gensyms (drawable context swap-p w)
    `(let ((,swap-p ,swap-buffers-p)
	   (,w ,widget))
       (let ((,context (gtk-widget-gl-context ,w))
             (,drawable (gtk-widget-gl-window ,w)))
         (if (and ,context ,drawable (gdk-gl-drawable-gl-begin ,drawable ,context))
             (unwind-protect
                  (progn
                    ,@body)
               (progn
                 (when ,swap-p
                   (when (gdk-gl-drawable-is-double-buffered ,drawable)
                     (gdk-gl-drawable-swap-buffers ,drawable)))
                 (gdk-gl-drawable-gl-end ,drawable)))
             (format t "gl-begin failed ~A ~A ~A~%" ,w ,drawable ,context))))))

(defmacro with-matrix-mode ((mode) &body body)
  `(progn
     (gl:matrix-mode ,mode)
     (gl:load-identity)
     ,@body
     (gl:matrix-mode :modelview)
     (gl:load-identity)))
