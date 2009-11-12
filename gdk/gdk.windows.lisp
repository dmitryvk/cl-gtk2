(in-package :gdk)

(defcfun gdk-window-new (g-object gdk-window :already-referenced)
  (parent (g-object gdk-window))
  (attributes (g-boxed-foreign gdk-window-attr))
  (attributes-mask gdk-window-attributes-type))

(export 'gdk-window-new)

(defcfun gdk-window-destroy :void
  (window (g-object gdk-window)))

(export 'gdk-window-destroy)

(defcfun (%gdk-window-at-pointer "gdk_window_at_pointer") (g-object gdk-window)
  (win-x (:pointer :int))
  (win-y (:pointer :int)))

(defun gdk-window-at-pointer ()
  (with-foreign-objects ((x :int) (y :int))
    (let ((window (%gdk-window-at-pointer x y)))
      (if window
          (values window (mem-ref x :int) (mem-ref y :int))
          (values nil nil nil)))))

(export 'get-window-at-pointe)

(defcfun gdk-window-show :void
  (window (g-object gdk-window)))

(export 'gdk-window-show)

(defcfun gdk-window-show-unraised :void
  (window (g-object gdk-window)))

(export 'gdk-window-show-unraised)

(defcfun gdk-window-hide :void
  (window (g-object gdk-window)))

(export 'gdk-window-hide)

(defcfun gdk-window-withdraw :void
  (window (g-object gdk-window)))

(export 'gdk-window-withdraw)

(defcfun gdk-window-iconify :void
  (window (g-object gdk-window)))

(export 'gdk-window-iconify)

(defcfun gdk-window-deiconify :void
  (window (g-object gdk-window)))

(export 'gdk-window-deiconify)

(defcfun gdk-window-stick :void
  (window (g-object gdk-window)))

(export 'gdk-window-stick)

(defcfun gdk-window-unstick :void
  (window (g-object gdk-window)))

(export 'gdk-window-unstick)

(defcfun gdk-window-maximize :void
  (window (g-object gdk-window)))

(export 'gdk-window-maximize)

(defcfun gdk-window-unmaximize :void
  (window (g-object gdk-window)))

(export 'gdk-window-unmaximize)

(defcfun gdk-window-fullscreen :void
  (window (g-object gdk-window)))

(export 'gdk-window-unfullscreen)

(defcfun gdk-window-move :void
  (window (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-move)

(defcfun gdk-window-resize :void
  (window (g-object gdk-window))
  (width :int)
  (height :int))

(export 'gdk-window-resize)

(defcfun gdk-window-move-resize :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-window-move-resize)

(defcfun gdk-window-scroll :void
  (window (g-object gdk-window))
  (dx :int)
  (dy :int))

(export 'gdk-window-scroll)

(defcfun gdk-window-move-region :void
  (window (g-object gdk-window))
  (region (g-boxed-foreign region))
  (dx :int)
  (dy :int))

(export 'gdk-window-move-region)

;; TODO: (because of >= 2.18)
;; void                gdk_window_flush                    (GdkWindow *window);
;; gboolean            gdk_window_ensure_native            (GdkWindow *window);

(defcfun gdk-window-reparent :void
  (window (g-object gdk-window))
  (new-parent (g-object gdk-window))
  (x :int)
  (y :int))

(export 'gdk-window-reparent)

(defcfun gdk-window-clear :void
  (window (g-object gdk-window)))

(export 'gdk-window-clear)

(defcfun gdk-window-clear-area :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-window-clear-area)

(defcfun gdk-window-clear-area-e :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'gdk-window-clear-area-e)

(defcfun gdk-window-raise :void
  (window (g-object gdk-window)))

(export 'gdk-window-raise)

(defcfun gdk-window-lower :void
  (window (g-object gdk-window)))

(export 'gdk-window-lower)

(defcfun gdk-window-restack :void
  (window (g-object gdk-window))
  (sibling (g-object gdk-window))
  (above :boolean))

(export 'gdk-window-restack)

(defcfun gdk-window-focus :void
  (window (g-object gdk-window))
  (timestamp :uint32))

(export 'gdk-window-focus)

(defcfun gdk-window-register-dnd :void
  (window (g-object gdk-window)))

(export 'gdk-window-register-dnd)

(defcfun gdk-window-begin-resize-drag :void
  (window (g-object gdk-window))
  (edge gdk-window-edge)
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-resize-drag)

(defcfun gdk-window-begin-move-drag :void
  (window (g-object gdk-window))
  (button :int)
  (root-x :int)
  (root-y :int)
  (timestamp :uint32))

(export 'gdk-window-begin-move-drag)

(defcfun gdk_window_constrain_size :void
  (geometry (g-boxed-foreign geometry))
  (flags gdk-window-hints)
  (width :int)
  (height :int)
  (new-width (:pointer :int))
  (new-height (:pointer :int)))

(defun gdk-window-constrain-size (geometry flags width height)
  (with-foreign-objects ((new-width :int) (new-height :int))
    (gdk_window_constrain_size geometry flags width height new-width new-height)
    (values (mem-ref new-width :int)
            (mem-ref new-height :int))))

(export 'gdk-window-constrain-size)

(defcfun gdk-window-beep :void
  (window (g-object gkd-window)))

(export 'gdk-window-beep)

(defcfun gdk-window-begin-paint-rect :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign rectangle)))

(export 'gdk-window-begin-paint-rect)

(defcfun gdk-window-begin-paint-region :void
  (window (g-object gdk-window))
  (region (g-boxed-foreign region)))

(export 'gdk-window-begin-paint-region)

(defcfun gdk-window-invalidate-rect :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign rectangle))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-rect)

(defcfun gdk-window-invalidate-region :void
  (window (g-object gdk-window))
  (region (g-boxed-foreign region))
  (invalidate-children :boolean))

(export 'gdk-window-invalidate-region)

(defcfun gdk_window_invalidate_maybe_recurse :void
  (window (g-object gdk-window))
  (region (g-boxed-foreign region))
  (recurse-p-fn :pointer)
  (user-data :pointer))

(defcallback gdk-window-invalidate-maybe-recurse-cb :boolean
  ((window (g-object gdk-window))
   (user-data :pointer))
  (let ((fn (stable-pointer-value user-data)))
    (funcall fn window)))

(defun gdk-window-invalidate-maybe-recurse (window region fn)
  (with-stable-pointer (ptr fn)
    (gdk_window_invalidate_maybe_recurse window region (callback gdk-window-invalidate-maybe-recurse-cb) ptr)))

(export 'gdk-window-invalidate-maybe-recurse)

(defcfun gdk-window-get-update-area (g-boxed-foreign region :return)
  (window (g-object gdk-window)))

(export 'gdk-window-get-update-area)

(defcfun gdk-window-freeze-updates :void
  (window (g-object gdk-window)))

(export 'gdk-window-freeze-updates)

(defcfun gdk-window-thaw-updates :void
  (window (g-object gdk-window)))

(export 'gdk-window-thaw-updates)

(defcfun gdk-window-process-all-updates :void)

(export 'gdk-window-process-all-updates)

(defcfun gdk-window-process-updates :void
  (window (g-object gdk-window))
  (update-children :boolean))

(export 'gdk-window-process-updates)

(defcfun gdk-window-set-debug-updates :void
  (settings :boolean))

(export 'gdk-window-set-debug-updates)

(defcfun gdk_window_get_internal_paint_info :void
  (window (g-object gdk-window))
  (real-drawable (:pointer (g-object drawable)))
  (x-offset (:pointer :int))
  (y-offset (:pointer :int)))

(defun gdk-window-get-internal-paint-info (window)
  (with-foreign-objects ((real-drawable :pointer) (x-offset :int) (y-offset :int))
    (gdk_window_get_internal_paint_info window real-drawable x-offset y-offset)
    (values (mem-ref real-drawable '(g-object drawable))
            (mem-ref x-offset :int)
            (mem-ref y-offset :int))))

(export 'gdk-window-get-internal-paint-info)

(defcfun gdk-window-enable-synchronized-configure :void
  (window (g-object gdk-window)))

(export 'gdk-window-enable-synchronized-configure)

(defcfun gdk-window-configure-finished :void
  (window (g-object gdk-window)))

(export 'gdk-window-configure-finished)

;; void                gdk_window_add_filter               (GdkWindow *window,
;;                                                          GdkFilterFunc function,
;;                                                          gpointer data);
;; void                gdk_window_remove_filter            (GdkWindow *window,
;;                                                          GdkFilterFunc function,
;;                                                          gpointer data);
;; GdkFilterReturn     (*GdkFilterFunc)                    (GdkXEvent *xevent,
;;                                                          GdkEvent *event,
;;                                                          gpointer data);

(defcfun gdk-window-shape-combine-mask :void
  (window (g-object window))
  (mask (g-object pixmap))
  (x :int)
  (y :int))

(export 'gdk-window-shape-combine-mask)

(defcfun gdk-window-shape-combine-region :void
  (window (g-object window))
  (region (g-boxed-foreign region))
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-shape-combine-region)

(defcfun gdk-window-set-child-shapes :void
  (window (g-object gdk-window)))

(export 'gdk-window-set-child-shapes)

(defcfun gdk-window-merge-child-shapes :void
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-shapes)

(defcfun gdk-window-input-shape-combine-mask :void
  (window (g-object gdk-window))
  (mask (g-object pixmap))
  (x :int)
  (y :int))

(export 'gdk-window-input-shape-combine-mask)

(defcfun gdk-window-input-shape-combine-region :void
  (window (g-object gdk-window))
  (shape-region (g-boxed-foreign region))
  (offset-x :int)
  (offset-y :int))

(export 'gdk-window-input-shape-combine-region)

(defcfun gdk-window-set-child-input-shapes :void
  (window (g-object gdk-window)))

(export 'gdk-window-set-chid-input-shapes)

(defcfun gdk-window-merge-child-input-shapes :void
  (window (g-object gdk-window)))

(export 'gdk-window-merge-child-input-shapes)

(defcfun gdk-window-set-static-gravities :boolean
  (window (g-object gdk-window))
  (use-static :boolean))

(export 'gdk-window-set-static-gravities)

;; ignored:
;; void                gdk_window_set_hints                (GdkWindow *window,
;;                                                          gint x,
;;                                                          gint y,
;;                                                          gint min_width,
;;                                                          gint min_height,
;;                                                          gint max_width,
;;                                                          gint max_height,
;;                                                          gint flags);

(defcfun gdk-window-set-back-pixmap :void
  (window (g-object gdk-window))
  (pixmap (g-object pixmap))
  (parent-relative :boolean))

(export 'gdk-window-set-back-pixmap)

(defcfun gdk_window_get_geometry :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (width (:pointer :int))
  (height (:pointer :int))
  (depth (:pointer :int)))

(defun gdk-window-get-geometry (window)
  (with-foreign-objects ((x :int) (y :int) (width :int) (height :int) (depth :int))
    (gdk_window_get_geometry window x y width height depth)
    (values (mem-ref x :int)
            (mem-ref y :int)
            (mem-ref width :int)
            (mem-ref height :int)
            (mem-ref depth :int))))

(export 'gdk-window-get-geometry)

(defcfun gdk-window-set-geometry-hints :void
  (window (g-object gdk-window))
  (geometry (g-boxed-foreign geometry))
  (geometry-mask gdk-window-hints))

(export 'gdk-window-set-geometry-hints)

(defcfun gdk_window_get_position :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-position (window)
  (with-foreign-objects ((x :int) (y :int))
    (gdk_window_get_position window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gdk-window-get-position)

(defcfun gdk_window_get_root_origin :void
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-root-origin (window)
  (with-foreign-objects ((x :int) (y :int))
    (gdk_window_get_root_origin window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gdk-window-get-root-origin)

(defcfun gdk_window_get_frame_extents :void
  (window (g-object gdk-window))
  (rectangle (g-boxed-foreign rectangle)))

(defun gdk-window-get-frame-extents (window)
  (let ((rectangle (make-rectangle)))
    (gdk_window_get_frame_extents window rectangle)
    rectangle))

(export 'gdk-window-get-frame-extents)

(defcfun gdk_window_get_origin :int
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gdk-window-get-origin (window)
  (with-foreign-objects ((x :int) (y :int))
    (gdk_window_get_origin window x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'gdk-window-get-origin)

;; ignored
;; gboolean            gdk_window_get_deskrelative_origin  (GdkWindow *window,
;;                                                          gint *x,
;;                                                          gint *y);

(defcfun gdk_window_get_root_coords :void
  (window (g-object gdk-window))
  (x :int)
  (y :int)
  (root-x :int)
  (root-y :int))

(defun gdk-window-get-root-coords (window x y)
  (with-foreign-objects ((root-x :int) (root-y :int))
    (gdk_window_get_root_coords window x y root-x root-y)
    (values (mem-ref root-x :int) (mem-ref root-y :int))))

(defcfun gdk_window_get_pointer (g-object gdk-window)
  (window (g-object gdk-window))
  (x (:pointer :int))
  (y (:pointer :int))
  (mask (:pointer modifier-type)))

(defun gdk-window-get-pointer (window)
  (with-foreign-objects ((x :int) (y :int) (mask 'modifier-type))
    (let ((w (gdk_window_get_pointer window x y mask)))
      (values w
              (mem-ref x :int)
              (mem-ref y :int)
              (mem-ref mask 'modifier-type)))))

(export 'gdk-window-get-pointer)

(defcfun gdk_window_get_decorations :boolean
  (window (g-object gdk-window))
  (decorations (:pointer gdk-w-m-decoration)))

(defun gdk-window-get-decorations (window)
  (with-foreign-object (decorations 'gdk-w-m-decoration)
    (gdk_window_get_decorations window decorations)
    (mem-ref decorations 'gdk-w-m-decoration)))

(defcfun gdk-window-set-icon :void
  (window (g-object gdk-window))
  (icon-window (g-object gdk-window))
  (pixmap (g-object pixmap))
  (mask (g-object mask)))

(export 'gdk-window-set-icon)

;; ignored
;; GList *             gdk_window_get_toplevels            (void);

(defcfun gdk-get-default-root-window (g-object gdk-window))

(export 'gdk-get-default-root-window)

;; TODO
;;                     GdkPointerHooks;
;; GdkPointerHooks *   gdk_set_pointer_hooks               (const GdkPointerHooks *new_hooks);

(defcfun gdk-offscreen-window-get-pixmap (g-object pixmap)
  (window (g-object gdk-window)))

(export 'gdk-offscreen-window-get-pixmap)

(defcfun (gdk-offscreen-window-embedder "gdk_offscreen_window_get_embedder") (g-object gdk-window)
  (window (g-object gdk-window)))

(defcfun gdk_offscreen_window_set_embedder :void
  (window (g-object gdk-window))
  (embedder (g-object gdk-window)))

(defun (setf gdk-offscreen-window-embedder) (new-value window)
  (gdk_offscreen_window_set_embedder window new-value))

(export 'gdk-offscreen-window-embedder)

(defcfun gdk-window-geometry-changed :void
  (window (g-object gdk-window)))

(export 'gdk-window-geometry-changed)

(defcfun gdk-window-redirect-to-drawable :void
  (window (g-object gdk-window))
  (drawable (g-object drawable))
  (src-x :int)
  (src-y :int)
  (dest-x :int)
  (dest-y :int)
  (width :int)
  (height :int))

(export 'gdk-window-redirect-to-drawable)

(defcfun gdk-window-remove-redirection :void
  (window (g-object gdk-window)))

(export 'gdk-window-remove-redirection)
