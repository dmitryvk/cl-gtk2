(in-package :gtk)

; TODO: GtkWidget

(defun widget-flags (widget)
  (convert-from-foreign (gtk-object-flags-as-integer widget) 'widget-flags))

(defun (setf widget-flags) (new-value widget)
  (setf (gtk-object-flags-as-integer widget)
        (convert-to-foreign new-value 'widget-flags))
  new-value)

(export 'widget-flags)

(defcstruct %gtk-widget
  (:object %gtk-object)
  (:private-flags :uint16)
  (:state :uint8)
  (:saved-state :uint8)
  (:name (:pointer :char))
  (:style :pointer)
  (:requisition requisition-cstruct)
  (:allocation allocation-cstruct)
  (:window :pointer)
  (:parent :pointer))

(defun widget-state (widget)
  (convert-from-foreign (foreign-slot-value (pointer widget) '%gtk-widget :state) 'state-type))

(export 'widget-state)

(defun widget-saved-state (widget)
  (convert-from-foreign (foreign-slot-value (pointer widget) '%gtk-widget :saved-state) 'state-type))

(export 'widget-saved-state)

(defmacro widget-p-fn (type)
  (let ((name (intern (format nil "WIDGET-~A-P" (symbol-name type)) (find-package :gtk))))
    `(progn (defun ,name (widget)
              (member ,type (widget-flags widget)))
            (export ',name))))

(widget-p-fn :toplevel)
(widget-p-fn :no-window)
(widget-p-fn :realized)
(widget-p-fn :mapped)
(widget-p-fn :visible)
(widget-p-fn :sensitive)
(widget-p-fn :parent-sensitive)
(widget-p-fn :can-focus)
(widget-p-fn :has-focus)
(widget-p-fn :can-default)
(widget-p-fn :has-default)
(widget-p-fn :has-grab)
(widget-p-fn :rc-style)
(widget-p-fn :composite-child)
(widget-p-fn :no-reparent)
(widget-p-fn :app-paintable)
(widget-p-fn :receives-default)
(widget-p-fn :double-buffered)
(widget-p-fn :no-show-all)

(defcfun (widget-unparent "gtk_widget_unparent") :void
  (widget g-object))

(export 'widget-unparent)

(defcfun gtk-widget-show :void
  (widget g-object))

(defcfun gtk-widget-show-all :void
  (widget g-object))

(defun widget-show (widget &key (all t))
  (if all
      (gtk-widget-show-all widget)
      (gtk-widget-show widget)))

(export 'widget-show)

(defcfun (widget-show-now "gtk_widget_show_now") :void
  (widget g-object))

(export 'widget-show-now)

(defcfun gtk-widget-hide :void
  (widget g-object))

(defcfun gtk-widget-hide-all :void
  (widget g-object))

(defun widget-hide (widget &key (all t))
  (if all
      (gtk-widget-hide-all widget)
      (gtk-widget-hide widget)))

(export 'widget-hide)

(defcfun (widget-map "gtk_widget_map") :void
  (widget g-object))

(export 'widget-map)

(defcfun (widget-unmap "gtk_widget_unmap") :void
  (widget g-object))

(export 'widget-unmap)

(defcfun (widget-realize "gtk_widget_realize") :void
  (width g-object))

(export 'widget-realize)

(defcfun (widget-unrealize "gtk_widget_unrealize") :void
  (width g-object))

(export 'widget-unrealize)

(defcfun (widget-queue-draw "gtk_widget_queue_draw") :void
  (widget (g-object widget)))

(export 'widget-queue-draw)

(defcfun (widget-queue-resize "gtk_widget_queue_resize") :void
  (widget (g-object widget)))

(export 'widget-queue-resize)

(defcfun (widget-queue-resize-no-redraw "gtk_widget_queue_resize_no_redraw") :void
  (widget (g-object widget)))

(export 'widget-queue-resize-no-redraw)

; TODO: gtk_widget_get_child_requisition

; TODO: gtk_widget_size_allocate

(defcfun (widget-add-accelerator "gtk_widget_add_accelerator") :void
  (widget g-object)
  (accel-signal :string)
  (accel-group g-object)
  (accel-key :uint)
  (accel-mods modifier-type)
  (accel-flags accel-flags))

(export 'widget-add-accelerator)

(defcfun (widget-remove-accelerator "gtk_widget_remove_accelerator") :void
  (widget g-object)
  (accel-group g-object)
  (accel-key :uint)
  (accel-mods modifier-type))

(export 'widget-remove-accelerator)

(defcfun (widget-set-accel-path "gtk_widget_set_accel_path") :void
  (widget g-object)
  (accel-path :string)
  (accel-group g-object))

(export 'widget-set-accel-path)

; TODO: gtk_widget_list_accel_closures

(defcfun gtk-widget-can-activate-accel :boolean
  (widget g-object)
  (signal-id :uint))

(defun widget-can-activate-accel (widget signal)
  (when (stringp signal) (setf signal (g-signal-lookup signal (g-type-from-object widget))))
  (gtk-widget-can-activate-accel widget signal))

(export 'widget-can-activate-accel)

(defcfun (widget-event "gtk_widget_event") :boolean
  (widget (g-object widget))
  (event (g-boxed-foreign event)))

(export 'widget-event)

(defcfun (widget-activate "gtk_widget_activate") :boolean
  (widget g-object))

(export 'widget-activate)

(defcfun (widget-reparent "gtk_widget_reparent") :void
  (widget g-object)
  (new-parent g-object))

(export 'widget-reparent)

(defcfun gtk-widget-intersect :boolean
  (widget g-object)
  (area (g-boxed-foreign rectangle))
  (intersection (g-boxed-foreign rectangle)))

(defun widget-intersect (widget rectangle)
  (let ((result (make-rectangle)))
    (when (gtk-widget-intersect widget rectangle result)
      result)))

(export 'widget-intersect)

(defcfun (widget-focus-p "gtk_widget_is_focus") :boolean
  (widget g-object))

(export 'widget-focus-p)

(defcfun (widget-grab-focus "gtk_widget_grab_focus") :void
  (widget g-object))

(export 'widget-grab-focus)

(defcfun (widget-grab-default "gtk_widget_grab_default") :void
  (widget g-object))

(export 'widget-grab-default)

(defcfun (widget-set-state "gtk_widget_set_state") :void
  (widget (g-object widget))
  (state state-type))

(export 'widget-set-state)

(defcfun (widget-ancestor "gtk_widget_get_ancestor") (g-object widget)
  (widget (g-object widget))
  (type g-type-designator))

(export 'widget-ancestor)

(defcfun gtk-widget-get-pointer :void
  (widget g-object)
  (x (:pointer :int))
  (y (:pointer :int)))

(defun widget-pointer (widget)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-widget-get-pointer widget x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'widget-pointer)

(defcfun (widget-is-ancestor "gtk_widget_is_ancestor") :boolean
  (widget g-object)
  (container g-object))

(export 'widget-is-ancestor)

(defcfun gtk-widget-translate-coordinates :boolean
  (src-widget g-object)
  (dst-widget g-object)
  (src-x :int)
  (src-y :int)
  (dst-x (:pointer :int))
  (dst-y (:pointer :int)))

(defun widget-translate-coordinates (src-widget dst-widget src-x src-y)
  (with-foreign-objects ((dst-x :int) (dst-y :int))
    (gtk-widget-translate-coordinates src-widget dst-widget src-x src-y dst-x dst-y)
    (values (mem-ref dst-x :int)
            (mem-ref dst-y :int))))

(export 'widget-translate-coordinates)

(defcfun (widget-ensure-style "gtk_widget_ensure_style") :void
  (widget g-object))

(export 'widget-ensure-style)

(defcfun (widget-reset-rc-styles "gtk_widget_reset_rc_styles") :void
  (widget g-object))

(export 'widget-reset-rc-styles)

(defcfun (widget-push-colormap "gtk_widget_push_colormap") :void
  (colormap (g-object gdk-colormap)))

(export 'widget-push-colormap)

(defcfun (widget-pop-colormap "gtk_widget_pop_colormap") :void)

(export 'widget-pop-colormap)

(defcfun (widget-default-colormap "gtk_widget_get_default_colormap") (g-object gdk-colormap))

(defcfun gtk-widget-set-default-colormap :void
  (colormap (g-object gdk-colormap)))

(defun (setf widget-default-colormap) (colormap)
  (gtk-widget-set-default-colormap colormap))

(export 'widget-default-colormap)

(defcfun (widget-default-style "gtk_widget_get_default_style") (g-object style))

(export 'widget-default-style)

(defcfun (widget-default-visual "gtk_widget_get_default_visual") (g-object visual))

(export 'widget-default-visual)

(defcfun (widget-default-direction "gtk_widget_get_default_direction") text-direction)

(defcfun gtk-widget-set-default-direction :void
  (direction text-direction))

(defun (setf widget-default-direction) (new-value)
  (gtk-widget-set-default-direction new-value))

(export 'widget-default-direction)

(defcfun (widget-shape-combine-mask "gtk_widget_shape_combine_mask") :void
  (widget (g-object widget))
  (shape-mask g-object)
  (offset-x :int)
  (offset-y :int))

(export 'widget-shape-combine-mask)

(defcfun (widget-input-shape-combine-mask "gtk_widget_input_shape_combine_mask") :void
  (widget (g-object widget))
  (shape-mask g-object)
  (offset-x :int)
  (offset-y :int))

(export 'widget-input-shape-combine-mask)

(defcfun gtk-widget-path :void
  (widget g-object)
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

(defcfun gtk-widget-class-path :void
  (widget g-object)
  (path-length (:pointer :uint))
  (path (:pointer (:pointer :char)))
  (path-reversed (:pointer (:pointer :char))))

(defun widget-path (widget &key (path-type :name))
  (assert (typep path-type '(member :name :class)))
  (with-foreign-object (path :pointer)
    (ecase path-type
      (:name (gtk-widget-path widget (null-pointer) path (null-pointer)))
      (:class (gtk-widget-class-path widget (null-pointer) path (null-pointer))))
    (mem-ref path '(g-string :free-from-foreign t))))

(export 'widget-path)

(defcfun (widget-modify-fg "gtk_widget_modify_fg") :void
  (widget (g-object widget))
  (state state-type)
  (color (g-boxed-foreign color)))

(export 'widget-modify-fg)

(defcfun (widget-modify-bg "gtk_widget_modify_bg") :void
  (widget (g-object widget))
  (state state-type)
  (color (g-boxed-foreign color)))

(export 'widget-modify-bg)

(defcfun (widget-modify-text "gtk_widget_modify_text") :void
  (widget (g-object widget))
  (state state-type)
  (color (g-boxed-foreign color)))

(export 'widget-modify-text)

(defcfun (widget-modify-base "gtk_widget_modify_base") :void
  (widget (g-object widget))
  (state state-type)
  (color (g-boxed-foreign color)))

(export 'widget-modify-base)

;void                gtk_widget_modify_font              (GtkWidget *widget,
;                                                         PangoFontDescription *font_desc);

(defcfun (widget-modify-cursor "gtk_widget_modify_cursor") :void
  (widget (g-object widget))
  (primary (g-boxed-foreign color))
  (secondary (g-boxed-foreign color)))

(export 'widget-modify-cursor)

(defcfun (widget-create-pango-context "gtk_widget_create_pango_context") (g-object :already-referenced)
  (widget g-object))

(export 'widget-create-pango-context)

(defcfun (widget-create-pango-layout "gtk_widget_create_pango_layout") (g-object pango-layout :already-referenced)
  (widget (g-object widget))
  (text :string))

(export 'widget-create-pango-layout)

(defcfun (widget-render-icon "gtk_widget_render_icon") g-object
  (widget g-object)
  (stock-id :string)
  (size icon-size)
  (detail :string))

(export 'widget-render-icon)

(defcfun (widget-push-composite-child "gtk_widget_push_composite_child") :void
  (widget g-object))

(export 'widget-push-composite-child)

(defcfun (widget-pop-composite-child "gtk_widget_pop_composite_child") :void
  (widget g-object))

(export 'widget-pop-composite-child)

(defcfun (widget-queue-clear "gtk_widget_queue_clear") :void
  (widget (g-object widget)))

(export 'widget-queue-clear)

(defcfun (widget-queue-clear-area "gtk_widget_queue_clear_area") :void
  (widget (g-object widget))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'widget-queue-clear-area)

(defcfun (widget-queue-draw-area "gtk_widget_queue_draw_area") :void
  (widget g-object)
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(export 'widget-queue-draw-area)

(defcfun (widget-reset-shapes "gtk_widget_reset_shapes") :void
  (widget g-object))

(export 'widget-reset-shapes)

(defcfun (widget-set-double-buffered "gtk_widget_set_double_buffered") :void
  (widget (g-object widget))
  (double-buffered :boolean))

(export 'widget-set-double-buffered)

(defcfun (widget-set-scroll-adjustments "gtk_widget_set_scroll_adjustments") :boolean
  (widget g-object)
  (hadjustment g-object)
  (vadjustment g-object))

(export 'widget-set-scroll-adjustments)

(defcfun (widget-mnemonic-activate "gtk_widget_mnemonic_activate") :boolean
  (widget (g-object widget))
  (group-cycling :boolean))

(export 'widget-mnemonic-activate)

; TODO: gtk_widget_class_install_style_property

; TOOD: gtk_widget_class_install_style_property_parser

; TODO: gtk_widget_class_list_style_properties

(defcfun (widget-region-intersect "gtk_widget_region_intersect") (g-boxed-foreign region :return)
  (widget (g-object widget))
  (region (g-boxed-foreign region)))

(export 'widget-region-intersect)

; ignored: gtk_widget_send_expose

(defcfun gtk-widget-style-get-property :void
  (widget g-object)
  (property-name :string)
  (value (:pointer g-value)))

(defcfun gtk-widget-class-find-style-property (:pointer g-param-spec)
  (class :pointer)
  (property-name :string))

(defcfun gtk-widget-class-list-style-properties (:pointer (:pointer g-param-spec))
  (class :pointer)
  (n-properties (:pointer :int)))

(defun widget-get-style-properties (type)
  (setf type (ensure-g-type type))
  (let ((class (g-type-class-ref type)))
    (unwind-protect
         (with-foreign-object (np :int)
           (let ((specs (gtk-widget-class-list-style-properties class np)))
             (unwind-protect
                  (loop
                     repeat (mem-ref np :int)
                     for i from 0
                     for spec = (mem-aref specs :pointer i)
                     collect (parse-g-param-spec spec))
               (g-free specs))))
      (g-type-class-unref class))))

(export 'widget-get-style-properties)

(defun widget-style-property-info (type property-name)
  (let ((class (g-type-class-ref type)))
    (unwind-protect
         (let ((g-param-spec (gtk-widget-class-find-style-property class property-name)))
           (parse-g-param-spec g-param-spec))
      (g-type-class-unref class))))

(export 'widget-style-property-info)

(defun widget-style-property-type (widget property-name)
  (let ((property-info (widget-style-property-info (g-type-from-object widget) property-name)))
    (g-class-property-definition-type property-info)))

(defun widget-style-property-value (widget property-name &optional property-type)
  (unless property-type (setf property-type (widget-style-property-type widget property-name)))
  (setf property-type (ensure-g-type property-type))
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue property-type)
    (prog1 (gtk-widget-style-get-property widget property-name gvalue)
      (g-value-unset gvalue))))

(export 'widget-style-property-value)

(defcfun (widget-child-focus "gtk_widget_child_focus") :boolean
  (widget g-object)
  (direction direction-type))

(export 'widget-child-focus)

(defcfun (widget-child-notify "gtk_widget_child_notify") :void
  (widget (g-object widget))
  (property-name :string))

(export 'widget-child-notify)

(defcfun (widget-freeze-child-notify "gtk_widget_freeze_child_notify") :void
  (widget g-object))

(export 'widget-freeze-child-notify)

(defcfun (widget-settings "gtk_widget_get_settings") g-object
  (widget g-object))

(export 'widget-settings)

(defcfun (widget-clipboard "gtk_widget_get_clipboard") (g-object clipboard)
  (widget (g-object widget))
  (selection gdk-atom-as-string))

(export 'widget-clipboard)

(defcfun (widget-display "gtk_widget_get_display") g-object
  (widget g-object))

(export 'widget-display)

(defcfun (widget-root-window "gtk_widget_get_root_window") g-object
  (widget g-object))

(export 'widget-root-window)

(defcfun (widget-screen "gtk_widget_get_screen") g-object
  (widget g-object))

(export 'widget-screen)

(defcfun (widget-has-screen "gtk_widget_has_screen") :boolean
  (widget g-object))

(export 'widget-has-screen)

(defcfun (widget-thaw-child-notify "gtk_widget_thaw_child_notify") :void
  (widget g-object))

(export 'widget-thaw-child-notify)

(defcfun (widget-mnemonic-labels "gtk_widget_list_mnemonic_labels") (glist (g-object widget) :free-from-foreign t)
  (widget (g-object widget)))

(export 'widget-mnemonic-labels)

(defcfun (widget-add-mnemonic-label "gtk_widget_add_mnemonic_label") :void
  (widget g-object)
  (label g-object))

(export 'widget-add-mnemonic-label)

(defcfun (widget-remove-mnemonic-label "gtk_widget_remove_mnemonic_label") :void
  (widget g-object)
  (label g-object))

(export 'widget-remove-mnemonic-label)

(defcfun (widget-action "gtk_widget_get_action") g-object
  (widget g-object))

(export 'widget-action)

(defcfun (widget-composited-p "gtk_widget_is_composited") :boolean
  (widget g-object))

(export 'widget-composited-p)

(defcfun (widget-error-bell "gtk_widget_error_bell") :void
  (widget g-object))

(export 'widget-error-bell)

(defcfun (widget-trigger-tooltip-query "gtk_tooltip_trigger_tooltip_query") :void
  (widget g-object))

(export 'widget-trigger-tooltip-query)

(defcfun gtk-widget-get-snapshot g-object
  (widget g-object)
  (clip-rectangle (g-boxed-foreign rectangle)))

(defun widget-snapshot (widget &optional clip-rectangle)
  (gtk-widget-get-snapshot widget clip-rectangle))

(export 'widget-snapshot)