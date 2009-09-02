(in-package :gtk)

; TODO: GtkWidget

(defun widget-flags (widget)
  (convert-from-foreign (gtk-object-flags-as-integer widget) 'widget-flags))

(defun (setf widget-flags) (new-value widget)
  (setf (gtk-object-flags-as-integer widget)
        (convert-to-foreign new-value 'widget-flags))
  new-value)

(export 'widget-flags)

(defcstruct %gtk-requisition
  (:width :int)
  (:height :int))

(defcstruct %gtk-allocation
  (:x :int)
  (:y :int)
  (:width :int)
  (:height :int))

(defcstruct %gtk-widget
  (:object %gtk-object)
  (:private-flags :uint16)
  (:state state-type)
  (:saved-state state-type)
  (:name (:pointer :char))
  (:style :pointer)
  (:requisition %gtk-requisition)
  (:allocation %gtk-allocation)
  (:window :pointer)
  (:parent :pointer))

(defun widget-state (widget)
  (foreign-slot-value (pointer widget) '%gtk-widget :state))

(export 'widget-state)
(defun widget-saved-state (widget)
  (foreign-slot-value (pointer widget) '%gtk-widget :saved-state))

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

; TODO: gtk_widget_event

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
  (intersection (g-boxed-foreign rectangle :in-out)))

(defun widget-intersect (widget rectangle)
  (let ((result (make-rectangle :x 0 :y 0 :width 0 :height 0)))
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

; TODO: gtk_widget_set_state

; TODO: gtk_widget_set_parent_window

; TODO: gtk_widget_get_parent_window

; TODO: gtk_widget_set_extension_events

; TODO: gtk_widget_get_extension_events


; fix ownership issues:
; TODO: gtk_widget_get_toplevel

; TODO: gtk_widget_get_ancestor

; TODO: gtk_widget_get_colormap

; TODO: gtk_widget_get_visual

(defcfun gtk-widget-get-pointer :void
  (widget g-object)
  (x (:pointer :int))
  (y (:pointer :int)))

(defun widget-pointer (widget)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-widget-get-pointer widget x y)
    (values (mem-ref x :int) (mem-ref y :int))))

(export 'widget-pointer)

(defcfun (widget-contained-p "gtk_widget_is_ancestor") :boolean
  (widget g-object)
  (container g-object))

(export 'widget-contained-p)

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

; TODO: gtk_widget_push_colormap

; TODO: gtk_widget_pop_colormap

; TODO: gtk_widget_set_default_colormap

; TODO: gtk_widget_get_default_colormap

; TODO: gtk_widget_get_default_style (ownership)

; TODO: gtk_widget_get_default_visual

(defcfun (widget-default-direction "gtk_widget_get_default_direction") text-direction)

(defcfun gtk-widget-set-default-direction :void
  (direction text-direction))

(defun (setf widget-default-direction) (new-value)
  (gtk-widget-set-default-direction new-value))

(export 'widget-default-direction)

; TODO: gtk_widget_shape_combine_mask 

; TODO: gtk_widget_input_shape_combine_mask

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

; TODO: gtk_widget_modify_style

; TODO: gtk_widget_get_modifier_style

; TODO: gtk_widget_modify_fg

; TODO: gtk_widget_modify_bg

; TODO: gtk_widget_modify_text

; TODO: gtk_widget_modify_base

; TODO: gtk_widget_modify_font

; TODO: gtk_widget_modify_cursor 

(defcfun (widget-create-pango-context "gtk_widget_create_pango_context") g-object
  (widget g-object))

(export 'widget-create-pango-context)

(defcfun (widget-get-pango-context "gtk_widget_get_pango_context") g-object
  (widget g-object))

(export 'widget-get-pango-context)

(defcfun (widget-create-pango-layout "gtk_widget_create_pango_layout") (g-object gdk::pango-layout)
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

(defcfun (widget-set-scroll-adjustments "gtk_widget_set_scroll_adjustments") :boolean
  (widget g-object)
  (hadjustment g-object)
  (vadjustment g-object))

(export 'widget-set-scroll-adjustments)

; TODO: gtk_widget_class_install_style_property

; TOOD: gtk_widget_class_install_style_property_parser

; TODO: gtk_widget_class_list_style_properties

; TODO: gtk_widget_region_intersect

; TODO: gtk_widget_send_expose

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

(defun widget-class-get-style-properties (type)
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

(export 'widget-class-get-style-properties)

(defun widget-child-property-type (widget property-name)
  (let* ((type (g-type-from-object widget))
         (class (g-type-class-ref type)))
    (unwind-protect
         (let ((g-param-spec (gtk-widget-class-find-style-property class property-name)))
           (when (null-pointer-p g-param-spec) (error "Widget ~A has no style-property named '~A'" widget property-name))
           (foreign-slot-value g-param-spec 'gobject:g-param-spec :value-type))
      (g-type-class-unref class))))

(defun widget-child-property-value (widget property-name &optional property-type)
  (unless property-type (setf property-type (widget-child-property-type widget property-name)))
  (setf property-type (ensure-g-type property-type))
  (with-foreign-object (gvalue 'g-value)
    (g-value-zero gvalue)
    (g-value-init gvalue property-type)
    (prog1 (gtk-widget-style-get-property widget property-name gvalue)
      (g-value-unset gvalue))))

(export 'widget-child-property-value)

(defcfun (widget-child-focus "gtk_widget_child_focus") :boolean
  (widget g-object)
  (direction direction-type))

(export 'widget-child-focus)

(defcfun (widget-freeze-child-notify "gtk_widget_freeze_child_notify") :void
  (widget g-object))

(export 'widget-freeze-child-notify)

(defcfun (widget-settings "gtk_widget_get_settings") g-object
  (widget g-object))

(export 'widget-settings)

; TODO: gtk_widget_get_clipboard

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

; TODO: gtk_widget_set_child_visible

(defcfun (widget-thaw-child-notify "gtk_widget_thaw_child_notify") :void
  (widget g-object))

(export 'widget-thaw-child-notify)

; TODO: gtk_widget_list_mnemonic_labels

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