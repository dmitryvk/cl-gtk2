(in-package :gdk)

(define-g-enum "GdkGrabStatus" grab-status () :success :already-grabbed :invalid-time :not-viewable :frozen)

(defcenum crossing-mode :normal :grab :ungrab :gtk-grab :gtk-ungrab :state-changed)
(export 'crossing-mode)

(defcenum notify-type (:ancestor 0) :virtual :inferior :nonlinear :nonlinear-virtual :unknown)
(export 'notify-type)

(gobject:define-g-enum "GdkFillRule"
    gdk-fill-rule
    (:export t :type-initializer "gdk_fill_rule_get_type")
  (:even-odd-rule 0)
  (:winding-rule 1))

(define-g-enum "GdkOverlapType"
    overlap-type
    (:export t :type-initializer "gdk_overlap_type_get_type")
  (:in 0)
  (:out 1)
  (:part 2))

(define-g-flags "GdkGCValuesMask"
    gc-values-mask
    (:export t :type-initializer "gdk_gc_values_mask_get_type")
  (:foreground 1)
  (:background 2)
  (:font 4)
  (:function 8)
  (:fill 16)
  (:tile 32)
  (:stipple 64)
  (:clip-mask 128)
  (:subwindow 256)
  (:ts-x-origin 512)
  (:ts-y-origin 1024)
  (:clip-x-origin 2048)
  (:clip-y-origin 4096)
  (:exposures 8192)
  (:line-width 16384)
  (:line-style 32768)
  (:cap-style 65536)
  (:join-style 131072))

(define-g-enum "GdkFunction"
    gdk-function
    (:export t :type-initializer "gdk_function_get_type")
  (:copy 0)
  (:invert 1)
  (:xor 2)
  (:clear 3)
  (:and 4)
  (:and-reverse 5)
  (:and-invert 6)
  (:noop 7)
  (:or 8)
  (:equiv 9)
  (:or-reverse 10)
  (:copy-invert 11)
  (:or-invert 12)
  (:nand 13)
  (:nor 14)
  (:set 15))

(define-g-enum "GdkFill"
    gdk-fill
    (:export t :type-initializer "gdk_fill_get_type")
  (:solid 0)
  (:tiled 1)
  (:stippled 2)
  (:opaque-stippled 3))

(define-g-enum "GdkSubwindowMode"
    subwindow-mode
    (:export t :type-initializer "gdk_subwindow_mode_get_type")
  (:clip-by-children 0)
  (:include-inferiors 1))

(define-g-enum "GdkLineStyle"
    line-style
    (:export t :type-initializer "gdk_line_style_get_type")
  (:solid 0)
  (:on-off-dash 1)
  (:double-dash 2))

(define-g-enum "GdkCapStyle"
    cap-style
    (:export t :type-initializer "gdk_cap_style_get_type")
  (:not-last 0)
  (:butt 1)
  (:round 2)
  (:projecting 3))

(define-g-enum "GdkJoinStyle"
    join-style
    (:export t :type-initializer "gdk_join_style_get_type")
  (:miter 0)
  (:round 1)
  (:bevel 2))

(define-g-enum "GdkRgbDither"
    rgb-dither
    (:export t :type-initializer "gdk_rgb_dither_get_type")
  (:none 0)
  (:normal 1)
  (:max 2))

(define-g-object-class "GdkDisplay" display ()
  ((:cffi name display-name (glib:g-string :free-from-foreign nil)
          "gdk_display_get_name" nil)
   (:cffi n-screens display-n-screens :int
          "gdk_display_get_n_screens" nil)
   (:cffi default-screen display-default-screen (g-object screen)
          "gdk_display_get_default_screen" nil)
   (:cffi devices display-devices (glib:glist g-object :free-from-foreign nil)
          "gdk_display_list_devices" nil)
   (:cffi supports-cursor-color display-supports-cursor-color :boolean
          "gdk_display_supports_cursor_color" nil)
   (:cffi supports-cursor-alpha display-supports-color-alpha :boolean
          "gdk_display_supports_cursor_alpha" nil)
   (:cffi default-cursor-size display-default-cursor-size :uint
          "gdk_display_get_default_cursor_size" nil)
   (:cffi default-group display-default-group (g-object gdk-window)
          "gdk_display_get_default_group" nil)
   (:cffi supports-selection-notification display-supports-selection-notification :boolean
          "gdk_display_supports_selection_notification" nil)
   (:cffi supports-clipboard-persistence display-supports-clipboard-persistence :boolean
          "gdk_display_supports_clipboard_persistence" nil)
   (:cffi supports-shapes display-supports-shapes :boolean
          "gdk_display_supports_shapes" nil)
   (:cffi supports-input-shapes display-supports-input-shapes :boolean
          "gdk_display_supports_input_shapes" nil)
   (:cffi supports-composite display-supports-composite :boolean
          "gdk_display_supports_composite" nil)
   (:cffi core-pointer display-core-pointer g-object
          "gdk_display_get_core_pointer" nil)))

;gdk_display_get_screen

(define-g-object-class "GdkDisplayManager" display-manager ()
  ((default-display display-manager-default-display "default-display" "GdkDisplay" t t)
   (:cffi displays display-manager-displays (glib:gslist (g-object display) :free-from-foreign t)
          "gdk_display_manager_list_displays" nil)))

(define-g-object-class "GdkVisual" visual () ())

(define-g-object-class "GdkColormap" gdk-colormap
  (:superclass g-object :export t :interfaces
               nil :type-initializer
               "gdk_colormap_get_type")
  nil)

(define-g-object-class "GdkScreen" screen ()
  ((font-options screen-font-options "font-options" "gpointer" t t)
   (resolution screen-resolution "resolution" "gdouble" t t)
   (:cffi default-colormap screen-default-colormap (g-object colormap)
          "gdk_screen_get_default_colormap" "gdk_screen_set_default_colormap")
   (:cffi system-colormap screen-system-colormap (g-object colormap)
          "gdk_screen_get_system_colormap" nil)
   (:cffi system-visual screen-system-visual (g-object visual)
          "gdk_screen_get_system_visual" nil)
   (:cffi rgb-colormap screen-rgb-colormap (g-object colormap)
          "gdk_screen_get_rgb_colormap" nil)
   (:cffi rgb-visual screen-visual (g-object visual)
          "gdk_screen_get_rgb_visual" nil)
   (:cffi rgba-colormap screen-rgba-colormap (g-object colormap)
          "gdk_screen_get_rgba_colormap" nil)
   (:cffi rgba-visual screen-rgba-visual (g-object visual)
          "gdk_screen_get_rgba_visual" nil)
   (:cffi composited-p screen-composited-p :boolean
          "gdk_screen_is_composited" nil)
   (:cffi root-window screen-root-window (g-object gdk-window)
          "gdk_screen_get_root_window" nil)
   (:cffi display screen-display (g-object display)
          "gdk_screen_get_display" nil)
   (:cffi number screen-number :int
          "gdk_screen_get_number" nil)
   (:cffi width screen-widget :int
          "gdk_screen_get_width" nil)
   (:cffi height screen-height :int
          "gdk_screen_get_height" nil)
   (:cffi width-mm screen-width-mm :int
          "gdk_screen_get_width_mm" nil)
   (:cffi height-mm screen-height-mm :int
          "gdk_screen_get_height_mm" nil)
   (:cffi visuals screen-visuals (glib:glist (g-object visual) :free-from-foreign t)
          "gdk_screen_list_visuals" nil)
   (:cffi toplevel-windows screen-toplevel-windows (glib:glist (g-object gdk-window) :free-from-foreign t)
          "gdk_screen_get_toplevel_windows" nil)
   (:cffi display-name screen-display-name (glib:g-string :free-from-foreign t)
          "gdk_screen_make_display_name" nil)
   (:cffi n-monitors screen-n-monitors :int
          "gdk_screen_get_n_monitors" nil)
   (:cffi active-window screen-active-window (g-object gdk-window)
          "gdk_screen_get_active_window" nil)
   (:cffi window-stack screen-window-stack (glib:glist (g-object gdk-window) :free-from-foreign t)
          "gdk_screen_get_window_stack" nil)))

(defcfun gdk-region-new :pointer)

(define-g-boxed-opaque region nil :alloc (gdk-region-new))

(export (boxed-related-symbols 'region))

(define-g-boxed-cstruct point nil
  (x :int :initform 0)
  (y :int :initform 0))

(export (boxed-related-symbols 'point))

(define-g-boxed-cstruct span nil
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0))

(export (boxed-related-symbols 'span))

(define-g-boxed-cstruct segment nil
  (x1 :int :initform 0)
  (y1 :int :initform 0)
  (x2 :int :initform 0)
  (y2 :int :initform 0))

(export (boxed-related-symbols 'segment))

(define-g-boxed-cstruct trapezoid nil
  (y1 :double :initform 0d0)
  (x11 :double :initform 0d0)
  (x21 :double :initform 0d0)
  (y2 :double :initform 0d0)
  (x12 :double :initform 0d0)
  (x22 :double :initform 0d0))

(export (boxed-related-symbols 'trapezoid))

(define-g-boxed-cstruct rectangle "GdkRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(export (boxed-related-symbols 'rectangle))

(define-g-boxed-opaque font "GdkFont"
  :alloc (error "GDK:FONT objects may not be allocated directly"))

(export (boxed-related-symbols 'font))

(define-g-boxed-cstruct color "GdkColor"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

(export (boxed-related-symbols 'color))

(define-g-object-class "GdkGC" graphics-context ()
  ((:cffi screen graphics-context-screen (g-object screen)
          "gdk_gc_get_screen" nil)
   (:cffi foreground graphics-context-foreground (g-boxed-foreign color)
          nil "gdk_gc_set_foreground")
   (:cffi background graphics-context-background (g-boxed-foreign color)
          nil "gdk_gc_set_background")
   (:cffi rgb-fg-color graphics-context-rgb-fg-color (g-boxed-foreign color)
          nil "gdk_gc_set_rgb_fg_color")
   (:cffi rgb-bg-color graphics-context-rgb-bg-color (g-boxed-foreign color)
          nil "gdk_gc_set_rgb_bg_color")
   (:cffi font graphics-context-font (g-boxed-foreign font)
          nil "gdk_gc_set_font")
   (:cffi function graphics-context-function gdk-function
          nil "gdk_gc_set_function")
   (:cffi fill graphics-context-fill gdk-fill
          nil "gdk_gc_set_fill")
   (:cffi tile graphics-context-tile (g-object pixmap)
          nil "gdk_gc_set_tile")
   (:cffi stipple graphics-context-stipple (g-object pixmap)
          nil "gdk_gc_set_stipple")
   (:cffi clip-mask graphics-context-clip-mask (g-object pixmap)
          nil "gdk_gc_set_clip_mask")
   (:cffi clip-rectangle graphics-context-clip-rectangle (g-boxed-foreign rectangle)
          nil "gdk_gc_set_clip_rectangle")
   (:cffi clip-region graphics-context-clip-region (g-boxed-foreign region)
          nil "gdk_gc_set_clip_region")
   (:cffi subwindow graphics-context-subwindow subwindow-mode
          nil "gdk_gc_set_subwindow")
   (:cffi exposures graphics-context-exposures :boolean
          nil "gdk_gc_set_exposures")
   (:cffi colormap graphics-context-colormap (g-object colormap)
          "gdk_gc_get_colormap" "gdk_gc_set_colormap")))

(define-g-object-class "GdkDrawable" drawable ()
  ((:cffi display drawable-display (g-object display)
          "gdk_drawable_get_display" nil)
   (:cffi screen drawable-screen (g-object screen)
          "gdk_drawable_get_screen" nil)
   (:cffi visual drawable-visual (g-object visual)
          "gdk_drawable_get_visual" nil)
   (:cffi colormap drawable-colormap (g-object colormap)
          "gdk_drawable_get_colormap" "gdk_drawable_set_colormap")
   (:cffi depth drawable-depth :int
          "gdk_drawable_get_depth" nil)
   (:cffi clip-region drawable-clip-region (g-boxed-foreign region :return)
          "gdk_drawable_get_clip_region" nil)
   (:cffi visible-region drawable-visible-region (g-boxed-foreign region :return)
          "gdk_drawable_get_visible_region" nil)))

(define-g-object-class "GdkPixmap" pixmap (:superclass drawable) ())

(define-g-object-class "GdkWindow" gdk-window (:superclass drawable) ())

(define-g-object-class "GdkKeymap" keymap () ())

(define-g-enum "GdkEventType" event-type ()
  (:nothing -1) (:delete 0)
  (:destroy 1) (:expose 2) (:motion-notify 3)
  (:button-press 4) (:2button-press 5) (:3button-press 6)
  (:button-release 7) (:key-press 8) (:key-release 9)
  (:enter-notify 10) (:leave-notify 11) (:focus-change 12)
  (:configure 13) (:map 14) (:unmap 15) (:property-notify 16)
  (:selection-clear 17) (:selection-request 18)
  (:selection-notify 19) (:proximity-in 20)
  (:proximity-out 21) (:drag-enter 22) (:drag-leave 23)
  (:drag-motion 24) (:drag-status 25) (:drop-start 26)
  (:drop-finished 27) (:client-event 28)
  (:visibility-notify 29) (:no-expose 30) (:scroll 31)
  (:window-state 32) (:setting 33) (:owner-change 34)
  (:grab-broken 35) (:damage 36)) 

(define-g-flags "GdkEventMask" gdk-event-mask (:export t :type-initializer "gdk_event_mask_get_type")
  (:exposure-mask 2) (:pointer-motion-mask 4)
  (:pointer-motion-hint-mask 8) (:button-motion-mask 16)
  (:button1-motion-mask 32) (:button2-motion-mask 64)
  (:button3-motion-mask 128) (:button-press-mask 256)
  (:button-release-mask 512) (:key-press-mask 1024)
  (:key-release-mask 2048) (:enter-notify-mask 4096)
  (:leave-notify-mask 8192) (:focus-change-mask 16384)
  (:structure-mask 32768) (:property-change-mask 65536)
  (:visibility-notify-mask 131072)
  (:proximity-in-mask 262144) (:proximity-out-mask 524288)
  (:substructure-mask 1048576) (:scroll-mask 2097152)
  (:all-events-mask 4194302))

(define-g-enum "GdkExtensionMode" gdk-extension-mode (:export t :type-initializer "gdk_extension_mode_get_type")
  (:none 0) (:all 1) (:cursor 2))

(define-g-enum "GdkWindowTypeHint" gdk-window-type-hint (:export t :type-initializer "gdk_window_type_hint_get_type")
  (:normal 0) (:dialog 1) (:menu 2) (:toolbar 3)
  (:splashscreen 4) (:utility 5) (:dock 6) (:desktop 7)
  (:dropdown-menu 8) (:popup-menu 9) (:tooltip 10)
  (:notification 11) (:combo 12) (:dnd 13))

(define-g-flags "GdkModifierType" modifier-type ()
  (:shift-mask 1) (:lock-mask 2) (:control-mask 4)
  (:mod1-mask 8) (:mod2-mask 16) (:mod3-mask 32)
  (:mod4-mask 64) (:mod5-mask 128)
  (:button1-mask 256) (:button2-mask 512)
  (:button3-mask 1024) (:button4-mask 2048)
  (:button5-mask 4096) (:super-mask 67108864)
  (:hyper-mask 134217728) (:meta-mask 268435456)
  (:release-mask 1073741824)
  (:modifier-mask 1543512063))

(define-g-enum "GdkScrollDirection" scroll-direction ()
  (:up 0) (:down 1)
  (:left 2) (:right 3))

(define-g-enum "GdkVisibilityState" visibility-state ()
  (:unobscured 0)
  (:partial 1) (:fully-obscured 2))

(define-g-enum "GdkPropertyState" property-state ()
  :new-value :delete)

(define-g-flags "GdkWindowState" window-state ()
  (:withdrawn 1)
  (:iconified 2) (:maximized 4) (:sticky 8) (:fullscreen 16)
  (:above 32) (:below 64))

(define-g-enum "GdkSettingAction" setting-action ()
  (:new 0) (:changed 1)
  (:deleted 2))

(define-g-enum "GdkOwnerChange" owner-change ()
  (:new-owner 0)
  (:destroy 1) (:close 2))

(define-g-flags "GdkEventMask" event-mask ()
  (:exposure-mask 2)
  (:pointer-motion-mask 4) (:pointer-motion-hint-mask 8)
  (:button-motion-mask 16) (:button1-motion-mask 32)
  (:button2-motion-mask 64) (:button3-motion-mask 128)
  (:button-press-mask 256) (:button-release-mask 512)
  (:key-press-mask 1024) (:key-release-mask 2048)
  (:enter-notify-mask 4096) (:leave-notify-mask 8192)
  (:focus-change-mask 16384) (:structure-mask 32768)
  (:property-change-mask 65536)
  (:visibility-notify-mask 131072)
  (:proximity-in-mask 262144) (:proximity-out-mask 524288)
  (:substructure-mask 1048576) (:scroll-mask 2097152)
  (:all-events-mask 4194302))

(define-g-enum "GdkFontType" font-type () :font :fontset)

(define-g-enum "GdkGravity" gravity ()
  (:north-west 1)
  :north
  :north-east
  :west
  :center
  :east
  :south-west
  :south
  :south-east
  :static)

(define-g-boxed-cstruct geometry "GdkGeometry"
  (min-width :int :initform 0)
  (min-height :int :initform 0)
  (max-width :int :initform 0)
  (max-height :int :initform 0)
  (base-width :int :initform 0)
  (base-height :int :initform 0)
  (width-increment :int :initform 0)
  (height-increment :int :initform 0)
  (min-aspect :double :initform 0.0d0)
  (max-aspect :double :initform 0.0d0)
  (gravity gravity :initform :north-west))

(export (boxed-related-symbols 'geometry))

(glib:at-init () (foreign-funcall-pointer (foreign-symbol-pointer "gdk_cursor_get_type") () :int))

(gobject:define-g-enum "GdkCursorType" cursor-type (:export t :type-initializer "gdk_cursor_type_get_type")
  (:x-cursor 0) (:arrow 2) (:based-arrow-down 4)
  (:based-arrow-up 6) (:boat 8) (:bogosity 10)
  (:bottom-left-corner 12) (:bottom-right-corner 14)
  (:bottom-side 16) (:bottom-tee 18) (:box-spiral 20)
  (:center-ptr 22) (:circle 24) (:clock 26)
  (:coffee-mug 28) (:cross 30) (:cross-reverse 32)
  (:crosshair 34) (:diamond-cross 36) (:dot 38)
  (:dotbox 40) (:double-arrow 42) (:draft-large 44)
  (:draft-small 46) (:draped-box 48) (:exchange 50)
  (:fleur 52) (:gobbler 54) (:gumby 56) (:hand1 58)
  (:hand2 60) (:heart 62) (:icon 64) (:iron-cross 66)
  (:left-ptr 68) (:left-side 70) (:left-tee 72)
  (:leftbutton 74) (:ll-angle 76) (:lr-angle 78)
  (:man 80) (:middlebutton 82) (:mouse 84)
  (:pencil 86) (:pirate 88) (:plus 90)
  (:question-arrow 92) (:right-ptr 94)
  (:right-side 96) (:right-tee 98) (:rightbutton 100)
  (:rtl-logo 102) (:sailboat 104)
  (:sb-down-arrow 106) (:sb-h-double-arrow 108)
  (:sb-left-arrow 110) (:sb-right-arrow 112)
  (:sb-up-arrow 114) (:sb-v-double-arrow 116)
  (:shuttle 118) (:sizing 120) (:spider 122)
  (:spraycan 124) (:star 126) (:target 128)
  (:tcross 130) (:top-left-arrow 132)
  (:top-left-corner 134) (:top-right-corner 136)
  (:top-side 138) (:top-tee 140) (:trek 142)
  (:ul-angle 144) (:umbrella 146) (:ur-angle 148)
  (:watch 150) (:xterm 152) (:last-cursor 153)
  (:blank-cursor -2) (:cursor-is-pixmap -1))

(export 'cursor-type)

(define-g-boxed-cstruct cursor "GdkCursor"
  (type cursor-type))

(export (boxed-related-symbols 'cursor))

(defctype gdk-atom :pointer)

(define-foreign-type gdk-atom-as-string-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser gdk-atom-as-string))

(defmethod translate-from-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-name value))

(defmethod translate-to-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-intern value nil))

;;;FIXME: Check correct type
(defctype native-window :uint32)

(define-foreign-type fixed-array ()
  ((element-type :reader fixed-array-element-type :initarg :element-type :initform (error "Element type must be specified"))
   (array-size :reader fixed-array-array-size :initarg :array-size :initform (error "Array size must be specified")))
  (:actual-type :pointer)
  (:documentation
   "CFFI foreign type for an array of a fixed length. Slot @code{element-type}@see-slot{fixed-array-element-type} specifies the type of elements and slot @code{array-size}@see-slot{fixed-array-array-size} specifies the size of array (in elements)."))

(define-parse-method fixed-array (element-type array-size)
  (make-instance 'fixed-array :element-type element-type :array-size array-size))

(defmethod translate-from-foreign (ptr (type fixed-array))
  (when (not (null-pointer-p ptr))
    (let ((result (make-array (fixed-array-array-size type)))
          (el-type (fixed-array-element-type type)))
      (loop
         for i from 0 below (fixed-array-array-size type)
         do (setf (aref result i) (mem-aref ptr el-type i)))
      result)))

(define-g-boxed-variant-cstruct event "GdkEvent"
  (type event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8))
  (:variant type
            ((:key-press :key-release) event-key
             (time :uint32)
             (state modifier-type)
             (keyval :uint)
             (length :int)
             (string (:string :free-from-foreign nil
                              :free-to-foreign nil))
             (hardware-keycode :uint16)
             (group :uint8)
             (is-modifier :uint))
            ((:button-press
              :2button-press
              :3button-press
              :button-release) event-button
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state :uint)
             (button :uint)
             (device (g-object device))
             (x-root :double)
             (y-root :double))
            ((:scroll) event-scroll
             (time :uint32)
             (x :double)
             (y :double)
             (state modifier-type)
             (direction scroll-direction)
             (device (g-object device))
             (x-root :double)
             (y-root :double))
            ((:motion-notify) event-motion
             (time :uint32)
             (x :double)
             (y :double)
             (axes (fixed-array :double 2))
             (state modifier-type)
             (is-hint :int16)
             (device (g-object device))
             (x-root :double)
             (y-root :double))
            ((:expose) event-expose
             (area rectangle :inline t)
             (region :pointer)
             (count :int))
            ((:visibility-notify) event-visibility
             (state visibility-state))
            ((:enter-notify :leave-notify) event-crossing
             (sub-window (g-object gdk-window))
             (time :uint32)
             (x :double)
             (y :double)
             (x-root :double)
             (y-root :double)
             (mode crossing-mode)
             (detail notify-type)
             (focus :boolean)
             (state :uint))
            ((:focus-change) event-focus
             (in :int16))
            ((:configure) event-configure
             (x :int)
             (y :int)
             (width :int)
             (height :int))
            ((:property-notify) event-property
             (atom gdk-atom)
             (time :uint32)
             (state property-state))
            ((:selection-clear
              :selection-notify
              :selection-request) event-selection
             (selection gdk-atom)
             (target gdk-atom)
             (property gdk-atom)
             (time :uint32)
             (requestor native-window))
            ((:drag-enter
              :drag-leave
              :drag-motion
              :drag-status
              :drop-start
              :drop-finished) event-dnd
             (drag-context :pointer)
             (time :uint32)
             (x-root :short)
             (y-root :short))
            ((:proximity-in
              :proximity-out) event-proximity
             (time :uint32)
             (device (g-object device)))
            ((:client-event) event-client
             (message-time gdk-atom)
             (data-format :ushort)
             (:variant data-format
                       (8 event-client-8
                          (data :uchar :count 20))
                       (16 event-client-16
                           (data :ushort :count 10))
                       (32 event-client-32
                           (data :ulong :count 5))))
            ((:no-expose) event-no-expose)
            ((:window-state) event-window-state
             (changed-mask window-state)
             (new-window-state window-state))
            ((:setting) event-setting
             (action setting-action)
             (name (:string :free-from-foreign nil :free-to-foreign nil)))
            ((:owner-change) event-owner-change
             (owner native-window)
             (reason owner-change)
             (selection gdk-atom)
             (time :uint32)
             (selection-time :uint32))
            ((:grab-broken) event-grab-broken
             (keyboard :boolean)
             (implicit :boolean)
             (grab-window (g-object gdk-window)))))

(export (boxed-related-symbols 'event))

(define-g-object-class "GdkDragContext" drag-context () ())

(define-g-flags "GdkWindowHints" window-hints ()
  :pos :min-size :max-size :base-size :aspect
  :resize-inc :win-gravity :user-pos :user-size)

(define-g-enum "GdkWindowEdge" window-edge ()
  (:north-west 0) (:north 1) (:north-east 2) (:west 3)
  (:east 4) (:south-west 5) (:south 6) (:south-east 7))

(define-g-enum "GdkColorspace" colorspace ()
  :rgb)

(define-g-object-class "GdkPixbuf" pixbuf ()
    ((colorspace pixbuf-colorspace "colorspace" "GdkColorspace" t nil)
     (n-channels pixbuf-n-channels "n-channels" "gint" t nil)
     (has-alpha pixbuf-has-alpha "has-alpha" "gboolean" t nil)
     (bits-per-sample pixbuf-bits-per-sample "bits-per-sample" "gint" t nil)
     (width pixbuf-width "width" "gint" t nil)
     (height pixbuf-height "height" "gint" t nil)
     (rowstride pixbuf-rowstride "rowstride" "gint" t nil)
     (pixels pixbuf-pixels "pixels" "gpointer" t nil)))

(define-g-object-class "GdkPixbufAnimation" pixbuf-animation ()
    nil) 

(define-g-object-class "GdkImage" gdk-image
  (:superclass g-object :export t :interfaces
               nil :type-initializer
               "gdk_image_get_type")
  ((:cffi colormap gdk-image-colormap (g-object gdk-colormap)
          "gdk_image_get_colormap" "gdk_image_set_colormap")))

(define-g-enum "GdkImageType"
    gdk-image-type
    (:export t :type-initializer "gdk_image_type_get_type")
  (:normal 0)
  (:shared 1)
  (:fastest 2))

(define-g-enum "GdkPixbufAlphaMode"
    pixbuf-alpha-mode
    (:export t :type-initializer "gdk_pixbuf_alpha_mode_get_type")
  (:bilevel 0)
  (:full 1))

