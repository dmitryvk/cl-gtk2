(in-package :gdk)

(define-g-enum "GdkGrabStatus" grab-status () :success :already-grabbed :invalid-time :not-viewable :frozen)

(define-g-object-class "GdkDisplay" display () ())

(define-g-object-class "GdkDisplayManager" display-manager ()
  ((default-display display-manager-default-display "default-display" "GdkDisplay" t t)))

(define-g-object-class "GdkScreen" screen ()
  ((font-options screen-font-options "font-options" "gpointer" t t)
   (resolution screen-resolution "resolution" "gdouble" t t)))

(define-g-object-class "GdkGC" graphics-context () ())

(define-g-object-class "GdkDrawable" drawable () ())

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

(define-g-boxed-cstruct rectangle "GdkRectangle"
  (x :int :initform 0)
  (y :int :initform 0)
  (width :int :initform 0)
  (height :int :initform 0))

(define-g-boxed-cstruct font "GdkFont"
  (type font-type :initform :font)
  (ascent :int :initform 0)
  (descent :int :initform 0))

(define-g-boxed-cstruct color "GdkColor"
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

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

(defcunion event-client-data-union
  (b :char :count 20)
  (s :short :count 10)
  (l :long :count 5))

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
            ((:button-prees
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
             (is-hint :int)
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
             (y-root :double))
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
              :drop-finished) event-drag
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