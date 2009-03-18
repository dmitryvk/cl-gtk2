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

(gobject::define-g-flags "GdkModifierType" modifier-type ()
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

(define-g-boxed-class ("GdkEvent" event-struct) event ()
  (type event-type)
  (window (g-object gdk-window))
  (send-event (:boolean :int8)))

(define-g-boxed-class nil event-key ((event type (:key-press :key-release)))
  (time :uint32)
  (state modifier-type)
  (keyval :uint)
  (length :int)
  (string (:string :free-from-foreign nil :free-to-foreign nil))
  (hardware-keycode :uint16)
  (group :uint8)
  (is-modifier :uint))

(define-g-boxed-class nil event-button ((event type (:button-press :2button-press :3button-press :button-release)))
  (time :uint32)
  (x :double)
  (y :double)
  (axes (fixed-array :double 2))
  (state :uint)
  (button :uint)
  (device (g-object device))
  (x-root :double)
  (y-root :double))

(define-g-boxed-class nil event-scroll ((event type :scroll))
  (time :uint32)
  (x :double)
  (y :double)
  (state modifier-type)
  (direction scroll-direction)
  (device (g-object device))
  (x-root :double)
  (y-root :double))

(define-g-boxed-class nil event-motion ((event type :motion-notify))
  (time :uint32)
  (x :double)
  (y :double)
  (axes (fixed-array :double 2))
  (state modifier-type)
  (is-hint :int)
  (device (g-object device))
  (x-root :double)
  (y-root :double))

(define-g-boxed-class "GdkRectangle" rectangle ()
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(define-g-boxed-class nil event-expose ((event type :expose))
  (area (g-boxed-inline rectangle))
  (region :pointer)
  (count :int))

(define-g-boxed-class nil event-visibility ((event type :visibility-notify))
  (state visibility-state))

(define-g-boxed-class nil event-crossing ((event type (:enter-notify :leave-notify)))
  (sub-window (g-object gdk-window))
  (time :uint32)
  (x :double)
  (y :double)
  (x-root :double)
  (y-root :double))

(define-g-boxed-class nil event-focus ((event type :focus-change))
  (in :int16))

(define-g-boxed-class nil event-configure ((event type :configure))
  (x :int)
  (y :int)
  (width :int)
  (height :int))

(defctype gdk-atom :pointer)

(define-foreign-type gdk-atom-as-string-type ()
  ()
  (:actual-type :pointer)
  (:simple-parser gdk-atom-as-string))

(defmethod translate-from-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-name value))

(defmethod translate-to-foreign (value (type gdk-atom-as-string-type))
  (gdk-atom-intern value nil))

(define-g-boxed-class nil event-property ((event type :property-notify))
  (atom gdk-atom)
  (time :uint32)
  (state property-state))

;;;FIXME: Check correct type
(defctype native-window :uint32)

(define-g-boxed-class nil event-selection ((event type (:selection-clear :selection-notify :selection-request)))
  (selection gdk-atom)
  (target gdk-atom)
  (property gdk-atom)
  (time :uint32)
  (requestor native-window))

(define-g-object-class "GdkDragContext" drag-context () ())

(define-g-boxed-class nil event-dnd ((event type (:drag-enter :drag-leave :drag-motion :drag-status :drop-start :drop-finished)))
  (drag-context :pointer)
  (time :uint32)
  (x-root :short)
  (y-root :short))

(define-g-boxed-class nil event-proximity ((event type (:proximity-in :proximity-out)))
  (time :uint32)
  (device (g-object device)))

(defcunion event-client-data-union
  (b :char :count 20)
  (s :short :count 10)
  (l :long :count 5))

(define-g-boxed-class nil event-client ((event type :client-event))
  (message-time gdk-atom)
  (data-format :ushort)
  (data event-client-data-union :parser 'event-client-data-union-parser :unparser 'event-client-data-union-unparser))

(defun event-client-data-union-parser (name pointer)
  (declare (ignore name))
  (ecase (foreign-slot-value pointer 'event-client 'data-format)
    (8 (convert-from-foreign (foreign-slot-pointer pointer 'event-client 'data) '(fixed-array :uchar 20)))
    (16 (convert-from-foreign (foreign-slot-pointer pointer 'event-client 'data) '(fixed-array :ushort 20)))
    (32 (convert-from-foreign (foreign-slot-pointer pointer 'event-client 'data) '(fixed-array :ulong 20)))))

(defun event-client-data-union-unparser (name pointer object)
  (declare (ignore name))
  (ecase (event-client-data-format object)
    (8 (loop
          with array-ptr = (foreign-slot-pointer pointer 'event-client 'data)
          for i from 0 below 20
          do (setf (mem-aref array-ptr :uchar i) (aref (event-client-data object) i))))
    (16 (loop
          with array-ptr = (foreign-slot-pointer pointer 'event-client 'data)
          for i from 0 below 20
          do (setf (mem-aref array-ptr :ushort i) (aref (event-client-data object) i))))
    (32 (loop
          with array-ptr = (foreign-slot-pointer pointer 'event-client 'data)
          for i from 0 below 20
          do (setf (mem-aref array-ptr :ulong i) (aref (event-client-data object) i))))))

(define-g-boxed-class nil event-no-expose ((event type :no-expose)))

(define-g-boxed-class nil event-window-state ((event type :window-state))
  (changed-mask window-state)
  (new-window-state window-state))

(define-g-boxed-class nil event-setting ((event type :setting))
  (action setting-action)
  (name (:string :free-from-foreign nil :free-to-foreign nil)))

(define-g-boxed-class nil event-owner-change ((event type :owner-change))
  (owner native-window)
  (reason owner-change)
  (selection gdk-atom)
  (time :uint32)
  (selection-time :uint32))

(define-g-boxed-class nil event-grab-broken ((event type :grab-broken))
  (keyboard :boolean)
  (implicit :boolean)
  (grab-window (g-object gdk-window)))

(define-g-enum "GdkFontType" font-type () :font :fontset)

(define-g-boxed-class "GdkFont" font ()
  (type font-type)
  (ascent :int)
  (descent :int))

(define-g-boxed-class "GdkColor" color ()
  (pixel :uint32 :initform 0)
  (red :uint16 :initform 0)
  (green :uint16 :initform 0)
  (blue :uint16 :initform 0))

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

(define-g-boxed-class "GdkGeometry" geometry ()
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

(define-g-flags "GdkWindowHints" window-hints ()
  :pos :min-size :max-size :base-size :aspect
  :resize-inc :win-gravity :user-pos :user-size)

(define-g-enum "GdkWindowEdge" window-edge ()
  (:north-west 0) (:north 1) (:north-east 2) (:west 3)
  (:east 4) (:south-west 5) (:south 6) (:south-east 7))

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