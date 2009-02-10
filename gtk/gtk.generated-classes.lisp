(in-package :gtk)
(define-g-enum "GtkTextDirection" text-direction (t) (:none 0) (:ltr 1)
               (:rtl 2))

(define-g-enum "GtkSizeGroupMode" size-group-mode (t) (:none 0) (:horizontal 1)
               (:vertical 2) (:both 3))

(define-g-enum "GtkUnit" unit (t) (:pixel 0) (:points 1) (:inch 2) (:mm 3))

(define-g-enum "GtkPrintStatus" print-status (t) (:initial 0) (:preparing 1)
               (:generating-data 2) (:sending-data 3) (:pending 4)
               (:pending-issue 5) (:printing 6) (:finished 7)
               (:finished-aborted 8))

(define-g-enum "GtkRecentSortType" recent-sort-type (t) (:none 0) (:mru 1)
               (:lru 2) (:custom 3))

(define-g-enum "GtkFileChooserAction" file-chooser-action (t) (:open 0)
               (:save 1) (:select-folder 2) (:create-folder 3))

(define-g-enum "GtkCellRendererAccelMode" cell-renderer-accel-mode (t) (:gtk 0)
               (:other 1))

(define-g-enum "GtkCellRendererMode" cell-renderer-mode (t) (:inert 0)
               (:activatable 1) (:editable 2))

(define-g-enum "GtkTreeViewColumnSizing" tree-view-column-sizing (t)
               (:grow-only 0) (:autosize 1) (:fixed 2))

(define-g-enum "GtkProgressBarOrientation" progress-bar-orientation (t)
               (:left-to-right 0) (:right-to-left 1) (:bottom-to-top 2)
               (:top-to-bottom 3))

(define-g-enum "GtkProgressBarStyle" progress-bar-style (t) (:continuous 0)
               (:discrete 1))

(define-g-enum "GtkUpdateType" update-type (t) (:continuous 0)
               (:discontinuous 1) (:delayed 2))

(define-g-enum "GtkMetricType" metric-type (t) (:pixels 0) (:inches 1)
               (:centimeters 2))

(define-g-enum "GtkSpinButtonUpdatePolicy" spin-button-update-policy (t)
               (:always 0) (:if-valid 1))

(define-g-enum "GtkCurveType" curve-type (t) (:linear 0) (:spline 1) (:free 2))

(define-g-enum "GtkImageType" image-type (t) (:empty 0) (:pixmap 1) (:image 2)
               (:pixbuf 3) (:stock 4) (:icon-set 5) (:animation 6)
               (:icon-name 7) (:gicon 8))

(define-g-enum "GtkArrowType" arrow-type (t) (:up 0) (:down 1) (:left 2)
               (:right 3) (:none 4))

(define-g-enum "GtkSortType" sort-type (t) (:ascending 0) (:descending 1))

(define-g-enum "GtkToolbarStyle" toolbar-style (t) (:icons 0) (:text 1)
               (:both 2) (:both-horiz 3))

(define-g-enum "GtkWrapMode" wrap-mode (t) (:none 0) (:char 1) (:word 2)
               (:word-char 3))

(define-g-enum "GtkJustification" justification (t) (:left 0) (:right 1)
               (:center 2) (:fill 3))

(define-g-enum "GtkButtonBoxStyle" button-box-style (t) (:default-style 0)
               (:spread 1) (:edge 2) (:start 3) (:end 4) (:center 5))

(define-g-enum "GtkSelectionMode" selection-mode (t) (:none 0) (:single 1)
               (:browse 2) (:multiple 3) (:extended 3))

(define-g-enum "GtkTreeViewGridLines" tree-view-grid-lines (t) (:none 0)
               (:horizontal 1) (:vertical 2) (:both 3))

(define-g-enum "GtkPackDirection" pack-direction (t) (:ltr 0) (:rtl 1) (:ttb 2)
               (:btt 3))

(define-g-enum "GtkPolicyType" policy-type (t) (:always 0) (:automatic 1)
               (:never 2))

(define-g-enum "GtkCornerType" corner-type (t) (:top-left 0) (:bottom-left 1)
               (:top-right 2) (:bottom-right 3))

(define-g-enum "GtkSensitivityType" sensitivity-type (t) (:auto 0) (:on 1)
               (:off 2))

(define-g-enum "GtkShadowType" shadow-type (t) (:none 0) (:in 1) (:out 2)
               (:etched-in 3) (:etched-out 4))

(define-g-enum "GtkIconSize" icon-size (t) (:invalid 0) (:menu 1)
               (:small-toolbar 2) (:large-toolbar 3) (:button 4) (:dnd 5)
               (:dialog 6))

(define-g-enum "GtkOrientation" orientation (t) (:horizontal 0) (:vertical 1))

(define-g-enum "GtkPositionType" position-type (t) (:left 0) (:right 1)
               (:top 2) (:bottom 3))

(define-g-enum "GtkReliefStyle" relief-style (t) (:normal 0) (:half 1)
               (:none 2))

(define-g-enum "GtkMessageType" message-type (t) (:info 0) (:warning 1)
               (:question 2) (:error 3) (:other 4))

(define-g-enum "GtkButtonsType" buttons-type (t) (:none 0) (:ok 1) (:close 2)
               (:cancel 3) (:yes-no 4) (:ok-cancel 5))

(define-g-enum "GtkWindowPosition" window-position (t) (:none 0) (:center 1)
               (:mouse 2) (:center-always 3) (:center-on-parent 4))

(define-g-enum "GtkWindowType" window-type (t) (:toplevel 0) (:popup 1))

(define-g-enum "GtkResizeMode" resize-mode (t) (:parent 0) (:queue 1)
               (:immediate 2))

(define-g-flags "GdkModifierType" gdk-modifier-type (t) (:shift-mask 1)
                (:lock-mask 2) (:control-mask 4) (:mod1-mask 8) (:mod2-mask 16)
                (:mod3-mask 32) (:mod4-mask 64) (:mod5-mask 128)
                (:button1-mask 256) (:button2-mask 512) (:button3-mask 1024)
                (:button4-mask 2048) (:button5-mask 4096)
                (:super-mask 67108864) (:hyper-mask 134217728)
                (:meta-mask 268435456) (:release-mask 1073741824)
                (:modifier-mask 1543512063))

(define-g-enum "GtkTextBufferTargetInfo" text-buffer-target-info (t)
               (:buffer-contents -1) (:rich-text -2) (:text -3))

(define-g-flags "GtkTextSearchFlags" text-search-flags (t) (:visible-only 1)
                (:text-only 2))

(define-g-interface "GtkBuildable" buildable (t))

(define-g-interface "GtkCellEditable" cell-editable (t))

(define-g-interface "GtkCellLayout" cell-layout (t))

(define-g-interface "GtkEditable" editable (t))

(define-g-interface "GtkFileChooser" file-chooser (t)
                    (extra-widget file-chooser-extra-widget "extra-widget"
                     "GtkWidget" t t)
                    (use-preview-label file-chooser-use-preview-label
                     "use-preview-label" "gboolean" t t)
                    (file-system-backend file-chooser-file-system-backend
                     "file-system-backend" "gchararray" nil nil)
                    (filter file-chooser-filter "filter" "GtkFileFilter" t t)
                    (action file-chooser-action "action" "GtkFileChooserAction"
                     t t)
                    (show-hidden file-chooser-show-hidden "show-hidden"
                     "gboolean" t t)
                    (do-overwrite-confirmation
                     file-chooser-do-overwrite-confirmation
                     "do-overwrite-confirmation" "gboolean" t t)
                    (preview-widget file-chooser-preview-widget
                     "preview-widget" "GtkWidget" t t)
                    (select-multiple file-chooser-select-multiple
                     "select-multiple" "gboolean" t t)
                    (local-only file-chooser-local-only "local-only" "gboolean"
                     t t)
                    (preview-widget-active file-chooser-preview-widget-active
                     "preview-widget-active" "gboolean" t t))

(define-g-interface "GtkFileChooserEmbed" file-chooser-embed (t))

(define-g-interface "GtkTreeModel" tree-model (t))

(define-g-interface "GtkTreeDragSource" tree-drag-source (t))

(define-g-interface "GtkTreeDragDest" tree-drag-dest (t))

(define-g-interface "GtkTreeSortable" tree-sortable (t))

(define-g-interface "GtkPrintOperationPreview" print-operation-preview (t))

(define-g-interface "GtkRecentChooser" recent-chooser (t)
                    (filter recent-chooser-filter "filter" "GtkRecentFilter" t
                     t)
                    (show-not-found recent-chooser-show-not-found
                     "show-not-found" "gboolean" t t)
                    (show-tips recent-chooser-show-tips "show-tips" "gboolean"
                     t t)
                    (show-icons recent-chooser-show-icons "show-icons"
                     "gboolean" t t)
                    (select-multiple recent-chooser-select-multiple
                     "select-multiple" "gboolean" t t)
                    (local-only recent-chooser-local-only "local-only"
                     "gboolean" t t)
                    (sort-type recent-chooser-sort-type "sort-type"
                     "GtkRecentSortType" t t)
                    (show-private recent-chooser-show-private "show-private"
                     "gboolean" t t)
                    (limit recent-chooser-limit "limit" "gint" t t)
                    (recent-manager recent-chooser-recent-manager
                     "recent-manager" "GtkRecentManager" nil nil))

(define-g-interface "GtkToolShell" tool-shell (t))

(define-g-interface "AtkImplementorIface" atk-implementor-iface (t))

(define-g-object-class "GtkObject" gtk-object (g-initially-unowned t) nil
                       (user-data gtk-object-user-data "user-data" "gpointer" t
                        t))

(define-g-object-class "GtkWidget" widget (gtk-object t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (name widget-name "name" "gchararray" t t)
                       (parent widget-parent "parent" "GtkContainer" t t)
                       (width-request widget-width-request "width-request"
                        "gint" t t)
                       (height-request widget-height-request "height-request"
                        "gint" t t)
                       (visible widget-visible "visible" "gboolean" t t)
                       (sensitive widget-sensitive "sensitive" "gboolean" t t)
                       (app-paintable widget-app-paintable "app-paintable"
                        "gboolean" t t)
                       (can-focus widget-can-focus "can-focus" "gboolean" t t)
                       (has-focus widget-has-focus "has-focus" "gboolean" t t)
                       (is-focus widget-is-focus "is-focus" "gboolean" t t)
                       (can-default widget-can-default "can-default" "gboolean"
                        t t)
                       (has-default widget-has-default "has-default" "gboolean"
                        t t)
                       (receives-default widget-receives-default
                        "receives-default" "gboolean" t t)
                       (composite-child widget-composite-child
                        "composite-child" "gboolean" t nil)
                       (style widget-style "style" "GtkStyle" t t)
                       (events widget-events "events" "GdkEventMask" t t)
                       (extension-events widget-extension-events
                        "extension-events" "GdkExtensionMode" t t)
                       (no-show-all widget-no-show-all "no-show-all" "gboolean"
                        t t)
                       (has-tooltip widget-has-tooltip "has-tooltip" "gboolean"
                        t t)
                       (tooltip-markup widget-tooltip-markup "tooltip-markup"
                        "gchararray" t t)
                       (tooltip-text widget-tooltip-text "tooltip-text"
                        "gchararray" t t)
                       (window widget-window "window" "GdkWindow" t nil))

(define-g-object-class "GtkContainer" container (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (border-width container-border-width "border-width"
                        "guint" t t)
                       (resize-mode container-resize-mode "resize-mode"
                        "GtkResizeMode" t t)
                       (child container-child "child" "GtkWidget" nil t))

(define-g-object-class "GtkBin" bin (container t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkWindow" gtk-window (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (type gtk-window-type "type" "GtkWindowType" t nil)
                       (title gtk-window-title "title" "gchararray" t t)
                       (startup-id gtk-window-startup-id "startup-id"
                        "gchararray" nil t)
                       (role gtk-window-role "role" "gchararray" t t)
                       (allow-shrink gtk-window-allow-shrink "allow-shrink"
                        "gboolean" t t)
                       (allow-grow gtk-window-allow-grow "allow-grow"
                        "gboolean" t t)
                       (resizable gtk-window-resizable "resizable" "gboolean" t
                        t)
                       (modal gtk-window-modal "modal" "gboolean" t t)
                       (window-position gtk-window-window-position
                        "window-position" "GtkWindowPosition" t t)
                       (default-width gtk-window-default-width "default-width"
                        "gint" t t)
                       (default-height gtk-window-default-height
                        "default-height" "gint" t t)
                       (destroy-with-parent gtk-window-destroy-with-parent
                        "destroy-with-parent" "gboolean" t t)
                       (icon gtk-window-icon "icon" "GdkPixbuf" t t)
                       (icon-name gtk-window-icon-name "icon-name" "gchararray"
                        t t)
                       (screen gtk-window-screen "screen" "GdkScreen" t t)
                       (type-hint gtk-window-type-hint "type-hint"
                        "GdkWindowTypeHint" t t)
                       (skip-taskbar-hint gtk-window-skip-taskbar-hint
                        "skip-taskbar-hint" "gboolean" t t)
                       (skip-pager-hint gtk-window-skip-pager-hint
                        "skip-pager-hint" "gboolean" t t)
                       (urgency-hint gtk-window-urgency-hint "urgency-hint"
                        "gboolean" t t)
                       (accept-focus gtk-window-accept-focus "accept-focus"
                        "gboolean" t t)
                       (focus-on-map gtk-window-focus-on-map "focus-on-map"
                        "gboolean" t t)
                       (decorated gtk-window-decorated "decorated" "gboolean" t
                        t)
                       (deletable gtk-window-deletable "deletable" "gboolean" t
                        t)
                       (gravity gtk-window-gravity "gravity" "GdkGravity" t t)
                       (transient-for gtk-window-transient-for "transient-for"
                        "GtkWindow" t t)
                       (opacity gtk-window-opacity "opacity" "gdouble" t t)
                       (is-active gtk-window-is-active "is-active" "gboolean" t
                        nil)
                       (has-toplevel-focus gtk-window-has-toplevel-focus
                        "has-toplevel-focus" "gboolean" t nil))

(define-g-object-class "GtkDialog" dialog (gtk-window t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (has-separator dialog-has-separator "has-separator"
                        "gboolean" t t))

(define-g-object-class "GtkAboutDialog" about-dialog (dialog t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (program-name about-dialog-program-name "program-name"
                        "gchararray" t t)
                       (version about-dialog-version "version" "gchararray" t
                        t)
                       (copyright about-dialog-copyright "copyright"
                        "gchararray" t t)
                       (comments about-dialog-comments "comments" "gchararray"
                        t t)
                       (website about-dialog-website "website" "gchararray" t
                        t)
                       (website-label about-dialog-website-label
                        "website-label" "gchararray" t t)
                       (license about-dialog-license "license" "gchararray" t
                        t)
                       (authors about-dialog-authors "authors" "GStrv" t t)
                       (documenters about-dialog-documenters "documenters"
                        "GStrv" t t)
                       (translator-credits about-dialog-translator-credits
                        "translator-credits" "gchararray" t t)
                       (artists about-dialog-artists "artists" "GStrv" t t)
                       (logo about-dialog-logo "logo" "GdkPixbuf" t t)
                       (logo-icon-name about-dialog-logo-icon-name
                        "logo-icon-name" "gchararray" t t)
                       (wrap-license about-dialog-wrap-license "wrap-license"
                        "gboolean" t t))

(define-g-object-class "GtkColorSelectionDialog" color-selection-dialog
                       (dialog t) ("AtkImplementorIface" "GtkBuildable")
                       (color-selection color-selection-dialog-color-selection
                        "color-selection" "GtkWidget" t nil)
                       (ok-button color-selection-dialog-ok-button "ok-button"
                        "GtkWidget" t nil)
                       (cancel-button color-selection-dialog-cancel-button
                        "cancel-button" "GtkWidget" t nil)
                       (help-button color-selection-dialog-help-button
                        "help-button" "GtkWidget" t nil))

(define-g-object-class "GtkFileChooserDialog" file-chooser-dialog (dialog t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"))

(define-g-object-class "GtkFontSelectionDialog" font-selection-dialog
                       (dialog t) ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkInputDialog" input-dialog (dialog t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkMessageDialog" message-dialog (dialog t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (message-type message-dialog-message-type "message-type"
                        "GtkMessageType" t t)
                       (buttons message-dialog-buttons "buttons"
                        "GtkButtonsType" nil nil)
                       (text message-dialog-text "text" "gchararray" t t)
                       (use-markup message-dialog-use-markup "use-markup"
                        "gboolean" t t)
                       (secondary-text message-dialog-secondary-text
                        "secondary-text" "gchararray" t t)
                       (secondary-use-markup
                        message-dialog-secondary-use-markup
                        "secondary-use-markup" "gboolean" t t)
                       (image message-dialog-image "image" "GtkWidget" t t))

(define-g-object-class "GtkRecentChooserDialog" recent-chooser-dialog
                       (dialog t)
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkRecentChooser"))

(define-g-object-class "GtkAssistant" assistant (gtk-window t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkPlug" plug (gtk-window t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (embedded plug-embedded "embedded" "gboolean" t nil)
                       (socket-window plug-socket-window "socket-window"
                        "GdkWindow" t nil))

(define-g-object-class "GtkItem" item (bin t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkMenuItem" menu-item (item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (right-justified menu-item-right-justified
                        "right-justified" "gboolean" t t)
                       (submenu menu-item-submenu "submenu" "GtkMenu" t t)
                       (accel-path menu-item-accel-path "accel-path"
                        "gchararray" t t))

(define-g-object-class "GtkCheckMenuItem" check-menu-item (menu-item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (active check-menu-item-active "active" "gboolean" t t)
                       (inconsistent check-menu-item-inconsistent
                        "inconsistent" "gboolean" t t)
                       (draw-as-radio check-menu-item-draw-as-radio
                        "draw-as-radio" "gboolean" t t))

(define-g-object-class "GtkRadioMenuItem" radio-menu-item (check-menu-item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (group radio-menu-item-group "group" "GtkRadioMenuItem"
                        nil t))

(define-g-object-class "GtkImageMenuItem" image-menu-item (menu-item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (image image-menu-item-image "image" "GtkWidget" t t))

(define-g-object-class "GtkSeparatorMenuItem" separator-menu-item (menu-item t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkTearoffMenuItem" tearoff-menu-item (menu-item t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkTreeItem" tree-item (item t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkButton" button (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (label button-label "label" "gchararray" t t)
                       (image button-image "image" "GtkWidget" t t)
                       (relief button-relief "relief" "GtkReliefStyle" t t)
                       (use-underline button-use-underline "use-underline"
                        "gboolean" t t)
                       (use-stock button-use-stock "use-stock" "gboolean" t t)
                       (focus-on-click button-focus-on-click "focus-on-click"
                        "gboolean" t t)
                       (xalign button-xalign "xalign" "gfloat" t t)
                       (yalign button-yalign "yalign" "gfloat" t t)
                       (image-position button-image-position "image-position"
                        "GtkPositionType" t t))

(define-g-object-class "GtkToggleButton" toggle-button (button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (active toggle-button-active "active" "gboolean" t t)
                       (inconsistent toggle-button-inconsistent "inconsistent"
                        "gboolean" t t)
                       (draw-indicator toggle-button-draw-indicator
                        "draw-indicator" "gboolean" t t))

(define-g-object-class "GtkCheckButton" check-button (toggle-button t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkRadioButton" radio-button (check-button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (group radio-button-group "group" "GtkRadioButton" nil
                        t))

(define-g-object-class "GtkColorButton" color-button (button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (use-alpha color-button-use-alpha "use-alpha" "gboolean"
                        t t)
                       (title color-button-title "title" "gchararray" t t)
                       (color color-button-color "color" "GdkColor" t t)
                       (alpha color-button-alpha "alpha" "guint" t t))

(define-g-object-class "GtkFontButton" font-button (button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (title font-button-title "title" "gchararray" t t)
                       (font-name font-button-font-name "font-name"
                        "gchararray" t t)
                       (use-font font-button-use-font "use-font" "gboolean" t
                        t)
                       (use-size font-button-use-size "use-size" "gboolean" t
                        t)
                       (show-style font-button-show-style "show-style"
                        "gboolean" t t)
                       (show-size font-button-show-size "show-size" "gboolean"
                        t t))

(define-g-object-class "GtkLinkButton" link-button (button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (uri link-button-uri "uri" "gchararray" t t)
                       (visited link-button-visited "visited" "gboolean" t t))

(define-g-object-class "GtkScaleButton" scale-button (button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (orientation scale-button-orientation "orientation"
                        "GtkOrientation" t t)
                       (value scale-button-value "value" "gdouble" t t)
                       (size scale-button-size "size" "GtkIconSize" t t)
                       (adjustment scale-button-adjustment "adjustment"
                        "GtkAdjustment" t t)
                       (icons scale-button-icons "icons" "GStrv" t t))

(define-g-object-class "GtkVolumeButton" volume-button (scale-button t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkAlignment" alignment (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (xalign alignment-xalign "xalign" "gfloat" t t)
                       (yalign alignment-yalign "yalign" "gfloat" t t)
                       (xscale alignment-xscale "xscale" "gfloat" t t)
                       (yscale alignment-yscale "yscale" "gfloat" t t)
                       (top-padding alignment-top-padding "top-padding" "guint"
                        t t)
                       (bottom-padding alignment-bottom-padding
                        "bottom-padding" "guint" t t)
                       (left-padding alignment-left-padding "left-padding"
                        "guint" t t)
                       (right-padding alignment-right-padding "right-padding"
                        "guint" t t))

(define-g-object-class "GtkFrame" frame (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (label frame-label "label" "gchararray" t t)
                       (label-xalign frame-label-xalign "label-xalign" "gfloat"
                        t t)
                       (label-yalign frame-label-yalign "label-yalign" "gfloat"
                        t t)
                       (shadow frame-shadow "shadow" "GtkShadowType" t t)
                       (shadow-type frame-shadow-type "shadow-type"
                        "GtkShadowType" t t)
                       (label-widget frame-label-widget "label-widget"
                        "GtkWidget" t t))

(define-g-object-class "GtkAspectFrame" aspect-frame (frame t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (xalign aspect-frame-xalign "xalign" "gfloat" t t)
                       (yalign aspect-frame-yalign "yalign" "gfloat" t t)
                       (ratio aspect-frame-ratio "ratio" "gfloat" t t)
                       (obey-child aspect-frame-obey-child "obey-child"
                        "gboolean" t t))

(define-g-object-class "GtkComboBox" combo-box (bin t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
                        "GtkCellEditable")
                       (model combo-box-model "model" "GtkTreeModel" t t)
                       (wrap-width combo-box-wrap-width "wrap-width" "gint" t
                        t)
                       (row-span-column combo-box-row-span-column
                        "row-span-column" "gint" t t)
                       (column-span-column combo-box-column-span-column
                        "column-span-column" "gint" t t)
                       (active combo-box-active "active" "gint" t t)
                       (add-tearoffs combo-box-add-tearoffs "add-tearoffs"
                        "gboolean" t t)
                       (tearoff-title combo-box-tearoff-title "tearoff-title"
                        "gchararray" t t)
                       (has-frame combo-box-has-frame "has-frame" "gboolean" t
                        t)
                       (focus-on-click combo-box-focus-on-click
                        "focus-on-click" "gboolean" t t)
                       (popup-shown combo-box-popup-shown "popup-shown"
                        "gboolean" t nil)
                       (button-sensitivity combo-box-button-sensitivity
                        "button-sensitivity" "GtkSensitivityType" t t))

(define-g-object-class "GtkComboBoxEntry" combo-box-entry (combo-box t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout"
                        "GtkCellEditable")
                       (text-column combo-box-entry-text-column "text-column"
                        "gint" t t))

(define-g-object-class "GtkEventBox" event-box (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (visible-window event-box-visible-window
                        "visible-window" "gboolean" t t)
                       (above-child event-box-above-child "above-child"
                        "gboolean" t t))

(define-g-object-class "GtkExpander" expander (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (expanded expander-expanded "expanded" "gboolean" t t)
                       (label expander-label "label" "gchararray" t t)
                       (use-underline expander-use-underline "use-underline"
                        "gboolean" t t)
                       (use-markup expander-use-markup "use-markup" "gboolean"
                        t t)
                       (spacing expander-spacing "spacing" "gint" t t)
                       (label-widget expander-label-widget "label-widget"
                        "GtkWidget" t t))

(define-g-object-class "GtkHandleBox" handle-box (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (shadow handle-box-shadow "shadow" "GtkShadowType" t t)
                       (shadow-type handle-box-shadow-type "shadow-type"
                        "GtkShadowType" t t)
                       (handle-position handle-box-handle-position
                        "handle-position" "GtkPositionType" t t)
                       (snap-edge handle-box-snap-edge "snap-edge"
                        "GtkPositionType" t t)
                       (snap-edge-set handle-box-snap-edge-set "snap-edge-set"
                        "gboolean" t t)
                       (child-detached handle-box-child-detached
                        "child-detached" "gboolean" t nil))

(define-g-object-class "GtkToolItem" tool-item (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (visible-horizontal tool-item-visible-horizontal
                        "visible-horizontal" "gboolean" t t)
                       (visible-vertical tool-item-visible-vertical
                        "visible-vertical" "gboolean" t t)
                       (is-important tool-item-is-important "is-important"
                        "gboolean" t t))

(define-g-object-class "GtkToolButton" tool-button (tool-item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (label tool-button-label "label" "gchararray" t t)
                       (use-underline tool-button-use-underline "use-underline"
                        "gboolean" t t)
                       (label-widget tool-button-label-widget "label-widget"
                        "GtkWidget" t t)
                       (stock-id tool-button-stock-id "stock-id" "gchararray" t
                        t)
                       (icon-name tool-button-icon-name "icon-name"
                        "gchararray" t t)
                       (icon-widget tool-button-icon-widget "icon-widget"
                        "GtkWidget" t t))

(define-g-object-class "GtkMenuToolButton" menu-tool-button (tool-button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (menu menu-tool-button-menu "menu" "GtkMenu" t t))

(define-g-object-class "GtkToggleToolButton" toggle-tool-button (tool-button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (active toggle-tool-button-active "active" "gboolean" t
                        t))

(define-g-object-class "GtkRadioToolButton" radio-tool-button
                       (toggle-tool-button t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (group radio-tool-button-group "group"
                        "GtkRadioToolButton" nil t))

(define-g-object-class "GtkSeparatorToolItem" separator-tool-item (tool-item t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (draw separator-tool-item-draw "draw" "gboolean" t t))

(define-g-object-class "GtkScrolledWindow" scrolled-window (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (hadjustment scrolled-window-hadjustment "hadjustment"
                        "GtkAdjustment" t t)
                       (vadjustment scrolled-window-vadjustment "vadjustment"
                        "GtkAdjustment" t t)
                       (hscrollbar-policy scrolled-window-hscrollbar-policy
                        "hscrollbar-policy" "GtkPolicyType" t t)
                       (vscrollbar-policy scrolled-window-vscrollbar-policy
                        "vscrollbar-policy" "GtkPolicyType" t t)
                       (window-placement scrolled-window-window-placement
                        "window-placement" "GtkCornerType" t t)
                       (window-placement-set
                        scrolled-window-window-placement-set
                        "window-placement-set" "gboolean" t t)
                       (shadow-type scrolled-window-shadow-type "shadow-type"
                        "GtkShadowType" t t))

(define-g-object-class "GtkViewport" viewport (bin t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (hadjustment viewport-hadjustment "hadjustment"
                        "GtkAdjustment" t t)
                       (vadjustment viewport-vadjustment "vadjustment"
                        "GtkAdjustment" t t)
                       (shadow-type viewport-shadow-type "shadow-type"
                        "GtkShadowType" t t))

(define-g-object-class "GtkMenuShell" menu-shell (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (take-focus menu-shell-take-focus "take-focus"
                        "gboolean" t t))

(define-g-object-class "GtkMenu" menu (menu-shell t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (active menu-active "active" "gint" t t)
                       (accel-group menu-accel-group "accel-group"
                        "GtkAccelGroup" t t)
                       (accel-path menu-accel-path "accel-path" "gchararray" t
                        t)
                       (attach-widget menu-attach-widget "attach-widget"
                        "GtkWidget" t t)
                       (tearoff-state menu-tearoff-state "tearoff-state"
                        "gboolean" t t)
                       (tearoff-title menu-tearoff-title "tearoff-title"
                        "gchararray" t t)
                       (monitor menu-monitor "monitor" "gint" t t))

(define-g-object-class "GtkRecentChooserMenu" recent-chooser-menu (menu t)
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkRecentChooser")
                       (show-numbers recent-chooser-menu-show-numbers
                        "show-numbers" "gboolean" t t))

(define-g-object-class "GtkMenuBar" menu-bar (menu-shell t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (pack-direction menu-bar-pack-direction "pack-direction"
                        "GtkPackDirection" t t)
                       (child-pack-direction menu-bar-child-pack-direction
                        "child-pack-direction" "GtkPackDirection" t t))

(define-g-object-class "GtkNotebook" notebook (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (tab-pos notebook-tab-pos "tab-pos" "GtkPositionType" t
                        t)
                       (show-tabs notebook-show-tabs "show-tabs" "gboolean" t
                        t)
                       (show-border notebook-show-border "show-border"
                        "gboolean" t t)
                       (scrollable notebook-scrollable "scrollable" "gboolean"
                        t t)
                       (tab-border notebook-tab-border "tab-border" "guint" nil
                        t)
                       (tab-hborder notebook-tab-hborder "tab-hborder" "guint"
                        t t)
                       (tab-vborder notebook-tab-vborder "tab-vborder" "guint"
                        t t)
                       (page notebook-page "page" "gint" t t)
                       (enable-popup notebook-enable-popup "enable-popup"
                        "gboolean" t t)
                       (group-id notebook-group-id "group-id" "gint" t t)
                       (group notebook-group "group" "gpointer" t t)
                       (homogeneous notebook-homogeneous "homogeneous"
                        "gboolean" t t))

(define-g-object-class "GtkTreeView" tree-view (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (model tree-view-model "model" "GtkTreeModel" t t)
                       (hadjustment tree-view-hadjustment "hadjustment"
                        "GtkAdjustment" t t)
                       (vadjustment tree-view-vadjustment "vadjustment"
                        "GtkAdjustment" t t)
                       (headers-visible tree-view-headers-visible
                        "headers-visible" "gboolean" t t)
                       (headers-clickable tree-view-headers-clickable
                        "headers-clickable" "gboolean" t t)
                       (expander-column tree-view-expander-column
                        "expander-column" "GtkTreeViewColumn" t t)
                       (reorderable tree-view-reorderable "reorderable"
                        "gboolean" t t)
                       (rules-hint tree-view-rules-hint "rules-hint" "gboolean"
                        t t)
                       (enable-search tree-view-enable-search "enable-search"
                        "gboolean" t t)
                       (search-column tree-view-search-column "search-column"
                        "gint" t t)
                       (fixed-height-mode tree-view-fixed-height-mode
                        "fixed-height-mode" "gboolean" t t)
                       (hover-selection tree-view-hover-selection
                        "hover-selection" "gboolean" t t)
                       (hover-expand tree-view-hover-expand "hover-expand"
                        "gboolean" t t)
                       (show-expanders tree-view-show-expanders
                        "show-expanders" "gboolean" t t)
                       (level-indentation tree-view-level-indentation
                        "level-indentation" "gint" t t)
                       (rubber-banding tree-view-rubber-banding
                        "rubber-banding" "gboolean" t t)
                       (enable-grid-lines tree-view-enable-grid-lines
                        "enable-grid-lines" "GtkTreeViewGridLines" t t)
                       (enable-tree-lines tree-view-enable-tree-lines
                        "enable-tree-lines" "gboolean" t t)
                       (tooltip-column tree-view-tooltip-column
                        "tooltip-column" "gint" t t))

(define-g-object-class "GtkIconView" icon-view (container t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout")
                       (pixbuf-column icon-view-pixbuf-column "pixbuf-column"
                        "gint" t t)
                       (text-column icon-view-text-column "text-column" "gint"
                        t t)
                       (markup-column icon-view-markup-column "markup-column"
                        "gint" t t)
                       (selection-mode icon-view-selection-mode
                        "selection-mode" "GtkSelectionMode" t t)
                       (orientation icon-view-orientation "orientation"
                        "GtkOrientation" t t)
                       (model icon-view-model "model" "GtkTreeModel" t t)
                       (columns icon-view-columns "columns" "gint" t t)
                       (item-width icon-view-item-width "item-width" "gint" t
                        t)
                       (spacing icon-view-spacing "spacing" "gint" t t)
                       (row-spacing icon-view-row-spacing "row-spacing" "gint"
                        t t)
                       (column-spacing icon-view-column-spacing
                        "column-spacing" "gint" t t)
                       (margin icon-view-margin "margin" "gint" t t)
                       (reorderable icon-view-reorderable "reorderable"
                        "gboolean" t t)
                       (tooltip-column icon-view-tooltip-column
                        "tooltip-column" "gint" t t))

(define-g-object-class "GtkBox" box (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (spacing box-spacing "spacing" "gint" t t)
                       (homogeneous box-homogeneous "homogeneous" "gboolean" t
                        t))

(define-g-object-class "GtkButtonBox" button-box (box t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (layout-style button-box-layout-style "layout-style"
                        "GtkButtonBoxStyle" t t))

(define-g-object-class "GtkHButtonBox" h-button-box (button-box t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVButtonBox" v-button-box (button-box t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVBox" v-box (box t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkColorSelection" color-selection (v-box t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (has-palette color-selection-has-palette "has-palette"
                        "gboolean" t t)
                       (has-opacity-control color-selection-has-opacity-control
                        "has-opacity-control" "gboolean" t t)
                       (current-color color-selection-current-color
                        "current-color" "GdkColor" t t)
                       (current-alpha color-selection-current-alpha
                        "current-alpha" "guint" t t))

(define-g-object-class "GtkFileChooserWidget" file-chooser-widget (v-box t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser"
                        "GtkFileChooserEmbed"))

(define-g-object-class "GtkFontSelection" font-selection (v-box t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (font-name font-selection-font-name "font-name"
                        "gchararray" t t)
                       (font font-selection-font "font" "GdkFont" t nil)
                       (preview-text font-selection-preview-text "preview-text"
                        "gchararray" t t))

(define-g-object-class "GtkGammaCurve" gamma-curve (v-box t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkRecentChooserWidget" recent-chooser-widget (v-box t)
                       ("AtkImplementorIface" "GtkBuildable"
                        "GtkRecentChooser"))

(define-g-object-class "GtkHBox" h-box (box t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkFileChooserButton" file-chooser-button (h-box t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkFileChooser")
                       (dialog file-chooser-button-dialog "dialog"
                        "GtkFileChooser" nil nil)
                       (focus-on-click file-chooser-button-focus-on-click
                        "focus-on-click" "gboolean" t t)
                       (title file-chooser-button-title "title" "gchararray" t
                        t)
                       (width-chars file-chooser-button-width-chars
                        "width-chars" "gint" t t))

(define-g-object-class "GtkStatusbar" statusbar (h-box t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (has-resize-grip statusbar-has-resize-grip
                        "has-resize-grip" "gboolean" t t))

(define-g-object-class "GtkCombo" combo (h-box t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (enable-arrow-keys combo-enable-arrow-keys
                        "enable-arrow-keys" "gboolean" t t)
                       (enable-arrows-always combo-enable-arrows-always
                        "enable-arrows-always" "gboolean" t t)
                       (case-sensitive combo-case-sensitive "case-sensitive"
                        "gboolean" t t)
                       (allow-empty combo-allow-empty "allow-empty" "gboolean"
                        t t)
                       (value-in-list combo-value-in-list "value-in-list"
                        "gboolean" t t))

(define-g-object-class "GtkFixed" fixed (container t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkPaned" paned (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (position paned-position "position" "gint" t t)
                       (position-set paned-position-set "position-set"
                        "gboolean" t t)
                       (min-position paned-min-position "min-position" "gint" t
                        nil)
                       (max-position paned-max-position "max-position" "gint" t
                        nil))

(define-g-object-class "GtkHPaned" h-paned (paned t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVPaned" v-paned (paned t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkLayout" layout (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (hadjustment layout-hadjustment "hadjustment"
                        "GtkAdjustment" t t)
                       (vadjustment layout-vadjustment "vadjustment"
                        "GtkAdjustment" t t)
                       (width layout-width "width" "guint" t t)
                       (height layout-height "height" "guint" t t))

(define-g-object-class "GtkSocket" socket (container t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkTable" table (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (n-rows table-n-rows "n-rows" "guint" t t)
                       (n-columns table-n-columns "n-columns" "guint" t t)
                       (column-spacing table-column-spacing "column-spacing"
                        "guint" t t)
                       (row-spacing table-row-spacing "row-spacing" "guint" t
                        t)
                       (homogeneous table-homogeneous "homogeneous" "gboolean"
                        t t))

(define-g-object-class "GtkTextView" text-view (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (pixels-above-lines text-view-pixels-above-lines
                        "pixels-above-lines" "gint" t t)
                       (pixels-below-lines text-view-pixels-below-lines
                        "pixels-below-lines" "gint" t t)
                       (pixels-inside-wrap text-view-pixels-inside-wrap
                        "pixels-inside-wrap" "gint" t t)
                       (editable text-view-editable "editable" "gboolean" t t)
                       (wrap-mode text-view-wrap-mode "wrap-mode" "GtkWrapMode"
                        t t)
                       (justification text-view-justification "justification"
                        "GtkJustification" t t)
                       (left-margin text-view-left-margin "left-margin" "gint"
                        t t)
                       (right-margin text-view-right-margin "right-margin"
                        "gint" t t)
                       (indent text-view-indent "indent" "gint" t t)
                       (tabs text-view-tabs "tabs" "PangoTabArray" t t)
                       (cursor-visible text-view-cursor-visible
                        "cursor-visible" "gboolean" t t)
                       (buffer text-view-buffer "buffer" "GtkTextBuffer" t t)
                       (overwrite text-view-overwrite "overwrite" "gboolean" t
                        t)
                       (accepts-tab text-view-accepts-tab "accepts-tab"
                        "gboolean" t t))

(define-g-object-class "GtkToolbar" toolbar (container t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkToolShell")
                       (orientation toolbar-orientation "orientation"
                        "GtkOrientation" t t)
                       (toolbar-style toolbar-toolbar-style "toolbar-style"
                        "GtkToolbarStyle" t t)
                       (show-arrow toolbar-show-arrow "show-arrow" "gboolean" t
                        t)
                       (tooltips toolbar-tooltips "tooltips" "gboolean" t t)
                       (icon-size toolbar-icon-size "icon-size" "GtkIconSize" t
                        t)
                       (icon-size-set toolbar-icon-size-set "icon-size-set"
                        "gboolean" t t))

(define-g-object-class "GtkTree" tree (container t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkCList" c-list (container t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (n-columns c-list-n-columns "n-columns" "guint" t nil)
                       (shadow-type c-list-shadow-type "shadow-type"
                        "GtkShadowType" t t)
                       (selection-mode c-list-selection-mode "selection-mode"
                        "GtkSelectionMode" t t)
                       (row-height c-list-row-height "row-height" "guint" t t)
                       (titles-active c-list-titles-active "titles-active"
                        "gboolean" t t)
                       (reorderable c-list-reorderable "reorderable" "gboolean"
                        t t)
                       (use-drag-icons c-list-use-drag-icons "use-drag-icons"
                        "gboolean" t t)
                       (sort-type c-list-sort-type "sort-type" "GtkSortType" t
                        t))

(define-g-object-class "GtkMisc" misc (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (xalign misc-xalign "xalign" "gfloat" t t)
                       (yalign misc-yalign "yalign" "gfloat" t t)
                       (xpad misc-xpad "xpad" "gint" t t)
                       (ypad misc-ypad "ypad" "gint" t t))

(define-g-object-class "GtkLabel" label (misc t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (label label-label "label" "gchararray" t t)
                       (attributes label-attributes "attributes"
                        "PangoAttrList" t t)
                       (use-markup label-use-markup "use-markup" "gboolean" t
                        t)
                       (use-underline label-use-underline "use-underline"
                        "gboolean" t t)
                       (justify label-justify "justify" "GtkJustification" t t)
                       (pattern label-pattern "pattern" "gchararray" nil t)
                       (wrap label-wrap "wrap" "gboolean" t t)
                       (wrap-mode label-wrap-mode "wrap-mode" "PangoWrapMode" t
                        t)
                       (selectable label-selectable "selectable" "gboolean" t
                        t)
                       (mnemonic-keyval label-mnemonic-keyval "mnemonic-keyval"
                        "guint" t nil)
                       (mnemonic-widget label-mnemonic-widget "mnemonic-widget"
                        "GtkWidget" t t)
                       (cursor-position label-cursor-position "cursor-position"
                        "gint" t nil)
                       (selection-bound label-selection-bound "selection-bound"
                        "gint" t nil)
                       (ellipsize label-ellipsize "ellipsize"
                        "PangoEllipsizeMode" t t)
                       (width-chars label-width-chars "width-chars" "gint" t t)
                       (single-line-mode label-single-line-mode
                        "single-line-mode" "gboolean" t t)
                       (angle label-angle "angle" "gdouble" t t)
                       (max-width-chars label-max-width-chars "max-width-chars"
                        "gint" t t))

(define-g-object-class "GtkAccelLabel" accel-label (label t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (accel-closure accel-label-accel-closure "accel-closure"
                        "GClosure" t t)
                       (accel-widget accel-label-accel-widget "accel-widget"
                        "GtkWidget" t t))

(define-g-object-class "GtkArrow" arrow (misc t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (arrow-type arrow-arrow-type "arrow-type" "GtkArrowType"
                        t t)
                       (shadow-type arrow-shadow-type "shadow-type"
                        "GtkShadowType" t t))

(define-g-object-class "GtkImage" image (misc t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (pixbuf image-pixbuf "pixbuf" "GdkPixbuf" t t)
                       (pixmap image-pixmap "pixmap" "GdkPixmap" t t)
                       (image image-image "image" "GdkImage" t t)
                       (mask image-mask "mask" "GdkPixmap" t t)
                       (file image-file "file" "gchararray" t t)
                       (stock image-stock "stock" "gchararray" t t)
                       (icon-set image-icon-set "icon-set" "GtkIconSet" t t)
                       (icon-size image-icon-size "icon-size" "gint" t t)
                       (pixel-size image-pixel-size "pixel-size" "gint" t t)
                       (pixbuf-animation image-pixbuf-animation
                        "pixbuf-animation" "GdkPixbufAnimation" t t)
                       (icon-name image-icon-name "icon-name" "gchararray" t t)
                       (storage-type image-storage-type "storage-type"
                        "GtkImageType" t nil)
                       (gicon image-gicon "gicon" "GIcon" t t))

(define-g-object-class "GtkCalendar" calendar (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (year calendar-year "year" "gint" t t)
                       (month calendar-month "month" "gint" t t)
                       (day calendar-day "day" "gint" t t)
                       (show-heading calendar-show-heading "show-heading"
                        "gboolean" t t)
                       (show-day-names calendar-show-day-names "show-day-names"
                        "gboolean" t t)
                       (no-month-change calendar-no-month-change
                        "no-month-change" "gboolean" t t)
                       (show-week-numbers calendar-show-week-numbers
                        "show-week-numbers" "gboolean" t t)
                       (show-details calendar-show-details "show-details"
                        "gboolean" t t)
                       (detail-width-chars calendar-detail-width-chars
                        "detail-width-chars" "gint" t t)
                       (detail-height-rows calendar-detail-height-rows
                        "detail-height-rows" "gint" t t))

(define-g-object-class "GtkCellView" cell-view (widget t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellLayout")
                       (background cell-view-background "background"
                        "gchararray" nil t)
                       (background-gdk cell-view-background-gdk
                        "background-gdk" "GdkColor" t t)
                       (background-set cell-view-background-set
                        "background-set" "gboolean" t t)
                       (model cell-view-model "model" "GtkTreeModel" t t))

(define-g-object-class "GtkDrawingArea" drawing-area (widget t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkCurve" curve (drawing-area t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (curve-type curve-curve-type "curve-type" "GtkCurveType"
                        t t)
                       (min-x curve-min-x "min-x" "gfloat" t t)
                       (max-x curve-max-x "max-x" "gfloat" t t)
                       (min-y curve-min-y "min-y" "gfloat" t t)
                       (max-y curve-max-y "max-y" "gfloat" t t))

(define-g-object-class "GtkEntry" entry (widget t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                        "GtkEditable")
                       (cursor-position entry-cursor-position "cursor-position"
                        "gint" t nil)
                       (selection-bound entry-selection-bound "selection-bound"
                        "gint" t nil)
                       (editable entry-editable "editable" "gboolean" t t)
                       (max-length entry-max-length "max-length" "gint" t t)
                       (visibility entry-visibility "visibility" "gboolean" t
                        t)
                       (has-frame entry-has-frame "has-frame" "gboolean" t t)
                       (inner-border entry-inner-border "inner-border"
                        "GtkBorder" t t)
                       (invisible-char entry-invisible-char "invisible-char"
                        "guint" t t)
                       (activates-default entry-activates-default
                        "activates-default" "gboolean" t t)
                       (width-chars entry-width-chars "width-chars" "gint" t t)
                       (scroll-offset entry-scroll-offset "scroll-offset"
                        "gint" t nil)
                       (text entry-text "text" "gchararray" t t)
                       (xalign entry-xalign "xalign" "gfloat" t t)
                       (truncate-multiline entry-truncate-multiline
                        "truncate-multiline" "gboolean" t t)
                       (shadow-type entry-shadow-type "shadow-type"
                        "GtkShadowType" t t)
                       (overwrite-mode entry-overwrite-mode "overwrite-mode"
                        "gboolean" t t)
                       (text-length entry-text-length "text-length" "guint" t
                        nil))

(define-g-object-class "GtkSpinButton" spin-button (entry t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkCellEditable"
                        "GtkEditable")
                       (adjustment spin-button-adjustment "adjustment"
                        "GtkAdjustment" t t)
                       (climb-rate spin-button-climb-rate "climb-rate"
                        "gdouble" t t)
                       (digits spin-button-digits "digits" "guint" t t)
                       (snap-to-ticks spin-button-snap-to-ticks "snap-to-ticks"
                        "gboolean" t t)
                       (numeric spin-button-numeric "numeric" "gboolean" t t)
                       (wrap spin-button-wrap "wrap" "gboolean" t t)
                       (update-policy spin-button-update-policy "update-policy"
                        "GtkSpinButtonUpdatePolicy" t t)
                       (value spin-button-value "value" "gdouble" t t))

(define-g-object-class "GtkRuler" ruler (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (lower ruler-lower "lower" "gdouble" t t)
                       (upper ruler-upper "upper" "gdouble" t t)
                       (position ruler-position "position" "gdouble" t t)
                       (max-size ruler-max-size "max-size" "gdouble" t t)
                       (metric ruler-metric "metric" "GtkMetricType" t t))

(define-g-object-class "GtkHRuler" h-ruler (ruler t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVRuler" v-ruler (ruler t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkRange" range (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (update-policy range-update-policy "update-policy"
                        "GtkUpdateType" t t)
                       (adjustment range-adjustment "adjustment"
                        "GtkAdjustment" t t)
                       (inverted range-inverted "inverted" "gboolean" t t)
                       (lower-stepper-sensitivity
                        range-lower-stepper-sensitivity
                        "lower-stepper-sensitivity" "GtkSensitivityType" t t)
                       (upper-stepper-sensitivity
                        range-upper-stepper-sensitivity
                        "upper-stepper-sensitivity" "GtkSensitivityType" t t)
                       (show-fill-level range-show-fill-level "show-fill-level"
                        "gboolean" t t)
                       (restrict-to-fill-level range-restrict-to-fill-level
                        "restrict-to-fill-level" "gboolean" t t)
                       (fill-level range-fill-level "fill-level" "gdouble" t t))

(define-g-object-class "GtkScale" scale (range t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (digits scale-digits "digits" "gint" t t)
                       (draw-value scale-draw-value "draw-value" "gboolean" t
                        t)
                       (value-pos scale-value-pos "value-pos" "GtkPositionType"
                        t t))

(define-g-object-class "GtkHScale" h-scale (scale t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVScale" v-scale (scale t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkScrollbar" scrollbar (range t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkHScrollbar" h-scrollbar (scrollbar t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVScrollbar" v-scrollbar (scrollbar t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkSeparator" separator (widget t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkHSeparator" h-separator (separator t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkVSeparator" v-separator (separator t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkHSV" h-s-v (widget t)
                       ("AtkImplementorIface" "GtkBuildable"))

(define-g-object-class "GtkInvisible" invisible (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (screen invisible-screen "screen" "GdkScreen" t t))

(define-g-object-class "GtkProgress" progress (widget t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (activity-mode progress-activity-mode "activity-mode"
                        "gboolean" t t)
                       (show-text progress-show-text "show-text" "gboolean" t
                        t)
                       (text-xalign progress-text-xalign "text-xalign" "gfloat"
                        t t)
                       (text-yalign progress-text-yalign "text-yalign" "gfloat"
                        t t))

(define-g-object-class "GtkProgressBar" progress-bar (progress t)
                       ("AtkImplementorIface" "GtkBuildable")
                       (fraction progress-bar-fraction "fraction" "gdouble" t
                        t)
                       (pulse-step progress-bar-pulse-step "pulse-step"
                        "gdouble" t t)
                       (orientation progress-bar-orientation "orientation"
                        "GtkProgressBarOrientation" t t)
                       (text progress-bar-text "text" "gchararray" t t)
                       (ellipsize progress-bar-ellipsize "ellipsize"
                        "PangoEllipsizeMode" t t)
                       (adjustment progress-bar-adjustment "adjustment"
                        "GtkAdjustment" t t)
                       (bar-style progress-bar-bar-style "bar-style"
                        "GtkProgressBarStyle" t t)
                       (activity-step progress-bar-activity-step
                        "activity-step" "guint" t t)
                       (activity-blocks progress-bar-activity-blocks
                        "activity-blocks" "guint" t t)
                       (discrete-blocks progress-bar-discrete-blocks
                        "discrete-blocks" "guint" t t))

(define-g-object-class "GtkOldEditable" old-editable (widget t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkEditable")
                       (text-position old-editable-text-position
                        "text-position" "gint" t t)
                       (editable old-editable-editable "editable" "gboolean" t
                        t))

(define-g-object-class "GtkText" text (old-editable t)
                       ("AtkImplementorIface" "GtkBuildable" "GtkEditable")
                       (hadjustment text-hadjustment "hadjustment"
                        "GtkAdjustment" t t)
                       (vadjustment text-vadjustment "vadjustment"
                        "GtkAdjustment" t t)
                       (line-wrap text-line-wrap "line-wrap" "gboolean" t t)
                       (word-wrap text-word-wrap "word-wrap" "gboolean" t t))

(define-g-object-class "GtkAdjustment" adjustment (gtk-object t) nil
                       (value adjustment-value "value" "gdouble" t t)
                       (lower adjustment-lower "lower" "gdouble" t t)
                       (upper adjustment-upper "upper" "gdouble" t t)
                       (step-increment adjustment-step-increment
                        "step-increment" "gdouble" t t)
                       (page-increment adjustment-page-increment
                        "page-increment" "gdouble" t t)
                       (page-size adjustment-page-size "page-size" "gdouble" t
                        t))

(define-g-object-class "GtkTreeViewColumn" tree-view-column (gtk-object t)
                       ("GtkBuildable" "GtkCellLayout")
                       (visible tree-view-column-visible "visible" "gboolean" t
                        t)
                       (resizable tree-view-column-resizable "resizable"
                        "gboolean" t t)
                       (width tree-view-column-width "width" "gint" t nil)
                       (spacing tree-view-column-spacing "spacing" "gint" t t)
                       (sizing tree-view-column-sizing "sizing"
                        "GtkTreeViewColumnSizing" t t)
                       (fixed-width tree-view-column-fixed-width "fixed-width"
                        "gint" t t)
                       (min-width tree-view-column-min-width "min-width" "gint"
                        t t)
                       (max-width tree-view-column-max-width "max-width" "gint"
                        t t)
                       (title tree-view-column-title "title" "gchararray" t t)
                       (expand tree-view-column-expand "expand" "gboolean" t t)
                       (clickable tree-view-column-clickable "clickable"
                        "gboolean" t t)
                       (widget tree-view-column-widget "widget" "GtkWidget" t
                        t)
                       (alignment tree-view-column-alignment "alignment"
                        "gfloat" t t)
                       (reorderable tree-view-column-reorderable "reorderable"
                        "gboolean" t t)
                       (sort-indicator tree-view-column-sort-indicator
                        "sort-indicator" "gboolean" t t)
                       (sort-order tree-view-column-sort-order "sort-order"
                        "GtkSortType" t t))

(define-g-object-class "GtkCellRenderer" cell-renderer (gtk-object t) nil
                       (mode cell-renderer-mode "mode" "GtkCellRendererMode" t
                        t)
                       (visible cell-renderer-visible "visible" "gboolean" t t)
                       (sensitive cell-renderer-sensitive "sensitive"
                        "gboolean" t t)
                       (xalign cell-renderer-xalign "xalign" "gfloat" t t)
                       (yalign cell-renderer-yalign "yalign" "gfloat" t t)
                       (xpad cell-renderer-xpad "xpad" "guint" t t)
                       (ypad cell-renderer-ypad "ypad" "guint" t t)
                       (width cell-renderer-width "width" "gint" t t)
                       (height cell-renderer-height "height" "gint" t t)
                       (is-expander cell-renderer-is-expander "is-expander"
                        "gboolean" t t)
                       (is-expanded cell-renderer-is-expanded "is-expanded"
                        "gboolean" t t)
                       (cell-background cell-renderer-cell-background
                        "cell-background" "gchararray" nil t)
                       (cell-background-gdk cell-renderer-cell-background-gdk
                        "cell-background-gdk" "GdkColor" t t)
                       (cell-background-set cell-renderer-cell-background-set
                        "cell-background-set" "gboolean" t t)
                       (editing cell-renderer-editing "editing" "gboolean" t
                        nil))

(define-g-object-class "GtkCellRendererText" cell-renderer-text
                       (cell-renderer t) nil
                       (text cell-renderer-text-text "text" "gchararray" t t)
                       (markup cell-renderer-text-markup "markup" "gchararray"
                        nil t)
                       (attributes cell-renderer-text-attributes "attributes"
                        "PangoAttrList" t t)
                       (single-paragraph-mode
                        cell-renderer-text-single-paragraph-mode
                        "single-paragraph-mode" "gboolean" t t)
                       (width-chars cell-renderer-text-width-chars
                        "width-chars" "gint" t t)
                       (wrap-width cell-renderer-text-wrap-width "wrap-width"
                        "gint" t t)
                       (alignment cell-renderer-text-alignment "alignment"
                        "PangoAlignment" t t)
                       (background cell-renderer-text-background "background"
                        "gchararray" nil t)
                       (foreground cell-renderer-text-foreground "foreground"
                        "gchararray" nil t)
                       (background-gdk cell-renderer-text-background-gdk
                        "background-gdk" "GdkColor" t t)
                       (foreground-gdk cell-renderer-text-foreground-gdk
                        "foreground-gdk" "GdkColor" t t)
                       (font cell-renderer-text-font "font" "gchararray" t t)
                       (font-desc cell-renderer-text-font-desc "font-desc"
                        "PangoFontDescription" t t)
                       (family cell-renderer-text-family "family" "gchararray"
                        t t)
                       (style cell-renderer-text-style "style" "PangoStyle" t
                        t)
                       (variant cell-renderer-text-variant "variant"
                        "PangoVariant" t t)
                       (weight cell-renderer-text-weight "weight" "gint" t t)
                       (stretch cell-renderer-text-stretch "stretch"
                        "PangoStretch" t t)
                       (size cell-renderer-text-size "size" "gint" t t)
                       (size-points cell-renderer-text-size-points
                        "size-points" "gdouble" t t)
                       (scale cell-renderer-text-scale "scale" "gdouble" t t)
                       (editable cell-renderer-text-editable "editable"
                        "gboolean" t t)
                       (strikethrough cell-renderer-text-strikethrough
                        "strikethrough" "gboolean" t t)
                       (underline cell-renderer-text-underline "underline"
                        "PangoUnderline" t t)
                       (rise cell-renderer-text-rise "rise" "gint" t t)
                       (language cell-renderer-text-language "language"
                        "gchararray" t t)
                       (ellipsize cell-renderer-text-ellipsize "ellipsize"
                        "PangoEllipsizeMode" t t)
                       (wrap-mode cell-renderer-text-wrap-mode "wrap-mode"
                        "PangoWrapMode" t t)
                       (background-set cell-renderer-text-background-set
                        "background-set" "gboolean" t t)
                       (foreground-set cell-renderer-text-foreground-set
                        "foreground-set" "gboolean" t t)
                       (family-set cell-renderer-text-family-set "family-set"
                        "gboolean" t t)
                       (style-set cell-renderer-text-style-set "style-set"
                        "gboolean" t t)
                       (variant-set cell-renderer-text-variant-set
                        "variant-set" "gboolean" t t)
                       (weight-set cell-renderer-text-weight-set "weight-set"
                        "gboolean" t t)
                       (stretch-set cell-renderer-text-stretch-set
                        "stretch-set" "gboolean" t t)
                       (size-set cell-renderer-text-size-set "size-set"
                        "gboolean" t t)
                       (scale-set cell-renderer-text-scale-set "scale-set"
                        "gboolean" t t)
                       (editable-set cell-renderer-text-editable-set
                        "editable-set" "gboolean" t t)
                       (strikethrough-set cell-renderer-text-strikethrough-set
                        "strikethrough-set" "gboolean" t t)
                       (underline-set cell-renderer-text-underline-set
                        "underline-set" "gboolean" t t)
                       (rise-set cell-renderer-text-rise-set "rise-set"
                        "gboolean" t t)
                       (language-set cell-renderer-text-language-set
                        "language-set" "gboolean" t t)
                       (ellipsize-set cell-renderer-text-ellipsize-set
                        "ellipsize-set" "gboolean" t t)
                       (align-set cell-renderer-text-align-set "align-set"
                        "gboolean" t t))

(define-g-object-class "GtkCellRendererAccel" cell-renderer-accel
                       (cell-renderer-text t) nil
                       (accel-key cell-renderer-accel-accel-key "accel-key"
                        "guint" t t)
                       (accel-mods cell-renderer-accel-accel-mods "accel-mods"
                        "GdkModifierType" t t)
                       (keycode cell-renderer-accel-keycode "keycode" "guint" t
                        t)
                       (accel-mode cell-renderer-accel-accel-mode "accel-mode"
                        "GtkCellRendererAccelMode" t t))

(define-g-object-class "GtkCellRendererCombo" cell-renderer-combo
                       (cell-renderer-text t) nil
                       (model cell-renderer-combo-model "model" "GtkTreeModel"
                        t t)
                       (text-column cell-renderer-combo-text-column
                        "text-column" "gint" t t)
                       (has-entry cell-renderer-combo-has-entry "has-entry"
                        "gboolean" t t))

(define-g-object-class "GtkCellRendererSpin" cell-renderer-spin
                       (cell-renderer-text t) nil
                       (adjustment cell-renderer-spin-adjustment "adjustment"
                        "GtkAdjustment" t t)
                       (climb-rate cell-renderer-spin-climb-rate "climb-rate"
                        "gdouble" t t)
                       (digits cell-renderer-spin-digits "digits" "guint" t t))

(define-g-object-class "GtkCellRendererPixbuf" cell-renderer-pixbuf
                       (cell-renderer t) nil
                       (pixbuf cell-renderer-pixbuf-pixbuf "pixbuf" "GdkPixbuf"
                        t t)
                       (pixbuf-expander-open
                        cell-renderer-pixbuf-pixbuf-expander-open
                        "pixbuf-expander-open" "GdkPixbuf" t t)
                       (pixbuf-expander-closed
                        cell-renderer-pixbuf-pixbuf-expander-closed
                        "pixbuf-expander-closed" "GdkPixbuf" t t)
                       (stock-id cell-renderer-pixbuf-stock-id "stock-id"
                        "gchararray" t t)
                       (stock-size cell-renderer-pixbuf-stock-size "stock-size"
                        "guint" t t)
                       (stock-detail cell-renderer-pixbuf-stock-detail
                        "stock-detail" "gchararray" t t)
                       (follow-state cell-renderer-pixbuf-follow-state
                        "follow-state" "gboolean" t t)
                       (icon-name cell-renderer-pixbuf-icon-name "icon-name"
                        "gchararray" t t)
                       (gicon cell-renderer-pixbuf-gicon "gicon" "GIcon" t t))

(define-g-object-class "GtkCellRendererProgress" cell-renderer-progress
                       (cell-renderer t) nil
                       (value cell-renderer-progress-value "value" "gint" t t)
                       (text cell-renderer-progress-text "text" "gchararray" t
                        t)
                       (pulse cell-renderer-progress-pulse "pulse" "gint" t t)
                       (text-xalign cell-renderer-progress-text-xalign
                        "text-xalign" "gfloat" t t)
                       (text-yalign cell-renderer-progress-text-yalign
                        "text-yalign" "gfloat" t t)
                       (orientation cell-renderer-progress-orientation
                        "orientation" "GtkProgressBarOrientation" t t))

(define-g-object-class "GtkCellRendererToggle" cell-renderer-toggle
                       (cell-renderer t) nil
                       (activatable cell-renderer-toggle-activatable
                        "activatable" "gboolean" t t)
                       (active cell-renderer-toggle-active "active" "gboolean"
                        t t)
                       (radio cell-renderer-toggle-radio "radio" "gboolean" t
                        t)
                       (inconsistent cell-renderer-toggle-inconsistent
                        "inconsistent" "gboolean" t t)
                       (indicator-size cell-renderer-toggle-indicator-size
                        "indicator-size" "gint" t t))

(define-g-object-class "GtkFileFilter" file-filter (gtk-object t) nil)

(define-g-object-class "GtkRecentFilter" recent-filter (gtk-object t) nil)

(define-g-object-class "GtkTooltips" tooltips (gtk-object t) nil)

(define-g-object-class "GtkSettings" settings (g-object t) nil
                       (gtk-double-click-time settings-gtk-double-click-time
                        "gtk-double-click-time" "gint" t t)
                       (gtk-double-click-distance
                        settings-gtk-double-click-distance
                        "gtk-double-click-distance" "gint" t t)
                       (gtk-cursor-blink settings-gtk-cursor-blink
                        "gtk-cursor-blink" "gboolean" t t)
                       (gtk-cursor-blink-time settings-gtk-cursor-blink-time
                        "gtk-cursor-blink-time" "gint" t t)
                       (gtk-cursor-blink-timeout
                        settings-gtk-cursor-blink-timeout
                        "gtk-cursor-blink-timeout" "gint" t t)
                       (gtk-split-cursor settings-gtk-split-cursor
                        "gtk-split-cursor" "gboolean" t t)
                       (gtk-theme-name settings-gtk-theme-name "gtk-theme-name"
                        "gchararray" t t)
                       (gtk-icon-theme-name settings-gtk-icon-theme-name
                        "gtk-icon-theme-name" "gchararray" t t)
                       (gtk-fallback-icon-theme
                        settings-gtk-fallback-icon-theme
                        "gtk-fallback-icon-theme" "gchararray" t t)
                       (gtk-key-theme-name settings-gtk-key-theme-name
                        "gtk-key-theme-name" "gchararray" t t)
                       (gtk-menu-bar-accel settings-gtk-menu-bar-accel
                        "gtk-menu-bar-accel" "gchararray" t t)
                       (gtk-dnd-drag-threshold settings-gtk-dnd-drag-threshold
                        "gtk-dnd-drag-threshold" "gint" t t)
                       (gtk-font-name settings-gtk-font-name "gtk-font-name"
                        "gchararray" t t)
                       (gtk-icon-sizes settings-gtk-icon-sizes "gtk-icon-sizes"
                        "gchararray" t t)
                       (gtk-modules settings-gtk-modules "gtk-modules"
                        "gchararray" t t)
                       (gtk-xft-antialias settings-gtk-xft-antialias
                        "gtk-xft-antialias" "gint" t t)
                       (gtk-xft-hinting settings-gtk-xft-hinting
                        "gtk-xft-hinting" "gint" t t)
                       (gtk-xft-hintstyle settings-gtk-xft-hintstyle
                        "gtk-xft-hintstyle" "gchararray" t t)
                       (gtk-xft-rgba settings-gtk-xft-rgba "gtk-xft-rgba"
                        "gchararray" t t)
                       (gtk-xft-dpi settings-gtk-xft-dpi "gtk-xft-dpi" "gint" t
                        t)
                       (gtk-cursor-theme-name settings-gtk-cursor-theme-name
                        "gtk-cursor-theme-name" "gchararray" t t)
                       (gtk-cursor-theme-size settings-gtk-cursor-theme-size
                        "gtk-cursor-theme-size" "gint" t t)
                       (gtk-alternative-button-order
                        settings-gtk-alternative-button-order
                        "gtk-alternative-button-order" "gboolean" t t)
                       (gtk-alternative-sort-arrows
                        settings-gtk-alternative-sort-arrows
                        "gtk-alternative-sort-arrows" "gboolean" t t)
                       (gtk-show-input-method-menu
                        settings-gtk-show-input-method-menu
                        "gtk-show-input-method-menu" "gboolean" t t)
                       (gtk-show-unicode-menu settings-gtk-show-unicode-menu
                        "gtk-show-unicode-menu" "gboolean" t t)
                       (gtk-timeout-initial settings-gtk-timeout-initial
                        "gtk-timeout-initial" "gint" t t)
                       (gtk-timeout-repeat settings-gtk-timeout-repeat
                        "gtk-timeout-repeat" "gint" t t)
                       (gtk-timeout-expand settings-gtk-timeout-expand
                        "gtk-timeout-expand" "gint" t t)
                       (gtk-color-scheme settings-gtk-color-scheme
                        "gtk-color-scheme" "gchararray" t t)
                       (gtk-enable-animations settings-gtk-enable-animations
                        "gtk-enable-animations" "gboolean" t t)
                       (gtk-touchscreen-mode settings-gtk-touchscreen-mode
                        "gtk-touchscreen-mode" "gboolean" t t)
                       (gtk-tooltip-timeout settings-gtk-tooltip-timeout
                        "gtk-tooltip-timeout" "gint" t t)
                       (gtk-tooltip-browse-timeout
                        settings-gtk-tooltip-browse-timeout
                        "gtk-tooltip-browse-timeout" "gint" t t)
                       (gtk-tooltip-browse-mode-timeout
                        settings-gtk-tooltip-browse-mode-timeout
                        "gtk-tooltip-browse-mode-timeout" "gint" t t)
                       (gtk-keynav-cursor-only settings-gtk-keynav-cursor-only
                        "gtk-keynav-cursor-only" "gboolean" t t)
                       (gtk-keynav-wrap-around settings-gtk-keynav-wrap-around
                        "gtk-keynav-wrap-around" "gboolean" t t)
                       (gtk-error-bell settings-gtk-error-bell "gtk-error-bell"
                        "gboolean" t t)
                       (color-hash settings-color-hash "color-hash"
                        "GHashTable" t nil)
                       (gtk-file-chooser-backend
                        settings-gtk-file-chooser-backend
                        "gtk-file-chooser-backend" "gchararray" t t)
                       (gtk-print-backends settings-gtk-print-backends
                        "gtk-print-backends" "gchararray" t t)
                       (gtk-print-preview-command
                        settings-gtk-print-preview-command
                        "gtk-print-preview-command" "gchararray" t t)
                       (gtk-enable-mnemonics settings-gtk-enable-mnemonics
                        "gtk-enable-mnemonics" "gboolean" t t)
                       (gtk-enable-accels settings-gtk-enable-accels
                        "gtk-enable-accels" "gboolean" t t)
                       (gtk-recent-files-limit settings-gtk-recent-files-limit
                        "gtk-recent-files-limit" "gint" t t)
                       (gtk-im-module settings-gtk-im-module "gtk-im-module"
                        "gchararray" t t)
                       (gtk-recent-files-max-age
                        settings-gtk-recent-files-max-age
                        "gtk-recent-files-max-age" "gint" t t)
                       (gtk-fontconfig-timestamp
                        settings-gtk-fontconfig-timestamp
                        "gtk-fontconfig-timestamp" "gint" t t)
                       (gtk-sound-theme-name settings-gtk-sound-theme-name
                        "gtk-sound-theme-name" "gchararray" t t)
                       (gtk-enable-input-feedback-sounds
                        settings-gtk-enable-input-feedback-sounds
                        "gtk-enable-input-feedback-sounds" "gboolean" t t)
                       (gtk-enable-event-sounds
                        settings-gtk-enable-event-sounds
                        "gtk-enable-event-sounds" "gboolean" t t)
                       (gtk-enable-tooltips settings-gtk-enable-tooltips
                        "gtk-enable-tooltips" "gboolean" t t)
                       (gtk-button-images settings-gtk-button-images
                        "gtk-button-images" "gboolean" t t)
                       (gtk-label-select-on-focus
                        settings-gtk-label-select-on-focus
                        "gtk-label-select-on-focus" "gboolean" t t)
                       (gtk-menu-images settings-gtk-menu-images
                        "gtk-menu-images" "gboolean" t t)
                       (gtk-scrolled-window-placement
                        settings-gtk-scrolled-window-placement
                        "gtk-scrolled-window-placement" "GtkCornerType" t t)
                       (gtk-can-change-accels settings-gtk-can-change-accels
                        "gtk-can-change-accels" "gboolean" t t)
                       (gtk-menu-popup-delay settings-gtk-menu-popup-delay
                        "gtk-menu-popup-delay" "gint" t t)
                       (gtk-menu-popdown-delay settings-gtk-menu-popdown-delay
                        "gtk-menu-popdown-delay" "gint" t t)
                       (gtk-menu-bar-popup-delay
                        settings-gtk-menu-bar-popup-delay
                        "gtk-menu-bar-popup-delay" "gint" t t)
                       (gtk-color-palette settings-gtk-color-palette
                        "gtk-color-palette" "gchararray" t t)
                       (gtk-toolbar-style settings-gtk-toolbar-style
                        "gtk-toolbar-style" "GtkToolbarStyle" t t)
                       (gtk-toolbar-icon-size settings-gtk-toolbar-icon-size
                        "gtk-toolbar-icon-size" "GtkIconSize" t t)
                       (gtk-entry-select-on-focus
                        settings-gtk-entry-select-on-focus
                        "gtk-entry-select-on-focus" "gboolean" t t)
                       (gtk-entry-password-hint-timeout
                        settings-gtk-entry-password-hint-timeout
                        "gtk-entry-password-hint-timeout" "guint" t t))

(define-g-object-class "GtkRcStyle" rc-style (g-object t) nil)

(define-g-object-class "GtkStyle" style (g-object t) nil)

(define-g-object-class "GtkTooltip" tooltip (g-object t) nil)

(define-g-object-class "GtkAccelGroup" accel-group (g-object t) nil
                       (is-locked accel-group-is-locked "is-locked" "gboolean"
                        t nil)
                       (modifier-mask accel-group-modifier-mask "modifier-mask"
                        "GdkModifierType" t nil))

(define-g-object-class "GtkAccelMap" accel-map (g-object t) nil)

(define-g-object-class "GtkAction" action (g-object t) ("GtkBuildable")
                       (name action-name "name" "gchararray" t nil)
                       (label action-label "label" "gchararray" t t)
                       (short-label action-short-label "short-label"
                        "gchararray" t t)
                       (tooltip action-tooltip "tooltip" "gchararray" t t)
                       (stock-id action-stock-id "stock-id" "gchararray" t t)
                       (icon-name action-icon-name "icon-name" "gchararray" t
                        t)
                       (visible-horizontal action-visible-horizontal
                        "visible-horizontal" "gboolean" t t)
                       (visible-vertical action-visible-vertical
                        "visible-vertical" "gboolean" t t)
                       (visible-overflown action-visible-overflown
                        "visible-overflown" "gboolean" t t)
                       (is-important action-is-important "is-important"
                        "gboolean" t t)
                       (hide-if-empty action-hide-if-empty "hide-if-empty"
                        "gboolean" t t)
                       (sensitive action-sensitive "sensitive" "gboolean" t t)
                       (visible action-visible "visible" "gboolean" t t)
                       (action-group action-action-group "action-group"
                        "GtkActionGroup" t t))

(define-g-object-class "GtkActionGroup" action-group (g-object t)
                       ("GtkBuildable")
                       (name action-group-name "name" "gchararray" t nil)
                       (sensitive action-group-sensitive "sensitive" "gboolean"
                        t t)
                       (visible action-group-visible "visible" "gboolean" t t))

(define-g-object-class "GtkBuilder" builder (g-object t) nil
                       (translation-domain builder-translation-domain
                        "translation-domain" "gchararray" t t))

(define-g-object-class "GtkClipboard" clipboard (g-object t) nil)

(define-g-object-class "GtkEntryCompletion" entry-completion (g-object t)
                       ("GtkBuildable" "GtkCellLayout")
                       (model entry-completion-model "model" "GtkTreeModel" t
                        t)
                       (minimum-key-length entry-completion-minimum-key-length
                        "minimum-key-length" "gint" t t)
                       (text-column entry-completion-text-column "text-column"
                        "gint" t t)
                       (inline-completion entry-completion-inline-completion
                        "inline-completion" "gboolean" t t)
                       (popup-completion entry-completion-popup-completion
                        "popup-completion" "gboolean" t t)
                       (popup-set-width entry-completion-popup-set-width
                        "popup-set-width" "gboolean" t t)
                       (popup-single-match entry-completion-popup-single-match
                        "popup-single-match" "gboolean" t t)
                       (inline-selection entry-completion-inline-selection
                        "inline-selection" "gboolean" t t))

(define-g-object-class "GtkIconFactory" icon-factory (g-object t)
                       ("GtkBuildable"))

(define-g-object-class "GtkIconTheme" icon-theme (g-object t) nil)

(define-g-object-class "GtkIMContext" i-m-context (g-object t) nil)

(define-g-object-class "GtkListStore" list-store (g-object t)
                       ("GtkBuildable" "GtkTreeModel" "GtkTreeDragSource"
                        "GtkTreeDragDest" "GtkTreeSortable"))

(define-g-object-class "GtkPageSetup" page-setup (g-object t) nil)

(define-g-object-class "GtkPrintContext" print-context (g-object t) nil)

(define-g-object-class "GtkPrintOperation" print-operation (g-object t)
                       ("GtkPrintOperationPreview")
                       (default-page-setup print-operation-default-page-setup
                        "default-page-setup" "GtkPageSetup" t t)
                       (print-settings print-operation-print-settings
                        "print-settings" "GtkPrintSettings" t t)
                       (job-name print-operation-job-name "job-name"
                        "gchararray" t t)
                       (n-pages print-operation-n-pages "n-pages" "gint" t t)
                       (current-page print-operation-current-page
                        "current-page" "gint" t t)
                       (use-full-page print-operation-use-full-page
                        "use-full-page" "gboolean" t t)
                       (track-print-status print-operation-track-print-status
                        "track-print-status" "gboolean" t t)
                       (unit print-operation-unit "unit" "GtkUnit" t t)
                       (show-progress print-operation-show-progress
                        "show-progress" "gboolean" t t)
                       (allow-async print-operation-allow-async "allow-async"
                        "gboolean" t t)
                       (export-filename print-operation-export-filename
                        "export-filename" "gchararray" t t)
                       (status print-operation-status "status" "GtkPrintStatus"
                        t nil)
                       (status-string print-operation-status-string
                        "status-string" "gchararray" t nil)
                       (custom-tab-label print-operation-custom-tab-label
                        "custom-tab-label" "gchararray" t t))

(define-g-object-class "GtkPrintSettings" print-settings (g-object t) nil)

(define-g-object-class "GtkRecentManager" recent-manager (g-object t) nil
                       (filename recent-manager-filename "filename"
                        "gchararray" t nil)
                       (limit recent-manager-limit "limit" "gint" t t)
                       (size recent-manager-size "size" "gint" t nil))

(define-g-object-class "GtkSizeGroup" size-group (g-object t) ("GtkBuildable")
                       (mode size-group-mode "mode" "GtkSizeGroupMode" t t)
                       (ignore-hidden size-group-ignore-hidden "ignore-hidden"
                        "gboolean" t t))

(define-g-object-class "GtkStatusIcon" status-icon (g-object t) nil
                       (pixbuf status-icon-pixbuf "pixbuf" "GdkPixbuf" t t)
                       (file status-icon-file "file" "gchararray" nil t)
                       (stock status-icon-stock "stock" "gchararray" t t)
                       (icon-name status-icon-icon-name "icon-name"
                        "gchararray" t t)
                       (gicon status-icon-gicon "gicon" "GIcon" t t)
                       (storage-type status-icon-storage-type "storage-type"
                        "GtkImageType" t nil)
                       (size status-icon-size "size" "gint" t nil)
                       (screen status-icon-screen "screen" "GdkScreen" t t)
                       (visible status-icon-visible "visible" "gboolean" t t)
                       (orientation status-icon-orientation "orientation"
                        "GtkOrientation" t nil)
                       (embedded status-icon-embedded "embedded" "gboolean" t
                        nil)
                       (blinking status-icon-blinking "blinking" "gboolean" t
                        t))

(define-g-object-class "GtkTextBuffer" text-buffer (g-object t) nil
                       (tag-table text-buffer-tag-table "tag-table"
                        "GtkTextTagTable" t nil)
                       (text text-buffer-text "text" "gchararray" t t)
                       (has-selection text-buffer-has-selection "has-selection"
                        "gboolean" t nil)
                       (cursor-position text-buffer-cursor-position
                        "cursor-position" "gint" t nil)
                       (copy-target-list text-buffer-copy-target-list
                        "copy-target-list" "GtkTargetList" t nil)
                       (paste-target-list text-buffer-paste-target-list
                        "paste-target-list" "GtkTargetList" t nil))

(define-g-object-class "GtkTextChildAnchor" text-child-anchor (g-object t) nil)

(define-g-object-class "GtkTextMark" text-mark (g-object t) nil
                       (name text-mark-name "name" "gchararray" t nil)
                       (left-gravity text-mark-left-gravity "left-gravity"
                        "gboolean" t nil))

(define-g-object-class "GtkTextTag" text-tag (g-object t) nil
                       (name text-tag-name "name" "gchararray" t nil)
                       (background text-tag-background "background"
                        "gchararray" nil t)
                       (foreground text-tag-foreground "foreground"
                        "gchararray" nil t)
                       (background-gdk text-tag-background-gdk "background-gdk"
                        "GdkColor" t t)
                       (foreground-gdk text-tag-foreground-gdk "foreground-gdk"
                        "GdkColor" t t)
                       (background-stipple text-tag-background-stipple
                        "background-stipple" "GdkPixmap" t t)
                       (foreground-stipple text-tag-foreground-stipple
                        "foreground-stipple" "GdkPixmap" t t)
                       (font text-tag-font "font" "gchararray" t t)
                       (font-desc text-tag-font-desc "font-desc"
                        "PangoFontDescription" t t)
                       (family text-tag-family "family" "gchararray" t t)
                       (style text-tag-style "style" "PangoStyle" t t)
                       (variant text-tag-variant "variant" "PangoVariant" t t)
                       (weight text-tag-weight "weight" "gint" t t)
                       (stretch text-tag-stretch "stretch" "PangoStretch" t t)
                       (size text-tag-size "size" "gint" t t)
                       (size-points text-tag-size-points "size-points"
                        "gdouble" t t)
                       (scale text-tag-scale "scale" "gdouble" t t)
                       (pixels-above-lines text-tag-pixels-above-lines
                        "pixels-above-lines" "gint" t t)
                       (pixels-below-lines text-tag-pixels-below-lines
                        "pixels-below-lines" "gint" t t)
                       (pixels-inside-wrap text-tag-pixels-inside-wrap
                        "pixels-inside-wrap" "gint" t t)
                       (editable text-tag-editable "editable" "gboolean" t t)
                       (wrap-mode text-tag-wrap-mode "wrap-mode" "GtkWrapMode"
                        t t)
                       (justification text-tag-justification "justification"
                        "GtkJustification" t t)
                       (direction text-tag-direction "direction"
                        "GtkTextDirection" t t)
                       (left-margin text-tag-left-margin "left-margin" "gint" t
                        t)
                       (indent text-tag-indent "indent" "gint" t t)
                       (strikethrough text-tag-strikethrough "strikethrough"
                        "gboolean" t t)
                       (right-margin text-tag-right-margin "right-margin"
                        "gint" t t)
                       (underline text-tag-underline "underline"
                        "PangoUnderline" t t)
                       (rise text-tag-rise "rise" "gint" t t)
                       (background-full-height text-tag-background-full-height
                        "background-full-height" "gboolean" t t)
                       (language text-tag-language "language" "gchararray" t t)
                       (tabs text-tag-tabs "tabs" "PangoTabArray" t t)
                       (invisible text-tag-invisible "invisible" "gboolean" t
                        t)
                       (paragraph-background text-tag-paragraph-background
                        "paragraph-background" "gchararray" nil t)
                       (paragraph-background-gdk
                        text-tag-paragraph-background-gdk
                        "paragraph-background-gdk" "GdkColor" t t)
                       (accumulative-margin text-tag-accumulative-margin
                        "accumulative-margin" "gboolean" t t)
                       (background-set text-tag-background-set "background-set"
                        "gboolean" t t)
                       (foreground-set text-tag-foreground-set "foreground-set"
                        "gboolean" t t)
                       (background-stipple-set text-tag-background-stipple-set
                        "background-stipple-set" "gboolean" t t)
                       (foreground-stipple-set text-tag-foreground-stipple-set
                        "foreground-stipple-set" "gboolean" t t)
                       (family-set text-tag-family-set "family-set" "gboolean"
                        t t)
                       (style-set text-tag-style-set "style-set" "gboolean" t
                        t)
                       (variant-set text-tag-variant-set "variant-set"
                        "gboolean" t t)
                       (weight-set text-tag-weight-set "weight-set" "gboolean"
                        t t)
                       (stretch-set text-tag-stretch-set "stretch-set"
                        "gboolean" t t)
                       (size-set text-tag-size-set "size-set" "gboolean" t t)
                       (scale-set text-tag-scale-set "scale-set" "gboolean" t
                        t)
                       (pixels-above-lines-set text-tag-pixels-above-lines-set
                        "pixels-above-lines-set" "gboolean" t t)
                       (pixels-below-lines-set text-tag-pixels-below-lines-set
                        "pixels-below-lines-set" "gboolean" t t)
                       (pixels-inside-wrap-set text-tag-pixels-inside-wrap-set
                        "pixels-inside-wrap-set" "gboolean" t t)
                       (editable-set text-tag-editable-set "editable-set"
                        "gboolean" t t)
                       (wrap-mode-set text-tag-wrap-mode-set "wrap-mode-set"
                        "gboolean" t t)
                       (justification-set text-tag-justification-set
                        "justification-set" "gboolean" t t)
                       (left-margin-set text-tag-left-margin-set
                        "left-margin-set" "gboolean" t t)
                       (indent-set text-tag-indent-set "indent-set" "gboolean"
                        t t)
                       (strikethrough-set text-tag-strikethrough-set
                        "strikethrough-set" "gboolean" t t)
                       (right-margin-set text-tag-right-margin-set
                        "right-margin-set" "gboolean" t t)
                       (underline-set text-tag-underline-set "underline-set"
                        "gboolean" t t)
                       (rise-set text-tag-rise-set "rise-set" "gboolean" t t)
                       (background-full-height-set
                        text-tag-background-full-height-set
                        "background-full-height-set" "gboolean" t t)
                       (language-set text-tag-language-set "language-set"
                        "gboolean" t t)
                       (tabs-set text-tag-tabs-set "tabs-set" "gboolean" t t)
                       (invisible-set text-tag-invisible-set "invisible-set"
                        "gboolean" t t)
                       (paragraph-background-set
                        text-tag-paragraph-background-set
                        "paragraph-background-set" "gboolean" t t))

(define-g-object-class "GtkTextTagTable" text-tag-table (g-object t) nil)

(define-g-object-class "GtkTreeModelFilter" tree-model-filter (g-object t)
                       ("GtkTreeModel" "GtkTreeDragSource")
                       (child-model tree-model-filter-child-model "child-model"
                        "GtkTreeModel" t nil)
                       (virtual-root tree-model-filter-virtual-root
                        "virtual-root" "GtkTreePath" t nil))

(define-g-object-class "GtkTreeModelSort" tree-model-sort (g-object t)
                       ("GtkTreeModel" "GtkTreeDragSource" "GtkTreeSortable")
                       (model tree-model-sort-model "model" "GtkTreeModel" t
                        nil))

(define-g-object-class "GtkTreeSelection" tree-selection (g-object t) nil)

(define-g-object-class "GtkTreeStore" tree-store (g-object t)
                       ("GtkBuildable" "GtkTreeModel" "GtkTreeDragSource"
                        "GtkTreeDragDest" "GtkTreeSortable"))

(define-g-object-class "GtkUIManager" u-i-manager (g-object t) ("GtkBuildable")
                       (add-tearoffs u-i-manager-add-tearoffs "add-tearoffs"
                        "gboolean" t t)
                       (ui u-i-manager-ui "ui" "gchararray" t nil))

(define-g-object-class "GtkWindowGroup" window-group (g-object t) nil)

