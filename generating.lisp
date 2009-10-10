(defpackage :gtk-generation
  (:use :cl :gobject :cffi :glib)
  (:export #:gtk-generate
           #:gtk-generate-child-properties))

(in-package :gtk-generation)

(load-foreign-library "libgtk-x11-2.0.so")

(defcfun gtk-init-check :boolean
  (argc (:pointer :int))
  (argv (:pointer (:pointer :string))))

(defun gtk-init ()
  (gtk-init-check (foreign-alloc :int :initial-element 0)
                  (foreign-alloc :string :initial-contents '("/usr/bin/sbcl")))
  #+nil(with-foreign-objects ((argc :int)
                         (argv '(:pointer :string) 1))
    (setf (mem-ref argc :int) 0
          (mem-ref argv '(:pointer :string)) (foreign-alloc :string :count 1
                                                            :initial-element "/usr/bin/sbcl"))
    (unwind-protect
         (unless (gtk-init-check argc argv)
           (error "Cannot initialize Gtk+"))
      (foreign-free (mem-ref argv '(:pointer :string))))))

(gtk-init)

(defcfun gtk-test-register-all-types :void)

(gtk-test-register-all-types)

(defun gtk-generate (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (generate-types-hierarchy-to-file
     stream
     "GtkObject"
     :include-referenced t
     :prefix "Gtk"
     :package (or (find-package :gtk) (make-package :gtk))
     :exceptions `(("GObject" gobject:g-object)
                   ("GtkObject" ,(intern "GTK-OBJECT" (find-package :gtk)))
                   ("GInitiallyUnowned" gobject::g-initially-unowned)
                   ("GtkWindow" ,(intern "GTK-WINDOW" (find-package :gtk)))
                   ("GtkUIManager" ,(intern "UI-MANAGER" (find-package :gtk)))
                   ("GtkUIManagerItemType" ,(intern "UI-MANAGER-ITEM-TYPE" (find-package :gtk))))
     :prologue (format nil "(in-package :gtk)")
     :interfaces '("GtkBuildable"
                   "GtkCellEditable"
                   "GtkCellLayout"
                   "GtkEditable"
                   "GtkFileChooser"
                   "GtkFileChooserEmbed"
                   "GtkTreeModel"
                   "GtkTreeDragSource"
                   "GtkTreeDragDest"
                   "GtkTreeSortable"
                   "GtkPrintOperationPreview"
                   "GtkRecentChooser"
                   "GtkToolShell"
                   "GtkOrientable"
                   "GtkActivatable"
                   "AtkImplementorIface")
     :objects '("GtkSettings" "GtkRcStyle" "GtkStyle" "GtkTooltip" "GtkAccelGroup"
                "GtkAccelMap" "GtkAction" "GtkActionGroup" "GtkBuilder" "GtkClipboard"
                "GtkEntryCompletion" "GtkIconFactory" "GtkIconTheme" "GtkIMContext"
                "GtkListStore" "GtkPageSetup" "GtkPrintContext" "GtkPrintOperation"
                "GtkPrintSettings" "GtkRecentManager" "GtkSizeGroup" "GtkStatusIcon"
                "GtkTextBuffer" "GtkTextChildAnchor" "GtkTextMark" "GtkTextTag"
                "GtkTextTagTable" "GtkTreeModelFilter" "GtkTreeModelSort"
                "GtkTreeSelection" "GtkTreeStore" "GtkUIManager" "GtkWindowGroup"
                "GtkToggleAction" "GtkRecentAction" "GtkRadioAction" "GtkItemFactory"
		"GtkPageSetupUnixDialog" "GtkPrintUnixDialog")
     :flags '("GtkTextSearchFlags" "GtkAccelFlags" "GtkArgFlags" "GtkAttachOptions"
              "GtkButtonAction" "GtkCalendarDisplayOptions" "GtkCellRendererState"
              "GtkDebugFlag" "GtkDestDefaults" "GtkDialogFlags" "GtkFileFilterFlags"
              "GtkIconLookupFlags" "GtkObjectFlags" "GtkPrivateFlags" "GtkRcFlags"
              "GtkRecentFilterFlags" "GtkSignalRunType" "GtkTargetFlags"
              "GtkTreeModelFlags" "GtkUIManagerItemType" "GtkWidgetFlags")
     :enums '("GtkTextDirection" "GtkSizeGroupMode" "GtkUnit" "GtkPrintStatus"
              "GtkRecentSortType" "GtkFileChooserAction" "GtkCellRendererAccelMode"
              "GtkCellRendererMode" "GtkTreeViewColumnSizing"
              "GtkProgressBarOrientation" "GtkProgressBarStyle" "GtkUpdateType"
              "GtkMetricType" "GtkSpinButtonUpdatePolicy" "GtkCurveType" "GtkImageType"
              "GtkArrowType" "GtkSortType" "GtkToolbarStyle" "GtkWrapMode"
              "GtkJustification" "GtkButtonBoxStyle" "GtkSelectionMode"
              "GtkTreeViewGridLines" "GtkPackDirection" "GtkPolicyType" "GtkCornerType"
              "GtkSensitivityType" "GtkShadowType" "GtkIconSize" "GtkOrientation"
              "GtkPositionType" "GtkReliefStyle" "GtkMessageType" "GtkButtonsType"
              "GtkWindowType" "GtkWindowPosition" "GtkResizeMode"
              "GtkTextBufferTargetInfo" "GtkStateType" "GtkDirectionType"
              "GtkDragResult" "GtkWidgetHelpType" "GtkPackType" "GtkNotebookTab"
              "GtkMovementStep" "GtkAnchorType" "GtkAssistantPageType"
              "GtkBuilderError" "GtkCellType" "GtkCListDragPos" "GtkCTreeExpanderStyle"
              "GtkCTreeExpansionType" "GtkCTreeLineStyle" "GtkCTreePos" "GtkDeleteType"
              "GtkExpanderStyle" "GtkFileChooserConfirmation" "GtkFileChooserError"
              "GtkIconThemeError" "GtkIconViewDropPosition" "GtkIMPreeditStyle"
              "GtkIMStatusStyle" "GtkMatchType" "GtkMenuDirectionType"
              "GtkNumberUpLayout" "GtkPageOrientation" "GtkPageSet"
              "GtkPathPriorityType" "GtkPathType" "GtkPreviewType" "GtkPrintDuplex"
              "GtkPrintError" "GtkPrintOperationAction" "GtkPrintOperationResult"
              "GtkPrintPages" "GtkPrintQuality" "GtkRcTokenType"
              "GtkRecentChooserError" "GtkRecentManagerError" "GtkResponseType"
              "GtkScrollStep" "GtkScrollType" "GtkSideType" "GtkSpinType"
              "GtkSubmenuDirection" "GtkSubmenuPlacement" "GtkTextWindowType"
              "GtkToolbarChildType" "GtkToolbarSpaceStyle" "GtkTreeViewDropPosition"
              "GtkTreeViewMode" "GtkVisibility" "GtkEntryIconPosition")
     :exclusions '("PangoStretch" "PangoVariant" "PangoStyle" "PangoUnderline" "GtkGLDrawingArea"
                   "GtkCList" "GtkCTree" "GtkCombo" "GtkFileSelection" "GtkItemFactory" "GtkList"
                   "GtkListItem" "GtkOldEditable" "GtkOptionMenu" "GtkPixmap" "GtkPreview"
                   "GtkText" "GtkTooltips" "GtkTipsQuery" "GtkTree" "GtkTreeItem")
     :additional-properties
     '(("GtkWindow"
	(:cffi gtk::focus gtk::gtk-window-focus (g-object gtk::widget)
	 "gtk_window_get_focus" "gtk_window_set_focus")
	(:cffi gtk::default-widget gtk::gtk-window-default-widget (g-object gtk::widget)
	 "gtk_window_get_default_widget" "gtk_window_set_default")
	(:cffi gtk::has-frame gtk::gtk-window-has-frame :boolean
	 "gtk_window_get_has_frame" "gtk_window_set_has_frame")
	(:cffi gtk::mnemonic-modifier gtk::gtk-window-mnemonic-modifier (g-object gdk::modifier-type)
	 "gtk_window_get_mnemonic_modifier" "gtk_window_set_mnemonic_modifier")
	(:cffi gtk::icon-list gtk::gtk-window-icon-list (glist gtk::pixbuf :free-from-foreign t :free-to-foreign t)
	 "gtk_window_get_icon_list" "gtk_window_set_icon_list")
	(:cffi gtk::group gtk::gtk-window-group (g-object gtk::window-group)
	 "gtk_window_get_group" nil)
        (:cffi gtk::keep-above gtk::gtk-window-keep-above :boolean
         nil "gtk_window_set_keep_above")
        (:cffi gtk::keep-below gtk::gtk-window-keep-below :boolean
         nil "gtk_window_set_keep_below"))
       ("GtkTreeViewColumn"
        (:cffi gtk::tree-view gtk::tree-view-column-tree-view g-object
	 "gtk_tree_view_column_get_tree_view" nil)
        (:cffi gtk::sort-column-id gtk::tree-view-column-sort-column-id :int
         "gtk_tree_view_column_get_sort_column_id" "gtk_tree_view_column_set_sort_column_id")
        (:cffi gtk::cell-renderers gtk::tree-view-column-cell-renderers (glist g-object  :free-from-foreign t)
         "gtk_tree_view_column_get_cell_renderers" nil))
       ("GtkTreeSelection"
        (:cffi gtk::mode gtk::tree-selection-mode gtk::selection-mode
         "gtk_tree_selection_get_mode" "gtk_tree_selection_set_mode")
        (:cffi gtk::select-function gtk::tree-selection-select-function nil
         gtk::tree-selection-get-selection-function gtk::tree-selection-set-select-function)
        (:cffi gtk::tree-view gtk::tree-selection-tree-view (g-object gtk::tree-view)
         "gtk_tree_selection_get_tree_view" nil))
       ("GtkTreeView"
        (:cffi gtk::selection gtk::tree-view-selection g-object
         "gtk_tree_view_get_selection" nil)
        (:cffi gtk::column-drag-function gtk::tree-view-column-drag-function nil
         nil gtk::tree-view-set-column-drag-function)
        (:cffi gtk::bin-window gtk::tree-view-bin-window g-object
         "gtk_tree_view_get_bin_window" nil)
        (:cffi gtk::search-equal-func gtk::tree-view-search-equal-func nil
         nil gtk::tree-view-set-search-equal-func)
        (:cffi gtk::search-entry gtk::tree-view-search-entry g-object
         "gtk_tree_view_get_search_entry" "gtk_tree_view_set_search_entry")
        (:cffi gtk::search-position-func gtk::tree-view-search-position-func nil
         nil gtk::tree-view-set-search-position-func)
        (:cffi gtk::row-separator-func gtk::tree-view-row-separator-func nil
         nil gtk::tree-view-set-row-separartor-func))
       ("GtkCellView"
        (:cffi gtk::displayed-row gtk::cell-view-displayed-row (g-boxed-foreign gtk::tree-path)
         "gtk_cell_view_get_displayed_row" "gtk_cell_view_set_displayed_row")
        (:cffi gtk::cell-renderers gtk::cell-view-cell-renderers (glist (g-object gtk::cell-renderer) :free-from-foreign t)
         "gtk_cell_view_get_cell_renderers" nil))
       ("GtkComboBox"
        (:cffi gtk::active-iter gtk::combo-box-active-iter (g-boxed-foreign gtk::tree-iter)
         gtk::combo-box-get-active-iter "gtk_combo_box_set_active_iter")
        (:cffi gtk::row-separator-func gtk::combo-box-separator-func nil
         nil gtk::combo-box-set-separator-func)
        (:cffi gtk::title gtk::combo-box-title (:string :free-from-foreign nil :free-to-foreign t)
         "gtk_combo_box_get_title" "gtk_combo_box_set_title"))
       ("GtkMenu"
        (:cffi gtk::screen gtk::menu-screen g-object
         nil "gtk_menu_set_screen")
        (:cffi gtk::title gtk::menu-title (:string :free-from-foreign nil :free-to-foreign t)
         "gtk_menu_get_title" "gtk_menu_set_title"))
       ("GtkToolItem"
        (:cffi gtk::expand gtk::tool-item-expand :boolean
         "gtk_tool_item_get_expand" "gtk_tool_item_set_expand")
        (:cffi gtk::use-drag-window gtk::tool-item-use-drag-window :boolean
         "gtk_tool_item_get_use_drag_window" "gtk_tool_item_set_use_drag_window")
        (:cffi gtk::icon-size gtk::tool-item-icon-size gtk::icon-size
         "gtk_tool_item_get_icon_size" nil)
        (:cffi gtk::orientation gtk::tool-item-orientation gtk::orientation
         "gtk_tool_item_get_orientation" nil)
        (:cffi gtk::toolbar-style gtk::tool-item-toolbar-style gtk::toolbar-style
         "gtk_tool_item_get_toolbar_style" nil)
        (:cffi gtk::relief-style gtk::tool-item-relief-style gtk::relief-style
         "gtk_tool_item_get_relief_style" nil))
       ("GtkMenuToolButton"
        (:cffi gtk::arrow-tooltip-text gtk::menu-tool-button-arrow-tooltip-text :string
         nil "gtk_menu_tool_button_set_arrow_tooltip_text")
        (:cffi gtk::arrow-tooltip-markup gtk::menu-tool-button-arrow-tooltip-markup :string
         nil "gtk_menu_tool_button_set_arrow_tooltip_markup"))
       ("GtkUIManager"
        (:cffi gtk::accel-group gtk::ui-manager-accel-group g-object
         "gtk_ui_manager_get_accel_group" nil))
       ("GtkActionGroup"
        (:cffi gtk::translate-function gtk::action-group-translate-function nil
         nil gtk::action-group-set-translate-func)
        (:cffi gtk::translation-domain gtk::action-group-translation-domain nil
         nil gtk::gtk-action-group-set-translation-domain))
       ("GtkAction"
        (:cffi gtk::accel-path gtk::action-accel-path (:string :free-from-foreign nil :free-to-foreign t)
         "gtk_action_get_accel_path" "gtk_action_set_accel_path")
        (:cffi gtk::accel-group gtk::action-accel-group g-object
         nil "gtk_action_set_accel_group"))
       ("GtkFileChooser"
        (:cffi gtk::current-name gtk::file-chooser-current-name (:string :free-to-foreign t :encoding :utf-8)
         nil "gtk_file_chooser_set_current_name")
        (:cffi gtk::filename gtk::file-chooser-filename (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_filename" "gtk_file_chooser_set_filename")
        (:cffi gtk::current-folder gtk::file-chooser-current-folder (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder" "gtk_file_chooser_set_current_folder")
        (:cffi gtk::uri gtk::file-chooser-uri (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_uri" "gtk_file_chooser_set_uri")
        (:cffi gtk::current-folder-uri gtk::file-chooser-current-folder-uri (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_current_folder_uri" "gtk_file_chooser_set_current_folder_uri")
        (:cffi gtk::preview-filename gtk::file-chooser-preview-filename (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_filename" nil)
        (:cffi gtk::preview-uri gtk::file-chooser-preview-uri (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_file_chooser_get_preview_uri" nil))
       ("GtkFileFilter"
        (:cffi gtk::name gtk::file-filter-name :string
         "gtk_file_filter_get_name" "gtk_file_filter_set_name"))
       ("GtkFontSelectionDialog"
        (:cffi gtk::font-name gtk::font-selection-dialog-font-name (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_font_selection_dialog_get_font_name" "gtk_font_selection_dialog_set_font_name")
        (:cffi gtk::preview-text gtk::font-selection-dialog-preview-text :string
         "gtk_font_selection_dialog_get_preview_text" "gtk_font_selection_dialog_set_preview_text")
        (:cffi gtk::apply-button gtk::font-selection-dialog-apply-button g-object
         "gtk_font_selection_dialog_get_apply_button" nil)
        (:cffi gtk::cancel-button gtk::font-selection-dialog-cancel-button g-object
         "gtk_font_selection_dialog_get_cancel_button" nil)
        (:cffi gtk::ok-button gtk::font-selection-dialog-ok-button g-object
         "gtk_font_selection_dialog_get_ok_button" nil))
       ("GtkFixed"
        (:cffi gtk::has-window gtk::fixed-has-window :boolean
         "gtk_fixed_get_has_window" "gtk_fixed_set_has_window"))
       ("GtkLayout"
        (:cffi gtk::bin-window gtk::layout-bin-window g-object
         "gtk_layout_get_bin_window" nil))
       ("GtkCalendar"
        (:cffi gtk::detail-function gtk::calendar-detail-function nil
         nil gtk::calendar-set-detail-function)
        (:cffi gtk::display-options gtk::calendar-display-options gtk::calendar-display-options
         "gtk_calendar_get_display_options" "gtk_calendar_set_display_options"))
       ("GtkContainer"
        (:cffi gtk::focus-child gtk::container-focus-child g-object
         "gtk_container_get_focus_child" "gtk_container_set_focus_child")
        (:cffi gtk::focus-vadjustment gtk::container-focus-vadjustment g-object
         "gtk_container_get_focus_vadjustment" "gtk_container_set_focus_vadjustment")
        (:cffi gtk::focus-hadjustment gtk::container-focus-hadjustment g-object
         "gtk_container_get_focus_hadjustment" "gtk_container_set_focus_hadjustment")
        (:cffi gtk::reallocate-redraws gtk::container-reallocate-redraws :boolean
         nil "gtk_container_set_reallocate_redraws"))
       ("GtkWidget"
	(:cffi gtk::parent-window gtk::widget-parent-window (g-object gdk::gdk-window)
	 "gtk_widget_get_parent_window" "gtk_widget_set_parent_window")
	(:cffi gtk::toplevel gtk::widget-toplevel (g-object gtk::widget)
	 "gtk_widget_get_toplevel" nil)
	(:cffi gtk::colormap gtk::widget-colormap (g-object gdk::gdk-colormap)
	 "gtk_widget_get_colormap" "gtk_widget_set_colormap")
	(:cffi gtk::visual gtk::widget-visual (g-object gdk::visual)
	 "gtk_widget_get_visual" nil)
	(:cffi gtk::modifier-style gtk::widget-modifier-style (g-object gtk::rc-style)
	 "gtk_widget_get_modifier_style" "gtk_widget_modify_style")
	(:cffi gtk::pango-context gtk::widget-pango-context g-object
	 "gtk_widget_get_pango_context" nil)
	(:cffi gtk::child-visible gtk::widget-child-visible :boolean
	 "gtk_widget_get_child_visible" "gtk_widget_set_child_visible")
        (:cffi gtk::direction gtk::widget-direction gtk::text-direction
         "gtk_widget_get_direction" "gtk_widget_set_direction")
        (:cffi gtk::composite-name gtk::widget-composite-name (glib:g-string :free-from-foreign t :free-to-foreign t)
         "gtk_widget_get_composite_name" "gtk_widget_set_composite_name")
        (:cffi gtk::redraw-on-allocate gtk::widget-redraw-on-allocate :boolean
         nil "gtk_widget_set_redraw_on_allocate")
        (:cffi gtk::accessible gtk::widget-accessible g-object
         "gtk_widget_get_accessible" nil)
        (:cffi gtk::tooltip-window gtk::widget-tooltip-window g-object
	 "gtk_widget_get_tooltip_window" "gtk_widget_set_tooltip_window")
        (:cffi gtk::style gtk::widget-style (g-object gtk::style)
         "gtk_widget_get_style" "gtk_widget_set_style"))
       ("GtkWindowGroup"
        (:cffi gtk::windows gtk::window-group-windows (glist (g-object gtk::gtk-window))
         "gtk_window_group_list_windows" nil))
       ("GtkTextTag"
        (:cffi gtk::priority gtk::text-tag-priority :int
         "gtk_text_tag_get_priority" "gtk_text_tag_set_priority"))
       ("GtkDialog"
        (:cffi gtk::content-area gtk::dialog-content-area (g-object gtk::v-box)
         "gtk_dialog_get_content_area" nil)
        (:cffi gtk::action-area gtk::dialog-action-area (g-object gtk::widget)
         "gtk_dialog_get_action_area" nil)
        (:cffi gtk::default-response gtk::dialog-default-response gtk::response-type
         nil "gtk_dialog_set_default_response"))
       ("GtkAssistant"
        (:cffi gtk::current-page gtk::assistant-current-page :int
         "gtk_assistant_get_current_page" "gtk_assistant_set_current_page")
        (:cffi gtk::n-pages gtk::assistant-n-pages :int
         "gtk_assistant_get_n_pages" nil)
        (:cffi gtk::forward-page-function gtk::assistant-forward-page-function nil
         nil gtk::set-assistant-forward-page-function))
       ("GtkLabel"
        (:cffi gtk::line-wrap gtk::label-line-wrap :boolean
         "gtk_label_get_line_wrap" "gtk_label_set_line_wrap")
        (:cffi gtk::line-wrap-mode gtk::label-line-wrap-mode gtk::pango-wrap-mode
         "gtk_label_get_line_wrap_mode" "gtk_label_set_line_wrap_mode")
        (:cffi gtk::layout gtk::label-layout g-object
         "gtk_label_get_layout" nil)
        (:cffi gtk::selection-bounds gtk::label-selection-bounds nil
         gtk::gtk-label-get-selection-bounds nil)
        (:cffi gtk::layout-offsets gtk::label-layout-offsets nil
         gtk::gtk-label-get-layout-offsets nil))
       ("GtkEntry"
        (:cffi gtk::layout gtk::entry-layout g-object
         "gtk_entry_get_layout" nil)
        (:cffi gtk::completion gtk::entry-completion (g-object gtk::entry-completion)
         "gtk_entry_get_completion" "gtk_entry_set_completion")
        (:cffi gtk::cursor-hadjustment gtk::entry-cursor-hadjustment (g-object gtk::adjustment)
         "gtk_entry_get_cursor_hadjustment" "gtk_entry_set_cursor_hadjustment")
        (:cffi gtk::layout-offset gtk::entry-layout-offset nil
         gtk::gtk-entry-layout-offset nil))
       ("GtkPageSetupUnixDialog"
	(:cffi gtk::page-setup gtk::page-setup-unix-dialog-page-setup (g-object gtk::page-setup)
	 "gtk_page_setup_unix_dialog_get_page_setup" "gtk_page_setup_unix_dialog_set_page_setup")
	(:cffi gtk::print-settings gtk::page-setup-unix-dialog-print-settings (g-object gtk::print-settings)
	 "gtk_page_setup_unix_dialog_get_print_settings" "gtk_page_setup_unix_dialog_set_print_settings"))
       ("GtkEntryCompletion"
        (:cffi gtk::entry gtk::entry-completion-entry (g-object gtk::entry)
         "gtk_entry_completion_get_entry" nil)
        (:cffi gtk::match-function gtk::entry-completion-match-function nil
         nil gtk::gtk-entry-completion-set-match-function))
       ("GtkEditable"
        (:cffi gtk::position gtk::editable-position :int
         "gtk_editable_get_position" "gtk_editable_set_position")
        (:cffi gtk::editable gtk::editable-editable :boolean
         "gtk_editable_get_editable" "gtk_editable_set_editable"))
       ("GtkTextMark"
        (:cffi gtk::visible gtk::text-mark-visible :boolean
         "gtk_text_mark_get_visible" "gtk_text_mark_set_visible")
        (:cffi gtk::deleted gtk::text-mark-deleted :boolean
         "gtk_text_mark_get_deleted" nil)
        (:cffi gtk::buffer gtk::text-mark-buffer (g-object gtk::text-buffer)
         "gtk_text_mark_get_buffer" nil))
       ("GtkTextBuffer"
        (:cffi gtk::modified gtk::text-buffer-modified :boolean
         "gtk_text_buffer_get_modified" "gtk_text_buffer_set_modified"))
       ("GtkToolShell"
        (:cffi gtk::icon-size gtk::tool-shell-icon-size gtk::icon-size
         "gtk_tool_shell_get_icon_size" nil)
        (:cffi gtk::orientation gtk::tool-shell-orientation gtk::orientation
         "gtk_tool_shell_get_orientation" nil)
        (:cffi gtk::relief-style gtk::tool-shell-relief-style gtk::relief-style
         "gtk_tool_shell_get_relief_style" nil)
        (:cffi gtk::style gtk::tool-shell-style gtk::toolbar-style
         "gtk_tool_shell_get_style" nil))
       ("GtkColorSelection"
        (:cffi gtk::previous-alpha gtk::color-selection-previous-alpha :uint16
         "gtk_color_selection_get_previous_alpha" "gtk_color_selection_set_previous_alpha")
        (:cffi gtk::previous-color gtk::color-selection-previous-color (g-boxed-foreign gdk::color)
         gtk::gtk-color-selection-get-previous-color gtk::gtk-color-selection-set-previous-color))
       ("GtkScrolledWindow"
        (:cffi gtk::hscrollbar gtk::scrolled-window-hscrollbar (g-object gtk::widget)
         "gtk_scrolled_window_get_hscrollbar" nil)
        (:cffi gtk::vscrollbar gtk::scrolled-window-vscrollbar (g-object gtk::widget)
         "gtk_scrolled_window_get_vscrollbar" nil))
       ("GtkBin"
        (:cffi gtk::child gtk::bin-child (g-object gtk::widget)
         "gtk_bin_get_child" nil))
       ("GtkTextChildAnchor"
        (:cffi gtk::deleted-p gtk::text-child-anchor-deleted-p :boolean
         "gtk_text_child_anchor_get_deleted" nil))))))

(defun gtk-generate-child-properties (filename)
  (with-open-file (stream filename :direction :output :if-exists :supersede)
    (let ((*package* (find-package :gtk))
          (*print-case* :downcase))
     (write-string "(in-package :gtk)" stream)
     (terpri stream)
     (format stream "~{~S~%~%~}" (gtk:generate-child-properties)))))