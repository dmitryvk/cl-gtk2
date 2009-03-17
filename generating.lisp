(defpackage :gtk-generation
  (:use :cl :gobject :cffi :glib)
  (:export #:gtk-generate))

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
    (gobject::generate-types-hierarchy-to-file
     stream
     "GtkObject"
     :include-referenced t
     :prefix "Gtk"
     :package (or (find-package :gtk) (make-package :gtk))
     :exceptions `(("GObject" gobject:g-object)
                   ("GtkObject" ,(intern "GTK-OBJECT" (find-package :gtk)))
                   ("GInitiallyUnowned" gobject::g-initially-unowned)
                   ("GtkWindow" ,(intern "GTK-WINDOW" (find-package :gtk))))
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
                   "AtkImplementorIface")
     :objects '("GtkSettings" "GtkRcStyle" "GtkStyle" "GtkTooltip" "GtkAccelGroup"
                "GtkAccelMap" "GtkAction" "GtkActionGroup" "GtkBuilder" "GtkClipboard"
                "GtkEntryCompletion" "GtkIconFactory" "GtkIconTheme" "GtkIMContext"
                "GtkListStore" "GtkPageSetup" "GtkPrintContext" "GtkPrintOperation"
                "GtkPrintSettings" "GtkRecentManager" "GtkSizeGroup" "GtkStatusIcon"
                "GtkTextBuffer" "GtkTextChildAnchor" "GtkTextMark" "GtkTextTag"
                "GtkTextTagTable" "GtkTreeModelFilter" "GtkTreeModelSort"
                "GtkTreeSelection" "GtkTreeStore" "GtkUIManager" "GtkWindowGroup")
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
              "GtkTreeViewMode" "GtkVisibility")
     :exclusions '("PangoStretch" "PangoVariant" "PangoStyle" "PangoUnderline")
     :additional-properties
     '(("GtkTreeViewColumn"
        (:cffi gtk::tree-view gtk::tree-view-column-tree-view g-object "gtk_tree_view_column_get_tree_view" nil)
        (:cffi gtk::sort-column-id gtk::tree-view-column-sort-column-id :int "gtk_tree_view_column_get_sort_column_id" "gtk_tree_view_column_set_sort_column_id")
        (:cffi gtk::cell-renderers gtk::tree-view-column-cell-renderers (glist g-object  :free-from-foreign t) "gtk_tree_view_column_get_cell_renderers" nil))
       ("GtkTreeSelection"
        (:cffi gtk::mode gtk::tree-selection-mode gtk::selection-mode "gtk_tree_selection_get_mode" "gtk_tree_selection_set_mode")
        (:cffi gtk::select-function gtk::tree-selection-select-function nil gtk::tree-selection-get-selection-function gtk::tree-selection-set-select-function))
       ("GtkTreeView"
        (:cffi gtk::selection gtk::tree-view-selection g-object "gtk_tree_view_get_selection" nil)
        (:cffi gtk::column-drag-function gtk::tree-view-column-drag-function nil nil gtk::tree-view-set-column-drag-function)
        (:cffi gtk::bin-window gtk::tree-view-bin-window g-object "gtk_tree_view_get_bin_window" nil)
        (:cffi gtk::search-equal-func gtk::tree-view-search-equal-func nil nil gtk::tree-view-set-search-equal-func)
        (:cffi gtk::search-entry gtk::tree-view-search-entry g-object "gtk_tree_view_get_search_entry" "gtk_tree_view_set_search_entry")
        (:cffi gtk::search-position-func gtk::tree-view-search-position-func nil nil gtk::tree-view-set-search-position-func)
        (:cffi gtk::row-separator-func gtk::tree-view-row-separator-func nil nil gtk::tree-view-set-row-separartor-func))
       ("GtkCellView"
        (:cffi gtk::displayed-row gtk::cell-view-displayed-row (g-boxed-ref gtk::tree-path) "gtk_cell_view_get_displayed_row" "gtk_cell_view_set_displayed_row"))))))