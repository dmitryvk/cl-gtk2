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
     :flags '("GtkTextSearchFlags")
     :enums '("GtkTextBufferTargetInfo")
     :exclusions '("PangoStretch" "PangoVariant" "PangoStyle" "PangoUnderline")
     :additional-properties
     '(("GtkTreeViewColumn"
        (:cffi gtk::tree-view gtk::tree-view-column-tree-view g-object "gtk_tree_view_column_get_tree_view" nil)
        (:cffi gtk::sort-column-id gtk::tree-view-column-sort-column-id :int "gtk_tree_view_column_get_sort_column_id" "gtk_tree_view_column_set_sort_column_id")
        (:cffi gtk::cell-renderers gtk::tree-view-column-cell-renderers (glist g-object  :free-from-foreign t) "gtk_tree_view_column_get_cell_renderers" nil))))))