(in-package :gtk)

;; GtkEntry

(defcfun (entry-layout "gtk_entry_get_layout") g-object ;;PangoLayout
  (entry (g-object entry)))

(export 'entry-layout)

(defcfun gtk-entry-get-layout-offsets :void
  (entry (g-object entry))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun gtk-entry-layout-offset (entry)
  (with-foreign-objects ((x :int) (y :int))
    (gtk-entry-get-layout-offsets entry x y)
    (list (mem-ref x :int) (mem-ref y :int))))

(defcfun (entry-layout-index-to-text-index "gtk_entry_layout_index_to_text_index") :int
  (entry (g-object entry))
  (layout-index :int))

(export 'entry-layout-index-to-text-index)

(defcfun (entry-text-index-to-layout-index "gtk_entry_text_index_to_layout_index") :int
  (entry (g-object entry))
  (text-index :int))

(export 'entry-text-index-to-layout-info)

(defcfun (entry-icon-at-pos "gtk_entry_get_icon_at_pos") :int
  (entry (g-object entry))
  (x :int)
  (y :int))

(export 'entry-icon-at-pos)

;; GtkEditable

(defcfun (editable-select-region "gtk_editable_select_region") :void
  (editable (g-object editable))
  (start :int)
  (end :int))

(export 'editable-select-region)

(defcfun gtk-editable-get-selection-bounds :boolean
  (editable (g-object editable))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun editable-selection (editable)
  (with-foreign-objects ((start :int) (end :int))
    (let ((selected-p (gtk-editable-get-selection-bounds editable start end)))
      (values selected-p (mem-ref start :int) (mem-ref end :int)))))

(export 'editable-selection)

(defcfun gtk-editable-insert-text :void
  (editable (g-object editable))
  (new-text :string)
  (new-text-length :int)
  (position (:pointer :int)))

(defun editable-insert-text (editable text position)
  (with-foreign-object (pos :int)
    (setf (mem-ref pos :int) position)
    (gtk-editable-insert-text editable text (length text) pos)
    (mem-ref pos :int)))

(export 'editable-insert-text)

(defcfun gtk-editable-delete-text :void
  (editable (g-object editable))
  (start-pos :int)
  (end-pos :int))

(defun editable-delete-text (editable &key start-pos end-pos)
  (gtk-editable-delete-text editable (or start-pos -1) (or end-pos -1)))

(export 'editable-delete-text)

(defcfun gtk-editable-get-chars g-string
  (editable (g-object editable))
  (start-pos :int)
  (end-pos :int))

(defun editable-get-chars (editable &key (start -1) (end -1))
  (gtk-editable-get-chars editable start end))

(export 'editable-get-chars)

(defcfun (editable-cut-clipboard "gtk_editable_cut_clipboard") :void
  (editable (g-object editable)))

(export 'editable-cut-clipboard)

(defcfun (editable-copy-clipboard "gtk_editable_copy_clipboard") :void
  (editable (g-object editable)))

(export 'editable-copy-clipboard)

(defcfun (editable-paste-clipboard "gtk_editable_paste_clipboard") :void
  (editable (g-object editable)))

(export 'editable-paste-clipboard)

(defcfun (editable-delete-selection "gtk_editable_delete_selection") :void
  (editable (g-object editable)))

(export 'editable-delete-selection)

(defcfun (editable-position "gtk_editable_get_position") :int
  (editable (g-object editable)))

(defcfun gtk-editable-set-position :void
  (editable (g-object editable))
  (pos :int))

(defun (setf editable-position) (position editable)
  (gtk-editable-set-position editable position))

(export 'editable-position)

(defcfun (editable-editable "gtk_editable_get_editable") :boolean
  (editable (g-object editable)))

(defcfun gtk-editable-set-editable :void
  (editable (g-object editable))
  (is-editable :boolean))

(defun (setf editable-editable) (is-editable editable)
  (gtk-editable-set-editable editable is-editable))

(export 'editable-editable)