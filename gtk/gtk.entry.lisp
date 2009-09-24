(in-package :gtk)

;; GtkEntry

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

(defcfun (entry-progress-pulse "gtk_entry_progress_pulse") :void
  (entry (g-object entry)))

(export 'entry-progress-pulse)

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

(defun editable-get-chars (editable &key (start 0) (end -1))
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

;; GtkEntryCompletion

(define-cb-methods entry-completion-match-func :boolean
  ((completion (g-object entry-completion))
   (key :string)
   (iter (g-boxed-foreign tree-iter))))

(defcfun (%gtk-entry-completion-set-match-func "gtk_entry_completion_set_match_func") :void
  (completion (g-object entry-completion))
  (func :pointer)
  (data :pointer)
  (destroy-notify :pointer))

(defun gtk-entry-completion-set-match-func (completion function)
  (if function
      (%gtk-entry-completion-set-match-func completion
                                            (callback entry-completion-match-func-cb)
                                            (create-fn-ref completion function)
                                            (callback entry-completion-match-func-destroy-notify))
      (%gtk-entry-completion-set-match-func completion
                                            (null-pointer)
                                            (null-pointer)
                                            (null-pointer))))

(defcfun (entry-completion-complete "gtk_entry_completion_complete") :void
  (completion (g-object entry-completion)))

(export 'entry-completion-complete)

(defcfun (entry-completion-completion-prefix "gtk_entry_completion_get_completion_prefix") (:string :free-from-foreign t)
  (completion (g-object entry-completion)))

(export 'entry-completion-completion-prefix)

(defcfun (entry-completion-insert-prefix "gtk_entry_completion_insert_prefix") :void
  (completion (g-object entry-completion)))

(export 'entry-completion-completion-prefix)

(defcfun (entry-completion-insert-action-text "gtk_entry_completion_insert_action_text") :void
  (completion (g-object entry-completion))
  (index :int)
  (text :string))

(export 'entry-completion-insert-action-text)

(defcfun (entry-copmletion-insert-action-markup "gtk_entry_completion_insert_action_markup") :void
  (completion (g-object entry-completion))
  (index :int)
  (markup :string))

(export 'entry-completion-insert-action-markup)

(defcfun (entry-completion-delete-action "gtk_entry_completion_delete_action") :void
  (completion (g-object entry-completion))
  (index :int))

(export 'entry-completion-delete-action)
