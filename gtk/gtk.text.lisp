(in-package :gtk)

;; text iter


(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod translate-from-foreign (value (type unichar))
  (code-char value))

(defmethod translate-to-foreign (value (type unichar))
  (char-code value))

(define-g-boxed-ref "GtkTextIter" text-iter
  (:free-function gtk-text-iter-free)
  (:alloc-function gtk-text-iter-alloc)
  (:slots (text-iter-buffer :reader "gtk_text_iter_get_buffer" :type (g-object text-buffer))
          (text-iter-offset :reader "gtk_text_iter_get_offset" :writer "gtk_text_iter_set_offset" :type :int)
          (text-iter-line :reader "gtk_text_iter_get_line" :writer "gtk_text_iter_set_line" :type :int)
          (text-iter-line-offset :reader "gtk_text_iter_get_line_offset" :writer "gtk_text_iter_set_line_offset" :type :int)
          (text-iter-visible-line-offset :reader "gtk_text_iter_get_visible_line_offset" :writer "gtk_text_iter_set_visible_line_offset" :type :int)
          (text-iter-char :reader "gtk_text_iter_get_char" :type unichar)
          (text-iter-pixbuf :reader "gtk_text_iter_get_pixbuf" :type (g-object pixbuf))
          (text-iter-marks :reader "gtk_text_iter_get_marks" :type (gslist (g-object text-mark) :free-from-foreign t))
          (text-iter-child-anchor :reader "gtk_text_iter_get_child_anchor" :type (g-object text-child-anchor))
          (text-iter-tags :reader "gtk_text_iter_get_tags" :type (gslist (g-object text-mark) :free-from-foreign t))
          (text-iter-chars-in-line :reader "gtk_text_iter_get_chars_in_line" :type :int)
          (text-iter-language :reader "gtk_text_iter_get_language" :type :pointer)
          (text-iter-is-end :reader "gtk_text_iter_is_end" :type :boolean)
          (text-iter-is-start :reader "gtk_text_iter_is_start" :type :boolean)
          (text-iter-can-insert :reader "gtk_text_iter_can_insert" :type :boolean)
          (text-iter-starts-word :reader "gtk_text_iter_starts_word" :type :boolean)
          (text-iter-ends-word :reader "gtk_text_iter_ends_word" :type :boolean)
          (text-iter-inside-word :reader "gtk_text_iter_inside_word" :type :boolean)
          (text-iter-starts-line :reader "gtk_text_iter_starts_line" :type :boolean)
          (text-iter-ends-line :reader "gtk_text_iter_ends_line" :type :boolean)
          (text-iter-starts-sentence :reader "gtk_text_iter_starts_sentence" :type :boolean)
          (text-iter-ends-sentence :reader "gtk_text_iter_ends_sentence" :type :boolean)
          (text-iter-inside-sentence :reader "gtk_text_iter_inside_sentence" :type :boolean)
          (text-iter-is-cursor-position :reader "gtk_text_iter_is_cursor_position" :type :boolean)
          ))
(export '(text-iter text-iter-offset text-iter-line text-iter-line-offset text-iter-visible-line-offset text-iter-char text-iter-pixbuf text-iter-marks text-iter-toggled-tags text-iter-child-anchor text-iter-tags text-iter-chars-in-line text-iter-language))

(defcstruct %text-iter
  (dummy1 :pointer)
  (dummy2 :pointer)
  (dummy3 :int)
  (dummy4 :int)
  (dummy5 :int)
  (dummy6 :int)
  (dummy7 :int)
  (dummy8 :int)
  (dummy9 :pointer)
  (dummy10 :pointer)
  (dummy11 :int)
  (dummy12 :int)
  (dummy13 :int)
  (dummy14 :pointer))

(defcfun gtk-text-iter-copy :pointer
  (iter :pointer))

(defcfun gtk-text-iter-free :void
  (iter :pointer))

(defun gtk-text-iter-alloc ()
  (with-foreign-object (iter '%text-iter)
    (gtk-text-iter-copy iter)))

(defcfun (text-iter-slice "gtk_text_iter_get_slice") (:string :free-from-foreign t)
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(export 'text-iter-slice)

(defcfun (text-iter-text "gtk_text_iter_get_text") (:string :free-from-foreign t)
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(export 'text-iter-text)

(defcfun (text-iter-visible-slice "gtk_text_iter_get_visible_slice") (:string :free-from-foreign t)
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(export 'text-iter-visible-slice)

(defcfun (text-iter-visible-text "gtk_text_iter_get_visible_text") (:string :free-from-foreign t)
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(export 'text-iter-visible-text)

(defcfun (text-iter-toggled-tags "gtk_text_iter_get_toggled_tags") (gslist (g-object text-tag))
  (iter (g-boxed-ref text-iter))
  (toggled-on :boolean))

(export 'text-iter-toggled-tags)

(defcfun (text-iter-begins-tag "gtk_text_iter_begins_tag") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-begins-tag)

(defcfun (text-iter-ends-tag "gtk_text_iter_ends_tag") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-ends-tag)

(defcfun (text-iter-toggles-tag "gtk_text_iter_toggles_tag") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-toggles-tag)

(defcfun (text-iter-has-tag "gtk_text_iter_has_tag") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-has-tag)

(defcfun (text-iter-editable "gtk_text_iter_editable") :boolean
  (iter (g-boxed-ref text-iter))
  (default :boolean))

(export 'text-iter-editable)

(defcfun gtk-text-iter-get-attributes :boolean
  (iter (g-boxed-ref text-iter))
  (values (g-object text-attributes)))

(defun text-iter-attributes (iter default-attributes)
  (let ((changed-p (gtk-text-iter-get-attributes iter default-attributes)))
    (values default-attributes changed-p)))

(export 'text-iter-attributes)

(defcfun gtk-text-iter-forward-chars :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-lines :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-word-ends :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-word-starts :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-cursor-positions :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-sentence-ends :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-sentence-starts :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-word-ends :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-visible-word-starts :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-cursor-positions :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-lines :boolean
  (iter (g-boxed-ref text-iter))
  (count :int))

(defun text-iter-move (iter &key (count 1) (by :char) (direction :forward))
  (assert (typep by '(member :char :line :word :cursor-position :sentence :visible-word :visible-line :visible-cursor-position)))
  (assert (typep direction '(member :forward :backward)))
  (when (and (member by '(:char :ine :cursor-position :visible-line :visible-cursor-position)) (eq direction :backward))
    (setf count (- count)))
  (ecase by
    (:char (gtk-text-iter-forward-chars iter count))
    (:line (gtk-text-iter-forward-lines iter count))
    (:word (if (eq direction :forward)
               (gtk-text-iter-forward-word-ends iter count)
               (gtk-text-iter-backward-word-starts iter count)))
    (:cursor-position (gtk-text-iter-forward-cursor-positions iter count))
    (:sentence (if (eq direction :forward)
                   (gtk-text-iter-forward-sentence-ends iter count)
                   (gtk-text-iter-backward-sentence-starts iter count)))
    (:visible-word (if (eq direction :forward)
                       (gtk-text-iter-forward-visible-word-ends iter count)
                       (gtk-text-iter-backward-visible-word-starts iter count)))
    (:visible-line (gtk-text-iter-forward-visible-lines iter count))
    (:visible-cursor-position (gtk-text-iter-forward-visible-cursor-positions iter count))))

(export 'text-iter-move)

(defcfun (text-iter-forward-to-end "gtk_text_iter_forward_to_end") :void
  (iter (g-boxed-ref text-iter)))

(export 'text-iter-forward-to-end)

(defcfun (text-iter-forward-to-line-end "gtk_text_iter_forward_to_line_end") :boolean
  (iter (g-boxed-ref text-iter)))

(export 'text-iter-forward-to-line-end)

(defcfun (text-iter-forward-to-tag-toggle "gtk_text_iter_forward_to_tag_toggle") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(defcfun (text-iter-backward-to-tag-toggle "gtk_text_iter_backward_to_tag_toggle") :boolean
  (iter (g-boxed-ref text-iter))
  (tag (g-object text-tag)))

(export '(text-iter-forward-to-tag-toggle text-iter-backward-to-tag-toggle))

(defcallback gtk-text-char-predicate :boolean ((char unichar) (user-data :pointer))
  (let ((function (get-stable-pointer-value user-data)))
    (funcall function char)))

(defcfun gtk-text-iter-forward-find-char :boolean
  (iter (g-boxed-ref text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-ref text-iter)))

(defcfun gtk-text-iter-backward-find-char :boolean
  (iter (g-boxed-ref text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-ref text-iter)))

(defun text-iter-find-char (iter predicate &key limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (with-stable-pointer (ptr predicate)
    (if (eq direction :forward)
        (gtk-text-iter-forward-find-char iter (callback gtk-text-char-predicate) ptr limit)
        (gtk-text-iter-backward-find-char iter (callback gtk-text-char-predicate) ptr limit))))

(export 'text-iter-find-char)

(defcfun gtk-text-iter-forward-search :boolean
  (iter (g-boxed-ref text-iter))
  (str (:string :free-to-foreign t))
  (flags text-search-flags)
  (match-start (g-boxed-ref text-iter))
  (match-end (g-boxed-ref text-iter))
  (limit (g-boxed-ref text-iter)))

(defcfun gtk-text-iter-backward-search :boolean
  (iter (g-boxed-ref text-iter))
  (str (:string :free-to-foreign t))
  (flags text-search-flags)
  (match-start (g-boxed-ref text-iter))
  (match-end (g-boxed-ref text-iter))
  (limit (g-boxed-ref text-iter)))

(defun text-iter-search (start-position string &key flags limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (let ((i1 (make-instance 'text-iter))
        (i2 (make-instance 'text-iter)))
    (if (if (eq direction :forward)
            (gtk-text-iter-forward-search start-position string flags i1 i2 limit)
            (gtk-text-iter-backward-search start-position string flags i1 i2 limit))
        (values t i1 i2)
        (progn (release i1)
               (release i2)
               (values nil nil nil)))))

(export 'text-iter-search)

(defcfun (text-iter-equal "gtk_text_iter_equal") :boolean
  (iter-1 (g-boxed-ref text-iter))
  (iter-2 (g-boxed-ref text-iter)))

(export 'text-iter-equal)

(defcfun (text-iter-compare "gtk_text_iter_compare") :int
  (iter-1 (g-boxed-ref text-iter))
  (iter-2 (g-boxed-ref text-iter)))

(export 'text-iter-compare)

(defcfun (text-iter-in-range "gtk_text_iter_in_range") :boolean
  (iter (g-boxed-ref text-iter))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(export 'text-iter-in-range)

(defcfun (text-iter-order "gtk_text_iter_order") :void
  (iter-1 (g-boxed-ref text-iter))
  (iter-2 (g-boxed-ref text-iter)))

(export 'text-iter-order)

;; text mark

(defcfun (text-mark-visible "gtk_text_mark_get_visible") :boolean
  (mark (g-object text-mark)))

(defcfun gtk-text-mark-set-visible :void
  (mark (g-object text-mark))
  (visible :boolean))

(defun (setf text-mark-visible) (new-value mark)
  (gtk-text-mark-set-visible mark new-value))

(export 'text-mark-visible)

(defcfun (text-mark-deleted "gtk_text_mark_get_deleted") :boolean
  (mark (g-object text-mark)))

(export 'text-mark-deleted)

(defcfun (text-mark-buffer "gtk_text_mark_get_buffer") (g-object text-buffer)
  (mark (g-object text-mark)))

(export 'text-mark-buffer)

;; text buffer

(defcfun (text-buffer-line-count "gtk_text_buffer_get_line_count") :int
  (buffer (g-object text-buffer)))

(export 'text-buffer-line-count)

(defcfun (text-buffer-char-count "gtk_text_buffer_get_char_count") :int
  (buffer (g-object text-buffer)))

(export 'text-buffer)

(defcfun (text-buffer-tag-table "gtk_text_buffer_get_tag_table") (g-object text-tag-table)
  (buffer (g-object text-buffer)))

(export 'text-buffer-tag-table)

(defcfun gtk-text-buffer-insert :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (text (:string :free-to-foreign t))
  (len :int))

(defcfun gtk-text-buffer-insert-at-cursor :void
  (buffer (g-object text-buffer))
  (text (:string :free-to-foreign t))
  (len :int))

(defcfun gtk-text-buffer-insert-interactive :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (text (:string :free-to-foreign t))
  (len :int)
  (default-editable :boolean))

(defcfun gtk-text-buffer-insert-interactive-at-cursor :boolean
  (buffer (g-object text-buffer))
  (text (:string :free-to-foreign t))
  (len :int)
  (default-editable :boolean))

(defun text-buffer-insert (buffer text &key (position :cursor) (interactive nil) (default-editable t))
  (assert (typep position '(or text-iter (member :cursor))))
  (if interactive
      (if (eq position :cursor)
          (gtk-text-buffer-insert-interactive-at-cursor buffer text (length text) default-editable)
          (gtk-text-buffer-insert-interactive buffer position text (length text) default-editable))
      (progn (if (eq position :cursor)
                 (gtk-text-buffer-insert-at-cursor buffer text (length text))
                 (gtk-text-buffer-insert buffer position text (length text)))
             t)))

(export 'text-buffer-insert)

(defcfun gtk-text-buffer-insert-range :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (range-start (g-boxed-ref text-iter))
  (range-end (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-insert-range-interactive :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (range-start (g-boxed-ref text-iter))
  (range-end (g-boxed-ref text-iter))
  (default-editable :boolean))

(defun text-buffer-insert-range (buffer position range-start range-end &key interactive default-editable)
  (if interactive
      (gtk-text-buffer-insert-range-interactive buffer position range-start range-end default-editable)
      (progn (gtk-text-buffer-insert-range buffer position range-start range-end)
             t)))

(export 'text-buffer-insert-range)

(defcfun gtk-text-buffer-delete :void
  (buffer (g-object text-buffer))
  (range-start (g-boxed-ref text-iter))
  (range-end (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-delete-interactive :boolean
  (buffer (g-object text-buffer))
  (range-start (g-boxed-ref text-iter))
  (range-end (g-boxed-ref text-iter))
  (default-editable :boolean))

(defun text-buffer-delete (buffer range-start range-end &key interactive default-editable)
  (if interactive
      (gtk-text-buffer-delete-interactive buffer range-start range-end default-editable)
      (progn (gtk-text-buffer-delete buffer range-start range-end)
             t)))

(export 'text-buffer-delete)

(defcfun gtk-text-buffer-backspace :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (interactive :boolean)
  (default-editable :boolean))

(defun text-buffer-backspace (buffer position &key interactive default-editable)
  (gtk-text-buffer-backspace buffer position interactive default-editable))

(export 'text-buffer-backspace)

(defcfun gtk-text-buffer-get-slice (:string :free-from-foreign t)
  (buffer (g-object text-buffer))
  (range-start (g-boxed-ref text-iter))
  (range-end (g-boxed-ref text-iter))
  (include-hidden-chars :boolean))

(defun text-buffer-slice (buffer range-start range-end &key include-hidden-chars)
  (gtk-text-buffer-get-slice buffer range-start range-end include-hidden-chars))

(export 'text-buffer-slice)

(defcfun (text-buffer-insert-pixbuf "gtk_text_buffer_insert_pixbuf") :void
  (buffer (g-object text-buffer))
  (position (g-boxed-ref text-iter))
  (pixbuf (g-object pixbuf)))

(export 'text-buffer-insert-pixbuf)

(defcfun gtk-text-buffer-insert-child-anchor :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (anchor (g-object text-child-anchor)))

(defcfun gtk-text-buffer-create-child-anchor (g-object text-child-anchor)
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter)))

(defun text-buffer-insert-child-anchor (buffer position &optional anchor)
  (if anchor
      (progn (gtk-text-buffer-insert-child-anchor buffer position anchor)
             anchor)
      (gtk-text-buffer-create-child-anchor buffer position)))

(export 'text-buffer-insert-child-anchor)

(defcfun gtk-text-buffer-create-mark (g-object text-mark)
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (position (g-boxed-ref text-iter))
  (left-gravity :boolean))

(defun text-buffer-create-mark (buffer name position &optional (left-gravity t))
  (gtk-text-buffer-create-mark buffer name position left-gravity))

(export 'text-buffer-create-mark)

(defcfun gtk-text-buffer-move-mark :void
  (buffer (g-object text-buffer))
  (mark (g-object text-mark))
  (position (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-move-mark-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (position (g-boxed-ref text-iter)))

(defun text-buffer-move-mark (buffer mark position)
  (etypecase mark
    (string (gtk-text-buffer-move-mark-by-name buffer mark position))
    (text-mark (gtk-text-buffer-move-mark buffer mark position))))

(export 'text-buffer-move-mark)

(defcfun (text-buffer-add-mark "gtk_text_buffer_add_mark") :void
  (buffer (g-object text-buffer))
  (mark (g-object text-mark))
  (position (g-boxed-ref text-iter)))

(export 'text-buffer-add-mark)

(defcfun gtk-text-buffer-delete-mark :void
  (buffer (g-object text-buffer))
  (mark (g-object text-mark)))

(defcfun gtk-text-buffer-delete-mark-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t)))

(defun text-buffer-delete-mark (buffer mark)
  (etypecase mark
    (string (gtk-text-buffer-delete-mark-by-name buffer mark))
    (text-mark (gtk-text-buffer-delete-mark buffer mark))))

(export 'text-buffer-delete-mark)

(defcfun (text-buffer-get-mark "gtk_text_buffer_get_mark") (g-object text-mark)
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t)))

(export 'text-buffer-get-mark)

(defcfun (text-buffer-insertion-mark "gtk_text_buffer_get_insert") (g-object text-mark)
  (buffer (g-object text-buffer)))

(export 'text-buffer-insertion-mark)

(defcfun (text-buffer-selection-bound "gtk_text_buffer_get_selection_bound") (g-object text-mark)
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t)))

(export 'text-buffer-selection-bound)

(defcfun (text-buffer-place-cursor "gtk_text_buffer_place_cursor") :void
  (buffer (g-object text-buffer))
  (position (g-boxed-ref text-iter)))

(export 'text-buffer-place-cursor)

(defcfun (text-buffer-select-range "gtk_text_buffer_select_range") :void
  (buffer (g-object text-buffer))
  (insertion-point (g-boxed-ref text-iter))
  (selection-bound (g-boxed-ref text-iter)))

(export 'text-buffer-select-range)

(defcfun gtk-text-buffer-apply-tag :void
  (buffer (g-object text-buffer))
  (tag (g-object text-tag))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-apply-tag-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defun text-buffer-apply-tag (buffer tag start end)
  (etypecase tag
    (string (gtk-text-buffer-apply-tag-by-name buffer tag start end))
    (text-tag (gtk-text-buffer-apply-tag buffer tag start end))))

(export 'text-buffer-apply-tag)

(defcfun gtk-text-buffer-remove-tag :void
  (buffer (g-object text-buffer))
  (tag (g-object text-tag))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-remove-tag-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defun text-buffer-remove-tag (buffer tag start end)
  (etypecase tag
    (string (gtk-text-buffer-remove-tag-by-name buffer tag start end))
    (text-tag (gtk-text-buffer-remove-tag buffer tag start end))))

(export 'text-buffer-remove-tag)

(defcfun (text-buffer-remove-all-tags "gtk_text_buffer_remove_all_tags") :void
  (buffer (g-object text-buffer))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defcfun gtk-text-buffer-get-iter-at-line-offset :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (line-number :int)
  (char-offset :int))

(defun text-buffer-get-iter-at-line-offset (buffer line-number char-offset)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-iter-at-line-offset buffer it line-number char-offset)))

(export 'text-buffer-get-iter-at-line-offset)

(defcfun gtk-text-buffer-get-iter-at-offset :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (char-offset :int))

(defun text-buffer-get-iter-at-offset (buffer offset)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-iter-at-offset buffer it offset)))

(export 'text-buffer-get-iter-at-offset)

(defcfun gtk-text-buffer-get-iter-at-line :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (line-number :int))

(defun text-buffer-get-iter-at-line (buffer line-number)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-iter-at-line buffer it line-number)))

(export 'text-buffet-get-iter-at-line)

(defcfun gtk-text-buffer-get-iter-at-mark :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (mark (g-object text-mark)))

(defun text-buffer-get-iter-at-mark (buffer mark)
  (when (stringp mark)
    (setf mark (text-buffer-get-mark buffer mark)))
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-iter-at-mark buffer it mark)))

(export 'text-buffer-get-iter-at-mark)

(defcfun gtk-text-buffer-get-iter-at-child-anchor :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter))
  (anchor (g-object text-child-anchor)))

(defun text-buffer-get-iter-at-child-anchor (buffer anchor)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-iter-at-child-anchor buffer it anchor)))

(export 'text-buffer-get-iter-at-child-anchor)

(defcfun gtk-text-buffer-get-start-iter :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter)))

(defun text-buffer-get-start-iter (buffer)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-start-iter buffer it)))

(export 'text-buffer-get-start-iter)

(defcfun gtk-text-buffer-get-end-iter :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-ref text-iter)))

(defun text-buffer-get-end-iter (buffer)
  (aprog1 (make-instance 'text-iter)
    (gtk-text-buffer-get-end-iter buffer it)))

(export 'text-buffer-get-end-iter)

(defcfun gtk-text-buffer-get-bounds :void
  (buffer (g-object text-buffer))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defun text-buffer-get-bounds (buffer)
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (gtk-text-buffer-get-bounds buffer start end)))

(export 'text-buffer-get-bounds)

(defcfun gtk-text-buffer-delete-selection :boolean
  (bufer (g-object text-buffer))
  (interactive :boolean)
  (default-editable :boolean))

(defun text-buffer-delete-selection (buffer &key interactive default-editable)
  (gtk-text-buffer-delete-selection buffer interactive default-editable))

(export 'text-buffer-delete-selection)

(defcfun gtk-text-buffer-paste-clipboard :void
  (buffer (g-object text-buffer))
  (clipboard (g-object clipboard))
  (override-location (g-boxed-ref text-iter))
  (default-editable :boolean))

(defun text-buffer-paste-clipboard (buffer clipboard &key position default-editable)
  (gtk-text-buffer-paste-clipboard buffer clipboard position default-editable))

(export 'text-buffer-paste-clipboard)

(defcfun (text-buffer-copy-clipboard "gtk_text_buffer_copy_clipboard") :void
  (buffer (g-object text-buffer))
  (clipboard (g-object clipboard)))

(export 'text-buffer-copy-clipboard)

(defcfun (text-buffer-cut-clipboard "gtk_text_buffer_cut_clipboard") :void
  (buffer (g-object text-buffer))
  (clipboard (g-object clipboard)))

(export 'text-buffer-cut-clipboard)

(defcfun gtk-text-buffer-get-selection-bounds :boolean
  (buffer (g-object text-buffer))
  (start (g-boxed-ref text-iter))
  (end (g-boxed-ref text-iter)))

(defun text-buffer-get-selection-bounds (buffer)
  (let ((i1 (make-instance 'text-iter))
        (i2 (make-instance 'text-iter)))
    (if (gtk-text-buffer-get-selection-bounds buffer i1 i2)
        (values i1 i2)
        (progn (release i1)
               (release i2)
               (values nil nil)))))

(export 'text-buffer-get-selection-bounds)

(defcfun (text-buffer-begin-user-action "gtk_text_buffer_begin_user_action") :void
  (buffer (g-object text-buffer)))

(export 'text-buffer-begin-user-action)

(defcfun (text-buffer-end-user-action "gtk_text_buffer_end_user_action") :void
  (buffer (g-object text-buffer)))

(export 'text-buffer-end-user-action)

(defmacro with-text-buffer-user-action ((buffer) &body body)
  (let ((g (gensym)))
    `(let ((,g ,buffer))
       (text-buffer-begin-user-action ,g)
       (unwind-protect
            (progn ,@body)
         (text-buffer-end-user-action ,g)))))

(export 'with-text-buffer-user-action)

(defcfun (text-buffer-add-selection-clipboard "gtk_text_buffer_add_selection_clipboard") :void
  (buffer (g-object text-buffer))
  (clipboard (g-object clipboard)))

(defcfun (text-buffer-remove-selection-clipboard "gtk_text_buffer_remove_selection_clipboard") :void
  (buffer (g-object text-buffer))
  (clipboard (g-object clipboard)))

(export 'text-buffer-remove-selection-clipboard)

;; enum                GtkTextBufferTargetInfo;
;; gboolean            (*GtkTextBufferDeserializeFunc)     (GtkTextBuffer *register_buffer,
;;                                                          GtkTextBuffer *content_buffer,
;;                                                          GtkTextIter *iter,
;;                                                          const guint8 *data,
;;                                                          gsize length,
;;                                                          gboolean create_tags,
;;                                                          gpointer user_data,
;;                                                          GError **error);
;; gboolean            gtk_text_buffer_deserialize         (GtkTextBuffer *register_buffer,
;;                                                          GtkTextBuffer *content_buffer,
;;                                                          GdkAtom format,
;;                                                          GtkTextIter *iter,
;;                                                          const guint8 *data,
;;                                                          gsize length,
;;                                                          GError **error);
;; gboolean            gtk_text_buffer_deserialize_get_can_create_tags
;;                                                         (GtkTextBuffer *buffer,
;;                                                          GdkAtom format);
;; void                gtk_text_buffer_deserialize_set_can_create_tags
;;                                                         (GtkTextBuffer *buffer,
;;                                                          GdkAtom format,
;;                                                          gboolean can_create_tags);
;; GtkTargetList*      gtk_text_buffer_get_copy_target_list
;;                                                         (GtkTextBuffer *buffer);
;; GdkAtom*            gtk_text_buffer_get_deserialize_formats
;;                                                         (GtkTextBuffer *buffer,
;;                                                          gint *n_formats);
;; GtkTargetList*      gtk_text_buffer_get_paste_target_list
;;                                                         (GtkTextBuffer *buffer);
;; GdkAtom*            gtk_text_buffer_get_serialize_formats
;;                                                         (GtkTextBuffer *buffer,
;;                                                          gint *n_formats);
;; GdkAtom             gtk_text_buffer_register_deserialize_format
;;                                                         (GtkTextBuffer *buffer,
;;                                                          const gchar *mime_type,
;;                                                          GtkTextBufferDeserializeFunc function,
;;                                                          gpointer user_data,
;;                                                          GDestroyNotify user_data_destroy);
;; GdkAtom             gtk_text_buffer_register_deserialize_tagset
;;                                                         (GtkTextBuffer *buffer,
;;                                                          const gchar *tagset_name);
;; GdkAtom             gtk_text_buffer_register_serialize_format
;;                                                         (GtkTextBuffer *buffer,
;;                                                          const gchar *mime_type,
;;                                                          GtkTextBufferSerializeFunc function,
;;                                                          gpointer user_data,
;;                                                          GDestroyNotify user_data_destroy);
;; GdkAtom             gtk_text_buffer_register_serialize_tagset
;;                                                         (GtkTextBuffer *buffer,
;;                                                          const gchar *tagset_name);
;; guint8*             (*GtkTextBufferSerializeFunc)       (GtkTextBuffer *register_buffer,
;;                                                          GtkTextBuffer *content_buffer,
;;                                                          const GtkTextIter *start,
;;                                                          const GtkTextIter *end,
;;                                                          gsize *length,
;;                                                          gpointer user_data);
;; guint8*             gtk_text_buffer_serialize           (GtkTextBuffer *register_buffer,
;;                                                          GtkTextBuffer *content_buffer,
;;                                                          GdkAtom format,
;;                                                          const GtkTextIter *start,
;;                                                          const GtkTextIter *end,
;;                                                          gsize *length);
;; void                gtk_text_buffer_unregister_deserialize_format
;;                                                         (GtkTextBuffer *buffer,
;;                                                          GdkAtom format);
;; void                gtk_text_buffer_unregister_serialize_format
;;                                                         (GtkTextBuffer *buffer,
;;                                                          GdkAtom format);

;; text tag

(defcfun (text-tag-priority "gtk_text_tag_get_priority") :int
  (tag (g-object text-tag)))

(defcfun gtk-text-tag-set-priority :void
  (tag (g-object text-tag))
  (priority :int))

(defun (setf text-tag-priority) (new-value tag)
  (gtk-text-tag-set-priority tag new-value))

(export 'text-tag-priority)

;; text tag table

(defcallback gtk-text-tag-table-foreach-function :void ((tag (g-object text-tag)) (data :pointer))
  (funcall (get-stable-pointer-value data) tag))

(defcfun (text-tag-table-add "gtk_text_tag_table_add") :void
  (table (g-object text-tag-table))
  (tag (g-object text-tag)))

(export 'text-tag-table-add)

(defcfun (text-tag-table-remove "gtk_text_tag_table_remove") :void
  (table (g-object text-tag-table))
  (tag (g-object text-tag)))

(export 'text-tag-table-remove)

(defcfun (text-tag-table-lookup "gtk_text_tag_table_lookup") (g-object text-tag)
  (table (g-object text-tag-table))
  (name (:string :free-to-foreign t)))

(export 'text-tag-table-lookup)

(defcfun gtk-text-tag-table-foreach :void
  (table (g-object text-tag-table))
  (function :pointer)
  (data :pointer))

(defun text-tag-table-foreach (table function)
  (with-stable-pointer (ptr function)
    (gtk-text-tag-table-foreach table (callback gtk-text-table-foreach-function) ptr)))

(export 'text-tag-table-foreach)

(defcfun (text-tag-table-size "gtk_text_tag_table_get_size") :int
  (table (g-object text-tag-table)))

(export 'text-tag-table-size)

;; text view

(defcfun gtk-text-view-scroll-to-mark :void
  (text-view (g-object text-view))
  (mark (g-object text-mark))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun text-view-scroll-to-mark (text-view mark &key (within-margin 0.4) (x-align 0.0 x-align-supplied) (y-align 0.0 y-align-supplied))
  (gtk-text-view-scroll-to-mark text-view mark within-margin (or x-align-supplied y-align-supplied) (coerce x-align 'double-float) (coerce y-align 'double-float)))

(export 'text-view-scroll-to-mark)

(defcfun gtk-text-view-scroll-to-iter :void
  (text-view (g-object text-view))
  (iter (g-object text-iter))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun text-view-scroll-to-iter (text-view iter &key (within-margin 0.4) (x-align 0.0 x-align-supplied) (y-align 0.0 y-align-supplied))
  (gtk-text-view-scroll-to-iter text-view iter within-margin (or x-align-supplied y-align-supplied) (coerce x-align 'double-float) (coerce y-align 'double-float)))

(export 'text-view-scroll-to-iter)

(defcfun (text-view-move-mark-onscreen "gtk_text_view_move_mark_onscreen") :boolean
  (text-view (g-object text-view))
  (mark (g-object text-mark)))

(export 'text-view-move-mark-onscreen)

(defcfun (text-view-place-cursor-onscreen "gtk_text_view_place_cursor_onscreen") :boolean
  (text-view (g-object text-view)))

(export 'text-view-place-cursor-onscreen)

(defcfun gtk-text-view-get-visible-rect :void
  (text-view (g-object text-view))
  (visible-rect (g-boxed-ptr rectangle :in-out)))

(defun text-view-visible-rect (text-view)
  (aprog1 (make-rectangle :x 0 :y 0 :width 0 :height 0)
    (gtk-text-view-get-visible-rect text-view it)))

(export 'text-view-visible-rect)

(defcfun gtk-text-view-get-iter-location :void
  (text-view (g-object text-view))
  (iter (g-boxed-ref text-iter))
  (location (g-boxed-ptr rectangle :in-out)))

(defun text-view-iter-location (text-view iter)
  (aprog1 (make-rectangle :x 0 :y 0 :width 0 :height 0)
    (gtk-text-view-get-iter-location text-view iter it)))

(export 'text-view-iter-location)


;; void                gtk_text_view_get_line_at_y         (GtkTextView *text_view,
;;                                                          GtkTextIter *target_iter,
;;                                                          gint y,
;;                                                          gint *line_top);
;; void                gtk_text_view_get_line_yrange       (GtkTextView *text_view,
;;                                                          const GtkTextIter *iter,
;;                                                          gint *y,
;;                                                          gint *height);
;; void                gtk_text_view_get_iter_at_location  (GtkTextView *text_view,
;;                                                          GtkTextIter *iter,
;;                                                          gint x,
;;                                                          gint y);
;; void                gtk_text_view_get_iter_at_position  (GtkTextView *text_view,
;;                                                          GtkTextIter *iter,
;;                                                          gint *trailing,
;;                                                          gint x,
;;                                                          gint y);
;; void                gtk_text_view_buffer_to_window_coords
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextWindowType win,
;;                                                          gint buffer_x,
;;                                                          gint buffer_y,
;;                                                          gint *window_x,
;;                                                          gint *window_y);
;; void                gtk_text_view_window_to_buffer_coords
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextWindowType win,
;;                                                          gint window_x,
;;                                                          gint window_y,
;;                                                          gint *buffer_x,
;;                                                          gint *buffer_y);
;; GdkWindow*          gtk_text_view_get_window            (GtkTextView *text_view,
;;                                                          GtkTextWindowType win);
;; GtkTextWindowType   gtk_text_view_get_window_type       (GtkTextView *text_view,
;;                                                          GdkWindow *window);
;; void                gtk_text_view_set_border_window_size
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextWindowType type,
;;                                                          gint size);
;; gint                gtk_text_view_get_border_window_size
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextWindowType type);
;; gboolean            gtk_text_view_forward_display_line  (GtkTextView *text_view,
;;                                                          GtkTextIter *iter);
;; gboolean            gtk_text_view_backward_display_line (GtkTextView *text_view,
;;                                                          GtkTextIter *iter);
;; gboolean            gtk_text_view_forward_display_line_end
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextIter *iter);
;; gboolean            gtk_text_view_backward_display_line_start
;;                                                         (GtkTextView *text_view,
;;                                                          GtkTextIter *iter);
;; gboolean            gtk_text_view_starts_display_line   (GtkTextView *text_view,
;;                                                          const GtkTextIter *iter);
;; gboolean            gtk_text_view_move_visually         (GtkTextView *text_view,
;;                                                          GtkTextIter *iter,
;;                                                          gint count);
;; void                gtk_text_view_add_child_at_anchor   (GtkTextView *text_view,
;;                                                          GtkWidget *child,
;;                                                          GtkTextChildAnchor *anchor);
;;                     GtkTextChildAnchor;
;; GtkTextChildAnchor* gtk_text_child_anchor_new           (void);
;; GList*              gtk_text_child_anchor_get_widgets   (GtkTextChildAnchor *anchor);
;; gboolean            gtk_text_child_anchor_get_deleted   (GtkTextChildAnchor *anchor);
;; void                gtk_text_view_add_child_in_window   (GtkTextView *text_view,
;;                                                          GtkWidget *child,
;;                                                          GtkTextWindowType which_window,
;;                                                          gint xpos,
;;                                                          gint ypos);
;; void                gtk_text_view_move_child            (GtkTextView *text_view,
;;                                                          GtkWidget *child,
;;                                                          gint xpos,
;;                                                          gint ypos);
