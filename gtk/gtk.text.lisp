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

(at-init () (foreign-funcall "gtk_text_iter_get_type" :int))

(define-g-boxed-opaque text-iter "GtkTextIter"
  :alloc (gtk-text-iter-alloc))

(define-boxed-opaque-accessor text-iter text-iter-buffer :reader "gtk_text_iter_get_buffer" :type (g-object text-buffer))
(define-boxed-opaque-accessor text-iter text-iter-offset :reader "gtk_text_iter_get_offset" :writer "gtk_text_iter_set_offset" :type :int)
(define-boxed-opaque-accessor text-iter text-iter-line :reader "gtk_text_iter_get_line" :writer "gtk_text_iter_set_line" :type :int)
(define-boxed-opaque-accessor text-iter text-iter-line-offset :reader "gtk_text_iter_get_line_offset" :writer "gtk_text_iter_set_line_offset" :type :int)
(define-boxed-opaque-accessor text-iter text-iter-visible-line-offset :reader "gtk_text_iter_get_visible_line_offset" :writer "gtk_text_iter_set_visible_line_offset" :type :int)
(define-boxed-opaque-accessor text-iter text-iter-char :reader "gtk_text_iter_get_char" :type unichar)
(define-boxed-opaque-accessor text-iter text-iter-pixbuf :reader "gtk_text_iter_get_pixbuf" :type (g-object pixbuf))
(define-boxed-opaque-accessor text-iter text-iter-marks :reader "gtk_text_iter_get_marks" :type (gslist (g-object text-mark) :free-from-foreign t))
(define-boxed-opaque-accessor text-iter text-iter-child-anchor :reader "gtk_text_iter_get_child_anchor" :type (g-object text-child-anchor))
(define-boxed-opaque-accessor text-iter text-iter-tags :reader "gtk_text_iter_get_tags" :type (gslist (g-object text-tag) :free-from-foreign t))
(define-boxed-opaque-accessor text-iter text-iter-chars-in-line :reader "gtk_text_iter_get_chars_in_line" :type :int)
(define-boxed-opaque-accessor text-iter text-iter-language :reader "gtk_text_iter_get_language" :type :pointer)
(define-boxed-opaque-accessor text-iter text-iter-is-end :reader "gtk_text_iter_is_end" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-is-start :reader "gtk_text_iter_is_start" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-starts-word :reader "gtk_text_iter_starts_word" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-ends-word :reader "gtk_text_iter_ends_word" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-inside-word :reader "gtk_text_iter_inside_word" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-starts-line :reader "gtk_text_iter_starts_line" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-ends-line :reader "gtk_text_iter_ends_line" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-starts-sentence :reader "gtk_text_iter_starts_sentence" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-ends-sentence :reader "gtk_text_iter_ends_sentence" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-inside-sentence :reader "gtk_text_iter_inside_sentence" :type :boolean)
(define-boxed-opaque-accessor text-iter text-iter-is-cursor-position :reader "gtk_text_iter_is_cursor_position" :type :boolean)

(export (boxed-related-symbols 'text-iter))
(export '(text-iter-buffer text-iter-offset text-iter-line
          text-iter-line-offset text-iter-visible-line-offset
          text-iter-char text-iter-pixbuf text-iter-marks
          text-iter-child-anchor text-iter-tags
          text-iter-chars-in-line text-iter-language text-iter-is-end
          text-iter-is-start text-iter-can-insert
          text-iter-starts-word text-iter-ends-word
          text-iter-inside-word text-iter-starts-line
          text-iter-ends-line text-iter-starts-sentence
          text-iter-ends-sentence text-iter-inside-sentence
          text-iter-is-cursor-position ))

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

(defun gtk-text-iter-alloc ()
  (with-foreign-object (iter '%text-iter)
    (gtk-text-iter-copy iter)))

(defcfun (text-iter-slice "gtk_text_iter_get_slice") (g-string :free-from-foreign t)
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(export 'text-iter-slice)

(defcfun (text-iter-text "gtk_text_iter_get_text") (g-string :free-from-foreign t)
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(export 'text-iter-text)

(defcfun (text-iter-visible-slice "gtk_text_iter_get_visible_slice") (g-string :free-from-foreign t)
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(export 'text-iter-visible-slice)

(defcfun (text-iter-visible-text "gtk_text_iter_get_visible_text") (g-string :free-from-foreign t)
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(export 'text-iter-visible-text)

(defcfun (text-iter-toggled-tags "gtk_text_iter_get_toggled_tags") (gslist (g-object text-tag))
  (iter (g-boxed-foreign text-iter))
  (toggled-on :boolean))

(export 'text-iter-toggled-tags)

(defcfun (text-iter-begins-tag "gtk_text_iter_begins_tag") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-begins-tag)

(defcfun (text-iter-ends-tag "gtk_text_iter_ends_tag") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-ends-tag)

(defcfun (text-iter-toggles-tag "gtk_text_iter_toggles_tag") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-toggles-tag)

(defcfun (text-iter-has-tag "gtk_text_iter_has_tag") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(export 'text-iter-has-tag)

(defcfun (text-iter-editable "gtk_text_iter_editable") :boolean
  (iter (g-boxed-foreign text-iter))
  (default :boolean))

(export 'text-iter-editable)

(defcfun (text-iter-can-insert "gtk_text_iter_can_insert") :boolean
  (iter (g-boxed-foreign text-iter))
  (default-editable :boolean))

(export 'text-iter-can-insert)

(defcfun gtk-text-iter-get-attributes :boolean
  (iter (g-boxed-foreign text-iter))
  (values (g-object text-attributes)))

(defun text-iter-attributes (iter default-attributes)
  (let ((changed-p (gtk-text-iter-get-attributes iter default-attributes)))
    (values default-attributes changed-p)))

(export 'text-iter-attributes)

(defcfun gtk-text-iter-forward-chars :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-lines :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-word-ends :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-word-starts :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-cursor-positions :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-sentence-ends :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-sentence-starts :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-word-ends :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-backward-visible-word-starts :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-cursor-positions :boolean
  (iter (g-boxed-foreign text-iter))
  (count :int))

(defcfun gtk-text-iter-forward-visible-lines :boolean
  (iter (g-boxed-foreign text-iter))
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
  (iter (g-boxed-foreign text-iter)))

(export 'text-iter-forward-to-end)

(defcfun (text-iter-forward-to-line-end "gtk_text_iter_forward_to_line_end") :boolean
  (iter (g-boxed-foreign text-iter)))

(export 'text-iter-forward-to-line-end)

(defcfun (text-iter-forward-to-tag-toggle "gtk_text_iter_forward_to_tag_toggle") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(defcfun (text-iter-backward-to-tag-toggle "gtk_text_iter_backward_to_tag_toggle") :boolean
  (iter (g-boxed-foreign text-iter))
  (tag (g-object text-tag)))

(export '(text-iter-forward-to-tag-toggle text-iter-backward-to-tag-toggle))

(defcallback gtk-text-char-predicate :boolean ((char unichar) (user-data :pointer))
  (let ((function (get-stable-pointer-value user-data)))
    (funcall function char)))

(defcfun gtk-text-iter-forward-find-char :boolean
  (iter (g-boxed-foreign text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign text-iter)))

(defcfun gtk-text-iter-backward-find-char :boolean
  (iter (g-boxed-foreign text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g-boxed-foreign text-iter)))

(defun text-iter-find-char (iter predicate &key limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (with-stable-pointer (ptr predicate)
    (if (eq direction :forward)
        (gtk-text-iter-forward-find-char iter (callback gtk-text-char-predicate) ptr limit)
        (gtk-text-iter-backward-find-char iter (callback gtk-text-char-predicate) ptr limit))))

(export 'text-iter-find-char)

(defcfun gtk-text-iter-forward-search :boolean
  (iter (g-boxed-foreign text-iter))
  (str (:string :free-to-foreign t))
  (flags text-search-flags)
  (match-start (g-boxed-foreign text-iter))
  (match-end (g-boxed-foreign text-iter))
  (limit (g-boxed-foreign text-iter)))

(defcfun gtk-text-iter-backward-search :boolean
  (iter (g-boxed-foreign text-iter))
  (str (:string :free-to-foreign t))
  (flags text-search-flags)
  (match-start (g-boxed-foreign text-iter))
  (match-end (g-boxed-foreign text-iter))
  (limit (g-boxed-foreign text-iter)))

(defun text-iter-search (start-position string &key flags limit (direction :forward))
  (assert (typep direction '(member :forward :backward)))
  (let ((i1 (make-instance 'text-iter))
        (i2 (make-instance 'text-iter)))
    (if (if (eq direction :forward)
            (gtk-text-iter-forward-search start-position string flags i1 i2 limit)
            (gtk-text-iter-backward-search start-position string flags i1 i2 limit))
        (values t i1 i2)
        (values nil nil nil))))

(export 'text-iter-search)

(defcfun (text-iter-equal "gtk_text_iter_equal") :boolean
  (iter-1 (g-boxed-foreign text-iter))
  (iter-2 (g-boxed-foreign text-iter)))

(export 'text-iter-equal)

(defcfun (text-iter-compare "gtk_text_iter_compare") :int
  (iter-1 (g-boxed-foreign text-iter))
  (iter-2 (g-boxed-foreign text-iter)))

(export 'text-iter-compare)

(defcfun (text-iter-in-range "gtk_text_iter_in_range") :boolean
  (iter (g-boxed-foreign text-iter))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(export 'text-iter-in-range)

(defcfun (text-iter-order "gtk_text_iter_order") :void
  (iter-1 (g-boxed-foreign text-iter))
  (iter-2 (g-boxed-foreign text-iter)))

(export 'text-iter-order)

;; text buffer

(defcfun (text-buffer-line-count "gtk_text_buffer_get_line_count") :int
  (buffer (g-object text-buffer)))

(export 'text-buffer-line-count)

(defcfun (text-buffer-char-count "gtk_text_buffer_get_char_count") :int
  (buffer (g-object text-buffer)))

(export 'text-buffer)

(defcfun gtk-text-buffer-insert :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (text (:string :free-to-foreign t))
  (len :int))

(defcfun gtk-text-buffer-insert-at-cursor :void
  (buffer (g-object text-buffer))
  (text (:string :free-to-foreign t))
  (len :int))

(defcfun gtk-text-buffer-insert-interactive :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
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
          (gtk-text-buffer-insert-interactive-at-cursor buffer text -1 default-editable)
          (gtk-text-buffer-insert-interactive buffer position text -1 default-editable))
      (progn (if (eq position :cursor)
                 (gtk-text-buffer-insert-at-cursor buffer text -1)
                 (gtk-text-buffer-insert buffer position text -1))
             t)))

(export 'text-buffer-insert)

(defcfun gtk-text-buffer-insert-range :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (range-start (g-boxed-foreign text-iter))
  (range-end (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-insert-range-interactive :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (range-start (g-boxed-foreign text-iter))
  (range-end (g-boxed-foreign text-iter))
  (default-editable :boolean))

(defun text-buffer-insert-range (buffer position range-start range-end &key interactive default-editable)
  (if interactive
      (gtk-text-buffer-insert-range-interactive buffer position range-start range-end default-editable)
      (progn (gtk-text-buffer-insert-range buffer position range-start range-end)
             t)))

(export 'text-buffer-insert-range)

(defcfun gtk-text-buffer-delete :void
  (buffer (g-object text-buffer))
  (range-start (g-boxed-foreign text-iter))
  (range-end (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-delete-interactive :boolean
  (buffer (g-object text-buffer))
  (range-start (g-boxed-foreign text-iter))
  (range-end (g-boxed-foreign text-iter))
  (default-editable :boolean))

(defun text-buffer-delete (buffer range-start range-end &key interactive default-editable)
  (if interactive
      (gtk-text-buffer-delete-interactive buffer range-start range-end default-editable)
      (progn (gtk-text-buffer-delete buffer range-start range-end)
             t)))

(export 'text-buffer-delete)

(defcfun gtk-text-buffer-backspace :boolean
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (interactive :boolean)
  (default-editable :boolean))

(defun text-buffer-backspace (buffer position &key interactive default-editable)
  (gtk-text-buffer-backspace buffer position interactive default-editable))

(export 'text-buffer-backspace)

(defcfun gtk-text-buffer-get-slice (:string :free-from-foreign t)
  (buffer (g-object text-buffer))
  (range-start (g-boxed-foreign text-iter))
  (range-end (g-boxed-foreign text-iter))
  (include-hidden-chars :boolean))

(defun text-buffer-slice (buffer range-start range-end &key include-hidden-chars)
  (gtk-text-buffer-get-slice buffer range-start range-end include-hidden-chars))

(export 'text-buffer-slice)

(defcfun (text-buffer-insert-pixbuf "gtk_text_buffer_insert_pixbuf") :void
  (buffer (g-object text-buffer))
  (position (g-boxed-foreign text-iter))
  (pixbuf (g-object pixbuf)))

(export 'text-buffer-insert-pixbuf)

(defcfun gtk-text-buffer-insert-child-anchor :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (anchor (g-object text-child-anchor)))

(defcfun gtk-text-buffer-create-child-anchor (g-object text-child-anchor)
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter)))

(defun text-buffer-insert-child-anchor (buffer position &optional anchor)
  (if anchor
      (progn (gtk-text-buffer-insert-child-anchor buffer position anchor)
             anchor)
      (gtk-text-buffer-create-child-anchor buffer position)))

(export 'text-buffer-insert-child-anchor)

(defcfun gtk-text-buffer-create-mark (g-object text-mark)
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (position (g-boxed-foreign text-iter))
  (left-gravity :boolean))

(defun text-buffer-create-mark (buffer name position &optional (left-gravity t))
  (gtk-text-buffer-create-mark buffer name position left-gravity))

(export 'text-buffer-create-mark)

(defcfun gtk-text-buffer-move-mark :void
  (buffer (g-object text-buffer))
  (mark (g-object text-mark))
  (position (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-move-mark-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (position (g-boxed-foreign text-iter)))

(defun text-buffer-move-mark (buffer mark position)
  (etypecase mark
    (string (gtk-text-buffer-move-mark-by-name buffer mark position))
    (text-mark (gtk-text-buffer-move-mark buffer mark position))))

(export 'text-buffer-move-mark)

(defcfun (text-buffer-add-mark "gtk_text_buffer_add_mark") :void
  (buffer (g-object text-buffer))
  (mark (g-object text-mark))
  (position (g-boxed-foreign text-iter)))

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
  (buffer (g-object text-buffer)))

(export 'text-buffer-selection-bound)

(defcfun (text-buffer-place-cursor "gtk_text_buffer_place_cursor") :void
  (buffer (g-object text-buffer))
  (position (g-boxed-foreign text-iter)))

(export 'text-buffer-place-cursor)

(defcfun (text-buffer-select-range "gtk_text_buffer_select_range") :void
  (buffer (g-object text-buffer))
  (insertion-point (g-boxed-foreign text-iter))
  (selection-bound (g-boxed-foreign text-iter)))

(export 'text-buffer-select-range)

(defcfun gtk-text-buffer-apply-tag :void
  (buffer (g-object text-buffer))
  (tag (g-object text-tag))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-apply-tag-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defun text-buffer-apply-tag (buffer tag start end)
  (etypecase tag
    (string (gtk-text-buffer-apply-tag-by-name buffer tag start end))
    (text-tag (gtk-text-buffer-apply-tag buffer tag start end))))

(export 'text-buffer-apply-tag)

(defcfun gtk-text-buffer-remove-tag :void
  (buffer (g-object text-buffer))
  (tag (g-object text-tag))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-remove-tag-by-name :void
  (buffer (g-object text-buffer))
  (name (:string :free-to-foreign t))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defun text-buffer-remove-tag (buffer tag start end)
  (etypecase tag
    (string (gtk-text-buffer-remove-tag-by-name buffer tag start end))
    (text-tag (gtk-text-buffer-remove-tag buffer tag start end))))

(export 'text-buffer-remove-tag)

(defcfun (text-buffer-remove-all-tags "gtk_text_buffer_remove_all_tags") :void
  (buffer (g-object text-buffer))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defcfun gtk-text-buffer-get-iter-at-line-offset :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (line-number :int)
  (char-offset :int))

(defun text-buffer-get-iter-at-line-offset (buffer line-number char-offset)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-iter-at-line-offset buffer iter line-number char-offset)
    iter))

(export 'text-buffer-get-iter-at-line-offset)

(defcfun gtk-text-buffer-get-iter-at-offset :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (char-offset :int))

(defun text-buffer-get-iter-at-offset (buffer offset)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-iter-at-offset buffer iter offset)
    iter))

(export 'text-buffer-get-iter-at-offset)

(defcfun gtk-text-buffer-get-iter-at-line :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (line-number :int))

(defun text-buffer-get-iter-at-line (buffer line-number)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-iter-at-line buffer iter line-number)
    iter))

(export 'text-buffet-get-iter-at-line)

(defcfun gtk-text-buffer-get-iter-at-mark :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (mark (g-object text-mark)))

(defun text-buffer-get-iter-at-mark (buffer mark)
  (when (stringp mark)
    (setf mark (text-buffer-get-mark buffer mark)))
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-iter-at-mark buffer iter mark)
    iter))

(export 'text-buffer-get-iter-at-mark)

(defcfun gtk-text-buffer-get-iter-at-child-anchor :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter))
  (anchor (g-object text-child-anchor)))

(defun text-buffer-get-iter-at-child-anchor (buffer anchor)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-iter-at-child-anchor buffer iter anchor)
    iter))

(export 'text-buffer-get-iter-at-child-anchor)

(defcfun gtk-text-buffer-get-start-iter :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter)))

(defun text-buffer-get-start-iter (buffer)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-start-iter buffer iter)
    iter))

(export 'text-buffer-get-start-iter)

(defcfun gtk-text-buffer-get-end-iter :void
  (buffer (g-object text-buffer))
  (iter (g-boxed-foreign text-iter)))

(defun text-buffer-get-end-iter (buffer)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-buffer-get-end-iter buffer iter)
    iter))

(export 'text-buffer-get-end-iter)

(defcfun gtk-text-buffer-get-bounds :void
  (buffer (g-object text-buffer))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defun text-buffer-get-bounds (buffer)
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (gtk-text-buffer-get-bounds buffer start end)
    (values start end)))

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
  (override-location (g-boxed-foreign text-iter))
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
  (clipboard (g-object clipboard))
  (default-editable :boolean))

(export 'text-buffer-cut-clipboard)

(defcfun gtk-text-buffer-get-selection-bounds :boolean
  (buffer (g-object text-buffer))
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter)))

(defun text-buffer-get-selection-bounds (buffer)
  (let ((i1 (make-instance 'text-iter))
        (i2 (make-instance 'text-iter)))
    (if (gtk-text-buffer-get-selection-bounds buffer i1 i2)
        (values i1 i2)
        (values nil nil))))

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

(defcfun gtk-text-buffer-deserialize :boolean
  (register-buffer (g-object text-buffer))
  (content-buffer (g-object text-buffer))
  (format gdk-atom-as-string)
  (iter (g-boxed-foreign text-iter))
  (data :pointer)
  (length gsize)
  (error :pointer))

(defun text-buffer-deserialize (register-buffer content-buffer format iter data)
  (let ((bytes (foreign-alloc :uint8 :count (length data))))
    (iter (for i from 0 below (length data))
          (setf (mem-aref bytes :uint8 i) (aref data i)))
    (unwind-protect
         (with-g-error (err)
           (gtk-text-buffer-deserialize register-buffer content-buffer
                                        format iter bytes (length data) err))
      (foreign-free bytes))))

(export 'text-buffer-deserialize)

(defcfun (text-buffer-deserialize-can-create-tags "gtk_text_buffer_deserialize_get_can_create_tags") :boolean
  (buffer (g-object text-buffer))
  (format gdk-atom-as-string))

(defcfun gtk-text-buffer-deserialize-set-can-create-tags :void
  (buffer (g-object text-buffer))
  (format gdk-atom-as-string)
  (can-create-tags :boolean))

(defun (setf text-buffer-deserialize-can-create-tags) (new-value buffer format)
  (gtk-text-buffer-deserialize-set-can-create-tags buffer format new-value))

(export 'text-buffer-deserialize-can-create-tags)

(defcfun gtk-text-buffer-get-deserialize-formats (:pointer gdk-atom-as-string)
  (text-buffer (g-object text-buffer))
  (n-formats (:pointer :int)))

(defun text-buffer-get-deserialize-formats (text-buffer)
  (with-foreign-object (n-formats :int)
    (let ((atoms-ptr (gtk-text-buffer-get-deserialize-formats text-buffer n-formats)))
      (iter (for i from 0 below (mem-ref n-formats :int))
            (for atom = (mem-aref atoms-ptr 'gdk-atom-as-string i))
            (collect atom)))))

(export 'text-buffer-get-deserialize-formats)

(defcfun gtk-text-buffer-get-serialize-formats (:pointer gdk-atom-as-string)
  (text-buffer (g-object text-buffer))
  (n-formats (:pointer :int)))

(defun text-buffer-get-serialize-formats (text-buffer)
  (with-foreign-object (n-formats :int)
    (let ((atoms-ptr (gtk-text-buffer-get-serialize-formats text-buffer n-formats)))
      (iter (for i from 0 below (mem-ref n-formats :int))
            (for atom = (mem-aref atoms-ptr 'gdk-atom-as-string i))
            (collect atom)))))

(export 'text-buffer-get-serialize-formats)

(defcallback gtk-text-buffer-deserialize-cb :boolean
    ((register-buffer (g-object text-buffer))
     (content-buffer (g-object text-buffer))
     (iter (g-boxed-foreign text-iter))
     (data :pointer)
     (length gsize)
     (create-tags :boolean)
     (user-data :pointer)
     (error :pointer))
  (with-catching-to-g-error (error)
    (let ((fn (stable-pointer-value user-data)))
      (restart-case
          (let ((bytes (iter (with bytes = (make-array length :element-type '(unsigned-byte 8)))
                             (for i from 0 below length)
                             (setf (aref bytes i) (mem-ref data :uint8 i))
                             (finally (return bytes)))))
            (progn (funcall fn register-buffer content-buffer iter bytes create-tags) t))
        (return-from-text-buffer-deserialize-cb ()
          (error 'g-error-condition
                 :domain "cl-gtk2"
                 :code 0
                 :message "'return-from-text-buffer-deserialize-cb' restart was called"))))))

(defcfun gtk-text-buffer-register-deserialize-format gdk-atom-as-string
  (buffer (g-object text-buffer))
  (mime-type :string)
  (function :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun text-buffer-register-deserialize-format (buffer mime-type function)
  (gtk-text-buffer-register-deserialize-format buffer mime-type
                                               (callback gtk-text-buffer-deserialize-cb)
                                               (allocate-stable-pointer function)
                                               (callback stable-pointer-free-destroy-notify-callback)))

(export 'text-buffer-register-deserialize-format)

(defcfun (text-buffer-register-deserialize-tagset "gtk_text_buffer_register_deserialize_tagset") gdk-atom-as-string
  (buffer (g-object text-buffer))
  (tagset-name :string))

(export 'text-buffer-register-deserialize-tagset)

(defcallback gtk-text-buffer-serialize-cb :pointer
    ((register-buffer (g-object text-buffer))
     (content-buffer (g-object text-buffer))
     (start-iter (g-boxed-foreign text-iter))
     (end-iter (g-boxed-foreign text-iter))
     (length (:pointer gsize))
     (user-data :pointer))
  (let ((fn (stable-pointer-value user-data)))
    (restart-case
        (let* ((bytes (funcall fn register-buffer content-buffer start-iter end-iter))
               (bytes-ptr (g-malloc (length bytes))))
          (setf (mem-ref length 'gsize) (length bytes))
          (iter (for i from 0 below (length bytes))
                (setf (mem-aref bytes-ptr :uint8 i) (aref bytes i)))
          bytes-ptr)
      (return-from-text-buffer-serialize-cb () nil))))

(defcfun gtk-text-buffer-register-serialize-format gdk-atom-as-string
  (buffer (g-object text-buffer))
  (mime-type :string)
  (function :pointer)
  (user-data :pointer)
  (destroy-notify :pointer))

(defun text-buffer-register-serialize-format (buffer mime-type function)
  (gtk-text-buffer-register-serialize-format buffer mime-type
                                             (callback gtk-text-buffer-serialize-cb)
                                             (allocate-stable-pointer function)
                                             (callback stable-pointer-free-destroy-notify-callback)))

(export 'text-buffer-register-serialize-format)

(defcfun (text-buffer-register-serialize-tagset "gtk_text_buffer_register_serialize_tagset") gdk-atom-as-string
  (buffer (g-object text-buffer))
  (tagset-name :string))

(export 'text-buffer-register-serialize-tagset)

(defcfun gtk-text-buffer-serialize :pointer
  (register-buffer (g-object text-buffer))
  (content-buffer (g-object text-buffer))
  (format gdk-atom-as-string)
  (start (g-boxed-foreign text-iter))
  (end (g-boxed-foreign text-iter))
  (length (:pointer gsize)))

(defun text-buffer-serialize (register-buffer content-buffer format start end)
  (with-foreign-object (length 'gsize)
    (let ((bytes (gtk-text-buffer-serialize register-buffer content-buffer format start end length)))
      (iter (for i from 0 to (mem-ref length 'gsize))
            (for byte = (mem-aref bytes :uint8 i))
            (collect byte result-type vector)
            (finally (g-free bytes))))))

(export 'text-buffer-serialize)

(defcfun (text-buffer-unregister-deserialize-format "gtk_text_buffer_unregister_deserialize_format") :void
  (buffer (g-object text-buffer))
  (format gdk-atom-as-string))

(export 'text-buffer-unregister-deserialize-format)

(defcfun (text-buffer-unregister-serialize-format "gtk_text_buffer_unregister_serialize_format") :void
  (buffer (g-object text-buffer))
  (format gdk-atom-as-string))

(export 'text-buffer-unregister-serialize-format)

;; text tag

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
    (gtk-text-tag-table-foreach table (callback gtk-text-tag-table-foreach-function) ptr)))

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
  (gtk-text-view-scroll-to-mark text-view mark (coerce within-margin 'double-float) (or x-align-supplied y-align-supplied) (coerce x-align 'double-float) (coerce y-align 'double-float)))

(export 'text-view-scroll-to-mark)

(defcfun gtk-text-view-scroll-to-iter :void
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (within-margin :double)
  (use-align :boolean)
  (x-align :double)
  (y-align :double))

(defun text-view-scroll-to-iter (text-view iter &key (within-margin 0.4) (x-align 0.0 x-align-supplied) (y-align 0.0 y-align-supplied))
  (gtk-text-view-scroll-to-iter text-view iter (coerce within-margin 'double-float) (or x-align-supplied y-align-supplied) (coerce x-align 'double-float) (coerce y-align 'double-float)))

(export 'text-view-scroll-to-iter)

(defcfun (text-view-scroll-mark-onscreen "gtk_text_view_scroll_mark_onscreen") :void
  (text-view (g-object text-view))
  (mark (g-object text-mark)))

(export 'text-view-scroll-mark-onscreen)

(defcfun (text-view-move-mark-onscreen "gtk_text_view_move_mark_onscreen") :boolean
  (text-view (g-object text-view))
  (mark (g-object text-mark)))

(export 'text-view-move-mark-onscreen)

(defcfun (text-view-place-cursor-onscreen "gtk_text_view_place_cursor_onscreen") :boolean
  (text-view (g-object text-view)))

(export 'text-view-place-cursor-onscreen)

(defcfun gtk-text-view-get-visible-rect :void
  (text-view (g-object text-view))
  (visible-rect (g-boxed-foreign rectangle)))

(defun text-view-visible-rect (text-view)
  (let ((rect (make-rectangle :x 0 :y 0 :width 0 :height 0)))
    (gtk-text-view-get-visible-rect text-view rect)
    rect))

(export 'text-view-visible-rect)

(defcfun gtk-text-view-get-iter-location :void
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (location (g-boxed-foreign rectangle)))

(defun text-view-iter-location (text-view iter)
  (let ((rect (make-rectangle :x 0 :y 0 :width 0 :height 0)))
    (gtk-text-view-get-iter-location text-view iter rect)
    rect))

(export 'text-view-iter-location)

(defcfun gtk-text-view-get-line-at-y :void
  (text-view (g-object text-view))
  (target-iter (g-boxed-foreign text-iter))
  (y :int)
  (line-top (:pointer :int)))

(defun text-view-get-line-at-y (text-view y)
  (let ((iter (make-instance 'text-iter)))
    (with-foreign-object (line-top :int)
      (gtk-text-view-get-line-at-y text-view iter y line-top)
      (values iter (mem-ref line-top :int)))))

(export 'text-view-get-line-at-y)

(defcfun gtk-text-view-get-line-yrange :void
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (y (:pointer :int))
  (height (:pointer :int)))

(defun text-view-get-line-yrange (text-view iter)
  (with-foreign-objects ((y :int) (height :int))
    (gtk-text-view-get-line-yrange text-view iter y height)
    (values (mem-ref y :int) (mem-ref height :int))))

(export 'text-view-get-line-yrange)

(defcfun gtk-text-view-get-iter-at-location :void
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (x :int)
  (y :int))

(defun text-view-get-iter-at-location (view x y)
  (let ((iter (make-instance 'text-iter)))
    (gtk-text-view-get-iter-at-location view iter x y)
    iter))

(export 'text-view-get-iter-at-location)

(defcfun gtk-text-view-get-iter-at-position :void
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (trailing (:pointer :int))
  (x :int)
  (y :int))

(defun text-view-get-iter-at-position (text-view x y)
  (with-foreign-object (trailing :int)
    (let ((iter (make-instance 'text-iter)))
      (gtk-text-view-get-iter-at-position text-view iter trailing x y)
      (values iter (mem-ref trailing :int)))))

(export 'text-view-get-iter-at-position)

(defcfun gtk-text-view-buffer-to-window-coords :void
  (text-view (g-object text-view))
  (win text-window-type)
  (buffer-x :int)
  (buffer-y :int)
  (window-x (:pointer :int))
  (window-y (:pointer :int)))

(defun text-view-buffer-to-window-coords (text-view window-type buffer-x buffer-y)
  (with-foreign-objects ((window-x :int) (window-y :int))
    (gtk-text-view-buffer-to-window-coords text-view window-type buffer-x buffer-y window-x window-y)
    (values (mem-ref window-x :int) (mem-ref window-y :int))))

(export 'text-view-buffer-to-window-coords)

(defcfun gtk-text-view-window-to-buffer-coords :void
  (text-view (g-object text-view))
  (win text-window-type)
  (window-x :int)
  (window-y :int)
  (buffer-x :pointer)
  (buffer-y :pointer))

(defun text-view-window-to-buffer-coords (text-view win window-x window-y)
  (with-foreign-objects ((buffer-x :int) (buffer-y :int))
    (gtk-text-view-window-to-buffer-coords text-view win window-x window-y buffer-x buffer-y)
    (values (mem-ref buffer-x :int)
            (mem-ref buffer-y :int))))

(export 'text-view-window-to-buffer-coords)

(defcfun gtk-text-view-get-window (g-object gdk:gdk-window)
  (text-view (g-object text-view))
  (win text-window-type))

(defun text-view-get-window (text-view win)
  (gtk-text-view-get-window text-view win))

(export 'text-view-get-window)

(defcfun (text-view-get-window-type "gtk_text_view_get_window_type") text-window-type
  (text-view (g-object text-view))
  (window (g-object gdk-window)))

(export 'text-view-get-window-type)

(defcfun gtk-text-view-set-border-window-size :void
  (text-view (g-object text-view))
  (window-type text-window-type)
  (size :int))

(defcfun (text-view-border-window-size "gtk_text_view_get_border_window_size") :int
  (text-view (g-object text-view))
  (window-type text-window-type))

(defun (setf text-view-border-window-size) (new-value text-view window-type)
  (gtk-text-view-set-border-window-size text-view window-type new-value)
  new-value)

(export 'text-view-border-window-size)

(defcfun (text-view-forward-display-line "gtk_text_view_forward_display_line") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter)))

(export 'text-view-forward-display-line)

(defcfun (text-view-backward-display-line "gtk_text_view_backward_display_line") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter)))

(export 'text-view-backward-display-line)

(defcfun (text-view-forward-display-line-end "gtk_text_view_forward_display_line_end") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter)))

(export 'text-view-forward-display-line-end)

(defcfun (text-view-backward-display-line-start "gtk_text_view_backward_display_line_start") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter)))

(export 'text-view-backward-display-line-start)

(defcfun (text-view-starts-display-line "gtk_text_view_starts_display_line") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter)))

(export 'text-view-starts-display-line)

(defcfun (text-view-move-visually "gtk_text_view_move_visually") :boolean
  (text-view (g-object text-view))
  (iter (g-boxed-foreign text-iter))
  (count :int))

(export 'text-view-move-visually)

(defcfun (text-view-add-child-at-anchor "gtk_text_view_add_child_at_anchor") :void
  (text-view g-object)
  (child g-object)
  (anchor g-object))

(export 'text-view-add-child-at-anchor)

(defcfun (text-child-anchor-widgets "gtk_text_child_anchor_get_widgets") (glist (g-object widget) :free-from-foreign t)
  (anchor (g-object text-child-anchor)))

(export 'text-child-anchor-widgets)

(defcfun (text-view-add-child-in-window "gtk_text_view_add_child_in_window") :void
  (text-view (g-object text-view))
  (child (g-object widget))
  (which-window text-window-type)
  (x-pos :int)
  (y-pos :int))

(export 'text-view-add-child-in-window)

(defcfun (text-view-move-child "gtk_text_view_move_child") :void
  (text-view (g-object text-view))
  (child (g-object widget))
  (x-pos :int)
  (y-pos :int))

(export 'text-view-move-child)
