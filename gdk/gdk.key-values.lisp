(in-package :gdk)

(defcfun (keymap-get-default "gdk_keymap_get_default") (g-object keymap))

(export 'keymap-get-default)

(defcfun (keymap-get-for-display "gdk_keymap_get_for_display") (g-object keymap)
  (display (g-object display)))

(export 'keymap-get-for-display)

(defcfun (keymap-lookup-key "gdk_keymap_lookup_key") :uint
  (keymap (g-object keymap))
  (key (g-boxed-foreign keymap-key)))

(export 'keymap-lookup-key)

(defcfun gdk_keymap_translate_keyboard_state :boolean
  (keymap (g-object keymap))
  (hardware-keycode :uint)
  (state modifier-type)
  (group :int)
  (keyval (:pointer :uint))
  (effective-group (:pointer :int))
  (level (:pointer :int))
  (consumed-modifiers (:pointer modifier-type)))

(defun keymap-translate-keyboard-state (keymap hardware-keycode state group)
  (with-foreign-objects ((keyval :uint) (effective-group :int) (level :int) (modifiers 'modifier-type))
    (if (gdk_keymap_translate_keyboard_state keymap hardware-keycode state group keyval effective-group level modifiers)
        (values (mem-ref keyval :uint)
                (mem-ref effective-group :int)
                (mem-ref level :int)
                (mem-ref modifiers 'modifier-type)))))

(export 'keymap-translate-keyboard-state)

(defcfun gdk_keymap_get_entries_for_keyval :boolean
  (keymap (g-object keymap))
  (keyval :uint)
  (keys (:pointer (:pointer keymap-key-cstruct)))
  (n-keys (:pointer :int)))

(defun keymap-get-entries-for-keyval (keymap keyval)
  (with-foreign-objects ((keys :pointer) (n-keys :int))
    (when (gdk_keymap_get_entries_for_keyval keymap keyval keys n-keys)
      (let ((keys (mem-ref keys :pointer))
            (n-keys (mem-ref n-keys :int)))
        (prog1
            (iter (for i from 0 below n-keys)
                  (for keymap-key = (convert-from-foreign (inc-pointer keys (* i (foreign-type-size 'keymap-key-cstruct)))
                                                          '(g-boxed-foreign keymap-key)))
                  (collect keymap-key))
          (glib:g-free keys))))))

(export 'keymap-get-entries-for-keyval)

(defcfun gdk_keymap_get_entries_for_keycode :boolean
  (keymap (g-object keymap))
  (hardware-keycode :uint)
  (keys (:pointer (:pointer keymap-key-cstruct)))
  (keyvals (:pointer (:pointer :uint)))
  (n-entries (:pointer :int)))

(defun keymap-get-entries-for-keymap (keymap hardware-keycode)
  (with-foreign-objects ((keys :pointer) (keyvals :pointer) (n-keys :int))
    (when (gdk_keymap_get_entries_for_keycode keymap hardware-keycode keys keyvals n-keys)
      (let ((keys (mem-ref keys :pointer))
            (keyvals (mem-ref keyvals :pointer))
            (n-keys (mem-ref n-keys :int)))
        (prog1
            (iter (for i from 0 below n-keys)
                  (for keyval = (mem-aref keyvals :uint))
                  (for keymap-key = (convert-from-foreign (inc-pointer keys (* i (foreign-type-size 'keymap-key-cstruct)))
                                                          '(g-boxed-foreign keymap-key)))
                  (collect keymap-key into r-keys)
                  (collect keyval into r-keyvals)
                  (finally (return (values r-keys r-keyvals))))
          (glib:g-free keys)
          (glib:g-free keyvals))))))

(export 'keymap-get-entries-for-keymap)

(defcfun (keyval-name "gdk_keyval_name") (:string :free-from-foreign nil)
  (keyval :uint))

(export 'keyval-name)

(defcfun (keyval-from-name "gdk_keyval_from_name") :uint
  (keyval-name :string))

(export 'keyval-from-name)

(defcfun (keyval-to-upper "gdk_keyval_to_upper") :uint
  (keyval :uint))

(export 'keyval-to-upper)

(defcfun (keyval-to-lower "gdk_keyval_to_lower") :uint
  (keyval :uint))

(export 'keyval-to-lower)

(defcfun (keyval-is-upper "gdk_keyval_is_upper") :boolean
  (keyval :uint))

(export 'keyval-is-upper)

(defcfun (keyval-is-lower "gdk_keyval_is_lower") :boolean
  (keyval :uint))

;; ignored:
;; void                gdk_keyval_convert_case             (guint symbol,
;;                                                          guint *lower,
;;                                                          guint *upper);

(define-foreign-type unichar ()
  ()
  (:actual-type :uint32)
  (:simple-parser unichar))

(defmethod translate-from-foreign (value (type unichar))
  (code-char value))

(defmethod translate-to-foreign (value (type unichar))
  (char-code value))

(defcfun (keyval-to-char "gdk_keyval_to_unicode") unichar
  (keyval :uint))

(export 'keyval-to-char)

(defcfun (char-to-keyval "gdk_unicode_to_keyval") :uint
  (char unichar))

(export 'char-to-keyval)