(in-package :gtk)

(defcfun (%gtk-color-selection-get-previous-color "gtk_color_selection_get_previous_color") :void
  (color-selection (g-object color-selection))
  (color (g-boxed-foreign color)))

(defun gtk-color-selection-get-previous-color (color-selection)
  (let ((color (make-color)))
    (%gtk-color-selection-get-previous-color color-selection color)
    color))

(defcfun gtk-color-selection-set-previous-color :void
  (color-selection (g-object color-selection))
  (color (g-boxed-foreign color)))

(defcfun (color-selection-adjusting-p "gtk_color_selection_is_adjusting") :boolean
  (color-selection g-object))

(export 'color-selection-adjusting-p)

(defcfun gtk-color-selection-palette-from-string :boolean
  (str :string)
  (colors :pointer)
  (n-colors :pointer))

(defun color-selection-palette-from-string (str)
  (with-foreign-objects ((colors :pointer) (n-colors :int))
    (when (gtk-color-selection-palette-from-string str colors n-colors)
      (iter (with colors-ar = (mem-ref colors :pointer))
            (for i from 0 below (mem-ref n-colors :int))
            (for color-ptr =
                 (inc-pointer colors-ar
                              (* i (foreign-type-size 'gdk::color-cstruct))))
            (for color = (convert-from-foreign color-ptr '(g-boxed-foreign color)))
            (collect color)
            (finally (g-free colors-ar))))))

(defcfun gtk-color-selection-palette-to-string (g-string :free-from-foreign t)
  (colors :pointer)
  (n-colors :int))

; TODO: gtk_color_selection_palette_to_string

; TODO: gtk_color_selection_set_change_palette_with_screen_hook

(defcfun (file-chooser-select-filename "gtk_file_chooser_select_filename") :boolean
  (file-chooser g-object)
  (filename :string))

(export 'file-chooser-select-filename)

(defcfun (file-chooser-unselect-filename "gtk_file_chooser_unselect_filename") :void
  (file-chooser g-object)
  (filename :string))

(export 'file-chooser-unselect-filename)

(defcfun (file-chooser-select-all "gtk_file_chooser_select_all") :void
  (file-chooser g-object))

(export 'file-chooser-select-all)

(defcfun (file-chooser-unselect-all "gtk_file_chooser_unselect_all") :void
  (file-chooser g-object))

(export 'file-chooser-unselect-all)

(defcfun (file-chooser-filenames "gtk_file_chooser_get_filenames") (gslist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-filenames)

(defcfun (file-chooser-select-uri "gtk_file_chooser_select_uri") :boolean
  (file-chooser g-object)
  (uri :string))

(export 'file-chooser-select-uri)

(defcfun (file-chooser-unselect-uri "gtk_file_chooser_unselect_uri") :void
  (file-chooser g-object)
  (uri :string))

(export 'file-chooser-unselect-uri)

(defcfun (file-chooser-uris "gtk_file_chooser_get_uris") (gslist (g-string :free-from-foreign t) :free-from-foreign t)
  (file-chooser g-object))

(export 'file-chooser-uris)

(defcfun (file-chooser-add-filter "gtk_file_chooser_add_filter") :void
  (chooser (g-object file-chooser))
  (filter (g-object file-filter)))

(export 'file-chooser-add-filter)

(defcfun (file-chooser-remove-filter "gtk_file_chooser_remove_filter") :void
  (chooser (g-object file-chooser))
  (filter (g-object file-filter)))

(export 'file-chooser-remove-filter)

(defcfun (file-chooser-filters "gtk_file_chooser_list_filters") (glist (g-string :free-from-foreign t) :free-from-foreign t)
  (chooser (g-object file-chooser)))

(export 'file-chooser-filters)

(defcfun gtk-file-chooser-add-shortcut-folder :boolean
  (file-chooser g-object)
  (folder :string)
  (error :pointer))

(defun file-chooser-add-shortcut-folder (file-chooser folder)
  (gtk-file-chooser-add-shortcut-folder file-chooser folder (null-pointer)))

(export 'file-chooser-add-shortcut-folder)

(defcfun gtk-file-chooser-remove-shortcut-folder :boolean
  (file-chooser g-object)
  (folder :string)
  (error :pointer))

(defun file-chooser-remove-shortcut-folder (file-chooser folder)
  (gtk-file-chooser-remove-shortcut-folder file-chooser folder (null-pointer)))

(export 'file-chooser-remove-shortcut-folder)

(defcfun (file-chooser-shortcut-folders "gtk_file_chooser_list_shortcut_folders") (gslist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-shortcut-folders)

(defcfun gtk-file-chooser-add-shortcut-folder-uri :boolean
  (file-chooser g-object)
  (folder-uri :string)
  (error :pointer))

(defun file-chooser-add-shortcut-folder-uri (file-chooser folder-uri)
  (gtk-file-chooser-add-shortcut-folder-uri file-chooser folder-uri (null-pointer)))

(export 'file-chooser-add-shortcut-folder-uri)

(defcfun gtk-file-chooser-remove-shortcut-folder-uri :boolean
  (file-chooser g-object)
  (folder-uri :string)
  (error :pointer))

(defun file-chooser-remove-shortcut-folder-uri (file-chooser folder-uri)
  (gtk-file-chooser-remove-shortcut-folder-uri file-chooser folder-uri (null-pointer)))

(export 'file-chooser-remove-shortcut-folder-uri)

(defcfun (file-chooser-shortcut-folder-uris "gtk_file_chooser_list_shortcut_folder_uris") (gslist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-shortcut-folder-uris)

(defcfun (file-chooser-get-current-folder-file "gtk_file_chooser_get_current_folder_file") g-object
  (chooser (g-object file-chooser)))

(defcfun gtk-file-chooser-set-current-folder-file :boolean
  (file-chooser (g-object file-chooser))
  (file g-object)
  (error :pointer))

(defun file-chooser-set-current-folder-file (file-chooser file)
  (gtk-file-chooser-set-current-folder-file file-chooser file (null-pointer)))

(export '(file-chooser-get-current-folder-file file-chooser-set-current-folder-file))

(defcfun (file-chooser-get-file "gtk_file_chooser_get_file") g-object
  (file-chooser (g-object file-chooser)))

(defcfun gtk-file-chooser-set-file g-object
  (file-chooser (g-object file-chooser))
  (file g-object)
  (error :pointer))

(defun file-chooser-set-file (file-chooser file)
  (gtk-file-chooser-set-file file-chooser file (null-pointer)))

(export '(file-chooser-get-file file-chooser-set-file))

(defcfun (file-chooser-unselect-file "gtk_file_chooser_unselect_file") :void
  (file-chooser (g-object file-chooser))
  (file g-object))

(export 'file-chooser-unselect-file)

(defcfun (file-filter-add-pattern "gtk_file_filter_add_pattern") :void
  (file-filter g-object)
  (pattern :string))

(export 'file-filter-add-pattern)

(defcfun (file-filter-add-pixbuf-formats "gtk_file_filter_add_pixbuf_formats") :void
  (file-filter g-object))

(export 'file-filter-add-pixbuf-formats)

; TODO: gtk_file_filter_add_custom

; TODO: gtk_file_filter_get_needed

; TODO: gtk_file_filter_filter

(defcfun (font-selection-face "gtk_font_selection_get_face") g-object
  (font-selection g-object))

(export 'font-selection-face)

(defcfun (font-selection-face-list "gtk_font_selection_get_face_list") g-object
  (font-selection g-object))

(export 'font-selection-face-list)

(defcfun (font-selection-family "gtk_font_selection_get_family") g-object
  (font-selection g-object))

(export 'font-selection-family)

(defcfun (font-selection-size "gtk_font_selection_get_size") :int
  (font-selection g-object))

(export 'font-selection-size)

(defcfun (font-selection-family-list "gtk_font_selection_get_family_list") g-object
  (font-selection g-object))

(export 'font-selection-family-list)

(defcfun (font-selection-preview-entry "gtk_font_selection_get_preview_entry") g-object
  (font-selection g-object))

(export 'font-selection-preview-entry)

(defcfun (font-selection-size-entry "gtk_font_selection_get_size_entry") g-object
  (font-selection g-object))

(export 'font-selection-size-entry)

(defcfun (font-selection-size-list "gtk_font_selection_get_size_list") g-object
  (font-selection g-object))

(export 'font-selection-size-list)