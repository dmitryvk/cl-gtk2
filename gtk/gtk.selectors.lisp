(in-package :gtk)

(defcfun (color-selection-adjusting-p "gtk_color_selection_is_adjusting") :boolean
  (color-selection g-object))

(export 'color-selection-adjusting-p)

; TODO: gtk_color_selection_palette_from_string

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

(defcfun (file-chooser-uris "gtk_file_chooser_get_uris") (gslist (g-string :free-from-foreign t))
  (file-chooser g-object))

(export 'file-chooser-uris)

; TODO: gtk_file_chooser_add_filter
; --- ownership issues

; TODO: gtk_file_chooser_remove_filter

; TODO: gtk_file_chooser_list_filters

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

; TODO: gtk_file_chooser_get_current_folder_file

; TODO: gtk_file_chooser_get_file

; TODO: gtk_file_chooser_get_files

; TODO: gtk_file_chooser_get_preview_file

; TODO: gtk_file_chooser_select_file

; TODO: gtk_file_chooser_set_current_folder_file

; TODO: gtk_file_chooser_set_file

; TODO: gtk_file_chooser_unselect_file

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