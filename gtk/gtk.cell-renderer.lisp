(in-package :gtk)

; TODO: GtkCellEditable

; TODO: GtkCellRenderer vtable

; TODO: gtk_cell_renderer_get_size

; TODO: gtk_cell_renderer_render

; TODO: gtk_cell_renderer_activate

; TODO: gtk_cell_renderer_start_editing

; TODO: gtk_cell_renderer_stop_editing

(defcfun gtk-cell-renderer-get-fixed-size :void
  (cell (g-object cell-renderer))
  (width (:pointer :int))
  (height (:pointer :int)))

(defun cell-renderer-get-fixed-size (cell)
  (with-foreign-objects ((width :int) (height :int))
    (gtk-cell-renderer-get-fixed-size cell width height)
    (values (mem-ref width :int)
            (mem-ref height :int))))

(export 'cell-renderer-get-fixed-size)

(defcfun (cell-renderer-set-fixed-size "gtk_cell_renderer_set_fixed_size") :void
  (cell (g-object cell-renderer))
  (width :int)
  (height :int))

(export 'cell-renderer-set-fixed-size)

; TODO: GtkCellRendererAccel

; TODO: GtkCellRendererCombo

; TODO: GtkCellRendererPixbuf

; TODO: GtkCellRendererProgress

; TODO: GtkCellRendererSpin

; TODO: GtkCellRendererText

; TODO: GtkCellRendererToggle

(defcfun (cell-renderer-text-set-fixed-height-from-font "gtk_cell_renderer_text_set_fixed_height_from_font") :void
  (renderer (g-object cell-renderer-text))
  (number-of-rows :int))

(export 'cell-renderer-text-set-fixed-height-from-font)
