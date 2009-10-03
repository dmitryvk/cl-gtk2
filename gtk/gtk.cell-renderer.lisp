(in-package :gtk)

; TODO: GtkCellEditable

; TODO: GtkCellRenderer vtable

; TODO: gtk_cell_renderer_get_size

; TODO: gtk_cell_renderer_render

; TODO: gtk_cell_renderer_activate

; TODO: gtk_cell_renderer_start_editing

; TODO: gtk_cell_renderer_stop_editing

; TODO: gtk_cell_renderer_get_fixed_size

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
