(in-package :gtk)

void                gtk_entry_append_text               (GtkEntry *entry,
                                                         const gchar *text);
void                gtk_entry_prepend_text              (GtkEntry *entry,
                                                         const gchar *text);
void                gtk_entry_set_position              (GtkEntry *entry,
                                                         gint position);
void                gtk_entry_select_region             (GtkEntry *entry,
                                                         gint start,
                                                         gint end);
gint                gtk_entry_layout_index_to_text_index
                                                        (GtkEntry *entry,
                                                         gint layout_index);
gint                gtk_entry_text_index_to_layout_index
                                                        (GtkEntry *entry,
                                                         gint text_index);
void                gtk_entry_set_completion            (GtkEntry *entry,
                                                         GtkEntryCompletion *completion);
GtkEntryCompletion* gtk_entry_get_completion            (GtkEntry *entry);
void                gtk_entry_set_cursor_hadjustment    (GtkEntry *entry,
                                                         GtkAdjustment *adjustment);
GtkAdjustment*      gtk_entry_get_cursor_hadjustment    (GtkEntry *entry);
