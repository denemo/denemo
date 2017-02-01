/* lyric.h
 *
 * (c)  2002-2005 Adam Tee, 2009 Richard Shann
 */

#ifndef LYRIC_H

#define LYRIC_H

#include <denemo/denemo.h>
void add_verse (DenemoAction * action, DenemoScriptParam * param);
void delete_verse (DenemoAction * action, DenemoScriptParam * param);
void reset_lyrics (DenemoStaff * staff, gint count);
gchar *get_text_from_view (GtkWidget * textview);
guint add_verse_to_staff (DenemoMovement * si, DenemoStaff * staff);
gchar *next_syllable (void);
void install_lyrics_preview (DenemoMovement * si, GtkWidget * top_vbox);
void hide_lyrics (void);
void show_lyrics (void);
gboolean lyric_changed_cb (GtkTextBuffer * buffer);
void select_lyrics (void);
gchar *get_lyrics_for_current_verse (DenemoStaff * thestaff);
gboolean put_lyrics_for_current_verse (DenemoStaff * staff, gchar * text);
gchar *get_lyrics_for_verse_num (gint number);
gboolean append_lyrics_for_current_verse (DenemoStaff * thestaff, gchar * text);
gboolean synchronize_lyric_cursor (gint offset);
GtkWidget *verse_get_current_view (DenemoStaff * staff);
gboolean verse_set_current (DenemoStaff * staff, gint id);
gint verse_get_current (DenemoStaff * staff);
void verse_set_current_text (DenemoStaff * staff, gchar * text);
gchar *verse_get_current_text (DenemoStaff * staff);
gboolean insert_text_in_verse (gchar * text);
gint syllable_count (void);
#endif
