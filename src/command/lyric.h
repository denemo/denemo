/* lyric.h
 *  
 * (c)  2002-2005 Adam Tee, 2009 Richard Shann 
 */

#ifndef LYRIC_H

#define LYRIC_H

#include <denemo/denemo.h>
void add_verse (GtkAction * action, DenemoScriptParam * param);
void delete_verse (GtkAction * action, DenemoScriptParam * param);
void reset_lyrics (DenemoStaff * staff, gint count);
gchar *get_text_from_view (GtkWidget * textview);
GtkWidget *add_verse_to_staff (DenemoMovement * si, DenemoStaff * staff);
gchar *next_syllable (void);
void install_lyrics_preview (DenemoMovement * si, GtkWidget * top_vbox);
void hide_lyrics (void);
void show_lyrics (void);
gboolean lyric_change (GtkTextBuffer * buffer);
void select_lyrics (void);
gchar *get_lyrics_for_current_verse (DenemoStaff * thestaff);
gboolean put_lyrics_for_current_verse (DenemoStaff * staff, gchar * text);
gchar *get_lyrics_for_verse_num (gint number);
gboolean append_lyrics_for_current_verse (DenemoStaff * thestaff, gchar * text);
#endif
