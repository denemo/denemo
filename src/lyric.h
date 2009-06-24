/* lyric.h
 *  
 * (c)  2002-2005 Adam Tee 
 */

#ifndef LYRIC_H

#define LYRIC_H

#include <denemo/denemo.h>
void add_verse(GtkAction *action, gpointer param);
void reset_lyrics(DenemoStaff *staff, gint count);
gchar *get_text_from_view(GtkWidget *textview);
GtkWidget *add_verse_to_staff(DenemoScore *si, DenemoStaff *staff);
gchar *next_syllable(gint count);
void install_lyrics_preview(DenemoScore *si, GtkWidget *top_vbox);
void hide_lyrics(void);
void show_lyrics(void);
gboolean lyric_change(GtkTextBuffer *buffer);
void select_lyrics(void);
#endif
