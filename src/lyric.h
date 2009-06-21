/* lyric.h
 *  
 * (c)  2002-2005 Adam Tee 
 */

#ifndef LYRIC_H

#define LYRIC_H

#include <denemo/denemo.h>
void add_verse(GtkAction *action, gpointer param);
void init_lyrics(DenemoStaff *staff);
gchar *get_text_from_view(GtkWidget *textview);
void add_verse_to_staff(DenemoStaff *staff);
gchar *next_syllable(void);
#endif
