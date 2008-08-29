/* lyric.h
 *  
 * (c)  2002-2005 Adam Tee 
 */

#ifndef LYRIC_H

#define LYRIC_H

#include <denemo/denemo.h>

void insertlyric(gpointer data);
void lyric_insert(GtkAction *action, gpointer param);
DenemoObject *
newlyric (gint baseduration, gint numdots, gchar *lys);

#endif
