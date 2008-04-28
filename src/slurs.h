/* slurs.h

   Functions for drawing slurs - header file

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005 Matthew Hiller
*/
   
#include <glib.h>

GSList *
push_slur_stack (GSList *slur_stack, gint x);

gint
top_slur_stack (GSList *slur_stack);

GSList
pop_slur_stack (GSList *slur_stack);

void
draw_slur (GdkPixmap *pixmap, GdkGC *gc, GSList **slur_stack,
	   gint x2, gint y);
