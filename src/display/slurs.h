/* slurs.h

   Functions for drawing slurs - header file

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005 Matthew Hiller
*/

#include <glib.h>

GSList *push_slur_stack (GSList * slur_stack, gint x, gint y);




void draw_slur (cairo_t * cr, GSList ** slur_stack, gint x2, gint y, gint y2);
void draw_slur_start (cairo_t * cr, gint x, gint y);
void draw_slur_end (cairo_t * cr, gint x, gint y);
