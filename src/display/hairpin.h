/* hairpin.h

   Functions for drawing hairpins - header file

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000, 2001 Matthew Hiller
*/

#include <glib.h>

GSList *push_hairpin_stack (GSList * hairpin_stack, gint x);

gint top_hairpin_stack (GSList * hairpin_stack);

GSList *pop_hairpin_stack (GSList * hairpin_stack);

void draw_hairpin (cairo_t * cr, GSList ** hairpin_stack, gint x2, gint y, gint dir);
