/* drawtuplets.cpp
 *
 * Functions for drawing tuplet indications
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw tuplet directive on the score
 *
 */
void
draw_tupbracket (cairo_t *cr,
		 gint xx, gint y, DenemoObject * theobj)
{
  static GString *tupopentext=NULL;
  if (!tupopentext)
    tupopentext = g_string_new (NULL);
  if(((tuplet *) theobj->object)->directives)
    draw_for_directives(cr, ((tuplet *) theobj->object)->directives, xx, y-4);
  else {

    if (theobj->type == TUPOPEN) {
      g_string_sprintf (tupopentext,
			"~%d", 	((tupopen *) theobj->object)->denominator);
      drawnormaltext_cr( cr, tupopentext->str, xx-4, y - 4);
    }
    else
      drawnormaltext_cr( cr, "|", xx+4, y - 4);
  }
}
