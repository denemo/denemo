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
		 gint xx, gint y, DenemoObject * theobj, gint start)
{
#define BRACKET_HEIGHT (20)
#define GAP (10)
  static GString *tupopentext=NULL;
  if (!tupopentext)
    tupopentext = g_string_new (NULL);
  if(((tuplet *) theobj->object)->directives)
    draw_for_directives(cr, ((tuplet *) theobj->object)->directives, xx, y-4);
  else {
    if (theobj->type == TUPOPEN) {
      g_string_sprintf (tupopentext,
			"%d", ((tupopen *) theobj->object)->denominator);
      drawnormaltext_cr( cr, "|", xx-4, y - BRACKET_HEIGHT + 12);
      cairo_rectangle (cr, xx, y - BRACKET_HEIGHT, 10, 1);
    }
    else {
      drawnormaltext_cr( cr, "|", xx+4, y - BRACKET_HEIGHT + 12);
      if(start){
        cairo_rectangle (cr, start-2, y - BRACKET_HEIGHT, (xx - start)/2 - GAP + 2, 2);
        drawnormaltext_cr(cr, tupopentext->str, (xx + start)/2, y - BRACKET_HEIGHT + 4);
        cairo_rectangle (cr, 2*GAP + (xx + start)/2, y - BRACKET_HEIGHT, (xx - start)/2 - GAP-3, 2);
        cairo_fill(cr);
      }
    }
  }
}
