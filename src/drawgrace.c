/* drawgrace.cpp
 *
 * Functions for drawing tuplet indications
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw grace note on the screen
 *
 */
void
draw_gracebracket (cairo_t *cr,
		   gint xx, gint y, DenemoObject * theobj)
{
  char *text="";

  if (theobj->type == GRACE_START)
    {
      text = _("\\");
    }
  else if (theobj->type == GRACE_END)
    {
      text = _("'");
    }

  drawnormaltext_cr( cr, text, xx, y - 4 );
}
