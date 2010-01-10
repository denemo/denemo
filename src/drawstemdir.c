/* drawstemdir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw stemming directive
 *
 */
void
draw_stem_directive (cairo_t *cr,
		     gint xx, gint y, DenemoObject * theobj)
{

  gchar *text = NULL;

  switch (((stemdirective *) theobj->object)->type)
    {
    case DENEMO_STEMUP:
      text = _("stem\nup");
      break;
    case DENEMO_STEMBOTH:
      text = _("auto\nstems");
      break;
    case DENEMO_STEMDOWN:
      text = _("stem\ndown");
      break;
    }

  drawnormaltext_cr( cr, text, xx, y - 4);
}
