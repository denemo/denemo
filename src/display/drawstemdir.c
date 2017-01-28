/* drawstemdir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "core/utils.h"              /* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw stemming directive
 *
 */
void
draw_stem_directive (cairo_t * cr, gint xx, gint y, DenemoObject * theobj)
{

  gchar *text = NULL;

  switch (((stemdirective *) theobj->object)->type)
    {
    case DENEMO_STEMUP:
      text = _("⬆stem");
      break;
    case DENEMO_STEMBOTH:
      text = _("⬆⬇stems");
      break;
    case DENEMO_STEMDOWN:
      text = _("⬇stems");
      break;
    }
  if (((stemdirective *) theobj->object)->directives)
    draw_for_directives (cr, ((stemdirective *) theobj->object)->directives, xx, y - 4, TRUE);
  else
    drawnormaltext_cr (cr, text, xx, y - 4);
}
