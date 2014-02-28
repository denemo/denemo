/* drawdynamic.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee
 */

#include "core/utils.h"              /* Includes <gdk.h> */
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw the given dynamic on the score
 */
void
draw_dynamic (cairo_t * cr, gint xx, gint y, DenemoObject * theobj)
{
  gint extra = 10;
  GString *tmp = NULL;

  tmp = (GString *) ((chord *) theobj->object)->dynamics->data;
  drawnormaltext_cr (cr, tmp->str, xx, y + (2 * STAFF_HEIGHT) + extra);
}
