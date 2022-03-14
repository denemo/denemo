/* drawlyric.cpp
 *
 * function to display Lyrics on score
 * for Denemo, a gtk+ frontend to GNU Lilypond
 *
 * (c) 2002 Adam Tee <eenajt@leeds.ac.uk>
 */

#include "core/utils.h"
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw lyrics on the score
 *
 */
void
draw_lyric (cairo_t * cr, gint xx, gint y, gchar * text)
{
  drawtext_cr (cr, text, xx, y + STAFF_HEIGHT + 16, 14.0);
}
