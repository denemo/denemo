/* drawselection.cpp
 * draws a blue bounding box around the currently selected music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "display/drawingprims.h"
#include "core/utils.h"

/**
 * Draw rectangle around the current selection
 *
 */
void
draw_selection (cairo_t * cr, gint x1, gint y1, gint x2, gint y2)
{
  cairo_rectangle (cr, x1 - 5, y1 - 20, x2 - x1, y2 - y1 + 40);
  cairo_stroke (cr);
}
