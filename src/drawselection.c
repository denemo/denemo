/* drawselection.cpp
 * draws a blue bounding box around the currently selected music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "drawingprims.h"
#include "utils.h"

/**
 * Draw rectangle around the current selection
 *
 */
void
draw_selection (GdkPixmap * pixmap, GdkGC * gc, gint x1, gint y1,
		gint x2, gint y2)
{
  gdk_draw_rectangle (pixmap, gc, FALSE, x1, y1, x2 - x1, y2 - y1);
}
