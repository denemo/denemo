/**
 * drawbarline.cpp
 * Routines to draw barlines
 *
 * (c) Adam Tee 2002-2005
 */
#include "display/drawingprims.h"
#include "core/utils.h"

/**
 * Draw a given barline on the score given the type
 *
 */
void
drawbarline (cairo_t * cr, gint xx, gint top_y, gint y, gint type)
{
  if (type == ORDINARY_BARLINE)
    {
      g_debug ("Ordinary Co-ords (%d,%d) - (%d,%d) ", xx, top_y, xx, y);
      //gdk_draw_line (pixmap, gc, xx, top_y, xx, y);
      cairo_move_to (cr, xx, top_y);
      cairo_line_to (cr, xx + 10, y);
      cairo_stroke (cr);
    }
  else if (type == DOUBLE_BARLINE)
    {
      cairo_move_to (cr, xx, top_y);
      cairo_line_to (cr, xx, y);
      cairo_move_to (cr, xx + 5, top_y);
      cairo_line_to (cr, xx + 5, y);
      cairo_stroke (cr);
      //gdk_draw_line (pixmap, gc, xx, top_y, xx, y);
      //gdk_draw_line (pixmap, gc, xx + 5, top_y, xx + 5, y);
    }
  else if (type == END_BARLINE)
    {
      cairo_move_to (cr, xx - 3, top_y);
      cairo_line_to (cr, xx - 3, y);
      cairo_stroke (cr);

      cairo_rectangle (cr, xx, top_y, 4, y);
      cairo_fill (cr);

      //gdk_draw_line (pixmap, gc, xx - 3, top_y, xx - 3, y);
      //gdk_draw_rectangle (pixmap, gc, TRUE, xx, top_y, 4, y);
    }
}
