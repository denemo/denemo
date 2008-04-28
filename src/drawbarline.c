/**
 * drawbarline.cpp
 * Routines to draw barlines
 *
 * (c) Adam Tee 2002-2005
 */
#include "drawingprims.h"
#include "utils.h"
#include "gcs.h"

/**
 * Draw a given barline on the score given the type
 *
 */
void
drawbarline (GdkPixmap * pixmap, GdkGC * gc, gint xx, gint top_y, gint y,
	     gint type)
{

  if (type == ORDINARY_BARLINE)
    {
      g_print ("Ordinary Co-ords (%d,%d) - (%d,%d) ", xx, top_y, xx, y);
      gdk_draw_line (pixmap, gc, xx, top_y, xx, y);
    }
  else if (type == DOUBLE_BARLINE)
    {
      gdk_draw_line (pixmap, gc, xx, top_y, xx, y);
      gdk_draw_line (pixmap, gc, xx + 5, top_y, xx + 5, y);
    }
  else if (type == END_BARLINE)
    {
      gdk_draw_line (pixmap, gc, xx - 3, top_y, xx - 3, y);
      gdk_draw_rectangle (pixmap, gc, TRUE, xx, top_y, 4, y);
    }


}
