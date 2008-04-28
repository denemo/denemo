/* drawtimesig.cpp
 *
 * Function for drawing the time signature
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */

/**
 * Draw timesig on the score
 *
 */
void
draw_timesig (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font, gint xx, gint y,
	      gint time1, gint time2)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));;
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc =
    pango_font_description_from_string (TIMESIGFONT);
  static GString *timesigtop;
  static GString *timesigbottom;

  if (!timesigtop)
    {
      timesigtop = g_string_new (NULL);
      timesigbottom = g_string_new (NULL);
    }
  g_string_sprintf (timesigtop, "%d", time1);
  g_string_sprintf (timesigbottom, "%d", time2);


  pango_layout_set_text (layout, timesigtop->str, -1);
  pango_layout_set_font_description (layout, desc);
  gint extra = LINE_SPACE / 2;
  gdk_draw_layout (pixmap, gc, xx, y - extra, layout);	//hard coded LINESPACE*0.5

  pango_layout_set_text (layout, timesigbottom->str, -1);
  pango_layout_set_font_description (layout, desc);
  extra = (3 * LINE_SPACE) / 2;
  gdk_draw_layout (pixmap, gc, xx, y + extra, layout);	//hardcoded LINESPACE * 1.5
  pango_font_description_free (desc);
}
