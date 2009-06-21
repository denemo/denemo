/* drawlyric.cpp 
 *
 * function to display Lyrics on score
 * for Denemo, a gtk+ frontend to GNU Lilypond 
 * 
 * (c) 2002 Adam Tee <eenajt@leeds.ac.uk>
 */

#include "utils.h"
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw lyrics on the score
 *
 */
void
draw_lyric (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	    gint xx, gint y, gchar *text)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));;
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);

  pango_layout_set_text (layout, text, -1);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  gdk_draw_layout (pixmap, gc, xx, y + STAFF_HEIGHT + 10, layout);

}
