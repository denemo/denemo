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
	    gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));;
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);

  gchar *text = NULL;
  gint length = 0;
  if (theobj->type == LYRIC)
    {
      text = ((lyric *) theobj->object)->lyrics->str;
      length = ((lyric *) theobj->object)->lyrics->len;
    }
  else if (theobj->type == CHORD)
    {
      text = ((chord *) theobj->object)->lyric->str;
      length = ((chord *) theobj->object)->lyric->len;
    }
#ifdef DEBUG
  g_print ("%s, %d\n", text, length);
#endif

  pango_layout_set_text (layout, text, -1);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  gdk_draw_layout (pixmap, gc, xx, y + STAFF_HEIGHT + 10, layout);

}
