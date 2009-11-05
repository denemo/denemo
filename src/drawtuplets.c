/* drawtuplets.cpp
 *
 * Functions for drawing tuplet indications
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw tuplet directive on the score
 *
 */
void
draw_tupbracket (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
		 gint xx, gint y, DenemoObject * theobj)
{
  static GString *tupopentext;
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);
  gint pos;

  if (!tupopentext)
    tupopentext = g_string_new (NULL);


  if (theobj->type == TUPOPEN)
    {
      g_string_sprintf (tupopentext,
			"~%d", 	((tupopen *) theobj->object)->denominator);
      pango_layout_set_text (layout, tupopentext->str, -1);
      pos = xx-4;
    }
  else if (theobj->type == TUPCLOSE)
    {

      pango_layout_set_text (layout, "|", -1);
      pos = xx+4;
    }
  pango_layout_set_font_description (layout, desc);
  gdk_draw_layout (pixmap, gc, pos, y - 4, layout);

  pango_font_description_free (desc);
}
