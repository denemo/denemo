/* drawgrace.cpp
 *
 * Functions for drawing tuplet indications
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw grace note on the screen
 *
 */
void
draw_gracebracket (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
		   gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);


  if (theobj->type == GRACE_START)
    {
      pango_layout_set_text (layout, _("\\"), -1);
    }
  else if (theobj->type == GRACE_END)
    {
      pango_layout_set_text (layout, _("'"), -1);

    }

  pango_layout_set_font_description (layout, desc);
  gdk_draw_layout (pixmap, gc, xx, y - 4, layout);
  pango_font_description_free (desc);
}
