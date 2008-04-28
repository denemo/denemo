/* drawstemdir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw stemming directive
 *
 */
void
draw_stem_directive (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
		     gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));;
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);

  gchar *text = NULL;

  switch (((stemdirective *) theobj->object)->type)
    {
    case DENEMO_STEMUP:
      text = _("stemup");
      break;
    case DENEMO_STEMBOTH:
      text = _("stemneutral");
      break;
    case DENEMO_STEMDOWN:
      text = _("stemdown");
      break;
    }
  pango_layout_set_text (layout, text, -1);
  pango_layout_set_font_description (layout, desc);
  gdk_draw_layout (pixmap, gc, xx, y - 4, layout);
  pango_font_description_free (desc);
}
