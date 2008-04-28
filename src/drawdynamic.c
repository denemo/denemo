/* drawdynamic.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw the given dynamic on the score
 */
void
draw_dynamic (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	      gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);

  gint extra = 10;
  GString *tmp = NULL;
//  gint tmpy = ((chord *) theobj->object)->lowesty;


  tmp = (GString *) ((chord *) theobj->object)->dynamics->data;
  pango_layout_set_text (layout, tmp->str, -1);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  gdk_draw_layout (pixmap, gc, xx, y + (2 * STAFF_HEIGHT) + extra, layout);
  //gdk_draw_text (pixmap, lily_directive_font, gc, xx,y + (2 * STAFF_HEIGHT) + extra, tmp->str, tmp->len);

}
