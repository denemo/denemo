/* drawlilydir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw a lilypond directive on the score
 *
 */
void
draw_lily_dir (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	       gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);

  // just use letter 'l' as indicator of lilydirective */
  pango_layout_set_text (layout,
			 "l"/*	 ((lilydirective *) theobj->object)->directive->str*/,
			 -1);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  gdk_draw_layout (pixmap, gc, xx, y - 4, layout);




}
