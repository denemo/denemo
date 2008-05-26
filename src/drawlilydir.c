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
  gchar first = *(((lilydirective *) theobj->object)->directive->str);
  if( first == '%' || first == '^' || first == '_' )//display comments, and markup above and below
    pango_layout_set_text (layout,
			   ((lilydirective *) theobj->object)->directive->str+1,
			   -1);
  // just use letters 'L' as indicator of general lilydirective */
  else
  pango_layout_set_text (layout,
			 "L"/*	 ((lilydirective *) theobj->object)->directive->str*/,
			 -1);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);

  gdk_draw_layout (pixmap, gc, xx, y+(first=='_'?STAFF_HEIGHT+20:-20), layout);




}
