/* drawfigure.cpp 
 *
 * function to display Figured Bass figures on score
 * for Denemo, a gtk+ frontend to GNU Lilypond 
 * 
 * (c) 2003-2005 Richard Shann <richard.shann@virgin.net>
 */

#include "utils.h"
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw figured bass on the score
 *
 */
void
draw_figure (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	     gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context;
  PangoLayout *layout;
  PangoFontDescription *desc;
  gchar *text = NULL;
  gint length = 0;
  chord *ch;
  if (theobj->type == FIGURE)
    {
      g_warning ("FIGURE type found and not handled");
    }
  else if (theobj->type == CHORD)
    {
      ch = (chord *) theobj->object;
      if (ch->is_figure)
	{
	  text = ((GString *) (ch->figure))->str;
	  length = ((GString *) (ch->figure))->len;
	}
      else
	{
	  DenemoObject *mud = (DenemoObject *) ((GList *) (ch->figure))->data;
	  chord *mych = (chord *) mud->object;
	  GString *mygstr = (GString *) mych->figure;
	  text = mygstr->str;
	  length = mygstr->len;
	}

    }
#ifdef DEBUG
  g_print ("%s, %d\n", text, length);
#endif

  context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  layout = pango_layout_new (context);
  pango_layout_set_text (layout, text, -1);
  desc = pango_font_description_from_string (FONT);
  pango_layout_set_font_description (layout, desc);
  pango_font_description_free (desc);
  //gdk_draw_text (pixmap, lyricfont, gc, xx, y+STAFF_HEIGHT+10, text, length);
  gdk_draw_layout (pixmap, gc, xx, y + STAFF_HEIGHT + 10, layout);


}


