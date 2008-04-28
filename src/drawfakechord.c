#include "utils.h"
#include <denemo/denemo.h>
#include <string.h>

/**
 * Draw fakechords on the score
 *
 */
void
draw_fakechord (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	     gint xx, gint y, DenemoObject * theobj)
{
  PangoContext *context;
  PangoLayout *layout;
  PangoFontDescription *desc;
  gchar *text = NULL;
  GString *temp = g_string_new("");
  gint length = 0;
  chord *ch;
  if (theobj->type == FAKECHORD)
    {
      g_warning ("FAKECHORD type found and not handled");
    }
  else if (theobj->type == CHORD)
    {
      ch = (chord *) theobj->object;
      if (ch->is_fakechord)
	{
	  temp = g_string_append(temp, ((GString *) ch->fakechord)->str);
	  if (ch->fakechord_extension != NULL)
	  	temp = g_string_append(temp, ((GString *) ch->fakechord_extension)->str);
	  text = ((GString *) (temp))->str;
	  //printf("\ntext in draw_fakechod == %s\n",text);
	  length = ((GString *) (temp))->len;
	}
      else
	{
	  DenemoObject *mud = (DenemoObject *) ((GList *) (ch->fakechord))->data;
	  chord *mych = (chord *) mud->object;
	  GString *mygstr = (GString *) mych->fakechord;
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
  gdk_draw_layout (pixmap, gc, xx, y + STAFF_HEIGHT - 10, layout);


}


