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
draw_figure (cairo_t *cr,
	     gint xx, gint y, DenemoObject * theobj)
{
  gchar *text = NULL;
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
	}
      else
	{
	  DenemoObject *mud = (DenemoObject *) ((GList *) (ch->figure))->data;
	  chord *mych = (chord *) mud->object;
	  GString *mygstr = (GString *) mych->figure;
	  text = mygstr->str;
	}

    }
  g_debug ("%s\n", text);

  drawnormaltext_cr( cr, text, xx, y + STAFF_HEIGHT + 10 );
}


