/* drawtimesig.cpp
 *
 * Function for drawing the time signature
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "utils.h"		/* Includes <gdk.h> */

/**
 * Draw timesig on the score
 *
 */
void
draw_timesig (cairo_t *cr, gint xx, gint y,
	      gint time1, gint time2)
{
  static GString *timesigtop;
  static GString *timesigbottom;

  if (!timesigtop)
    {
      timesigtop = g_string_new (NULL);
      timesigbottom = g_string_new (NULL);
    }
  g_string_sprintf (timesigtop, "%d", time1);
  g_string_sprintf (timesigbottom, "%d", time2);


  gint extra = LINE_SPACE / 2;
  cairo_select_font_face( cr, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL );
  cairo_set_font_size( cr, 24.0 );

  cairo_move_to( cr, xx,y+20 );
  cairo_show_text( cr, timesigtop->str );

  cairo_move_to( cr, xx,y+40 );
  cairo_show_text( cr, timesigbottom->str );
}
