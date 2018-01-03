/* drawlilydir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Adam Tee, 2008, 2009 Richard Shann
 */

#include "core/utils.h"         /* Includes <gdk.h> */
#include "command/lilydirectives.h"
#include "command/scorelayout.h"
#include <denemo/denemo.h>
#include <string.h>
#include <math.h>
#include <stdlib.h>
/**
 * Draw a standalone Denemo directive on the score as a vertical bar or graphic and/or text
 *
 */
void
draw_lily_dir (cairo_t * cr, gint xx, gint y, gint highy, gint lowy, DenemoObject * theobj, gboolean selected, gboolean at_cursor)
{
  DenemoDirective *directive = ((lilydirective *) theobj->object);
  gchar *first = (directive->postfix && directive->postfix->len) ? directive->postfix->str : " ";
  guint layout = selected_layout_id ();
  gdouble only = (directive->layouts && !wrong_layout (directive, layout)) ? 0.5 : 0.0;
  gdouble exclude = (directive->layouts && wrong_layout (directive, layout)) ? 0.9 : 0.0;
  //if (lily->y && lily->y != layout)
  //  exclude = 0.9;
  cairo_save (cr);

  selected ? cairo_set_source_rgba (cr, 0.0, 0.0, 1.0, at_cursor ? 1.0 : 0.5) : directive->graphic ? cairo_set_source_rgb (cr, 0.0 + exclude, 0.0 + only, 0.0) : cairo_set_source_rgba (cr, 0.4 + exclude, 0.5 + only, 0.4, at_cursor ? 1.0 : 0.5);
  if (directive->graphic)
    {
      //FIXME there may be scripts expecting a different positioning code
      gdouble gx = xx + directive->gx - (((DenemoGraphic *) directive->graphic)->width) / 2;
      gdouble gy = y + MID_STAFF_HEIGHT + directive->gy - (((DenemoGraphic *) directive->graphic)->height) / 2;

      drawbitmapinverse_cr (cr, (DenemoGraphic *) directive->graphic, gx, gy, FALSE);
    }
 if ((directive->graphic==NULL) || 
        (!(directive->override&DENEMO_OVERRIDE_GRAPHIC)) || 
        ((directive->minpixels/2 - abs(directive->gx)) < 0)) //show position of standalone directive unless override graphic is set or graphic is displaced too far from object position
  {
    at_cursor ? cairo_set_source_rgba (cr, 0.2 + exclude, 0.3 + only, 0.8, 0.5) : cairo_set_source_rgba (cr, 0.4 + exclude, 0.5 + only, 0.4, 0.5);


    cairo_rectangle (cr, xx + 10, y - 20, 2, STAFF_HEIGHT + 26);
    cairo_arc (cr, xx + 10 + 1.5, y - 20, 6, 0.0, 2 * M_PI);
    cairo_fill (cr);
    cairo_move_to (cr, xx + 10, y - 20);
    cairo_line_to (cr, xx + 10 + directive->gx, y + MID_STAFF_HEIGHT + directive->gy);
    cairo_stroke (cr);
  }
  if (directive->display)
    {                           //store display position x,y as well
#define MAXLEN (4)
      gchar c = 0;              //if it is a long string only show it all when cursor is on it, also only display from first line
      gchar *p;
      for (p = directive->display->str; *p; p = g_utf8_next_char (p))
        {
          if (*p == '\n' || (!at_cursor && (p - directive->display->str) > MAXLEN))
            {
              c = *p;
              *p = 0;
              break;
            }
        }
      if (at_cursor)
        cairo_set_source_rgba (cr, exclude, only, 0.0, 1.0), drawlargetext_cr (cr, directive->display->str, xx + directive->tx, y + lowy + directive->ty - 8);
      else
        drawnormaltext_cr (cr, directive->display->str, xx + directive->tx, y + lowy + directive->ty - 8);
      if (c)
        {
          *p = c;
        }
    }
  else
    //FIXME do this by creating a display field
  if ((!directive->graphic) && (*first == '%' || *first == '^' || *first == '_'))
    {                           //display comments, and markup above and below
      if (at_cursor)
        cairo_set_source_rgba (cr, exclude, only, 0.0, 1.0), drawlargetext_cr (cr, first + 1, xx, *first == '_' ? y + lowy + 20 : y - highy - 20);
      else
        drawnormaltext_cr (cr, first + 1, xx, *first == '_' ? y + lowy + 20 : y - highy - 20);
    }
  cairo_restore (cr);
}
