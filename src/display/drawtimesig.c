/* drawtimesig.cpp
 *
 * Function for drawing the time signature
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "core/utils.h"              /* Includes <gdk.h> */
#include "command/scorelayout.h"
#include "command/lilydirectives.h"
/**
 * Draw timesig on the score
 *
 */
void
draw_timesig (cairo_t * cr, gint xx, gint y, gint time1, gint time2, timesig * timesig)
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

  gint override = 0;
  if (timesig->directives)
    {
      gint count = 0;
      GList *g = timesig->directives;
      cairo_save (cr);
      for (; g; g = g->next, count++)
        {
          DenemoDirective *directive = g->data;
          guint layout = selected_layout_id ();
		  gdouble only = (directive->layouts && !wrong_layout (directive, layout)) ? 0.5 : 0.0;
		  gdouble exclude = (directive->layouts && wrong_layout (directive, layout)) ? 0.9 : 0.0;
          override = override | directive->override;
		  directive->graphic ? cairo_set_source_rgb (cr, 0.0 + exclude, 0.0 + only, 0.0) : cairo_set_source_rgba (cr, 0.4 + exclude, 0.5 + only, 0.4, 1.0);

          if (directive->display)
            {
              drawnormaltext_cr (cr, directive->display->str, xx + directive->tx, y + count * 10);
            }
          if (directive->graphic)
            {
              drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx + count, y + directive->gy, FALSE);
            }
        }
      cairo_restore (cr);
    }
  if (!(DENEMO_OVERRIDE_GRAPHIC & override))
    {
      cairo_select_font_face (cr, "Sans", CAIRO_FONT_SLANT_NORMAL, CAIRO_FONT_WEIGHT_NORMAL);
      cairo_set_font_size (cr, 24.0);

      cairo_move_to (cr, xx, y + 20);
      cairo_show_text (cr, timesigtop->str);

      cairo_move_to (cr, xx, y + 40);
      cairo_show_text (cr, timesigbottom->str);
    }
}
