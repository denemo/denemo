/* drawtuplets.cpp
 *
 * Functions for drawing tuplet indications
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller
 */

#include "core/utils.h"              /* Includes <gdk.h> */
#include <denemo/denemo.h>


/**
 * Draw tuplet directive on the score
 *
 */
void
draw_tupbracket (cairo_t * cr, gint xx, gint y, DenemoObject * theobj, gint start)
{
#define BRACKET_HEIGHT (20)
#define GAP (10)
  static GString *tupopentext = NULL;
  if (!tupopentext)
    tupopentext = g_string_new (NULL);
  cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.4);
  if (((tuplet *) theobj->object)->directives)
    draw_for_directives (cr, ((tuplet *) theobj->object)->directives, xx, y - 4, TRUE);
  else
    {
      if (theobj->type == TUPOPEN)
        {
          g_string_sprintf (tupopentext, "%d", ((tupopen *) theobj->object)->denominator);      //save number for end tuplet call
          cairo_rectangle (cr, xx - 2, y - BRACKET_HEIGHT, 2, 15);      //small vertical
        }
      else
        {
          cairo_rectangle (cr, xx + GAP - 3, y - BRACKET_HEIGHT, 2, 15);
          if (start)
            {
              cairo_rectangle (cr, start - 2, y - BRACKET_HEIGHT, 2, 15);       //small vertical at start tuplet
              cairo_rectangle (cr, start - 2, y - BRACKET_HEIGHT, (xx - start) / 2 - GAP + 2, 2);       // draw line from start tuplet position to half way less gap
              drawnormaltext_cr (cr, tupopentext->str, (xx + start) / 2, y - BRACKET_HEIGHT + 4);       //print number in gap
              cairo_rectangle (cr, 2 * GAP + (xx + start) / 2, y - BRACKET_HEIGHT, (xx - start) / 2 - GAP - 3, 2);      //draw line rest of way
            }
          else
            {
              drawnormaltext_cr (cr, "End", xx, y - BRACKET_HEIGHT);
            }
        }
      cairo_fill (cr);
    }
}
