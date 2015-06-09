/* slurs.cpp
 *
 * Functions for drawing slurs
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee, Matthew Hiller
 */

#include <denemo/denemo.h>
#include "core/utils.h"              /* Includes <gdk.h> */

GSList *
push_slur_stack (GSList * slur_stack, gint x)
{
  slur_stack = g_slist_prepend (slur_stack, GINT_TO_POINTER (x));
  return slur_stack;
}

gint
top_slur_stack (GSList * slur_stack)
{
  if (slur_stack)
    return GPOINTER_TO_INT (slur_stack->data);
  else
    return -1;
}

GSList *
pop_slur_stack (GSList * slur_stack)
{
  if (slur_stack)
    {
      GSList *head = slur_stack;

      slur_stack = g_slist_remove_link (slur_stack, head);
      g_slist_free_1 (head);
      return slur_stack;
    }
  else
    return NULL;
}

void
draw_slur (cairo_t * cr, GSList ** slur_stack, gint x2, gint y)
{
  gint x1 = top_slur_stack (*slur_stack);
  if (x1 != -1)
    {
      *slur_stack = pop_slur_stack (*slur_stack);

      cairo_set_line_width (cr, 1.0);
      cairo_move_to (cr, x1, y - 15);
      cairo_rel_curve_to (cr, (x2 - x1) / 3, -8, (x2 - x1) * 2 / 3, -8, (x2 - x1), 0);
      cairo_stroke (cr);
    }
  else
    {
      cairo_set_line_width (cr, 1.0);
      cairo_move_to (cr, 0, y - 15);
      cairo_rel_curve_to (cr, (x2) / 3, -8, (x2) * 2 / 3, -8, (x2), 0);
      cairo_stroke (cr);
    }
}

void
draw_slur_start (cairo_t * cr, gint x, gint y)
{

  cairo_set_line_width (cr, 1.0);
  cairo_move_to (cr, x, y - 15);
  cairo_rel_line_to (cr, 8, -4);
  cairo_stroke (cr);

}

void
draw_slur_end (cairo_t * cr, gint x, gint y)
{

  cairo_set_line_width (cr, 1.0);
  cairo_move_to (cr, x + 5, y - 15);
  cairo_rel_line_to (cr, -9, -4);
  cairo_stroke (cr);

}
