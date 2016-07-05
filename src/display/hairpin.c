/* hairpin.cpp
 *
 * Functions for drawing hairpins
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee, Matthew Hiller
 */

#include <denemo/denemo.h>
#include "core/utils.h"              /* Includes <gdk.h> */

/**
 * Push elemnet onto hairpin stack
 *
 */
GSList *
push_hairpin_stack (GSList * hairpin_stack, gint x)
{
  hairpin_stack = g_slist_prepend (hairpin_stack, GINT_TO_POINTER (x));
  return hairpin_stack;
}

/**
 * Get the top element of the stack's data
 *
 */
gint
top_hairpin_stack (GSList * hairpin_stack)
{
  if (hairpin_stack)
    return GPOINTER_TO_INT (hairpin_stack->data);
  else
    return -1;
}

/**
 * Pop the head of the hairpin stack
 *
 */
GSList *
pop_hairpin_stack (GSList * hairpin_stack)
{
  if (hairpin_stack)
    {
      GSList *head = hairpin_stack;

      hairpin_stack = g_slist_remove_link (hairpin_stack, head);
      g_slist_free_1 (head);
      return hairpin_stack;
    }
  else
    return NULL;
}

/**
 * Draw a hairpin onto the score
 *
 */
void
draw_hairpin (cairo_t * cr, GSList ** hairpin_stack, gint x2, gint y, gint dir)
{
  gint x1 = top_hairpin_stack (*hairpin_stack);
  y += STAFF_HEIGHT*2;
  cairo_set_line_width (cr, 1.0);
 // allow drawing from off window
    {

      if (x1 == x2)
        x2 += 5;
      if (dir)
        {
          cairo_move_to (cr, x2, y - 20);
          cairo_line_to (cr, x1, y - 15);
          cairo_line_to (cr, x2, y - 10);
          cairo_stroke (cr);
        }
      else
        {
          cairo_move_to (cr, x1, y - 20);
          cairo_line_to (cr, x2, y - 15);
          cairo_line_to (cr, x1, y - 10);
          cairo_stroke (cr);
        }
    }
}
