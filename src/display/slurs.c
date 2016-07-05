/* slurs.cpp
 *
 * Functions for drawing slurs
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee, Matthew Hiller
 */

#include <denemo/denemo.h>
#include "core/utils.h"              /* Includes <gdk.h> */
#include <math.h>

GSList *
push_slur_stack (GSList * slur_stack, gint x, gint y)
{
  slur_stack = g_slist_prepend (slur_stack, GINT_TO_POINTER ((x&0xFFFF)+(y<<16)));
  return slur_stack;
}

static gint
top_slur_stack_x (GSList * slur_stack)
{
  if (slur_stack)
    return 0xFFFF & GPOINTER_TO_INT (slur_stack->data);
  else
    return -1;
}
static gint
top_slur_stack_y (GSList * slur_stack)
{
  if (slur_stack)
    return  (GPOINTER_TO_INT (slur_stack->data))>>16;
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
draw_slur (cairo_t * cr, GSList ** slur_stack, gint x2, gint y, gint y2)
{
  gint x1 = top_slur_stack_x (*slur_stack);
  gint y1 = top_slur_stack_y (*slur_stack);
  gint dir = (y1+y2>40?-1:1);


  if (x1 != -1)
    {
      x1 += 6;//over note head
      *slur_stack = pop_slur_stack (*slur_stack);

      cairo_set_line_width (cr, 1.0);
      cairo_move_to (cr, x1, y1 + y - 12 * dir);
      cairo_rel_curve_to (cr, (x2 - x1) / 3, (y2 - y1 - 5* dir)*1/3 -8 * dir, (x2 - x1) * 2 / 3, (y2 - y1 - 5* dir)*2/3 - 8* dir, (x2 - x1), y2 - y1 - 5* dir);
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
  cairo_save (cr);
  cairo_set_source_rgba (cr, 0.1, 0.9, 0.1, 0.8);
  cairo_translate (cr, x+6, y - 15);
  cairo_rotate (cr, -M_PI / 3.0);
  cairo_scale (cr, 0.7, -0.7);
  drawfetachar_cr (cr, 0xD8, 0, 0);
  cairo_fill (cr);
  cairo_restore (cr);
}

void
draw_slur_end (cairo_t * cr, gint x, gint y)
{
  cairo_save (cr);
  cairo_set_source_rgba (cr, 0.9, 0.1, 0.1, 0.8);
   cairo_translate (cr, x+5, y - 15);
  cairo_rotate (cr, -M_PI / 1.5);
  cairo_scale (cr, 0.7, -0.7);
  drawfetachar_cr (cr, 0xD9, 0, 0);
  //cairo_arc (cr, x + 5, y - 16, 4, 0.0, 2 * M_PI);
  cairo_fill (cr);
  cairo_restore (cr);
}
