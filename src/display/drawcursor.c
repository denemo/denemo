/* drawcursor.c
 * functions for drawing the cursor
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005  Matthew Hiller, Adam Tee, 2010 Richard Shann
 */
#include <math.h>
#include "display/drawingprims.h"
#include "core/utils.h"
#include "display/displayanimation.h"
#include "core/view.h"

#define CURSOR_WIDTH (10*scale)
#define CURSOR_HEIGHT (6*scale)
#define CURSOR_MINUS (CURSOR_HEIGHT/2)

/**
 * Draw the cursor on the canvas at the given position
 * insert_control is the last gap from the previous note except when the cursor is on a measure boundary in which case it is +1 or -1 to indicate where next inserted note will go.
 * minpixels is width of rectangle to draw indicating an object position (used when not a CHORD
 */
void
draw_cursor (cairo_t * cr, DenemoMovement * si, gint xx, gint y, gint insert_control, gint minpixels, gint dclef)
{
  if (!cr)
    return;
  gint height = calculateheight (si->cursor_y, dclef);
  gdouble scale = transition_cursor_scale ();

  cairo_save (cr);
  cairo_set_source_rgba (cr, 255 * (si->cursoroffend), (!si->cursoroffend) * (!si->cursor_appending) * 255, (!si->cursoroffend) * (si->cursor_appending) * 255, 0.4);
  if (si->cursor_appending)
    {
      cairo_rectangle (cr, xx - (si->cursoroffend ? CURSOR_WIDTH : 0), height + y - CURSOR_HEIGHT, 2 * CURSOR_WIDTH, 2 * CURSOR_HEIGHT);
      cairo_fill (cr);
      
      cairo_set_source_rgba (cr, 0, 0, 0,1);
      cairo_rectangle (cr, xx - (si->cursoroffend ? CURSOR_WIDTH : 0), height + y - CURSOR_HEIGHT, 2 * CURSOR_WIDTH, 2 * CURSOR_HEIGHT);
      cairo_stroke (cr);
    
      
      
    }
  else if (minpixels)
    {
       
     

      cairo_rectangle (cr, xx - CURSOR_WIDTH / 2, height + y - CURSOR_MINUS - CURSOR_HEIGHT / 2, minpixels, 2 * CURSOR_HEIGHT);
      cairo_fill (cr);
      cairo_set_line_width (cr, 2);
      cairo_set_source_rgba (cr, 0.6, 0.6, 0, 0.60);
      cairo_rectangle (cr, xx + - CURSOR_WIDTH / 2, y  - CURSOR_HEIGHT * 2  , minpixels, STAFF_HEIGHT + CURSOR_HEIGHT * 4);
      cairo_stroke (cr);
     
    }
  else
    {
      cairo_rectangle (cr, xx - CURSOR_WIDTH / 2, height + y - CURSOR_MINUS - CURSOR_HEIGHT / 2, 2 * CURSOR_WIDTH, 2 * CURSOR_HEIGHT);
      cairo_fill (cr);
      
      
      cairo_set_source_rgba (cr, 0, 0, 0,1);
      cairo_rectangle (cr, xx - CURSOR_WIDTH / 2, height + y - CURSOR_MINUS - CURSOR_HEIGHT / 2, 2 * CURSOR_WIDTH, 2 * CURSOR_HEIGHT);
      cairo_stroke (cr);
      
      
    }



  {
    gdouble length = 20 / si->zoom;
    gdouble insert_pos = CURSOR_WIDTH * 0.8;
    if (!si->cursor_appending)
      {
        insert_pos = -insert_control / 4;
      }
    else if (si->cursoroffend)
      insert_pos = insert_control * CURSOR_WIDTH;
    static gboolean on;
    on = !on;//g_print (" %d and %x\n", gtk_widget_has_focus (gtk_widget_get_parent (Denemo.scorearea)), gtk_widget_get_state_flags (Denemo.scorearea));
    //g_debug("on is %d %d\n", on,  Denemo.prefs.cursor_highlight);
    if (((!Denemo.prefs.cursor_highlight) || (on && Denemo.prefs.cursor_highlight)) && (gtk_widget_has_focus (Denemo.scorearea) && gtk_widget_is_focus (Denemo.scorearea)))
      {
        cairo_set_source_rgb (cr, 0, 0, 255);
        cairo_set_line_width (cr, 4);
        cairo_move_to (cr, xx + insert_pos, y + 4);
        cairo_rel_line_to (cr, 0, STAFF_HEIGHT - 8);
        cairo_stroke (cr);
        //setcairocolor( cr, paintgc );

        if (Denemo.project->view == DENEMO_PAGE_VIEW)
          {
            cairo_set_line_width (cr, 6.0 / si->zoom);
            cairo_set_source_rgba (cr, 0, 1, 0, 0.40);
            cairo_arc (cr, xx + CURSOR_WIDTH / 2, height + y, length, 0, 2 * M_PI);
            cairo_stroke (cr);
          }
        if(Denemo.project->audio_recording)
            highlight_audio_record();
      }
  }
  cairo_restore (cr);

  /* Now draw ledgers if necessary and we're done */
  draw_ledgers (cr, height, height, xx, y, CURSOR_WIDTH);
}
