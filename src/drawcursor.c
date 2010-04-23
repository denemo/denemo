/* drawcursor.cpp
 * functions for drawing the cursor
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005  Matthew Hiller, Adam Tee
 */

#include "drawingprims.h"
#include "gcs.h"
#include "utils.h"

#define CURSOR_MINUS 2
#define CURSOR_WIDTH 10
#define CURSOR_HEIGHT 5


/**
 * Draw the cursor on the canvas at the given position
 *
 */
void
draw_cursor (cairo_t *cr, DenemoScore * si,
	     gint xx, gint y, input_mode mode, gint dclef)
{
  if(!cr) return;
  gint height = calculateheight (si->cursor_y, dclef);

  static GdkGC *blackgc = NULL;
  static GdkGC *graygc;
  static GdkGC *greengc;
  static GdkGC *redgc;
  static GdkGC *bluegc;
  static GdkGC *purplegc;
  GdkGC *paintgc;

  if (!blackgc)
    {
      blackgc = gcs_blackgc ();
      graygc = gcs_graygc ();
      greengc = gcs_greengc ();
      redgc = gcs_redgc ();
      bluegc = gcs_bluegc ();
      purplegc = gcs_purplegc ();
    }

  paintgc = (mode & INPUTREST) ? graygc :
    (mode & INPUTBLANK) ? bluegc :
    (mode & INPUTEDIT) ? purplegc : si->cursoroffend ? redgc : greengc;

  cairo_save( cr );
  setcairocolor( cr, paintgc );
  cairo_rectangle( cr, xx, height + y - CURSOR_MINUS, CURSOR_WIDTH, CURSOR_HEIGHT );
  cairo_fill( cr );
  cairo_restore( cr );

  /* Now draw ledgers if necessary and we're done */
  draw_ledgers (cr, height, height, xx, y, CURSOR_WIDTH);
}
