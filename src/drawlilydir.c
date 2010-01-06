/* drawlilydir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Adam Tee, 2008, 2009 Richard Shann
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>
#include <string.h>
#include "gcs.h"
/**
 * Draw a lilypond directive on the score as a vertical bar and text if appropriate
 *
 */
void
draw_lily_dir (cairo_t *cr,
	       gint xx, gint y, gint highy, gint lowy, DenemoObject * theobj, gboolean selected)
{
  lilydirective *lily = ((lilydirective *) theobj->object);
  gchar *first = (lily->postfix && lily->postfix->len)? lily->postfix->str:" ";

  if(lily->graphic){
    gint width = lily->width;
    gint  height = lily->height;  
    drawbitmapinverse_cr (cr, (GdkBitmap *)lily->graphic,
		     xx + lily->gx, y + lily->gy, width, height);
  }
  else
  {
    cairo_save(cr);
    if(selected)
      cairo_set_source_rgb( cr, 0.0, 0.0, 1.0 );
    else
      cairo_set_source_rgb( cr, 0.0, 1.0, 0.0 );

    cairo_rectangle (cr, xx/*-2*/, y, 10, STAFF_HEIGHT);
    cairo_fill( cr );
    cairo_restore(cr);
  }
  if(lily->display) {  //store display position x,y as well
    drawnormaltext_cr( cr, lily->display->str, xx+ lily->tx, y+lowy+lily->ty );
  }
#if 1
  else
  //do this by creating a display field

  if( *first == '%' || *first == '^' || *first == '_' ) { //display comments, and markup above and below
    drawnormaltext_cr( cr, first+1, xx, *first=='_'?y+lowy+20:y-highy-20 );
  }
#endif

}
