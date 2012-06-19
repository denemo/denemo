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
/**
 * Draw a standalone Denemo directive on the score as a vertical bar or graphic and/or text
 *
 */
void
draw_lily_dir (cairo_t *cr,
	       gint xx, gint y, gint highy, gint lowy, DenemoObject * theobj, gboolean selected)
{
  DenemoDirective *lily = ((lilydirective *) theobj->object);
  gchar *first = (lily->postfix && lily->postfix->len)? lily->postfix->str:" ";
  guint layout = selected_layout_id();
  gdouble only = lily->y?((lily->y==layout)?0.5:0.0):0.0;
  gdouble exclude = lily->x?((lily->x==layout)?0.5:0.0):0.0;

  cairo_save(cr);
    
  selected?
      cairo_set_source_rgb( cr, 0.0, 0.0, 1.0 ):
      lily->graphic? cairo_set_source_rgb( cr, 0.0+exclude, 0.0+only, 0.0 ):
          cairo_set_source_rgb( cr, 0.4+exclude, 0.5+only, 0.4 );
  if(lily->graphic){
    gint width = lily->graphic->width;
    gint  height = lily->graphic->height;  
    //FIXME there may be scripts expecting a different positioning code
    drawbitmapinverse_cr (cr, (DenemoGraphic *)lily->graphic,
			  xx + lily->gx - (((DenemoGraphic *)lily->graphic)->width)/2, y + MID_STAFF_HEIGHT + lily->gy -  (((DenemoGraphic *)lily->graphic)->height)/2, FALSE);
  } else {
    cairo_rectangle (cr, xx/*-2*/, y, 10, STAFF_HEIGHT);
    cairo_fill( cr );
  }
  if(lily->display) {  //store display position x,y as well
    drawnormaltext_cr( cr, lily->display->str, xx+ lily->tx, y+lowy+lily->ty );
  }
  else
  //FIXME do this by creating a display field
    if((!lily->graphic) && ( *first == '%' || *first == '^' || *first == '_' )) { //display comments, and markup above and below
      drawnormaltext_cr( cr, first+1, xx, *first=='_'?y+lowy+20:y-highy-20 );
    }
    cairo_restore(cr);
}
