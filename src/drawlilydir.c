/* drawlilydir.cpp
 *
 * Functions for drawing stemming directives
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include <denemo/denemo.h>
#include <string.h>
#include "gcs.h"
/**
 * Draw a lilypond directive on the score as a vertical green line and text if appropriate
 *
 */
void
draw_lily_dir (GdkPixmap * pixmap, GdkGC * gc, GdkFont * font,
	       gint xx, gint y, gint highy, gint lowy, DenemoObject * theobj, gboolean selected)
{
  lilydirective *lily = ((lilydirective *) theobj->object);
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);
  gchar *first = (lily->postfix->str);

  if(lily->graphic){
    gint width = lily->width;
    gint  height = lily->height;  
    drawbitmapinverse (pixmap, gcs_lightbluegc(), lily->graphic,
		     xx, y+lowy, width, height);
  }
  if(lily->display) {  //store display position x,y as well
    pango_layout_set_text (layout,
			   lily->display->str,
			   -1);
    pango_layout_set_font_description (layout, desc);
    gdk_draw_layout (pixmap, selected?gcs_bluegc():gc, xx/*+display x */, y+lowy+20/*+display y */, layout);
  }
#if 1
  else
  //do this by creating a display field

  if( *first == '%' || *first == '^' || *first == '_' ) { //display comments, and markup above and below
    pango_layout_set_text (layout,
			   first+1,
			   -1);
    pango_layout_set_font_description (layout, desc);
     
    gdk_draw_layout (pixmap, selected?gcs_bluegc():gc, xx, *first=='_'?y+lowy+20:y-highy-20 /*y+(*first=='_'?STAFF_HEIGHT+20:-20)*/, layout);
  }
#endif



   pango_font_description_free (desc); 



  gdk_draw_rectangle (pixmap, selected?gcs_bluegc():gcs_greengc(), TRUE, xx, y+3*STAFF_HEIGHT/2, 2, STAFF_HEIGHT/2);
}
