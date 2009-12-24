/* drawclefs.cpp
 * functions for drawing clefs
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include "utils.h"		/* Includes gtk.h */
#include <denemo/denemo.h>

/* Include the pixmaps; they'll actually be made into GdkPixmaps with the
 * GDK create_from_xpm_d functions. Doing things this way circumvents the need
 * to install the pixmaps into a /usr/share directory of some kind before
 * the program can be run from anywhere on the system. */

#define NUMCLEFTYPES 6
#define TREBLE_TOPOFFSET 30 
#define BASS_TOPOFFSET 10
#define ALTO_TOPOFFSET 21
#define G_8_TOPOFFSET 30 
#define TENOR_TOPOFFSET -9
#define SOPRANO_TOPOFFSET 20

/**
 * This function draws the clef appropriate for the current context
 * onto the backing pixmap 
 */
void
draw_clef (GdkPixmap * pixmap, GdkGC * gc, gint xx, gint y, clef *clef)
{
  gint type = clef->type;
  static gint clefoffsets[NUMCLEFTYPES] =
    { TREBLE_TOPOFFSET, BASS_TOPOFFSET, ALTO_TOPOFFSET, G_8_TOPOFFSET,
    TENOR_TOPOFFSET, SOPRANO_TOPOFFSET
  };
  static gunichar clef_char[NUMCLEFTYPES] =
    { 0xc9, 0xc7, 0xc5, 0xc9, 0xc5, 0xc5 
  };

  gboolean override = FALSE;
  if(clef->directives) {
    gint count=0;
    GList *g=clef->directives;
    for(;g;g=g->next, count++) {
      DenemoDirective* directive = g->data;
      override = override || directive->override;
      if(directive->display) { 
	PangoContext *context =
	  gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
	PangoLayout *layout = pango_layout_new (context);
	PangoFontDescription *desc = pango_font_description_from_string (FONT);
	pango_layout_set_text (layout,
			       directive->display->str,
			       -1);
	pango_layout_set_font_description (layout, desc);
	gdk_draw_layout (Denemo.gui->pixmap, gc, xx + directive->tx, y+count*10, layout);
      }
      if(directive->graphic) {
	gint width, height;
	gdk_drawable_get_size(GDK_DRAWABLE(directive->graphic), &width, &height);
	drawbitmapinverse (pixmap, gc, directive->graphic,
			   xx+directive->gx+count,  y+directive->gy, width, height);
      }
    }
  }
  if(!override) {
    drawfetachar( pixmap, gc, clef_char[type], xx, y+clefoffsets[type] );
  }
}
