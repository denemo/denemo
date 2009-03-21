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

#include "../pixmaps/feta26-clefs-treble.xbm"
#include "../pixmaps/feta26-clefs-bass.xbm"
#include "../pixmaps/feta26-clefs-alto.xbm"
#include "../pixmaps/feta26-clefs-g_8.xbm"

#define NUMCLEFTYPES 6
#define TREBLE_WIDTH 26
#define TREBLE_HEIGHT 76
#define TREBLE_TOPOFFSET -16
#define BASS_WIDTH 27
#define BASS_HEIGHT 32
#define BASS_TOPOFFSET 1
#define ALTO_WIDTH 27
#define ALTO_HEIGHT 41
#define ALTO_TOPOFFSET 0
#define G_8_TOPOFFSET -8
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
  static GdkPixmap *clefs[NUMCLEFTYPES] =
    { NULL, NULL, NULL, NULL, NULL, NULL };
  static gint clefwidths[NUMCLEFTYPES] =
    { TREBLE_WIDTH, BASS_WIDTH, ALTO_WIDTH, TREBLE_WIDTH, ALTO_WIDTH,
    TREBLE_WIDTH
  };
  static gint clefheights[NUMCLEFTYPES] =
    { TREBLE_HEIGHT, BASS_HEIGHT, ALTO_HEIGHT, TREBLE_HEIGHT, ALTO_HEIGHT,
    TREBLE_HEIGHT
  };
  static gint clefoffsets[NUMCLEFTYPES] =
    { TREBLE_TOPOFFSET, BASS_TOPOFFSET, ALTO_TOPOFFSET, G_8_TOPOFFSET,
    TENOR_TOPOFFSET, SOPRANO_TOPOFFSET
  };

  if (!clefs[0])
    {
      clefs[DENEMO_TREBLE_CLEF] = bitmaphelper (NULL, feta26_clefs_treble);
      clefs[DENEMO_BASS_CLEF] = bitmaphelper (NULL, feta26_clefs_bass);
      clefs[DENEMO_ALTO_CLEF] = bitmaphelper (NULL, feta26_clefs_alto);
      clefs[DENEMO_G_8_CLEF] = bitmaphelper (NULL, feta26_clefs_g_8);
      clefs[DENEMO_TENOR_CLEF] = bitmaphelper (NULL, feta26_clefs_alto);
      clefs[DENEMO_SOPRANO_CLEF] = bitmaphelper (NULL, feta26_clefs_alto);
    }

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
	gdk_drawable_get_size(directive->graphic, &width, &height);
	drawbitmapinverse (pixmap, gc, directive->graphic,
			   xx+directive->gx+count,  y+directive->gy, width, height);
      }
    }
  }
  if(!override)
      drawbitmapinverse (pixmap, gc, clefs[type],
		     xx, y + clefoffsets[type],
		     clefwidths[type], clefheights[type]);
}
