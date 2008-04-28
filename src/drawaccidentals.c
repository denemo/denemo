/* drawaccidentals.cpp
 * functions for drawing accidentals
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

#include "utils.h"		/* Includes gtk.h */
#include <denemo/denemo.h>

/* Include the pixmaps; they'll actually be made into GdkPixmaps with the
 * GDK create_from_xpm_d functions. Doing things this way circumvents the need
 * to install the pixmaps into a /usr/share directory of some kind before
 * the program can be run from anywhere on the system. */

#include "../pixmaps/feta26-accidentals--2.xbm"
#include "../pixmaps/feta26-accidentals--1.xbm"
#include "../pixmaps/feta26-accidentals-0.xbm"
#include "../pixmaps/feta26-accidentals-1.xbm"
#include "../pixmaps/feta26-accidentals-2.xbm"

#include "accwidths.h"

gint accwidths[NUMACCTYPES] =
  { DOUBLEFLAT_WIDTH, FLAT_WIDTH, NATURAL_WIDTH, SHARP_WIDTH,
  DOUBLESHARP_WIDTH
};

/**
 * Draw an accidental given the specific enshift value
 *
 */
void
draw_accidental (GdkPixmap * pixmap, GdkGC * gc, gint xx, gint aggheight,
		 gint enshift)
{
  static GdkPixmap *accidentals[NUMACCTYPES] =
    { NULL, NULL, NULL, NULL, NULL };
  static gint accheights[NUMACCTYPES] =
    { DOUBLEFLAT_HEIGHT, FLAT_HEIGHT, NATURAL_HEIGHT, SHARP_HEIGHT,
    DOUBLESHARP_HEIGHT
  };
  static gint accoffsets[NUMACCTYPES] =
    { DOUBLEFLAT_OFFSET, FLAT_OFFSET, NATURAL_OFFSET,
    SHARP_OFFSET, DOUBLESHARP_OFFSET
  };
  gint n = enshift + 2;		/* convenient index into arrays */
  //gint drawnx;

  if (!accidentals[0])
    {
      accidentals[0] = bitmaphelper (NULL, feta26_accidentals__2);
      accidentals[1] = bitmaphelper (NULL, feta26_accidentals__1);
      accidentals[2] = bitmaphelper (NULL, feta26_accidentals_0);
      accidentals[3] = bitmaphelper (NULL, feta26_accidentals_1);
      accidentals[4] = bitmaphelper (NULL, feta26_accidentals_2);
    }

  drawbitmapinverse (pixmap, gc, accidentals[n],
		     xx, aggheight - accoffsets[n],
		     accwidths[n], accheights[n]);
}
