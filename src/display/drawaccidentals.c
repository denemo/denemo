/* drawaccidentals.cpp
 * functions for drawing accidentals
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

#include "core/utils.h"              /* Includes gtk.h */
#include <denemo/denemo.h>

/* Include the pixmaps; they'll actually be made into GdkPixmaps with the
 * GDK create_from_xpm_d functions. Doing things this way circumvents the need
 * to install the pixmaps into a /usr/share directory of some kind before
 * the program can be run from anywhere on the system. */

#include "display/accwidths.h"

gint accwidths[NUMACCTYPES] = { DOUBLEFLAT_WIDTH, FLAT_WIDTH, NATURAL_WIDTH, SHARP_WIDTH,
  DOUBLESHARP_WIDTH
};

gunichar acc_char[NUMACCTYPES] = { 0x43, 0x3a, 0x36, 0x2e, 0x45
};

/**
 * Draw an accidental given the specific enshift value
 *
 */
void
draw_accidental (cairo_t * cr, gint xx, gint aggheight, gint enshift)
{
  gint n = enshift + 2;         /* convenient index into arrays */

  drawfetachar_cr (cr, acc_char[n], xx, aggheight);
}
