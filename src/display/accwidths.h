/* accwidths.h
 * holds accidental widths, which is used both in draw_accidental
 * and setpixelmin
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee
 */
#ifndef ACCWIDTHS_H
#define ACCWIDTHS_H

#include <glib.h>

#define NUMACCTYPES 5
#define DOUBLEFLAT_WIDTH 15
#define DOUBLEFLAT_HEIGHT 26
#define DOUBLEFLAT_OFFSET 18
#define FLAT_WIDTH 9
#define FLAT_HEIGHT 26
#define FLAT_OFFSET 18
#define NATURAL_WIDTH 7
#define NATURAL_HEIGHT 30
#define NATURAL_OFFSET 14
#define SHARP_WIDTH 11
#define SHARP_HEIGHT 32
#define SHARP_OFFSET 15
#define DOUBLESHARP_WIDTH 10
#define DOUBLESHARP_HEIGHT 10
#define DOUBLESHARP_OFFSET 5

#define EXTRABACKOFF 3

extern gint accwidths[NUMACCTYPES];

#endif
