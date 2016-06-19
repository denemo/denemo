/* calculatepositions.h
 * header file for functions that calculate the positions at which score
 * objects are drawn
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee
 */

#include <denemo/denemo.h>
#include "core/twoints.h"

void find_xes_in_measure (DenemoMovement * si, gint measurenum);

void find_xes_in_all_measures (DenemoMovement * si);
