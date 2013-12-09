/* calculatepositions.h
 * header file for functions that calculate the positions at which score
 * objects are drawn
 *  
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee
 */

#include <denemo/denemo.h>
#include "twoints.h"

struct twoints find_xes_in_measure (DenemoMovement * si, gint measurenum, gint time1, gint time2);

void find_xes_in_all_measures (DenemoMovement * si);
