/* displayanimation.h
   header file for functions that animate transitions in the Denemo display,


   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2012 Richard Shann
*/

#include <denemo/denemo.h>

#ifndef DISPLAYANIMATION_H
#define DISPLAYANIMATION_H

void set_viewport_transition (gint amount);
void set_staff_transition (gint amount);
void set_movmement_transition (gint amount);
void set_measure_transition (gint amount, gboolean all);
void set_cursor_transition (void);

gdouble transition_offset (void);
gdouble transition_cursor_scale (void);

gdouble measure_transition_offset (gboolean current);

gdouble staff_transition_offset (void);
gdouble movement_transition_offset (void);
void set_movement_transition (gint amount);
#endif
