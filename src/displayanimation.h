/* displayanimation.h
   header file for functions that animate transitions in the Denemo display,
  

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2012 Richard Shann
*/

#include <denemo/denemo.h>

#ifndef DISPLAYANIMATION_H
#define DISPLAYANIMATION_H

void set_viewport_transition(gint amount);


gdouble 
transition_offset(void);
gdouble 
transition_cursor_scale(void);
#endif
