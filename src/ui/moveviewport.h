/* moveviewport.h
   header file for functions that change leftmeasurenum, rightmeasurenum,
   top_measure, bottom_measure

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005 Matthew Hiller
*/

#include <denemo/denemo.h>

#ifndef MOVEVIEWPORT_H
#define MOVEVIEWPORT_H



void update_hscrollbar (DenemoProject * si);

void update_vscrollbar (DenemoProject * si);

gboolean set_rightmeasurenum (DenemoMovement * si);

void set_bottom_staff (DenemoProject * gui);

void isoffleftside (DenemoProject * gui);

void isoffrightside (DenemoProject * gui);

void page_viewport (void);

void move_viewport_up (DenemoProject * gui);

void move_viewport_down (DenemoProject * si);

gboolean set_currentmeasurenum (DenemoProject * si, gint dest);

gboolean moveto_currentmeasurenum (DenemoProject * si, gint dest, gint leftmeasurenum);

void vertical_scroll (GtkAdjustment * adjust, gpointer dummy);

void horizontal_scroll (GtkAdjustment * adjust, gpointer dummy);

void scroll_left (void);
void scroll_right (void);

gboolean set_currentstaffnum (DenemoProject * si, gint dest);

gboolean moveto_currentstaffnum (DenemoProject * si, gint dest);

gdouble transition_offset (void);
gdouble transition_cursor_scale (void);
#endif
