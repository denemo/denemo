/* moveviewport.h
   header file for functions that change leftmeasurenum, rightmeasurenum,
   top_measure, bottom_measure

   for Denemo, a gtk+ frontend to GNU Lilypond
   (c) 2000-2005 Matthew Hiller
*/

#include <denemo/denemo.h>

#ifndef MOVEVIEWPORT_H
#define MOVEVIEWPORT_H



void
update_hscrollbar (DenemoGUI *si);

void
update_vscrollbar (DenemoGUI *si);

gboolean
set_rightmeasurenum (DenemoScore *si);

void
set_bottom_staff (DenemoGUI *gui);

void
isoffleftside (DenemoGUI *gui);

void
isoffrightside (DenemoGUI *gui);

void
page_viewport(void);

void
move_viewport_up (DenemoGUI *gui);

void
move_viewport_down (DenemoGUI *si);

gboolean
set_currentmeasurenum (DenemoGUI *si, gint dest);

gboolean
moveto_currentmeasurenum(DenemoGUI *si, gint dest);

void
vertical_scroll (GtkAdjustment *adjust, gpointer dummy);

void
horizontal_scroll (GtkAdjustment *adjust, gpointer dummy);

void scroll_left(void);
void scroll_right (void);

gboolean
set_currentstaffnum(DenemoGUI *si, gint dest);

gboolean
moveto_currentstaffnum(DenemoGUI *si, gint dest);

gdouble 
transition_offset(void);
gdouble 
transition_cursor_scale(void);
#endif
