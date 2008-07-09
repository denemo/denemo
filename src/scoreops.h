/* scoreops.h
 * headers for functions dealing with the whole score

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */
 
#include <denemo/denemo.h>

#ifndef SCOREOPS_H
#define SCOREOPS_H

void new_empty_score(DenemoGUI * gui);
void new_score(DenemoGUI * gui);
void init_score (DenemoScore *si, DenemoGUI * gui);

void free_score (DenemoGUI *gui);
void next_movement (GtkAction *action);
void prev_movement (GtkAction *action);
void insert_movement_before (GtkAction *action);
void insert_movement_after (GtkAction *action);
void delete_movement (GtkAction *action);
void movement_props_dialog (GtkAction *action);
gboolean
goto_movement_staff_obj (DenemoGUI * gui, gint movementnum, gint staffnum, gint measurenum, gint objnum);
#endif
