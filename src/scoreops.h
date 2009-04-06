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
void next_movement (GtkAction *action, DenemoScriptParam *param);
void prev_movement (GtkAction *action, DenemoScriptParam *param);
void insert_movement_before (GtkAction *action, gpointer param);
void insert_movement_after (GtkAction *action, gpointer param);
void delete_movement (GtkAction *action, gpointer param);
void movement_props_dialog (GtkAction *action, gpointer param);
gboolean
goto_movement_staff_obj (DenemoGUI * gui, gint movementnum, gint staffnum, gint measurenum, gint objnum);
#endif
