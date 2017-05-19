/* score.h
 * headers for functions dealing with the whole score

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */

#include <denemo/denemo.h>

#ifndef SCOREOPS_H
#define SCOREOPS_H

void point_to_empty_movement /*new_empty_score */ (DenemoProject * gui);
void point_to_new_movement /*new_score */ (DenemoProject * gui);
void init_score (DenemoMovement * si, DenemoProject * gui);
DenemoMovement *clone_movement (DenemoMovement * si);
void free_movement (DenemoProject * gui);
void deletescore (GtkWidget * widget, DenemoProject * gui);
void updatescoreinfo (DenemoProject * gui);
void next_movement (DenemoAction * action, DenemoScriptParam * param);
void prev_movement (DenemoAction * action, DenemoScriptParam * param);
void append_new_movement (DenemoAction * action, DenemoScriptParam * param);
void append_blank_movement (void);
void insert_movement_before (DenemoAction * action, DenemoScriptParam * param);
void insert_movement_after (DenemoAction * action, DenemoScriptParam * param);
void delete_movement (DenemoAction * action, DenemoScriptParam * param);
void movement_props_dialog (DenemoAction * action, DenemoScriptParam * param);
gboolean goto_movement_staff_obj (DenemoProject * gui, gint movementnum, gint staffnum, gint measurenum, gint objnum, gint leftmeasurenum);

void PopPosition (DenemoAction * action, DenemoScriptParam * param);
void PushPosition (DenemoAction * action, DenemoScriptParam * param);
void PopPushPosition (DenemoAction * action, DenemoScriptParam * param);
void reset_movement_numbers (DenemoProject * gui);
void set_movement_selector (DenemoProject *gui);
DenemoObject *get_object_by_position (gint movementnum, gint staffnum, gint measurenum, gint objnum);
#endif
