/* scoreops.h
 * headers for functions dealing with the whole score

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller */

#include <denemo/denemo.h>

#ifndef SCOREOPS_H
#define SCOREOPS_H

void point_to_empty_movement /*new_empty_score */ (DenemoGUI * gui);
void point_to_new_movement /*new_score */ (DenemoGUI * gui);
void init_score (DenemoScore * si, DenemoGUI * gui);
DenemoScore *clone_movement (DenemoScore * si);
void free_score (DenemoGUI * gui);
void deletescore (GtkWidget * widget, DenemoGUI * gui);
void updatescoreinfo (DenemoGUI * gui);
void next_movement (GtkAction * action, DenemoScriptParam * param);
void prev_movement (GtkAction * action, DenemoScriptParam * param);
void append_new_movement (GtkAction * action, gpointer param);
void append_blank_movement (void);
void insert_movement_before (GtkAction * action, DenemoScriptParam * param);
void insert_movement_after (GtkAction * action, DenemoScriptParam * param);
void delete_movement (GtkAction * action, gpointer param);
void movement_props_dialog (GtkAction * action, gpointer param);
gboolean goto_movement_staff_obj (DenemoGUI * gui, gint movementnum, gint staffnum, gint measurenum, gint objnum);

void PopPosition (GtkAction * action, DenemoScriptParam * param);
void PushPosition (GtkAction * action, DenemoScriptParam * param);
void PopPushPosition (GtkAction * action, DenemoScriptParam * param);
void reset_movement_numbers (DenemoGUI * gui);
void set_movement_selector (DenemoGUI *gui);
#endif
