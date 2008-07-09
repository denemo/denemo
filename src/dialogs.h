/** 
 * dialogs.h
 * header files for callbacks that create dialog boxes prompting the
 * user for further action
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller 
 */

#include <denemo/denemo.h>
#ifndef __DIALOGS_H__
#define __DIALOGS_H__




typedef enum actiontype
{ CHANGEINITIAL, INSERT } 
actiontype;

void clef_change_initial(GtkAction *action);
void clef_change_insert(GtkAction *action);

void key_change_initial(GtkAction *action);
void key_change_insert(GtkAction *action);

void timesig_change_initial(GtkAction *action);
void timesig_change_insert(GtkAction *action);

void
clef_change (DenemoGUI *gui,actiontype action);

void
key_change (DenemoGUI *gui,  actiontype action);

void
timesig_change (DenemoGUI *gui, actiontype action);

void
score_mwidth_change (GtkAction *action, gpointer callback_data);

void
score_staffspace_change (GtkAction *action, gpointer callback_data);




gboolean
staff_properties_change (GtkAction *action, gpointer callback_data /* FIXME sometimes used */);



void
playback_properties_change (GtkAction *action);



void
tomeasurenum (GtkAction *action);

void
preferences_change (GtkAction *action);

void
header_change (GtkAction *action);

void
score_properties_dialog(GtkAction *action);

void export_pdf_action (GtkAction *action);

#endif /* __DIALOGS_H__ */
