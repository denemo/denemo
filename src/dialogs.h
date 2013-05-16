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

void clef_change_initial (GtkAction * action, DenemoScriptParam * param);
void clef_change_insert (GtkAction * action, DenemoScriptParam * param);

void key_change_initial (GtkAction * action, DenemoScriptParam * param);
void key_change_insert (GtkAction * action, DenemoScriptParam * param);

void timesig_change_initial (GtkAction * action, DenemoScriptParam * param);
void timesig_change_insert (GtkAction * action, DenemoScriptParam * param);

void clef_change (DenemoGUI * gui, actiontype action);

void key_change (DenemoGUI * gui, actiontype action);

void timesig_change (DenemoGUI * gui, actiontype action);

void score_mwidth_change (GtkAction * action, gpointer callback_data);

void score_staffspace_change (GtkAction * action, gpointer callback_data);




void staff_properties_change_cb (GtkAction * action, DenemoScriptParam * param);




void playback_properties_change (GtkAction * action, gpointer param);



void tomeasurenum (GtkAction * action, gpointer param);

void preferences_change (GtkAction * action, gpointer param);

void header_change (GtkAction * action, gpointer param);

void score_properties_dialog (GtkAction * action, DenemoScriptParam * param);

void export_pdf_action (GtkAction * action, DenemoScriptParam * param);

const gchar *get_clef_name (guint type);

#endif /* __DIALOGS_H__ */
