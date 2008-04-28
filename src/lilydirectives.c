/* lilydirectives.cpp 
 * Implements lilydirectives which are not notes 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * A Tee  (c) 2000-2005
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"


struct callbackdata
{
  DenemoGUI *gui;
  GtkWidget *entry;
};

/**
 * If the curObj is a chord with a note at the cursor position 
 * return that note, else return NULL
 */
static note *
findnote(DenemoObject *curObj, gint cursory) {
  note *curnote = NULL;
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->notes ) {
    GList *notes = ((chord *) curObj->object)->notes;
    for(;notes; notes = notes->next){
      curnote =  (note*)notes->data;
      //g_print("comparing %d and %d\n", cursory, curnote->y);
      if(cursory == curnote->mid_c_offset)
	break;
      curnote = NULL;
   }

  }
     return curnote;
}
/**
 * Insert the lilypond directive into the score
 *
 */
static void
insertdirective (GtkWidget * widget, gpointer data)
{

  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoGUI* gui = cbdata->gui;
  DenemoScore *si = gui->si;
  note *curnote;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  gchar *directivestring =
    (gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->entry));
  if (curObj && curObj->type == LILYDIRECTIVE)
    ((lilydirective *) curObj->object)->directive = g_string_new(directivestring);//FIXME memory leak of old directive
  else
    if((curnote = findnote(curObj, gui->si->cursor_y)) != NULL) {
      curnote->directive = g_string_new(directivestring);//FIXME memory leak of old directive
      score_status(gui, TRUE);
    }
    else    
      object_insert (gui, lily_directive_new (directivestring)), displayhelper(gui);
}

/**
 * Lilypond directive.  Allows user to insert a lilypond directive 
 * to the score at the current cursor position
 */
void
lily_directive (GtkAction * action, DenemoGUI *gui)
{
//  int i;
  DenemoScore * si = gui->si;
  static struct callbackdata cbdata;
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *okbutton;
  GtkWidget *cancelbutton;
  GtkWidget *label;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  dialog = gtk_dialog_new ();
  gtk_window_set_title (GTK_WINDOW (dialog), _("Insert LilyDirective"));

  label = gtk_label_new (_("Insert Lilydirective:"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		      TRUE, TRUE, 0);
  gtk_widget_show (label);

  entry = gtk_entry_new ();

 if (curObj && curObj->type == LILYDIRECTIVE && ((lilydirective *) curObj->object)->directive)
	{
		gtk_entry_set_text (GTK_ENTRY (entry),
		((GString *) ((lilydirective *) curObj->object)->directive)->str);
	}
 note *curnote = findnote(curObj, gui->si->cursor_y);
 if(curnote && curnote->directive)
   gtk_entry_set_text (GTK_ENTRY (entry), curnote->directive->str);

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), entry,
		      TRUE, TRUE, 0);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_NONE);/* FIXME re-write this dialog to use
									      the conventional response ids */
  gtk_entry_set_activates_default(GTK_ENTRY (entry), TRUE);
  gtk_widget_show (entry);



  okbutton = gtk_button_new_with_label (_("OK"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area), okbutton,
		      TRUE, TRUE, 0);
  cbdata.gui = gui;
  cbdata.entry = entry;
  gtk_signal_connect (GTK_OBJECT (okbutton), "clicked",
		      GTK_SIGNAL_FUNC (insertdirective), &cbdata);
  gtk_signal_connect_object (GTK_OBJECT (okbutton), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (dialog));
  gtk_widget_show (okbutton);

  cancelbutton = gtk_button_new_with_label (_("Cancel"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area),
		      cancelbutton, TRUE, TRUE, 0);
  gtk_signal_connect_object (GTK_OBJECT (cancelbutton), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (dialog));
  gtk_widget_show (cancelbutton);
  gtk_widget_grab_focus (entry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show (dialog);
}
