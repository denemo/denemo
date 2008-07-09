/* lyric.cpp
 *
 * Functions for the manipulations of lyrics
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 2002-2005 Adam Tee
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


/**
 * Create new lyric object
 *
 * @param baseduration base duration of the lyric
 * @param numdots	number of dots the lyric has
 * @param lys the actual lyric
 * @return the new lyric object
 */
DenemoObject *
newlyric (gint baseduration, gint numdots, gchar * lys)
{
  DenemoObject *thelyric = newchord (baseduration, numdots, 0);
  ((chord *) thelyric->object)->lyric = g_string_new (lys);
  ((chord *) thelyric->object)->is_syllable = FALSE;
  set_basic_numticks (thelyric);
  return thelyric;
}


struct callbackdata
{
  DenemoGUI *gui;
  GtkWidget *entry;
  GtkWidget *extender;
  GtkWidget *center;
};

/**
 * Lyric insertion callback function used be lyric_insert
 * 
 * @param data callback data structure contain info about the lyric and the scoreinfo 
 * structure
 */
void
insertlyric (gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoGUI *gui = cbdata->gui;
  DenemoScore *si = gui->si;
  DenemoObject *curObj = (DenemoObject *) (si->currentobject ?
					   si->currentobject->data : NULL);
  gchar *lyric = (gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->entry));
  if (curObj && curObj->type == CHORD)
    {
      if (!((chord *) curObj->object)->lyric)
	((chord *) curObj->object)->lyric = g_string_new (lyric);
      else
	g_string_assign (((chord *) curObj->object)->lyric, lyric);

      ((chord *) curObj->object)->is_syllable =
	gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->extender));
      ((chord *) curObj->object)->center_lyric =
	gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->center));
    }
    do
      {
	if (si->currentobject->next)
	  cursorright (gui);
	else if (gui->si->currentmeasure->next)
	  measureright (gui);
	else 
	  break;
	curObj =
	  si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
      }
    while ((curObj != NULL) && 
	   ((curObj->type != CHORD)||
	     ((curObj->type == CHORD)&& (((chord *) curObj->object)->notes==NULL))));
}

/**
 * Insert/Edit lyric menu callback. presents user with a dialog
 * to insert the lyric
 * @param action the action event of the menuitem
 * @param gui pointer to the DenemoGUI structure
 */
void
lyric_insert (GtkAction * action)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;
  GtkWidget *extender;
  GtkWidget *center;
  GtkWidget *okbutton;
  GtkWidget *cancelbutton;
  GtkWidget *table;

  static struct callbackdata cbdata;
  DenemoScore *si = gui->si;
  DenemoObject *curObj = (DenemoObject *)
    (si->currentobject ? si->currentobject->data : NULL);
  dialog = gtk_dialog_new_with_buttons (_("Insert Lyric"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);


  label = gtk_label_new (_("Insert Lyric:"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		      TRUE, TRUE, 0);
  gtk_widget_show (label);
  entry = gtk_entry_new ();
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->lyric)
    gtk_entry_set_text (GTK_ENTRY (entry),
			((chord *) curObj->object)->lyric->str);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), entry, TRUE, TRUE,
		      0);

  gtk_widget_show (entry);

  extender = gtk_check_button_new_with_label ("Extend Syllable");
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), extender,
		      TRUE, TRUE, 0);
  gtk_widget_show (extender);

  center = gtk_check_button_new_with_label ("Center");
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), center,
		      TRUE, TRUE, 0);
  gtk_widget_show (center);


  cbdata.gui = gui;
  cbdata.entry = entry;
  cbdata.extender = extender;
  cbdata.center = center;

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  gtk_widget_grab_focus (entry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      insertlyric (&cbdata);
      ((DenemoStaff*)si->currentstaff->data)->haslyrics=TRUE;
      score_status(gui, TRUE);
      displayhelper (gui);
    }
  gtk_widget_destroy (dialog);

}
