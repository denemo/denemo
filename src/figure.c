/* figure.cpp
 *
 * Functions for the manipulations of figured basses
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 2003-2006 Richard Shann
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "figure.h"
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
 * Function to actually insert a figure to an object
 *
 */
void
insertfigure (GtkWidget * widget, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoGUI *gui = cbdata->gui;
  DenemoScore *si = gui->si;
  static staff_info null_info;
  GString *current_figure;
  if (si->currentobject != NULL) {
    DenemoObject *curObj = (DenemoObject *) si->currentobject ?
      (DenemoObject *) si->currentobject->data : NULL;
    gchar *figure = (gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->entry));
    if(strlen(figure)<1)
      figure = "_";/* in case user deleted the figure to yield <> */
    /* translate the input somewhat */
    GString *f = g_string_new("");
    gchar *c = figure;
    for(c=figure;*c;c++) {
      if(*c=='+') {
	if(c==figure || *(c-1)==' ' || *(c-1)=='*' || *(c-1)=='|' || *(c-1)=='/')
	  g_string_append(f, "_+");
	else
	  g_string_append(f,"+");
      }else
	if(*c=='-') {
	  if(c==figure || *(c-1)==' ' ||*(c-1)=='*' || *(c-1)=='|' || *(c-1)=='/')
	    g_string_append(f, "_-");
	  else
	    g_string_append(f,"-");
	}else
	  if(*c=='/')
	    g_string_append(f, "|");
          else {
	    if(*c=='*')
	      g_string_append(f, " ");
		else
		  g_string_append_c(f, *c);
	  }
    }
    
    if (curObj && curObj->type == CHORD)
      ((chord *) curObj->object)->is_figure = TRUE;
    ((chord *) curObj->object)->figure = g_string_new(f->str);//FIXME memory leak of old figure
    g_string_free(f, TRUE);
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
    while ((curObj != NULL) && (curObj->type != CHORD));
    
    
    si->has_figures = (gpointer)TRUE; //&null_info;
    gui->changecount++;
  } // if currentobject not null
  else {
    warningdialog("No current object to attach a figure to");	 
  } 
}

/**
 * Creates figured bass entry dialog
 *
 */
void
figure_insert (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;

  DenemoScore *si = gui->si;
  static struct callbackdata cbdata;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  /* if (si->lily_file)
     return; edit lily text instead */
  //if (curObj && curObj->type == CHORD
  //    && ((chord *) curObj->object)->is_figure)
  //  return;			/* edit the lily text */
  dialog = gtk_dialog_new_with_buttons (_("Insert/Edit Figure"),
					GTK_WINDOW (gui->window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);


  label = gtk_label_new (_("Give figures followed by Enter key"));

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		      TRUE, TRUE, 0);

  entry = gtk_entry_new ();
  cbdata.gui = gui;
  cbdata.entry = entry;

 if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->figure)
	{
		gtk_entry_set_text (GTK_ENTRY (entry),
		((GString *) ((chord *) curObj->object)->figure)->str);
	}
	

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), entry,
		      TRUE, TRUE, 0);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_grab_focus (entry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      insertfigure (NULL, &cbdata);
      ((DenemoStaff*)si->currentstaff->data)->hasfigures=TRUE;
      displayhelper (gui);
    }
  gtk_widget_destroy (dialog);
}
