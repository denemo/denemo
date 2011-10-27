/* dynamic.cpp 
 * Implements lilydirectives which are not notes 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001, 2002  Adam Tee
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
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
  DenemoScore *si;
  GtkWidget *combobox;
};

static gchar *directives[15] =
  { "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff",
  "sf", "fp", "sfz", "cr", "rc", "dr", "rd"
};


void
add_dynamic (DenemoObject * mudelaobj, GString * dynamic)
{
  if (mudelaobj && mudelaobj->type == CHORD)
    {
      ((chord *) mudelaobj->object)->dynamics =
	g_list_append (((chord *) mudelaobj->object)->dynamics, dynamic);
      ((chord *) mudelaobj->object)->has_dynamic = TRUE;
    }
}

static void
insert_it (GtkWidget * widget, gpointer data)
{
  DenemoObject *mudelaobj;
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoScore *si = cbdata->si;
  GString *directivestring = NULL;
  gchar *string =
    (gchar *)
    gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (cbdata->combobox)->entry));

  directivestring = g_string_new (string);
  mudelaobj = (DenemoObject *)
    (si->currentobject ? si->currentobject->data : NULL);

  add_dynamic (mudelaobj, directivestring);

}




void
insert_dynamic (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  int i;
  static struct callbackdata cbdata;
  GtkWidget *dialog;
  GtkWidget *combo;
  GtkWidget *okbutton;
  GtkWidget *cancelbutton;
  GtkWidget *label;
  GList *directivelist = NULL;


  dialog = gtk_dialog_new_with_buttons (_("Insert Dynamic"), NULL,	/* parent window */
						 (GtkDialogFlags)
						 (GTK_DIALOG_MODAL |
						  GTK_DIALOG_DESTROY_WITH_PARENT),
						 GTK_STOCK_OK,
						 GTK_RESPONSE_ACCEPT,
						 GTK_STOCK_CANCEL,
						 GTK_STOCK_CANCEL, NULL);



  label = gtk_label_new (_("Insert Dynamic"));
#ifdef _USE_GTK3_	
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)), label,
		      TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		      TRUE, TRUE, 0);
#endif
  gtk_widget_show (label);

  combo = gtk_combo_new ();
  if (!directivelist)
    for (i = 0; i < 15; i++)
      {
	directivelist = g_list_append (directivelist, directives[i]);
      }

  gtk_combo_set_popdown_strings (GTK_COMBO (combo), directivelist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (combo)->entry), directives[0]);
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)), combo,
		      TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), combo,
		      TRUE, TRUE, 0);
#endif
  gtk_widget_show (combo);



  okbutton = gtk_button_new_with_label (_("OK"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->action_area), okbutton,
		      TRUE, TRUE, 0);
  cbdata.si = gui->si;
  cbdata.combobox = combo;

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      insert_it (NULL, &cbdata);
      displayhelper (gui);
    }

  gtk_widget_destroy (dialog);
}
