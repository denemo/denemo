/* playbackprops.c
 * callback that creates a "Playback Properties" dialog box asking
 * the user to change the properties of the playback for the current 
 * score
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee, Matthew Hiller */

#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "utils.h"
#include "prefops.h"

struct callbackdata
{
  GtkWidget *tempo;
  GtkWidget *checkplayback;
};

static void
set_preferences (struct callbackdata *cbdata)
{
  DenemoPrefs *prefs =  &Denemo.prefs;
  
#define ASSIGNTEXT(field) \
    g_string_assign (prefs->field,\
    gtk_entry_get_text (GTK_ENTRY (cbdata->field)));

#define ASSIGNBOOLEAN(field) \
    prefs->field =\
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cbdata->field));

#define ASSIGNINT(field) \
    prefs->field =\
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(cbdata->field));

Denemo.gui->si->tempo =
        gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (cbdata->tempo));

  writeXMLPrefs (prefs);
}


void
playback_properties_change (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  GtkWidget *main_vbox;
  GtkWidget *notebook;
  GtkWidget *button;


  static struct callbackdata cbdata;

  dialog = gtk_dialog_new_with_buttons (_("Playback properties"), NULL,
					(GtkDialogFlags) 
					(GTK_DIALOG_MODAL |
					GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);
  notebook = gtk_notebook_new ();
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)), notebook, TRUE,
		      TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		      TRUE, 0);
#endif
#define NEWPAGE(thelabel) \
  main_vbox = gtk_vbox_new (FALSE, 1);\
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), main_vbox, NULL);\
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), main_vbox,\
                                                           _(thelabel));

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field =\
    gtk_check_button_new_with_label (thelabel); \
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (field),\
		            (gboolean)Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (main_vbox), field, FALSE, TRUE, 0);\
  cbdata.field = field;

#define TEXTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (_(thelabel));\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_entry_new ();\
  gtk_entry_set_text (GTK_ENTRY (field), Denemo.prefs.field->str);\
  gtk_box_pack_start (GTK_BOX (hbox), field, TRUE, TRUE, 0);\
  cbdata.field = field;

#define INTENTRY_LIMITS(thelabel, field, min, max) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), gui->si->field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;  

  NEWPAGE("Playback")
  INTENTRY_LIMITS("Tempo", tempo, 10, 250);
   
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
 
  gtk_widget_show_all(dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_preferences (&cbdata);
    }
  gtk_widget_destroy (dialog);
}
