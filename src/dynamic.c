/* dynamic.c
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


static gchar *directives[15] = { "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff",
  "sf", "fp", "sfz", "cr", "rc", "dr", "rd"
};


void
add_dynamic (DenemoObject * mudelaobj, GString * dynamic)
{
  if (mudelaobj && mudelaobj->type == CHORD)
    {
      ((chord *) mudelaobj->object)->dynamics = g_list_append (((chord *) mudelaobj->object)->dynamics, dynamic);
      ((chord *) mudelaobj->object)->has_dynamic = TRUE;
    }
}

static void
insert_it (gint num)
{
  DenemoObject *mudelaobj;
  GString *directivestring = NULL;

  directivestring = g_string_new (directives[num]);
  mudelaobj = (DenemoObject *) (Denemo.gui->si->currentobject ? Denemo.gui->si->currentobject->data : NULL);

  add_dynamic (mudelaobj, directivestring);
}

void
insert_dynamic (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *combo;
  GtkWidget *label;
  GtkWidget *content;
  guint i;

  dialog = gtk_dialog_new_with_buttons (_("Insert Dynamic"), NULL, (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_STOCK_CANCEL, NULL);


  content = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  label = gtk_label_new (_("Insert Dynamic"));
  gtk_container_add (GTK_CONTAINER (content), label);
#if GTK_MAJOR_VERSION==3
  combo = gtk_combo_box_text_new ();
  for (i = 0; i < G_N_ELEMENTS (directives); i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), directives[i]);

  gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 0);
#else
  GList *directivelist = NULL;
  combo = gtk_combo_new ();
  if (!directivelist)
    for (i = 0; i < 15; i++)
      {
        directivelist = g_list_append (directivelist, directives[i]);
      }

  gtk_combo_set_popdown_strings (GTK_COMBO (combo), directivelist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (combo)->entry), directives[0]);
#endif
  gtk_container_add (GTK_CONTAINER (content), combo);
  gtk_widget_show_all (dialog);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gint num = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));

      insert_it (num);
    }

  g_signal_connect_swapped (dialog, "response", G_CALLBACK (gtk_widget_destroy), dialog);

  gtk_widget_destroy (dialog);
  displayhelper (gui);
}
