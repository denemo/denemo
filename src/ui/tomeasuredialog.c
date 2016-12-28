/* tomeasuredialog.cpp
 * creates a dialog box asking the user if he wants to jump to a
 * particular measure

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <gtk/gtk.h>
#include <stdio.h>
#include <stdlib.h>
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "display/draw.h"
#include "ui/moveviewport.h"
#include "command/staff.h"
#include "core/utils.h"

void
tomeasurenum (DenemoAction * action, gpointer param)
{

  DenemoProject *gui = Denemo.project;
  gint measnum;
  gint max_mesure = g_list_length (((DenemoStaff *) (gui->movement->thescore->data))->themeasures);

  if (!action)
    {
      if (((DenemoScriptParam *) param)->string != NULL)
        {
          measnum = atoi (((DenemoScriptParam *) param)->string->str);
          if (measnum > max_mesure)
            set_currentmeasurenum (gui, max_mesure);
          if (measnum <= 0)
            set_currentmeasurenum (gui, 1);
          else
            set_currentmeasurenum (gui, measnum);
        }
    }
  else
    {
      GtkWidget *dialog;
      GtkWidget *label;
      GtkWidget *measure;
      static gint last_measure = 1;

      dialog = gtk_dialog_new_with_buttons (_("Go to measure"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);

      gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

      GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
      label = gtk_label_new (_("Go to measure:"));
      gtk_container_add (GTK_CONTAINER (content_area), label);

      measure = gtk_spin_button_new_with_range (1.0, (gdouble) max_mesure, 1.0);
      gtk_container_add (GTK_CONTAINER (content_area), measure);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (measure), (gdouble) last_measure);
      gtk_label_set_mnemonic_widget (GTK_LABEL (label), measure);
      gtk_entry_set_activates_default (GTK_ENTRY (measure), TRUE);
      gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
      gtk_widget_show_all (dialog);
      if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
        {
          last_measure = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (measure));
          set_currentmeasurenum (gui, last_measure);
        }
      gtk_widget_destroy (dialog);
    }
}
