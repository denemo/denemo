#include "dialogs.h"
#include <ctype.h>
#include <stdlib.h>
/* This is broken at the moment because the program doesn't pause
 * when the dialog is created. Fix me. */

void
tupletchangedialog (DenemoObject * theobj, GtkWidget * scorearea)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *numerator;
  GtkWidget *denominator;
  GString *entrycontent = NULL;

  dialog = gtk_dialog_new_with_buttons (_("Customize tuplet multiplier"), NULL, (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);
  if (!entrycontent)
    entrycontent = g_string_new (NULL);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  label = gtk_label_new (_("Numerator"));
  gtk_container_add (GTK_CONTAINER (content_area), label);

  numerator = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d", ((tupopen *) theobj->object)->numerator);
  gtk_entry_set_text (GTK_ENTRY (numerator), entrycontent->str);

  gtk_container_add (GTK_CONTAINER (content_area), numerator);

  label = gtk_label_new (_("Denominator"));
  gtk_container_add (GTK_CONTAINER (content_area), label);

  denominator = gtk_entry_new ();
  g_string_sprintf (entrycontent, "%d", ((tupopen *) theobj->object)->denominator);
  gtk_entry_set_text (GTK_ENTRY (denominator), entrycontent->str);

  gtk_container_add (GTK_CONTAINER (content_area), denominator);

  gtk_widget_grab_focus (numerator);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (numerator), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (denominator), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      ((tupopen *) theobj->object)->numerator = atoi (gtk_entry_get_text (GTK_ENTRY (numerator)));
      ((tupopen *) theobj->object)->denominator = atoi (gtk_entry_get_text (GTK_ENTRY (denominator)));
    }

  gtk_widget_destroy (dialog);
}
