/* changenotehead.c
 * Changes the type of notehead if required
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) Adam Tee 2000-2005
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "display/calculatepositions.h"
#include "command/contexts.h"
#include "ui/dialogs.h"
#include "display/draw.h"
#include "command/object.h"
#include "command/staff.h"
#include "core/utils.h"
#include "command/commandfuncs.h"
/**
 * Array of different Notehead types
 */
gchar *notehead[4] = { N_("Normal"), N_("Cross"), N_("Diamond"),
  N_("Harmonic")
};


/**
 * Set the correct enum value for the selected
 * notehead
 */
enum headtype
texttohead (gchar * text)
{
  if (g_strcasecmp (text, _("Normal")) == 0)
    return DENEMO_NORMAL_NOTEHEAD;
  else if (g_strcasecmp (text, _("Cross")) == 0)
    return DENEMO_CROSS_NOTEHEAD;
  else if (g_strcasecmp (text, _("Diamond")) == 0)
    return DENEMO_DIAMOND_NOTEHEAD;
  else if (g_strcasecmp (text, _("Harmonic")) == 0)
    return DENEMO_HARMONIC_NOTEHEAD;
  else
    return DENEMO_NORMAL_NOTEHEAD;
}


/**
 * Set current notes notehead to the selected
 * value
 */
void
insertnotehead (DenemoMovement * si, gchar * notehead_string)
{
  DenemoObject *obj = (DenemoObject *) (si->currentobject ? si->currentobject->data : NULL);


  if (obj != NULL && obj->type == CHORD)
    {
      /* Lilypond's behavior is a bit anomalous here. It doesn't seem
       * to like giving chords non-standard noteheads. This is
       * just a default behavior for the time being. */
      ((note *) ((chord *) obj->object)->notes->data)->noteheadtype = texttohead (notehead_string);
    }

}

/**
 * Notehead selection dialog
 * Displays the notehead type in a Combobox
 * Callback - insert_notehead
 */
void
set_notehead (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *combo;
  GtkWidget *content_area;
  static GList *list = NULL;    //NOTE required for gtk<2.24

  guint i;
  if (!action)
    {
      if (((DenemoScriptParam *) param)->string && ((DenemoScriptParam *) param)->string->len)
        {
          insertnotehead (gui->movement, ((DenemoScriptParam *) param)->string->str);
          ((DenemoScriptParam *) param)->status = TRUE;
          return;
        }
      else
        {
          if (param)
            ((DenemoScriptParam *) param)->status = FALSE;
          return;
        }
    }

  if (!list)
    {
      for (i = 0; i < 4; i++)
        {
          list = g_list_append (list, _(notehead[i]));
        }
    }

  dialog = gtk_dialog_new_with_buttons (_("Change Notehead"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);


  content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  label = gtk_label_new (_("Select Notehead Type"));
  gtk_container_add (GTK_CONTAINER (content_area), label);
#if GTK_MAJOR_VERSION==3
  combo = gtk_combo_box_text_new ();
  for (i = 0; i < G_N_ELEMENTS (notehead); i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (combo), notehead[i]);
  gtk_combo_box_set_active (GTK_COMBO_BOX (combo), 0);
#else
  combo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (combo), list);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (combo)->entry), _(notehead[0]));
  gtk_combo_box_set_active (GTK_COMBO (combo), 0);
#endif
  gtk_container_add (GTK_CONTAINER (content_area), combo);
  gtk_widget_grab_focus (combo);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
#if GTK_MAJOR_VERSION==3
      gint num = gtk_combo_box_get_active (GTK_COMBO_BOX (combo));
      insertnotehead (gui->movement, notehead[num]);
#else
      gchar *noteheadstring = (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (combo)->entry));
      insertnotehead (gui->movement, noteheadstring);
#endif
    }
  gtk_widget_destroy (dialog);
  displayhelper (gui);
}
