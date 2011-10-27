/* changenotehead.c
 * Changes the type of notehead if required
 * 
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) Adam Tee 2000-2005
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calculatepositions.h"
#include "contexts.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"
#include "commandfuncs.h"
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
insertnotehead (DenemoScore * si, gchar * notehead_string)
{
  DenemoObject *obj = (DenemoObject *)
    (si->currentobject ? si->currentobject->data : NULL);


  if (obj != NULL && obj->type == CHORD)
    {
      /* Lilypond's behavior is a bit anomalous here. It doesn't seem
       * to like giving chords non-standard noteheads. This is
       * just a default behavior for the time being. */
      ((note *) ((chord *) obj->object)->notes->data)->noteheadtype =
	texttohead (notehead_string);
    }

}

/**
 * Notehead selection dialog
 * Displays the notehead type in a Combobox
 * Callback - insert_notehead
 */
void
set_notehead (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *combo;
  gint i;
  static GList *list = NULL;
  if(!action) {
    if(  ((DenemoScriptParam *)param)->string && ((DenemoScriptParam *)param)->string->len) {
      insertnotehead (gui->si, ((DenemoScriptParam *)param)->string->str);
      ((DenemoScriptParam *)param)->status = TRUE;
      return;
    } else {
      if(param)
	((DenemoScriptParam *)param)->status = FALSE;
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

  dialog =
    gtk_dialog_new_with_buttons (_("Change Notehead"),
				 GTK_WINDOW (Denemo.window),
				 (GtkDialogFlags) (GTK_DIALOG_MODAL |
						   GTK_DIALOG_DESTROY_WITH_PARENT),
				 GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
				 GTK_STOCK_CANCEL, GTK_STOCK_CANCEL, NULL);



  label = gtk_label_new (_("Select Notehead Type"));
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)),
		      label, TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      label, TRUE, TRUE, 0);
#endif
  gtk_widget_show (label);

  combo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (combo), list);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (combo)->entry), _(notehead[0]));
#ifdef _USE_GTK3_
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)), combo,
		      TRUE, TRUE, 0);
#else
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), combo,
		      TRUE, TRUE, 0);
#endif
  gtk_widget_show (combo);


  gtk_widget_grab_focus (combo);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (GTK_COMBO (combo)->entry),
				   TRUE);
  gtk_widget_show (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gchar *noteheadstring =
	(gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (combo)->entry));
      insertnotehead (gui->si, noteheadstring);
    }

  gtk_widget_destroy (dialog);
  displayhelper (gui);
}
