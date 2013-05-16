/* clefdialog.c
 * callback that creates "change staff" dialog box
 * user for further action

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005, 2008  Matthew Hiller,Adam Tee, Richard Shann
 */

#include <gtk/gtk.h>
#include <string.h>
#include "calculatepositions.h"
#include "chordops.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "draw.h"
#include "dialogs.h"
#include "measureops.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"

#define COLUMN_NAME (0)
#define COLUMN_ID (1)


typedef struct ClefInfo
{
  enum clefs clef;
  gchar *name;

} ClefInfo;

// TODO: make this globally accessable
static ClefInfo clef_info[] = {
  {DENEMO_TREBLE_CLEF, N_("Treble")},
  {DENEMO_BASS_CLEF, N_("Bass")},
  {DENEMO_ALTO_CLEF, N_("Alto")},
  {DENEMO_G_8_CLEF, N_("Treble Octava bassa")},
  {DENEMO_TENOR_CLEF, N_("Tenor")},
  {DENEMO_SOPRANO_CLEF, N_("Soprano")},
  {DENEMO_F_8_CLEF, N_("Bass Octava bassa")},
  {DENEMO_FRENCH_CLEF, N_("French")},
};

const gchar *
get_clef_name (guint type)
{
  if (type >= 0 && (type < G_N_ELEMENTS (clef_info)))
    return clef_info[type].name;
  return NULL;
}

static enum clefs
get_clef_from_name (gchar * name)
{
  guint i;
  for (i = 0; i < G_N_ELEMENTS (clef_info); i++)
    {
      if (!g_ascii_strcasecmp (name, clef_info[i].name))
        return clef_info[i].clef;
    }
  return DENEMO_INVALID_CLEF;
}

/**
 * Callback to insert a clef change 
 * calls clef change with the INSERT argument
 */
void
clef_change_insert (GtkAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, clefname);
  DenemoGUI *gui = Denemo.gui;
  if (clefname == NULL)
    clef_change (gui, INSERT);
  else
    {
      enum clefs clef = get_clef_from_name (clefname);
      if (clef != DENEMO_INVALID_CLEF)
        object_insert (gui, dnm_newclefobj (clef));
      displayhelper (gui);
    }
}

/**
 * Callback to change the inital clef on a staff 
 * calls clef change with the CHANGEINITIAL argument
 */
void
clef_change_initial (GtkAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, clefname);
  DenemoGUI *gui = Denemo.gui;
  if (clefname == NULL)
    clef_change (gui, CHANGEINITIAL);
  else
    {
      enum clefs clef = get_clef_from_name (clefname);
      if (clef != DENEMO_INVALID_CLEF)
        dnm_setinitialclef (gui->si, gui->si->currentstaff->data, clef);
      displayhelper (gui);
    }
}

void
dnm_setinitialclef (DenemoScore * si, DenemoStaff * curstaffstruct, enum clefs clef)
{
  curstaffstruct->clef.type = clef;
  find_leftmost_staffcontext (curstaffstruct, si);
  fixnoteheights (curstaffstruct);
  find_xes_in_all_measures (si);
}

/**
 * Clef change dialog, two modes
 * 1,  Set Initial clef
 * 2,  Insert Clef Change
 */
void
clef_change (DenemoGUI * gui, actiontype action)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *combobox;
  GtkListStore *list_store;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  int i;

  dialog = gtk_dialog_new_with_buttons (((action == CHANGEINITIAL) ? _("Set Clef") : _("Insert clef change")), NULL,    /* parent window */
                                        (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_STOCK_CANCEL, NULL);


  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  label = gtk_label_new (_("Select desired clef"));
  gtk_container_add (GTK_CONTAINER (content_area), label);

  list_store = gtk_list_store_new (2, G_TYPE_STRING, G_TYPE_INT);
  combobox = gtk_combo_box_new_with_model (GTK_TREE_MODEL (list_store));        //FIXME This looks like it could be made more simple to read by using someting like gtk_combo_text_box or something

  for (i = 0; i < (gint) G_N_ELEMENTS (clef_info); i++)
    {
      gtk_list_store_append (list_store, &iter);
      gtk_list_store_set (list_store, &iter, COLUMN_NAME, _(clef_info[i].name), COLUMN_ID, clef_info[i].clef, -1);

      if (clef_info[i].clef == curstaffstruct->clef.type)
        {
          gtk_combo_box_set_active_iter (GTK_COMBO_BOX (combobox), &iter);
        }
    }


  renderer = gtk_cell_renderer_text_new ();
  gtk_cell_layout_pack_start (GTK_CELL_LAYOUT (combobox), renderer, TRUE);
  gtk_cell_layout_add_attribute (GTK_CELL_LAYOUT (combobox), renderer, "text", COLUMN_NAME);

  gtk_container_add (GTK_CONTAINER (content_area), combobox);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      enum clefs clef;
      gtk_combo_box_get_active_iter (GTK_COMBO_BOX (combobox), &iter);
      gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter, COLUMN_ID, &clef, -1);

      if (action == CHANGEINITIAL)
        {
          dnm_setinitialclef (gui->si, curstaffstruct, clef);
        }
      else
        {
          if (gui->si->currentobject && ((DenemoObject *) gui->si->currentobject->data)->type == CLEF)
            deleteobject (gui);
          object_insert (gui, dnm_newclefobj (clef));

        }
      score_status (gui, TRUE);
      displayhelper (gui);
    }
  gtk_widget_destroy (dialog);
}
