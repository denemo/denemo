/* scoreprops.c
 * Callback that creates a "Score Properties" dialog box.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee, Matthew Hiller */

#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "core/utils.h"
#include "core/view.h"
#include "command/commandfuncs.h"
#include "command/staff.h" //  signal_structural_change (Denemo.project); surely in wrong file...
#include "ui/dialogs.h"

#define COL_NAME 0
#define COL_VALUE 1
#define COL_PTR 2

/**
 * Lilyponds supported font sizes
 */
static gchar *fontsizes[8] = {
  "11", "13", "14", "16", "18", "20", "23", "26"
};

/**
 * Lilyponds supported paper sizes
 */
static gchar *papersizes[6] = {
  "a4", "a6", "a5", "legal", "letter", "tabloid"
};



typedef struct papersetupcb
{
  GtkWidget *papersize;
  GtkWidget *lilyversion;
  GtkWidget *portrait;
  GtkWidget *fontsize;
  GtkWidget *lilypond;
} papersetupcb;
/* UNUSED
static void
cell_edited (GtkCellRendererText * cellrenderertext, gchar * path_string, gchar * new_text, GtkTreeModel * model)
{
  GtkTreePath *path = gtk_tree_path_new_from_string (path_string);
  GtkTreeIter iter;

  gtk_tree_model_get_iter (model, &iter, path);
  gtk_list_store_set (GTK_LIST_STORE (model), &iter, COL_VALUE, new_text, -1);
  gtk_tree_path_free (path);
}
*/
static gboolean
abandon_editprops_custom_scoreblock (DenemoProject * gui)
{
  if (gui->custom_scoreblocks)
    return confirm (_("Custom LilyPond Score Block"), _("You will need to edit the LilyPond text to copy these edits from the standard scoreblock.\nIt might be easier to edit your custom scoreblock directly. Abandon?"));
  return FALSE;
}

/**
 * Function to set the printed score parameters
 *
 */
static void
setpaperconfig (papersetupcb * cbdata, DenemoProject * gui)
{
#if GTK_MAJOR_VERSION==3
  gchar *val = (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (cbdata->papersize));
  if (!val) val = "a4";
  g_string_assign (gui->lilycontrol.papersize, val);
  val = (gchar *) gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (cbdata->fontsize));
  if (!val) val = "18";
  g_string_assign (gui->lilycontrol.staffsize, val);
#else
  gchar *val = (gchar *) gtk_combo_box_get_active_text (cbdata->papersize);
  if (!val) val = "a4";
  g_string_assign (gui->lilycontrol.papersize, val );
  val = (gchar *) gtk_combo_box_get_active_text (cbdata->fontsize);
  if (!val) val = "18";
  g_string_assign (gui->lilycontrol.staffsize, val);
#endif
  g_string_assign (gui->lilycontrol.lilyversion, (gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->lilyversion)));


  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->portrait)))
    {
      gui->lilycontrol.orientation = TRUE;
    }
  else
    gui->lilycontrol.orientation = FALSE;

  //g_debug(" %s %s %d %d \n", gui->lilycontrol.papersize->str, gui->lilycontrol.lilyversion->str, gui->lilycontrol.fontsize, gui->lilycontrol.orientation);
  score_status (gui, TRUE);
}


/**
 * Create and run a modal score properties dialog.
 */
void
score_properties_dialog (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;

  GET_1PARAM (action, param, fontsize); //note "fontsize" is internally called the staffsize
  if (query)
    {
      if (*query)
        if (!strcmp ("fontsize", query))
          {
            g_string_assign (param->string, Denemo.project->lilycontrol.staffsize->str);
            param->status = TRUE;
          }
      return;
    }

  if (fontsize)
    {
      signal_structural_change (Denemo.project);
      g_string_assign (Denemo.project->lilycontrol.staffsize, fontsize);
      param->status = TRUE;
      return;
    }

  if (abandon_editprops_custom_scoreblock (gui))
    return;
  GtkWidget *dialog;
  GtkWidget *notebook;
  GtkWidget *label;
  GtkWidget *measure_width;
  GtkWidget *staff_spacing;
  gint i;
  DenemoMovement *si = gui->movement;
  //g_assert (si != NULL);
 
  dialog = gtk_dialog_new_with_buttons (_("Score properties"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  //gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  notebook = gtk_notebook_new ();

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), notebook);

  // Layout
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
  label = gtk_label_new_with_mnemonic (_("Display Layout"));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);

  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  label = gtk_label_new (_("Measure width (pixels):"));
  gtk_container_add (GTK_CONTAINER (hbox), label);
  measure_width = gtk_spin_button_new_with_range (10.0, 1000, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (measure_width), (gdouble) si->measurewidth);
  gtk_container_add (GTK_CONTAINER (hbox), measure_width);

  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, FALSE, 0);
  label = gtk_label_new (_("Staff spacing (pixels):"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_container_add (GTK_CONTAINER (hbox), label);
  staff_spacing = gtk_spin_button_new_with_range ((3 * STAFF_HEIGHT)/2, 1000, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (staff_spacing), (gdouble) si->staffspace);
  gtk_container_add (GTK_CONTAINER (hbox), staff_spacing);

  papersetupcb *setup = (papersetupcb *) g_malloc0 (sizeof (papersetupcb));
  vbox = gtk_vbox_new (FALSE, 1);
  label = gtk_label_new_with_mnemonic (_("Typesetter"));
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, label);

#if GTK_MAJOR_VERSION==3
  GtkWidget *papersize = gtk_combo_box_text_new ();
  for (i = 0; i < G_N_ELEMENTS (papersizes); i++)
    {
      gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (papersize), papersizes[i]);
      if (!strcmp (gui->lilycontrol.papersize->str, papersizes[i]))
        gtk_combo_box_set_active (GTK_COMBO_BOX (papersize), i);
    }
#else
  GtkWidget *papersize = gtk_combo_box_entry_new_text ();
  for (i = 0; i < G_N_ELEMENTS (papersizes); i++)
    gtk_combo_box_append_text (GTK_COMBO_BOX (papersize), papersizes[i]);
  gtk_entry_set_text (GTK_ENTRY (GTK_BIN (papersize)->child), gui->lilycontrol.papersize->len ? gui->lilycontrol.papersize->str : "");
#endif
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (vbox), hbox);
  label = gtk_label_new (_("Paper Size"));
  gtk_container_add (GTK_CONTAINER (hbox), label);
  gtk_container_add (GTK_CONTAINER (hbox), papersize);

#if GTK_MAJOR_VERSION==3
  GtkWidget *fontsizecombo = gtk_combo_box_text_new ();
  for (i = 0; i < G_N_ELEMENTS (fontsizes); i++)
    {
      gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (fontsizecombo), fontsizes[i]);
      if (!strcmp (gui->lilycontrol.staffsize->str, fontsizes[i]))
        gtk_combo_box_set_active (GTK_COMBO_BOX (fontsizecombo), i);
    }
#else
  GtkWidget *fontsizecombo = gtk_combo_box_entry_new_text ();
  for (i = 0; i < G_N_ELEMENTS (fontsizes); i++)
    gtk_combo_box_append_text (GTK_COMBO_BOX (fontsizecombo), fontsizes[i]);
  gtk_entry_set_text (GTK_ENTRY (GTK_BIN (fontsizecombo)->child), gui->lilycontrol.staffsize->len ? gui->lilycontrol.staffsize->str : "");
#endif
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (vbox), hbox);
  label = gtk_label_new (_("Font Size"));
  gtk_container_add (GTK_CONTAINER (hbox), label);
  gtk_container_add (GTK_CONTAINER (hbox), fontsizecombo);

  hbox = gtk_hbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (vbox), hbox);
  label = gtk_label_new (_("Lilypond Version"));
  gtk_container_add (GTK_CONTAINER (hbox), label);
  GtkWidget *lilyversion = gtk_entry_new ();
  gtk_container_add (GTK_CONTAINER (hbox), lilyversion);
  gtk_entry_set_text (GTK_ENTRY (lilyversion), gui->lilycontrol.lilyversion->len ? gui->lilycontrol.lilyversion->str : "");

  GtkWidget *portraitradio = gtk_radio_button_new_with_label (NULL, _("Portrait"));
  gtk_container_add (GTK_CONTAINER (vbox), portraitradio);

  GtkWidget *landscaperadio = gtk_radio_button_new_with_label (gtk_radio_button_get_group (GTK_RADIO_BUTTON (portraitradio)), _("Landscape"));
  gtk_container_add (GTK_CONTAINER (vbox), landscaperadio);

  if (gui->lilycontrol.orientation)
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (portraitradio), TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (landscaperadio), TRUE);

  setup->papersize = papersize;
  setup->fontsize = fontsizecombo;
  setup->portrait = portraitradio;
  setup->lilyversion = lilyversion;

  score_status (gui, TRUE);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {

      /*
         if (!si->lily_file)
         {
         gchar *value;
         GString *dest;
         gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list_store), &iter);
         do
         {
         gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter,
         COL_VALUE, &value, COL_PTR, &dest, -1);
         g_string_assign (dest, value);
         }
         while (gtk_tree_model_iter_next
         (GTK_TREE_MODEL (list_store), &iter));
         }
       */

      // Set layout options
      gint width = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (measure_width));
      if (width != si->measurewidth)
        {
          si->measurewidth = width;
          adjustmeasurewidth (si, 0);
          draw_score_area();
        }

      gint spacing = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (staff_spacing));
      if (spacing != si->staffspace)
        {
          si->staffspace = spacing;
          draw_score_area();
        }
      setpaperconfig (setup, gui);

    }
  g_free (setup);
  signal_structural_change (Denemo.project);
  gtk_widget_destroy (dialog);
}





/***************** movement properties *********************/
struct callbackdata
{
  DenemoProject *gui;

  GtkWidget *title;
  GtkWidget *subtitle;
  GtkWidget *poet;
  GtkWidget *composer;
  GtkWidget *meter;
  GtkWidget *opus;
  GtkWidget *arranger;
  GtkWidget *instrument;
  GtkWidget *dedication;
  GtkWidget *piece;
  GtkWidget *head;
  GtkWidget *copyright;
  GtkWidget *footer;
  GtkWidget *tagline;

  GtkWidget *lilypond_before;
  GtkWidget *lilypond_after;
  // GtkWidget *layout;
  // GtkWidget *extra;
};

/**
 * Creates a dialog for setting properties of current movement gui->movement
 *
 */
void
movement_props_dialog (DenemoAction * action, DenemoScriptParam * param)
{

}
