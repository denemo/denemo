/* prefdialog.cpp
 * functions for a preferences dialog
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <dirent.h>
#include <denemo/denemo.h>
#include <denemo/denemo_version.h>
#include "prefops.h"
#include "utils.h"
#include "plugin.h"

struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypathentry;
  GtkWidget *checkimmediateplayback;
  GtkWidget *checklilystyle;
  GtkWidget *checkautosaveparts;
  GtkWidget *checkclone;
  GtkWidget *checkautosave;
  GtkWidget *checknotationpalette;
  GtkWidget *checkrhythmpalette;
  GtkWidget *checkarticulationpalette;
  GtkWidget *autosaveentry;
  GtkWidget *browserentry;
  GtkWidget *pdfviewer;
  GtkWidget *texteditor;
  GtkWidget *denemopath;
};

struct callbackdata1
{
  DenemoGUI *gui;
  GtkListStore *model;
};

#define COLUMN_LOADED (0)
#define COLUMN_PLUGIN (1)

/**
 * Callback to enable/disable the autosave entry when the auto save button is 
 * clicked
 */
static void
toggle_autosave (GtkToggleButton * togglebutton, GtkWidget * autosaveentry)
{
  gtk_widget_set_sensitive (autosaveentry,
			    gtk_toggle_button_get_active (togglebutton));
}

/**
 * Callback to load/unload a plugin
 */
static void
toggle_plugin (GtkCellRendererToggle * cell, gchar * path_str, gpointer data)
{
  GtkListStore *model = ((struct callbackdata1 *) data)->model;
  DenemoGUI *gui = ((struct callbackdata1 *) data)->gui;
  GtkTreePath *path = gtk_tree_path_new_from_string (path_str);
  GtkTreeIter iter;
  gboolean enabled;
  gchar *plugin;

  gtk_tree_model_get_iter (GTK_TREE_MODEL (model), &iter, path);
  gtk_tree_model_get (GTK_TREE_MODEL (model), &iter, COLUMN_LOADED, &enabled,
		      -1);

  //g_print ("Path str %s \n", path_str);
  gtk_tree_model_get (GTK_TREE_MODEL (model), &iter, COLUMN_PLUGIN, &plugin,
		      -1);
  //g_print ("plugin %s\n", plugin);
  enabled ^= 1;
  if (enabled)
    {
      denemo_plugin_init (plugin, gui);
      // g_warning ("TODO: Load plugin");
    }
  else
    {
      denemo_plugin_cleanup (plugin, gui);
      //g_warning ("TODO: Unload plugin\n");
    }

  gtk_list_store_set (model, &iter, COLUMN_LOADED, enabled, -1);
  g_free (plugin);
  gtk_tree_path_free (path);
}

static void
set_preferences (struct callbackdata *cbdata)
{
  DenemoPrefs *prefs = cbdata->prefs;


  g_string_assign (prefs->lilypath,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->lilypathentry)));
  g_string_assign (prefs->browser,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->browserentry)));
  g_string_assign (prefs->pdfviewer,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->pdfviewer)));
  g_string_assign (prefs->texteditor,
		   gtk_entry_get_text (GTK_ENTRY (cbdata->texteditor)));
  g_string_assign (prefs->denemopath,
                   gtk_entry_get_text (GTK_ENTRY (cbdata->denemopath)));

  prefs->immediateplayback =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
				  (cbdata->checkimmediateplayback));
  prefs->lilyentrystyle =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->checklilystyle));
  prefs->createclones =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->checkclone));

  prefs->autosave =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->checkautosave));
  prefs->autosave_timeout =
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON
				      (cbdata->autosaveentry));

  prefs->articulation_palette =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
				  (cbdata->checkarticulationpalette));
  prefs->notation_palette =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
				  (cbdata->checknotationpalette));
  prefs->rhythm_palette =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON
				  (cbdata->checkrhythmpalette));
  prefs->saveparts =
  	gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON
  				  (cbdata->checkautosaveparts));
  //g_print ("Timeout %d \n", prefs->autosave_timeout);

  /* Now write it all to denemorc */
  writeXMLPrefs (prefs);

}

void
preferences_change (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *lilypathentry;
  GtkWidget *browserentry;
  GtkWidget *checkimmediateplayback;
  GtkWidget *checklilystyle;
  GtkWidget	*checkautosaveparts;
  GtkWidget *checkclone;
  GtkWidget *checkautosave;
  GtkWidget *autosaveentry;
  GtkWidget *pdfviewer;
  GtkWidget *texteditor;
  GtkWidget *denemopath;
  GtkWidget *checknotationpalette;
  GtkWidget *checkrhythmpalette;
  GtkWidget *checkarticulationpalette;
  GtkWidget *notebook;
  GtkWidget *hbox;
  GtkWidget *vbox;
  GtkListStore *list_store;
  GtkWidget *tree;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  static struct callbackdata cbdata;
  static struct callbackdata1 cbdata1;
  g_assert (gui != NULL);

  dialog = gtk_dialog_new_with_buttons (_("Preferences - Denemo"),
					GTK_WINDOW (gui->window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  notebook = gtk_notebook_new ();
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		      TRUE, 0);

  /*
   * Note entry settings
   */
  table = gtk_table_new (13, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("Entry"));


  checkimmediateplayback =
    gtk_check_button_new_with_label (_
				     ("Play back entered notes immediately"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkimmediateplayback),
				Denemo.prefs.immediateplayback);
  gtk_table_attach (GTK_TABLE (table), checkimmediateplayback, 0, 2, 0, 1,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);

  checklilystyle =
    gtk_check_button_new_with_label (_("Use lilypond-style note entry"));
  /* Comment that out when the feature is added */
  gtk_widget_set_sensitive (checklilystyle, FALSE);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checklilystyle),
				Denemo.prefs.lilyentrystyle);
  gtk_table_attach (GTK_TABLE (table), checklilystyle, 0, 2, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  checkclone =
    gtk_check_button_new_with_label (_("Clone chords when tying them"));
  gtk_widget_set_sensitive (checkclone, FALSE);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkclone),
				Denemo.prefs.createclones);
  gtk_table_attach (GTK_TABLE (table), checkclone, 0, 2, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  checknotationpalette =
    gtk_check_button_new_with_label (_("Display durations toolbar"));
  gtk_widget_set_sensitive (checknotationpalette, TRUE);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checknotationpalette),
				Denemo.prefs.notation_palette);
  gtk_table_attach (GTK_TABLE (table), checknotationpalette, 0, 2, 4, 5,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  checkarticulationpalette =
    gtk_check_button_new_with_label (_("Display articulation palette"));
  gtk_widget_set_sensitive (checkarticulationpalette, TRUE);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkarticulationpalette),
				Denemo.prefs.articulation_palette);
  gtk_table_attach (GTK_TABLE (table), checkarticulationpalette, 0, 2, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);


  checkrhythmpalette =
    gtk_check_button_new_with_label (_("Display rhythm pattern toolbar"));
  gtk_widget_set_sensitive (checkrhythmpalette, TRUE);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkrhythmpalette),
				Denemo.prefs.rhythm_palette);
  gtk_table_attach (GTK_TABLE (table), checkrhythmpalette, 0, 2, 8, 9,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);





  hbox = gtk_hbox_new (FALSE, 8);
  gtk_table_attach (GTK_TABLE (table), hbox, 0, 2, 10, 11,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  checkautosave = gtk_check_button_new_with_label (_("Autosave every"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkautosave),
				Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), checkautosave, FALSE, FALSE, 0);

  autosaveentry = gtk_spin_button_new_with_range (1, 50, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (autosaveentry),
			     Denemo.prefs.autosave_timeout);
  gtk_widget_set_sensitive (autosaveentry, Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), autosaveentry, FALSE, FALSE, 0);

  label = gtk_label_new (_("minute(s)"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
  g_signal_connect (GTK_OBJECT (checkautosave),
		    "toggled", G_CALLBACK (toggle_autosave), autosaveentry);


  checkautosaveparts = gtk_check_button_new_with_label(_("Autosave Parts"));
  gtk_widget_set_sensitive (checkarticulationpalette, FALSE);
   gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkautosaveparts),
				Denemo.prefs.saveparts);
  gtk_table_attach(GTK_TABLE(table), checkautosaveparts, 0,2, 11,12,
  				   (GtkAttachOptions) (GTK_FILL),
  				   (GtkAttachOptions) (0), 0,0);
  /*
   * External (Helper) Programs 
   */
  table = gtk_table_new (4, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("Externals"));


  label = gtk_label_new (_("Path to Lilypond:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  lilypathentry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (lilypathentry), Denemo.prefs.lilypath->str);
  gtk_table_attach (GTK_TABLE (table), lilypathentry, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new (_("Pdf Viewer"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  pdfviewer = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (pdfviewer), Denemo.prefs.pdfviewer->str);
  gtk_table_attach (GTK_TABLE (table), pdfviewer, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  label = gtk_label_new (_("Text Edtior"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 2, 3,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  texteditor = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (texteditor), Denemo.prefs.texteditor->str);
  gtk_table_attach (GTK_TABLE (table), texteditor, 1, 2, 2, 3,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new (_("Default Save Path"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 3, 4,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  denemopath = gtk_entry_new ();
  if (Denemo.prefs.denemopath != NULL)
  	gtk_entry_set_text (GTK_ENTRY (denemopath), Denemo.prefs.denemopath->str);
  gtk_table_attach (GTK_TABLE (table), denemopath, 1, 2, 3, 4,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);


  /*
   * Plugins settings
   */
  vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 12);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), vbox, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), vbox,
				   _("Plugins"));

  list_store = gtk_list_store_new (2, G_TYPE_BOOLEAN, G_TYPE_STRING);

  if (gui->plugins)
    {
      GList *tmp;
      for (tmp = gui->plugins; tmp; tmp = tmp->next)
	{
	  gtk_list_store_append (list_store, &iter);
	  gtk_list_store_set (list_store, &iter,
			      COLUMN_LOADED, TRUE,
			      COLUMN_PLUGIN,
			      (gchar *) ((PluginData *) tmp->data)->title,
			      -1);

	}
    }

  GList *plugins = NULL;
  plugins = get_plugins_list (plugins);
  if (plugins)
    {
      GList *tmp;
      //g_print ("list of plugins is populated\n");
      for (tmp = plugins; tmp; tmp = tmp->next)
	{
	  gtk_list_store_append (list_store, &iter);
	  gtk_list_store_set (list_store, &iter,
			      COLUMN_LOADED, FALSE,
			      COLUMN_PLUGIN, (gchar *) tmp->data, -1);
	}
    }

  gtk_list_store_append (list_store, &iter);
  gtk_list_store_set (list_store, &iter,
		      COLUMN_LOADED, FALSE,
		      COLUMN_PLUGIN, "Dummy plugin 1", -1);

  gtk_list_store_append (list_store, &iter);
  gtk_list_store_set (list_store, &iter,
		      COLUMN_LOADED, TRUE,
		      COLUMN_PLUGIN, "Dummy plugin 2", -1);

  tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list_store));
  gtk_box_pack_start (GTK_BOX (vbox), tree, TRUE, TRUE, 0);
  g_object_unref (G_OBJECT (list_store));
  cbdata1.gui = gui;
  cbdata1.model = list_store;
  renderer = gtk_cell_renderer_toggle_new ();
  g_signal_connect (renderer, "toggled", G_CALLBACK (toggle_plugin),
		    &cbdata1);
  column =
    gtk_tree_view_column_new_with_attributes ("Enabled", renderer, "active",
					      COLUMN_LOADED, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

  column = gtk_tree_view_column_new_with_attributes ("Plugin",
						     gtk_cell_renderer_text_new
						     (), "text",
						     COLUMN_PLUGIN, NULL);

  gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);

  /*
   * Help settings
   */
  table = gtk_table_new (1, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
  gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
				   _("Help Settings"));


  label = gtk_label_new (_("Help Browser:"));
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  browserentry = gtk_entry_new ();
  gtk_entry_set_text (GTK_ENTRY (browserentry), Denemo.prefs.browser->str);
  gtk_table_attach (GTK_TABLE (table), browserentry, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);



  /* Set up the callback data */
  cbdata.prefs = &Denemo.prefs;
  cbdata.lilypathentry = lilypathentry;
  cbdata.checkimmediateplayback = checkimmediateplayback;
  cbdata.checklilystyle = checklilystyle;
  cbdata.checkautosaveparts = checkautosaveparts;
  cbdata.checkarticulationpalette = checkarticulationpalette;
  cbdata.checknotationpalette = checknotationpalette;
  cbdata.checkrhythmpalette = checkrhythmpalette;
  cbdata.checkclone = checkclone;
  cbdata.checkautosave = checkautosave;
  cbdata.autosaveentry = autosaveentry;
  cbdata.browserentry = browserentry;
  cbdata.pdfviewer = pdfviewer;
  cbdata.texteditor = texteditor;
  cbdata.denemopath = denemopath;

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_entry_set_activates_default (GTK_ENTRY (lilypathentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (autosaveentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (browserentry), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (pdfviewer), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (texteditor), TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY (denemopath), TRUE);
  
  gtk_widget_grab_focus (lilypathentry);
  gtk_widget_show_all (dialog);
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      set_preferences (&cbdata);
    }
  gtk_widget_destroy (dialog);
}

GList *
get_plugins_list (GList * plugins)
{
  DIR *dir;
  struct dirent *dirent;
  char *name;
  //dirname = g_strconcat(dirname, "/denemo/", NULL);
  //g_print ("Directory %s\n", get_plugin_dir ());
  dir = opendir (get_plugin_dir ());

  if (!dir)
    return NULL;

  while ((dirent = readdir (dir)) != NULL)
    {
      //g_print ("Filename is %s\n", dirent->d_name);
      if ((name = stripname (dirent->d_name)) != NULL)
	plugins = g_list_append (plugins, name);
    }

  return (plugins);
}
