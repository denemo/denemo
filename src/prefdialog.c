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
#ifdef _HAVE_JACK_
#include "jackmidi.h"
#endif

struct callbackdata
{
  DenemoPrefs *prefs;
  GtkWidget *lilypath;
  GtkWidget *immediateplayback;
  GtkWidget *saveparts;
  GtkWidget *autosave;
  GtkWidget *notation_palette;
  GtkWidget *rhythm_palette;
  GtkWidget *object_palette;
  GtkWidget *articulation_palette;
  GtkWidget *visible_directive_buttons;
  GtkWidget *autoupdate;

  GtkWidget *autosave_timeout;
  GtkWidget *maxhistory;
  GtkWidget *browser;
  GtkWidget *pdfviewer;
  GtkWidget *imageviewer;
  GtkWidget *sequencer;
  GtkWidget *midi_in;
#ifdef _HAVE_JACK_
  GtkWidget *jacktransport;
  GtkWidget *jacktransport_start_stopped;
  GtkWidget *jack_at_startup;
#endif
  GtkWidget *texteditor;
  GtkWidget *denemopath;
  GtkWidget *temperament;
  GtkWidget *strictshortcuts;
  GtkWidget *resolution;
  GtkWidget *overlays;
  GtkWidget *continuous;
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
toggle_autosave (GtkToggleButton * togglebutton, GtkWidget * autosave_timeout)
{
  g_debug("autosave now %d\n", 
     gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(autosave_timeout)));
  gtk_widget_set_sensitive (autosave_timeout,
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


#define ASSIGNTEXT(field) \
  g_string_assign (prefs->field,\
                   gtk_entry_get_text (GTK_ENTRY (cbdata->field)));

#define ASSIGNBOOLEAN(field) \
  prefs->field =\
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cbdata->field));

#define ASSIGNINT(field) \
   prefs->field =\
    gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(cbdata->field));


  ASSIGNTEXT(lilypath)
  ASSIGNTEXT(browser) 
  ASSIGNTEXT(pdfviewer)
  ASSIGNTEXT(imageviewer)
  ASSIGNTEXT(texteditor)
  ASSIGNTEXT(denemopath)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
  ASSIGNTEXT(sequencer)
  ASSIGNTEXT(midi_in)
#ifdef _HAVE_JACK_
  ASSIGNBOOLEAN(jacktransport)
  ASSIGNBOOLEAN(jacktransport_start_stopped)
  ASSIGNBOOLEAN(jack_at_startup)
#endif
  ASSIGNTEXT(temperament)
  ASSIGNBOOLEAN(strictshortcuts)
  ASSIGNBOOLEAN(overlays)
  ASSIGNBOOLEAN(continuous)
  ASSIGNINT(resolution)
  ASSIGNINT(maxhistory)
  ASSIGNBOOLEAN(immediateplayback)
  ASSIGNBOOLEAN(autosave)
  ASSIGNINT(autosave_timeout)
  ASSIGNBOOLEAN(articulation_palette)
  ASSIGNBOOLEAN(visible_directive_buttons)
  ASSIGNBOOLEAN(autoupdate)
  ASSIGNBOOLEAN(notation_palette)
  ASSIGNBOOLEAN(rhythm_palette)
  ASSIGNBOOLEAN(object_palette)
  ASSIGNBOOLEAN(saveparts)
  //g_print ("Timeout %d \n", prefs->autosave_timeout);

  /* Now write it all to denemorc */
  writeXMLPrefs (prefs);
}

void
preferences_change (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *main_vbox;
  GtkWidget *autosave;
  GtkWidget *autosave_timeout;
  GtkWidget *maxhistory;
  GtkWidget *notebook;
  GtkWidget *hbox;
  GtkWidget *vbox;
  GtkListStore *list_store;
  GtkWidget *tree;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GtkWidget *entrywidget;
  static struct callbackdata cbdata;
  g_assert (gui != NULL);

  dialog = gtk_dialog_new_with_buttons (_("Preferences - Denemo"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_STOCK_CANCEL,
					NULL);

  gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  notebook = gtk_notebook_new ();
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), notebook, TRUE,
		      TRUE, 0);

#define NEWPAGE(thelabel) \
    main_vbox = gtk_vbox_new (FALSE, 1);\
    gtk_notebook_append_page (GTK_NOTEBOOK (notebook), main_vbox, NULL);\
    gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), main_vbox,\
                                                           _(thelabel));

#define BOOLEANENTRY(thelabel, field) \
  GtkWidget *field =\
    gtk_check_button_new_with_label (thelabel); \
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (field),\
				Denemo.prefs.field);\
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

#define INTENTRY(thelabel, field) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  field = gtk_spin_button_new_with_range (1, 50, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

#define INTENTRY_LIMITS(thelabel, field, min, max) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);\
  label = gtk_label_new (thelabel);\
  gtk_misc_set_alignment (GTK_MISC (label), 1, 0.5);\
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_spin_button_new_with_range (min, max, 1.0);\
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (field), Denemo.prefs.field);\
  gtk_box_pack_start (GTK_BOX (hbox), field, FALSE, FALSE, 0);\
  cbdata.field = field;

  /*
   * Note entry settings
   */
  
  NEWPAGE("View");
  
  BOOLEANENTRY("Play back entered notes immediately", immediateplayback);  
  BOOLEANENTRY("Display duration toolbar", notation_palette);
  BOOLEANENTRY("Display articulation palette", articulation_palette);
  BOOLEANENTRY("Display Titles. Controls etc", visible_directive_buttons);

  BOOLEANENTRY("Display rhythm pattern toolbar", rhythm_palette);
  BOOLEANENTRY("Display object menu toolbar", object_palette);


  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  autosave = gtk_check_button_new_with_label (_("Autosave every"));
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (autosave),
				Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), autosave, FALSE, FALSE, 0);

  autosave_timeout = gtk_spin_button_new_with_range (1, 50, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (autosave_timeout),
			     Denemo.prefs.autosave_timeout);
  gtk_widget_set_sensitive (autosave_timeout, Denemo.prefs.autosave);
  gtk_box_pack_start (GTK_BOX (hbox), autosave_timeout, FALSE, FALSE, 0);
  g_debug("autosave %p\n", autosave);
  label = gtk_label_new (_("minute(s)"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);
  g_signal_connect (GTK_OBJECT (autosave),
		    "toggled", G_CALLBACK (toggle_autosave), autosave_timeout);


  BOOLEANENTRY("Autosave Parts", saveparts);
  TEXTENTRY("Sequencer Device", sequencer)
  TEXTENTRY("Midi Input Device", midi_in)


  /*
   * Pitch Entry Parameters 
   */
  NEWPAGE("Pitch Entry");

  TEXTENTRY("Temperament", temperament)
  BOOLEANENTRY("Use Overlays", overlays);
  BOOLEANENTRY("Continuous Entry", continuous);

  /*
   * Shortcut control 
   */
  NEWPAGE("Shortcuts");

  //  TEXTENTRY("Strict", strictshortcuts)
  BOOLEANENTRY("Strict Shortcuts", strictshortcuts);

  /*
   * External (Helper) Programs 
   */
  NEWPAGE("Externals");
 
  TEXTENTRY("Path to Lilypond", lilypath)
  TEXTENTRY("Pdf Viewer", pdfviewer)
  TEXTENTRY("Image Viewer", imageviewer)
  TEXTENTRY("Text Editor", texteditor)
  TEXTENTRY("Default Save Path", denemopath)
  BOOLEANENTRY("Update the command set on startup", autoupdate);
  /*
   * Plugins settings
   */
  /*
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
  */
 
  /*
   * Excerpt Menu 
   */
  NEWPAGE("Excerpt");
 
  INTENTRY_LIMITS(_("resolution"), resolution, 72, 600);

  /*
   * Jack Menu
   */
#ifdef _HAVE_JACK_
  NEWPAGE("JACK");
  BOOLEANENTRY("Enable Jack Transport", jacktransport);
  BOOLEANENTRY("Jack Transport starts stopped", jacktransport_start_stopped);
  BOOLEANENTRY("Enable Jack at startup", jack_at_startup);
  /* Start/Restart Button */
  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (main_vbox), hbox, FALSE, TRUE, 0);
  GtkWidget *jack_activate = gtk_button_new_with_label("Start/Restart Jack Client");
  gtk_box_pack_start (GTK_BOX (hbox), jack_activate, FALSE, FALSE, 0);
  g_signal_connect (G_OBJECT (jack_activate), "clicked",
    G_CALLBACK (jack_start_restart), (gpointer) NULL);

#endif


  /*
   * Help settings
   */
  NEWPAGE("Help Settings")
  TEXTENTRY("Help Browser", browser)
  INTENTRY(_("Max recent files"), maxhistory)

  /* Set up the callback data */

#define SETCALLBACKDATA(field) \
  cbdata.field = field;
  
  cbdata.prefs = &Denemo.prefs;
  SETCALLBACKDATA(autosave);
  SETCALLBACKDATA(autosave_timeout); 
  SETCALLBACKDATA(maxhistory);
  
  //gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  //gtk_entry_set_activates_default (GTK_ENTRY (lilypath), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (autosave_timeout), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (maxhistory), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (browser), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (pdfviewer), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (texteditor), TRUE);
  //gtk_entry_set_activates_default (GTK_ENTRY (denemopath), TRUE);
  
  //gtk_widget_grab_focus (lilypath);
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
