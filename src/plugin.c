/* plugin.c 
 * 
 * Denemo plugin handler, uses gmodule
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Adam Tee
 */

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <denemo/denemo.h>
#include <denemo/denemo_version.h>
#include <glib.h>
#include <gmodule.h>
#include "plugin.h"
#include "utils.h"


/*typedef void initialise (struct scoreinfo *si);*/


void
load_plugin (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  g_print ("In Load Plugin\n");

  if (gui)
    init_plugins (gui);
  else
    g_print ("Scoreinfo is 0x0");

}


/**
 * List Loaded Plugins
 *
 *
 */
void
list_available_plugins (GtkAction *action, gpointer param)
{

}


/**
 *
 * List loaded plugins 
 */
void
list_loaded_plugins (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *dialog;
  GtkTreeStore *tree_store;
  GtkWidget *tree;
  GtkTreeIter iter;
  GtkTreeViewColumn *column;
  GtkCellRenderer *renderer;
  GList *tmp = NULL;


  dialog = gtk_dialog_new_with_buttons (_("Loaded Plugins"), NULL,
					(GtkDialogFlags)
					(GTK_DIALOG_MODAL |
					 GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					NULL, NULL, NULL);


  gtk_widget_set_usize (GTK_WIDGET (dialog), 250, 150);

  /* 
   * CList to show loaded plugins
   */
  tree_store = gtk_tree_store_new (2, G_TYPE_STRING, G_TYPE_STRING);
  gtk_tree_store_append (tree_store, &iter, NULL);
  //clist = gtk_clist_new_with_titles (2, titles);
  //gtk_clist_set_column_width (GTK_CLIST (clist), 0, 150);

  if (gui->plugins)
    {
      for (tmp = gui->plugins; tmp; tmp = tmp->next)
	{
	  //item[0][0] = (gchar *) ((PluginData *) tmp->data)->title;
	  //item[0][1] = "Loaded";
	  //gtk_clist_append (GTK_CLIST (clist), item[0]);
	  gtk_tree_store_set (tree_store, &iter, 0,
			      (gchar *) ((PluginData *) tmp->data)->title,
			      1, "Loaded", -1);
	}
    }

  tree = gtk_tree_view_new_with_model (GTK_TREE_MODEL (tree_store));
  g_object_unref (G_OBJECT (tree_store));
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "foreground", "red", NULL);
  column = gtk_tree_view_column_new_with_attributes ("Plugin", renderer,
						     "text", 0, NULL);


  gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);


  renderer = gtk_cell_renderer_text_new ();
  column = gtk_tree_view_column_new_with_attributes ("Loaded",
						     renderer,
						     "text", 1, NULL);
  gtk_tree_view_append_column (GTK_TREE_VIEW (tree), column);


  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), tree, TRUE, TRUE,
		      0);
  gtk_widget_show (tree);

  gtk_widget_show (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    ;

  gtk_widget_destroy (dialog);
  if (tmp)
    g_list_free (tmp);
}

void
denemo_plugin_init (const gchar * name, DenemoGUI * gui)
{
  gchar *path;
  PluginData *data = NULL;
  /*initialise *init = NULL; */
/*  void (*initialise)(struct scoreinfo *);
  GList *gl;*/

  data = (PluginData *) g_new0 (PluginData, 1);

  if (!data)
    {
      g_print ("allocation error");
      return;
    }

  path = g_module_build_path (get_plugin_dir(), name);
  path = g_strdup_printf ("%s/lib%s.so.%d", get_plugin_dir(), name,
			  DENEMO_PLUGIN_API_MAJOR);

  data->handle = g_module_open (path, (GModuleFlags) 0);

  if (!data->handle)
    {
      fprintf (stderr, "Error opening %s: %s\n", path, g_module_error ());
      g_free (data);
      return;
    }
  gboolean foundsymbol = g_module_symbol (data->handle, "denemo_plugin_init",
					  (gpointer *) & data->initialise);

  if (!foundsymbol)
    {
      g_warning ("Can't find symbol denemo_plugin_init in %s", path);
      g_module_close (data->handle);
      g_free (data);
    }
  g_print ("Load Symbol\n");
  data->initialise (gui, data);

  gui->plugins = g_list_append (gui->plugins, data);
  
}


/**
 *  Clean up plugin
 *
 */
gint
denemo_plugin_cleanup (const gchar * name, DenemoGUI * gui)
{
  gint ret = -1;
  GList *temp = NULL;
  for (temp = gui->plugins; temp; temp = temp->next)
    {
      PluginData *plugin = (PluginData *) temp->data;
      // g_print("plugin name %s\n", plugin->title);
      if (plugin && (strcmp (plugin->title, name) == 0))
	{
	  plugin->clean_up (gui, plugin);
	  g_print ("Unloading Plugin\n");
	  gui->plugins = g_list_remove (gui->plugins, plugin);
	  gui->plugincounter--;
	  g_module_close (plugin->handle);
	  g_free (plugin);
	  ret = 1;
	}
    }
  return ret;
}

/**
 * Should unload all loaded modules.
 *
 *
 */
void
unloadplugins (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GList *temp = NULL;

  for (temp = gui->plugins; temp; temp = temp->next)
    {
      PluginData *plugin = (PluginData *) temp->data;
      if (plugin)
	{
	  plugin->clean_up (gui, plugin);
	  g_print ("Unloading Plugin\n");
	  gui->plugins = g_list_remove (gui->plugins, plugin);
	  gui->plugincounter--;
	  g_module_close (plugin->handle);
	  g_free (plugin);
	}
    }

}


/*
 * Stripname for matching plugins of form
 * lib(.*).so
 *
 */
gchar *
stripname (gchar * d_name)
{
  int len, blen;
  gchar buf[8];

  if (strncmp (d_name, "lib", 3))
    return NULL;

  len = strlen (d_name);
  snprintf (buf, 8, ".so.%d", DENEMO_PLUGIN_API_MAJOR);
  blen = strlen (buf);

  if (strncmp (&d_name[len - blen], buf, blen))
    return NULL;

  d_name[len - blen] = '\0';

  return &d_name[3];
}


static void
init_dynamic_plugins_dir (gchar * dirname, DenemoGUI * gui)
{
  GDir *dir;
  gchar *name, *filename;
  GError *error = NULL;
  dir = g_dir_open (dirname, 0, &error);
  if (error)
    {
      g_print ("Error opening plugin: %s\n", error->message);
      return;
    }
  while ((filename = (gchar *) g_dir_read_name (dir)) != NULL)
    {
      if ((name = stripname (filename)) != NULL)
	denemo_plugin_init (name, gui);
    }
}


static void
init_dynamic_plugins (DenemoGUI * gui)
{
  gchar *plugin_path = g_build_filename (get_plugin_dir (), "denemo", NULL);
  init_dynamic_plugins_dir (plugin_path, gui);
  g_free (plugin_path);
}


void
init_plugins (DenemoGUI * gui)
{
  printf ("Initialising plugins\n");
  if (g_module_supported ())
    init_dynamic_plugins (gui);

}
