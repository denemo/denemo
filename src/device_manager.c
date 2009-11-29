#include <glib.h>
#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include "jackmidi.h"
#include "pitchentry.h"
#include "device_manager.h"

GtkWidget           *view;
GtkTreeSelection *selection;
GtkTreeModel        *model;

enum
{
  COL_DEVICE = 0,
  NUM_COLS
} ;

static gchar *
get_selection_as_char(){
  GtkTreeIter iter;

  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     (GtkTreeModel **) &view, &iter))
    return NULL;
  gchar *name;
  gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0,
		                           &name, -1);
  return name;
}

/** TODO this is obsolete. Search through the array instead 
 * Get the device number of the selected device
 */
static gint
get_device_number(){
  gint i;
  gchar *name = get_selection_as_char();
  if (!name)
    return -1;
  else { 
    gchar port_name[12];
    for(i=0;i<maxnumber_of_clients();i++){
      sprintf(port_name, "%s:%d", jackmidi_default_client_name(), i);
      if (!strcmp(name, port_name))
	return i;
    }
  }
  return -1;  
}

/** TODO this is obsolete. This should search the GList and find position
 * Get the port number of the selected port 
 */
static gint
get_port_number(){
  gint i;
  gchar *name = get_selection_as_char();
  if (!name)
    return -1;
  else { 
    gchar port_name[12];
    for(i=0;i<maxnumber_of_ports();i++){
      sprintf(port_name, "%s:%d", jackmidi_default_port_name(), i);
      if (!strcmp(name, port_name))
	return i;
    }
  }
  return -1;  
}

static GtkTreeModel *
refresh_model (void)
{
  GtkTreeStore  *treestore;
  GtkTreeIter    toplevel, child;
  gint i;

  treestore = gtk_tree_store_new(NUM_COLS,
                                 G_TYPE_STRING);

  for (i=0;Denemo.prefs.midi_device[i].client_name;i++){
    gtk_tree_store_append(treestore, &toplevel, NULL);
    gtk_tree_store_set(treestore, &toplevel,
		   COL_DEVICE, 
		   Denemo.prefs.midi_device[i].client_name->str, 
		   -1);


    /* Append port name as child to the second top level row*/
    GList *n = Denemo.prefs.midi_device[i].port_names;
    while (n){
      gtk_tree_store_append(treestore, &child, &toplevel);
      gtk_tree_store_set(treestore, &child,
                     COL_DEVICE, 
		     ((GString *) ((GList *) n)->data)->str,
                     -1);
      n = n->next;
    }
  }
  return GTK_TREE_MODEL(treestore);
}

void device_manager_create_device()
{
  if(create_jack_midi_client() >= 0){
    g_debug("\nJust added device\n");
    refresh_model();
  }
}

void device_manager_remove_device()
{
  if(remove_jack_midi_client() >= 0){
    g_debug("\nJust removed device\n");
    refresh_model();
  }
}

void device_manager_create_port()
{
  gint device_number = get_device_number();
  if (device_number <0)
    return;
  if(create_jack_midi_port(device_number) >= 0){
    g_debug("\nJust created midi device\n");
    refresh_model();
  }
}

void device_manager_remove_port()
{
  gint device_number = get_device_number();
  if (device_number <0)            
    return;
  get_selection_as_char();
  if(remove_jack_midi_port(device_number) >= 0){
    g_debug("\nJust removed midi device\n");
    //remove or refresh GtkTree 
  }
}

GtkWidget *
DeviceManager (void)
{
  GtkTreeViewColumn   *col;
  GtkCellRenderer     *renderer;

  view = gtk_tree_view_new();

  col = gtk_tree_view_column_new();

  gtk_tree_view_column_set_title(col, "Device/Port");

  /* pack tree view column into tree view */
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), col);

  renderer = gtk_cell_renderer_text_new();

  /* pack cell renderer into tree view column */
  gtk_tree_view_column_pack_start(col, renderer, TRUE);

  /* connect 'text' property of the cell renderer to
   *  model column that contains the first name */
  gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DEVICE);

  model = refresh_model();

  gtk_tree_view_set_model(GTK_TREE_VIEW(view), model);

  g_object_unref(model); /* destroy model automatically with view */

  gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(view)),
                              GTK_SELECTION_SINGLE);
  
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view)); 

  return view;
}


