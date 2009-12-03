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
GtkTreeStore  *treestore;
GtkTreeIter    toplevel, child, iter_parent;

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
  g_debug("\n***name = %s\n",name);
  return name;
}

/** 
 * Get the parent of the tree when child is selected
 */
static gchar *
get_device_selection_as_char(){
  gint err;
  gchar *name;
  GtkTreeIter iter;

  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     (GtkTreeModel **) &view, &iter_parent))
    return NULL;
  if(gtk_tree_model_iter_parent (model, &iter, &iter_parent))
      gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0,
		                           &name, -1);
  else 
      gtk_tree_model_get(GTK_TREE_MODEL(model), &iter_parent, 0,
				           &name, -1);

  
  g_debug("\n***name = %s\n",name);
  return name;
}

static void
remove_selection(){
   GtkTreeIter iter;
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     (GtkTreeModel **) &view, &iter))
    return;

  gtk_tree_store_remove(treestore, &iter);
}

add_device_to_tree(gchar *device_name){
  gtk_tree_store_append(treestore, &toplevel, NULL);
  gtk_tree_store_set(treestore, &toplevel,
  				COL_DEVICE, 
  				device_name, 
				-1);
}

add_port_to_tree(gchar *port_name){
  gtk_tree_store_append(treestore, &child, &iter_parent);
  gtk_tree_store_set(treestore, &child,
  				COL_DEVICE, 
  				port_name, 
				-1);
}

/** TODO this is obsolete. Search through the array instead 
 * Get the device number of the selected device
 */
static gint
get_device_number(){
  gint i;
  gchar *name = get_device_selection_as_char();
  if (!name)
    return -1;
  else { 
    gchar port_name[15]; //TODO replace with gstring for resizinging purposes
    for(i=0;i<maxnumber_of_clients();i++){
      sprintf(port_name, "%s:%d", (gchar *) jackmidi_default_client_name(), i);
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
    gchar port_name[15]; //TODO replace with gstring for resizinging purposes
    for(i=0;i<maxnumber_of_ports();i++){
      sprintf(port_name, "%s:%d", (gchar *) jackmidi_default_port_name(), i);
      if (!strcmp(name, port_name))
	return i;
    }
  }
  return -1;  
}

static GtkTreeModel *
create_model (void)
{
 
  treestore = gtk_tree_store_new(NUM_COLS,
                                 G_TYPE_STRING);

  return GTK_TREE_MODEL(treestore);
}

void
device_manager_refresh_model(void)
{
  gint i;
  gtk_tree_store_clear(treestore); //clear list
  
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
}

void device_manager_create_device()
{
  gint client_number = create_jack_midi_client();
  
  if (client_number >= 0){
    g_debug("\nJust added device\n");
    add_device_to_tree(Denemo.prefs.midi_device[client_number].client_name->str);
  }
}

void device_manager_remove_device()
{
  gint device_number = get_device_number();
  if (device_number<0)
    return;
  remove_jack_midi_client(get_device_number()); 
  g_debug("\nJust removed device\n");
  remove_selection();
}

void device_manager_create_port()
{
  gint port_number;
  gint device_number;
  GList *n;
  gchar *port_name;
  device_number = get_device_number();
  
  if (device_number<0)
    return;
  port_number = create_jack_midi_port(device_number);
  if (port_number >= 0){
    n = g_list_nth(Denemo.prefs.midi_device[device_number].port_names, port_number);
    port_name = ((GString *) ((GList *) n)->data)->str; 
    g_debug("\nJust created midi device\n");
    add_port_to_tree(port_name);
    //device_manager_refresh_model();
  }
}

void device_manager_remove_port()
{
  gint device_number = get_device_number();
  gint port_number = get_port_number();
  if (device_number <0 || port_number <0)          
    return;
  if(remove_jack_midi_port(device_number, port_number) >= 0){
    g_debug("\nJust removed midi device\n");
    remove_selection();
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

  model = create_model();
  
  gtk_tree_view_set_model(GTK_TREE_VIEW(view), model);

  g_object_unref(model); /* destroy model automatically with view */

  gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(view)),
                              GTK_SELECTION_SINGLE);
  
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view)); 

  return view;
}


