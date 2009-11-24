#include <gtk/gtk.h>
#include "jackmidi.h"

GtkWidget           *view;
GtkTreeSelection *selection;
GtkTreeModel        *model;

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

/** 
 * Get the device number of the selected device
 */
static gint
get_device_number(){
  gint i;
  gchar *name = get_selection_as_char();
  if (!name)
    return -1;
  else { 
    gchar port_name[10];
    for(i=0;i<maxnumber_of_clients();i++){
      sprintf(port_name, "%s%d", "Jack:", i);
      if (!strcmp(name, port_name))
	return i;
    }
  }
  return -1;  
}

/** 
 * Get the port number of the selected port 
 */
static gint
get_port_number(){
  gint i;
  gchar *name = get_selection_as_char();
  if (!name)
    return -1;
  else { 
    gchar port_name[10];
    for(i=0;i<maxnumber_of_ports();i++){
      sprintf(port_name, "%s%d", "Denemo:", i);
      if (!strcmp(name, port_name))
	return i;
    }
  }
  return -1;  
}

void device_manager_create_device()
{
  if(create_jack_midi_client() >= 0){
    g_debug("\nJust added device\n");
    //add to or refresh GtkTree 
  }
}

void device_manager_remove_device()
{
  if(remove_jack_midi_client() >= 0){
    g_debug("\nJust removed device\n");
    //remove or refresh GtkTree 
  }
}

void device_manager_create_port()
{
  gint device_number = get_device_number();
  if (device_number <0)
    return;
  if(create_jack_midi_port(device_number) >= 0){
    g_debug("\nJust created midi device\n");
    //add to or refresh GtkTree 
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

enum
{
  COL_DEVICE = 0,
  NUM_COLS
} ;

static GtkTreeModel *
create_and_fill_model (void)
{
  GtkTreeStore  *treestore;
  GtkTreeIter    toplevel, child;

  treestore = gtk_tree_store_new(NUM_COLS,
                                 G_TYPE_STRING);

  /* Append a top level row and leave it empty */
  gtk_tree_store_append(treestore, &toplevel, NULL);
  gtk_tree_store_set(treestore, &toplevel,
                     COL_DEVICE, "Jack:0",
                     -1);

  /* Append a second top level row, and fill it with some data */
  gtk_tree_store_append(treestore, &toplevel, NULL);
  gtk_tree_store_set(treestore, &toplevel,
                     COL_DEVICE, "Jack:1",
                     -1);

  /* Append a child to the second top level row, and fill in some data */
  gtk_tree_store_append(treestore, &child, &toplevel);
  gtk_tree_store_set(treestore, &child,
                     COL_DEVICE, "Denemo:0",
                     -1);

  return GTK_TREE_MODEL(treestore);
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

  model = create_and_fill_model();

  gtk_tree_view_set_model(GTK_TREE_VIEW(view), model);

  g_object_unref(model); /* destroy model automatically with view */

  gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(view)),
                              GTK_SELECTION_SINGLE);
  
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view)); 

  return view;
}


