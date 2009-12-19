#include <glib.h>
#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include "jackmidi.h"
#include "pitchentry.h"
#include "device_manager.h"

GtkWidget           *theview;
GtkTreeSelection *selection;
GtkTreeModel        *model;
GtkTreeStore  *treestore;
GtkTreeIter    toplevel, child, iter_parent;
GList *DevicePort_list;

enum
{
  COL_DEVICE = 0,
  NUM_COLS
} ;
 
static gchar *
get_selection_as_char(){
  GtkTreeIter iter;
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     (GtkTreeModel **) &treestore, &iter))
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
			     (GtkTreeModel **) &treestore, &iter_parent))
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
			     (GtkTreeModel **) &treestore, &iter))
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

static void 
append_to_drop_down_list(gint device_number, GList *n){
  gchar *DevicePortName;
  DevicePortName = g_strconcat(Denemo.prefs.midi_device[device_number].client_name->str,
		"->",
		((GString *) ((GList *) n)->data)->str,
		NULL);
  DevicePort_list = g_list_append(DevicePort_list, DevicePortName);
}

static void
clear_DevicePort_list(){
  g_list_foreach(DevicePort_list, (GFunc) g_free, NULL);
  //g_list_free(DevicePort_list);
  DevicePort_list = NULL;
}

void
device_manager_refresh_model(void)
{
  gint i;
  if(treestore)
    gtk_tree_store_clear(treestore); //clear tree
  clear_DevicePort_list();       //clear list

  for (i=0;Denemo.prefs.midi_device[i].client_name;i++){
    gtk_tree_store_append(treestore, &toplevel, NULL);
    gtk_tree_store_set(treestore, &toplevel,
		   COL_DEVICE, 
		   Denemo.prefs.midi_device[i].client_name->str, 
		   -1);


    /* Append port name as child to the second top level row*/
    GList *n = Denemo.prefs.midi_device[i].port_names;
    g_print("Putting client %s into model first port %p\n", Denemo.prefs.midi_device[i].client_name->str, n);

    while (n){
      gtk_tree_store_append(treestore, &child, &toplevel);
      gtk_tree_store_set(treestore, &child,
                     COL_DEVICE, 
		     ((GString *) ((GList *) n)->data)->str,
                     -1);
      g_print("Putting port %s into model\n", ((GString *) ((GList *) n)->data)->str);
      append_to_drop_down_list(i, n);
      n = n->next;
    }
  }
}

DevicePort *
device_manager_get_DevicePort(gchar *staff_DP){
  DevicePort *dp = (DevicePort*)g_malloc0(sizeof(DevicePort));
  GList *n;
  gint i;
  gint port_number = 0;
  gchar *DP_string;

  for (i=0;Denemo.prefs.midi_device[i].client_name;i++){
    GList *n = Denemo.prefs.midi_device[i].port_names;
    while (n){
      DP_string = g_strconcat(Denemo.prefs.midi_device[i].client_name->str,
		"->",
		((GString *) ((GList *) n)->data)->str,
		NULL);

      if (!strcmp(staff_DP, DP_string)){
        dp->device_number = i;
	dp->port_number = port_number;
	g_free(DP_string);
	return dp;
      }
      port_number++; 
      g_free(DP_string);
      n=n->next;
    }
  } 
    /* If it does not match any */
    dp->device_number = -1;
    dp->port_number = -1;
    return dp;
}

GList *
device_manager_DevicePort_list(){
  device_manager_refresh_model();
  return DevicePort_list;
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

static int
check_for_duplicate(gint device_number, gchar *new_name)
{
  GList *n;
  
  for (n=Denemo.prefs.midi_device[device_number].port_names;n;n=n->next){	
    gchar *s = (((GString *) ((GList *) n)->data)->str);
    if (!strcmp(s, new_name));
      return 0;
  } 
  return -1;
}

static void 
cell_edited (GtkCellRendererText* cellrenderertext,
	gchar* path_string, gchar* new_name,
	GtkTreeModel* treemodel)
{
  gint device_number;
  gint port_number;
  GtkTreePath *path = gtk_tree_path_new_from_string (path_string);
  GtkTreeIter iter;
  
  gtk_tree_model_get_iter (model, &iter, path);
  g_debug("\n***path_string = %s, new text == %s\n", path_string, new_name);
 
     gchar **device_path_str = g_strsplit(path_string,":",2);
     device_number = atoi(device_path_str[0]);
     if (device_path_str[1]){
	port_number = atoi(device_path_str[1]);
	if(!rename_jack_midi_port(device_number, port_number, new_name))
	  gtk_tree_store_set (treestore, &iter, 0, new_name, -1);
     } else {
	g_debug("can't change device name yet");
     }
     g_strfreev(device_path_str);
     //FIXME memory leak - use g_strrstr() instead g_free(device_path_str);
  gtk_tree_path_free (path);
}

GtkWidget *
DeviceManager (void)
{
  GtkTreeViewColumn   *col;
  GtkCellRenderer     *renderer;
  if(theview)
    return theview;
  theview = gtk_tree_view_new();

  col = gtk_tree_view_column_new();

  gtk_tree_view_column_set_title(col, "Device/Port");

  /* pack tree view column into tree view */
  gtk_tree_view_append_column(GTK_TREE_VIEW(theview), col);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "editable", TRUE, NULL);
  g_signal_connect(renderer, "edited", (GCallback)cell_edited, model); 

  /* pack cell renderer into tree view column */
  gtk_tree_view_column_pack_start(col, renderer, TRUE);

  /* connect 'text' property of the cell renderer to
   *  model column that contains the first name */
  gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DEVICE);

  model = create_model();
  
  gtk_tree_view_set_model(GTK_TREE_VIEW(theview), model);

  /* never destroy the view  g_object_unref(model);  destroy model automatically with view */

  gtk_tree_selection_set_mode(gtk_tree_view_get_selection(GTK_TREE_VIEW(theview)),
                              GTK_SELECTION_SINGLE);
  
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(theview)); 
   /* do not let it be destroyed */
  g_object_ref(theview);
  return theview;
}


