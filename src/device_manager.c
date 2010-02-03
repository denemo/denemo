#include <glib.h>
#include <math.h>
#include <string.h>
#include <gtk/gtk.h>
#include "jackmidi.h"
#include "pitchentry.h"
#include "device_manager.h"

#define MD Denemo.prefs.midi_device

enum
{
  COL_DEVICE = 0,
  NUM_COLS
} ;
 
static gchar *
get_selection_as_char(GtkWidget *view){
  GtkTreeIter iter;
  GtkTreeSelection *selection;
  GtkTreeModel *model;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     &model, &iter))
    return NULL;
  gchar *name;
  gtk_tree_model_get(GTK_TREE_MODEL(model),
		  		&iter, 0,
		                &name, -1);

  g_debug("\n***name = %s\n",name);
  return name;
}

/** 
 * Get the parent of the tree when child is selected
 */
static gchar *
get_device_selection_as_char(gpointer v){
  gint err;
  gchar *name;
  GtkTreeIter iter;
  GtkTreeIter parent;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkWidget *view = (GtkWidget *) v;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     &model, &parent))
    return NULL;

  if(gtk_tree_model_iter_parent (GTK_TREE_MODEL(model), &iter, &parent))
      gtk_tree_model_get(GTK_TREE_MODEL(model), &iter, 0,
		                           &name, -1);
  else 
      gtk_tree_model_get(GTK_TREE_MODEL(model), &parent, 0,
				           &name, -1);

  g_debug("\n***name = %s\n",name);
  return name;
}

static void
remove_selection(GtkWidget *view){
  GtkTreeIter iter;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeStore *store;
  
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			     &model, &iter))
    return;
  store = GTK_TREE_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(view)));

  gtk_tree_store_remove(store, &iter);
}

static void 
add_device_to_tree(gchar *device_name, GtkWidget *view){
  GtkTreeIter iter;
  GtkTreeStore *store;
 
  store = GTK_TREE_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(view)));

  gtk_tree_store_append(store, &iter, NULL);
  gtk_tree_store_set(store, &iter,
  				COL_DEVICE, 
  				device_name, 
				-1);
}

static void
add_port_to_tree(gchar *port_name, GtkWidget *view){
  GtkTreeIter iter;
  GtkTreeIter parent;
  GtkTreeSelection *selection;
  GtkTreeModel *model;
  GtkTreeStore *store;

  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(view));
  if(!gtk_tree_selection_get_selected(GTK_TREE_SELECTION(selection),
			   &model, &parent))
    return;

  store = GTK_TREE_STORE(gtk_tree_view_get_model
		(GTK_TREE_VIEW(view)));
  gtk_tree_store_append(store, &iter, &parent);
  gtk_tree_store_set(store, &iter,
  				COL_DEVICE, 
  				port_name, 
				-1);
}

#define ARRAY Denemo.prefs.midi_device_array
static gint
get_parent_device_number(GtkWidget *view){
  gint i;
  gchar *name = get_device_selection_as_char(view);
  DeviceManagerDevice *d;
  if (!name)
    return -1; 
  else { 
    for (i=0;i<ARRAY->len;i++){
      d = &g_array_index(ARRAY, DeviceManagerDevice, i);
      if(!strcmp(name, d->client_name->str))
	return i;
    }
  }
  return -1;  
}
#undef ARRAY

#define ARRAY Denemo.prefs.midi_device_array
static gint
get_device_number(GtkWidget *view){
  gint i;
  gchar *name = get_selection_as_char(view);
  DeviceManagerDevice *d;
  if (!name)
    return -1; 
  else { 
    for (i=0;i<ARRAY->len;i++){
      d = &g_array_index(ARRAY, DeviceManagerDevice, i);
      if(!strcmp(name, d->client_name->str))
	return i;
    }
  }
  return -1;  
}
#undef ARRAY

#define ARRAY Denemo.prefs.midi_device[get_parent_device_number(view)].ports_array
static gint
get_port_number(GtkWidget *view){
  gint i;
  DeviceManagerPort *p;
  gchar *name = get_selection_as_char(view);
  
  if (!name)
    return -1;
  else { 
    for(i=0;i<ARRAY->len;i++){
      p = &g_array_index(ARRAY, DeviceManagerPort, i);
      if (!strcmp(name, p->port_name->str))
	return i;
    }
  }
  return -1;  
}
#undef ARRAY

void
device_manager_refresh_model(GtkWidget *view)
{
  GtkTreeIter parent; 
  GtkTreeIter child;
  GtkTreeStore *treestore;
  gint i,j;

  treestore = GTK_TREE_STORE(gtk_tree_view_get_model
		       (GTK_TREE_VIEW(view)));
  if(MD==NULL)
    return;
  if(treestore)
    gtk_tree_store_clear(treestore); //clear tree

  for (i=0;MD[i].client_name;i++){
    gtk_tree_store_append(treestore, &parent, NULL);
    gtk_tree_store_set(treestore, &parent,
		   COL_DEVICE, 
		   MD[i].client_name->str, 
		   -1);

    if( MD[i].ports==NULL)
      return;
    /* Append port name as child to the second top level row*/
    for(j=0;MD[i].ports[j].port_name;j++){
	g_print("Putting client %s into model  port %d\n", MD[i].client_name->str, j);

  
      gtk_tree_store_append(treestore, &child, &parent);
      gtk_tree_store_set(treestore, &child,
                     COL_DEVICE,
			 MD[i].ports[j].port_name->str,
                     -1);
      g_print("Putting port %s into model\n", MD[i].ports[j].port_name->str);
    }
  }
}
DevicePort *
device_manager_get_DevicePort(gchar *staff_DP){
  if(Denemo.prefs.midi_device==NULL)
    return NULL;
  DevicePort *dp = (DevicePort*)g_malloc0(sizeof(DevicePort));

  GList *n;
  gint i, j;
  gint port_number = 0;
  gchar *DP_string;

  for (i=0;MD[i].client_name;i++){
    for(j=0;MD[i].ports[j].port_name;j++) {
      DP_string = g_strconcat(MD[i].client_name->str,
		"->",
		MD[i].ports[j].port_name->str,
		NULL);

      if (!strcmp(staff_DP, DP_string)){
        dp->device_number = i;
	dp->port_number = j;
	g_free(DP_string);
	return dp;
      }
      g_free(DP_string);
    }
  }

    /* If it does not match any */
  
    return NULL;
}

GList *
device_manager_DevicePort_list(){
  GList *DevicePort_list = NULL;
  gchar *DevicePortName;
  gint i,j;

  for (i=0;MD[i].client_name;i++){

   
    if( MD[i].ports!=NULL)
      for(j=0;MD[i].ports[j].port_name;j++){
        DevicePortName = g_strconcat(MD[i].client_name->str,
		"->",
		MD[i].ports[j].port_name->str,
		NULL);
        DevicePort_list = g_list_append(DevicePort_list, DevicePortName);
      }
    }
  return DevicePort_list;
}

#define ARRAY Denemo.prefs.midi_device_array
void device_manager_create_device(GtkWidget *button, gpointer v)
{
  GtkWidget *view = GTK_WIDGET(v);
  stop_jack();
  if(ARRAY == NULL)
    ARRAY = g_array_new(TRUE, TRUE, sizeof(DeviceManagerDevice));
  g_array_set_size(ARRAY, ARRAY->len+1);
  Denemo.prefs.midi_device = (DeviceManagerDevice *)ARRAY->data;
  DeviceManagerDevice *d = &g_array_index(ARRAY, DeviceManagerDevice, ARRAY->len-1);
  d->client_name = g_string_new("denemo");
  g_string_append_printf(d->client_name, "_%d", ARRAY->len);
  // add_device_to_tree(Denemo.prefs.midi_device[client_number]client_name->str);
 add_device_to_tree(d->client_name->str, view);
 g_print("added device index %d\n", ARRAY->len-1);
}

void device_manager_remove_device(GtkWidget *button, gpointer v)
{
  GtkWidget *view = GTK_WIDGET(v);
  stop_jack();
  gint j = get_device_number(view);
  if (j<0)
    return;
  g_array_remove_index(ARRAY, j);
  Denemo.prefs.midi_device = (DeviceManagerDevice *)ARRAY->data;  
  g_debug("\nJust removed device %d\n", j);
  remove_selection(view);
}
#undef ARRAY


void device_manager_create_port(GtkWidget *button, gpointer v)
{
#define ARRAY Denemo.prefs.midi_device[j].ports_array
  GtkWidget *view = GTK_WIDGET(v);
  stop_jack();
  gint j = get_device_number(view);
  if(j<0)
    return;
  if(ARRAY==NULL) {
    ARRAY = g_array_new(TRUE, TRUE, sizeof(DeviceManagerPort));
  }
  g_array_set_size(ARRAY, ARRAY->len+1);
  Denemo.prefs.midi_device[j].ports = (DeviceManagerPort *)ARRAY->data;
  DeviceManagerPort *p = &g_array_index(ARRAY, DeviceManagerPort, ARRAY->len-1);
  p->port_name = g_string_new("midi_out");
  g_string_append_printf(p->port_name, "_%d", ARRAY->len);
  add_port_to_tree(p->port_name->str, view);
  g_print("added port index %d\n", ARRAY->len-1);
}

void device_manager_remove_port(GtkWidget *button, gpointer v)
{
  GtkWidget *view = GTK_WIDGET(v);
  gint device_number = get_parent_device_number(view);
  gint port_number = get_port_number(view);
  g_debug("\nRemove device #%d port #%d\n", device_number, port_number);
#if 0
  if (device_number <0 || port_number <0)          
    return;
  if(remove_jack_midi_port(device_number, port_number) >= 0){
    g_debug("\nJust removed midi device\n");
    remove_selection();
  }
#endif
}
#undef ARRAY

static int
check_for_duplicate(gint device_number, gchar *new_name)
{
#if 0
  GList *n;
  
  for (n=Denemo.prefs.midi_device[device_number].port_names;n;n=n->next){	
    gchar *s = (((GString *) ((GList *) n)->data)->str);
    if (!strcmp(s, new_name));
      return 0;
  } 
#endif
  return -1;
}

static void 
cell_edited (GtkCellRendererText* cellrenderertext,
	gchar* path_string, gchar* new_name,
	GtkTreeModel* treemodel)
{
  g_debug("\n***path_string = %s***\n", path_string);
#if 0
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
#endif
}

void 
DeviceManager (GtkWidget *main_vbox)
{
  GtkTreeViewColumn   *col;
  GtkCellRenderer     *renderer;
  GtkTreeStore        *treestore;
  GtkWidget 	      *hbox;
  GtkWidget	      *view;

  view = gtk_tree_view_new();

#define BUTTON(thelabel, field, thecallback, data) \
  hbox = gtk_hbox_new (FALSE, 8);\
  gtk_box_pack_start (GTK_BOX (vbox1), hbox, FALSE, FALSE, 0);\
  GtkWidget *field = gtk_button_new_with_label(thelabel);\
  gtk_box_pack_start (GTK_BOX (vbox1), field, FALSE, FALSE, 0);\
  g_signal_connect (G_OBJECT (field), "clicked",\
  G_CALLBACK (thecallback), (gpointer) data);

  col = gtk_tree_view_column_new();

  gtk_tree_view_column_set_title(col, "Device/Port");

  /* pack tree view column into tree view */
  gtk_tree_view_append_column(GTK_TREE_VIEW(view), col);

  renderer = gtk_cell_renderer_text_new();
  g_object_set(renderer, "editable", TRUE, NULL);
  g_signal_connect(renderer, "edited", (GCallback)cell_edited, GTK_TREE_MODEL(treestore)); 

  /* pack cell renderer into tree view column */
  gtk_tree_view_column_pack_start(col, renderer, TRUE);

  /* connect 'text' property of the cell renderer to
   *  model column that contains the first name */
  gtk_tree_view_column_add_attribute(col, renderer, "text", COL_DEVICE);

  treestore = gtk_tree_store_new(NUM_COLS, G_TYPE_STRING);

  gtk_tree_view_set_model(GTK_TREE_VIEW(view), GTK_TREE_MODEL(treestore));

  /* never destroy the view  g_object_unref(model);  destroy model automatically with view */
  /* do not let it be destroyed */
  g_object_ref(view);

  GtkWidget *mhbox = gtk_hbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (main_vbox), mhbox, FALSE, FALSE, 0);

  GtkWidget *vbox1 = gtk_vbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (mhbox), vbox1, FALSE, FALSE, 0);

  BUTTON("Add Device", midi_add_device, device_manager_create_device, GTK_TREE_VIEW(view)); 
  BUTTON("Remove Device", midi_remove_device, device_manager_remove_device, GTK_TREE_VIEW(view)); 
  BUTTON("Add Port", midi_device_add_port, device_manager_create_port, GTK_TREE_VIEW(view));
  BUTTON("Remove Port", midi_device_remove_port, device_manager_remove_port, GTK_TREE_VIEW(view));

  GtkWidget *vbox2 = gtk_vbox_new(FALSE, 5);
  gtk_box_pack_start (GTK_BOX (mhbox), vbox2, FALSE, FALSE, 0);

  gtk_box_pack_start (GTK_BOX (vbox2), view, FALSE, FALSE, 0);
  gtk_widget_show (view);
  device_manager_refresh_model(view);
}


