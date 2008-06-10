/* kbd-interface.cpp
 *  functions for implementing the customize keyboard dialog
 *
 *  for Denemo, thu GNU graphical music notation editor
 *  (c) 2000-2005 Matthew Hiller 
 */

#include <config.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <string.h>

#include <gdk/gdkkeysyms.h>
#include <gtk/gtk.h>

#include "kbd-custom.h"
#include "prefops.h"

/*
 * Returns True if the key event is just a modifier key, False otherwise
 * TODO look for a gdk function doing that properly
 */
static gboolean
isModifier(GdkEventKey *event)
{
  /* This check for modifier values on the event may not be right,
     if the contents of gdkkeysyms.h are OS-dependent. I don't believe
     they are. */
  return event->keyval >= GDK_Shift_L && event->keyval <= GDK_Hyper_R;
}

static void
validate_keymap_name (GtkEntry * entry, GtkDialog * dialog)
{
  const gchar *name = gtk_entry_get_text (GTK_ENTRY (entry));
  gtk_dialog_set_response_sensitive (GTK_DIALOG (dialog),
				     GTK_RESPONSE_ACCEPT, strlen (name) > 0);
}

static gboolean
capture_add_binding(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  guint keyval;
  GdkModifierType modifiers;
  guint command_idx;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  GtkTreePath *path;
  gint *array;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  //get the shortcut
  if (isModifier(event))
      return TRUE;
  modifiers = dnm_sanitize_key_state(event);
  
  //get the command_index
  selection = gtk_tree_view_get_selection(cbdata->command_view);
  gtk_tree_selection_get_selected(selection, &model, &iter);
  path = gtk_tree_model_get_path(model, &iter);
  array = gtk_tree_path_get_indices(path);
  command_idx = array[0];
  gtk_tree_path_free(path);
  //set the new binding
  add_keybinding_from_idx(cbdata->the_keymap, event->keyval, modifiers,
          command_idx);
  //TODO? advertize on the status bar the fact a keybinding was stolen
  //clean the GUI
  gtk_statusbar_pop(cbdata->statusbar, cbdata->context_id);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_key_press);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_focus_out);
  
  return TRUE;
}

static gboolean
capture_look_binding(GtkWidget *widget, GdkEventKey *event, gpointer user_data)
{
  guint keyval;
  GdkModifierType modifiers;
  GtkTreeModel *model;
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  GtkTreePath *path;
  gint command_idx;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  //get the shortcut
  if (isModifier(event))
      return TRUE;
  modifiers = dnm_sanitize_key_state(event);
 
  //look for the keybinding
  command_idx = lookup_keybinding(cbdata->the_keymap, event->keyval, modifiers);
  //if the binding is associated to a command 
  if (command_idx != -1) {
      model = gtk_tree_view_get_model(cbdata->command_view);
      selection = gtk_tree_view_get_selection(cbdata->command_view);
      gtk_tree_model_iter_nth_child(model, &iter, NULL, command_idx);
      gtk_tree_selection_select_iter(selection, &iter);
      path = gtk_tree_model_get_path(model, &iter);
      gtk_tree_view_scroll_to_cell(cbdata->command_view, path, NULL, FALSE,
              0, 0);
      gtk_tree_path_free(path);
  }
      
  //clean the GUI
  gtk_statusbar_pop(cbdata->statusbar, cbdata->context_id);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_key_press);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_focus_out);
  
  return TRUE;
}

static gboolean
stop_capture_binding(GtkWidget *widget, GdkEventFocus *event,
        gpointer user_data)
{
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  gtk_statusbar_pop(cbdata->statusbar, cbdata->context_id);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_key_press);
  g_signal_handler_disconnect(GTK_WIDGET(widget), cbdata->handler_focus_out);
  return FALSE;
}

static void
kbd_interface_add_binding(GtkButton *button, gpointer user_data)
{
  GtkTreeSelection *selection;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  // check a command is selected
  selection = gtk_tree_view_get_selection(cbdata->command_view);
  if (!gtk_tree_selection_get_selected(selection, NULL, NULL))
      return;
  gtk_statusbar_push(cbdata->statusbar, cbdata->context_id,
          N_("Press a shortcut sequence"));
  cbdata->handler_key_press = g_signal_connect(GTK_WIDGET(button),
          "key-press-event", G_CALLBACK(capture_add_binding), user_data);
  cbdata->handler_focus_out = g_signal_connect(GTK_WIDGET(button),
          "focus-out-event", G_CALLBACK(stop_capture_binding), user_data);
}

static void
kbd_interface_look_binding(GtkButton *button, gpointer user_data)
{
  GtkTreeSelection *selection;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  gtk_statusbar_push(cbdata->statusbar, cbdata->context_id,
          N_("Press a shortcut sequence"));
  cbdata->handler_key_press = g_signal_connect(GTK_WIDGET(button),
          "key-press-event", G_CALLBACK(capture_look_binding), user_data);
  cbdata->handler_focus_out = g_signal_connect(GTK_WIDGET(button),
          "focus-out-event", G_CALLBACK(stop_capture_binding), user_data);
}

static void
kbd_interface_del_binding(GtkButton *button, gpointer user_data)
{
  GtkTreeSelection *selection;
  gchar *binding;
  GtkTreeModel *model;
  GtkTreeIter iter;
  guint command_idx;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  selection = gtk_tree_view_get_selection(cbdata->binding_view);
  //if no binding is selected, we do nothing
  if (!gtk_tree_selection_get_selected(selection, &model, &iter))
      return;
  //else get the binding and remove it
  gtk_tree_model_get(model, &iter, 0, &binding, -1);
  remove_keybinding_from_string(cbdata->the_keymap, binding);
  g_free(binding);
}

void
configure_keyboard_dialog (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *dialog;
  GtkWidget *vbox;
  GtkWidget *table;
  GtkWidget *hseparator;
  GtkWidget *label;
  GtkWidget *category;
  GtkWidget *command;
  GtkWidget *button;
  GtkWidget *addbutton;
  GtkWidget *delbutton;
  GtkWidget *lookbutton;
  GtkWidget *statusbar;
  GtkWidget *button_save;
  GtkWidget *button_save_as;
  GtkWidget *button_load;
  GtkWidget *button_load_from;
  GtkWidget *scrolledwindow;
  GtkWidget *treeview;
  GtkWidget *command_view;
  GtkWidget *binding_view;
  GtkWidget *command_tree_view;
  GtkWidget *binding_tree_view;
  GtkListStore *list_store = NULL;
  GtkCellRenderer *renderer;
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  gint i;
  guint context_id;
  keyboard_dialog_data cbdata;

  //getting a binding view and a command view and connecting the change of
  //command selection the the change of the model displayed by the binding view
  binding_view = keymap_get_binding_view();
  binding_tree_view = gtk_bin_get_child(GTK_BIN(binding_view));
  command_view = GTK_WIDGET(keymap_get_command_view(Denemo.prefs.the_keymap));
  command_tree_view = gtk_bin_get_child(GTK_BIN(command_view));
  
  dialog = gtk_dialog_new_with_buttons (_("Keyboard shortcuts"),
					GTK_WINDOW (gui->window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_CLOSE, GTK_RESPONSE_CLOSE,
					NULL);

  vbox = gtk_vbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), vbox, TRUE, TRUE,
		      0);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 12);

  table = gtk_table_new (2, 2, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);
  
  button_save = gtk_button_new_with_label (_("Save to Default Keymap File"));
  gtk_table_attach (GTK_TABLE (table), button_save, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);
  
  button_save_as =
    gtk_button_new_with_label (_("Save As Alternate Keymap File"));
  gtk_table_attach (GTK_TABLE (table), button_save_as, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  button_load = gtk_button_new_with_label (_("Load Default Keymap File"));
  gtk_table_attach (GTK_TABLE (table), button_load, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  button_load_from =
    gtk_button_new_with_label (_("Load Alternate Keymap File"));
  gtk_table_attach (GTK_TABLE (table), button_load_from, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  table = gtk_table_new (6, 6, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, TRUE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  gtk_table_attach (GTK_TABLE (table), command_view, 0, 3, 0, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);
  
  gtk_table_attach (GTK_TABLE (table), binding_view, 3, 6, 0, 5,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);
  
  addbutton = gtk_button_new_from_stock (GTK_STOCK_ADD);
  gtk_table_attach (GTK_TABLE (table), addbutton, 3, 4, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  
  delbutton = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
  //gtk_widget_set_sensitive (delbutton, FALSE);
  gtk_table_attach (GTK_TABLE (table), delbutton, 4, 5, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
 
  lookbutton = gtk_button_new_from_stock (GTK_STOCK_FIND);
  gtk_table_attach (GTK_TABLE (table), lookbutton, 5, 6, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  
  statusbar = gtk_statusbar_new();
  context_id = gtk_statusbar_get_context_id(GTK_STATUSBAR(statusbar), "");
  gtk_statusbar_set_has_resize_grip(GTK_STATUSBAR(statusbar), FALSE);
  gtk_box_pack_end (GTK_BOX (vbox), statusbar, FALSE, TRUE, 0);
  
  cbdata.addbutton = GTK_BUTTON(addbutton);
  cbdata.delbutton = GTK_BUTTON(delbutton);
  cbdata.lookbutton = GTK_BUTTON(lookbutton);
  cbdata.statusbar = GTK_STATUSBAR(statusbar);
  cbdata.context_id = context_id;
  cbdata.command_view = GTK_TREE_VIEW(command_tree_view);
  cbdata.binding_view = GTK_TREE_VIEW(binding_tree_view);
  cbdata.the_keymap = Denemo.prefs.the_keymap;
  cbdata.command_idx = -1;
  //setup the link between command_view and binding_view
  selection = gtk_tree_view_get_selection(GTK_TREE_VIEW(command_tree_view));
  gtk_tree_selection_set_select_function(selection,
          keymap_change_binding_view_on_command_selection, &cbdata, NULL);
  //selecting the first command
  gtk_tree_model_get_iter_first(gtk_tree_view_get_model(
              GTK_TREE_VIEW(command_tree_view)), &iter);
  gtk_tree_selection_select_iter(selection, &iter);

/*
  label = gtk_label_new_with_mnemonic (_("_Scheme:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  scheme = gtk_combo_box_new_text ();
  update_keymaps_list (GTK_COMBO_BOX (scheme));
  gtk_table_attach (GTK_TABLE (table), scheme, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  gtk_combo_box_set_active (GTK_COMBO_BOX (scheme), 0);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), scheme);
  g_signal_connect (scheme, "changed", G_CALLBACK (change_scheme), NULL);

  button_save = gtk_button_new_from_stock (GTK_STOCK_SAVE_AS);
  gtk_table_attach (GTK_TABLE (table), button_save, 2, 3, 0, 1,
		    (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);

  button = gtk_button_new_from_stock (GTK_STOCK_DELETE);
  gtk_table_attach (GTK_TABLE (table), button, 3, 4, 0, 1,
		    (GtkAttachOptions) (0), (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_sensitive (button, FALSE);

  label = gtk_label_new_with_mnemonic (_("C_ategory:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  // FIXME: Does not work with translated command names
  category = gtk_combo_box_new_text ();
  gtk_table_attach (GTK_TABLE (table), category, 1, 4, 2, 3,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), category);

  for (i = 0; i < kbd_categories_length; i++)
    {
      gtk_combo_box_append_text (GTK_COMBO_BOX (category),
				 _(kbd_categories[i]));
    }

  label = gtk_label_new_with_mnemonic (_("_Command:"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  // FIXME: Does not work with translated command names
  command = gtk_combo_box_new_text ();
  gtk_table_attach (GTK_TABLE (table), command, 1, 4, 3, 4,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), command);

  scrolledwindow = gtk_scrolled_window_new (NULL, NULL);
  gtk_box_pack_start (GTK_BOX (vbox), scrolledwindow, TRUE, TRUE, 0);
  gtk_scrolled_window_set_shadow_type (GTK_SCROLLED_WINDOW (scrolledwindow),
				       GTK_SHADOW_IN);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolledwindow),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

  list_store = gtk_list_store_new (2, G_TYPE_STRING,*/	/* binding label */
//				   G_TYPE_STRING);	/* command name */
/*

  treeview = gtk_tree_view_new_with_model (GTK_TREE_MODEL (list_store));
  gtk_widget_set_size_request (treeview, -1, 150);
  renderer = gtk_cell_renderer_text_new ();

  gtk_tree_view_insert_column_with_attributes (GTK_TREE_VIEW (treeview), -1,
					       "Shortcuts", renderer,
					       "text", COL_LABEL, NULL);

  gtk_container_add (GTK_CONTAINER (scrolledwindow), treeview);

  hbuttonbox = gtk_hbutton_box_new ();
  gtk_box_pack_start (GTK_BOX (vbox), hbuttonbox, FALSE, TRUE, 0);
  gtk_button_box_set_layout (GTK_BUTTON_BOX (hbuttonbox),
			     GTK_BUTTONBOX_START);
  gtk_box_set_spacing (GTK_BOX (hbuttonbox), 8);

  button = gtk_button_new_from_stock (GTK_STOCK_ADD);
  gtk_container_add (GTK_CONTAINER (hbuttonbox), button);

  button = gtk_button_new_from_stock (GTK_STOCK_REMOVE);
  gtk_container_add (GTK_CONTAINER (hbuttonbox), button);
  gtk_widget_set_sensitive (button, FALSE);

  button = gtk_button_new_from_stock (GTK_STOCK_FIND);
  gtk_container_add (GTK_CONTAINER (hbuttonbox), button);


  struct cbdata2 cbdata;
  cbdata.command = GTK_COMBO_BOX (command);
  cbdata.category = GTK_COMBO_BOX (category);
  cbdata.treeview = GTK_TREE_VIEW (treeview);
  cbdata.thekeymap = Denemo.prefs.the_keymap;
  cbdata.dialog = GTK_DIALOG (dialog);

  g_signal_connect (category, "changed", G_CALLBACK (category_changed),
		    &cbdata);
  g_signal_connect (command, "changed", G_CALLBACK (command_changed),
		    &cbdata);
  g_signal_connect (button_save, "clicked", G_CALLBACK (save_keymap_as),
		    &cbdata);

  gtk_combo_box_set_active (GTK_COMBO_BOX (category), 0);
*/
  //Connecting signals
  g_signal_connect (addbutton, "clicked",
          G_CALLBACK(kbd_interface_add_binding), &cbdata);
  g_signal_connect (lookbutton, "clicked",
          G_CALLBACK(kbd_interface_look_binding), &cbdata);
  g_signal_connect (delbutton, "clicked",
          G_CALLBACK(kbd_interface_del_binding), &cbdata);
  
  g_signal_connect (GTK_OBJECT (button_save), "clicked",
		      G_CALLBACK(save_standard_keymap_file), cbdata.the_keymap);
  g_signal_connect (GTK_OBJECT (button_save_as), "clicked",
		      G_CALLBACK(save_keymap_dialog), cbdata.the_keymap);
  g_signal_connect (GTK_OBJECT (button_load), "clicked",
		      G_CALLBACK(load_standard_keymap_file_wrapper), cbdata.the_keymap);
  g_signal_connect (GTK_OBJECT (button_load_from), "clicked",
		      G_CALLBACK(load_keymap_dialog), cbdata.the_keymap);


  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}
