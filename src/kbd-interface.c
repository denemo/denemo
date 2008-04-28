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

#define CLIST_WIDTH 180
#define CLIST_HEIGHT 180

#define COL_LABEL (0)
#define COL_COMMAND (1)

struct callbackdata
{
  GtkWidget *kbd_window;
  GtkWidget *commands_list;
  GtkWidget *binding_list;
  GtkWidget *hbox1;
  GtkWidget *buttontable;
  GtkWidget *statusbar1;
  gint statusbar1_context_id;
  gint command_selected;
  gint keybinding_number_selected;
  keymap *the_keymap;
};

/**
 *  
 *
 */
void
set_binding_list (GtkWidget * commands_list, gint row, gint column,
		  GdkEventButton * event, struct callbackdata *cbdata)
{
  GList *iterator;
  KeybindingInfo *ki;
  gint i;
  gchar *rowtext[1];
  gchar *mods;

  gtk_clist_freeze (GTK_CLIST (cbdata->binding_list));
  gtk_clist_clear (GTK_CLIST (cbdata->binding_list));
  for (iterator = cbdata->the_keymap->commands[row], i = 0; iterator;
       iterator = iterator->next, i++)
    {
      ki = (KeybindingInfo *) iterator->data;
      mods = NULL;
      set_state (ki->state, &mods);
      rowtext[0] =
	g_strdup_printf ("%s%s", mods ? _(mods) : "",
			 _(gdk_keyval_name (ki->keyval)));
      gtk_clist_append (GTK_CLIST (cbdata->binding_list), rowtext);
      g_free (rowtext[0]);
    }
  gtk_clist_thaw (GTK_CLIST (cbdata->binding_list));
  cbdata->command_selected = row;
  cbdata->keybinding_number_selected = -1;
}

/**
 * Sets the key bindings number
 */
void
set_keybinding_number (GtkWidget * binding_list, gint row, gint column,
		       GdkEventButton * event, struct callbackdata *cbdata)
{
  cbdata->keybinding_number_selected = row;
}

/**
 * Sets the sensitvity of the bottom half of the dialog
 *
 */
static void
set_widget_sensitivity (struct callbackdata *cbdata, gboolean boolean)
{
  gtk_widget_set_sensitive (cbdata->hbox1, boolean);
  gtk_widget_set_sensitive (cbdata->buttontable, boolean);
}

/**
 * Goto the given key binding
 *
 */
static void
jump (GtkWidget * kbd_window, GdkEventKey * event,
      struct callbackdata *cbdata)
{
  /* This check for modifier values on the event may not be right,
     if the contents of gdkkeysyms.h are OS-dependent. I don't believe
     they are. */
  if (event->keyval < GDK_Shift_L || event->keyval > GDK_Hyper_R)
    {
      KeybindingInfo *ki;

      if ((ki = lookup_keybinding (cbdata->the_keymap, event->keyval,
				   event->state)))
	{
	  gtk_clist_select_row (GTK_CLIST (cbdata->commands_list),
				ki->command_number, 0);
	  gtk_clist_moveto (GTK_CLIST (cbdata->commands_list),
			    ki->command_number, 0, 0.5, 0.0);
	  gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
			      cbdata->statusbar1_context_id, "");
	}
      else
	gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
			    cbdata->statusbar1_context_id,
			    _("Binding not found"));
      set_widget_sensitivity (cbdata, TRUE);
      gtk_signal_disconnect_by_func (GTK_OBJECT (kbd_window),
				     GTK_SIGNAL_FUNC (jump), cbdata);
    }
}

/**
 * Add given binding to the keymap
 *
 */
static void
add (GtkWidget * kbd_window, GdkEventKey * event, struct callbackdata *cbdata)
{
  /* This check for modifier values on the event may not be right,
     if the contents of gdkkeysyms.h are OS-dependent. I don't believe
     they are. */
  if ((event->keyval < GDK_Shift_L || event->keyval > GDK_Hyper_R)
      && cbdata->command_selected != -1)
    {
      gint command_returned;
      gchar *text;

      /* If the key combination entered was already bound, remark
         upon this on the status bar */
      if ((command_returned = add_keybinding (cbdata->the_keymap,
					      event->keyval,
					      event->state,
					      cbdata->command_selected)) !=
	  -1)
	{
	  text =
	    g_strdup_printf (_("Previous mapping of %s (%d) to %s removed"),
			     _(gdk_keyval_name (event->keyval)),
			     MASK_FILTER (event->state),
			     _(denemo_commands[command_returned].name));

	  gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
			      cbdata->statusbar1_context_id, text);
	  g_free (text);
	}
      else
	gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
			    cbdata->statusbar1_context_id, "");
      set_widget_sensitivity (cbdata, TRUE);
      set_binding_list (cbdata->commands_list, cbdata->command_selected,
			0, NULL, cbdata);
      gtk_signal_disconnect_by_func (GTK_OBJECT (kbd_window),
				     GTK_SIGNAL_FUNC (add), cbdata);
    }
}

/**
 * Listens for keyentry and then calls jump to
 * goto this binding if it exists
 */
void
listen_then_jump (GtkWidget * button_keybinding_search,
		  struct callbackdata *cbdata)
{
  set_widget_sensitivity (cbdata, FALSE);
  gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
		      cbdata->statusbar1_context_id,
		      _("Type the keystroke to look for"));
  gtk_signal_connect (GTK_OBJECT (cbdata->kbd_window), "key_press_event",
		      GTK_SIGNAL_FUNC (jump), cbdata);
}

/* Some duplicated code in this function, but nothing terrible */
/**
 * Add given binding to to keymap
 *
 */
void
listen_then_add (GtkWidget * button_add_keybinding,
		 struct callbackdata *cbdata)
{
  if (cbdata->command_selected != -1)
    {
      set_widget_sensitivity (cbdata, FALSE);
      gtk_statusbar_push (GTK_STATUSBAR (cbdata->statusbar1),
			  cbdata->statusbar1_context_id,
			  _("Type the keystroke to add for this command"));
      gtk_signal_connect (GTK_OBJECT (cbdata->kbd_window), "key_press_event",
			  GTK_SIGNAL_FUNC (add), cbdata);
    }
}


/**
 * Remove binding from keymap
 *
 */
void
remove_binding (GtkWidget * button_remove_binding,
		struct callbackdata *cbdata)
{
  if (cbdata->command_selected != -1
      && cbdata->keybinding_number_selected != -1)
    {
      KeybindingInfo *ki;

      ki =
	(KeybindingInfo *) g_list_nth (cbdata->the_keymap->
				       commands[cbdata->
						command_selected],
				       cbdata->
				       keybinding_number_selected)->data;
      remove_keybinding (cbdata->the_keymap, ki->keyval, ki->state);
      set_binding_list (cbdata->commands_list, cbdata->command_selected, 0,
			NULL, cbdata);
    }
}

/**
 * Original keymap dialog allows user to set individual bindings and 
 * save to an alternative file
 */
void
configure_keyboard_dialog_OLD (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *kbd_window;
  GtkWidget *vbox3;
  GtkWidget *hbox1;
  GtkWidget *vbox4;
  GtkWidget *commandlabel;
  GtkWidget *scrolled_window1;
  GtkWidget *commands_list;
  GtkWidget *hbuttonbox2;
  GtkWidget *button_keybinding_search;
  GtkWidget *vbox5;
  GtkWidget *binding_list_label;
  GtkWidget *scrolled_window2;
  GtkWidget *binding_list;
  GtkWidget *hbuttonbox3;
  GtkWidget *button_add_binding;
  GtkWidget *button_remove_binding;
  GtkWidget *hrule;
  GtkWidget *buttontable;
  GtkWidget *button_ok;
  GtkWidget *button_ok_save;
  GtkWidget *button_save;
  GtkWidget *button_save_as;
  GtkWidget *button_load;
  GtkWidget *button_load_from;
  GtkWidget *statusbar1;
  keymap *the_keymap = Denemo.prefs.the_keymap;
  GtkTooltips *tooltips;
  gint i;
  gchar *rowtext[1];
  static struct callbackdata cbdata;

  tooltips = gtk_tooltips_new ();

  cbdata.the_keymap = the_keymap;
#if GTK_MAJOR_VERSION > 1
  kbd_window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
#else
  kbd_window = gtk_window_new (GTK_WINDOW_DIALOG);
#endif
  gtk_window_set_title (GTK_WINDOW (kbd_window), _("Customize Keybindings"));
  cbdata.kbd_window = kbd_window;

  vbox3 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox3);
  gtk_container_add (GTK_CONTAINER (kbd_window), vbox3);

  hbox1 = gtk_hbox_new (FALSE, 0);
  gtk_widget_show (hbox1);
  gtk_box_pack_start (GTK_BOX (vbox3), hbox1, TRUE, TRUE, 0);
  cbdata.hbox1 = hbox1;

  vbox4 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox4);
  gtk_box_pack_start (GTK_BOX (hbox1), vbox4, TRUE, TRUE, 0);

  commandlabel = gtk_label_new (_("Available Commands:"));
  gtk_widget_show (commandlabel);
  gtk_box_pack_start (GTK_BOX (vbox4), commandlabel, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (commandlabel), GTK_JUSTIFY_LEFT);

  scrolled_window1 = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window1),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_box_pack_start (GTK_BOX (vbox4), scrolled_window1, TRUE, TRUE, 0);
  gtk_widget_show (scrolled_window1);

  commands_list = gtk_clist_new (1);
  gtk_clist_set_selection_mode (GTK_CLIST (commands_list),
				GTK_SELECTION_SINGLE);
  gtk_clist_set_reorderable (GTK_CLIST (commands_list), FALSE);

  /* Actually initialize the elements of the list */

  for (i = 0; i < denemo_commands_size; i++)
    {
      rowtext[0] = _(denemo_commands[i].name);
      gtk_clist_append (GTK_CLIST (commands_list), rowtext);
    }
  gtk_widget_set_usize (commands_list, CLIST_WIDTH, CLIST_HEIGHT);
  gtk_clist_set_column_auto_resize (GTK_CLIST (commands_list), 0, TRUE);
  gtk_widget_show (commands_list);
  gtk_container_add (GTK_CONTAINER (scrolled_window1), commands_list);
  cbdata.commands_list = commands_list;

  hbuttonbox2 = gtk_hbox_new (TRUE, 0);
  gtk_widget_show (hbuttonbox2);
  gtk_box_pack_start (GTK_BOX (vbox4), hbuttonbox2, FALSE, TRUE, 0);

  button_keybinding_search = gtk_button_new_with_label (_("Search"));
  gtk_widget_show (button_keybinding_search);
  gtk_box_pack_start (GTK_BOX (hbuttonbox2), button_keybinding_search, FALSE,
		      TRUE, 0);
  GTK_WIDGET_SET_FLAGS (button_keybinding_search, GTK_CAN_DEFAULT);
  gtk_tooltips_set_tip (tooltips, button_keybinding_search,
			_
			("Click here, then enter a keystroke. Denemo will jump to the command it corresponds to in the Available Commands list."),
			NULL);

  vbox5 = gtk_vbox_new (FALSE, 0);
  gtk_widget_show (vbox5);
  gtk_box_pack_start (GTK_BOX (hbox1), vbox5, TRUE, TRUE, 0);

  binding_list_label = gtk_label_new (_("Bindings for Selected Command:\n"));
  gtk_widget_show (binding_list_label);
  gtk_box_pack_start (GTK_BOX (vbox5), binding_list_label, FALSE, FALSE, 0);
  gtk_label_set_justify (GTK_LABEL (binding_list_label), GTK_JUSTIFY_LEFT);

  scrolled_window2 = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (scrolled_window2),
				  GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);
  gtk_widget_show (scrolled_window2);
  gtk_box_pack_start (GTK_BOX (vbox5), scrolled_window2, TRUE, TRUE, 0);

  binding_list = gtk_clist_new (1);
  gtk_clist_set_reorderable (GTK_CLIST (binding_list), FALSE);
  gtk_widget_set_usize (binding_list, CLIST_WIDTH, CLIST_HEIGHT);
  gtk_clist_set_column_auto_resize (GTK_CLIST (binding_list), 0, TRUE);
  gtk_widget_show (binding_list);
  gtk_container_add (GTK_CONTAINER (scrolled_window2), binding_list);
  cbdata.binding_list = binding_list;

  hbuttonbox3 = gtk_hbox_new (TRUE, 0);
  gtk_widget_show (hbuttonbox3);
  gtk_box_pack_start (GTK_BOX (vbox5), hbuttonbox3, FALSE, TRUE, 0);

  button_add_binding = gtk_button_new_with_label (_("Add Binding"));
  gtk_widget_show (button_add_binding);
  gtk_box_pack_start (GTK_BOX (hbuttonbox3), button_add_binding, FALSE,
		      TRUE, 0);
  GTK_WIDGET_SET_FLAGS (button_add_binding, GTK_CAN_DEFAULT);

  button_remove_binding
    = gtk_button_new_with_label (_("Delete Selected Binding"));
  gtk_widget_show (button_remove_binding);
  gtk_box_pack_start (GTK_BOX (hbuttonbox3), button_remove_binding, FALSE,
		      TRUE, 0);
  GTK_WIDGET_SET_FLAGS (button_remove_binding, GTK_CAN_DEFAULT);

  hrule = gtk_hseparator_new ();
  gtk_widget_show (hrule);
  gtk_box_pack_start (GTK_BOX (vbox3), hrule, FALSE, FALSE, 0);

  buttontable = gtk_table_new (3, 2, FALSE);
  gtk_widget_show (buttontable);
  gtk_box_pack_start (GTK_BOX (vbox3), buttontable, FALSE, FALSE, 0);
  cbdata.buttontable = buttontable;

  button_ok_save = gtk_button_new_with_label (_("OK and Save As Default"));
  gtk_widget_show (button_ok_save);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_ok_save,
			     0, 1, 0, 1);
  GTK_WIDGET_SET_FLAGS (button_ok_save, GTK_CAN_DEFAULT);

  button_ok = gtk_button_new_with_label (_("OK (no save)"));
  gtk_widget_show (button_ok);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_ok, 1, 2, 0, 1);
  GTK_WIDGET_SET_FLAGS (button_ok, GTK_CAN_DEFAULT);

  button_save = gtk_button_new_with_label (_("Save to Default Keymap File"));
  gtk_widget_show (button_save);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_save,
			     0, 1, 1, 2);
  GTK_WIDGET_SET_FLAGS (button_save, GTK_CAN_DEFAULT);

  button_save_as =
    gtk_button_new_with_label (_("Save As Alternate Keymap File"));
  gtk_widget_show (button_save_as);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_save_as,
			     1, 2, 1, 2);
  GTK_WIDGET_SET_FLAGS (button_save_as, GTK_CAN_DEFAULT);

  button_load = gtk_button_new_with_label (_("Load Default Keymap File"));
  gtk_widget_show (button_load);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_load,
			     0, 1, 2, 3);
  GTK_WIDGET_SET_FLAGS (button_load, GTK_CAN_DEFAULT);

  button_load_from =
    gtk_button_new_with_label (_("Load Alternate Keymap File"));
  gtk_widget_show (button_load_from);
  gtk_table_attach_defaults (GTK_TABLE (buttontable), button_load_from,
			     1, 2, 2, 3);
  GTK_WIDGET_SET_FLAGS (button_load_from, GTK_CAN_DEFAULT);

  statusbar1 = gtk_statusbar_new ();
  cbdata.statusbar1_context_id
    = gtk_statusbar_get_context_id (GTK_STATUSBAR (statusbar1), "kbd_custom");
  gtk_widget_show (statusbar1);
  gtk_box_pack_start (GTK_BOX (vbox3), statusbar1, FALSE, FALSE, 0);
  cbdata.statusbar1 = statusbar1;

  cbdata.command_selected = -1;
  cbdata.keybinding_number_selected = -1;

  /* Now that the GUI's set up, set up the callbacks */

  gtk_signal_connect (GTK_OBJECT (commands_list), "select_row",
		      GTK_SIGNAL_FUNC (set_binding_list), &cbdata);
  gtk_signal_connect (GTK_OBJECT (button_keybinding_search), "clicked",
		      GTK_SIGNAL_FUNC (listen_then_jump), &cbdata);
  gtk_signal_connect (GTK_OBJECT (binding_list), "select_row",
		      GTK_SIGNAL_FUNC (set_keybinding_number), &cbdata);
  gtk_signal_connect (GTK_OBJECT (button_add_binding), "clicked",
		      GTK_SIGNAL_FUNC (listen_then_add), &cbdata);
  gtk_signal_connect (GTK_OBJECT (button_remove_binding), "clicked",
		      GTK_SIGNAL_FUNC (remove_binding), &cbdata);

  gtk_signal_connect_object (GTK_OBJECT (button_ok), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (kbd_window));
  gtk_signal_connect (GTK_OBJECT (button_ok_save), "clicked",
		      GTK_SIGNAL_FUNC (save_standard_keymap_file_wrapper),
		      gui);
  gtk_signal_connect_object (GTK_OBJECT (button_ok_save), "clicked",
			     GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (kbd_window));
  gtk_signal_connect (GTK_OBJECT (button_save), "clicked",
		      GTK_SIGNAL_FUNC (save_standard_keymap_file_wrapper),
		      gui);
  gtk_signal_connect (GTK_OBJECT (button_save_as), "clicked",
		      GTK_SIGNAL_FUNC (save_keymap_dialog), the_keymap);
  gtk_signal_connect (GTK_OBJECT (button_load), "clicked",
		      GTK_SIGNAL_FUNC (load_standard_keymap_file_wrapper),
		      the_keymap);
  gtk_signal_connect (GTK_OBJECT (button_load_from), "clicked",
		      GTK_SIGNAL_FUNC (load_keymap_dialog), the_keymap);

  /* And show the window */
  gtk_window_set_modal (GTK_WINDOW (kbd_window), TRUE);
  gtk_window_set_position (GTK_WINDOW (kbd_window), GTK_WIN_POS_MOUSE);
  gtk_widget_show (kbd_window);
}

struct cbdata2
{
  keymap *thekeymap;
  GtkTreeView *treeview;
  GtkComboBox *category;
  GtkComboBox *command;
  GtkDialog *dialog;
};

/**
 * Callback invoked when the category is changed
 */
static void
category_changed (GtkComboBox * category, struct cbdata2 *cbdata)
{
  gint a = gtk_combo_box_get_active (category);
  gchar *cat = kbd_categories[a];
  gint i;

  GtkListStore *model =
    (GtkListStore *) gtk_combo_box_get_model (cbdata->command);
  gtk_list_store_clear (model);

  for (i = 0; i < unmenued_commands_length; i++)
    {
      if (strcmp (unmenued_commands[i].category, cat) == 0)
	{
	  gtk_combo_box_append_text (cbdata->command,
				     _(unmenued_commands[i].name));
	}
    }
  gtk_combo_box_set_active (cbdata->command, 0);
}

/**
 * Callback invoked when the command is changed.
 * Updates the list of keybindings for the selected command.
 */
static void
command_changed (GtkComboBox * command, struct cbdata2 *cbdata)
{
  gchar *name;
  GtkTreeIter iter;
  int i;

  name = gtk_combo_box_get_active_text (command);
  GtkListStore *list_store =
    (GtkListStore *) gtk_tree_view_get_model (cbdata->treeview);
  gtk_list_store_clear (list_store);

  for (i = 0; i < denemo_commands_size; i++)
    {
      if (strcmp (denemo_commands[i].name, name) == 0)
	{
	  GList *c;
	  for (c = cbdata->thekeymap->commands[i]; c != NULL;
	       c = g_list_next (c))
	    {
	      KeybindingInfo *ki = (KeybindingInfo *) c->data;
	      gtk_list_store_append (list_store, &iter);
	      gtk_list_store_set (list_store, &iter,
				  COL_LABEL, gdk_keyval_name (ki->keyval),
				  COL_COMMAND, denemo_commands[i].name, -1);
	    }
	}
    }

}

static void
validate_keymap_name (GtkEntry * entry, GtkDialog * dialog)
{
  const gchar *name = gtk_entry_get_text (GTK_ENTRY (entry));
  gtk_dialog_set_response_sensitive (GTK_DIALOG (dialog),
				     GTK_RESPONSE_ACCEPT, strlen (name) > 0);
}

/**
 * Save the selected keymap under a different name
 */
static void
save_keymap_as (GtkButton * button, struct cbdata2 *cbdata)
{
  GtkWidget *dialog;
  GtkWidget *box;
  GtkWidget *label;
  GtkWidget *entry;

  dialog = gtk_dialog_new_with_buttons (_("Save keyboard shortcut"),
					GTK_WINDOW (cbdata->dialog),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_dialog_set_response_sensitive (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT,
				     FALSE);

  box = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), box, TRUE, TRUE,
		      0);
  gtk_container_set_border_width (GTK_CONTAINER (box), 12);

  label = gtk_label_new_with_mnemonic (_("_Name:"));
  gtk_box_pack_start (GTK_BOX (box), label, FALSE, TRUE, 0);

  entry = gtk_entry_new ();
  gtk_box_pack_start (GTK_BOX (box), entry, FALSE, TRUE, 0);
  gtk_label_set_mnemonic_widget (GTK_LABEL (label), entry);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  g_signal_connect (entry, "changed", G_CALLBACK (validate_keymap_name),
		    dialog);

  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      const gchar *name = gtk_entry_get_text (GTK_ENTRY (entry));
      gchar *file_name = g_strconcat (name, ".keymaprc", NULL);
      gchar *path = g_build_filename (locatedotdenemo (), file_name, NULL);

      // TODO: Actually copy the keymap

      g_print ("%s\n", path);
      g_free (file_name);
      g_free (path);
    }

  gtk_widget_destroy (dialog);

}


static void
change_scheme (GtkComboBox * scheme, gpointer data)
{
  // TODO: load the keymap
  // TODO: disable delete for Default keymap
}

/**
 * Populate the combo with the names of all .keymaprc-files found under ~/.denemo
 * TODO: Include keymaps from /etc/denemo
 */
static void
update_keymaps_list (GtkComboBox * combo)
{
  GtkListStore *model = (GtkListStore *) gtk_combo_box_get_model (combo);
  gtk_list_store_clear (model);
  gtk_combo_box_append_text (GTK_COMBO_BOX (combo), "Default");

  const gchar *dotdenemo = locatedotdenemo ();
  if(dotdenemo==NULL)
    return;
  GDir *dir = g_dir_open (dotdenemo, 0, NULL);
  g_assert (dir != NULL);

  const gchar *file = NULL;
  while ((file = g_dir_read_name (dir)) != NULL)
    {
      if (g_str_has_suffix (file, ".keymaprc"))
	{
	  gchar *name = g_strndup (file, strlen (file) - 9);
	  gtk_combo_box_append_text (GTK_COMBO_BOX (combo), name);
	  g_free (name);
	}
    }
  g_dir_close (dir);
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
  GtkWidget *button_save;
  GtkWidget *scheme;
  GtkWidget *scrolledwindow;
  GtkWidget *treeview;
  GtkWidget *hbuttonbox;
  GtkListStore *list_store = NULL;
  GtkCellRenderer *renderer;
  gint i;

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

  table = gtk_table_new (4, 4, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  hseparator = gtk_hseparator_new ();
  gtk_table_attach (GTK_TABLE (table), hseparator, 0, 4, 1, 2,
		    (GtkAttachOptions) (GTK_EXPAND | GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

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

  list_store = gtk_list_store_new (2, G_TYPE_STRING,	/* binding label */
				   G_TYPE_STRING);	/* command name */


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

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}
