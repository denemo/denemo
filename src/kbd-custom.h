/* kbd-custom.h
   Header files for customizing keyboard configuration
   
   For Denemo, the GNU graphical music notation package
   (c) 2000-2005 Olivier Vermersch, Matthew Hiller */

#ifndef KBD_CUSTOM_H
#define KBD_CUSTOM_H

#include <stdio.h>
#include <limits.h>
#include <gdk/gdk.h>
#include <denemo/denemo.h>

#define MASK_FILTER(state) state & (GDK_SHIFT_MASK | GDK_CONTROL_MASK \
				    | GDK_MOD1_MASK)

typedef struct _keyboard_dialog_data
{
	GtkButton *addbutton;
	GtkButton *delbutton;
	GtkButton *lookbutton;
	GtkStatusbar *statusbar;
	guint context_id;
	GtkTreeView *command_view;
	GtkTreeView *binding_view;
	GtkTextView *text_view;
	keymap *the_keymap;
	gulong handler_key_press;
	gulong handler_focus_out;
	gint command_idx;
} keyboard_dialog_data;

guint
dnm_sanitize_key_event(GdkEventKey *event);

gboolean
isModifier(GdkEventKey *event);

//adapted from gtk because we want to allow uppercase accelerator through
//A, instead of <Shift>a
//we need this to be consistent with other modified values.
void dnm_accelerator_parse(const gchar *accelerator,
		guint *accelerator_key,
		GdkModifierType *accelerator_mods);

gchar *dnm_accelerator_name(guint accelerator_key,
		GdkModifierType accelerator_mods);

/**
 * List of all categories.
 * This list also defines the order of the
 *  categories in the keyboard shortcut dialog.
 */
extern gchar* kbd_categories[];
extern gint kbd_categories_length;

void
configure_keyboard (gpointer callback_data, guint callback_action,
		    GtkWidget *widget);

keymap *
allocate_keymap (const gchar *action_group_name);

void
free_keymap(keymap *the_keymap);

void
register_command(keymap *the_keymap, GtkAction *action, const gchar *name, const gchar *label, const gchar *tooltip, gpointer callback);

void
alphabeticalize_commands(keymap *the_keymap);

//keymap *
//create_keymap (const gchar *filename);



void
keymap_clear_bindings (keymap *the_keymap);

guint
keymap_size (keymap *the_keymap);

void
keymap_foreach_command_binding (keymap *the_keymap, guint command_idx,
		GFunc func, gpointer user_data);

//-1 if the binding is not found
gint
lookup_keybinding (keymap *the_keymap, gint keyval, GdkModifierType state);

//-1 if the binding is not found
gint
lookup_keybinding_from_string (keymap *the_keymap, const gchar *binding_name);

//GList*
//lookup_keybindings_from_name(keymap* keymap, const gchar* name);

//GList*
//lookup_keybindings_by_idx(keymap* keymap, guint idx);

gint 
lookup_index_from_name (keymap * keymap, const gchar *command_name);

const gchar *
lookup_name_from_idx(keymap *keymap, guint command_idx);

const gchar *
lookup_label_from_idx(keymap *keymap, guint command_idx);

//const gchar *
//lookup_label_from_name(keymap *keymap, const gchar *command_name);

void
remove_keybinding (keymap *the_keymap, gint keyval, GdkModifierType state);

void
remove_keybinding_from_string (keymap *the_keymap, const gchar *binding);

typedef enum {
	POS_FIRST = 0,
	POS_LAST
} KbdPosition;

gint
add_keybinding_from_idx (keymap * the_keymap, gint keyval,
        GdkModifierType state, guint command_idx, KbdPosition pos);
gint
add_keybinding_from_name (keymap *the_keymap, gint keyval,
        GdkModifierType state, const gchar *command_name, KbdPosition pos);

gint
keymap_update_accel(keymap *the_keymap, GtkAction *action, guint keyval,
		GdkModifierType modifiers);

gint
keymap_accel_quick_edit_snooper(GtkWidget *grab_widget, GdkEventKey *event,
		gpointer func_data);

GtkAction *
action_of_name(keymap *the_keymap, gchar *command_name);

gboolean
execute_callback_from_idx(keymap *the_keymap, guint command_idx);
gboolean
execute_callback_from_name(keymap *the_keymap, const gchar* command_name);

gboolean
idx_has_callback (keymap *the_keymap, guint command_idx);

void
dump_command_info(keymap *the_keymap, gint command_idx);

void
load_keymap_dialog (GtkWidget *Widget, keymap *the_keymap);

void
load_default_keymap_file_wrapper (GtkWidget *widget,
				   keymap *the_keymap);

void
load_default_keymap_file (keymap *the_keymap);

void
load_system_keymap_dialog (GtkWidget * widget, keymap * the_keymap);
void
save_keymap_dialog (GtkWidget *widget, keymap *the_keymap);

void
save_default_keymap_file_wrapper (GtkAction *action);

void
save_default_keymap_file (GtkWidget *widget, keymap *the_keymap);

void
set_state(gint state, gchar **value);

GtkWidget *
keymap_get_command_view(keymap *the_keymap);

GtkWidget *
keymap_get_binding_view();

void
keymap_cleanup_command_view(keyboard_dialog_data *data);

gboolean
keymap_change_binding_view_on_command_selection(GtkTreeSelection *selection,
		GtkTreeModel *model,
		GtkTreePath *path,
		gboolean path_currently_selected,
		gpointer data);

void
update_accel_labels(keymap *the_keymap, guint command_idx);
#endif
