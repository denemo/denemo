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
  GtkButton *add2button;
  GtkButton *delbutton;
  GtkButton *lookbutton;
  GtkStatusbar *statusbar;
  guint context_id;
  GtkTreeView *command_view;
  GtkTreeView *binding_view;
  GtkTextView *text_view;
  gulong handler_key_press;
  gulong handler_focus_out;
  gint command_id;
  gint two_key;                 // 0 means not two key, 1 means first key, 2 second.
  GString *twokeylist;          //list of commands whose shortcuts start with pressed key
  gint first_keyval;
  gint first_modifiers;
} keyboard_dialog_data;

guint dnm_sanitize_key_event (GdkEventKey * event);
guint dnm_hyper_sanitize_key_event (GdkEventKey * event);
guint dnm_meta_sanitize_key_state (GdkEventKey * event);
gboolean isModifier (GdkEventKey * event);

//adapted from gtk because we want to allow uppercase accelerator through
//A, instead of <Shift>a
//we need this to be consistent with other modified values.
void dnm_accelerator_parse (const gchar * accelerator, guint * accelerator_key, GdkModifierType * accelerator_mods);

gchar *dnm_accelerator_name (guint accelerator_key, GdkModifierType accelerator_mods);

/**
 * List of all categories.
 * This list also defines the order of the
 *  categories in the keyboard shortcut dialog.
 */
extern gchar *kbd_categories[];
extern gint kbd_categories_length;

void configure_keyboard (gpointer callback_data, guint callback_action, GtkWidget * widget);

keymap *allocate_keymap (void);

void free_keymap (keymap * the_keymap);

void register_command (gchar * name, gchar * label, gchar * tooltip, gpointer callback);
void register_command_row(keymap* the_keymap, command_row* command);

void alphabeticalize_commands (keymap * the_keymap);

//keymap *
//create_keymap (const gchar *filename);



void keymap_clear_bindings (keymap * the_keymap);

guint keymap_size (keymap * the_keymap);

//-1 if the binding is not found
gint lookup_command_for_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state);

//-1 if the binding is not found
gint lookup_command_for_keyevent (GdkEventKey * event);

//-1 if the binding is not found
gint lookup_command_for_keybinding_name (keymap * the_keymap, const gchar * binding_name);


gint lookup_command_from_name (keymap * keymap, const gchar * command_name);

const gchar *lookup_name_from_idx (keymap * keymap, gint command_id);

const gchar *lookup_label_from_idx (keymap * keymap, gint command_id);

const gchar *lookup_tooltip_from_idx (keymap * keymap, gint command_id);

gboolean lookup_hidden_from_idx (keymap * keymap, guint command_id);

gpointer lookup_callback_from_idx (keymap * keymap, gint command_id);

void remove_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state);

void remove_keybinding_from_name (keymap * the_keymap, const gchar * binding);

typedef enum
{
  POS_FIRST = 0,
  POS_LAST
} ListPosition;

gint add_keybinding_to_idx (keymap * the_keymap, gint keyval, GdkModifierType state, guint command_id, ListPosition pos);
gint add_keybinding_to_named_command (keymap * the_keymap, gint keyval, GdkModifierType state, const gchar * command_name, ListPosition pos);
gint add_named_binding_to_idx (keymap * the_keymap, gchar * kb_name, guint command_id, ListPosition pos);
gint keymap_update_accel (keymap * the_keymap, DenemoAction * action, guint keyval, GdkModifierType modifiers);

gint keymap_accel_quick_edit_snooper (GtkWidget * grab_widget, GdkEventKey * event, DenemoAction *action);

DenemoAction *lookup_action_from_name (gchar * command_name);
const DenemoAction *lookup_action_from_idx (keymap * keymap, gint command_id);
const gchar *lookup_menu_path_from_idx (keymap * keymap, gint command_id);

gboolean execute_callback_from_idx (keymap * the_keymap, guint command_id);
gboolean execute_callback_from_name (const gchar * command_name);

void dump_command_info (keymap * the_keymap, gint command_id);
guint dnm_sanitize_key_state (GdkEventKey * event);
void load_keymap_dialog (void);


void load_default_keymap_file (void);

void load_system_keymap_dialog (void);
void save_keymap_dialog (void);
void load_keymap_dialog_location (gchar * location);
void save_default_keymap_file_wrapper (DenemoAction * action, DenemoScriptParam * param);

void save_default_keymap_file (void);

void set_state (gint state, gchar ** value);

GtkWidget *keymap_get_command_view (keymap * the_keymap, GtkWidget *entry, GtkWidget *next, keyboard_dialog_data *cbdata);

GtkWidget *keymap_get_binding_view ();

gboolean keymap_cleanup_command_view (keyboard_dialog_data * data);

gboolean keymap_change_binding_view_on_command_selection (GtkTreeSelection * selection, GtkTreeModel * model, GtkTreePath * path, gboolean path_currently_selected, gpointer data);

void update_accel_labels (keymap * the_keymap, guint command_id);

gboolean command_has_binding (guint command_id);

gint add_keybinding_for_name (gchar * name, gchar * binding);
gint add_keybinding_for_command (gint idx, gchar * binding);

void update_all_labels (keymap * the_keymap);
void dnm_clean_event (GdkEventKey * event);
gint add_twokeybinding_to_idx (keymap * the_keymap, gint first_keyval, GdkModifierType first_state, gint keyval, GdkModifierType state, guint command_id, ListPosition pos);

void command_row_init(command_row *command);
gboolean keymap_get_command_row (keymap * the_keymap, command_row ** row, guint command_id);
gboolean is_action_id_builtin(gint id);
gboolean is_action_name_builtin(gchar* command_name);
const gchar *get_menu_label (gchar *name);
gchar *get_menu_position (gchar *menupath);
void update_bindings_model(GtkListStore * model, GList* bindings);
command_row* get_or_create_command(gchar* name);
gboolean load_keymap_files(GList* files);
GString *keymap_get_bindings (keymap * the_keymap);
gchar* get_scheme_from_idx(gint idx);
#endif
