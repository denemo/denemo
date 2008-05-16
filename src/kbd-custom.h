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

struct name_and_function
{
  /** Category (GUI) */
  gchar *category;

  /** Command name */
  gchar *name;

  GtkFunction function;
};

struct name_action_and_function
{
  /** command name */
  gchar *name;

  gint callback_action;
  union func_union func;
};

extern struct name_and_function unmenued_commands[];
extern gint unmenued_commands_length;

extern struct name_action_and_function *denemo_commands;
extern gint denemo_commands_size;

extern GtkItemFactoryEntry menu_items[];
extern GtkActionEntry menu_entries[];
extern gint n_menu_items;

#define KBD_CATEGORY_NAVIGATION		N_("Navigation")
#define KBD_CATEGORY_NOTE_ENTRY		N_("Note entry")
#define KBD_CATEGORY_REST_ENTRY		N_("Rest entry")
#define KBD_CATEGORY_ARTICULATION	N_("Articulation")
#define KBD_CATEGORY_EDIT		N_("Edit")
#define KBD_CATEGORY_MEASURE		N_("Measure")
#define KBD_CATEGORY_STAFF		N_("Staff")
#define KBD_CATEGORY_PLAYBACK		N_("Playback")
#define KBD_CATEGORY_OTHER		N_("Other")

guint
dnm_sanitize_key_event(GdkEventKey *event);

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
init_keymap ();

keymap *
create_keymap (const gchar *filename);

void
clear_keymap (keymap *the_keymap);

KeybindingInfo *
lookup_keybinding (keymap *the_keymap, gint keyval, gint state);

GList*
lookup_keybinding_by_name(keymap* keymap, const gchar* name);

void
remove_keybinding (keymap *the_keymap, gint keyval, gint state);

gint
add_keybinding (keymap *the_keymap, gint keyval, gint state,
		gint command_number);

void
load_keymap_dialog (GtkWidget *Widget, keymap *the_keymap);

void
load_standard_keymap_file_wrapper (GtkWidget *widget,
				   keymap *the_keymap);

void
load_standard_keymap_file (keymap *the_keymap);

gboolean
load_keymap_file (gchar *filename, keymap *the_keymap);

void
save_keymap_dialog (GtkWidget *widget, keymap *the_keymap);

void
save_standard_keymap_file_wrapper (GtkWidget *widget,
				  DenemoGUI *gui);

void
save_standard_keymap_file (keymap *the_keymap);

void
save_keymap_file (gchar *filename, keymap *the_keymap);

void
set_state(gint state, gchar **value);


#endif
