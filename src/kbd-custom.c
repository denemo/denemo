/* kbd-custom.cpp
 *  Low-level data structure routines and file I/O for customizing keyboard
 *  configuration.
 *  
 *  For Denemo, the GNU graphical music notation package
 *  (c) 2000-2005 
 *      Olivier Vermersch, Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include <gdk/gdk.h>
#include <gtk/gtk.h>
#include <glib.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include <denemo/denemo.h>
#include "commandfuncs.h"
#include "kbd-custom.h"
#include "kbd-interface.h"
#include "keyresponses.h"
#include "prefops.h"
#include "selectops.h"
#include "utils.h"
#include "playback.h"
#include "keyboard.h"

#if GTK_MINOR_VERSION < 10
//Hmm, should we define these as 0, so that they don't mask anything in gtk 2.8
#define  GDK_SUPER_MASK ( 1 << 26)
#define  GDK_HYPER_MASK  (1 << 27)
#define  GDK_META_MASK   (1 << 28)
#endif

#define DEFAULT_KEYMAP "Default.keymap"
//index of columns in the keymap command list store
enum {
    COL_TYPE = 0,
    COL_ENTRY,
    COL_BINDINGS,
    N_COLUMNS
};

typedef struct _command_row {
    KeymapCommandType type;
    gpointer entry;
    GtkListStore *bindings;
}command_row;

static void
load_keymap_file_named (keymap * the_keymap, gchar *keymapfile, gchar *fallback);

/* Returns the state of the event after removing the modifiers consumed by the
 * system and unwanted modifiers. Use this before doing anything based on the
 * (keyval, state) pair in an event handler.
 */
guint
dnm_sanitize_key_state(GdkEventKey *event)
{
    guint ret = event->state;
    GdkModifierType consumed;
    /* We want to ignore irrelevant modifiers like ScrollLock */

    gdk_keymap_translate_keyboard_state (NULL, event->hardware_keycode,
        event->state, event->group, NULL, NULL, NULL, &consumed);
    /* removing consumed modifiers from ret */
    ret &= ~consumed;
    /* removing other unwanted modifiers from event->state */
    ret &= (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_MOD1_MASK);
    return ret;
}

/*
 * Returns True if the key event is just a modifier key, False otherwise
 * TODO look for a gdk function doing that properly
 */
gboolean
isModifier(GdkEventKey *event)
{
    /* This check for modifier values on the event may not be right,
       if the contents of gdkkeysyms.h are OS-dependent. I don't believe
       they are. */
    return event->keyval >= GDK_Shift_L && event->keyval <= GDK_Hyper_R;
}

static inline gboolean
is_alt (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'a' || string[1] == 'A') &&
	  (string[2] == 'l' || string[2] == 'L') &&
	  (string[3] == 't' || string[3] == 'T') &&
	  (string[4] == '>'));
}

static inline gboolean
is_ctl (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'c' || string[1] == 'C') &&
	  (string[2] == 't' || string[2] == 'T') &&
	  (string[3] == 'l' || string[3] == 'L') &&
	  (string[4] == '>'));
}

static inline gboolean
is_modx (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'm' || string[1] == 'M') &&
	  (string[2] == 'o' || string[2] == 'O') &&
	  (string[3] == 'd' || string[3] == 'D') &&
	  (string[4] >= '1' && string[4] <= '5') &&
	  (string[5] == '>'));
}

static inline gboolean
is_ctrl (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'c' || string[1] == 'C') &&
	  (string[2] == 't' || string[2] == 'T') &&
	  (string[3] == 'r' || string[3] == 'R') &&
	  (string[4] == 'l' || string[4] == 'L') &&
	  (string[5] == '>'));
}

static inline gboolean
is_shft (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 's' || string[1] == 'S') &&
	  (string[2] == 'h' || string[2] == 'H') &&
	  (string[3] == 'f' || string[3] == 'F') &&
	  (string[4] == 't' || string[4] == 'T') &&
	  (string[5] == '>'));
}

static inline gboolean
is_shift (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 's' || string[1] == 'S') &&
	  (string[2] == 'h' || string[2] == 'H') &&
	  (string[3] == 'i' || string[3] == 'I') &&
	  (string[4] == 'f' || string[4] == 'F') &&
	  (string[5] == 't' || string[5] == 'T') &&
	  (string[6] == '>'));
}

static inline gboolean
is_control (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'c' || string[1] == 'C') &&
	  (string[2] == 'o' || string[2] == 'O') &&
	  (string[3] == 'n' || string[3] == 'N') &&
	  (string[4] == 't' || string[4] == 'T') &&
	  (string[5] == 'r' || string[5] == 'R') &&
	  (string[6] == 'o' || string[6] == 'O') &&
	  (string[7] == 'l' || string[7] == 'L') &&
	  (string[8] == '>'));
}

static inline gboolean
is_release (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'r' || string[1] == 'R') &&
	  (string[2] == 'e' || string[2] == 'E') &&
	  (string[3] == 'l' || string[3] == 'L') &&
	  (string[4] == 'e' || string[4] == 'E') &&
	  (string[5] == 'a' || string[5] == 'A') &&
	  (string[6] == 's' || string[6] == 'S') &&
	  (string[7] == 'e' || string[7] == 'E') &&
	  (string[8] == '>'));
}

static inline gboolean
is_meta (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'm' || string[1] == 'M') &&
	  (string[2] == 'e' || string[2] == 'E') &&
	  (string[3] == 't' || string[3] == 'T') &&
	  (string[4] == 'a' || string[4] == 'A') &&
	  (string[5] == '>'));
}

static inline gboolean
is_super (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 's' || string[1] == 'S') &&
	  (string[2] == 'u' || string[2] == 'U') &&
	  (string[3] == 'p' || string[3] == 'P') &&
	  (string[4] == 'e' || string[4] == 'E') &&
	  (string[5] == 'r' || string[5] == 'R') &&
	  (string[6] == '>'));
}

static inline gboolean
is_hyper (const gchar *string)
{
  return ((string[0] == '<') &&
	  (string[1] == 'h' || string[1] == 'H') &&
	  (string[2] == 'y' || string[2] == 'Y') &&
	  (string[3] == 'p' || string[3] == 'P') &&
	  (string[4] == 'e' || string[4] == 'E') &&
	  (string[5] == 'r' || string[5] == 'R') &&
	  (string[6] == '>'));
}

void
dnm_accelerator_parse (const gchar     *accelerator,
		       guint           *accelerator_key,
		       GdkModifierType *accelerator_mods)
{
  guint keyval;
  GdkModifierType mods;
  gint len;
  
  if (accelerator_key)
    *accelerator_key = 0;
  if (accelerator_mods)
    *accelerator_mods = 0;
  g_return_if_fail (accelerator != NULL);
  
  keyval = 0;
  mods = 0;
  len = strlen (accelerator);
  while (len)
    {
      if (*accelerator == '<')
	{
	  if (len >= 9 && is_release (accelerator))
	    {
	      accelerator += 9;
	      len -= 9;
	      mods |= GDK_RELEASE_MASK;
	    }
	  else if (len >= 9 && is_control (accelerator))
	    {
	      accelerator += 9;
	      len -= 9;
	      mods |= GDK_CONTROL_MASK;
	    }
	  else if (len >= 7 && is_shift (accelerator))
	    {
	      accelerator += 7;
	      len -= 7;
	      mods |= GDK_SHIFT_MASK;
	    }
	  else if (len >= 6 && is_shft (accelerator))
	    {
	      accelerator += 6;
	      len -= 6;
	      mods |= GDK_SHIFT_MASK;
	    }
	  else if (len >= 6 && is_ctrl (accelerator))
	    {
	      accelerator += 6;
	      len -= 6;
	      mods |= GDK_CONTROL_MASK;
	    }
	  else if (len >= 6 && is_modx (accelerator))
	    {
	      static const guint mod_vals[] = {
		GDK_MOD1_MASK, GDK_MOD2_MASK, GDK_MOD3_MASK,
		GDK_MOD4_MASK, GDK_MOD5_MASK
	      };

	      len -= 6;
	      accelerator += 4;
	      mods |= mod_vals[*accelerator - '1'];
	      accelerator += 2;
	    }
	  else if (len >= 5 && is_ctl (accelerator))
	    {
	      accelerator += 5;
	      len -= 5;
	      mods |= GDK_CONTROL_MASK;
	    }
	  else if (len >= 5 && is_alt (accelerator))
	    {
	      accelerator += 5;
	      len -= 5;
	      mods |= GDK_MOD1_MASK;
	    }
          else if (len >= 6 && is_meta (accelerator))
	    {
	      accelerator += 6;
	      len -= 6;
	      mods |= GDK_META_MASK;
	    }
          else if (len >= 7 && is_hyper (accelerator))
	    {
	      accelerator += 7;
	      len -= 7;
	      mods |= GDK_HYPER_MASK;
	    }
          else if (len >= 7 && is_super (accelerator))
	    {
	      accelerator += 7;
	      len -= 7;
	      mods |= GDK_SUPER_MASK;
	    }
	  else
	    {
	      gchar last_ch;
	      
	      last_ch = *accelerator;
	      while (last_ch && last_ch != '>')
		{
		  last_ch = *accelerator;
		  accelerator += 1;
		  len -= 1;
		}
	    }
	}
      else
	{
	  keyval = gdk_keyval_from_name (accelerator);
	  accelerator += len;
	  len -= len;
	}
    }
  
  if (accelerator_key)
  //The line we modify, so that uppercase letter are processed as we want
  //  *accelerator_key = gdk_keyval_to_lower (keyval);
    *accelerator_key = keyval;
  if (accelerator_mods)
    *accelerator_mods = mods;
}

gchar*
dnm_accelerator_name (guint           accelerator_key,
		      GdkModifierType accelerator_mods)
{
  static const gchar text_release[] = "<Release>";
  static const gchar text_shift[] = "<Shift>";
  static const gchar text_control[] = "<Control>";
  static const gchar text_mod1[] = "<Alt>";
  static const gchar text_mod2[] = "<Mod2>";
  static const gchar text_mod3[] = "<Mod3>";
  static const gchar text_mod4[] = "<Mod4>";
  static const gchar text_mod5[] = "<Mod5>";
  static const gchar text_meta[] = "<Meta>";
  static const gchar text_super[] = "<Super>";
  static const gchar text_hyper[] = "<Hyper>";
  guint l;
  gchar *keyval_name;
  gchar *accelerator;

  accelerator_mods &= GDK_MODIFIER_MASK;

  //The line we modify, so that uppercase letter are processed as we want
  //keyval_name = gdk_keyval_name (gdk_keyval_to_lower (accelerator_key));
  keyval_name = gdk_keyval_name (accelerator_key);
  if (!keyval_name)
    keyval_name = "";

  l = 0;
  if (accelerator_mods & GDK_RELEASE_MASK)
    l += sizeof (text_release) - 1;
  if (accelerator_mods & GDK_SHIFT_MASK)
    l += sizeof (text_shift) - 1;
  if (accelerator_mods & GDK_CONTROL_MASK)
    l += sizeof (text_control) - 1;
  if (accelerator_mods & GDK_MOD1_MASK)
    l += sizeof (text_mod1) - 1;
  if (accelerator_mods & GDK_MOD2_MASK)
    l += sizeof (text_mod2) - 1;
  if (accelerator_mods & GDK_MOD3_MASK)
    l += sizeof (text_mod3) - 1;
  if (accelerator_mods & GDK_MOD4_MASK)
    l += sizeof (text_mod4) - 1;
  if (accelerator_mods & GDK_MOD5_MASK)
    l += sizeof (text_mod5) - 1;
  l += strlen (keyval_name);
  if (accelerator_mods & GDK_META_MASK)
    l += sizeof (text_meta) - 1;
  if (accelerator_mods & GDK_HYPER_MASK)
    l += sizeof (text_hyper) - 1;
  if (accelerator_mods & GDK_SUPER_MASK)
    l += sizeof (text_super) - 1;

  accelerator = g_new (gchar, l + 1);

  l = 0;
  accelerator[l] = 0;
  if (accelerator_mods & GDK_RELEASE_MASK)
    {
      strcpy (accelerator + l, text_release);
      l += sizeof (text_release) - 1;
    }
  if (accelerator_mods & GDK_SHIFT_MASK)
    {
      strcpy (accelerator + l, text_shift);
      l += sizeof (text_shift) - 1;
    }
  if (accelerator_mods & GDK_CONTROL_MASK)
    {
      strcpy (accelerator + l, text_control);
      l += sizeof (text_control) - 1;
    }
  if (accelerator_mods & GDK_MOD1_MASK)
    {
      strcpy (accelerator + l, text_mod1);
      l += sizeof (text_mod1) - 1;
    }
  if (accelerator_mods & GDK_MOD2_MASK)
    {
      strcpy (accelerator + l, text_mod2);
      l += sizeof (text_mod2) - 1;
    }
  if (accelerator_mods & GDK_MOD3_MASK)
    {
      strcpy (accelerator + l, text_mod3);
      l += sizeof (text_mod3) - 1;
    }
  if (accelerator_mods & GDK_MOD4_MASK)
    {
      strcpy (accelerator + l, text_mod4);
      l += sizeof (text_mod4) - 1;
    }
  if (accelerator_mods & GDK_MOD5_MASK)
    {
      strcpy (accelerator + l, text_mod5);
      l += sizeof (text_mod5) - 1;
    }
  if (accelerator_mods & GDK_META_MASK)
    {
      strcpy (accelerator + l, text_meta);
      l += sizeof (text_meta) - 1;
    }
  if (accelerator_mods & GDK_HYPER_MASK)
    {
      strcpy (accelerator + l, text_hyper);
      l += sizeof (text_hyper) - 1;
    }
  if (accelerator_mods & GDK_SUPER_MASK)
    {
      strcpy (accelerator + l, text_super);
      l += sizeof (text_super) - 1;
    }
  strcpy (accelerator + l, keyval_name);

  return accelerator;
}

/**
 * Warns user about old keymap file
 *
 */
static void
old_keymap_dialog ()
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_WARNING,
				   GTK_BUTTONS_CLOSE,
				   _("Old keymap file found"));


  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    _
					    ("From version 0.7.5, Denemo uses an xml file to store its key bindings. "
					     "I have found an old-style file, and am using that for now.\n\n"
					     "If you won't want to use an old version of Denemo in future, "
					     "please go to \"Edit,Set Keybindings\" and click \"OK and Save As Default\" "
					     "in order to avoid seeing this message again.\n\n"
					     "Thanks."));

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/**
 * Warns user that there was no keymap available to load  
 *
 */
static void
no_map_dialog ()
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL,
				   (GtkDialogFlags)
				   (GTK_DIALOG_MODAL |
				    GTK_DIALOG_DESTROY_WITH_PARENT),
				   GTK_MESSAGE_WARNING,
				   GTK_BUTTONS_CLOSE,
				   _
				   ("Keyboard shortcuts could not be found"));


  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
					    _
					    ("No keymap file was found in either"
					     " the systemwide Denemo directory"
					     " or in .denemo directory within your "
					     "home directory. Please go to"
					     " Edit/Set Keybindings to construct a custom "
					     "interface or to load one from"
					     " an alternate keymap file."));

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}

/*
 * Allocates a keymap.
 * action_group_name is the name of the group of actions for the commands
 * of the keymap.
 */
keymap *allocate_keymap(const gchar *action_group_name)
{
  keymap *the_keymap = (keymap *) g_malloc (sizeof (keymap));
  the_keymap->action_group_name = g_strdup(action_group_name);
  //empty list store of commands
  //3 columns :
  //- type of action, a KeymapCommandType
  //- pointer to an action entry, gpointer
  //- pointer to a list store for storing the bindings of a command
  the_keymap->commands = gtk_list_store_new(N_COLUMNS,
          G_TYPE_INT,
          G_TYPE_POINTER,
          GTK_TYPE_LIST_STORE);
  
  //empty index reference
  the_keymap->idx_from_name =
      g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  the_keymap->idx_from_keystring =
      g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free); 
  return the_keymap;
}

/**
 * Utility function for free_keymap
 */
static void
free_element (gpointer value, gpointer user_data)
{
  g_free(value);
}

void
free_keymap(keymap *the_keymap)
{
    g_object_unref(the_keymap->commands);
    g_hash_table_destroy(the_keymap->idx_from_name);
    g_hash_table_destroy(the_keymap->idx_from_keystring);
    g_free(the_keymap->action_group_name); 
}

void
register_entry_commands(keymap *the_keymap, gpointer entries, guint n,
        KeymapCommandType type)
{
    guint i;
    guint *value;
    const gchar *name;
    gpointer entry;
    GtkTreeIter iter;
    GtkListStore *bindings;

    for (i = 0; i < n; i++) {
        //get the index of the new row
        value = (guint *) g_malloc(sizeof(guint));
        *value = gtk_tree_model_iter_n_children(
                GTK_TREE_MODEL(the_keymap->commands), NULL);
        
        //add a new row
        gtk_list_store_append(the_keymap->commands, &iter);
        
        //get information specific to the KeymapCommandType
        switch (type) {
            case KeymapEntry:
                entry = (GtkActionEntry *) entries + i;
                name = ((GtkActionEntry *) entry)->name;
                break;
            case KeymapToggleEntry:
                entry = (GtkToggleActionEntry *) entries + i;
                name = ((GtkToggleActionEntry *) entry)->name;
                break;
            case KeymapRadioEntry:
                entry = (GtkRadioActionEntry *) entries + i;
                name = ((GtkRadioActionEntry *) entry)->name;
                break;
            default:
                return;
        }
        //allocate a new bindings list store
        bindings = gtk_list_store_new(1, G_TYPE_STRING);
#if DEBUG
        //This code is only relevant to developpers, to check that no action
        //entry masks another. Users cannot add actions.
        gint idx = lookup_index_from_name(the_keymap, name);
        if (idx != -1) {
            g_warning("Command %s is inserted more than once, aborting...\n",
                    name);
            // exit(2);FIXME dirty
        }
#endif
        //insert the information in the list store
        gtk_list_store_set(the_keymap->commands, &iter,
                COL_TYPE, type,
                COL_ENTRY, entry,
                COL_BINDINGS, bindings,
                -1);
        //insert the command name in the index reference
        g_hash_table_insert(the_keymap->idx_from_name,
                g_strdup(name), value);

        //drop the reference to the bindings list store, so that it is freed
        //with the command list store
        g_object_unref(bindings);

#if DEBUG
        g_print("Inserting command %s -> %d\n", name, *value);
#endif
    }
}

static gint
command_iter_sort(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b,
        gpointer user_data)
{
  GtkTreeIter *iters[2];
  KeymapCommandType type;
  gpointer entry;
  const gchar *names[2];
  gint i;
  iters[0] = a; iters[1] = b;
  for (i = 0; i < 2; i++) {
      gtk_tree_model_get(model, iters[i],
              COL_TYPE, &type, COL_ENTRY, &entry, -1);
      switch (type) {
          case KeymapEntry:
              names[i] = ((GtkActionEntry *) entry)->name;
              break;
          case KeymapToggleEntry:
              names[i] = ((GtkToggleActionEntry *) entry)->name;
              break;
          case KeymapRadioEntry:
              names[i] = ((GtkRadioActionEntry *) entry)->name;
              break;
          default:
              names[i] = NULL;
              break;
      }
  }
  return strcmp(names[0], names[1]);
}

void
end_command_registration(keymap *the_keymap)
{
  gint i, n;
  guint *value;
  const gchar *command_name;
  GtkTreeModel *model = GTK_TREE_MODEL(the_keymap->commands);
  GtkTreeSortable *sortable = GTK_TREE_SORTABLE(the_keymap->commands);
  n = gtk_tree_model_iter_n_children(model, NULL);
  gtk_tree_sortable_set_sort_func(sortable, 0, command_iter_sort, NULL, NULL);
  gtk_tree_sortable_set_sort_column_id(sortable, 0, GTK_SORT_ASCENDING);
  for (i = 0; i < n; i++) {
      command_name = lookup_name_from_idx(the_keymap, i);
      value = (guint *) g_hash_table_lookup(the_keymap->idx_from_name,
              command_name);
      *value = i;
  }
}

//False if command_idx is an invalid index or keymap is null, true otherwise
//TODO keymap should not be NULL
static gboolean
keymap_get_command_row(keymap *the_keymap, command_row *row, guint command_idx)
{
  if (!the_keymap) {
    warningdialog("This should not happen...");
    return FALSE;
  }
    GtkTreeModel *model = GTK_TREE_MODEL(the_keymap->commands);
    GtkTreeIter iter;
    if (!gtk_tree_model_iter_nth_child(model, &iter, NULL, command_idx))
        return FALSE;
    gtk_tree_model_get(model, &iter,
            COL_TYPE, &row->type,
            COL_ENTRY, &row->entry,
            COL_BINDINGS, &row->bindings,
            -1);
    return TRUE;
}



static gboolean
keymap_clear_bindings_in_row(GtkTreeModel *model, GtkTreePath *path,
        GtkTreeIter *iter, gpointer data)
{
    GtkListStore *bindings;
    gtk_tree_model_get(model, iter, COL_BINDINGS, &bindings, -1);
    gtk_list_store_clear(bindings);
    g_object_unref(bindings);
    return FALSE;
}


static void
catname(gchar *name, GString *str, gchar *separator) {
  if(str)
    g_string_append_printf(str, "%s%s", name, separator);
}

static void
newlinename(gchar *name, GString *str) {
 catname(name, str, "\n");
}
static void
listname(gchar *name, GString *str) {
  catname(name, str, " ");
}


/**
 * Clears the keymap of all entries. Leaves  the content of commands and
 * idx_from_name untouched, removes the content of bindings and
 * idx_from_keystring
 *
 */
void
keymap_clear_bindings (keymap * the_keymap)
{
  gtk_tree_model_foreach(GTK_TREE_MODEL(the_keymap->commands),
          keymap_clear_bindings_in_row, NULL);
  g_hash_table_remove_all (the_keymap->idx_from_keystring);
}

/*
 * Returns the number of commands in the keymap
 */
guint
keymap_size (keymap *the_keymap)
{
    return gtk_tree_model_iter_n_children(GTK_TREE_MODEL(the_keymap->commands),
            NULL);
}

/*
 * executes function fun on all bindings attached to a command. The arguments
 * passed to the function are the value of the current binding and the
 * additionnal user data
 */
void
keymap_foreach_command_binding (keymap *the_keymap, guint command_idx,
        GFunc func, gpointer user_data)
{
    command_row row;
    gchar *binding;
    GtkTreeIter iter;
    GtkListStore *bindings;
    GtkTreeModel *model_bind;
    if(!keymap_get_command_row(the_keymap, &row, command_idx))
      return;
    //get the first list element, if the list is empty returns
    model_bind = GTK_TREE_MODEL(row.bindings);
    if (!gtk_tree_model_get_iter_first(model_bind, &iter))
      return;
    //walk through the list and execute func on the binding
    do {
        //retrieve the binding
        gtk_tree_model_get(model_bind, &iter, 0, &binding, -1);
        //execute func
        func (binding, user_data);
        //free the binding
        g_free(binding);
    } while (gtk_tree_model_iter_next(model_bind, &iter));
    //unref bindings
    g_object_unref(row.bindings);
}
/**
 *  Search through keybindings for a specific binding
 *
 */
gint
lookup_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state)
{
  gint res;
  gchar *name = dnm_accelerator_name(keyval, state);
  res = lookup_keybinding_from_string(the_keymap, name);
  g_free(name);
  return res;
}

gint
lookup_keybinding_from_string (keymap * the_keymap,
        const gchar *binding_name)
{
  gpointer *value = g_hash_table_lookup (the_keymap->idx_from_keystring,
          binding_name);
  if (value)
    return *((guint *)value);
  else
    return -1;
}

/**
 * Look up a key binding by name.
 * FIXME: This is inefficent. The keybinding structures needs a rework to be more usefull and robust.
 *
 * deprecated and not reimplemented, developper should use
 * keymap_foreach_command_bindings
 * 
 * @param keymap
 * @param name
 * @returns the list of keybinding, or NULL if not found
 */
/*
GList *
lookup_keybindings_by_name (keymap * keymap, const gchar * name)
{
  gpointer value = g_hash_table_lookup(keymap->idx_from_name, name);
  if (value)
      return lookup_keybindings_by_idx(keymap, *(guint *)value);
  else {
      g_warning ("Could not find command '%s'\n", name);
      return NULL;
  }
}

GList *
lookup_keybindings_by_idx (keymap * keymap, guint idx)
{
  return g_array_index(keymap->bindings, GList *, idx);
}
*/

/**
 * Look up for a command index.
 *
 * @param keymap
 * @param name
 */
gint 
lookup_index_from_name (keymap * keymap, const gchar *command_name)
{
  gpointer value = g_hash_table_lookup(keymap->idx_from_name, command_name);
  if (value)
      return *(guint *) value;
  else
      return -1;
}

//do not free the result
//returns NULL if not found
const gchar *
lookup_name_from_idx (keymap * keymap, guint command_idx)
{
  const gchar *res = NULL;
  command_row row;
  if (!keymap_get_command_row(keymap, &row, command_idx))
      return NULL;
  switch (row.type) {
      case KeymapEntry:
          res = ((GtkActionEntry *) row.entry)->name;
          break;
      case KeymapToggleEntry:
          res = ((GtkToggleActionEntry *) row.entry)->name;
          break;
      case KeymapRadioEntry:
          res = ((GtkRadioActionEntry *) row.entry)->name;
          break;
      default:
          res = NULL;
          break;
  }
  g_object_unref(row.bindings);
  return res;
}

//do not free the result
//returns NULL if not found
const gchar *
lookup_label_from_idx (keymap * keymap, guint command_idx)
{
  const gchar *res = NULL;
  command_row row;
  if (!keymap_get_command_row(keymap, &row, command_idx))
      return NULL;
  switch (row.type) {
      case KeymapEntry:
          res = ((GtkActionEntry *) row.entry)->label;
          break;
      case KeymapToggleEntry:
          res = ((GtkToggleActionEntry *) row.entry)->label;
          break;
      case KeymapRadioEntry:
          res = ((GtkRadioActionEntry *) row.entry)->label;
          break;
      default:
          res = NULL;
          break;
  }
  g_object_unref(row.bindings);
  return res;
}

//returns the accel, "" if no accel defined. free the result
//the accel is the first keybinding of the list
static gchar *
keymap_get_accel(keymap *the_keymap, guint command_idx)
{
  command_row row;
  GtkTreeModel *model_bind;
  GtkTreeIter iter;
  gchar *res;
  
  if (!keymap_get_command_row(the_keymap, &row, command_idx))
      return g_strdup("");
  model_bind = GTK_TREE_MODEL(row.bindings);
  if (!gtk_tree_model_get_iter_first(model_bind, &iter)) {
      g_object_unref(row.bindings);
      return g_strdup("");
  }
  gtk_tree_model_get(model_bind, &iter, 0, &res, -1);
  g_object_unref(row.bindings);
  return res;
}

static gint
findActionGroupByName(gconstpointer a, gconstpointer b)
{
    GtkActionGroup *action_group = GTK_ACTION_GROUP(a);
    const gchar * searched_name = (const gchar *) b;
    return strcmp(gtk_action_group_get_name(action_group), b);
}

static GtkActionGroup *
get_action_group(keymap *the_keymap, DenemoGUI *gui)
{
  GList *keymap_action_group, *action_group_list;
  action_group_list = gtk_ui_manager_get_action_groups(Denemo.ui_manager);
  keymap_action_group = g_list_find_custom(action_group_list,
          the_keymap->action_group_name, findActionGroupByName);
  return GTK_ACTION_GROUP(keymap_action_group->data);
}

/* Updates the label of the widgets proxying an action with the bindings
 * present in the keymap. Updates the labels in all the guis open in the app
 * (case of multiple windows)
 */
static void
update_accel_labels(keymap *the_keymap, guint command_idx)
{
  GtkAction *action;
  GtkActionGroup *action_group;
  GList *guis;
  DenemoGUI *gui;
  //Getting the accel
  const gchar *command_name = lookup_name_from_idx(the_keymap, command_idx);
  GString *str=g_string_new("");
  //TODO don't use all the bindings as accels
  keymap_foreach_command_binding (the_keymap, command_idx,
          (GFunc)listname, str);
  //Prepare the new label
  gchar *base;
  //FIXME use translate_dnm_to_gtk
  base = lookup_label_from_idx(the_keymap, command_idx);
  gchar *c;
  for(c=str->str;*c;c++) {
      if(*c=='<') *c = ' ';
      if(*c=='>') *c = '-';
  }

  gchar *markup = g_strdup_printf("%s <span style=\"italic\" stretch=\"condensed\" weight=\"bold\" foreground=\"blue\">%s</span>", base, str->str);

  gui = Denemo.gui;
      action_group = get_action_group(the_keymap, gui);
      action = gtk_action_group_get_action(action_group, command_name);
      //For all widgets proxying the action, change the label
      GSList *h = gtk_action_get_proxies (action);
      for(;h;h=h->next) {
          GtkWidget *widget = h->data;
          GtkWidget *child = (GtkWidget *)gtk_bin_get_child(GTK_BIN(widget));
          if(GTK_IS_BUTTON(child)) {
              child = gtk_bin_get_child(GTK_BIN(child));
          }
          //FIXME others?? toolitem ...
          if(GTK_IS_LABEL(child)) {
              gtk_label_set_markup(GTK_LABEL(child), markup);
          }
      }

  //free allocated strings                                 
  g_free(markup);
  g_string_free(str, TRUE);
}

static void
remove_keybinding_bindings_helper(keymap *the_keymap, guint command_idx,
        const gchar *binding)
{
  gboolean found = FALSE;
  gchar *cur_binding;
  command_row row;
  GtkTreeIter iter;
  if (!keymap_get_command_row(the_keymap, &row, command_idx))
      return;
  GtkTreeModel *model_bind = GTK_TREE_MODEL(row.bindings);
  if (!gtk_tree_model_get_iter_first(model_bind, &iter)) {
      g_object_unref(row.bindings);
      return;
  }
  do {
      gtk_tree_model_get(model_bind, &iter, 0, &cur_binding, -1);
      if (!strcmp(binding, cur_binding)) {
          found = TRUE;
          break;
      }
      g_free(cur_binding);
  } while (gtk_tree_model_iter_next(model_bind, &iter));
 
  if (found) {
      gtk_list_store_remove(row.bindings, &iter);
  }

  g_object_unref(row.bindings);
}

void
remove_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state)
{
  gchar *name = dnm_accelerator_name(keyval, state);
  remove_keybinding_from_string(the_keymap, name);
  g_free(name);
}

void
remove_keybinding_from_string (keymap * the_keymap, const gchar *binding)
{
  gint *value;
  value = (gint *) g_hash_table_lookup(the_keymap->idx_from_keystring, binding);
  if (value) {
      remove_keybinding_bindings_helper(the_keymap, *value, binding);
      update_accel_labels(the_keymap, *value);
      g_hash_table_remove(the_keymap->idx_from_keystring, binding);
  }
}

/*
 * Insert a binding to the bindings of command_idx.
 */
static void
add_keybinding_bindings_helper(keymap *the_keymap, guint command_idx,
        const gchar *binding, KbdPosition pos)
{
  command_row row;
  GtkTreeIter iter;
  
  if (!keymap_get_command_row(the_keymap, &row, command_idx))
      return;
  GtkTreeModel *model_bind = GTK_TREE_MODEL(row.bindings);
  
  if (pos == POS_FIRST)
      gtk_list_store_prepend(row.bindings, &iter);
  else if (pos == POS_LAST)
      gtk_list_store_append(row.bindings, &iter);
  else {
    g_object_unref(row.bindings);
    return;
  }
         
  gtk_list_store_set(row.bindings, &iter, 0, binding, -1);
  
  g_object_unref(row.bindings);
}

gint
add_keybinding_from_name(keymap * the_keymap, gint keyval,
        GdkModifierType state, const gchar *command_name, KbdPosition pos) 
{
  gpointer value;
  guint command_idx;
  value = g_hash_table_lookup(the_keymap->idx_from_name, command_name);
  if (!value) {
      g_warning("add_keybinding: %s, command does not exist");
      return -1;
  }
  command_idx = *(guint *) value;
  return add_keybinding_from_idx(the_keymap, keyval, state, command_idx, pos);
}
    
/**
 * Adds a keybinding to the_keymap.  If the key was already bound,
 * this function removes the old binding and replaces it, returning
 * the number of the command this keybinding was attached to. Otherwise
 * returns -1. 
 */
gint
add_keybinding_from_idx (keymap * the_keymap, gint keyval,
        GdkModifierType state, guint command_idx, KbdPosition pos)
{
  guint *new_idx;
  gint old_command_idx;
  gpointer value;
  gchar *kb_name;
  gboolean flag_update_accel;
  
  kb_name = dnm_accelerator_name(keyval, state);
  old_command_idx = lookup_keybinding(the_keymap, keyval, state);
  
  //if the keybinding was previously used, remove it from bindings and update
  //its accels
  if (old_command_idx != -1) {
      remove_keybinding_bindings_helper(the_keymap, old_command_idx, kb_name);
      update_accel_labels(the_keymap, old_command_idx);
  }  

  //add the keybinding to the binding on idx_command
   add_keybinding_bindings_helper(the_keymap, command_idx, kb_name, pos);

  //update the accel labels of the command
  update_accel_labels(the_keymap, command_idx);

  //add or modify an entry in idx_from_keystring
  new_idx = (guint *) g_malloc(sizeof(guint));
  *new_idx = command_idx;
  g_hash_table_insert(the_keymap->idx_from_keystring, g_strdup(kb_name),
          new_idx);
  
  g_free(kb_name);
  return old_command_idx;
}

//helper for the keymap update function
//old_command_idx : the command that had the binding before the accel change
//new_command_idx : the command whose accel is changed
//old_accel : the accel of new_command before the accel change
//new_accel : the current accel of new_command
//return 1 if the accel was changed, 0 otherwise
//DEPRECATED we do not use gtk accels anymore
/*
static gint
keymap_update_accel_helper(keymap *the_keymap, gint old_command_idx,
        gint new_command_idx, const gchar *old_accel, const gchar *new_accel)
{
    //if the old_accel is equal to the new_accel, don't do anything
    if (!strcmp(old_accel, new_accel))
        return 0;

    //remove new_accel from the bindings of old_command
    if (old_command_idx != -1)
      remove_keybinding_bindings_helper(the_keymap, old_command_idx, new_accel);
        
    //prepend new_accel to the bindings of new_command
    add_keybinding_bindings_helper(the_keymap, new_command_idx, new_accel, 0);

    //if old_command != -1 and != new_command set accel
    if (old_command_idx != -1 && old_command_idx != new_command_idx)
        setAccelKey_from_idx(the_keymap, old_command_idx);
        
    //if new command != -1 set accel
    if (new_command_idx != -1)
        setAccelKey_from_idx(the_keymap, new_command_idx);
        
    //if old_command != new_command modify idx_from_keystring
    if (old_command_idx != new_command_idx) {
        guint * value = (guint *) g_hash_table_lookup(
                the_keymap->idx_from_keystring, new_accel);
        if (value)
            *value = new_command_idx;
        else {
            value = (guint *) g_malloc(sizeof(guint));
            *value = new_command_idx;
            g_hash_table_insert(the_keymap->idx_from_keystring,
                    g_strdup(new_accel), value);
        }
        
    }
    return 1;
}
*/
//we have to reproduce this function here since it is static in gtkmenu.c
static void
stolen_gtk_menu_stop_navigating_submenu (GtkMenu *menu)
{
  if (menu->navigation_region) {
    gdk_region_destroy (menu->navigation_region);
    menu->navigation_region = NULL;
  }
  if (menu->navigation_timeout) {
    g_source_remove (menu->navigation_timeout);
    menu->navigation_timeout = 0;
  }
}

//call this function after the function supposed to update the accel of action
//to (keyval, modifiers)
//returns 1 if the accelerator of action is equal to (keyval, modifiers) (ie
//the change was successful, 0 otherwise
//DEPRECATED we do not use gtk accels anymore
/*
gint
keymap_update_accel(keymap *the_keymap, GtkAction *action, guint keyval,
        GdkModifierType modifiers)
{
  gint res;
  GList *tmp;
  GtkAccelKey accel_key;
  gchar *new_command_old_accel;
  gchar *new_command_new_accel;
  const gchar *new_command_path;
  gint old_command_idx = lookup_keybinding(the_keymap, keyval, modifiers);
  gint new_command_idx = lookup_index_from_name(the_keymap,
          gtk_action_get_name(action));
#ifdef DEBUG
  g_print("Bindings before accel update\n");
  dump_command_info(the_keymap, old_command_idx);
  dump_command_info(the_keymap, new_command_idx);
#endif
  new_command_old_accel = keymap_get_accel(the_keymap, new_command_idx);
  new_command_path = gtk_action_get_accel_path(action);
  gtk_accel_map_lookup_entry(new_command_path, &accel_key);
  new_command_new_accel = dnm_accelerator_name(accel_key.accel_key,
          accel_key.accel_mods);
  //if the accel of new_command has changed clean the keymap
  res = keymap_update_accel_helper(the_keymap, old_command_idx, new_command_idx,
             new_command_old_accel, new_command_new_accel);
#ifdef DEBUG
  g_print("Bindings after accel update\n");
  dump_command_info(the_keymap, old_command_idx);
  dump_command_info(the_keymap, new_command_idx);
#endif
  g_free(new_command_old_accel);
  g_free(new_command_new_accel);
  return res;
}
*/
gint
keymap_accel_quick_edit_snooper(GtkWidget *grab_widget, GdkEventKey *event,
		gpointer func_data)
{
  guint keyval;
  GdkModifierType modifiers;
  GtkAction *action;
  keymap *the_keymap = (keymap *) func_data;
  GtkMenu *menu = GTK_MENU(grab_widget);
  GtkMenuClass *menu_class = GTK_MENU_GET_CLASS(menu);
  GtkMenuShellClass *parent_class = g_type_class_peek_parent(menu_class);
  //check if this a quick edit
  //first try to handle the event with the key_press handler of the parent
  //class (this allows navigation in the menu to take precedence over
  //the quick edit), 
  stolen_gtk_menu_stop_navigating_submenu (menu);
  if (GTK_WIDGET_CLASS (parent_class)->key_press_event (grab_widget,
              event)) {
      //This was some navigation command in the submenu, and it was
      //performed, no need to process further
      return TRUE;
  }
  //If the KeyEvent is only a modifier, stop processing here
  if (isModifier(event))
      return TRUE;
  //TODO here could be added some check to see if we allow the quick edit
  //for exemple, one could suppress quick edits if the new accel is
  //already the keybind of another function.
  keyval = event->keyval;
  modifiers = dnm_sanitize_key_state(event);
  //TODO this may be evil since active_menu_item is not available in the
  //doc of GTK. It is accessible all the same, and we NEED it
  action = 
#if GTK_MINOR_VERSION <10
    g_object_get_data(G_OBJECT(GTK_MENU_SHELL(menu)->active_menu_item), "action");
#else
  gtk_widget_get_action(GTK_MENU_SHELL(menu)->active_menu_item);
#endif
  gint idx = lookup_index_from_name(the_keymap, gtk_action_get_name(action));
  //If this menu item has no action or the action is not registered in the
  //keymap, we give up
  if (!action || idx == -1)
    return TRUE;
  //Add the keybinding
  add_keybinding_from_idx(the_keymap, keyval, modifiers, idx, POS_FIRST);
  return TRUE;
}

gboolean
execute_callback_from_idx(keymap *the_keymap, guint command_idx, DenemoGUI *gui)
{
  gboolean res = TRUE;
  const gchar *command_name;
  GtkAction *action;
  GtkActionGroup *action_group;
  gpointer f;

  command_name = lookup_name_from_idx(the_keymap, command_idx);
  action_group = get_action_group(the_keymap, gui);
  action = gtk_action_group_get_action(action_group, command_name);
#if DEBUG
  //check for the existence of a callback, enables to detect action entries
  //where the callback is lacking
  command_row row;
  if (!keymap_get_command_row(the_keymap, &row, command_idx))
      return FALSE;
  g_object_unref(row.bindings);
  switch (row.type) {
      case KeymapEntry:
          f = (((GtkActionEntry *) row.entry)->callback);
          if (!f) {
            res = FALSE;
          }
          break;
      case KeymapToggleEntry:
          f = (((GtkToggleActionEntry *) row.entry)->callback);
          if (f) {
            res = FALSE;
          }
          break;
      case KeymapRadioEntry:
          //Since the callback is added once for all radio entries at the time
          //they are included in the action group, we do not
          //perform a check here
          break;
  }
#endif
  gtk_action_activate(action);
  return res;
}

//prints info on the data of the keymap relative to a command
void
dump_command_info (keymap *the_keymap, gint command_idx)
{
  gchar *cur_binding;
  command_row row;
  GtkTreeIter iter;
  GtkTreeModel *model_bind;
  
  if (command_idx == -1) {
      g_print("no command\n");
      return;
  }
  g_print ("command %s (%d)\nBindings:\n",
          lookup_name_from_idx(the_keymap, command_idx), command_idx);
  if(!keymap_get_command_row(the_keymap, &row, command_idx))
      return;
  model_bind = GTK_TREE_MODEL(row.bindings);
  if(!gtk_tree_model_get_iter_first(model_bind, &iter)) {
      g_object_unref(row.bindings);
      return;
  }
  do {
    gtk_tree_model_get(model_bind, &iter, 0, &cur_binding, -1);
    g_print("\t%s (%d)\n", cur_binding,
            lookup_keybinding_from_string(the_keymap, cur_binding));
    g_free(cur_binding);
  } while (gtk_tree_model_iter_next(model_bind, &iter));
  g_object_unref(row.bindings);
}




/**
 * This checks to see if there's a .denemo/keymaps directory in the user's
 * home directory, tries to create one if there isn't, and returns the
 * path to it
 *
 */

const gchar *
locatekeymapdir ()
{
  static gchar *keymapdir = NULL;

  gboolean err;
  if (!keymapdir)
    {
      keymapdir = g_build_filename (locatedotdenemo(), "keymaps", NULL);
    }
  err = g_mkdir_with_parents(keymapdir, 0770);
  if(err) {
    warningdialog("Could not create .denemo/keymaps for your keymaps");
    g_free(keymapdir);
    keymapdir = NULL;
  }

  return keymapdir;
}





struct callbackdata
{
  keymap *the_keymap;
  GtkWidget *filesel;
};

/**
 * This function is a callback that is wrapper for
 * load_keymap_file 
 *FIXME note that non xml file support has been commented out
 */
void
load_keymap_from_dialog (GtkWidget * widget, struct callbackdata *cbdata)
{
  gchar *name = (gchar *)
    gtk_file_selection_get_filename (GTK_FILE_SELECTION (cbdata->filesel));
  if(g_file_test (name, G_FILE_TEST_EXISTS))
     load_keymap_file_named(cbdata->the_keymap, NULL, name);
}

/**
 * Function for loading a keymap from an arbitrary place by way of
 * a user dialog.  Similar to file_open. called from kbd-interface.c:configure_keyboard_dialog_OLD
 */

void
load_keymap_dialog_location (GtkWidget * widget, keymap * the_keymap, gchar *location)
{
  GtkWidget *filesel;
  static struct callbackdata cbdata;//FIXME why static????
  filesel = gtk_file_selection_new (_("Load keymap"));
  gtk_file_selection_set_filename (GTK_FILE_SELECTION (filesel), location);
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
		      "clicked", GTK_SIGNAL_FUNC (load_keymap_from_dialog),
		      &cbdata);
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->ok_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->cancel_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));

  cbdata.the_keymap = the_keymap;
  cbdata.filesel = filesel;
  gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
  gtk_window_set_position (GTK_WINDOW (filesel), GTK_WIN_POS_MOUSE);
  gtk_widget_show (filesel);
}

void
load_keymap_dialog (GtkWidget * widget, keymap * the_keymap)
{
  gchar *keymapdir = g_strdup_printf("%s%c", locatekeymapdir(),G_DIR_SEPARATOR);
  if(keymapdir)
    load_keymap_dialog_location (widget, the_keymap, keymapdir);
  else
    warningdialog("Cannot access your local .denemo");
  g_free(keymapdir);
}

void
load_system_keymap_dialog (GtkWidget * widget, keymap * the_keymap)
{
  gchar *systemwide = g_build_filename (get_data_dir (), "keymaps", DEFAULT_KEYMAP,
                                        NULL);
  if(systemwide)
    load_keymap_dialog_location (widget, the_keymap, systemwide);
  else
    warningdialog("Installation error");
  g_free(systemwide);
}


/**
 * Wrapper function to load keymap file
 * 
 */
void
load_default_keymap_file_wrapper (GtkWidget * widget, keymap * the_keymap)
{
  load_default_keymap_file (the_keymap);

}

/*
 * load_keymap_file_named: load a keymap file localrc, or if it fails, systemwide

 */
static void
load_keymap_file_named (keymap * the_keymap, gchar *localrc, gchar *systemwide) {

  // keymap_clear_bindings (the_keymap); this doesn't prevent keybindings hanging around???

  if(localrc) {
    g_print ("Trying local file %s as xml...", localrc);
    if (load_xml_keymap (localrc, the_keymap) == -1)
      {
	g_print ("..no.\nTrying systemwide file %s as xml...", systemwide);
	if (load_xml_keymap (systemwide, the_keymap) == -1)
	  {
	    g_print ("..no.\nNo useful keymaps found.\n");
	    no_map_dialog ();
	  }
	else
	  g_print ("..ok.\n");
      }
    else
      g_print ("..ok.\n");
  }
  else {
    if (load_xml_keymap (systemwide, the_keymap) == -1)
      warningdialog("Could not load keymap file selected");
  }
}

/**
 * Load the either the local default keymap 
 * or (if that doesn't load) the global default keymap
 */
void
load_default_keymap_file (keymap * the_keymap)
{
  gchar *localrc = NULL;
  const gchar *keymapdir = locatekeymapdir ();
  gchar *systemwide = g_build_filename (get_data_dir (), "keymaps", DEFAULT_KEYMAP,
                                        NULL);
  //g_print ("systemwide = %s\n", systemwide);
  if(keymapdir)
    localrc = g_build_filename (keymapdir, DEFAULT_KEYMAP, NULL);
  load_keymap_file_named (the_keymap, localrc, systemwide);
  g_free(localrc);
  g_free(systemwide);
}

static GScannerConfig scanner_config_template = {
  (" \t\r\n") /* cset_skip_characters */ ,
  (G_CSET_a_2_z "_0123456789/." G_CSET_A_2_Z) /* cset_identifier_first */ ,
  (G_CSET_a_2_z
   "_0123456789/."
   G_CSET_A_2_Z G_CSET_LATINS G_CSET_LATINC) /* cset_identifier_nth */ ,
  ("#\n") /* cpair_comment_single */ ,

  FALSE /* case_sensitive */ ,

  TRUE /* skip_comment_multi */ ,
  TRUE /* skip_comment_single */ ,
  TRUE /* scan_comment_multi */ ,
  TRUE /* scan_identifier */ ,
  TRUE /* scan_identifier_1char */ ,
  FALSE /* scan_identifier_NULL */ ,
  TRUE /* scan_symbols */ ,
  FALSE /* scan_binary */ ,
  FALSE /* scan_octal */ ,
  FALSE /* scan_float */ ,
  FALSE /* scan_hex */ ,
  FALSE /* scan_hex_dollar */ ,
  TRUE /* scan_string_sq */ ,
  TRUE /* scan_string_dq */ ,
  FALSE /* numbers_2_int */ ,
  FALSE /* int_2_float */ ,
  TRUE /* identifier_2_string */ ,
  TRUE /* char_2_token */ ,
  TRUE /* symbol_2_token */ ,
  FALSE				/* scope_0_fallback */
};

/**
 * Callback for saving the keymap to a given file
 *
 */
void
save_keymap_from_dialog (GtkWidget * widget, struct callbackdata *cbdata)
{
  save_xml_keymap ((gchar *)
		   gtk_file_selection_get_filename (GTK_FILE_SELECTION
						    (cbdata->filesel)),
		   cbdata->the_keymap);

}

/**
 * Function for saving a keymap to an arbitrary place by way of
 * a user dialog.  Similar to file_saveas. 
 */
void
save_keymap_dialog (GtkWidget * widget, keymap * the_keymap)
{
  GtkWidget *filesel;
  static gchar *keymapdir = NULL;//FIXME static????
  static struct callbackdata cbdata;

  if (!keymapdir)
    keymapdir = g_strdup_printf("%s%c", locatekeymapdir(),G_DIR_SEPARATOR);
  filesel = gtk_file_selection_new (_("Save keymap"));
  gtk_file_selection_set_filename (GTK_FILE_SELECTION (filesel), keymapdir);
  gtk_signal_connect (GTK_OBJECT (GTK_FILE_SELECTION (filesel)->ok_button),
		      "clicked", GTK_SIGNAL_FUNC (save_keymap_from_dialog),
		      &cbdata);
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->ok_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));
  gtk_signal_connect_object (GTK_OBJECT
			     (GTK_FILE_SELECTION (filesel)->cancel_button),
			     "clicked", GTK_SIGNAL_FUNC (gtk_widget_destroy),
			     GTK_OBJECT (filesel));

  cbdata.the_keymap = the_keymap;
  cbdata.filesel = filesel;
  gtk_window_set_modal (GTK_WINDOW (filesel), TRUE);
  gtk_window_set_position (GTK_WINDOW (filesel), GTK_WIN_POS_MOUSE);
  gtk_widget_show (filesel);
}

/**
 * Wrapper function for saving the keymap to the standard place
 *
 */
void
save_default_keymap_file_wrapper (GtkWidget * widget, DenemoGUI *gui)
{
  keymap * the_keymap = Denemo.prefs.the_keymap;  
  save_default_keymap_file (widget, the_keymap);
}

/**
 * Saves the keymap as the user's default keymap
 *
 */
void
save_default_keymap_file (GtkWidget *widget, keymap * the_keymap)
{
  gchar *localrc = NULL;
  const gchar *keymapdir = locatekeymapdir ();
  if(keymapdir)
    localrc = g_build_filename (keymapdir, DEFAULT_KEYMAP, NULL);
  save_xml_keymap (localrc, the_keymap);
  g_free(localrc);
}

/**
 * This function gets the caller a string useful for display
 */
void
set_state (gint state, gchar ** value)
{
  switch (state)
    {
    case 1:
      *value = "Shift+";
      break;
    case 4:
      *value = "Ctrl+";
      break;
    case 8:
      *value = "Alt+";
      break;
    case 5:
      *value = "Ctrl+Shift+";
      break;
    case 9:
      *value = "Alt+Shift+";
      break;
    case 12:
      *value = "Alt+Ctrl+";
      break;
    case 13:
      *value = "Alt+Ctrl+Shift+";
      break;
    }
}

static void
command_name_data_function (GtkTreeViewColumn *col,
                            GtkCellRenderer   *renderer,
                            GtkTreeModel      *model,
                            GtkTreeIter       *iter,
                            gpointer           user_data)
{
    KeymapCommandType type;
    gpointer entry;
    const gchar *name;
    gtk_tree_model_get(model, iter,
            COL_TYPE, &type,
            COL_ENTRY, &entry,
            -1);
    switch (type) {
        case KeymapEntry:
            name = (((GtkActionEntry *) entry)->name);
            break;
        case KeymapToggleEntry:
            name = (((GtkToggleActionEntry *) entry)->name);
            break;
        case KeymapRadioEntry:
            name = (((GtkRadioActionEntry *) entry)->name);
            break;
    }
    g_object_set(renderer, "text", name, NULL);
}

static gboolean
search_equal_func(GtkTreeModel *model, gint column, const gchar *key,
        GtkTreeIter *iter, gpointer search_data)
{
  KeymapCommandType type;
  gpointer entry;
  const gchar *name;
  gboolean res;
  gchar *name_trunk;
  gtk_tree_model_get(model, iter,
          COL_TYPE, &type,
          COL_ENTRY, &entry,
          -1);
  switch (type) {
      case KeymapEntry:
          name = (((GtkActionEntry *) entry)->name);
          break;
      case KeymapToggleEntry:
          name = (((GtkToggleActionEntry *) entry)->name);
          break;
      case KeymapRadioEntry:
          name = (((GtkRadioActionEntry *) entry)->name);
          break;
  }
  name_trunk = g_strndup(name, strlen(key));
  res = strcmp(name_trunk, key) == 0;
  g_free(name_trunk);
  return !res;
}

GtkWidget *
keymap_get_command_view(keymap *the_keymap)
{
  GtkScrolledWindow *res2;
  GtkTreeView *res;
  GtkTreeViewColumn *col;
  GtkCellRenderer *renderer;
  GtkTreeSelection *selection;
  GtkTreeIter iter;
  GtkTreeModel *model;

  model = GTK_TREE_MODEL(the_keymap->commands);
  //setting up the tree view
  res = GTK_TREE_VIEW(gtk_tree_view_new());
  gtk_tree_view_set_model(res, model);
  
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, N_("Commands"));
  gtk_tree_view_append_column(res, col);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func(col, renderer,
          command_name_data_function, NULL, NULL);
 
  selection = gtk_tree_view_get_selection(res);
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_BROWSE);

  gtk_tree_view_set_search_equal_func(res, search_equal_func, NULL, NULL);
  gtk_tree_view_set_enable_search(res, TRUE);
  
  //setting up the scrolledwindow
  res2 = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new(NULL, NULL));
  gtk_container_add(GTK_CONTAINER(res2), GTK_WIDGET(res));
  gtk_scrolled_window_set_policy(res2, GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);

  //FIXME adapt so that 10~15 rows are visible
  gtk_widget_set_size_request(GTK_WIDGET(res2), -1, 300);
  
  return GTK_WIDGET(res2);
}

void row_inserted_handler(GtkTreeModel *model, GtkTreePath *arg1,
        GtkTreeIter *arg2, gpointer user_data)
{
    keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
    //g_print("insert\n");
    if (cbdata->command_idx != -1)
        update_accel_labels(cbdata->the_keymap, cbdata->command_idx);
}

void row_deleted_handler(GtkTreeModel *model, GtkTreePath *arg1,
        gpointer user_data)
{
    keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
    //g_print("delete\n");
    if (cbdata->command_idx != -1)
        update_accel_labels(cbdata->the_keymap, cbdata->command_idx);
}

//Performs cleanup on the keymap when a command view is closed
void
keymap_cleanup_command_view(keyboard_dialog_data *data)
{
    GtkTreeModel *model;
    model = gtk_tree_view_get_model(data->binding_view);
    if (model) {
        g_signal_handlers_disconnect_by_func(model, row_deleted_handler, data);
    }  
}

gboolean
keymap_change_binding_view_on_command_selection(GtkTreeSelection *selection,
        GtkTreeModel *model,
        GtkTreePath *path,
        gboolean path_currently_selected,
        gpointer data)
{
  GtkTreeView *binding_view;
  GtkListStore *bindings;
  GtkTreeIter iter;
  GtkTreeModel *command_model;
  GtkTreeModel *old_binding_model;
  GtkTextBuffer *text_buffer;
  KeymapCommandType type;
  gpointer entry;
  gint *array;
  const gchar *tooltip;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *)data;

  //if the same command is selected again, we do nothing
  if (path_currently_selected)
      return TRUE;
  
  //getting the binding_view
  binding_view = cbdata->binding_view;

  //disconnecting signals of the old binding view
  old_binding_model = gtk_tree_view_get_model(binding_view);
  if (old_binding_model) {
    //g_signal_handlers_disconnect_by_func(old_binding_model,
    //        row_inserted_handler, data);
    g_signal_handlers_disconnect_by_func(old_binding_model,
            row_deleted_handler, data);
  }
  
  //getting the new model
  gtk_tree_model_get_iter(model, &iter, path);
  gtk_tree_model_get(model, &iter,
          COL_TYPE, &type,
          COL_ENTRY, &entry,
          COL_BINDINGS, &bindings, -1);
  //getting the new command_idx
  array = gtk_tree_path_get_indices(path);
  cbdata->command_idx = array[0];
  
  //setting the model and releasing our reference
  gtk_tree_view_set_model(binding_view, GTK_TREE_MODEL(bindings));
  //g_signal_connect(bindings, "row-inserted", row_inserted_handler, data);
  g_signal_connect(bindings, "row-deleted", G_CALLBACK(row_deleted_handler),
          data);
  g_object_unref(bindings);
  //changing the tooltip
  text_buffer = gtk_text_view_get_buffer(cbdata->text_view);
  switch (type) {
      case KeymapEntry:
          tooltip = ((GtkActionEntry *) entry)->tooltip;
          break;
      case KeymapToggleEntry:
          tooltip = ((GtkToggleActionEntry *) entry)->tooltip;
          break;
      case KeymapRadioEntry:
          tooltip = ((GtkRadioActionEntry *) entry)->tooltip;
          break;
  }
  if(tooltip){
    gchar *plain;
    pango_parse_markup (tooltip,-1,0,NULL, &plain, 0, NULL);
                                             
    gtk_text_buffer_set_text(text_buffer, plain, -1);
    g_free(plain);
  }
  //perform the selection
  return TRUE;
}

GtkWidget *
keymap_get_binding_view()
{
  GtkScrolledWindow *res2;
  GtkTreeView *res;
  GtkTreeViewColumn *col;
  GtkCellRenderer *renderer;
  GtkTreeModel *model;
  GtkTreeIter iter;
  GtkTreeSelection *selection;

  //setting up the tree view
  res = GTK_TREE_VIEW(gtk_tree_view_new());
  
  col = gtk_tree_view_column_new();
  gtk_tree_view_column_set_title(col, N_("Bindings"));
  gtk_tree_view_column_set_sizing(col, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
  gtk_tree_view_append_column(res, col);

  renderer = gtk_cell_renderer_text_new();
  gtk_tree_view_column_pack_start(col, renderer, TRUE);
  gtk_tree_view_column_add_attribute(col, renderer, "text", 0);
  
  selection = gtk_tree_view_get_selection(res);
  gtk_tree_selection_set_mode(selection, GTK_SELECTION_SINGLE);

  gtk_tree_view_set_enable_search(res, FALSE);

  gtk_tree_view_set_reorderable(res, TRUE);
  //setting up the scrolledwindow
  res2 = GTK_SCROLLED_WINDOW(gtk_scrolled_window_new(NULL, NULL));
  gtk_container_add(GTK_CONTAINER(res2), GTK_WIDGET(res));
  gtk_scrolled_window_set_policy(res2, GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  //FIXME adapt so that 10~15 rows are visible
  gtk_widget_set_size_request(GTK_WIDGET(res2), -1, 300);
  
  return GTK_WIDGET(res2);
}
