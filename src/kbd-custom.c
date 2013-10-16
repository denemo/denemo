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
#if GTK_MAJOR_VERSION==3
#include <gdk/gdkkeysyms-compat.h>      //FIXME Look for something more gtk3 like
#endif
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
#include "file.h"
#include "view.h"
#include "keymapio.h"
#include "utils.h"

#define DENEMO_TWO_KEY_SEPARATOR ","
#if 0                           //GTK_MINOR_VERSION < 10
//Hmm, should we define these as 0, so that they don't mask anything in gtk 2.8
#define  GDK_SUPER_MASK ( 1 << 26)
#define  GDK_HYPER_MASK  (1 << 27)
#define  GDK_META_MASK   (1 << 28)
#endif



#define USER_KEYMAP "Default"

/**
 * load_keymap_files:
 * @files: The files to test. Must end with NULL. String are freed.
 *
 * Takes a list of keymap and try to load them until one is loaded successfully.
 *
 * Returns: FALSE if no keymap has been loaded, TRUE either.
 **/

static gboolean
load_keymap_files(gchar* files[])
{
  gboolean ret = FALSE;
  gint i;

  for(i = 0; files[i]; i++)
    {
      if(!ret && load_xml_keymap(files[i]) == 0)
        ret = TRUE;
      g_free(files[i]);
    }
  return ret;
}

gboolean
is_action_id_builtin(gint id)
{
  command_row command;
  keymap_get_command_row(Denemo.map, &command, id);
  return command.script_type == COMMAND_BUILTIN;
}

gboolean
is_action_name_builtin(gchar* command_name)
{
  gint* id = NULL;

  id = g_hash_table_lookup (Denemo.map->idx_from_name, command_name);
  if(!id)
  {
    g_debug("Requesting a invalid action name");
    return TRUE;
  }
  
  return is_action_id_builtin(*id);
}

void
command_row_init(command_row *command)
{
  command->name = _("No name");
  command->label = _("No label");
  command->tooltip = _("No indication what this done beyond the name and label");
  command->hidden = FALSE;
  command->deleted = FALSE;
  command->bindings = gtk_list_store_new (1, G_TYPE_STRING);
  command->callback = NULL;
  command->action = NULL;
  command->type = KeymapEntry;
  command->script_type = COMMAND_BUILTIN;
  command->locations = NULL;
}

void
dnm_clean_event (GdkEventKey * event)
{
  if (!Denemo.prefs.strictshortcuts)
    {
      guint ret;
      //g_print("Called %s\n", gdk_keyval_name(event->keyval));
      gdk_keymap_translate_keyboard_state (gdk_keymap_get_default (), event->hardware_keycode, GDK_MOD2_MASK /*NumLock forcing numeric keypad to give numbers */ ,
                                           0 /*group 0 */ , &ret, NULL, NULL, NULL);
      if (ret >= 'A' && ret <= 'G')
        ret += ('a' - 'A');
      event->keyval = ret;

      //g_print("Changed to %s\n", gdk_keyval_name(event->keyval));
    }
}



/* Returns the state of the event after removing the modifiers consumed by the
 * system and unwanted modifiers. Use this before doing anything based on the
 * (keyval, state) pair in an event handler.
 */
guint
dnm_sanitize_key_state (GdkEventKey * event)
{
  guint ret = event->state;
  if (!Denemo.prefs.strictshortcuts)
    {
      return ret;
    }

#if 1
  GdkModifierType consumed;
  /* We want to ignore irrelevant modifiers like ScrollLock */

  gdk_keymap_translate_keyboard_state (gdk_keymap_get_default (), event->hardware_keycode, event->state, event->group, NULL, NULL, NULL, &consumed);
  /* removing consumed modifiers from ret */
  ret &= ~consumed;
  /* removing other unwanted modifiers from event->state */
  ret &= (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_MOD1_MASK | GDK_MOD2_MASK | GDK_MOD3_MASK | GDK_MOD4_MASK | GDK_MOD5_MASK /*    these make numlock required to be off for example */ );
#endif
  return ret;
}

/* Returns the state of the event after removing the modifiers consumed by the
 * system and even more unwanted modifiers. Use this if sanitize is insufficient.
 */
guint
dnm_hyper_sanitize_key_state (GdkEventKey * event)
{
  guint ret = event->state;
#if 1
  GdkModifierType consumed;
  /* We want to ignore irrelevant modifiers like ScrollLock */

  gdk_keymap_translate_keyboard_state (gdk_keymap_get_default (), event->hardware_keycode, event->state, event->group, NULL, NULL, NULL, &consumed);
  /* removing consumed modifiers from ret */
  ret &= ~consumed;
  /* removing other unwanted modifiers from event->state */
  ret &= (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_MOD1_MASK);
#endif
  return ret;
}

/* Returns the state of the event as for hyper_sanitize
 * additionally undoes the effect of CapsLock on keyval when Shift is not pressed.
 * note that event->keycode & keystring are not altered, leaving event inconsistent.
 * this could conceivably become a problem in other developments.
 * Use this if hyper sanitize is insufficient.
 */
guint
dnm_meta_sanitize_key_state (GdkEventKey * event)
{
  guint ret = event->state;
#if 1
  if (ret & GDK_LOCK_MASK)
    {
      if (!(ret & GDK_SHIFT_MASK))
        event->keyval += ('a' - 'A');
    }

  /* removing everything other than control shift and alt modifiers from event->state */
  ret &= (GDK_CONTROL_MASK | GDK_SHIFT_MASK | GDK_MOD1_MASK);
#endif
  return ret;
}



/*
 * Returns True if the key event is just a modifier key, False otherwise
 * TODO look for a gdk function doing that properly
 */
gboolean
isModifier (GdkEventKey * event)
{
  /* This check for modifier values on the event may not be right,
     if the contents of gdkkeysyms.h are OS-dependent. I don't believe
     they are. */
  return (event->keyval >= GDK_Shift_L && event->keyval <= GDK_Hyper_R) || (event->keyval == GDK_Num_Lock);
}

static inline gboolean
is_alt (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'a' || string[1] == 'A') && (string[2] == 'l' || string[2] == 'L') && (string[3] == 't' || string[3] == 'T') && (string[4] == '>'));
}

static inline gboolean
is_ctl (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'c' || string[1] == 'C') && (string[2] == 't' || string[2] == 'T') && (string[3] == 'l' || string[3] == 'L') && (string[4] == '>'));
}

static inline gboolean
is_modx (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'm' || string[1] == 'M') && (string[2] == 'o' || string[2] == 'O') && (string[3] == 'd' || string[3] == 'D') && (string[4] >= '1' && string[4] <= '5') && (string[5] == '>'));
}

static inline gboolean
is_ctrl (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'c' || string[1] == 'C') && (string[2] == 't' || string[2] == 'T') && (string[3] == 'r' || string[3] == 'R') && (string[4] == 'l' || string[4] == 'L') && (string[5] == '>'));
}

static inline gboolean
is_shft (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 's' || string[1] == 'S') && (string[2] == 'h' || string[2] == 'H') && (string[3] == 'f' || string[3] == 'F') && (string[4] == 't' || string[4] == 'T') && (string[5] == '>'));
}

static inline gboolean
is_shift (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 's' || string[1] == 'S') && (string[2] == 'h' || string[2] == 'H') && (string[3] == 'i' || string[3] == 'I') && (string[4] == 'f' || string[4] == 'F') && (string[5] == 't' || string[5] == 'T') && (string[6] == '>'));
}

static inline gboolean
is_control (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'c' || string[1] == 'C') && (string[2] == 'o' || string[2] == 'O') && (string[3] == 'n' || string[3] == 'N') && (string[4] == 't' || string[4] == 'T') && (string[5] == 'r' || string[5] == 'R') && (string[6] == 'o' || string[6] == 'O') && (string[7] == 'l' || string[7] == 'L') && (string[8] == '>'));
}

static inline gboolean
is_release (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'r' || string[1] == 'R') && (string[2] == 'e' || string[2] == 'E') && (string[3] == 'l' || string[3] == 'L') && (string[4] == 'e' || string[4] == 'E') && (string[5] == 'a' || string[5] == 'A') && (string[6] == 's' || string[6] == 'S') && (string[7] == 'e' || string[7] == 'E') && (string[8] == '>'));
}

static inline gboolean
is_meta (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'm' || string[1] == 'M') && (string[2] == 'e' || string[2] == 'E') && (string[3] == 't' || string[3] == 'T') && (string[4] == 'a' || string[4] == 'A') && (string[5] == '>'));
}

static inline gboolean
is_super (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 's' || string[1] == 'S') && (string[2] == 'u' || string[2] == 'U') && (string[3] == 'p' || string[3] == 'P') && (string[4] == 'e' || string[4] == 'E') && (string[5] == 'r' || string[5] == 'R') && (string[6] == '>'));
}

static inline gboolean
is_hyper (const gchar * string)
{
  return ((string[0] == '<') && (string[1] == 'h' || string[1] == 'H') && (string[2] == 'y' || string[2] == 'Y') && (string[3] == 'p' || string[3] == 'P') && (string[4] == 'e' || string[4] == 'E') && (string[5] == 'r' || string[5] == 'R') && (string[6] == '>'));
}

void
dnm_accelerator_parse (const gchar * accelerator, guint * accelerator_key, GdkModifierType * accelerator_mods)
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
          len = 0;
        }
    }

  if (accelerator_key)
    //The line we modify, so that uppercase letter are processed as we want
    //  *accelerator_key = gdk_keyval_to_lower (keyval);
    *accelerator_key = keyval;
  if (accelerator_mods)
    *accelerator_mods = mods;
}

//#include "gdkdisplay-x11.h"
//gboolean gdk_keymap_get_caps_lock_state (GdkKeymapX11 *keymap_x11) {
//return keymap_x11->caps_lock_state;
//}
// if((gdk_keymap_get_caps_lock_state (gdk_keymap_get_default())!=0) != ((accelerator_mods&GDK_SHIFT_MASK)!=0))


gchar *
dnm_accelerator_name (guint accelerator_key, GdkModifierType accelerator_mods)
{

  if (!Denemo.prefs.strictshortcuts)
    {
      GString *name;

      name = g_string_new (gdk_keyval_name (accelerator_key));
      if (name->len > 3 && (*name->str == 'K') && (*(name->str + 1) == 'P') && (*(name->str + 2) == '_'))
        {
          if((*(name->str + 3) !='7') && (*(name->str + 3) !='8') && (*(name->str + 3) !='9'))
            g_string_erase (name, 0, 3);    //force numeric keypad KP_ names to normal except for 7 8 9 which are not needed for duration entry
        }
      //g_print("label %s\nname %s\n", gtk_accelerator_get_label(accelerator_key, 0), gdk_keyval_name(accelerator_key));
      // g_print("mods were %x\n", accelerator_mods);
#if 0
      //do not let caps lock affect shift of backspace etc
      if ((accelerator_key == GDK_BackSpace) || (accelerator_key == GDK_Left) || (accelerator_key == GDK_Right) || (accelerator_key == GDK_Up) || (accelerator_key == GDK_Down) || (accelerator_key == GDK_Page_Up) || (accelerator_key == GDK_Page_Down) || (accelerator_key == GDK_Home) || (accelerator_key == GDK_End) || (accelerator_key == GDK_Insert) || (accelerator_key == GDK_Delete) || (accelerator_key == GDK_KP_Decimal) || (accelerator_key == GDK_period))
        accelerator_mods &= ~GDK_LOCK_MASK;
#else

      if (!((name->len == 1) && (*name->str >= 'a') && (*name->str <= 'z')))
        if (!((name->len == 1) && (*name->str >= '0') && (*name->str <= '9')))
          accelerator_mods &= ~GDK_LOCK_MASK;
#endif
      // g_print("mods %x\n", accelerator_mods);
      // if (accelerator_mods&GDK_SHIFT_MASK)

      //    if((name->len==1) && (*name->str>='A') && (*name->str<='Z'))
      //      *name->str += ('a'-'A');
      if (!((name->len == 1) && (*name->str >= 'a') && (*name->str <= 'z')))
        if (((accelerator_mods & GDK_LOCK_MASK) != 0) != ((accelerator_mods & GDK_SHIFT_MASK) != 0))
          g_string_prepend (name, "Shft+");

      if (((accelerator_mods & GDK_LOCK_MASK) != 0) != ((accelerator_mods & GDK_SHIFT_MASK) != 0))
        {
          if ((name->len == 1) && (*name->str >= 'a') && (*name->str <= 'z'))
            *name->str -= ('a' - 'A');
        }

      if ((accelerator_mods & GDK_CONTROL_MASK))
        g_string_prepend (name, "Ctrl+");

      if ((accelerator_mods & GDK_MOD1_MASK))
        g_string_prepend (name, "Alt+");
      if ((accelerator_mods & GDK_HYPER_MASK))
        g_string_prepend (name, "Hypr+");
      if ((accelerator_mods & GDK_MOD4_MASK))
        g_string_prepend (name, "Mod4+");
      if ((accelerator_mods & GDK_MOD5_MASK))
        g_string_prepend (name, "Mod5+");
      return g_string_free (name, FALSE);
    }


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
 * Warns user that there was no keymap available to load  
 *
 */
/* UNUSED
static void
no_map_dialog ()
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL, (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_MESSAGE_WARNING, GTK_BUTTONS_CLOSE, _("Keyboard shortcuts could not be found"));


  gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), _("No commands file was found in either" " the systemwide Denemo directory" " or in .denemo directory within your " "home directory. This is an installation error. You can use" " Edit/Command Management to construct a custom " "interface or to load one from" " a commandset file."));

  gtk_widget_show_all (dialog);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}
*/
/*
 * Allocates a keymap.
 * action_group_name is the name of the group of actions for the commands
 * of the keymap.
 */
keymap *
allocate_keymap (void)
{
  keymap *the_keymap = (keymap *) g_malloc (sizeof (keymap));
  the_keymap->commands = gtk_list_store_new (N_COLUMNS,
                                             G_TYPE_INT,                //type
                                             G_TYPE_POINTER,            //action
                                             G_TYPE_POINTER,            //name
                                             G_TYPE_POINTER,            //label
                                             G_TYPE_POINTER,            //tooltip
                                             G_TYPE_POINTER,            //callback
                                             GTK_TYPE_LIST_STORE,       //bindings
                                             G_TYPE_BOOLEAN,            //hidden
                                             G_TYPE_BOOLEAN,            //deleted
                                             G_TYPE_INT,                //type
                                             G_TYPE_POINTER
    );

  //empty index reference
  the_keymap->idx_from_name = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);
  the_keymap->idx_from_keystring = g_hash_table_new_full (g_str_hash, g_str_equal, g_free, g_free);

  the_keymap->continuations_table = g_hash_table_new (g_str_hash, g_str_equal);

  the_keymap->cursors = g_hash_table_new (g_int_hash, g_int_equal);
  //  g_print("Created hash table %p\n", the_keymap->cursors);
  return the_keymap;
}


void
free_keymap (keymap * the_keymap)
{
  g_object_unref (the_keymap->commands);
  g_hash_table_destroy (the_keymap->idx_from_name);
  g_hash_table_destroy (the_keymap->idx_from_keystring);
  g_hash_table_destroy (the_keymap->continuations_table);
}


void register_command_row(keymap* the_keymap, command_row* command){
  guint *value;
  GtkTreeIter iter = { 0, NULL, NULL, NULL };

  //get the index of the new row
  value = (guint *) g_malloc (sizeof (guint));
  *value = gtk_tree_model_iter_n_children (GTK_TREE_MODEL (the_keymap->commands), NULL);

  //add a new row
  gtk_list_store_append (the_keymap->commands, &iter);

#if DEBUG
  //This code is only relevant to developers, to check that no action
  //entry masks another. Users cannot add actions. THIS IS CHANGING NOW...
  gint idx = lookup_command_from_name (the_keymap, command->name);
  if (idx != -1)
    {
      g_warning ("Command %s is inserted more than once, aborting...\n", command->name);
      // exit(2);FIXME dirty
    }
#endif
  //insert the information in the list store
  gtk_list_store_set (the_keymap->commands, &iter,
                      COL_TYPE, command->type,
                      COL_ACTION, command->action,
                      COL_NAME, command->name,
                      COL_LABEL, command->label,
                      COL_TOOLTIP, command->tooltip,
                      COL_CALLBACK, command->callback,
                      COL_BINDINGS, command->bindings,
                      COL_SCRIPTTYPE, command->script_type,
                      COL_LOCATIONS, command->locations,
                      -1);
  //insert the command name in the index reference
  g_hash_table_insert (the_keymap->idx_from_name, g_strdup (command->name), value);

  //drop the reference to the bindings list store, so that it is freed
  //with the command list store
  g_object_unref (command->bindings);

#if DEBUG
  g_print ("Inserting command %s %s %s %p  -> %d\n", command->name, command->label, command->tooltip, command->callback, *value);
#endif
}

/* Used for compatibility with register_command.h */
void
register_command (keymap * the_keymap, GtkAction * action, gchar * name, gchar * label, gchar * tooltip, gpointer callback)
{
  command_row command;
  command_row_init(&command);
  command.name = name;
  command.label = label;
  command.tooltip = tooltip;
  command.callback = callback;
  command.action = action;
  register_command_row(the_keymap, &command);
}


static gint
command_iter_sort (GtkTreeModel * model, GtkTreeIter * a, GtkTreeIter * b, G_GNUC_UNUSED gpointer user_data)
{
  GtkTreeIter *iters[2];
 // KeymapCommandType type;
  gpointer action;
  const gchar *names[2];
  gint i;
  iters[0] = a;
  iters[1] = b;
  for (i = 0; i < 2; i++)
    {
     // gtk_tree_model_get (model, iters[i], /*COL_TYPE, &type,*/ COL_ACTION, &action, -1);

     // names[i] = gtk_action_get_name (action);

	gtk_tree_model_get (model, iters[i], COL_LABEL, names+i, -1);
    }
  return strcmp (names[0], names[1]);
}

void
alphabeticalize_commands (keymap * the_keymap)
{
  // return;
  g_print ("alphabeticalizing the commands");
  gint i, n;
  guint *value;
  const gchar *command_name;
  GtkTreeModel *model = GTK_TREE_MODEL (the_keymap->commands);
  GtkTreeSortable *sortable = GTK_TREE_SORTABLE (the_keymap->commands);
  n = gtk_tree_model_iter_n_children (model, NULL);
  gtk_tree_sortable_set_sort_func (sortable, 0, command_iter_sort, NULL, NULL);
  gtk_tree_sortable_set_sort_column_id (sortable, 0, GTK_SORT_ASCENDING);
  for (i = 0; i < n; i++)
    {
      command_name = lookup_name_from_idx (the_keymap, i);
      value = (guint *) g_hash_table_lookup (the_keymap->idx_from_name, command_name);
      if (value)
        *value = i;
      else
        g_warning ("Error in keymap read");
    }
}

//False if command_idx is an invalid index or keymap is null, true otherwise
//TODO keymap should not be NULL
gboolean
keymap_get_command_row (keymap * the_keymap, command_row * row, guint command_idx)
{
  if (!the_keymap)
    {
      warningdialog (_("This should not happen..."));
      return FALSE;
    }
  GtkTreeModel *model = GTK_TREE_MODEL (the_keymap->commands);
  GtkTreeIter iter;
  if (!gtk_tree_model_iter_nth_child (model, &iter, NULL, command_idx))
    return FALSE;
  gtk_tree_model_get (model, &iter,
                      COL_TYPE, &row->type,
                      COL_ACTION, &row->action,
                      COL_NAME, &row->name,
                      COL_LABEL, &row->label,
                      COL_CALLBACK, &row->callback,
                      COL_TOOLTIP, &row->tooltip,
                      COL_BINDINGS, &row->bindings,
                      COL_HIDDEN, &row->hidden,
                      COL_DELETED, &row->deleted,
                      COL_SCRIPTTYPE, &row->script_type,
                      COL_LOCATIONS, &row->locations,
                      -1);
  return TRUE;
}



static gboolean
keymap_clear_bindings_in_row (GtkTreeModel * model, G_GNUC_UNUSED GtkTreePath * path, GtkTreeIter * iter, G_GNUC_UNUSED gpointer data)
{
  GtkListStore *bindings;
  gtk_tree_model_get (model, iter, COL_BINDINGS, &bindings, -1);
  gtk_list_store_clear (bindings);
  g_object_unref (bindings);
  return FALSE;
}


static void
catname (gchar * name, GString * str, gchar * separator)
{
  if (str)
    g_string_append_printf (str, "%s%s", name, separator);
}

#if 0
static void
newlinename (gchar * name, GString * str)
{
  catname (name, str, "\n");
}
#endif

static void
listname (gchar * name, GString * str)
{
  catname (name, str, " ");
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
  gtk_tree_model_foreach (GTK_TREE_MODEL (the_keymap->commands), keymap_clear_bindings_in_row, NULL);
  g_hash_table_remove_all (the_keymap->idx_from_keystring);
}

/*
 * Returns the number of commands in the keymap
 */
guint
keymap_size (keymap * the_keymap)
{
  return gtk_tree_model_iter_n_children (GTK_TREE_MODEL (the_keymap->commands), NULL);
}

/* returns TRUE if command has at least one binding */
gboolean
command_has_binding (guint command_idx)
{
  command_row row;
  GtkTreeIter iter;
  if (keymap_get_command_row (Denemo.map, &row, command_idx))
    {
      GtkTreeModel *model_bind = GTK_TREE_MODEL (row.bindings);
      if (!gtk_tree_model_get_iter_first (model_bind, &iter))
        return FALSE;
      else
        return TRUE;
    }
  return FALSE;
}

/*
 * executes function fun on all bindings attached to a command. The arguments
 * passed to the function are the value of the current binding and the
 * additionnal user data
 */
void
keymap_foreach_command_binding (keymap * the_keymap, guint command_idx, GFunc func, gpointer user_data)
{
  command_row row;
  gchar *binding;
  GtkTreeIter iter;
  GtkTreeModel *model_bind;
  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return;
  //get the first list element, if the list is empty returns
  model_bind = GTK_TREE_MODEL (row.bindings);
  if (!gtk_tree_model_get_iter_first (model_bind, &iter))
    return;
  //walk through the list and execute func on the binding
  do
    {
      //retrieve the binding
      gtk_tree_model_get (model_bind, &iter, 0, &binding, -1);
      //execute func
      func (binding, user_data);
      //free the binding
      g_free (binding);
    }
  while (gtk_tree_model_iter_next (model_bind, &iter));
  //unref bindings
  g_object_unref (row.bindings);
}

/**
 *  Search through keybindings for a specific binding
 *  return the command idx for the command that is bound to the keyval,state pair, or -1 if none
 */
gint
lookup_command_for_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state)
{
  gint res;
  gchar *name = dnm_accelerator_name (keyval, state);
  res = lookup_command_for_keybinding_name (the_keymap, name);
  g_free (name);
  return res;
}

/* weaker lookup of keybinding */
gint
lookup_command_for_keyevent (GdkEventKey * event)
{
  keymap *the_keymap = Denemo.map;
  gint command_idx = lookup_command_for_keybinding (the_keymap, event->keyval,
                                                    dnm_sanitize_key_state (event));
#if 0
  if (!Denemo.prefs.strictshortcuts)
    {
      //    lookup_command_for_keybinding (the_keymap, event->keyval,
      //                             dnm_sanitize_key_state(event));
      if (command_idx == -1)
        command_idx = lookup_command_for_keybinding (the_keymap, event->keyval, dnm_hyper_sanitize_key_state (event));
      if (command_idx == -1)
        command_idx = lookup_command_for_keybinding (the_keymap, event->keyval, dnm_meta_sanitize_key_state (event));
    }
#endif
  return command_idx;
}


/* looks up the command idx for the binding of name binding_name */
gint
lookup_command_for_keybinding_name (keymap * the_keymap, const gchar * binding_name)
{
  gpointer *value = g_hash_table_lookup (the_keymap->idx_from_keystring,
                                         binding_name);
  if (value)
    return *((guint *) value);
  else
    return -1;
}



/**
 * Look up for a command index.
 *
 * @param keymap
 * @param name
 */
gint
lookup_command_from_name (keymap * keymap, const gchar * command_name)
{
  gpointer value = g_hash_table_lookup (keymap->idx_from_name, command_name);
  if (value)
    return *(guint *) value;
  else
    return -1;
}

//do not free the result
//returns NULL if not found
const GtkAction *
lookup_action_from_idx (keymap * keymap, gint command_idx)
{
	if (command_idx == -1)
		return NULL;	
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return NULL;
  g_object_unref (row.bindings);
  return row.action;
}

//do not free the result
//returns NULL if not found
gpointer
lookup_callback_from_idx (keymap * keymap, gint command_idx)
{
	if (command_idx == -1)
		return NULL;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return NULL;
  g_object_unref (row.bindings);
  return row.callback;
}

//do not free the result
//returns NULL if not found
const gchar *
lookup_name_from_idx (keymap * keymap, gint command_idx)
{
  const gchar *res = NULL;
	if (command_idx == -1)
		return NULL;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return NULL;
  res = gtk_action_get_name (row.action);
  g_object_unref (row.bindings);
  return res;
}

//do not free the result
//returns NULL if not found
const gchar *
lookup_tooltip_from_idx (keymap * keymap, gint command_idx)
{
  const gchar *res = NULL;
  if (command_idx == -1)
		return NULL;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return NULL;
  res = row.tooltip;            //FIXME label is a property g_object_get_prop...

  g_object_unref (row.bindings);
  return res;
}

gboolean
lookup_hidden_from_idx (keymap * keymap, guint command_idx)
{
  gboolean res = FALSE;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return FALSE;
  res = row.hidden;             //FIXME label is a property g_object_get_prop...
  g_object_unref (row.bindings);
  return res;
}


gboolean
lookup_deleted_from_idx (keymap * keymap, guint command_idx)
{
  gboolean res = FALSE;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return 0;
  res = row.deleted;            //FIXME label is a property g_object_get_prop...
  g_object_unref (row.bindings);
  return res;
}


//do not free the result
//returns NULL if not found
const gchar *
lookup_label_from_idx (keymap * keymap, gint command_idx)
{
  const gchar *res = NULL;	
  if (command_idx == -1)
		return NULL;
  command_row row;
  if (!keymap_get_command_row (keymap, &row, command_idx))
    return NULL;
  res = row.label;              //FIXME label is a property g_object_get_prop...

  g_object_unref (row.bindings);
  return res;
}
const gchar *
lookup_menu_path_from_idx (keymap * keymap, gint command_idx)
{
	gchar *menupath = NULL;
	if (command_idx == -1)
		return NULL;
  GtkAction *action = (GtkAction *) lookup_action_from_idx (keymap, command_idx);
	if (action)
		menupath = g_object_get_data (G_OBJECT (action), "menupath");
	return menupath;
}
//returns the accel, "" if no accel defined. free the result
//the accel is the first keybinding of the list
#if 0
static gchar *
keymap_get_accel (keymap * the_keymap, guint command_idx)
{
  command_row row;
  GtkTreeModel *model_bind;
  GtkTreeIter iter;
  gchar *res;

  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return g_strdup ("");
  model_bind = GTK_TREE_MODEL (row.bindings);
  if (!gtk_tree_model_get_iter_first (model_bind, &iter))
    {
      g_object_unref (row.bindings);
      return g_strdup ("");
    }
  gtk_tree_model_get (model_bind, &iter, 0, &res, -1);
  g_object_unref (row.bindings);
  return res;
}
#endif

#if 0
static gint
findActionGroupByName (gconstpointer a, gconstpointer b)
{
  GtkActionGroup *action_group = GTK_ACTION_GROUP (a);
  const gchar *searched_name = (const gchar *) b;
  return strcmp (gtk_action_group_get_name (action_group), b);
}
#endif



/* Updates the label of the widgets proxying an action with the bindings
 * present in the keymap.
 */
void
update_accel_labels (keymap * the_keymap, guint command_idx)
{
  GtkAction *action;

  //Getting the accel
  const gchar *command_name = lookup_name_from_idx (the_keymap, command_idx);
  GString *str = g_string_new ("");
  //TODO don't use all the bindings as accels
  keymap_foreach_command_binding (the_keymap, command_idx, (GFunc) listname, str);
  //Prepare the new label
  const gchar *base;
  //FIXME use translate_dnm_to_gtk
  base = lookup_label_from_idx (the_keymap, command_idx);
#if 0
  //FIXME here generate a locale dependent name using gtk_accelerator_get_label after back-tracking to find the keyval from the binding (stripping off the prefixes we have added etc). This will be needed for language translation (i.e. _N() should be applied to the gdk_keyval_name() but the label we are writing here should be better with a translated indication of the keybinding (Left becomes Links in German etc).
  // we have to store an invariant gdk name, so we should look it up here to get the keyval  and from that derive a locale specific name to use on the label. In any case the following transformation is redundant

  gchar *c;
  for (c = str->str; *c; c++)
    {
      if (*c == '<')
        *c = ' ';
      if (*c == '>')
        *c = '-';
    }
#endif
  gchar *escape_base = g_markup_escape_text(base, -1);
  gchar *markup;
  if(str->len)
	markup = g_strdup_printf ("%s <span style=\"italic\" stretch=\"condensed\" weight=\"bold\" foreground=\"blue\">%s</span>", escape_base, str->str);
  else
	markup = g_strdup (escape_base);
  g_free (escape_base);

  action = gtk_action_group_get_action (Denemo.action_group, command_name);
  //For all widgets proxying the action, change the label
  GSList *h = gtk_action_get_proxies (action);
  for (; h; h = h->next)
    {
      GtkWidget *widget = h->data;
      GtkWidget *child = (GtkWidget *) gtk_bin_get_child (GTK_BIN (widget));
      if (GTK_IS_BUTTON (child))
        {
          child = gtk_bin_get_child (GTK_BIN (child));
        }
      //FIXME others?? toolitem ...
      if (GTK_IS_LABEL (child))
        {
          gtk_label_set_markup (GTK_LABEL (child), markup);
        }
    }

  //free allocated strings                                 
  g_free (markup);
  g_string_free (str, TRUE);
}

void
update_all_labels (keymap * the_keymap)
{
  gint command_idx, num = keymap_size (the_keymap);
  for (command_idx = 0; command_idx < num; command_idx++)
    update_accel_labels (the_keymap, command_idx);
}

//if binding is a two-key binding, update a table of such bindings, adding is add is true else removing
static void
update_continuations_table (keymap * the_keymap, const gchar * binding, gboolean add)
{
  gchar *second = g_strrstr (binding, DENEMO_TWO_KEY_SEPARATOR);
  if (second)
    {
      gchar *shortcut = g_strdup (binding);
      *(shortcut + (second - binding)) = 0;     // split into two strings at the separator
      gchar *value = shortcut + (second - binding) + 1;
      //g_print("Two key shortcuts %s %s\n", shortcut, value);
      if (add)
        {
          GList *thelist = g_hash_table_lookup (the_keymap->continuations_table, shortcut);
          thelist = g_list_append (thelist, value);
          g_hash_table_insert (the_keymap->continuations_table, shortcut, thelist);
        }
      else
        {
          GList *thelist = g_hash_table_lookup (the_keymap->continuations_table, shortcut);
          if (thelist == NULL)
            g_warning ("Missing shortcut in table");
          else
            {
              GList *g;
              for (g = thelist; g; g = g->next)
                {
                  if (!strcmp (value, (gchar *) g->data))
                    {
                      thelist = g_list_delete_link (thelist, g);
                      g_hash_table_insert (the_keymap->continuations_table, shortcut, thelist);
                      //unlikely you can get the memory back... g_free(shortcut);
                    }
                }
            }
        }
    }
}

static void
remove_keybinding_bindings_helper (keymap * the_keymap, guint command_idx, const gchar * binding)
{
  gboolean found = FALSE;
  gchar *cur_binding;
  command_row row;
  GtkTreeIter iter;
  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return;
  GtkTreeModel *model_bind = GTK_TREE_MODEL (row.bindings);
  if (!gtk_tree_model_get_iter_first (model_bind, &iter))
    {
      g_object_unref (row.bindings);
      return;
    }
  do
    {
      gtk_tree_model_get (model_bind, &iter, 0, &cur_binding, -1);
      if (!strcmp (binding, cur_binding))
        {
          found = TRUE;
          break;
        }
      g_free (cur_binding);
    }
  while (gtk_tree_model_iter_next (model_bind, &iter));

  if (found)
    {
      gtk_list_store_remove (row.bindings, &iter);
      update_continuations_table (the_keymap, binding, FALSE);
    }

  g_object_unref (row.bindings);
}

void
remove_keybinding (keymap * the_keymap, gint keyval, GdkModifierType state)
{
  gchar *name = dnm_accelerator_name (keyval, state);
  remove_keybinding_from_name (the_keymap, name);
  g_free (name);
}

void
remove_keybinding_from_name (keymap * the_keymap, const gchar * binding)
{
  gint *value;
  value = (gint *) g_hash_table_lookup (the_keymap->idx_from_keystring, binding);
  if (value)
    {
      remove_keybinding_bindings_helper (the_keymap, *value, binding);
      update_accel_labels (the_keymap, *value);
      g_hash_table_remove (the_keymap->idx_from_keystring, binding);
    }
}

/*
 * Insert a binding to the bindings of command_idx.
 * pos indicates where in the list of bindings to insert this binding.
 */
static void
add_keybinding_bindings_helper (keymap * the_keymap, guint command_idx, const gchar * binding, ListPosition pos)
{
  command_row row;
  GtkTreeIter iter;

  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return;

  if (pos == POS_FIRST)
    gtk_list_store_prepend (row.bindings, &iter);
  else if (pos == POS_LAST)
    gtk_list_store_append (row.bindings, &iter);
  else
    {
      g_object_unref (row.bindings);
      return;
    }

  gtk_list_store_set (row.bindings, &iter, 0, binding, -1);

  g_object_unref (row.bindings);

  update_continuations_table (the_keymap, binding, TRUE);
}

gint
add_keybinding_to_named_command (keymap * the_keymap, gint keyval, GdkModifierType state, const gchar * command_name, ListPosition pos)
{
  gpointer value;
  guint command_idx;
  value = g_hash_table_lookup (the_keymap->idx_from_name, command_name);
  if (!value)
    {
      g_warning ("add_keybinding: %s, command does not exist", command_name);
      return -1;
    }
  command_idx = *(guint *) value;
  return add_keybinding_to_idx (the_keymap, keyval, state, command_idx, pos);
}

gint
add_named_binding_to_idx (keymap * the_keymap, gchar * kb_name, guint command_idx, ListPosition pos)
{
  guint *new_idx;
  gint old_command_idx;
  old_command_idx = lookup_command_for_keybinding_name (the_keymap, kb_name);   //lookup_keybinding(the_keymap, keyval, state);  
  gchar *title = NULL;
  gchar *prompt = NULL;
  if (old_command_idx >= 0)
    {
      if ((!Denemo.prefs.return_key_is_special) || strcmp (kb_name, "Return"))
        {
          title = g_strdup_printf (_("The Command %s Responds to the Shortcut %s"), lookup_name_from_idx (Denemo.map, old_command_idx), kb_name);
          prompt = g_strdup_printf (_("Lose the shortcut %s for this?"), kb_name);
        }
    }

  if (title && (pos == POS_FIRST) && (old_command_idx >= 0) && (!confirm (title, prompt)))
    {
      g_free (title);
      g_free (prompt);
      return old_command_idx;
    }

  if (old_command_idx >= 0)
    {
      remove_keybinding_bindings_helper (the_keymap, old_command_idx, kb_name);
      update_accel_labels (the_keymap, old_command_idx);
    }
  //add the keybinding to the binding on idx_command
  add_keybinding_bindings_helper (the_keymap, command_idx, kb_name, pos);

  //update the accel labels of the command
  update_accel_labels (the_keymap, command_idx);

  //add or modify an entry in idx_from_keystring
  new_idx = (guint *) g_malloc (sizeof (guint));
  *new_idx = command_idx;
  g_hash_table_insert (the_keymap->idx_from_keystring, g_strdup (kb_name), new_idx);
  g_free (title);
  g_free (prompt);
  return old_command_idx;
}


/**
 * Adds a keybinding to the_keymap.  If the key was already bound,
 * this function removes the old binding and replaces it, returning
 * the number of the command this keybinding was attached to. Otherwise
 * returns -1. 
 * if pos is POS_FIRST, then the user is adding the binding - get confirmation before stealing
 * if POS_LAST it is the command set being loaded, do not ask
 */
gint
add_keybinding_to_idx (keymap * the_keymap, gint keyval, GdkModifierType state, guint command_idx, ListPosition pos)
{
  gint old_command_idx;
  gchar *kb_name;
  kb_name = dnm_accelerator_name (keyval, state);
  old_command_idx = add_named_binding_to_idx (the_keymap, kb_name, command_idx, pos);
  g_free (kb_name);
  //Denemo.accelerator_status = TRUE;
  return old_command_idx;
}

gint
add_twokeybinding_to_idx (keymap * the_keymap, gint first_keyval, GdkModifierType first_state, gint keyval, GdkModifierType state, guint command_idx, ListPosition pos)
{
  gint old_command_idx;
  gchar *kb_name;
  kb_name = g_strdup_printf ("%s" DENEMO_TWO_KEY_SEPARATOR "%s", dnm_accelerator_name (first_keyval, first_state), dnm_accelerator_name (keyval, state));
  old_command_idx = add_named_binding_to_idx (the_keymap, kb_name, command_idx, pos);
  g_free (kb_name);
  //Denemo.accelerator_status = TRUE;
  return old_command_idx;
}

/* force keybinding on action of name, returning old command id */
gint
add_keybinding_for_name (gchar * name, gchar * binding)
{
  guint idx = lookup_command_from_name (Denemo.map, name);
  if (idx != -1)
    {
      return add_named_binding_to_idx (Denemo.map, binding, idx, POS_LAST);
    }
  return -1;
}

/* force keybinding on action of id, returning old command id */
gint
add_keybinding_for_command (gint idx, gchar * binding)
{

  if (idx != -1)
    {
      return add_named_binding_to_idx (Denemo.map, binding, idx, POS_LAST);
    }
  return -1;
}

#if 0
//we have to reproduce this function here since it is static in gtkmenu.c
static void
stolen_gtk_menu_stop_navigating_submenu (GtkMenu * menu)
{
  if (menu->navigation_region)
    {
      cairo_region_destroy (menu->navigation_region);
      menu->navigation_region = NULL;
    }
  if (menu->navigation_timeout)
    {
      g_source_remove (menu->navigation_timeout);
      menu->navigation_timeout = 0;
    }
}
#endif

gint
keymap_accel_quick_edit_snooper (GtkWidget * grab_widget, GdkEventKey * event)
{
  guint keyval;
  GdkModifierType modifiers;
  GtkAction *action = NULL;
  keymap *the_keymap = Denemo.map;
  GtkMenu *menu = GTK_MENU (grab_widget);

  if (Denemo.prefs.menunavigation && ((event->keyval == 0xFF1B) || (event->keyval == 0xFF51) || (event->keyval == 0xFF52) || (event->keyval == 0xFF53) || (event->keyval == 0xFF54)))
    {
//Esc and arrows for navigating menus
      return FALSE;
    }


#if GTK_MAJOR_VERSION == 3
//JEREMIAH PLEASE TEST!!        
  if (GTK_IS_ACTIVATABLE (gtk_menu_shell_get_selected_item (GTK_MENU_SHELL (menu))))
    action = gtk_activatable_get_related_action ((GtkActivatable*) gtk_menu_shell_get_selected_item (GTK_MENU_SHELL (menu)));
#else
  if (GTK_MENU_SHELL (menu)->active_menu_item)
    action = gtk_widget_get_action (GTK_MENU_SHELL (menu)->active_menu_item);   //note this is not gtk_menu_get_active(menu) except after a selection has been made, we want the menu item that the pointer has moved to before it is selected.
#endif


  //If this menu item has no action we give up
  if (!action)
    return FALSE;

  //If the KeyEvent is only a modifier, stop processing here
  if (isModifier (event))
    return TRUE;
  dnm_clean_event (event);
  modifiers = dnm_sanitize_key_state (event);
  keyval = event->keyval;

  gint idx = lookup_command_from_name (the_keymap, gtk_action_get_name (action));
  //If this menu item  action is not registered in the
  //keymap, we give up
  if (idx == -1)
    return TRUE;
  //Add the keybinding
  add_keybinding_to_idx (the_keymap, keyval, modifiers, idx, POS_FIRST);
  return TRUE;
}

gboolean
idx_has_callback (keymap * the_keymap, guint command_idx)
{
  if (command_idx == -1)
    return FALSE;
  gboolean res = TRUE;
  command_row row;
  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return FALSE;
  g_object_unref (row.bindings);
  res = (gboolean) (intptr_t) row.callback;
  return res;
}



GtkAction *
lookup_action_from_name (gchar * command_name)
{
  // GtkActionGroup *action_group = get_action_group(the_keymap);
  return gtk_action_group_get_action (Denemo.action_group, command_name);
}

gboolean
execute_callback_from_idx (keymap * the_keymap, guint command_idx)
{
  const gchar *command_name;
  command_name = lookup_name_from_idx (the_keymap, command_idx);
  return execute_callback_from_name (command_name);
}

gboolean
execute_callback_from_name (const gchar * command_name)
{
  gchar *text = NULL;
  GtkAction *action = action_of_name (the_keymap, (gchar *) command_name);
  text = (gchar *) g_object_get_data (G_OBJECT (action), "scheme");
  if (text && *text)
    call_out_to_guile (text);
  else
    gtk_action_activate (action);
  return TRUE;
}

//prints info on the data of the keymap relative to a command
void
dump_command_info (keymap * the_keymap, gint command_idx)
{
  gchar *cur_binding;
  command_row row;
  GtkTreeIter iter;
  GtkTreeModel *model_bind;

  if (command_idx == -1)
    {
      g_print ("no command\n");
      return;
    }
  g_print ("command %s (%d)\nKeyboard Shortcuts:\n", lookup_name_from_idx (the_keymap, command_idx), command_idx);
  if (!keymap_get_command_row (the_keymap, &row, command_idx))
    return;
  model_bind = GTK_TREE_MODEL (row.bindings);
  if (!gtk_tree_model_get_iter_first (model_bind, &iter))
    {
      g_object_unref (row.bindings);
      return;
    }
  do
    {
      gtk_tree_model_get (model_bind, &iter, 0, &cur_binding, -1);
      g_print ("\t%s (%d)\n", cur_binding, lookup_command_for_keybinding_name (the_keymap, cur_binding));
      g_free (cur_binding);
    }
  while (gtk_tree_model_iter_next (model_bind, &iter));
  g_object_unref (row.bindings);
}




/**
 * This checks to see if there's a .denemo/keymaps directory in the user's
 * home directory, tries to create one if there isn't, and returns the
 * path to it
 *
 */

const gchar *
get_user_keymap_dir ()
{
  static gchar *keymapdir = NULL;

  gboolean err;
  if (!keymapdir)
    {
      keymapdir = g_build_filename (get_user_data_dir (), COMMANDS_DIR, NULL);
    }
  err = g_mkdir_with_parents (keymapdir, 0770);
  if (err)
    {
      warningdialog (_("Could not create .denemo/actions for your customized commands"));
      g_free (keymapdir);
      keymapdir = NULL;
    }

  return keymapdir;
}







/**
 *  loads a command set (aka keymap) from a file in a file selector.
 * This function is a callback that is wrapper for
 * load_keymap_file amongst others
 *FIXME note that non xml file support has been commented out
 */
void
load_keymap_from_dialog (gchar * filename)
{
  gchar* files[] = { g_strdup(filename), NULL };
  if (g_file_test (filename, G_FILE_TEST_EXISTS))
    load_keymap_files (files);
  g_free (filename);
  Denemo.accelerator_status = TRUE;
}

#if 0
static void
show_type (GtkWidget * widget, gchar * message)
{
  g_print ("%s%s\n", message, widget ? g_type_name (G_TYPE_FROM_INSTANCE (widget)) : "NULL widget");
}
#endif

/**
 * Function for loading a command set (aka keymap) from location by way of
 * a user dialog. 
 */
void
load_keymap_dialog_location (gchar * location)
{
  gchar *filename = file_dialog ("Load Command Set", TRUE, location);
  if (filename)
    load_keymap_from_dialog (filename);
}

void
load_keymap_dialog ()
{
  gchar *keymapdir = g_strdup_printf ("%s%c", get_user_keymap_dir (), G_DIR_SEPARATOR);
  if (keymapdir)
    load_keymap_dialog_location (keymapdir);
  else
    warningdialog (_("Cannot access your local .denemo"));
  g_free (keymapdir);
}

void
load_system_keymap_dialog (void)
{
  gchar *systemwide = g_build_filename (get_system_data_dir (), COMMANDS_DIR, USER_KEYMAP, KEYMAP_EXT,
                                        NULL);
  if (systemwide)
    load_keymap_dialog_location (systemwide);
  else
    warningdialog (_("Installation error"));
  g_free (systemwide);
}

void
load_default_keymap_file ()
{
  gchar* user_keymap_file = g_strconcat (USER_KEYMAP, KEYMAP_EXT, NULL);
  gchar* default_keymap_file = g_strconcat (DEFAULT_KEYMAP, KEYMAP_EXT, NULL);

  gchar* files[] = {
    g_build_filename (g_get_current_dir (), COMMANDS_DIR, user_keymap_file, NULL),
    g_build_filename (get_user_keymap_dir (), user_keymap_file, NULL),
    g_build_filename (get_system_data_dir (), COMMANDS_DIR, user_keymap_file, NULL),
    g_build_filename (get_user_keymap_dir (), default_keymap_file, NULL),
    g_build_filename (get_system_data_dir (), COMMANDS_DIR, default_keymap_file, NULL),
    NULL
  };

  if(!load_keymap_files (files))
    g_warning ("Unable to load default keymap");
  
  g_free(default_keymap_file);
  g_free (user_keymap_file);
}

/* UNUSED
static GScannerConfig scanner_config_template = {
  (" \t\r\n") // cset_skip_characters
  ,(G_CSET_a_2_z "_0123456789/." G_CSET_A_2_Z) // cset_identifier_first
  ,(G_CSET_a_2_z "_0123456789/." G_CSET_A_2_Z G_CSET_LATINS G_CSET_LATINC) // cset_identifier_nth
  ,("#\n") // cpair_comment_single

  ,FALSE // case_sensitive

  ,TRUE // skip_comment_multi
  ,TRUE // skip_comment_single
  ,TRUE // scan_comment_multi
  ,TRUE // scan_identifier
  ,TRUE // scan_identifier_1char
  ,FALSE //scan_identifier_NULL
  ,TRUE // scan_symbols
  ,FALSE // scan_binary
  ,FALSE // scan_octal
  ,FALSE // scan_float
  ,FALSE // scan_hex
  ,FALSE // scan_hex_dollar
  ,TRUE // scan_string_sq
  ,TRUE // scan_string_dq
  ,FALSE // numbers_2_int
  ,FALSE // int_2_float
  ,TRUE // identifier_2_string
  ,TRUE // char_2_token
  ,TRUE // symbol_2_token
  ,FALSE                         // scope_0_fallback
};
*/
/**
 * Callback for saving the keymap to a given file
 *
 */
void
save_keymap_from_dialog (gchar * filename)
{

  gchar *extendedname = substitute_extension (filename, "commands");
  save_xml_keymap (extendedname);       //no longer save keybindings here

  gchar *fname = substitute_extension (extendedname, "shortcuts");
  save_xml_keybindings (fname);
  g_free (fname);
  g_free (extendedname);
  g_free (filename);
}

/**
 * Function for saving a keymap to an arbitrary place by way of
 * a user dialog.  Similar to file_saveas. 
 */
void
save_keymap_dialog (void)
{
  gchar *keymapdir = NULL;
  keymapdir = g_build_filename (get_user_keymap_dir (), NULL);
  gchar *filename = file_dialog (_("Save Command Set"), FALSE, keymapdir);
  if (filename)
    save_keymap_from_dialog (filename);
  g_free (keymapdir);
}

/**
 * Wrapper function for saving the keymap to the standard place
 *
 */
void
save_default_keymap_file_wrapper (GtkAction * action, gpointer param)
{
  save_default_keymap_file ();
}

/**
 * Saves the keymap as the user's default keymap
 *
 */
void
save_default_keymap_file (void)
{
  gchar *localrc = NULL;
  const gchar *keymapdir = get_user_keymap_dir ();
  if (keymapdir)
    {
	  gchar* default_keymap_file = g_strconcat (DEFAULT_KEYMAP, KEYMAP_EXT, NULL);
      localrc = g_build_filename (keymapdir, default_keymap_file, NULL); 
      g_free(default_keymap_file);
      save_xml_keymap (localrc);        //no longer saves keybindings
      g_free (localrc);
      localrc = g_build_filename (keymapdir, DEFAULT_KEYBINDINGS, NULL);
      save_xml_keybindings (localrc);
      g_free (localrc);
    }

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
command_name_data_function (G_GNUC_UNUSED GtkTreeViewColumn * col, GtkCellRenderer * renderer, GtkTreeModel * model, GtkTreeIter * iter, G_GNUC_UNUSED gpointer user_data)
{
  KeymapCommandType type;
  gpointer action;
  const gchar *name;
  gtk_tree_model_get (model, iter, COL_TYPE, &type, COL_ACTION, &action, -1);
  name = gtk_action_get_name (action);

  g_object_set (renderer, "text", name, NULL);
}
static void
label_data_function (G_GNUC_UNUSED GtkTreeViewColumn * col, GtkCellRenderer * renderer, GtkTreeModel * model, GtkTreeIter * iter, G_GNUC_UNUSED gpointer user_data)
{
  const gchar *name;
  gtk_tree_model_get (model, iter, COL_LABEL, &name, -1);
  g_object_set (renderer, "text", name, NULL);
}

static void
command_hidden_data_function (G_GNUC_UNUSED GtkTreeViewColumn * col, GtkCellRenderer * renderer, GtkTreeModel * model, GtkTreeIter * iter, G_GNUC_UNUSED gpointer user_data)
{
  KeymapCommandType type;
  gpointer action;
  gboolean hidden;
  gtk_tree_model_get (model, iter, COL_TYPE, &type, COL_ACTION, &action, -1);
  hidden = g_object_get_data (G_OBJECT (action), "hidden") ? TRUE : FALSE;
  g_object_set (renderer, "active", hidden, NULL);
}

/* UNUSED
static void
command_deleted_data_function (GtkTreeViewColumn * col, GtkCellRenderer * renderer, GtkTreeModel * model, GtkTreeIter * iter, gpointer user_data)
{
  KeymapCommandType type;
  gpointer action;
  gboolean deleted;
  gtk_tree_model_get (model, iter, COL_TYPE, &type, COL_ACTION, &action, -1);
  deleted = g_object_get_data (G_OBJECT (action), "deleted") ? TRUE : FALSE;
  g_object_set (renderer, "active", deleted, NULL);
}
*/
static gboolean search_tooltip=1;//implemented as searching substrings in tooltip, could be a level of match, for number of words present in tooltip & label
static gint last_idx=-1;//implemented as last found idx
static gboolean
search_equal_func (GtkTreeModel * model, gint G_GNUC_UNUSED column, const gchar * key, GtkTreeIter * iter, G_GNUC_UNUSED gpointer search_data)
{

  gchar *lookin;
  gchar *name;
  gboolean notfound;
  if(search_tooltip)
   gtk_tree_model_get (model, iter, COL_TOOLTIP, &lookin, -1);
  else
   gtk_tree_model_get (model, iter, COL_LABEL, &lookin, -1);
  gchar *this, *that;
  this = g_utf8_casefold (lookin, -1);
  that =  g_utf8_casefold (key, -1);
  if(search_tooltip)
  {
	  
	  gtk_tree_model_get (model, iter, COL_NAME, &name, -1);
	  const gint idx = lookup_command_from_name(Denemo.map, name);//= lookup_action_from_name (name);
	  notfound = (NULL == g_strstr_len (this, -1, that));
	  if((!notfound) && (idx <= last_idx))
		notfound = TRUE;
	  if(!notfound) 
			last_idx = idx;
  } 
  else
  {	
    notfound = (strcmp (this, that) < 0);
  }
  g_free(this);
  g_free(that);
  //g_free (lookin); The doc says that name should be freed, but two calls in succession yield the same pointer.
  return notfound;
}

/*toggle hidden on action at row in command list */
static void
toggle_hidden_on_action (G_GNUC_UNUSED GtkCellRendererToggle * cell_renderer, gchar * path)
{
  gint command_idx = atoi (path);
  GtkAction *action = (GtkAction *) lookup_action_from_idx (Denemo.map, command_idx);
  if (GTK_IS_ACTION (action))
    {
      gboolean hidden = (g_object_get_data (G_OBJECT (action), "hidden") != NULL);
      set_visibility_for_action (action, hidden);
    }
}

/*toggle deleted on action at row in command list */
/* UNUSED
static void
toggle_deleted_on_action (GtkCellRendererToggle * cell_renderer, gchar * path)
{
  gint command_idx = atoi (path);
  GtkAction *action = (GtkAction *) lookup_action_from_idx (Denemo.map, command_idx);
  if (GTK_IS_ACTION (action))
    {
      gboolean deleted = (g_object_get_data (G_OBJECT (action), "deleted") != NULL);
      //set_visibility_for_action(action, deleted);
      g_object_set_data (G_OBJECT (action), "deleted", (gboolean *) (intptr_t) ! deleted);
    }
}
*/
extern GtkWidget *get_command_view();
static void
search_next (GtkWidget *SearchEntry)
{
	if ( gtk_tree_selection_get_selected (gtk_tree_view_get_selection (GTK_TREE_VIEW(get_command_view())), NULL, NULL))
		;//last_idx++;
	else
		last_idx = -1;
  //FIXME issue some signal to cause a search to be made
  g_signal_emit_by_name (SearchEntry, "insert-at-cursor", "");
  gtk_widget_grab_focus (SearchEntry);
}
static void toggle_tooltip_search (void)
{
	search_tooltip = !search_tooltip;
}
static void selection_changed (GtkTreeSelection *selection, GtkWidget *SearchEntry) {
	gint command_idx;
	
	GtkTreeModel *model = GTK_TREE_MODEL (Denemo.map->commands);
	
	GtkTreeIter iter = { 0, NULL, NULL, NULL };
	const gchar *command_name;
	if(gtk_tree_selection_get_selected (selection, NULL, &iter))
	  {
		gtk_tree_model_get (model, &iter, COL_NAME, &command_name, -1);
		last_idx = lookup_command_from_name(Denemo.map, command_name);
	  }
}
GtkWidget *
keymap_get_command_view (keymap * the_keymap, GtkWidget *SearchEntry, GtkWidget *SearchNext)
{
  GtkScrolledWindow *res2;
  GtkTreeView *res;
  GtkTreeViewColumn *col;
  GtkCellRenderer *renderer;
  GtkTreeSelection *selection;
  GtkTreeModel *model;

  model = GTK_TREE_MODEL (the_keymap->commands);
  //setting up the tree view
  res = GTK_TREE_VIEW (gtk_tree_view_new ());

  
  gtk_tree_view_set_model (res, model);


  col = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (col, _("Command"));
  gtk_tree_view_append_column (res, col);

  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (col, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (col, renderer, label_data_function, NULL, NULL);

#if 0
/* including this column makes the command manager too wide */
  col = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (col, _("Commands"));
  gtk_tree_view_append_column (res, col);

  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (col, renderer, TRUE);
  gtk_tree_view_column_set_cell_data_func (col, renderer, command_name_data_function, NULL, NULL);
#endif
  col = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (col, _("Hidden"));
  gtk_tree_view_append_column (res, col);

  renderer = gtk_cell_renderer_toggle_new ();
  gtk_tree_view_column_pack_start (col, renderer, TRUE);
  gtk_tree_view_column_add_attribute (col, renderer, "active", COL_HIDDEN);
  gtk_tree_view_column_set_cell_data_func (col, renderer, command_hidden_data_function, NULL, NULL);
  g_signal_connect (renderer, "toggled", (GCallback) toggle_hidden_on_action, NULL);

#if 0
  col = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (col, _("Deleted"));
  gtk_tree_view_append_column (res, col);

  renderer = gtk_cell_renderer_toggle_new ();
  gtk_tree_view_column_pack_start (col, renderer, TRUE);
  gtk_tree_view_column_add_attribute (col, renderer, "active", COL_DELETED);
  gtk_tree_view_column_set_cell_data_func (col, renderer, command_deleted_data_function, NULL, NULL);
  g_signal_connect (renderer, "toggled", (GCallback) toggle_deleted_on_action, NULL);
#endif



  selection = gtk_tree_view_get_selection (res);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_BROWSE);
  
  g_signal_connect (G_OBJECT(selection),  "changed", G_CALLBACK(selection_changed), (gpointer)SearchEntry);
  
  gtk_tree_view_set_search_equal_func (res, search_equal_func, NULL, NULL);
//gtk_tree_view_set_search_column (res, COL_LABEL);
  gtk_tree_view_set_enable_search (res, TRUE);



  //setting up the scrolledwindow
  res2 = GTK_SCROLLED_WINDOW (gtk_scrolled_window_new (NULL, NULL));
  gtk_container_add (GTK_CONTAINER (res2), GTK_WIDGET (res));
  gtk_scrolled_window_set_policy (res2, GTK_POLICY_NEVER, GTK_POLICY_ALWAYS);

  //FIXME adapt so that 10~15 rows are visible
  gtk_widget_set_size_request (GTK_WIDGET (res2), -1, 300);
  
  GtkWidget *vbox = gtk_vbox_new (FALSE, 8);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 8);

  gtk_tree_view_set_search_entry (res, SearchEntry);
  GtkWidget *label = gtk_label_new (_("Search"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (hbox), SearchEntry, FALSE, TRUE, 0);
  
  gtk_box_pack_start (GTK_BOX (hbox), SearchNext, FALSE, TRUE, 0);

  GtkWidget *toggle = gtk_check_button_new_with_label (_("Search in tooltip"));
  gtk_toggle_button_set_active (toggle, TRUE);
  gtk_widget_set_can_focus (toggle, FALSE);
  g_signal_connect(G_OBJECT(toggle), "toggled", G_CALLBACK(toggle_tooltip_search), NULL);
  gtk_box_pack_end (GTK_BOX (hbox), toggle, FALSE, TRUE, 0);
  
  g_signal_connect_swapped (G_OBJECT (SearchNext), "clicked", G_CALLBACK (search_next), SearchEntry);
  
  gtk_box_pack_start (GTK_BOX (vbox), hbox, FALSE, TRUE, 0);
  gtk_box_pack_start (GTK_BOX (vbox), res2, TRUE, TRUE, 0);
		

  return GTK_WIDGET (res2);
}

void
row_inserted_handler (GtkTreeModel * model, GtkTreePath * arg1, GtkTreeIter * arg2, gpointer user_data)
{
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  //g_print("insert\n");
  if (cbdata->command_idx != -1)
    update_accel_labels (Denemo.map, cbdata->command_idx);
}

void
row_deleted_handler (GtkTreeModel * model, GtkTreePath * arg1, gpointer user_data)
{
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) user_data;
  //g_print("delete\n");
  if (cbdata->command_idx != -1)
    update_accel_labels (Denemo.map, cbdata->command_idx);
}

//Performs cleanup on the keymap when a command view is closed
gboolean
keymap_cleanup_command_view (keyboard_dialog_data * data)
{
#if 0
  GtkTreeModel *model;
  model = gtk_tree_view_get_model (data->binding_view);
  if (model)
    {
      g_signal_handlers_disconnect_by_func (model, row_deleted_handler, data);
    }

   Denemo.command_manager = NULL;
   return FALSE;//allow window to be destroyed
#endif   
   activate_action ("/MainMenu/ViewMenu/" "ToggleCommandManager");
  return TRUE; 
   
}
const gchar *
get_menu_label (gchar *name)
{ const gchar *label = NULL;
  GtkAction *action = gtk_action_group_get_action (Denemo.action_group, name);
  if(action) 
  {
	label = gtk_action_get_label (action);

   }
 if(!label) 
		label = name;
 return label;  
}

/* caller must free */
gchar *get_menu_position (gchar *menupath)
 {
	 if(menupath==NULL)
		menupath = g_strdup(_("Built-in, see file denemoui.xml for position"));
	 GString *position = g_string_new("");
	 gchar *path = g_strdup(menupath/* + 1 skip over the initial delimeter*/);
	 gchar *element = strtok (path, "/");
	 if(element) {
		g_string_append (position, get_menu_label(element));
		while ((element = strtok (NULL, "/"))) {
			if(*element)
				g_string_append_printf (position, "->%s", get_menu_label(element));	
			else
				g_string_append	 (position, "**");
			}
	}
	g_free(path);
	return g_string_free (position, FALSE);  
  }
gboolean
keymap_change_binding_view_on_command_selection (GtkTreeSelection * selection, GtkTreeModel * model, GtkTreePath * path, gboolean path_currently_selected, gpointer data)
{
  GtkTreeView *binding_view;
  GtkListStore *bindings;
  GtkTreeIter iter;
  GtkTreeModel *old_binding_model;
  GtkTextBuffer *text_buffer;
  KeymapCommandType type;
  gpointer action;
  gint *array;
  const gchar *tooltip;
  keyboard_dialog_data *cbdata = (keyboard_dialog_data *) data;

  //if the same command is selected again, we do nothing
  if (path_currently_selected)
    return TRUE;

  //getting the binding_view
  binding_view = cbdata->binding_view;

  //disconnecting signals of the old binding view
  old_binding_model = gtk_tree_view_get_model (binding_view);
  if (old_binding_model)
    {
      //g_signal_handlers_disconnect_by_func(old_binding_model,
      //        row_inserted_handler, data);
      g_signal_handlers_disconnect_by_func (old_binding_model, row_deleted_handler, data);
    }

  //getting the new model
  gtk_tree_model_get_iter (model, &iter, path);
  gtk_tree_model_get (model, &iter, COL_TYPE, &type, COL_ACTION, &action, COL_TOOLTIP, &tooltip, COL_BINDINGS, &bindings, -1);
  //getting the new command_idx
  array = gtk_tree_path_get_indices (path);
  cbdata->command_idx = array[0];

  //setting the model and releasing our reference
  gtk_tree_view_set_model (binding_view, GTK_TREE_MODEL (bindings));
  //g_signal_connect(bindings, "row-inserted", row_inserted_handler, data);
  g_signal_connect (bindings, "row-deleted", G_CALLBACK (row_deleted_handler), data);
  g_object_unref (bindings);
  //changing the tooltip
  text_buffer = gtk_text_view_get_buffer (cbdata->text_view);


  if (tooltip)
    {
      gchar *plain;
      gchar *menupath = get_menu_position (g_object_get_data (G_OBJECT (action), "menupath"));
      
	  gchar *text = g_strdup_printf (_( "%s\nLocation: %s\nInternal Name: %s"), tooltip, menupath, gtk_action_get_name(action));
      pango_parse_markup (text, -1, 0, NULL, &plain, 0, NULL);
	  g_free(text);
      gtk_text_buffer_set_text (text_buffer, plain, -1);
      g_free (plain);
      g_free(menupath);
    }
  //perform the selection
  return TRUE;
}

GtkWidget *
keymap_get_binding_view ()
{
  GtkScrolledWindow *res2;
  GtkTreeView *res;
  GtkTreeViewColumn *col;
  GtkCellRenderer *renderer;
  GtkTreeSelection *selection;

  //setting up the tree view
  res = GTK_TREE_VIEW (gtk_tree_view_new ());

  col = gtk_tree_view_column_new ();
  gtk_tree_view_column_set_title (col, _("Shortcuts"));
  gtk_tree_view_column_set_sizing (col, GTK_TREE_VIEW_COLUMN_GROW_ONLY);
  gtk_tree_view_append_column (res, col);

  renderer = gtk_cell_renderer_text_new ();
  gtk_tree_view_column_pack_start (col, renderer, TRUE);
  gtk_tree_view_column_add_attribute (col, renderer, "text", 0);

  selection = gtk_tree_view_get_selection (res);
  gtk_tree_selection_set_mode (selection, GTK_SELECTION_SINGLE);

  gtk_tree_view_set_enable_search (res, FALSE);

  gtk_tree_view_set_reorderable (res, TRUE);
  //setting up the scrolledwindow
  res2 = GTK_SCROLLED_WINDOW (gtk_scrolled_window_new (NULL, NULL));
  gtk_container_add (GTK_CONTAINER (res2), GTK_WIDGET (res));
  gtk_scrolled_window_set_policy (res2, GTK_POLICY_NEVER, GTK_POLICY_AUTOMATIC);

  //FIXME adapt so that 10~15 rows are visible
  gtk_widget_set_size_request (GTK_WIDGET (res2), -1, 300);

  return GTK_WIDGET (res2);
}
