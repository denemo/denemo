/* keysigdialog.c
 * Prompts the user to change the key signature
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "ui/dialogs.h"
#include "display/draw.h"
#include "command/object.h"
#include "command/staff.h"
#include "core/utils.h"
#include "core/cache.h"
#include "ui/keysigdialog.h"
#include "command/keyresponses.h"
#include "audio/pitchentry.h"

#define KEYNAME_ARRAY_OFFSET 7

//FIXME: Make those translatable
static gchar *majorkeys[15] = { "C flat", "G flat", "D flat", "A flat", "E flat", "B flat", "F",
  "C", "G", "D", "A", "E", "B", "F sharp", "C sharp"
};

static gchar *minorkeys[15] = { "A flat", "E flat", "B flat", "F", "C", "G", "D",
  "A", "E", "B", "F sharp", "C sharp", "G sharp", "D sharp", "A sharp"
};

static gchar *uminorkeys[15] = { "A FLAT", "E FLAT", "B FLAT", "F", "C", "G", "D",
  "A", "E", "B", "F SHARP", "C SHARP", "G SHARP", "D SHARP", "A SHARP"
};

static gchar *umajorkeys[15] = { "C FLAT", "G FLAT", "D FLAT", "A FLAT", "E FLAT", "B FLAT", "F",
  "C", "G", "D", "A", "E", "B", "F SHARP", "C SHARP"
};
/* UNUSED
static gchar *modes[7] = { "lydian", "ionian", "mixolydian", "dorian", "aeolian", "phrygian",
  "locrain"
};
*/
typedef struct keysig_data
{
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
//  GtkWidget *okbutton;
  GList *majorlist;
  GList *minorlist;
  GtkWidget *radiobutton2;
  gboolean initial;
} keysig_data;


/**
 * Finds key name and returns its numeric value
 *
 * Returns G_MININT if keyname cannot be found
 */

gint
findkey (GtkWidget * combobox, GList * list)
{
#if GTK_MAJOR_VERSION==3
  gchar *tokeystring = gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (combobox));
#else
  gchar *tokeystring = (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (combobox)->entry));
#endif
  gint ret;
  ret = g_list_position (list, g_list_find_custom (list, tokeystring, (GCompareFunc) strcmp));

  if (ret != -1)
    return ret - KEYNAME_ARRAY_OFFSET;
  else
    return G_MININT;
}

/**
 * Sets the initial key signature on either the current staff or
 * across the entire score.
 */
static void
set_keysig (keysig_data * cbdata)
{
  DenemoMovement *si = Denemo.project->movement;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ? 1 : 0;

  if (isminor == 0)
    tokey = findkey (cbdata->majorkeycombo, cbdata->majorlist);
  else
    tokey = findkey (cbdata->minorkeycombo, cbdata->minorlist);

  if (tokey != G_MININT)
    {
      if (cbdata->checkbutton && !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->checkbutton)))
        {
          for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
            {
              curstaffstruct = (DenemoStaff *) curstaff->data;
              dnm_setinitialkeysig (curstaffstruct, tokey - mode, isminor);
            }
          find_leftmost_allcontexts (si);
        }
      else
        {
          dnm_setinitialkeysig (curstaffstruct, tokey - mode, isminor);
        }
    }
  score_status (Denemo.project, TRUE);
}

/**
 * Inserts a key signature change either on a single staff or
 * across the entire score
 */
static void
insert_keysig (keysig_data * kdata)
{
  staffnode *curstaff;
  DenemoMovement *si = Denemo.project->movement;
  measurenode *curmeasure;
  gint tokey, mode;
  DenemoObject *newkey = NULL;
  tokey = mode = 0;

  gint isminor = gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (kdata->radiobutton2)) ? 1 : 0;

  if (isminor == 0)
    tokey = findkey (kdata->majorkeycombo, kdata->majorlist);
  else
    tokey = findkey (kdata->minorkeycombo, kdata->minorlist);

  if (tokey != G_MININT)
    {
      take_snapshot ();  
      if (kdata->checkbutton && !gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (kdata->checkbutton)))
        {
          for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
            {
              curmeasure = g_list_nth (staff_first_measure_node (curstaff), si->currentmeasurenum - 1);
              if (curmeasure)
                {
                    if (curmeasure == si->currentmeasure)
                        ((DenemoMeasure*)curmeasure->data)->objects = g_list_insert ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, newkey = dnm_newkeyobj ((tokey - mode), isminor, mode), si->cursor_x);
                    else
                    {
                        if(si->cursor_x<2)
                            ((DenemoMeasure*)curmeasure->data)->objects = g_list_prepend ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, newkey = dnm_newkeyobj ((tokey - mode), isminor, mode));
                        else
                            ((DenemoMeasure*)curmeasure->data)->objects = g_list_append ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, newkey = dnm_newkeyobj ((tokey - mode), isminor, mode));
                    }
                    newkey->keysig = newkey->object;
                    cache_measure (curmeasure); //to give the new keysig cached values
                    update_keysig_cache (curmeasure, g_list_find (((DenemoMeasure*)curmeasure->data)->objects, newkey));  //to update everything beyond the newkey
                    if (curmeasure == si->currentmeasure)
                        si->currentobject = g_list_nth ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, si->cursor_x);
                    staff_show_which_accidentals ((DenemoStaff *) curstaff->data);
                }
            }                   /* End for all staffs*/
        }                       /* End if check button all staffs*/
      else
        {
          object_insert (Denemo.project, newkey = dnm_newkeyobj (tokey - mode, isminor, mode));
          newkey->keysig = newkey->object;
          cache_measure (si->currentmeasure); //to give the new keysig cached values
          update_keysig_cache (si->currentmeasure, g_list_find (((DenemoMeasure*)si->currentmeasure->data)->objects, newkey)); //to update everything beyond the newkey
          staff_show_which_accidentals ((DenemoStaff *) si->currentstaff->data);
        }
      si->cursor_appending = FALSE;
      if (newkey)
        {
            adjust_tonal_center (((keysig *) (newkey->object))->accs);
        }
    }                           /* End if valid key*/
}
//static void sensitize_ok_button (keysig_data *data) {
  //   gtk_widget_set_sensitive (data->okbutton, TRUE);
//}
/**
 * Update the keysig dialogs combobox with the
 * major keys
 */
void
majorcallback (GtkWidget * widget, struct keysig_data *data)
{
//  sensitize_ok_button (data);
  gtk_widget_hide (data->minorkeycombo);
  gtk_widget_show (data->majorkeycombo);
}

/**
 * Update the keysig dialogs combobox with the
 * minor keys
 */
void
minorcallback (GtkWidget * widget, struct keysig_data *data)
{
//  sensitize_ok_button (data);
  gtk_widget_hide (data->majorkeycombo);
  gtk_widget_show (data->minorkeycombo);
}

/* interprets the scheme_string to set key number and isminor value
 */
static gboolean
key_from_string (GString * scheme_string, gint * tokey, gint * isminor)
{

  gchar *upper = g_ascii_strup (scheme_string->str, scheme_string->len);
  gint UNSET = G_MININT;
  gint length = 0;
  *isminor = (g_strstr_len (upper, -1, "MINOR")) ? 1 : 0;
  //g_debug("upper %s, scheme %s\n", upper, scheme_string->str);

  gchar **keystosearch = (*isminor ? uminorkeys : umajorkeys);
  gint i;
  for (*tokey = UNSET, i = 0; i < 15; i++)
    if (g_strstr_len (upper, -1, keystosearch[i]))
      {
        if (strlen (keystosearch[i]) > length)
          {
            *tokey = i - KEYNAME_ARRAY_OFFSET;
            length = strlen (keystosearch[i]);
          }
      }

  g_free (upper);
  if (*tokey != UNSET)
    return TRUE;
  return FALSE;
}

/**
 * callback for inserting a keysig change
 *  calls key_change with the INSERT argument
 * if called with action==NULL inserts the keychange from the GString param
 * and returns the key in it
 */
void
key_change_insert (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, keyname);
  DenemoProject *gui = Denemo.project;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  if (keyname == NULL)
    key_change (gui, INSERT);
  else
    {
      gint tokey, isminor;
      GString *scheme_str = g_string_new (keyname);
      gboolean valid = key_from_string (scheme_str, &tokey, &isminor);
      g_string_free (scheme_str, TRUE);
      if (valid)
        {
          object_insert (gui, dnm_newkeyobj (tokey, isminor, 0));
          staff_show_which_accidentals (gui->movement->currentstaff->data);
          displayhelper (gui);
        }
      gchar *key;
      isminor = 0;
      gboolean number;
      // FIXME try to find the previous keysig object, else return initial keysig
      /* if currentobject is keysig change return the keysig */
      DenemoObject *curobj = (DenemoObject *) Denemo.project->movement->currentobject->data;
      if (curobj && curobj->type == KEYSIG)
        {
          isminor = ((keysig *) curobj->object)->isminor;
          number = ((keysig *) curobj->object)->number + KEYNAME_ARRAY_OFFSET;
        }
      else
        {
          isminor = (curstaffstruct->keysig.isminor == 1);
          number = curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET;
        }
      /* return initial key */
      if (isminor)
        key = g_strdup_printf ("%s %s", minorkeys[number], "Minor");
      else
        key = g_strdup_printf ("%s %s", majorkeys[number], "Major");
      g_string_assign (param->string, key);
      g_free (key);
    }
}


gchar *
get_prevailing_keysig_name (void)
{ 
  gchar *key;
  keysig *keysig = get_prevailing_context (KEYSIG);
  gboolean isminor = keysig->isminor;
  gint number = keysig->number + KEYNAME_ARRAY_OFFSET;
  if (isminor)
    key = g_strdup_printf ("%s %s", minorkeys[number], "Minor");
  else
    key = g_strdup_printf ("%s", majorkeys[number]); //do not use Major as the crude algorithm identifies the letter A in major as the note name A !!
  return key;
}
/**
 * callback for changing the initial keysig
 *  calls key_change with the CHANGEINITIAL argument
 */
void
key_change_initial (DenemoAction * action, DenemoScriptParam * param)
{
  GET_1PARAM (action, param, keyname);
  DenemoProject *gui = Denemo.project;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->movement->currentstaff->data;
  if (keyname == NULL)
    key_change (gui, CHANGEINITIAL);
  else
    {
      if (query && !strcmp ("keysigname", query))
        {
          gchar *key;
          if (curstaffstruct->keysig.isminor == 1)
            key = g_strdup_printf ("%s %s", minorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET], "Minor");
          else
            key = g_strdup_printf ("%s %s", majorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET], "Major");
          g_string_assign (param->string, key);
          param->status = TRUE;
          g_free (key);
        } else 
        {        
        
          gint tokey, isminor;
          GString *scheme_str = g_string_new (keyname);
          gboolean valid = key_from_string (scheme_str, &tokey, &isminor);
          g_string_free (scheme_str, TRUE);
          if (valid)
            {
              param->status = TRUE;
              dnm_setinitialkeysig (curstaffstruct, tokey, isminor);
              displayhelper (gui);
            }
        }
    }
}

static void
button_response (GtkWidget * dialog, gint response_id, keysig_data * data)
{
  DenemoProject *gui = Denemo.project;
  if (response_id == GTK_RESPONSE_ACCEPT)
    {
      if (data->initial)
        set_keysig (data);
      else
        {
          if (gui->movement->currentobject && ((DenemoObject *) gui->movement->currentobject->data)->type == KEYSIG)
            gui->movement->cursor_appending?deletepreviousobject(NULL, NULL):deleteobject (NULL, NULL);
          insert_keysig (data);
        }
      cache_all ();// this may be overkill, but if a keysig change is being placed immediately after a clef change then the object does not have a clef cached on it.        
      score_status (gui, TRUE);
      displayhelper (gui);
    }
  gtk_widget_destroy (dialog);
  g_free (data);
}

/**
 * Key sig change dialog
 * Allows user to select key from a drop down list
 *
 */
void
key_change (DenemoProject * gui, actiontype action)
{
  GtkWidget *dialog;
  gboolean initial = action == CHANGEINITIAL ? TRUE : FALSE;
  /* GUI setup */
  dialog = gtk_dialog_new_with_buttons (_("Key Signature Change"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);
 // GtkWidget *okbutton = gtk_dialog_add_button (GTK_DIALOG(dialog), _("_OK"), GTK_RESPONSE_ACCEPT);
  //gtk_widget_set_sensitive (okbutton, FALSE);
  if (action == CHANGEINITIAL)
    gtk_window_set_title (GTK_WINDOW (dialog), _("Change initial key signature"));
  else
    gtk_window_set_title (GTK_WINDOW (dialog), _("Insert key signature change"));

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
  gtk_container_add (GTK_CONTAINER (content_area), vbox);

  keysig_data *keysig_widgets = (keysig_data *) g_malloc0 (sizeof (keysig_data));
//  keysig_widgets->okbutton = okbutton;
  GtkWidget *label;
  GtkWidget *radiobutton1, *radiobutton2;
  GtkWidget *checkbutton;

  static GList *majorlist = NULL;
  static GList *minorlist = NULL;

  gint i;
  if (majorlist == NULL)
    {
      for (i = 0; i < G_N_ELEMENTS (majorkeys); i++)
        majorlist = g_list_append (majorlist, majorkeys[i]);
      for (i = 0; i < G_N_ELEMENTS (minorkeys); i++)
        minorlist = g_list_append (minorlist, minorkeys[i]);
    }
#if GTK_MAJOR_VERSION==3
  GtkWidget *majorkeycombo = gtk_combo_box_text_new ();
  GtkWidget *minorkeycombo = gtk_combo_box_text_new ();
  for (i = 0; i < G_N_ELEMENTS (majorkeys); i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (majorkeycombo), majorkeys[i]);
  for (i = 0; i < G_N_ELEMENTS (minorkeys); i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT (minorkeycombo), minorkeys[i]);
#else
  GtkWidget *majorkeycombo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (majorkeycombo), majorlist);
  GtkWidget *minorkeycombo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (minorkeycombo), minorlist);
#endif
//  g_signal_connect_swapped (majorkeycombo, "changed", sensitize_ok_button, keysig_widgets);
//  g_signal_connect_swapped (minorkeycombo, "changed", sensitize_ok_button, keysig_widgets);
  GtkWidget *pack_to_vbox = gtk_vbox_new (FALSE, 1);
  label = gtk_label_new (_("Select desired key signature"));
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), label);

  radiobutton1 = gtk_radio_button_new_with_label (NULL, _("Major"));
  g_signal_connect (G_OBJECT (radiobutton1), "clicked", G_CALLBACK (majorcallback), keysig_widgets);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), radiobutton1);

  radiobutton2 = gtk_radio_button_new_with_label (gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton1)), _("Minor"));
  g_signal_connect (G_OBJECT (radiobutton2), "clicked", G_CALLBACK (minorcallback), keysig_widgets);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), radiobutton2);

#if GTK_MAJOR_VERSION==3
  gtk_combo_box_set_active (GTK_COMBO_BOX (majorkeycombo), 7);
  gtk_combo_box_set_active (GTK_COMBO_BOX (minorkeycombo), 7);
#endif

  gtk_container_add (GTK_CONTAINER (pack_to_vbox), majorkeycombo);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), minorkeycombo);

  if (gui->movement->currentobject && ((DenemoObject *) gui->movement->currentobject->data)->type == KEYSIG)
    checkbutton = NULL;
   else
    {
    checkbutton = gtk_check_button_new_with_label (_("Current Staff Only?"));
    gtk_container_add (GTK_CONTAINER (pack_to_vbox), checkbutton);
    } 

  keysig_widgets->checkbutton = checkbutton;
  keysig_widgets->radiobutton2 = radiobutton2;
  keysig_widgets->majorkeycombo = majorkeycombo;
  keysig_widgets->minorkeycombo = minorkeycombo;
  keysig_widgets->majorlist = majorlist;
  keysig_widgets->minorlist = minorlist;
  keysig_widgets->initial = initial;

  gtk_widget_grab_focus (majorkeycombo);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton1), TRUE);
  gtk_widget_hide (keysig_widgets->minorkeycombo);
  gtk_widget_show (keysig_widgets->majorkeycombo);

  gtk_container_add (GTK_CONTAINER (vbox), pack_to_vbox);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);
  gtk_widget_hide (keysig_widgets->minorkeycombo);
    
  g_signal_connect (dialog, "response", G_CALLBACK (button_response), keysig_widgets);
}
