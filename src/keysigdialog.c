/* keysigdialog.c
 * Prompts the user to change the key signature
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller 
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"
#include "keysigdialog.h"

#define KEYNAME_ARRAY_OFFSET 7

static gchar *majorkeys[15] =
  { "C flat", "G flat", "D flat", "A flat", "E flat", "B flat", "F",
  "C", "G", "D", "A", "E", "B", "F sharp", "C sharp"
};

static gchar *minorkeys[15] =
  { "A flat", "E flat", "B flat", "F", "C", "G", "D",
  "A", "E", "B", "F sharp", "C sharp", "G sharp", "D sharp", "A sharp"
};

static gchar *uminorkeys[15] =
  { "A FLAT", "E FLAT", "B FLAT", "F", "C", "G", "D",
  "A", "E", "B", "F SHARP", "C SHARP", "G SHARP", "D SHARP", "A SHARP"
};

static gchar *umajorkeys[15] =
  { "C FLAT", "G FLAT", "D FLAT", "A FLAT", "E FLAT", "B FLAT", "F",
  "C", "G", "D", "A", "E", "B", "F SHARP", "C SHARP"
};

static gchar *modes[7] =
  { "lydian", "ionian", "mixolydian", "dorian", "aeolian", "phrygian",
"locrain" };

typedef struct keysig_data
{
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
  GList     *majorlist;
  GList     *minorlist;
  GtkWidget *radiobutton2;
  gboolean  initial;
}keysig_data;

void set_keysig (struct keysig_data *data);
/**
 * Finds key name and returns its numeric value
 *
 * Returns G_MININT if keyname cannot be found 
 */

gint
findkey (GtkWidget * combobox, GList *list)
{
#if GTK_MAJOR_VERSION==3
  gchar *tokeystring = gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (combobox));
#else
   gchar *tokeystring =
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (combobox)->entry));
#endif
  gint ret;
  ret = g_list_position
    (list,
     g_list_find_custom (list, tokeystring,
                           (GCompareFunc) strcmp));
  
  if (ret != -1)
    return ret - KEYNAME_ARRAY_OFFSET;
  else
    return G_MININT;
}

/**
 * Sets the initial key signature on either the current staff or 
 * across the entire score.
 */
void
set_keysig (keysig_data *cbdata)
{
  DenemoScore *si = Denemo.gui->si;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ?
    1 : 0;

  if (isminor == 0)
    tokey = findkey (cbdata->majorkeycombo, cbdata->majorlist);
  else
    tokey = findkey (cbdata->minorkeycombo, cbdata->minorlist);
  
  if (tokey != G_MININT)
    {
      if (gtk_toggle_button_get_active
	  (GTK_TOGGLE_BUTTON (cbdata->checkbutton)))
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
  score_status(Denemo.gui, TRUE);
}

/**
 * Inserts a key signature change either on a single staff or
 * across the entire score
 */
void
insert_keysig (keysig_data *kdata)
{
  staffnode *curstaff;
  DenemoScore *si = Denemo.gui->si;
  measurenode *curmeasure;
  gint tokey, mode;
  DenemoObject *newkey = NULL;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (kdata->radiobutton2)) ?
    1 : 0;

  if (isminor == 0)
    tokey = findkey (kdata->majorkeycombo, kdata->majorlist);
  else 
    tokey = findkey (kdata->minorkeycombo, kdata->minorlist);
  
  if (tokey != G_MININT)
    {
      if (gtk_toggle_button_get_active
	  (GTK_TOGGLE_BUTTON (kdata->checkbutton)))
	{
	  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
	    {
	      curmeasure = g_list_nth (firstmeasurenode (curstaff),
				       si->currentmeasurenum - 1);
	      if(curmeasure) {
		curmeasure->data = g_list_append ((objnode *) curmeasure->data,
						  newkey = dnm_newkeyobj ((tokey - mode),
								 isminor, mode));
		if (curmeasure == si->currentmeasure)
		  si->currentobject =
		    g_list_nth ((objnode *) curmeasure->data, si->cursor_x);
		showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
	      }
	    }			/* End for */
	}			/* End if */
      else
	{

	  object_insert (Denemo.gui, newkey = dnm_newkeyobj (tokey - mode, isminor, mode));
	  showwhichaccidentalswholestaff ((DenemoStaff *) si->currentstaff->
					  data);
	}
      si->cursor_appending = FALSE;
      if(newkey)
	adjust_tonal_center( ((keysig*)(newkey->object))->accs);
    }				/* End if */
}

/**
 * Update the keysig dialogs combobox with the 
 * major keys
 */
void
majorcallback (GtkWidget * widget, struct keysig_data *data)
{
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
  gtk_widget_hide (data->majorkeycombo);
  gtk_widget_show (data->minorkeycombo);
}

/* interprets the scheme_string to set key number and isminor value
 */
static gboolean
key_from_string(GString *scheme_string, gint *tokey, gint *isminor) {

  gchar *upper = g_ascii_strup(scheme_string->str, scheme_string->len);
  gint UNSET = G_MININT;
  gint length=0;
  *isminor =  (g_strstr_len(upper, scheme_string->len, "MINOR"))?1:0;
  //g_print("upper %s, scheme %s\n", upper, scheme_string->str);
  
  gchar **keystosearch = (*isminor?uminorkeys:umajorkeys);
  gint i;
  for(*tokey=UNSET, i=0;i<15;i++)
    if(g_str_has_prefix( upper, keystosearch[i])) {
      if(strlen(keystosearch[i])>length){
	*tokey = i - KEYNAME_ARRAY_OFFSET;
	length = strlen(keystosearch[i]);
      }
    }

  g_free(upper);
  if(*tokey!=UNSET)
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
key_change_insert (GtkAction * action, DenemoScriptParam * param)
{
  GET_1PARAM(action, param, keyname);
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  if(keyname==NULL)
    key_change (gui, INSERT);
  else {
    gint tokey, isminor;
    GString *scheme_str = g_string_new(keyname);
    gboolean valid = key_from_string(scheme_str, &tokey, &isminor);
    g_string_free(scheme_str, TRUE);
    if(valid) {
      object_insert (gui, dnm_newkeyobj (tokey, isminor, 0));
      showwhichaccidentalswholestaff (gui->si->currentstaff->data);
      displayhelper (gui);
    }
    gchar *key;
    isminor=0;
    gboolean number;
    // FIXME try to find the previous keysig object, else return initial keysig
    /* if currentobject is keysig change return the keysig */
    DenemoObject *curobj = (DenemoObject *)Denemo.gui->si->currentobject->data;
    if(curobj && curobj->type==KEYSIG){
      isminor = ((keysig *)curobj->object)->isminor;
      number = ((keysig *)curobj->object)->number + KEYNAME_ARRAY_OFFSET;
    } else {
      isminor = (curstaffstruct->keysig.isminor == 1);
      number = curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET;
    }
    /* return initial key */
    if (isminor)
      key = g_strdup_printf( "%s %s",minorkeys[number], "Minor");
    else
      key = g_strdup_printf( "%s %s",majorkeys[number], "Major");
    g_string_assign(param->string, key);
    g_free(key);
  }
}

/**
 * callback for changing the initial keysig 
 *  calls key_change with the CHANGEINITIAL argument
 */
void
key_change_initial (GtkAction * action, DenemoScriptParam * param)
{
  GET_1PARAM(action, param, keyname);
  DenemoGUI *gui = Denemo.gui;
  DenemoStaff *curstaffstruct = (DenemoStaff *) gui->si->currentstaff->data;
  if(keyname==NULL)
    key_change (gui, CHANGEINITIAL);
  else {
    gint tokey, isminor;
    GString *scheme_str = g_string_new(keyname);
    gboolean valid = key_from_string(scheme_str, &tokey, &isminor);
    g_string_free(scheme_str, TRUE);
    if(valid) {
      dnm_setinitialkeysig (curstaffstruct, tokey, isminor);
      displayhelper (gui);
    }

    gchar *key;
    if (curstaffstruct->keysig.isminor == 1)
      key = g_strdup_printf( "%s %s",minorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET], "Minor");
    else
      key = g_strdup_printf( "%s %s",majorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET], "Major");
    g_string_assign(param->string, key);
    g_free(key);

  }
}
 
static void 
button_response(GtkWidget *dialog, gint response_id, keysig_data *data) 
{
  DenemoGUI *gui = Denemo.gui;
  if (data->initial)
    set_keysig (data);
  else
    {
      if(gui->si->currentobject && ((DenemoObject*)gui->si->currentobject->data)->type==KEYSIG)
	deleteobject(gui);
      insert_keysig (data);
     }
  score_status(gui, TRUE);
  displayhelper (gui);
  gtk_widget_destroy (dialog);
  g_free(data);
} 
/**
 * Key sig change dialog
 * Allows user to select key from a drop down list
 *
 */
void
key_change (DenemoGUI * gui, actiontype action)
{
  GtkWidget *dialog;
  gboolean initial = action == CHANGEINITIAL?TRUE:FALSE; 
  /* GUI setup */
  dialog = gtk_dialog_new_with_buttons (_("Key Signature Change"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) 
					(GTK_DIALOG_MODAL |
					 GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);
  
  if (action == CHANGEINITIAL)
    gtk_window_set_title (GTK_WINDOW (dialog),
			  _("Change initial key signature"));
  else 
    gtk_window_set_title (GTK_WINDOW (dialog),
			  _("Insert key signature change"));
  
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  GtkWidget *vbox = gtk_vbox_new(FALSE,1);
  gtk_container_add (GTK_CONTAINER (content_area), vbox);
 
  keysig_data *keysig_widgets = (keysig_data *) g_malloc0(sizeof(keysig_data));
  GtkWidget *label;
  GtkWidget *radiobutton1, *radiobutton2, *radiobutton3;
  GtkWidget *checkbutton;
  DenemoScore *si = Denemo.gui->si;
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;
  
  static GList *majorlist = NULL;
  static GList *minorlist = NULL;
  
  gint i;
  for(i=0;i<G_N_ELEMENTS(majorkeys);i++)
    majorlist = g_list_append(majorlist, majorkeys[i]);
  for(i=0;i<G_N_ELEMENTS(minorkeys);i++)
    minorlist = g_list_append(minorlist, minorkeys[i]);

#if GTK_MAJOR_VERSION==3
  GtkWidget *majorkeycombo = gtk_combo_box_text_new ();
  GtkWidget *minorkeycombo = gtk_combo_box_text_new ();
  for(i=0;i<G_N_ELEMENTS(majorkeys);i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(majorkeycombo), majorkeys[i]);
  for(i=0;i<G_N_ELEMENTS(minorkeys);i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(minorkeycombo), minorkeys[i]);
#else 
  GtkWidget *majorkeycombo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (majorkeycombo), majorlist);  
  GtkWidget *minorkeycombo = gtk_combo_new ();
  gtk_combo_set_popdown_strings (GTK_COMBO (minorkeycombo), minorlist);  
#endif

  GtkWidget *pack_to_vbox = gtk_vbox_new(FALSE,1);
  label = gtk_label_new (_("Select desired key signature"));
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), label);

  radiobutton1 = gtk_radio_button_new_with_label (NULL, _("Major"));
  g_signal_connect (G_OBJECT (radiobutton1), "clicked",
		    G_CALLBACK(majorcallback), keysig_widgets);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), radiobutton1);

  radiobutton2 = gtk_radio_button_new_with_label
    (gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton1)), _("Minor"));
  g_signal_connect (G_OBJECT (radiobutton2), "clicked",
		    G_CALLBACK(minorcallback), keysig_widgets);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), radiobutton2);

  gtk_container_add (GTK_CONTAINER (pack_to_vbox), majorkeycombo);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), minorkeycombo);

  checkbutton = gtk_check_button_new_with_label (_("Apply to all staves?"));
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), checkbutton);

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
  gtk_widget_show_all(dialog);
  gtk_widget_hide(keysig_widgets->minorkeycombo);

  g_signal_connect(dialog, "response", G_CALLBACK(button_response), keysig_widgets);
}
