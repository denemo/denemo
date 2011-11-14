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

static gint
find_element_position(gchar **haystack, gchar *needle)
{
  gint i;
  for(i=0;i<G_N_ELEMENTS(haystack);i++)
    if (g_strcmp0(haystack[i], needle) == 0)
      return i;
}

gint
findmode (keysig_data *ksdata)
{
  gint ret = -1;
  gchar *mode = gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (ksdata->modenamecombo));
  ret = find_element_position(modes, mode);
  return ret - 1;
}

/**
 * Finds key name and returns its numeric value
 *
 * Returns G_MININT if keyname cannot be found 
 */

gint
findkey (GtkWidget * combobox, gint type)
{
  gchar *tokeystring = gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (combobox));

  gint ret;
  if (type == 2)
    ret = find_element_position(modes, tokeystring);
  else if (type == 1)
    ret = find_element_position(minorkeys, tokeystring);
  else
    ret = find_element_position(majorkeys, tokeystring);
  
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
set_keysig (GtkWidget * widget, keysig_data *cbdata)
{
  DenemoScore *si = Denemo.gui->si;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ?
    1 :
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton3)) ?
    2 : 0;
  tokey = findkey (cbdata->majorkeycombo, isminor);
  if (isminor == 2)
    mode = findmode (cbdata->modenamecombo);

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
insert_keysig (GtkWidget * widget, gpointer data)
{
  struct keysig_data *cbdata = (struct keysig_data *) data;
  staffnode *curstaff;
  DenemoScore *si = Denemo.gui->si;
  measurenode *curmeasure;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ?
    1 :
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton3)) ?
    2 : 0;
  tokey = findkey (cbdata->majorkeycombo, isminor);
  if (isminor == 2)
    mode = findmode (cbdata->modenamecombo);

  if (tokey != G_MININT)
    {
      if (gtk_toggle_button_get_active
	  (GTK_TOGGLE_BUTTON (cbdata->checkbutton)))
	{
	  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
	    {
	      curmeasure = g_list_nth (firstmeasurenode (curstaff),
				       si->currentmeasurenum - 1);
	      if(curmeasure) {
		curmeasure->data = g_list_append ((objnode *) curmeasure->data,
						  dnm_newkeyobj ((tokey - mode),
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

	  object_insert (Denemo.gui, dnm_newkeyobj (tokey - mode, isminor, mode));
	  showwhichaccidentalswholestaff ((DenemoStaff *) si->currentstaff->
					  data);
	}
      si->cursor_appending = FALSE;
    adjust_tonal_center( ((keysig*)((DenemoObject*)si->currentobject->data)->object)->accs);
    }				/* End if */

  g_free(cbdata);
}

/**
 * Update the keysig dialogs combobox with the 
 * major keys
 */
void
majorcallback (GtkWidget * widget, struct modedata *data)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;

  gtk_combo_box_set_active(GTK_COMBO_BOX (data->majorkeycombo),
         curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET);

  gtk_widget_hide (data->minorkeycombo);
  gtk_widget_hide (data->modenamecombo);
  gtk_widget_show (data->majorkeycombo);
}

/**
 * Update the keysig dialogs combobox with the 
 * minor keys
 */
void
minorcallback (GtkWidget * widget, struct modedata *data)
{
  DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.gui->si->currentstaff->data;

  gtk_combo_box_set_active(GTK_COMBO_BOX (data->minorkeycombo),
         curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET);

  gtk_widget_hide (data->majorkeycombo);
  gtk_widget_hide (data->modenamecombo);
  gtk_widget_show (data->minorkeycombo);
}


/**
 * Update the keysig dialogs combobox with the 
 * modes
 */
void
modecallback (GtkWidget * widget, struct modedata *data)
{
  gtk_widget_hide (data->minorkeycombo);
  gtk_widget_show (data->majorkeycombo);
  gtk_widget_show (data->modenamecombo);
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

GtkWidget *
keysig_widget_new(keysig_data *keysig_widgets)
{
  GtkWidget *label;
  GtkWidget *radiobutton1, *radiobutton2, *radiobutton3;
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo = gtk_combo_box_text_new ();
  GtkWidget *minorkeycombo = gtk_combo_box_text_new ();
  GtkWidget *modenamecombo = gtk_combo_box_text_new ();
  gint i;
  DenemoScore *si = Denemo.gui->si;
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;

  for(i=0;i<G_N_ELEMENTS(majorkeys);i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(majorkeycombo), majorkeys[i]);
  for(i=0;i<G_N_ELEMENTS(minorkeys);i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(minorkeycombo), minorkeys[i]);
  for(i=0;i<G_N_ELEMENTS(modes);i++)
    gtk_combo_box_text_append_text (GTK_COMBO_BOX_TEXT(modenamecombo), modes[i]);

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

  radiobutton3 = gtk_radio_button_new_with_label
    (gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton1)), _("Mode"));
  g_signal_connect (G_OBJECT (radiobutton3), "clicked",
		    G_CALLBACK(modecallback), keysig_widgets);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), radiobutton3);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), majorkeycombo);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), minorkeycombo);
  gtk_container_add (GTK_CONTAINER (pack_to_vbox), modenamecombo);

  checkbutton = gtk_check_button_new_with_label (_("Apply to all staves?"));

  gtk_container_add (GTK_CONTAINER (pack_to_vbox), checkbutton);

  keysig_widgets->checkbutton = checkbutton;
  keysig_widgets->radiobutton1 = radiobutton1;
  keysig_widgets->radiobutton2 = radiobutton2;
  keysig_widgets->radiobutton3 = radiobutton3;
  keysig_widgets->majorkeycombo = majorkeycombo;
  keysig_widgets->minorkeycombo = minorkeycombo;
  keysig_widgets->modenamecombo = modenamecombo;

  gtk_widget_grab_focus (majorkeycombo);

  if (curstaffstruct->keysig.isminor == 2)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton3), TRUE);
      gtk_widget_show (keysig_widgets->modenamecombo);
    }
  else if (curstaffstruct->keysig.isminor == 1)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton2), TRUE);
      gtk_widget_hide (keysig_widgets->majorkeycombo);
      gtk_widget_hide (keysig_widgets->modenamecombo);
      gtk_widget_show (keysig_widgets->minorkeycombo);
      gtk_combo_box_set_active(GTK_COMBO_BOX (minorkeycombo),
	 curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET);
    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton1), TRUE);
      gtk_widget_hide (keysig_widgets->minorkeycombo);
      gtk_widget_hide (keysig_widgets->modenamecombo);
      gtk_widget_show (keysig_widgets->majorkeycombo);
      gtk_combo_box_set_active(GTK_COMBO_BOX (majorkeycombo),
	curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET);
    }
  return pack_to_vbox;
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
  GtkWidget *keysig = keysig_widget_new(keysig_widgets);
  gtk_container_add (GTK_CONTAINER (vbox), keysig);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);
 
  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      if (action == CHANGEINITIAL)
	{
	  set_keysig (NULL, keysig_widgets);
	}
      else
	{
	  if(gui->si->currentobject && ((DenemoObject*)gui->si->currentobject->data)->type==KEYSIG)
	    deleteobject(gui);
	  insert_keysig (NULL, keysig_widgets);
	}
      score_status(gui, TRUE);
      displayhelper (gui);
    }

  gtk_widget_destroy (dialog);
}
