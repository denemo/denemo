/* keysigdialog.cpp
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


/*static gchar *modes[7] =
  {
    "ionian", "locrian", "aeolian", "mixolydian",
    "lydian", "phrygian", "dorian"
  };

static gchar *pitches[17] =
  {
    "c", "c#", "db", "d", "d#", "eb", "e", "f", "f#", "gb", "g", "g#", "ab" ,"a", "a#", "bb" ,"b"
  };
*/


gint
findmode (GtkWidget * modebox, modedata *mdata)
{
  gint ret = -1;
  gchar *mode =
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (modebox)->entry));
  ret =
    g_list_position (mdata->modelist,
		     g_list_find_custom (mdata->modelist, mode,
					 (GCompareFunc) strcmp));

  /*if(ret != -1)
     {


     }
   */
  return ret - 1;
}

/**
 * Finds key name and returns its numeric value
 *
 * Returns G_MININT if keyname cannot be found 
 */

gint
findkey (GtkWidget * combobox, modedata *mdata, gint type)
{
  gchar *tokeystring =
    (gchar *) gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (combobox)->entry));
  gint ret;
  if (type == 2)
    {
      ret = g_list_position
	(mdata->majorlist,
	 g_list_find_custom (mdata->majorlist, tokeystring, 
			     (GCompareFunc) strcmp));
    }
  else if (type == 1)
    ret = g_list_position
      (mdata->minorlist,
       g_list_find_custom (mdata->minorlist, tokeystring, 
			   (GCompareFunc) strcmp));
  else
    ret = g_list_position
      (mdata->majorlist,
       g_list_find_custom (mdata->majorlist, tokeystring, 
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
set_keysig (GtkWidget * widget, gpointer data)
{
  struct keysig_callbackdata *cbdata = (struct keysig_callbackdata *) data;
  DenemoScore *si = cbdata->gui->si;
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ?
    1 :
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton3)) ?
    2 : 0;
  tokey = findkey (cbdata->combobox, cbdata->mdata, isminor);
  if (isminor == 2)
    mode = findmode (cbdata->mode, cbdata->mdata);

  //printf("\n tokey in set_keysig calling findmode returns %i\n",tokey);
  //printf("\n mode in set_keysig calling findmode returns %i\n",mode);

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
	  dnm_setinitialkeysig (cbdata->curstaffstruct, tokey - mode, isminor);
	}

    }
  score_status(cbdata->gui, TRUE);
}

/**
 * Inserts a key signature change either on a single staff or
 * across the entire score
 */
void
insert_keysig (GtkWidget * widget, gpointer data)
{
  struct keysig_callbackdata *cbdata = (struct keysig_callbackdata *) data;
  staffnode *curstaff;
  DenemoScore *si = cbdata->gui->si;
  measurenode *curmeasure;
  gint tokey, mode;
  tokey = mode = 0;

  gint isminor =
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton2)) ?
    1 :
    gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (cbdata->radiobutton3)) ?
    2 : 0;
  tokey = findkey (cbdata->combobox,cbdata->mdata, isminor);
  if (isminor == 2)
    mode = findmode (cbdata->mode, cbdata->mdata);


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

	  object_insert (cbdata->gui, dnm_newkeyobj (tokey - mode, isminor, mode));
	  showwhichaccidentalswholestaff ((DenemoStaff *) si->currentstaff->
					  data);
	}
      si->cursor_appending = FALSE;
    adjust_tonal_center( ((keysig*)((DenemoObject*)si->currentobject->data)->object)->accs);
    }				/* End if */

  g_free(cbdata->mdata);
  g_free(cbdata);
}

/**
 * Update the keysig dialogs combobox with the 
 * major keys
 */
void
majorcallback (GtkWidget * widget, struct modedata *data)
{
  gchar *text =
    g_strdup (gtk_entry_get_text
	      (GTK_ENTRY (GTK_COMBO (data->combobox)->entry)));

  gtk_combo_set_popdown_strings (GTK_COMBO (data->combobox), data->majorlist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (data->combobox)->entry), text);

  gtk_widget_hide (data->pitchcombo);
  g_free (text);
}


/**
 * Update the keysig dialogs combobox with the 
 * minor keys
 */
void
minorcallback (GtkWidget * widget, struct modedata *data)
{
  gchar *text =
    g_strdup (gtk_entry_get_text
	      (GTK_ENTRY (GTK_COMBO (data->combobox)->entry)));

  gtk_combo_set_popdown_strings (GTK_COMBO (data->combobox), data->minorlist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (data->combobox)->entry), text);

  gtk_widget_hide (data->pitchcombo);
  g_free (text);
}


/**
 * Update the keysig dialogs combobox with the 
 * modes
 */
void
modecallback (GtkWidget * widget, struct modedata *data)
{
  gchar *text =
    g_strdup (gtk_entry_get_text (GTK_ENTRY (GTK_COMBO (data)->entry)));
  gtk_combo_set_popdown_strings (GTK_COMBO (data), data->majorlist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (data)->entry), text);
  g_free (text);
}

/**
 * Update the keysig dialogs combobox with the 
 * pitches for the modes
 */
void
modedialog (GtkWidget * widget, struct modedata *mdata)
{

  gtk_combo_set_popdown_strings (GTK_COMBO (mdata->pitchcombo), mdata->modelist);
  gtk_entry_set_text (GTK_ENTRY (GTK_COMBO (mdata->pitchcombo)->entry),
		      modes[0]);
  gtk_widget_show (mdata->pitchcombo);

  //gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (mdata->radiobutton3), TRUE);
//  modecallback (NULL, (gpointer) mdata->combobox);

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
/**
 * Key sig change dialog
 * Allows user to select key from a drop down list
 * 
 */
void
key_change (DenemoGUI * gui, actiontype action)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *radiobutton1, *radiobutton2, *radiobutton3;
  GtkWidget *checkbutton;
  GtkWidget *combobox;
  GtkWidget *hbox;
  GtkWidget *pitchescombo = gtk_combo_new ();	//combobox to hold pitches for modes
  gint i;
  static GString *menustring = NULL;
  DenemoScore *si = gui->si;
  DenemoStaff *curstaffstruct = (DenemoStaff *) si->currentstaff->data;
  keysig_callbackdata *cbdata = (keysig_callbackdata *) g_malloc0(sizeof(keysig_callbackdata));
  modedata *mdata = cbdata->mdata = (modedata *) g_malloc0(sizeof(modedata));
  static GList *majorlist = NULL;
  static GList *minorlist = NULL;
  static GList *modelist = NULL;
  //static GList *pitchlist = NULL;

  if (si->lily_file && action == CHANGEINITIAL)
    return;			/* no code for this yet - just edit textually */

  /* Initialization */
  if (!menustring)
    menustring = g_string_new (NULL);

  if (!majorlist)
    for (i = 0; i < 15; i++)
      {
	majorlist = g_list_append (majorlist, majorkeys[i]);
	minorlist = g_list_append (minorlist, minorkeys[i]);
      }
  if (!modelist)
    for (i = 0; i < 7; i++)
      modelist = g_list_append (modelist, modes[i]);

  mdata->majorlist = majorlist;
  mdata->minorlist = minorlist;
  mdata->modelist = modelist;

/*
  if(!pitchlist)
    for(i=0; i<17; i++)
      pitchlist = g_list_append(pitchlist, pitches[i]);
*/
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

  label = gtk_label_new (_("Select desired key signature"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      label, TRUE, TRUE, 0);
  gtk_widget_show (label);



  combobox = gtk_combo_new ();

  hbox = gtk_hbox_new (FALSE, 0);

  mdata->combobox = combobox;
  mdata->dialog = dialog;
  mdata->pitchcombo = pitchescombo;
  //mdata.radiobutton3 = radiobutton3;

  radiobutton1 = gtk_radio_button_new_with_label (NULL, _("Major"));
  gtk_signal_connect (G_OBJECT (radiobutton1), "clicked",
		      GTK_SIGNAL_FUNC (majorcallback), mdata);
  gtk_box_pack_start (GTK_BOX (hbox), radiobutton1, TRUE, TRUE, 0);
  gtk_widget_show (radiobutton1);

  radiobutton2 = gtk_radio_button_new_with_label
    (gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1)), _("Minor"));
  gtk_signal_connect (G_OBJECT (radiobutton2), "clicked",
		      GTK_SIGNAL_FUNC (minorcallback), mdata);
  gtk_box_pack_start (GTK_BOX (hbox), radiobutton2, TRUE, TRUE, 0);
  gtk_widget_show (radiobutton2);


  radiobutton3 = gtk_radio_button_new_with_label
    (gtk_radio_button_group (GTK_RADIO_BUTTON (radiobutton1)), _("Mode"));

  gtk_signal_connect (G_OBJECT (radiobutton3), "clicked",
		      GTK_SIGNAL_FUNC (modedialog), mdata);
  gtk_box_pack_start (GTK_BOX (hbox), radiobutton3, TRUE, TRUE, 0);
  gtk_widget_show (radiobutton3);

  if (curstaffstruct->keysig.isminor == 2)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton3), TRUE);
      modedialog (NULL, mdata);

    }
  else if (curstaffstruct->keysig.isminor == 1)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton2), TRUE);
      minorcallback (NULL, mdata);
      gtk_entry_set_text
	(GTK_ENTRY (GTK_COMBO (combobox)->entry),
	 minorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET]);

    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (radiobutton1), TRUE);
      majorcallback (NULL, mdata);
      gtk_entry_set_text
	(GTK_ENTRY (GTK_COMBO (combobox)->entry),
	 majorkeys[curstaffstruct->keysig.number + KEYNAME_ARRAY_OFFSET]);

    }
  /* This setting-active will also complete the initialization of
   * the combobox */

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      combobox, TRUE, TRUE, 0);
  gtk_widget_show (combobox);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      pitchescombo, TRUE, TRUE, 0);

  gtk_box_pack_end (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, TRUE, TRUE, 0);
  gtk_widget_show (hbox);

  checkbutton = gtk_check_button_new_with_label (_("Apply to all staves?"));
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox),
		      checkbutton, TRUE, TRUE, 0);
  gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (checkbutton), TRUE);
  gtk_widget_show (checkbutton);


  cbdata->gui = gui;
  cbdata->curstaffstruct = curstaffstruct;
  cbdata->checkbutton = checkbutton;
  cbdata->combobox = combobox;
  cbdata->radiobutton2 = radiobutton2;
  cbdata->radiobutton3 = radiobutton3;
  cbdata->mode = pitchescombo;
  cbdata->mdata = mdata;
  gtk_entry_set_activates_default (GTK_ENTRY (GTK_COMBO (combobox)->entry),
				   TRUE);
  gtk_entry_set_activates_default (GTK_ENTRY
				   (GTK_COMBO (pitchescombo)->entry), TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);

  gtk_widget_grab_focus (combobox);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      if (action == CHANGEINITIAL)
	{
	  set_keysig (NULL, cbdata);
	}
      else
	{
	  if(gui->si->currentobject && ((DenemoObject*)gui->si->currentobject->data)->type==KEYSIG)
	    deleteobject(gui);
	  insert_keysig (NULL, cbdata);
	}
      score_status(gui, TRUE);
      displayhelper (gui);
    }

  gtk_widget_destroy (dialog);

}
