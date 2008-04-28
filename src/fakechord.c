#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "fakechord.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"

struct callbackdata
{
  DenemoGUI *gui;
  GtkWidget *entry;
};
/**
 * Allocate new fakechord structure from the heap
 * and initialise
 *
 */
DenemoObject *
newfakechord (gint baseduration, gint numdots, gchar * figs)
{
  DenemoObject *thefakechord = newchord (baseduration, numdots, 0);

  ((chord *) thefakechord->object)->fakechord = (gpointer) g_string_new (figs);
  ((chord *) thefakechord->object)->is_fakechord = TRUE;
  set_basic_numticks (thefakechord);

  return thefakechord;
}
/**
 * Apply the fakechord to the given chord if it does not already have one
 * otherwise assign to the chords existing fakechord
 */
static void
apply_fakechord (chord * ch, gchar * fig)
{

  if (!ch->fakechord)
    {
      ch->fakechord =
	g_list_append (NULL, newfakechord (ch->baseduration, ch->numdots, fig));
      ch->is_fakechord = FALSE;
    }
  else
    {
      DenemoObject *mud = (DenemoObject *) (((GList *) ch->fakechord)->data);
      chord *mych = (chord *) mud->object;
      GString *mygstr = (GString *) mych->fakechord;
      g_string_assign (mygstr, fig);	/* FIXME g_free(mygstr->str) first ? */
      if (mud->user_string)
	{
	  g_free (mud->user_string);
	  mud->user_string = NULL;
	}
    }
}


/**
 * Get the fakechords if it has one
 *
 */
static GString *
get_fakechord (chord * ch)
{
  DenemoObject *mud;
  chord *mych;
  if (!ch->fakechord)
    {
      g_warning ("No fakechord attached to this note - useless to edit it!");
      return NULL;
    }
  g_assert (!ch->is_fakechord);
  mud = (DenemoObject *) (((GList *) ch->fakechord)->data);
  g_assert (mud);
  mych = (chord *) mud->object;
  g_assert (mych);
  return (GString *) mych->fakechord;
}

void
separate_fakechord_elements (gchar *fakechord, DenemoObject *curObj)
{
  gboolean has_extension = FALSE;
  gboolean has_pedal_bass = FALSE;
  gchar tmp2;
  GString *base = g_string_new("");
  GString *extension = g_string_new("");

  do {
    if ((*fakechord != ':') && (has_extension == FALSE))
      g_string_sprintfa(base, "%c", *fakechord);
    if (*fakechord == ':')
      has_extension = TRUE;
    if (has_extension)
      g_string_sprintfa(extension, "%c", *fakechord);
    if (*fakechord = '/')
      has_pedal_bass = TRUE;// not used!!!
  } while (*++fakechord);
	
  //g_print("\nthe base chord is %s\n", base->str);
  //g_print("\nthe chord extension is %s\n", extension->str);

 
  if (curObj && curObj->type == CHORD) {
    ((chord *) curObj->object)->is_fakechord = TRUE;
    ((chord *) curObj->object)->fakechord = base;
    if (has_extension)
      ((chord *) curObj->object)->fakechord_extension = extension;
    else {
      g_string_free(extension, TRUE);
      ((chord *) curObj->object)->fakechord_extension = NULL;
    }
  }
  
}


/**
 * Function to actually insert a fakechord to an object
 *
 */
void
insertfakechord (GtkWidget * widget, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoGUI *gui = cbdata->gui;
  DenemoScore *si = gui->si;
  static staff_info null_info;
  GString *current_fakechord;
  if (si->currentobject != NULL) {
	  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
	    (DenemoObject *) si->currentobject->data : NULL;
	  gchar *fakechord = (gchar *) gtk_entry_get_text (GTK_ENTRY (cbdata->entry));
	  separate_fakechord_elements(fakechord, curObj);
	  do
	    {
	      if (si->currentobject->next)
		cursorright (gui);
	      else
		measureright (gui);
	      curObj =
		si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
	    }
	  while ((curObj != NULL) && (curObj->type != CHORD));

	  si->has_fakechords = &null_info;
	  gui->changecount++;
  }
  else {
	warningdialog("CLIPPY: There is no object here to attach a fakechord to.  May I suggest creating a staff with the harmonic rhythm in it to attach the fakechords to.");	 
  }
}


/**
 * Creates fakebook style chord entry dialog
 *
 */
void
fakechord_insert (GtkAction * action, DenemoGUI * gui)
{
  GtkWidget *dialog;
  GtkWidget *entry;
  GtkWidget *label;
  GString *temp = g_string_new("");

  DenemoScore *si = gui->si;
  static struct callbackdata cbdata;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  dialog = gtk_dialog_new_with_buttons (_("Insert/Edit Fake Chord"),
					GTK_WINDOW (gui->window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);


  label = gtk_label_new (_("Give Chords followed by Enter key"));

  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), label,
		      TRUE, TRUE, 0);

  entry = gtk_entry_new ();
  cbdata.gui = gui;
  cbdata.entry = entry;
  
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->fakechord)
	{
		temp = g_string_append(temp, (((GString *) ((chord *) curObj->object)->fakechord)->str));
		if (((chord *) curObj->object)->fakechord_extension != NULL)
			temp = g_string_append(temp, (((GString *) ((chord *) curObj->object)->fakechord_extension)->str));
		//printf("\n temp == %s\n",temp->str);
		gtk_entry_set_text (GTK_ENTRY (entry), ((GString *) temp)->str);	
	}
	
 
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), entry,
		      TRUE, TRUE, 0);
  gtk_entry_set_activates_default (GTK_ENTRY (entry), TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_grab_focus (entry);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      insertfakechord (NULL, &cbdata);
      ((DenemoStaff*)si->currentstaff->data)->hasfakechords=TRUE;
      displayhelper (gui);
    }
  gtk_widget_destroy (dialog);
}
