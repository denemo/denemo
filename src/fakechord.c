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
  DenemoProject *gui;
  gchar *string;
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
/* UNUSED
static void
apply_fakechord (chord * ch, gchar * fig)
{

  if (!ch->fakechord)
    {
      ch->fakechord = g_list_append (NULL, newfakechord (ch->baseduration, ch->numdots, fig));
      ch->is_fakechord = FALSE;
    }
  else
    {
      DenemoObject *mud = (DenemoObject *) (((GList *) ch->fakechord)->data);
      chord *mych = (chord *) mud->object;
      GString *mygstr = (GString *) mych->fakechord;
      g_string_assign (mygstr, fig);    // FIXME g_free(mygstr->str) first ?
    }
}
*/

/**
 * Get the fakechords if it has one
 *
 */
/* UNUSED
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
*/
void
separate_fakechord_elements (gchar * fakechord, DenemoObject * curObj)
{
  gboolean has_extension = FALSE;

  GString *base = g_string_new ("");
  GString *extension = g_string_new ("");

  do
    {
      if ((*fakechord != ':') && (has_extension == FALSE))
        g_string_sprintfa (base, "%c", *fakechord);
      if (*fakechord == ':')
        has_extension = TRUE;
      if (has_extension)
        g_string_sprintfa (extension, "%c", *fakechord);
      //if (*fakechord == '/')
      //  has_pedal_bass = TRUE;  // not used!!!
    }
  while (*++fakechord);

  //g_print("\nthe base chord is %s\n", base->str);
  //g_print("\nthe chord extension is %s\n", extension->str);


  if (curObj && curObj->type == CHORD)
    {
      ((chord *) curObj->object)->is_fakechord = TRUE;
      ((chord *) curObj->object)->fakechord = base;
      //if (has_extension)
      //((chord *) curObj->object)->fakechord_extension = extension;
      //else {
      g_string_free (extension, TRUE);
      //((chord *) curObj->object)->fakechord_extension = NULL;
      //}
    }

}


/**
 * Function to actually insert a fakechord to an object
 *
 */
gboolean
insertfakechord (GtkWidget * widget, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoProject *gui = cbdata->gui;
  DenemoScore *si = gui->si;
  if (cbdata->string == NULL)
    return FALSE;
  if (si->currentobject != NULL)
    {
      DenemoObject *curObj = (DenemoObject *) si->currentobject->data;
      //gchar *fakechord = cbdata->string;
      //separate_fakechord_elements(fakechord, curObj);
      if (((chord *) curObj->object)->fakechord)
        g_string_assign (((chord *) curObj->object)->fakechord, cbdata->string);
      else
        ((chord *) curObj->object)->fakechord = g_string_new (cbdata->string);
      do
        {
          if (si->currentobject->next)
            movecursorright (NULL, NULL);
          else if (gui->si->currentmeasure->next)
            movetomeasureright (NULL, NULL);
          else
            break;
          curObj = si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
        }
      while ((curObj != NULL) && (curObj->type != CHORD));
      if (!si->has_fakechords)
        {
          si->has_fakechords = (gpointer) TRUE;
          signal_structural_change (gui);
        }
      score_status (gui, TRUE);
      return TRUE;
    }
  else
    {
      warningdialog (_("There is no object here to attach a fakechord to."));
    }
  return FALSE;
}


/**
 * Creates fakebook style chord entry dialog
 *
 */
void
fakechord_insert (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  gchar *string;
  gchar *PreValue = NULL;
  GString *temp = g_string_new ("");
  DenemoScore *si = gui->si;
  static struct callbackdata cbdata;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;

  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->fakechord)
    {

      PreValue = (((GString *) ((chord *) curObj->object)->fakechord)->str);

    }



  string = string_dialog_entry (gui, "Insert/Edit Fake Chord", "Give Chords followed by Enter key", PreValue);

  cbdata.gui = gui;
  cbdata.string = string;

  if (string)
    {
      if (insertfakechord (NULL, &cbdata))
        ((DenemoStaff *) si->currentstaff->data)->hasfakechords = TRUE;
      displayhelper (gui);
    }
  g_string_free (temp, TRUE);
  g_free (string);
}
