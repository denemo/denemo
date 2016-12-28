#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "command/chord.h"
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include "command/fakechord.h"
#include "ui/dialogs.h"
#include "display/draw.h"
#include "command/object.h"
#include "command/staff.h"
#include "core/utils.h"

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

  //g_debug("\nthe base chord is %s\n", base->str);
  //g_debug("\nthe chord extension is %s\n", extension->str);


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
  DenemoMovement *si = gui->movement;
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
          else if (gui->movement->currentmeasure->next)
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
void
delete_fakechords (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *thestaff = (DenemoStaff *) gui->movement->currentstaff->data;
  if (confirm (_("Chord Symbol Deletion"), _("Delete all Chord Symbols from this staff?")))
    {
      thestaff->hasfakechords = FALSE;
      gui->movement->has_fakechords = FALSE;

      score_status (gui, TRUE);
      measurenode *curmeasure;
      for (curmeasure = thestaff->themeasures; curmeasure; curmeasure = curmeasure->next)
        {
          objnode *curobj;
          for (curobj = ((DenemoMeasure*)curmeasure->data)->objects; curobj; curobj = curobj->next)
            {
              DenemoObject *curObj = (DenemoObject *) curobj->data;
              if (curObj && curObj->type == CHORD)
                {
                  GString *s = ((chord *) curObj->object)->fakechord;
                  if (s)
                    g_string_free (s, TRUE);
                  ((chord *) curObj->object)->fakechord = NULL;
                  ((chord *) curObj->object)->is_fakechord = 0;
                }
            }
        }
    }
}

/**
 * Creates fakebook style chord entry dialog
 *
 */
void
fakechord_insert (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  gchar *string;
  gchar *PreValue = NULL;
  GString *temp = g_string_new ("");
  DenemoMovement *si = gui->movement;
  static struct callbackdata cbdata;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;

  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->fakechord)
    {

      PreValue = (((GString *) ((chord *) curObj->object)->fakechord)->str);

    }



  string = string_dialog_entry (gui, _("Insert/Edit Chord Symbol"), _("Give Chord(s) in LilyPond Notation\nE.g. c:m7 aes:aug7\n(Separate chord changes with spaces)"), PreValue);

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
