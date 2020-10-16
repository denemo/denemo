/* figure.c
 *
 * Functions for the manipulations of figured basses
 *
 * for Denemo, a gtk+ frontend for GNU Lilypond
 * (c) 2003-2006 Richard Shann
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "command/chord.h"
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include "command/figure.h"
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
 * Function to actually insert a figure to an object
 *
 */
static void
insertfigure (gboolean filter, gpointer data)
{
  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoProject *gui = cbdata->gui;
  DenemoMovement *si = gui->movement;
  gchar filter_sep = filter ? '1' : '|';
  gchar filter_spc = filter ? '*' : ' ';
  if (si->measurewidth == DENEMO_INITIAL_MEASURE_WIDTH)
    si->measurewidth = DENEMO_INITIAL_MEASURE_WIDTH * 2;

  if (si->currentobject != NULL)
    {
      DenemoObject *curObj = (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
      gchar *figure = (cbdata->string);
      if (strlen (figure) < 1)
        figure = "_";           /* in case user deleted the figure to yield <> */
      /* translate the input somewhat */
      GString *f = g_string_new ("");
      gchar *c = figure;
      for (c = figure; *c; c++)
        {
          if (*c == '+')
            {
              if (c == figure || *(c - 1) == ' ' || *(c - 1) == filter_spc || *(c - 1) == '|' || *(c - 1) == filter_sep)
                g_string_append (f, "_+");
              else
                g_string_append (f, "+");
            }
          else if (*c == '-')
            {
              if (c == figure || *(c - 1) == ' ' || *(c - 1) == filter_spc || *(c - 1) == '|' || *(c - 1) == filter_sep)
                g_string_append (f, "_-");
              else
                g_string_append (f, "-");
            }
          else if (*c == filter_sep)
            g_string_append (f, "|");
          else
            {
              if (*c == filter_spc)
                g_string_append (f, " ");
              else
                g_string_append_c (f, *c);
            }
        }

	  gchar last='\0';
	  for (c=f->str;*c;c++)
			{
			  if (*c==' ')
						continue;
			  if (*c=='|')
				{
					if ((last=='\0') || (last=='|') || (*(c+1)=='\0'))
						 {
							 last = *c;
							*c = ' ';//remove doubled, initial or final | characters
							continue;
						 }
				}
			 last = *c;
			}
      if (curObj && curObj->type == CHORD)
        ((chord *) curObj->object)->is_figure = TRUE;
            if(((chord *) curObj->object)->figure)
                g_string_free (((chord *) curObj->object)->figure, TRUE);
      ((chord *) curObj->object)->figure = g_string_new (f->str);
      g_string_free (f, TRUE);
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


      si->has_figures = (gpointer) TRUE;        //&null_info;
      score_status (gui, TRUE);
    }                           // if currentobject not null
  else
    {
      warningdialog (_("No current object to attach a figure to"));
    }
}




void
delete_figured_bass (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *thestaff = (DenemoStaff *) gui->movement->currentstaff->data;
  if (confirm ("Figured Bass Deletion", "Delete all figured bass markings from this staff?"))
    {
      thestaff->hasfigures = FALSE;
      gui->movement->has_figures = FALSE;
      signal_structural_change (gui);
      measurenode *curmeasure;
      for (curmeasure = thestaff->themeasures; curmeasure; curmeasure = curmeasure->next)
        {
          objnode *curobj;
          for (curobj = ((DenemoMeasure*)curmeasure->data)->objects; curobj; curobj = curobj->next)
            {
              DenemoObject *curObj = (DenemoObject *) curobj->data;
              if (curObj && curObj->type == CHORD)
                {
                  GString *s = ((chord *) curObj->object)->figure;
                  if (s)
                    g_string_free (s, TRUE);
                  ((chord *) curObj->object)->figure = NULL;
                }
            }
        }
    }
}

void
hide_figured_bass (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *thestaff = (DenemoStaff *) gui->movement->currentstaff->data;
  if (thestaff->hasfigures)
    signal_structural_change (gui);
  thestaff->hasfigures = FALSE;
}

/* turn on figured bass if any figures are present */
void
show_figured_bass (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoStaff *thestaff = (DenemoStaff *) gui->movement->currentstaff->data;
  measurenode *curmeasure;
  if (!thestaff->hasfigures)
      signal_structural_change (gui);
  for (curmeasure = thestaff->themeasures; curmeasure; curmeasure = curmeasure->next)
    {
      objnode *curobj;
      for (curobj = ((DenemoMeasure*)curmeasure->data)->objects; curobj; curobj = curobj->next)
        {
          DenemoObject *curObj = (DenemoObject *) curobj->data;
          if (curObj && curObj->type == CHORD)
            {
              GString *s = ((chord *) curObj->object)->figure;
              if (s)
                thestaff->hasfigures = TRUE;
            }
        }
    }
}

/**
 * Creates figured bass entry dialog
 * if action==NULL it is a call from Scheme with param->string holding "query" or "figures n..." to retrieve or set the figures
 */
void
figure_insert (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  gchar *string = NULL;
  gchar *PreValue = NULL;
  DenemoMovement *si = gui->movement;
  static struct callbackdata cbdata;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
  if (curObj==NULL) return;
  if (curObj->type != CHORD) return;
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->figure)
    PreValue = ((GString *) ((chord *) curObj->object)->figure)->str;

  if (!action && param && param->string)
    {                           //Called from scheme, could be "query" or setting the values
      GString *values = param->string;
      gchar *str;
      gint i;
      if (!strcmp (values->str, "query"))
        {
          if (PreValue && *PreValue && strcmp (PreValue, "_"))
            {                   //there is a figure, other than the "no figure" sign
              param->status = TRUE;
              g_string_assign (param->string, PreValue);
            }
          else                  //the no figure case leave status FALSE and return NULL
            g_string_assign (param->string, "");
        }
      else                      //detect the string "figures" followed by a separator character and then the figures
        for (i = 0; i < values->len; i += strlen (values->str + i) + 1)
          {
            if ((str = g_strstr_len (values->str + i, strlen (values->str + i), "figures")))
              {
                string = g_strdup (str + strlen ("figures") + 1);
                break;
              }
          }
    }
  else
    {                           //interactive
      string = string_dialog_entry (gui, "Insert/Edit Figure", "Give figures followed by Enter key", PreValue);
    }
  cbdata.gui = gui;
  cbdata.string = string;

  if (string)
    {
      insertfigure (action != NULL, &cbdata);
      //also \set Staff.useBassFigureExtenders = ##t
      if (!((DenemoStaff *) si->currentstaff->data)->hasfigures)
        {
          signal_structural_change (gui);
          ((DenemoStaff *) si->currentstaff->data)->hasfigures = TRUE;
        }
      displayhelper (gui);
    }
  g_free (string);
}
