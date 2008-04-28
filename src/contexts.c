/* contexts.cpp
 * Context finders: functions that find the current clef, key, and time
 * signature contexts for the measures being displayed
 *
 * These functions are also invoked when a staff is created
 * 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include "contexts.h"
#include "drawingprims.h"
#include "objops.h"
#include "staffops.h"

/**
 * This function finds the first DenemoObject of type thetype before measure
 * curmeasure. It returns NULL if it was unable to find such a DenemoObject 
 * 
 * @param curmeasure the current measure to search
 * @param thetype type of DenemoObject to find
 * @return the first DenemoObject of type the type
 */
static DenemoObject *
find_context (measurenode * curmeasure, gint thetype)
{
  objnode *curobj;
  if (curmeasure && curmeasure->prev)
    curmeasure = curmeasure->prev;
  else
    return NULL;

  curobj = lastobjnode (curmeasure);
  while (1)
    {
      while (!curobj)
	{			/* Cycle back to preceding measure */
	  if (!curmeasure->prev)
	    return NULL;	/* That is, we've fallen off the beginnig */
	  curmeasure = curmeasure->prev;
	  curobj = lastobjnode (curmeasure);
	}
      if (((DenemoObject *) curobj->data)->type == thetype)
	return (DenemoObject *) curobj->data;
      else
	curobj = curobj->prev;
    }
}



/**
 * This function finds the first DenemoObject of type thetype before si->currentobject
 * It returns NULL if it was unable to find such a DenemoObject 
 * @param si the DenemoScore with si->currentobject and si->currentmeasure set
 * @param thetype type of DenemoObject to find
 * @return the first DenemoObject of type thetype
 */
static DenemoObject *
find_context_of_object (DenemoScore *si, gint thetype)
{
  objnode *curobj = si->currentobject;
  measurenode *curmeasure = si->currentmeasure;
  if(curobj==NULL || curmeasure==NULL)
    return NULL;
  while (curobj)
    {
      if (((DenemoObject *) curobj->data)->type == thetype)
	return (DenemoObject *) curobj->data;
      else
	curobj = curobj->prev;
     if (!curobj)
	{			/* go back to preceding measure */
	  curmeasure = curmeasure->prev;
	  if(curmeasure)
	    curobj = lastobjnode (curmeasure);
	}
    }
  return NULL;
}




/* find the clef in which the currentobject lies */
gint 
find_prevailing_clef(DenemoScore *si) {
DenemoStaff *curstaff = ((DenemoStaff*)si->currentstaff->data);
DenemoObject *obj= find_context_of_object (si, CLEF);
// g_print("prevailing clef %d\n",  obj? ((clef *) obj->object)->type:curstaff->sclef);
return obj? ((clef *) obj->object)->type:
  curstaff->sclef;
}

/**
 * Finds the first occurrences of the clef, keysig and timesig of the 
 * current staff  inserting them into the passed CURSTAFFSTRUCT
 * 
 * @param curstaffstruct the current staff
 * @param si the scoreinfo structure
 * @return none
 */
void
find_leftmost_staffcontext (DenemoStaff * curstaffstruct, DenemoScore * si)
{
  measurenode *leftmeasure = g_list_nth (curstaffstruct->measures,
					 si->leftmeasurenum - 1);
  DenemoObject *obj;

  if ((obj = find_context (leftmeasure, CLEF)))
    curstaffstruct->leftmost_clefcontext = ((clef *) obj->object)->type;
  else
    curstaffstruct->leftmost_clefcontext = curstaffstruct->sclef;
  if ((obj = find_context (leftmeasure, KEYSIG)))
    curstaffstruct->leftmost_keysigcontext = ((keysig *) obj->object)->number;
  else
    curstaffstruct->leftmost_keysigcontext = curstaffstruct->skey;
  /*  dnm_setinitialkeysig  (curstaffstruct, curstaffstruct->leftmost_keysigcontext,
      curstaffstruct->skey_isminor);*/
  initkeyaccs (curstaffstruct->leftmost_keyaccs,
	       curstaffstruct->leftmost_keysigcontext);
  curstaffstruct->leftmost_keywidth =
    draw_key (NULL, NULL, 0, 0, curstaffstruct->leftmost_keysigcontext,
	      0, 0, FALSE);
  if ((obj = find_context (leftmeasure, TIMESIG)))
    {
      curstaffstruct->leftmost_time1context =
	((timesig *) obj->object)->time1;
      curstaffstruct->leftmost_time2context =
	((timesig *) obj->object)->time2;
    }
  else
    {
      curstaffstruct->leftmost_time1context = curstaffstruct->stime1;
      curstaffstruct->leftmost_time2context = curstaffstruct->stime2;
    }
  if ((obj = find_context (leftmeasure, STEMDIRECTIVE)))
    curstaffstruct->leftmost_stem_directive =
      ((stemdirective *) obj->object)->type;
  else
    curstaffstruct->leftmost_stem_directive = DENEMO_STEMBOTH;
}

/**
 * Finds the first occurrences of the clef, keysig and timesig of the 
 * across the entir score
 * @param si the scoreinfo structure
 * @return none
 */
void
find_leftmost_allcontexts (DenemoScore * si)
{
  staffnode *curstaff = si->thescore;

  si->maxkeywidth = G_MININT;
  for (; curstaff; curstaff = curstaff->next)
    {
      find_leftmost_staffcontext ((DenemoStaff *) curstaff->data, si);
      si->maxkeywidth = MAX (si->maxkeywidth,
			     ((DenemoStaff *) curstaff->data)->
			     leftmost_keywidth);
    }
}
