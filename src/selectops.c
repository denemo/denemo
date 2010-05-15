/**
 * selectops.c
 * operations for selecting, cutting, copying, and pasting music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee 
 *
 * TODO: Fix Undo/Redo
 */

#include <string.h>
#include "calculatepositions.h"
#include "commandfuncs.h"
#include <denemo/denemo.h>
#include "draw.h"
#include "objops.h"
#include "measureops.h"
#include "selectops.h"
#include "staffops.h"
#include "prefops.h"
/*For save selection function*/
#include "utils.h"
#define DEBUG 1
/**
 * The copy buffer is a GList of objnode *s -- at first, I was going
 * to use staffnode *s, measurenode *s, and then objnode *s, but I
 * realized that'd be overkill and just complicate the implementation
 * unnecessarily.
 *
 * Each item in the copybuffer list corresponds to the stuff in
 * the buffer on each staff.  
 */

static void
undo (DenemoGUI * gui);
static void
redo (DenemoGUI * gui);
static GList *copybuffer = NULL; // this is a list one for each staff of lists of objects

static gint staffsinbuffer = 0;
static gint measurebreaksinbuffer = 0;

static GList *clipboards = NULL;

static GList *clone_obj_list(GList *g) {
  GList *ret=NULL;
  do {
    ret = g_list_append(ret, dnm_clone_object(g->data));
  } while((g=g->next));
  return ret;
}
void push_clipboard (void) {
  GList *thecopy = NULL;
  GList *g;
  for(g=copybuffer;g;g=g->next) {
    thecopy = g_list_append(thecopy, clone_obj_list(g->data));
  }
  clipboards = g_list_prepend(clipboards, thecopy);
}

gboolean pop_clipboard(void) {
  GList *thecopy = NULL;
  if(clipboards==NULL)
    return FALSE;
  thecopy = clipboards->data;
  clipboards = g_list_remove(clipboards, thecopy);
  clearbuffer();
  copybuffer = thecopy;
  return TRUE;
}


/**
 *  sets current object to the given cursor position
 * 
 */
void
setcurrentobject (DenemoScore * si, gint cursorpos)
{

  g_debug ("Set Current Object Cursor pos %d\n", cursorpos);

  si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				  cursorpos);
  //g_assert (si->currentobject != NULL);
}

/**
 *  clearbuffer
 *  Clears the copybuffer of data
 *  Arguments - None
 *  return - none
 */
void
clearbuffer ()
{
  g_list_foreach (copybuffer, freeobjlist, NULL);
  g_list_free (copybuffer);
  copybuffer = NULL;
  staffsinbuffer = 0;
  measurebreaksinbuffer = 0;
}


/**
 *  saveselection 
 *  Saves the current selection to a given file
 * 
 *  @param si pointer to the score information structure
 *  @return none
 */
void
saveselection (DenemoScore * si)
{
  if (si->markstaffnum == 0)	/* Indicator that there's no selection.  */
    return;

  clearbuffer ();

  staffsinbuffer = si->laststaffmarked - si->firststaffmarked + 1;

  copytobuffer (si);
  si->savebuffer = copybuffer;
  /* Test code for save selection
     FILE *fp;
     GString *file = NULL; 
     file = g_string_new(locatedotdenemo());
     g_string_append(file, "/denemoanalysispattern");

     filesaveselection(file->str, si);
     clearbuffer ();
     g_free(file);
   */
}

/**
 *   copytobuffer
 *   Copies selection to the copybuffer
 *   
 *   @param si pointer to the score information structure
 */
void
copytobuffer (DenemoScore * si)
{
  staffnode *curstaff;
  measurenode *curmeasure;
  objnode *curobj;
  objnode *theobjs;
  DenemoObject *clonedobject;
  gint i, j, k;

  if (si->markstaffnum == 0)	/* Indicator that there's no selection.  */
    return;

  clearbuffer ();

  staffsinbuffer = si->laststaffmarked - si->firststaffmarked + 1;
  g_debug ("No staffs in copybuffer %d\n", staffsinbuffer);
  /* Staff loop.  */
  for (i = si->firststaffmarked, curstaff = g_list_nth (si->thescore, i - 1);
       curstaff && i <= si->laststaffmarked; curstaff = curstaff->next, i++)
    {
      if (((DenemoStaff *) curstaff->data)->is_parasite)
	continue;
      /* Initialize first ->data for copybuffer to NULL.  */
      theobjs = NULL;
      /* Measure loop.  */
      for (j = si->firstmeasuremarked, k = si->firstobjmarked,
	   curmeasure = g_list_nth (firstmeasurenode (curstaff), j - 1);
	   curmeasure && j <= si->lastmeasuremarked;
	   curmeasure = curmeasure->next, j++)
	{
	  for (curobj = g_list_nth ((objnode *) curmeasure->data, k);
	       /* cursor_x is 0-indexed */
	       curobj && (j < si->lastmeasuremarked
			  || k <= si->lastobjmarked);
	       curobj = curobj->next, k++)
	    {
	      clonedobject = dnm_clone_object ((DenemoObject *) curobj->data);
	      theobjs = g_list_append (theobjs, clonedobject);
	    }			/* End object loop */
	  g_debug ("cloned objects on staff \n");
	  
	  if (j < si->lastmeasuremarked || k < si->lastobjmarked)
	    {
	      if(!((j==si->lastmeasuremarked))) {
		g_debug ("Insert measurebreak obj in copybuffer");
		/* ???outdated comment??? That is, there's another measure, the cursor is in appending
		   position, or the selection spans multiple staffs, in which 
		   case another measure boundary should be added.  */
		theobjs = g_list_append (theobjs, newmeasurebreakobject ());
		if (i == si->firststaffmarked)
		  measurebreaksinbuffer++;
	      }
	    }
	  k = 0;		/* Set it for next run through object loop */
	  
	}			/* End measure loop */
      if ((staffsinbuffer > 1) && (i < si->laststaffmarked))
	{
	  theobjs = g_list_append (theobjs, newstaffbreakobject ());
	  g_debug ("Inserting Staffbreak object in copybuffer");
	}
      copybuffer = g_list_append (copybuffer, theobjs);
    }				/* End staff loop */
}


/**
 *  cuttobuffer
 *  Cuts selection to the copybuffer, removing it from the score
 *
 *  @param si pointer to score information structure
 */
static void
cuttobuffer (DenemoScore * si, gboolean copyfirst)
{
  staffnode *curstaff;
  measurenode *curmeasure;
  objnode *tempobj;
  gint i, jcounter, //jcounter is marking the position of the measure currently being cleared I think
    max;
  if (!si->markstaffnum)
    return;
  if(copyfirst)
    copytobuffer (si);
  gint staffs_removed_measures = 0;// a count of removed measures in the case where multiple staffs are involved
  gint lmeasurebreaksinbuffer = si->lastmeasuremarked - si->firstmeasuremarked;
  gint lstaffsinbuffer = si->laststaffmarked - si->firststaffmarked + 1;
  if(copyfirst) {
    g_assert( lmeasurebreaksinbuffer == measurebreaksinbuffer);
    g_assert(lstaffsinbuffer == staffsinbuffer);
  }
  if (lstaffsinbuffer == 1)
    {
      /* Just a single staff is a special case, again.  */
      jcounter = si->firstmeasuremarked; //currently clearing stuff from the firstmeasuremarked
      curmeasure = g_list_nth (firstmeasurenode (si->currentstaff), jcounter - 1);

      /* Clear the relevant part of the first measure selected */
      if (lmeasurebreaksinbuffer)
	max = G_MAXINT;
      else
	max = si->lastobjmarked;
      for (i = si->firstobjmarked;
	   ((tempobj = g_list_nth ((objnode *) curmeasure->data,
				   si->firstobjmarked)) && i <= max); i++)
	{
	  curmeasure->data =
	    g_list_remove_link ((objnode *) curmeasure->data, tempobj);
	  freeobject ((DenemoObject *) tempobj->data);
	  g_list_free_1 (tempobj);
	}
      jcounter++; //move on to the second measure being cleared
      curmeasure = curmeasure->next;

      if (!si->thescore->next)
	{
	  /* That is, the score has only this one staff
	   remove the (whole) measures between the first and last - which may be partial.*/
	  if (lmeasurebreaksinbuffer - 1 > 0)
	    {
	      curmeasure =
		removemeasures (si, jcounter - 1, lmeasurebreaksinbuffer - 1, TRUE);
	      jcounter += lmeasurebreaksinbuffer - 1;// increased by the number of measures *between* first and last marked
	    }
	}
      else
	for (; curmeasure && jcounter < si->lastmeasuremarked;
	     curmeasure = curmeasure->next, jcounter++)
	  {
	    freeobjlist (curmeasure->data, NULL);
	    curmeasure->data = NULL;
	  }
      /* Now clear the relevant part of the last measure selected */
      if (curmeasure && (jcounter <= si->lastmeasuremarked))
	{
	  for (i = 0; curmeasure->data && i <= si->lastobjmarked; i++)
	    {
	      tempobj = (objnode *) curmeasure->data;
	      curmeasure->data =
		g_list_remove_link ((objnode *) curmeasure->data, tempobj);
	      freeobject ((DenemoObject *) tempobj->data);
	      g_list_free_1 (tempobj);
	    }
	  /* And delete it, if the measure's been cleared and there's only
	     one staff.  */
#if 0
	  if (!curmeasure->data && !si->thescore->next)
	    removemeasures (si, jcounter - 1, 1, TRUE);//WRONG the other measures have been removed, so jcounter no longer indexes anything in the staff!
#else
	  if (!curmeasure->data && !si->thescore->next)
	    removemeasures (si, g_list_position(firstmeasurenode(si->currentstaff), curmeasure), 1, TRUE);
#endif



	}
      showwhichaccidentalswholestaff ((DenemoStaff *) si->currentstaff->data);
      beamsandstemdirswholestaff ((DenemoStaff *) si->currentstaff->data);
    } // end of single staff
  else
    {				/* Multiple staff selection */
      if (lstaffsinbuffer == (gint) (g_list_length (si->thescore)))
	{
	  /* Every staff was part of the selection */
	  if (lmeasurebreaksinbuffer > 0)
	    {
	      
	      removemeasures (si, si->firstmeasuremarked - 1,
			    lmeasurebreaksinbuffer+1, TRUE);
	      staffs_removed_measures = lmeasurebreaksinbuffer;
	    }
	  else
	    for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
	      {
		curmeasure = g_list_nth (firstmeasurenode (curstaff),  si->firstmeasuremarked-1);
		freeobjlist (curmeasure->data, NULL);
		curmeasure->data = NULL;
		
		showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
		beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
	      }
	}
      else
	{
	  /* Staff loop */
	  for (i = si->firststaffmarked,
	       curstaff = g_list_nth (si->thescore, i - 1);
	       curstaff && i <= si->laststaffmarked;
	       curstaff = curstaff->next, i++)
	    {
	      if (((DenemoStaff *) curstaff->data)->is_parasite)
		continue;
	      /* Measure loop */
	      for (jcounter = si->firstmeasuremarked,
		   curmeasure = g_list_nth (firstmeasurenode (curstaff),
					    jcounter - 1);
		   curmeasure && jcounter <= si->lastmeasuremarked;
		   curmeasure = curmeasure->next, jcounter++)
		{
		  freeobjlist (curmeasure->data, NULL);
		  curmeasure->data = NULL;
		}
	      showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
	      beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
	    }
	}
    }
  si->firststaffmarked = si->markstaffnum = 0;//only the latter is needed, but there was some confusion at one time...
  /* And set some currents. This would probably be better to split off
   * into a more-generalized version of setcurrents or something;
   * what's here is more-or-less copied from dnm_deleteobject in
   * commandfuncs */

  si->currentmeasurenum = si->firstmeasuremarked - (staffs_removed_measures?1:0);

  if(si->currentmeasurenum<1) {
    si->currentmeasurenum = 1;
  }


  si->currentmeasure = g_list_nth (firstmeasurenode (si->currentstaff),
				   si->currentmeasurenum - 1);
#if 0
  if(si->currentmeasure==NULL)
    si->currentmeasure = g_list_last (firstmeasurenode (si->currentstaff));
#endif



  si->cursor_x = si->firstobjmarked;
  if (si->cursor_x <
      (gint) (g_list_length ((objnode *) si->currentmeasure->data)))
    {
      si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				      si->cursor_x);
      si->cursor_appending = FALSE;
    }
  else
    {
      si->currentobject = g_list_last ((objnode *) si->currentmeasure->data);
      si->cursor_appending = TRUE;
    }
  //if clef has been deleted we need to re-validate leftmost clef - would only apply if the clef being deleted was off the left side of screen - some sort of scripting scenario...
  find_leftmost_allcontexts(si);

  
  isoffleftside(Denemo.gui);
  isoffrightside(Denemo.gui);

  /*   isoffleftside;  */
  /*find_xes_in_all_measures (si);
    nudgerightward (Denemo.gui);
    gtk_widget_draw (si->scorearea, NULL);  */
}



DenemoObjType get_clip_obj_type(gint m, gint n) {
  if(copybuffer==NULL)
    return -1;
  GList *stafflist = g_list_nth(copybuffer, m);
  if(stafflist==NULL)
    return -1;
  GList *curbufferobj = g_list_nth(stafflist->data, n);
  if(curbufferobj==NULL || curbufferobj->data==NULL )
    return -1;
  return ((DenemoObject*)(curbufferobj->data))->type;
}

// insert the nth object from the copybuffer into music at the cursor position
// return TRUE if inserted
gboolean insert_clip_obj(gint m, gint n) {
  DenemoScore *si = Denemo.gui->si;
  staffnode *curstaff = si->currentstaff;
  if(copybuffer==NULL)
    return FALSE;
  GList *stafflist = g_list_nth(copybuffer, m);
  if(stafflist==NULL)
    return FALSE;
  objnode *curbufferobj = g_list_nth(stafflist->data, n);
  if(curbufferobj==NULL)
    return FALSE;
  DenemoObject *clonedobj; 
  DenemoObject *curobj = (DenemoObject*)curbufferobj->data; 
  clonedobj = dnm_clone_object (curobj);
  
  clonedobj->starttick = (si->currentobject?
			     ((DenemoObject *)si->currentobject->data)->starttickofnextnote: 0);
  Denemo.gui->si->currentmeasure->data =
    g_list_insert ((objnode *)si->currentmeasure->data,
		   clonedobj, si->cursor_x);
  si->cursor_x++;
  if (si->cursor_appending)
    si->currentobject = g_list_last ((objnode *) si->currentmeasure->data);
  else
    si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				    si->cursor_x);
  beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
  find_xes_in_all_measures (si);
  return TRUE;
}
/**
 * pastefrombuffer
 * Pastes the current buffer to the score
 *

 *
 * The updates that are done towards the bottom of this function -
 * beamsandstemdirswholestaff, find_xes_in_all_measures, etc. - are
 * too much gruntwork and are inefficient in terms of everything
 * but additional lines-of-code required for implementation. 
 * return FALSE if no pasting was done.
 */

static gboolean
pastefrombuffer (void)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  staffnode *curstaff;
  measurenode *curmeasure;
  GList *curbuffernode = copybuffer;
  objnode *curbufferobj;
  gint initialinsertat;
  DenemoObject *clonedobject;
  gint staffs_used = 0;//number of staffs we have pasted into after the first
  gint i, j;
  gint measuretoaddat = si->currentmeasurenum;
  if((staffsinbuffer>1) || (measurebreaksinbuffer>0)) {
    /* g_print("si->appending=%d si->cursoroffend=%d curobjnot1st=%d\n", si->cursor_appending, si->cursoroffend, 
       (si->currentobject!=si->currentmeasure->data));*/

    if((!si->cursor_appending) && (si->currentobject!=si->currentmeasure->data))
      return FALSE;
    if(si->currentobject) {
      //g_print("Adding %d measures at %d\n", measurebreaksinbuffer+(staffsinbuffer==1), si->currentmeasurenum);
      if(si->cursor_appending) {
	addmeasures (si, si->currentmeasurenum, measurebreaksinbuffer+(staffsinbuffer==1), (staffsinbuffer>1));
	measureright(NULL);measuretoaddat++;//Better check measureright worked
      } else {
	addmeasures (si, si->currentmeasurenum-1, measurebreaksinbuffer+(staffsinbuffer==1), (staffsinbuffer>1));
      }
      setcurrents (gui->si);
    }
    //currentobject is NULL, currentmeasure is first of added measures
  }

  /* All right. Any necessary measures have been inserted - now paste away */

  if (staffsinbuffer == 1)
    initialinsertat = si->cursor_x;
  else
    initialinsertat = 0;

  g_debug ("Insert At position %d\n", initialinsertat);

  for (curstaff = si->currentstaff; curstaff && curbuffernode;
       curstaff? curstaff = si->currentstaff:0, curbuffernode = curbuffernode->next)
    {

      g_debug ("Current staff %x, Current Staff Next %x,"
	       " CurBuf %x, CurBuf Next %x\n",
	       curstaff, curstaff->next, curbuffernode, curbuffernode->next);

      //gint prevailing_clef = find_prevailing_clef(si);

      curmeasure = g_list_nth (firstmeasurenode (curstaff),
			       si->currentmeasurenum - 1);

      
      for (curbufferobj = (objnode *) curbuffernode->data;
	   curbufferobj && curmeasure; curbufferobj = curbufferobj->next)
	{
	  DenemoObject *curobj = (DenemoObject *) curbufferobj->data;
	  if (curobj->type == STAFFBREAK)
	    {

	     break;
	    }
	  else if (curobj->type ==
		   MEASUREBREAK)
	    {
	      /*Do nothing as we will not insert a new barline at 
		this point. It is done automatically */
	      g_debug("Have measurebreak object\n");
	    
	    }
	  else
	    {
	      g_debug("Paste: Cursor Position %d\n", si->cursor_x);
	      clonedobject =
		dnm_clone_object (curobj);
	      
	      clonedobject->starttick = (si->currentobject?
					 ((DenemoObject *)si->currentobject->data)->starttickofnextnote: 0);//guess
	      
	      g_debug ("offend %d start of next note %d\n", 
		       si->cursoroffend, clonedobject->starttick);
	      insertion_point (si);
	      object_insert(gui, clonedobject);

	      si->cursoroffend = (si->currentobject?
		((DenemoObject *)si->currentobject->data)->starttickofnextnote
				  >= (WHOLE_NUMTICKS * si->cursortime1 / si->cursortime2): 0);   // guess  


	    } //not a staff break
	}			/* End bufferobj loop */
      fixnoteheights(curstaff->data);
      showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
      beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
      si->currentmeasurenum =  measuretoaddat;
      curstaff = si->currentstaff->next;
      if(staffsinbuffer>1) {
	DenemoScriptParam param; 
	if(!staffdown(&param))
	  break;//FIXME wrap this function up to be called from C
	staffs_used++;
      }
      setcurrents (gui->si);
    } /* End staff loop */
  // g_print("check %d against %d\n", staffsinbuffer, staffs_used);
  while(staffs_used--)
    {
      DenemoScriptParam param; 
      staffup(&param);
    }
  si->currentmeasure = g_list_nth (firstmeasurenode (si->currentstaff),
				   si->currentmeasurenum - 1);
  si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				  initialinsertat);
  if (!si->currentobject)
    /* Yah. There wasn't anything in the buffer after all. */
    si->cursor_appending = TRUE;
  else
    si->cursor_appending = FALSE;
  find_xes_in_all_measures (si);

  g_debug ("End of Paste Cursor X: %d\n", si->cursor_x);
  return TRUE;
}


DenemoObject *get_mark_object(void){
  DenemoGUI * gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if(!si->markstaffnum)
    return NULL;
  staffnode *curstaff = g_list_nth (si->thescore, si->firststaffmarked - 1);
  DenemoStaff *firststaff =  (DenemoStaff *) curstaff->data;
  measurenode *firstmeasure = g_list_nth (firststaff->measures,  si->firstmeasuremarked - 1);
  objnode *firstobj = g_list_nth (firstmeasure->data, si->firstobjmarked);
  //g_print("First %d\n",  si->firstobjmarked);
  return firstobj? ((DenemoObject *)firstobj->data):NULL;
}
DenemoObject *get_point_object(void){
  DenemoGUI * gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if(!si->markstaffnum)
    return NULL;
  staffnode *curstaff = g_list_nth (si->thescore, si->laststaffmarked - 1);
  DenemoStaff *laststaff =  (DenemoStaff *) curstaff->data;
  measurenode *lastmeasure = g_list_nth (laststaff->measures,  si->lastmeasuremarked - 1);
  objnode *lastobj = g_list_nth (lastmeasure->data, si->lastobjmarked);
  return lastobj? ((DenemoObject *)lastobj->data):NULL;
}



/**
 *  setmark
 *  Sets the current mark for the start of the buffer
 *
 *  @param gui pointer to the DenemoGUI structure
 */
void
set_mark (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  si->markstaffnum = si->currentstaffnum;
  si->markmeasurenum = si->currentmeasurenum;
  si->markcursor_x = si->cursor_x;
  calcmarkboundaries (si);
}

/**
 * unset_mark
 * Remove the current mark
 *
 * @param gui pointer to the DenemoGUI structure
 */
void
unset_mark (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  si->markstaffnum = 0;
  calcmarkboundaries (si);
}


gboolean in_selection(DenemoScore *si) {
  if(si->markstaffnum) {
    if(si->currentstaffnum >= si->firststaffmarked &&
       si->currentstaffnum <= si->laststaffmarked) {
      if(si->currentmeasurenum == si->firstmeasuremarked) {
	if (si->currentmeasurenum == si->lastmeasuremarked) {
	  if((si->cursor_x >= si->firstobjmarked) &&
	     (si->cursor_x <= si->lastobjmarked))
	    return TRUE;
	  else return FALSE;
	}
	if (si->currentmeasurenum < si->lastmeasuremarked){
	  if(si->cursor_x >= si->firstobjmarked)
	    return TRUE;
	  return FALSE;
	}
      }
      if(si->currentmeasurenum > si->firstmeasuremarked) {
	if (si->currentmeasurenum == si->lastmeasuremarked) {
	  if((si->cursor_x <= si->lastobjmarked))
	    return TRUE;
	  else return FALSE;
	}
	if (si->currentmeasurenum < si->lastmeasuremarked)
	  return TRUE;
      }
    }
  }
  return FALSE;  
}

/* save/restore selection */
static      gint firststaff;
static      gint laststaff;
static      gint firstobj;
static      gint lastobj;
static      gint firstmeasure;
static      gint lastmeasure;

void save_selection(DenemoScore *si) {
  firststaff = si->firststaffmarked;
  laststaff = si->laststaffmarked;
  firstobj = si->firstobjmarked;
  lastobj = si->lastobjmarked;
  firstmeasure =si->firstmeasuremarked;
  lastmeasure =si->lastmeasuremarked;
}
void restore_selection(DenemoScore *si) {
  si->firststaffmarked = firststaff;
  si->laststaffmarked = laststaff;
  si->firstobjmarked  = firstobj;
  si->lastobjmarked = lastobj;
  si->firstmeasuremarked = firstmeasure;
  si->lastmeasuremarked = lastmeasure;
}



/**
 * goto_mark
 * goto the current mark without changing the selection
 *
 * 
 */
void
goto_mark (GtkAction *action, DenemoScriptParam *param)
{
  DenemoScriptParam local_param;
  local_param.status = TRUE;
  DenemoScore *si = Denemo.gui->si;
  if(!action)
    ((DenemoScriptParam *)param)->status = si->markstaffnum;
  else
    param = &local_param;
  if(si->markstaffnum){
    save_selection(si);
    set_currentmeasurenum (Denemo.gui, si->markmeasurenum);
    set_currentstaffnum (Denemo.gui,si->markstaffnum);
    while(si->cursor_x < si->markcursor_x && param->status)
      cursorright(param);
    restore_selection(si);
    if(!action)
      displayhelper(Denemo.gui);
  } 
}

/**
 * goto_selection_start
 * move cursor the first object in the selection without changing the selection
 *
 * 
 */
void
goto_selection_start (GtkAction *action, DenemoScriptParam *param)
{
  DenemoScore *si = Denemo.gui->si;
  if(!action)
    ((DenemoScriptParam *)param)->status = si->markstaffnum;
  if(si->markstaffnum){
    gint first = si->firstobjmarked;
    save_selection(si);
    set_currentmeasurenum (Denemo.gui, si->firstmeasuremarked);
    set_currentstaffnum (Denemo.gui,si->firststaffmarked);
    while(si->cursor_x < first)
      cursorright(param);
    restore_selection(si);
    if(!action)
      displayhelper(Denemo.gui);
  } 
}




static GSList *positions=NULL;
DenemoPosition *pop_position(void) {
  DenemoPosition *pos;
  if(positions) {
    pos = positions->data;
    positions = g_slist_delete_link(positions, positions);
    return pos;
  }
  return NULL;
}

void push_position(void) {
  DenemoScore *si = Denemo.gui->si;
  DenemoPosition *pos = ( DenemoPosition *)g_malloc(sizeof(DenemoPosition));
  pos->movement =  g_list_index(Denemo.gui->movements, si)+1;
  pos->staff =  si->currentstaffnum;
  pos->measure = si->currentmeasurenum;
  pos->object =  si->currentobject?si->cursor_x+1:0;
  if(pos->movement)
     positions = g_slist_prepend(positions, pos);
  else
    g_free(pos);
  //g_print("%d %d %d %d \n", pos->movement, pos->staff, pos->measure, pos->object);
}


/**
 *  copywrapper
 *  Wrapper function for the copy command
 *  
 * @param action pointer to the GTKAction event
 * @param gui pointer to the DenemoGUI structure
 */
void
copywrapper (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  copytobuffer (gui->si);
}

/**
 * cutwrapper
 * Wrapper function for the cut command
 *
 * @param action pointer to the GTKAction event
 * @param gui pointer to the DenemoGUI structure 
 */
void
cutwrapper (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  cuttobuffer (gui->si, TRUE);
  //check that measurewidths is long enough after cutting empty measures
  displayhelper (gui);
}

void
delete_selection(void) {
  DenemoGUI *gui = Denemo.gui;
  cuttobuffer (gui->si, FALSE);
  displayhelper (gui);
}
/**
 * pastewrapper
 * Wrapper function for the paste command
 *
 * @param gui pointer to the DenemoGUI structure
 * @param action pointer to the GtkAction event
 */
void
pastewrapper (GtkAction *action, DenemoScriptParam *param)
{  
  DenemoGUI *gui = Denemo.gui;
  gboolean pasted = pastefrombuffer ();
  if(pasted) {
  score_status(gui, TRUE);
  displayhelper (gui);
  } else {
    if(!action && param)
      param->status = FALSE;
    else
      if(copybuffer)
	warningdialog("Cannot paste multiple measures into middle of a measure");
      else
	warningdialog("Nothing to paste");
  }
}


/**
 * saveselwrapper
 * Wrapper function for the Save selection command
 *
 * @param action pointer to the GtkAction event 
 * @param gui pointer to the DenemoGUI structure
 */
void
saveselwrapper (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  saveselection (gui->si);
}

/**
 * mark_boundaries_helper
 * Helper function which marks the boundaries of the 
 * mark
 *
 * Inputs 
 * @param si pointer to the DenemoScore structure
 * @param mark_staff  
 * @param mark_measure -
 * @param mark_object -
 * @param point_staff -
 * @param point_measure -
 * @param point_object -
 * @param type -
 */
static void
mark_boundaries_helper (DenemoScore * si, gint mark_staff,
			gint mark_measure, gint mark_object, gint point_staff,
			gint point_measure, gint point_object,
			enum drag_selection_type type)
{
  if (mark_staff)
    {
      si->firststaffmarked = MIN (mark_staff, point_staff);
      si->laststaffmarked = MAX (mark_staff, point_staff);

      switch (type)
	{
	case NO_DRAG:
	  /* error, really.  */
	  break;
	case NORMAL_SELECT:
	case WHOLE_MEASURES:
	  /* I was thinking of handling these with a fallthrough, but
	     the commonality in setting si->firstmeasuremarked and
	     si->lastmeasuremarked caused it not to work out cleanly.  */
	  si->firstmeasuremarked = MIN (mark_measure, point_measure);
	  si->lastmeasuremarked = MAX (mark_measure, point_measure);
	  if (type == NORMAL_SELECT
	        && si->firststaffmarked == si->laststaffmarked )
	    {
	      if (mark_measure < point_measure)
		{
		  si->firstobjmarked = mark_object;
		  si->lastobjmarked = point_object;
		}
	      else if (mark_measure > point_measure)
		{
		  si->firstobjmarked = point_object;
		  si->lastobjmarked = mark_object;
		}
	      else
		{		/* Same measure */
		  si->firstobjmarked = MIN (mark_object, point_object);
		  si->lastobjmarked = MAX (mark_object, point_object);
		}
	    }
	  else
	    {
	      si->firstobjmarked = 0;
	      si->lastobjmarked = G_MAXINT;
	    }
	  break;
	case WHOLE_STAFFS:
	  si->firstmeasuremarked = 1;
	  si->lastmeasuremarked = g_list_length (si->measurewidths);
	  si->firstobjmarked = 0;
	  si->lastobjmarked = G_MAXINT;
	}
    }
}

/**
 * calcmarkboundaries
 * Wrapper function for the mark_boundaries_helper function
 * drag selection type is set to NORMAL_SELECT
 *
 * Inputs 
 * scoreinfo - score information
 */
void
calcmarkboundaries (DenemoScore * si)
{
  mark_boundaries_helper (si, si->markstaffnum, si->markmeasurenum,
			  si->markcursor_x, si->currentstaffnum,
			  si->currentmeasurenum, si->cursor_x, NORMAL_SELECT);
}

void
swap_point_and_mark(GtkAction *action, gpointer param) {
  DenemoScore * si = Denemo.gui->si;
  gint temp =  si->currentstaffnum;
  si->currentstaffnum = si->markstaffnum;
  si->markstaffnum = temp;

  temp =  si->currentmeasurenum;
  si->currentmeasurenum = si->markmeasurenum;
  si->markmeasurenum = temp;

  temp =  si->cursor_x;
  si->cursor_x = si->markcursor_x;
  si->markcursor_x = temp;
  setcurrentobject (si, si->cursor_x);
  calcmarkboundaries (si);
  displayhelper(Denemo.gui);
}
/**
 * undowrapper
 * Wrapper function for the undo command
 *
 * Inputs 
 * data - pointer to the score 
 * callback_action - unused
 * widget - unused
 */
void
undowrapper (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  undo (gui);
  displayhelper (gui);
}

/**
 * redowrapper
 * Wrapper function for the redo command
 *
 * Inputs 
 * data - pointer to the score 
 * callback_action - unused
 * widget - unused
 */
void
redowrapper (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  redo (gui);
  displayhelper (gui);
}


/**
 * undo
 * Self explantory - undo's the previous command
 *
 * Input
 * scoreinfo - score data
 */
static void
undo (DenemoGUI * gui)
{
  unre_data *undo=NULL;
  unre_data *redo = (unre_data *) g_malloc (sizeof (unre_data));//FIXME memory leak

  if (gui->notsaved /* consider has the changed been to this movement?? */&& g_queue_get_length (gui->si->undodata) > 0)
    {
      gui->si->undo_redo_mode = UNDO;
      undo = (unre_data *) g_queue_pop_head (gui->si->undodata);
      memcpy (redo, undo, sizeof (*undo));

      gui->si->currentstaffnum = undo->staffnum;
      gui->si->currentmeasurenum = undo->measurenum;
      setcurrents (gui->si);
calcmarkboundaries (gui->si);
      gui->si->cursor_x = undo->position;
      if (undo->action == ACTION_INSERT)
	{
	  g_debug ("Undo Action Insert:  Remove Object from score\n");
	  g_debug ("staffnum %d, measurenum %d, position %d\n",
		   undo->staffnum, undo->measurenum, undo->position);

	  setcurrentobject (gui->si, gui->si->cursor_x);

	  g_debug ("Position after set_currents %d\n", gui->si->cursor_x);

	  dnm_deleteobject (gui->si);
	  /*  redo->action = ACTION_DELETE;*/
	}
      else if (undo->action == ACTION_DELETE)
	{

	  object_insert (gui, undo->object);

	  g_debug ("UNDO Delete Object %d\n",
		   ((DenemoObject *) undo->object)->type);
	  g_debug ("Cursor position before UNDO %d\n", gui->si->cursor_x);
	  g_debug ("Cursor Position %d\n", gui->si->cursor_x);

	  setcurrents (gui->si);
calcmarkboundaries (gui->si);
	}
      else if (undo->action == ACTION_CHANGE)
	{
	  displayhelper (gui);
	}
    }

  /* update_redo_info (gui->si, redo);*/

  if (undo)
    g_free (undo);
}

/**
 * redo
 * Self explanitary - redoes the previous undo command
 *
 * Input
 * scoreinfo - score data
 */
void
redo (DenemoGUI * gui)
{
  unre_data *undo = NULL;
  unre_data *redo = NULL;
  
  DenemoScore *si = gui->si;
  if (gui->notsaved && g_queue_get_length (si->redodata) > 0)
    {
      si->undo_redo_mode = REDO;
      redo = (unre_data *) g_queue_pop_head (si->redodata);

      g_debug ("List length %d\n", g_queue_get_length (si->undodata));
      g_debug ("ACtion %d\n", redo->action);

      if (redo->action == ACTION_INSERT)
	{
	  si->currentstaffnum = redo->staffnum;
	  si->currentmeasurenum = redo->measurenum;
	  si->cursor_x = redo->position;
	  setcurrents (si);
calcmarkboundaries (si);

	  g_debug ("Position after set_currents %d\n", gui->si->cursor_x);

	  dnm_deleteobject (gui->si);
	}
      else if (redo->action == ACTION_DELETE)
	{

	  g_debug ("Redo Action Insert:  Remove Object from score\n");
	  g_debug ("staffnum %d, measurenum %d, position %d\n",
		   redo->staffnum, redo->measurenum, redo->position);

	  si->currentstaffnum = redo->staffnum;
	  si->currentmeasurenum = redo->measurenum;
	  si->cursor_x = redo->position;
	  setcurrents (si);
calcmarkboundaries (si);
	  object_insert (gui, (DenemoObject *) redo->object);

	  redo->action = ACTION_DELETE;
	}
      else if (redo->action == ACTION_CHANGE)
	{
	  g_debug ("Do something useful\n");
	}
    }
}

/**
 *  update_undo_info
 *  
 *  Updates the undo list with current operation.
 *  Is passed score structure and undo_data structure
 *
 */
void
update_undo_info (DenemoScore * si, unre_data * undo)
{
  unre_data *tmp = NULL;

  g_debug ("Undo structure: Action %d, Position %d,  Staff %d, Measure %d\n",
	   undo->action, undo->position, undo->staffnum, undo->measurenum); 

  if (g_queue_get_length (si->undodata) == MAX_UNDOS)
    {
      tmp = g_queue_pop_tail (si->undodata);
    }

  g_queue_push_head (si->undodata, undo);
  
}


/**
 * update_redo_info
 *  
 *  Updates the redo list with last undo operation.
 *  Is passed score structure and redo_data structure
 *  @param si pointer to the DenemoScore structure
 *  @param redo redo data structure to prepend to the queue
 */

void
update_redo_info (DenemoScore * si, unre_data * redo)
{
  unre_data *tmp = NULL;

  g_debug ("Redo structure: Action %d, Position %d,  Staff %d, Measure %d\n",
	   redo->action, redo->position, redo->staffnum, redo->measurenum);


  if (g_queue_get_length (si->redodata) == MAX_UNDOS)
    {
      tmp = g_queue_pop_tail (si->redodata);
      if (tmp)
	g_free (tmp);
    }
  g_queue_push_head (si->redodata, redo);
}
