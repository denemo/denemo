/**
 * select.c
 * operations for selecting, cutting, copying, and pasting music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee, 2011 Richard Shann
 *
 *
 */

#include <string.h>
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include <denemo/denemo.h>
#include "display/draw.h"
#include "display/calculatepositions.h"
#include "command/object.h"
#include "command/measure.h"
#include "command/select.h"
#include "command/staff.h"
#include "core/prefops.h"
#include "command/lyric.h"
#include "command/lilydirectives.h"
#include "command/score.h"
#include "core/cache.h"
#include "core/view.h"
#include "command/contexts.h"
#include "ui/moveviewport.h"
/*For save selection function*/
#include "core/utils.h"
/**
 * The copy buffer is a GList of objnode *s -- at first, I was going
 * to use staffnode *s, measurenode *s, and then objnode *s, but I
 * realized that'd be overkill and just complicate the implementation
 * unnecessarily.
 *
 * Each item in the copybuffer list corresponds to the stuff in
 * the buffer on each staff.
 */

static gboolean XesNeedRecalculating;
static void undo (DenemoProject * gui);
static void redo (DenemoProject * gui);
static GList *copybuffer = NULL;        // this is a list one for each staff of lists of objects

static gint staffsinbuffer = 0;
static gint measurebreaksinbuffer = 0;

static GList *clipboards = NULL;
typedef struct DenemoClipboard
{
  GList *objectlist;
  gint staffsinbuffer;
  gint measurebreaksinbuffer;
} DenemoClipboard;
static GList *
clone_obj_list (GList * g)
{
  GList *ret = NULL;
  do
    {
      ret = g_list_append (ret, dnm_clone_object (g->data));
    }
  while ((g = g->next));
  return ret;
}

// pushes the current copybuffer; pushes a NULL clipboard if none.
void
push_clipboard (void)
{
  GList *thecopy = NULL;
  DenemoClipboard *clip = (DenemoClipboard *) g_malloc0 (sizeof (DenemoClipboard));
  GList *g;
  for (g = copybuffer; g; g = g->next)
    {
      thecopy = g_list_append (thecopy, clone_obj_list (g->data));
    }
  clip->objectlist = thecopy;
  clip->measurebreaksinbuffer = measurebreaksinbuffer;
  clip->staffsinbuffer = staffsinbuffer;
  clipboards = g_list_prepend (clipboards, clip);
}
//takes the count'th stored clipboard and makes it the current clipboard
gboolean
pop_clipboard (gint count)
{
  GList *thecopy = NULL;
  DenemoClipboard *clip;
  if (clipboards == NULL)
    return FALSE;
  clip = (DenemoClipboard *) g_list_nth_data (clipboards, count);
  if (!clip)
    return FALSE;
  clipboards = g_list_remove (clipboards, clip);
  clearbuffer ();
  if (clip->objectlist)
    {
      thecopy = clip->objectlist;
      measurebreaksinbuffer = clip->measurebreaksinbuffer;
      staffsinbuffer = clip->staffsinbuffer;
      copybuffer = thecopy;
    }
  g_free (clip);
  return TRUE;
}

/* returns the top clipboard popped off the stack.
   The caller must free the clipboard with
   when done */
GList *
pop_off_clipboard (void)
{
  GList *thecopy = NULL;
  if (clipboards && clipboards->data)
    thecopy = ((DenemoClipboard *) clipboards->data)->objectlist;
  if(clipboards)
    g_free (clipboards->data);
  clipboards = g_list_remove (clipboards, clipboards->data);
  return thecopy;
}

/**
 *  sets current object to the given cursor position
 *
 */
void
setcurrentobject (DenemoMovement * si, gint cursorpos)
{

  g_debug ("Set Current Object Cursor pos %d\n", cursorpos);

  si->currentobject = g_list_nth ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects, cursorpos);
  //g_assert (si->currentobject != NULL);
}

/**
 *  clearbuffer
 *  Clears the copybuffer of data
 *  Arguments - None
 *  return - none
 */
void
clearbuffer (void)
{
  g_list_foreach (copybuffer, (GFunc)freeobjlist, NULL);
  g_list_free (copybuffer);
  copybuffer = NULL;
  staffsinbuffer = 0;
  measurebreaksinbuffer = 0;
}

void
free_clipboard (GList * clipboard)
{
  if (clipboard)
    {
      push_clipboard ();
      copybuffer = clipboard;
      measurebreaksinbuffer = 0;
      staffsinbuffer = 1;
      clearbuffer ();
      pop_clipboard (0);
    }
}

gint
get_staffs_in_clipboard (void)
{
  return staffsinbuffer;
}

void
insert_clipboard (GList * clipboard)
{
  if (clipboard)
    {
      push_clipboard ();
      copybuffer = clipboard;
      measurebreaksinbuffer = 0;
      staffsinbuffer = 1;
      call_out_to_guile ("(d-Paste)");
      copybuffer = NULL;
      pop_clipboard (0);
      displayhelper (Denemo.project);
      score_status(Denemo.project, TRUE);
    }
}


/**
 *   copytobuffer
 *   Copies selection to the copybuffer
 *
 *   @param si pointer to the score information structure
 */
void
copytobuffer (DenemoMovement * si)
{
  staffnode *curstaff;
  measurenode *curmeasure;
  objnode *curobj;
  objnode *theobjs;
  DenemoObject *clonedobject;
  gint i = 0, j = 0, k = 0;

  if (si->markstaffnum == 0)    /* Indicator that there's no selection.  */
    {
     clearbuffer ();
     call_out_to_guile ("(CreateScriptForDirective)");

    }
  else
    {
        call_out_to_guile ("(set! CreateScriptForDirective::clipboard #f)");
        Denemo.project->movement->directive_on_clipboard = NULL;
    }
  if  (si->markstaffnum == 0)
        return;
  clearbuffer ();
  si->selection_is_copied = TRUE;
  staffsinbuffer = si->selection.laststaffmarked - si->selection.firststaffmarked + 1;
  g_debug ("No staffs in copybuffer %d\n", staffsinbuffer);
  /* Staff loop.  */
  for (i = si->selection.firststaffmarked, curstaff = g_list_nth (si->thescore, i - 1); curstaff && i <= si->selection.laststaffmarked; curstaff = curstaff->next, i++)
    {
      if (((DenemoStaff *) curstaff->data)->is_parasite)
        continue;
      /* Initialize first ->data for copybuffer to NULL.  */
      theobjs = NULL;
      /* Measure loop.  */
      for (j = si->selection.firstmeasuremarked, k = si->selection.firstobjmarked, curmeasure = g_list_nth (staff_first_measure_node (curstaff), j - 1); curmeasure && j <= si->selection.lastmeasuremarked; curmeasure = curmeasure->next, j++)
        {
          for (curobj = g_list_nth ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, k);
               /* cursor_x is 0-indexed */
               curobj && (j < si->selection.lastmeasuremarked || k <= si->selection.lastobjmarked); curobj = curobj->next, k++)
            {
              clonedobject = dnm_clone_object ((DenemoObject *) curobj->data);
              theobjs = g_list_append (theobjs, clonedobject);
            }                   /* End object loop */
          g_debug ("cloned objects on staff \n");

          if (j < si->selection.lastmeasuremarked || k < si->selection.lastobjmarked)
            {
              if (!((j == si->selection.lastmeasuremarked)))
                {
                  g_print ("Insert measurebreak obj in copybuffer %d %d", j, k);
                  /* ???outdated comment??? That is, there's another measure, the cursor is in appending
                     position, or the selection spans multiple staffs, in which
                     case another measure boundary should be added.  */
                  theobjs = g_list_append (theobjs, newmeasurebreakobject ());
                  if (i == si->selection.firststaffmarked)
                    measurebreaksinbuffer++;
                }
            }
          k = 0;                /* Set it for next run through object loop */

        }                       /* End measure loop */
      if ((staffsinbuffer > 1) && (i < si->selection.laststaffmarked))
        {
          theobjs = g_list_append (theobjs, newstaffbreakobject ());
          g_debug ("Inserting Staffbreak object in copybuffer");
        }
      if (theobjs)
        copybuffer = g_list_append (copybuffer, theobjs);
    }                           /* End staff loop */
}


/**
 *  cuttobuffer
 *  Cuts selection to the copybuffer, removing it from the score
 *
 *  @param si pointer to score information structure
 */
static void
cuttobuffer (DenemoMovement * si, gboolean copyfirst)
{
  staffnode *curstaff;
  measurenode *curmeasure;
  objnode *tempobj;
  gint i, jcounter,             //jcounter is marking the position of the measure currently being cleared I think
    max;
  if (!si->markstaffnum)
    return;
  take_snapshot ();
  if (copyfirst)
    copytobuffer (si);
  gint staffs_removed_measures = 0;     // a count of removed measures in the case where multiple staffs are involved
  gint lmeasurebreaksinbuffer = si->selection.lastmeasuremarked - si->selection.firstmeasuremarked;
  gint lstaffsinbuffer = si->selection.laststaffmarked - si->selection.firststaffmarked + 1;
  if (copyfirst)
    {
      if (!(lmeasurebreaksinbuffer == measurebreaksinbuffer))
        g_warning ("logic of copy to buffer seems wrong about measure breaks");
      if (!(lstaffsinbuffer == staffsinbuffer))
        g_warning ("logic of copy to buffer seems wrong about staff breaks");
    }
  if (lstaffsinbuffer == 1)
    {
      /* Just a single staff is a special case, again.  */
      jcounter = si->selection.firstmeasuremarked;      //currently clearing stuff from the firstmeasuremarked
      curmeasure = g_list_nth (staff_first_measure_node (si->currentstaff), jcounter - 1);

      /* Clear the relevant part of the first measure selected */
      if (lmeasurebreaksinbuffer)
        max = G_MAXINT;
      else
        max = si->selection.lastobjmarked;
      for (i = si->selection.firstobjmarked; ((tempobj = g_list_nth ((objnode *) ((DenemoMeasure*)curmeasure->data)->objects, si->selection.firstobjmarked)) && i <= max); i++)
        {
           ((DenemoMeasure*)curmeasure->data)->objects = g_list_remove_link ((objnode *)  ((DenemoMeasure*)curmeasure->data)->objects, tempobj);
          freeobject ((DenemoObject *) tempobj->data);
          g_list_free_1 (tempobj);
        }
      jcounter++;               //move on to the second measure being cleared
      curmeasure = curmeasure->next;

      if (!si->thescore->next)
        {
          /* That is, the score has only this one staff
             remove the (whole) measures between the first and last - which may be partial. */
          if (lmeasurebreaksinbuffer - 1 > 0)
            {
              curmeasure = removemeasures (si, jcounter - 1, lmeasurebreaksinbuffer - 1, TRUE);
              jcounter += lmeasurebreaksinbuffer - 1;   // increased by the number of measures *between* first and last marked
            }
        }
      else
        for (; curmeasure && jcounter < si->selection.lastmeasuremarked; curmeasure = curmeasure->next, jcounter++)
          {
            freeobjlist (((DenemoMeasure*)curmeasure->data)->objects);
            ((DenemoMeasure*)curmeasure->data)->objects = NULL;
          }
      /* Now clear the relevant part of the last measure selected */
      if (curmeasure && (jcounter <= si->selection.lastmeasuremarked))
        {
          for (i = 0; ((DenemoMeasure*)curmeasure->data)->objects && i <= si->selection.lastobjmarked; i++)
            {
              tempobj = (objnode *)  ((DenemoMeasure*)curmeasure->data)->objects;
               ((DenemoMeasure*)curmeasure->data)->objects = g_list_remove_link ((objnode *)  ((DenemoMeasure*)curmeasure->data)->objects, tempobj);
              freeobject ((DenemoObject *) tempobj->data);
              g_list_free_1 (tempobj);
            }
          /* And delete it, if the measure's been cleared and there's only
             one staff.  */

          if (!((DenemoMeasure*)curmeasure->data)->objects && !si->thescore->next)
            removemeasures (si, g_list_position (staff_first_measure_node (si->currentstaff), curmeasure), 1, TRUE);
        }

      cache_staff (si->currentstaff);

      staff_fix_note_heights ((DenemoStaff*)si->currentstaff->data);
      staff_show_which_accidentals ((DenemoStaff *) si->currentstaff->data);
      staff_beams_and_stems_dirs ((DenemoStaff *) si->currentstaff->data);
    }                           // end of single staff
  else
    {                           /* Multiple staff selection */
      if (lstaffsinbuffer == (gint) (g_list_length (si->thescore)))
        {
          /* Every staff was part of the selection */
          if (lmeasurebreaksinbuffer > 0)
            {

              removemeasures (si, si->selection.firstmeasuremarked - 1, lmeasurebreaksinbuffer + 1, TRUE);
              staffs_removed_measures = lmeasurebreaksinbuffer;

              cache_all ();
            }
          else
            for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
              {
                curmeasure = g_list_nth (staff_first_measure_node (curstaff), si->selection.firstmeasuremarked - 1);
                freeobjlist ( ((DenemoMeasure*)curmeasure->data)->objects);
                 ((DenemoMeasure*)curmeasure->data)->objects = NULL;

                cache_staff (curstaff);
                staff_fix_note_heights ((DenemoStaff*)curstaff->data);
                staff_show_which_accidentals ((DenemoStaff *) curstaff->data);
                staff_beams_and_stems_dirs ((DenemoStaff *) curstaff->data);
              }
        }
      else
        {
          /* Staff loop */
          for (i = si->selection.firststaffmarked, curstaff = g_list_nth (si->thescore, i - 1); curstaff && i <= si->selection.laststaffmarked; curstaff = curstaff->next, i++)
            {
              if (((DenemoStaff *) curstaff->data)->is_parasite)
                continue;
              /* Measure loop */
              for (jcounter = si->selection.firstmeasuremarked, curmeasure = g_list_nth (staff_first_measure_node (curstaff), jcounter - 1); curmeasure && jcounter <= si->selection.lastmeasuremarked; curmeasure = curmeasure->next, jcounter++)
                {
                  freeobjlist ( ((DenemoMeasure*)curmeasure->data)->objects);
                   ((DenemoMeasure*)curmeasure->data)->objects = NULL;
                }

              cache_staff (curstaff);

              staff_show_which_accidentals ((DenemoStaff *) curstaff->data);
              staff_beams_and_stems_dirs ((DenemoStaff *) curstaff->data);
            }
        }



    }
  si->selection.firststaffmarked = si->markstaffnum = 0;        //only the latter is needed, but there was some confusion at one time...
  /* And set some currents. This would probably be better to split off
   * into a more-generalized version of setcurrents or something;
   * what's here is more-or-less copied from dnm_deleteobject in
   * commandfuncs */

  si->currentmeasurenum = si->selection.firstmeasuremarked - (staffs_removed_measures ? 1 : 0);

  if (si->currentmeasurenum < 1)
    {
      si->currentmeasurenum = 1;
    }


  si->currentmeasure = g_list_nth (staff_first_measure_node (si->currentstaff), si->currentmeasurenum - 1);

  si->cursor_x = si->selection.firstobjmarked;
  if (si->cursor_x < (gint) (g_list_length ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects)))
    {
      si->currentobject = g_list_nth ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects, si->cursor_x);
      si->cursor_appending = FALSE;
    }
  else
    {
      si->currentobject = g_list_last ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects);
      si->cursor_appending = TRUE;
    }
  //if clef has been deleted we need to re-validate leftmost clef - would only apply if the clef being deleted was off the left side of screen - some sort of scripting scenario...
  find_leftmost_allcontexts (si);


  isoffleftside (Denemo.project);
  isoffrightside (Denemo.project);

  score_status (Denemo.project, TRUE);

}



DenemoObjType
get_clip_obj_type (gint m, gint n)
{
  if (copybuffer == NULL)
    return -1;
  GList *stafflist = g_list_nth (copybuffer, m);
  if (stafflist == NULL)
    return -1;
  GList *curbufferobj = g_list_nth (stafflist->data, n);
  if (curbufferobj == NULL || curbufferobj->data == NULL)
    return -1;
  return ((DenemoObject *) (curbufferobj->data))->type;
}

gint
get_clip_objs (gint m)
{
  if (copybuffer == NULL)
    return -1;
  GList *stafflist = g_list_nth (copybuffer, m);
  if (stafflist == NULL)
    return -1;
  return g_list_length (stafflist->data);
}



// insert the nth object from the mth staff from the copybuffer into music at the cursor position
// return TRUE if inserted
gboolean
insert_clip_obj (gint m, gint n)
{
  DenemoMovement *si = Denemo.project->movement;
  if (copybuffer == NULL)
    return FALSE;
  GList *stafflist = g_list_nth (copybuffer, m);
  if (stafflist == NULL)
    return FALSE;
  objnode *curbufferobj = g_list_nth (stafflist->data, n);
  if (curbufferobj == NULL)
    return FALSE;
  DenemoObject *clonedobj;
  DenemoObject *curobj = (DenemoObject *) curbufferobj->data;
  clonedobj = dnm_clone_object (curobj);
  object_insert (Denemo.project, clonedobj);
  cache_measure (si->currentmeasure);
  //reset_cursor_stats (si);
  if (clonedobj->type == CLEF)
    staff_fix_note_heights ((DenemoStaff *) si->currentstaff->data);
  else
    if (clonedobj->type == CHORD)
        newclefify (clonedobj);
  staff_beams_and_stems_dirs ((DenemoStaff *) si->currentstaff->data);
  find_xes_in_all_measures (si);
  showwhichaccidentals ((objnode *) ((DenemoMeasure*)si->currentmeasure->data)->objects);

  return TRUE;
}


DenemoObject *
get_mark_object (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (!si->markstaffnum)
    return NULL;
  staffnode *curstaff = g_list_nth (si->thescore, si->selection.firststaffmarked - 1);
  DenemoStaff *firststaff = (DenemoStaff *) curstaff->data;
  measurenode *firstmeasure = g_list_nth (firststaff->themeasures, si->selection.firstmeasuremarked - 1);
  objnode *firstobj = g_list_nth (((DenemoMeasure*)firstmeasure->data)->objects, si->selection.firstobjmarked);
  //g_debug("First %d\n",  si->selection.firstobjmarked);
  return firstobj ? ((DenemoObject *) firstobj->data) : NULL;
}

DenemoObject *
get_point_object (void)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  if (!si->markstaffnum)
    return NULL;
  staffnode *curstaff = g_list_nth (si->thescore, si->selection.laststaffmarked - 1);
  DenemoStaff *laststaff = (DenemoStaff *) curstaff->data;
  measurenode *lastmeasure = g_list_nth (laststaff->themeasures, si->selection.lastmeasuremarked - 1);
  objnode *lastobj = g_list_nth (((DenemoMeasure*)lastmeasure->data)->objects, si->selection.lastobjmarked);
  return lastobj ? ((DenemoObject *) lastobj->data) : NULL;
}

/**
 * mark_boundaries_helper
 * Helper function which marks the boundaries of the
 * mark
 *
 * Inputs
 * @param si pointer to the DenemoMovement structure
 * @param mark_staff
 * @param mark_measure -
 * @param mark_object -
 * @param point_staff -
 * @param point_measure -
 * @param point_object -
 * @param type -
 */
static void
mark_boundaries_helper (DenemoMovement * si, gint mark_staff, gint mark_measure, gint mark_object, gint point_staff, gint point_measure, gint point_object, enum drag_selection_type type)
{
  si->selection_is_copied = FALSE;
  if (mark_staff)
    {
      si->selection.firststaffmarked = MIN (mark_staff, point_staff);
      si->selection.laststaffmarked = MAX (mark_staff, point_staff);

      switch (type)
        {
        case NO_DRAG:
          /* error, really.  */
          break;
        case NORMAL_SELECT:
        case WHOLE_MEASURES:
          /* I was thinking of handling these with a fallthrough, but
             the commonality in setting si->selection.firstmeasuremarked and
             si->selection.lastmeasuremarked caused it not to work out cleanly.  */
          si->selection.firstmeasuremarked = MIN (mark_measure, point_measure);
          si->selection.lastmeasuremarked = MAX (mark_measure, point_measure);
          if (type == NORMAL_SELECT && si->selection.firststaffmarked == si->selection.laststaffmarked)
            {
              if (mark_measure < point_measure)
                {
                  si->selection.firstobjmarked = mark_object;
                  si->selection.lastobjmarked = point_object;
                }
              else if (mark_measure > point_measure)
                {
                  si->selection.firstobjmarked = point_object;
                  si->selection.lastobjmarked = mark_object;
                }
              else
                {               /* Same measure */
                  si->selection.firstobjmarked = MIN (mark_object, point_object);
                  si->selection.lastobjmarked = MAX (mark_object, point_object);
                }
            }
          else
            {
              si->selection.firstobjmarked = 0;
              si->selection.lastobjmarked = G_MAXINT - 1;
            }
          break;
        case WHOLE_STAFFS:
          si->selection.firstmeasuremarked = 1;
          si->selection.lastmeasuremarked = g_list_length (si->measurewidths);
          si->selection.firstobjmarked = 0;
          si->selection.lastobjmarked = G_MAXINT - 1;
        }
    }
}


/**
 *  setmark
 *  Sets the current mark for the start of the buffer
 *
 */
void
set_mark (DenemoAction* action, DenemoScriptParam * param)
{
  DenemoMovement *si = Denemo.project->movement;
  si->selection_is_copied = FALSE;
  si->markstaffnum = si->currentstaffnum;
  si->markmeasurenum = si->currentmeasurenum;
  si->markcursor_x = si->cursor_x;
  calcmarkboundaries (si);
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw(Denemo.scorearea);
}

/**
 *  set_point
 *  Sets the current cursor position as the end of the selection
 *
 */
void
set_point (DenemoAction* action, DenemoScriptParam * param)
{
  DenemoMovement *si = Denemo.project->movement;
  if (si->markstaffnum)
    {
      mark_boundaries_helper (si, si->markstaffnum, si->markmeasurenum, si->markcursor_x, si->currentstaffnum, si->currentmeasurenum, si->cursor_x, NORMAL_SELECT);

    }
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw(Denemo.scorearea);
}

gboolean
mark_status (void)
{
  return Denemo.project->movement->markstaffnum != 0;
}

/**
 * unset_mark
 * Remove the current mark
 *
 */
void
unset_mark (DenemoAction* action, DenemoScriptParam * param)
{
  DenemoMovement *si = Denemo.project->movement;
  si->markstaffnum = 0;
  calcmarkboundaries (si);
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw(Denemo.scorearea);
}


gboolean
in_selection (DenemoMovement * si)
{
  if (si->markstaffnum)
    {
      if (si->currentstaffnum >= si->selection.firststaffmarked && si->currentstaffnum <= si->selection.laststaffmarked)
        {
          if (si->currentmeasurenum == si->selection.firstmeasuremarked)
            {
              if (si->currentmeasurenum == si->selection.lastmeasuremarked)
                {
                  if (si->cursor_appending) 
                    return FALSE;
                  if ((si->cursor_x >= si->selection.firstobjmarked) && (si->cursor_x <= si->selection.lastobjmarked))
                    return TRUE;
                  else
                    return FALSE;
                }
              if (si->currentmeasurenum < si->selection.lastmeasuremarked)
                {
                  if (si->cursor_x >= si->selection.firstobjmarked)
                    return TRUE;
                  return FALSE;
                }
            }
          if (si->currentmeasurenum > si->selection.firstmeasuremarked)
            {
              if (si->currentmeasurenum == si->selection.lastmeasuremarked)
                {
                  if ((si->cursor_x <= si->selection.lastobjmarked))
                    return TRUE;
                  else
                    return FALSE;
                }
              if (si->currentmeasurenum < si->selection.lastmeasuremarked)
                return TRUE;
            }
        }
    }
  return FALSE;
}

/* save/restore selection */
static gint firststaff;
static gint laststaff;
static gint firstobj;
static gint lastobj;
static gint firstmeasure;
static gint lastmeasure;

void
save_selection (DenemoMovement * si)
{
  firststaff = si->selection.firststaffmarked;
  laststaff = si->selection.laststaffmarked;
  firstobj = si->selection.firstobjmarked;
  lastobj = si->selection.lastobjmarked;
  firstmeasure = si->selection.firstmeasuremarked;
  lastmeasure = si->selection.lastmeasuremarked;
}

void
restore_selection (DenemoMovement * si)
{
  si->selection.firststaffmarked = firststaff;
  si->selection.laststaffmarked = laststaff;
  si->selection.firstobjmarked = firstobj;
  si->selection.lastobjmarked = lastobj;
  si->selection.firstmeasuremarked = firstmeasure;
  si->selection.lastmeasuremarked = lastmeasure;
}



/**
 * goto_mark
 * goto the current mark without changing the selection
 *
 *
 */
void
goto_mark (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoScriptParam local_param;
  local_param.status = TRUE;
  DenemoMovement *si = Denemo.project->movement;
  if (!action)
    ((DenemoScriptParam *) param)->status = si->markstaffnum;
  else
    param = &local_param;
  if (si->markstaffnum)
    {
      save_selection (si);
      set_currentmeasurenum (Denemo.project, si->markmeasurenum);
      set_currentstaffnum (Denemo.project, si->markstaffnum);
      while (si->cursor_x < si->markcursor_x && param->status)
        cursorright (NULL, param);
      restore_selection (si);
      if (!action)
        displayhelper (Denemo.project);
    }
}

/**
 * goto_selection_start
 * move cursor the first object in the selection without changing the selection
 *
 *
 */
void
goto_selection_start (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoMovement *si = Denemo.project->movement;
  if (!action)
    ((DenemoScriptParam *) param)->status = si->markstaffnum;
  if (si->markstaffnum)
    {
      gint first = si->selection.firstobjmarked;
      save_selection (si);
      set_currentmeasurenum (Denemo.project, si->selection.firstmeasuremarked);
      set_currentstaffnum (Denemo.project, si->selection.firststaffmarked);
      while (si->cursor_x < first)
        cursorright (NULL, NULL);
      restore_selection (si);
      if (!action)
        displayhelper (Denemo.project);
    }
}




static GSList *positions = NULL;
DenemoPosition *
pop_position (void)
{
  DenemoPosition *pos;
  if (positions)
    {
      pos = positions->data;
      positions = g_slist_delete_link (positions, positions);
      return pos;
    }
  return NULL;
}

void
get_position (DenemoMovement * si, DenemoPosition * pos)
{
  pos->movement = g_list_index (Denemo.project->movements, si) + 1;
  pos->staff = si->currentstaffnum;
  pos->measure = si->currentmeasurenum;
  pos->object = si->currentobject ? si->cursor_x + 1 : 0;
  pos->appending = si->cursor_appending;
  pos->offend = si->cursoroffend;
  pos->leftmeasurenum = si->leftmeasurenum;
}

void
push_position (void)
{
  DenemoMovement *si = Denemo.project->movement;
  DenemoPosition *pos = (DenemoPosition *) g_malloc (sizeof (DenemoPosition));
  get_position (si, pos);
  if (pos->movement)
    positions = g_slist_prepend (positions, pos);
  else
    g_free (pos);
  //g_debug("%d %d %d %d \n", pos->movement, pos->staff, pos->measure, pos->object);
}

static void
push_given_position (DenemoPosition * pos)
{
  DenemoPosition *position = (DenemoPosition *) g_malloc (sizeof (DenemoPosition));
  memcpy (position, pos, sizeof (DenemoPosition));
  positions = g_slist_prepend (positions, position);
}

/**
 *  copywrapper
 *  Wrapper function for the copy command
 *
 * @param action pointer to the DenemoAction event
 * @param gui pointer to the DenemoProject structure
 */
void
copywrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  copytobuffer (gui->movement);
}

/**
 * cutwrapper
 * Wrapper function for the cut command
 *
 * @param action pointer to the DenemoAction event
 * @param gui pointer to the DenemoProject structure
 */
void
cutwrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  cuttobuffer (gui->movement, TRUE);
  //check that measurewidths is long enough after cutting empty measures
  displayhelper (gui);
}

void
delete_selection (void)
{
  DenemoProject *gui = Denemo.project;
  cuttobuffer (gui->movement, FALSE);
  displayhelper (gui);
}

/**
 * pastewrapper
 * Wrapper function for the paste command
 *
 * @param gui pointer to the DenemoProject structure
 * @param action pointer to the DenemoAction event
 */
void
pastewrapper (DenemoAction * action, DenemoScriptParam * param)
{
  stage_undo (Denemo.project->movement, ACTION_STAGE_END);        //undo is a queue (ie stack) so we push the end first

  if ((Denemo.project->movement->directive_on_clipboard) && (copybuffer == NULL))
    call_out_to_guile ("(eval-string CreateScriptForDirective::clipboard)");
  else
    call_out_to_guile ("(DenemoPaste)");
  //FIXME if not success a ACTION_SCRIPT_ERROR will have been put in the undo queue...
  stage_undo (Denemo.project->movement, ACTION_STAGE_START);

  score_status (Denemo.project, TRUE);
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
calcmarkboundaries (DenemoMovement * si)
{
  mark_boundaries_helper (si, si->markstaffnum, si->markmeasurenum, si->markcursor_x, si->currentstaffnum, si->currentmeasurenum, si->cursor_x, NORMAL_SELECT);
}

void
swap_point_and_mark (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoMovement *si = Denemo.project->movement;
  gint temp = si->currentstaffnum;
  si->currentstaffnum = si->markstaffnum;
  si->markstaffnum = temp;

  temp = si->currentmeasurenum;
  si->currentmeasurenum = si->markmeasurenum;
  si->markmeasurenum = temp;

  temp = si->cursor_x;
  si->cursor_x = si->markcursor_x;
  si->markcursor_x = temp;
  setcurrentobject (si, si->cursor_x);
  calcmarkboundaries (si);
  displayhelper (Denemo.project);
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
undowrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  XesNeedRecalculating = FALSE;
  undo (gui);
  if (XesNeedRecalculating)
     setcurrents (Denemo.project->movement), find_xes_in_all_measures (Denemo.project->movement);
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
redowrapper (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  redo (gui);
  displayhelper (gui);
}

/* store the passed object as ACTION_CHANGE undo information */
/* potentially we could optimize the storage of undo information by telescoping changes to the same object when the undo is staged, it would mean keeping a global note of whether the undo is currently staged. We would peek at the head of the queue and if it was an ACTION_CHANGE at the same position we could free the stored object and replace it with the clone created here */
void
store_for_undo_change (DenemoMovement * si, DenemoObject * curobj)
{
  if (!si->undo_guard)
    {
      DenemoUndoData *data = (DenemoUndoData *) g_malloc (sizeof (DenemoUndoData));
      data->object = dnm_clone_object (curobj);
      get_position (si, &data->position);
      data->action = ACTION_CHANGE;
      update_undo_info (si, data);
    }
}

void
store_for_undo_measure_create (DenemoMovement * si, gint staffnum, gint measurenum)
{
  if (!si->undo_guard)
    {
      DenemoUndoData *data = (DenemoUndoData *) g_malloc (sizeof (DenemoUndoData));
      data->position.staff = staffnum;
      data->position.measure = measurenum + 1;
      data->action = ACTION_MEASURE_CREATE;
      update_undo_info (si, data);
    }
}

static void
free_chunk (DenemoUndoData * chunk)
{
  g_debug ("free %d\n", chunk->action);
  switch (chunk->action)
    {
    case ACTION_STAGE_START:
    case ACTION_STAGE_END:
    case ACTION_SCRIPT_ERROR:
      return;                   //statically allocated

    case ACTION_INSERT:
    case ACTION_DELETE:
    case ACTION_CHANGE:
      freeobject (chunk->object);
      g_free (chunk);
      break;

    case ACTION_MEASURE_CREATE:
    case ACTION_MEASURE_REMOVE:
      g_free (chunk);
      break;
    case ACTION_MEASURE_DELETE:
    case ACTION_MEASURE_INSERT:
      if (chunk->object)
        free_measure (chunk->object);
      g_free (chunk);
      break;
    case ACTION_SNAPSHOT:
      g_warning ("Snapshot free is not implemented");
      g_free (chunk);
      break;
    default:
      g_warning ("Unknown type of undo data %d", chunk->action);
    }
}



static DenemoUndoData ActionStageStart = { ACTION_STAGE_START };
static DenemoUndoData ActionStageEnd = { ACTION_STAGE_END };
static DenemoUndoData ActionScriptError = { ACTION_SCRIPT_ERROR };

void
stage_undo (DenemoMovement * si, action_type type)
{
  switch (type)
    {
    case ACTION_STAGE_START:
      {
        if (g_queue_is_empty (si->undodata))
          return;
        DenemoUndoData *chunk = g_queue_peek_head (si->undodata);
        if (chunk->action == ACTION_STAGE_END)
          {
            chunk = g_queue_pop_head (si->undodata);
            // free_chunk(chunk); not needed, is static anyway
            //g_debug("Script did not need undoing");
          }
        else
          update_undo_info (si, &ActionStageStart);
      }
      break;
    case ACTION_STAGE_END:
      update_undo_info (si, &ActionStageEnd);
      break;
    case ACTION_SCRIPT_ERROR:
      update_undo_info (si, &ActionScriptError);
      break;
    default:
      g_warning ("Unknown undo action %d will not be stored", type);
    }
}

//return a string describing the top of the undo stack, or one below if stage start.
// caller must g_free
gchar *
get_last_change (DenemoMovement * si)
{
  DenemoUndoData *last = g_queue_peek_head (si->undodata);
  gint n = 0;
  while (last && ((last->action == ACTION_STAGE_START) || (last->action == ACTION_STAGE_END)))
    last = g_queue_peek_nth (si->undodata, ++n);
  if (last == NULL)
    return NULL;

  switch (last->action)
    {
    case ACTION_SNAPSHOT:
      return g_strdup_printf ("Snapshot (e.g. measure delete, cut, paste and sadly many other things ... ");
      break;
    case ACTION_INSERT:
      return g_strdup_printf ("Insert the object at staff %d measure %d position %d; ", last->position.staff, last->position.measure, last->position.object + 1);
    case ACTION_DELETE:
      return g_strdup_printf ("Deleted a %s at staff %d measure %d position %d; ", DenemoObjTypeNames[((DenemoObject *) last->object)->type], last->position.staff, last->position.measure, last->position.object);
      break;
    case ACTION_CHANGE:
      return g_strdup_printf ("Change %s at staff %d measure %d position %d; ", DenemoObjTypeNames[((DenemoObject *) last->object)->type], last->position.staff, last->position.measure, last->position.object);
      break;
    case ACTION_MEASURE_CREATE:
      return g_strdup_printf ("Create %s; at staff %d measure %d position %d; ", DenemoObjTypeNames[((DenemoObject *) last->object)->type], last->position.staff, last->position.measure, last->position.object);
      break;
    case ACTION_MEASURE_REMOVE:
      return g_strdup_printf ("Remove %s; at staff %d measure %d position %d; ", DenemoObjTypeNames[((DenemoObject *) last->object)->type], last->position.staff, last->position.measure, last->position.object);
    case ACTION_MEASURE_DELETE:
      return g_strdup_printf ("Delete Measure at staff %d measure %d position %d; ", last->position.staff, last->position.measure, last->position.object);
    case ACTION_NOOP:
      return g_strdup_printf ("No-op; ");
      break;
    default:
      return g_strdup_printf ("Unknown action %d\n", last->action);
    }

}

// snapshot the current movement for undo
gboolean
take_snapshot (void)
{
  if (!Denemo.project->movement->undo_guard)
    {
      DenemoUndoData *chunk;
      chunk = (DenemoUndoData *) g_malloc (sizeof (DenemoUndoData));
      chunk->object = (DenemoObject *) clone_movement (Denemo.project->movement);
      //fix up somethings...
      get_position (Denemo.project->movement, &chunk->position);
      chunk->position.appending = 0;
      chunk->action = ACTION_SNAPSHOT;
      update_undo_info (Denemo.project->movement, chunk);

      return TRUE;
    }
  else
    return FALSE;
}

static void
print_queue (gchar * msg, GQueue * q)
{
  GList *g;
  g_print ("%s", msg);
  for (g = q->head; g; g = g->next)
    {
      DenemoUndoData *chunk = g->data;
      switch (chunk->action)
        {
        case ACTION_STAGE_START:
          g_print ("[");
          break;
        case ACTION_STAGE_END:
          g_print ("]\n");
          break;
        case ACTION_SNAPSHOT:
          g_print ("Snapshot; ");
          break;
        case ACTION_INSERT:
          g_print ("Ins; ");
          break;
        case ACTION_DELETE:
          g_print ("Del %s; ", DenemoObjTypeNames[((DenemoObject *) chunk->object)->type]);
          break;
        case ACTION_CHANGE:
          g_print ("Chn %s; ", DenemoObjTypeNames[((DenemoObject *) chunk->object)->type]);
          break;
        case ACTION_MEASURE_CREATE:
          g_print ("Create; ");
          break;
        case ACTION_MEASURE_REMOVE:
          g_print ("Remove; ");
          break;
        case ACTION_MEASURE_DELETE:
          g_print ("Delete Measure; ");
          break;
        case ACTION_NOOP:
          g_print ("No-op; ");
          break;
        default:
          g_print ("Unknown action %d\n", chunk->action);

        }
    }
  g_print ("End queue");
}


static gboolean
position_for_chunk (DenemoProject * gui, DenemoUndoData * chunk)
{
  DenemoScriptParam param;
  param.status = TRUE;
  //g_debug("undo guard before %d level is %d\n undo action is %d\n",  gui->movement->undo_guard, gui->undo_level, chunk->action);

  switch (chunk->action)
    {
    case ACTION_CHANGE:
      if (chunk->position.object == 0)
        return FALSE;           //Cannot undo a change in an empty measure=>undo queue is corrupt
      //FALL THRU
    case ACTION_INSERT:
    case ACTION_DELETE:
    case ACTION_MEASURE_CREATE:        //this creates an (blank)measure
    case ACTION_MEASURE_REMOVE:        //this is the action that removes a blank measure at pos
    case ACTION_MEASURE_DELETE:        //this is the action that deletes measure at pos
      {
        goto_movement_staff_obj (NULL /*non-interactive*/, -1, chunk->position.staff, chunk->position.measure, chunk->position.object, chunk->position.leftmeasurenum);
      }
      break;
    case ACTION_NOOP:
      break;
    default:
      break;
    }
  return param.status;
}

//Takes the action needed for one chunk of undo/redo data

static void
action_chunk (DenemoProject * gui, DenemoUndoData ** pchunk)
{
  DenemoUndoData *chunk = *pchunk;
  switch (chunk->action)
    {
    case ACTION_MEASURE_CREATE:
      {
        //delete the empty measure in the chunk->position.staff at measure number chunk->position->object
        dnm_deletemeasure (gui->movement);
        chunk->action = ACTION_MEASURE_REMOVE;
        if (chunk->position.measure > 1)
          chunk->position.measure--;
        else
          chunk->action = ACTION_NOOP;

        if (!gui->movement->currentmeasure)
          {
            g_warning ("position after undo insert Bug in select.c");
            position_for_chunk (gui, chunk);
            //movetoend(NULL, NULL);
          }
      }
      break;

    case ACTION_MEASURE_REMOVE:
      {
        //create empty measure in the chunk->position.staff at measure number chunk->position->object
        insertmeasureafter (NULL, NULL);

        cache_measure (Denemo.project->movement->currentmeasure);//rather than cache_staff (Denemo.project->movement->currentstaff); //rather than cach_all ();

        chunk->action = ACTION_MEASURE_CREATE;
        chunk->position.measure++;
        if (!gui->movement->currentmeasure)
          {
            g_warning ("position after undo insert Bug in select.c");
            position_for_chunk (gui, chunk);    //????
            //movetoend(NULL, NULL);
          }
      }
      break;
    case ACTION_MEASURE_DELETE:
      {
        //insert the chunk object (a DenemoMeasure) at chunk->position.staff
        GList *curstaff = g_list_nth (gui->movement->thescore, chunk->position.staff -1); //g_print ("Selecting staff %d\n",  chunk->position.staff -1);
        ((DenemoStaff *) curstaff->data)->themeasures = g_list_insert (staff_first_measure_node (curstaff), chunk->object, chunk->position.measure - 1);
        ((DenemoStaff *) curstaff->data)->nummeasures++;
        //find_xes_in_measure (Denemo.project->movement,  chunk->position.measure);
        cache_staff (curstaff);
        staff_fix_note_heights (curstaff->data);
        chunk->action = ACTION_MEASURE_INSERT;

        chunk->object = NULL;
        if (g_list_length ( ((DenemoStaff *) curstaff->data)->themeasures) > g_list_length (Denemo.project->movement->measurewidths))
            Denemo.project->movement->measurewidths = g_list_append (Denemo.project->movement->measurewidths, GINT_TO_POINTER (Denemo.project->movement->measurewidth)); 
          
        position_for_chunk (gui, chunk);
        XesNeedRecalculating = TRUE;
        //setcurrents (Denemo.project->movement);
      }
      break;
    case ACTION_MEASURE_INSERT:
      {
        chunk->object = clone_measure (gui->movement->currentmeasure->data);
        dnm_deletemeasure (gui->movement);
        chunk->action = ACTION_MEASURE_DELETE;
      }
      break;

    case ACTION_STAGE_START:
      gui->undo_level++;

      *pchunk = &ActionStageEnd;
      break;
    case ACTION_STAGE_END:
      gui->undo_level--;
      *pchunk = &ActionStageStart;
      setcurrents (Denemo.project->movement);
      break;
    case ACTION_SCRIPT_ERROR:
      //chunk = &ActionScriptError;
      gui->undo_level = 0;
      break;

    case ACTION_INSERT:
      {
       if(gui->movement->currentobject) {
			chunk->object = dnm_clone_object (gui->movement->currentobject->data);
			dnm_deleteobject (gui->movement);
			chunk->action = ACTION_DELETE;
		}
      }
      break;
    case ACTION_DELETE:
      {
        object_insert (gui, chunk->object);cache_all ();
        chunk->action = ACTION_INSERT;
        chunk->object = NULL;
      }
      break;
    case ACTION_CHANGE:
      { 
        if(gui->movement->currentobject) {
			DenemoObject *temp = gui->movement->currentobject->data;
			gui->movement->currentobject->data = chunk->object;
			chunk->object = temp;cache_all ();
		}
      }
      break;
    case ACTION_SNAPSHOT:
      {

        DenemoMovement *si = (DenemoMovement *) chunk->object;
        gint initial_guard = gui->movement->undo_guard;
        gint initial_changecount = gui->movement->changecount;
        gboolean initial_redo_invalid = gui->movement->redo_invalid;
        gpointer initial_smf = gui->movement->smf;
        // replace gui->movement in gui->movements with si
        GList *find = g_list_find (gui->movements, gui->movement);
        if (find)
          {
            find->data = si;
            GList *g, *gorig = NULL, *curstaff;
            for (curstaff = gui->movement->thescore; curstaff; curstaff = curstaff->next)
              {
                DenemoStaff *thestaff = curstaff->data;
                gorig = g = thestaff->verse_views;
                thestaff->verse_views = NULL;
                for (; g; g = g->next)
                  {
                    gchar *text = get_text_from_view (g->data);
                    gtk_widget_destroy (gtk_widget_get_parent(g->data));      
                    thestaff->verse_views = g_list_append (thestaff->verse_views, text);
                    if (thestaff->current_verse_view == g)
                      thestaff->current_verse_view = g_list_last (thestaff->verse_views);
                  }

                {
                  GList *direc;
                  for (direc = thestaff->staff_directives; direc; direc = direc->next)
                    {
                      DenemoDirective *directive = direc->data;
                      if (directive->widget)
                        {
                          gtk_widget_destroy (directive->widget);
                          directive->widget = NULL;
                        }
                      //widget_for_staff_directive(directive);
                    }
                }
                {
                  GList *direc;
                  for (direc = thestaff->voice_directives; direc; direc = direc->next)
                    {
                      DenemoDirective *directive = direc->data;
                      if (directive->widget)
                        {
                          gtk_widget_destroy (directive->widget);
                          directive->widget = NULL;
                        }
                      //widget_for_voice_directive(directive);
                    }
                }
              }


            {
              GList *direc;
              for (direc = gui->movement->movementcontrol.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  if(directive->widget) gtk_widget_destroy (directive->widget);
                  directive->widget = NULL;

                }
            }
            {
              GList *direc;
              for (direc = gui->movement->header.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  if(directive->widget) gtk_widget_destroy (directive->widget);
                  directive->widget = NULL;
                }
            }
            {
              GList *direc;
              for (direc = gui->movement->layout.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  if(directive->widget) gtk_widget_destroy (directive->widget);
                  directive->widget = NULL;
                }
            }

            g_list_free (gorig);
            chunk->object = (DenemoObject *) gui->movement;
            //FIXME fix up other values in stored object si?????? voice/staff directive widgets
            gui->movement = si;
            for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
              {
                DenemoStaff *thestaff = curstaff->data;
                gorig = g = thestaff->verse_views;
                gint curversenum = g_list_position (g, thestaff->current_verse_view);
                thestaff->verse_views = NULL;

                for (; g; g = g->next)
                  {
                    add_verse_to_staff (si, thestaff);
                    gtk_text_buffer_set_text (gtk_text_view_get_buffer ((GtkTextView *) thestaff->current_verse_view->data), g->data, -1);
                    gtk_widget_show (thestaff->current_verse_view->data);
                    g_signal_connect (G_OBJECT (gtk_text_view_get_buffer (thestaff->current_verse_view->data)), "changed", G_CALLBACK (lyric_changed_cb), NULL);
                  }
                thestaff->current_verse_view = g_list_nth (thestaff->verse_views, curversenum);


                {
                  GList *direc;
                  for (direc = thestaff->staff_directives; direc; direc = direc->next)
                    {
                      DenemoDirective *directive = direc->data;
                      directive->widget = NULL;
                      widget_for_staff_directive (directive, thestaff->staffmenu);
                    }
                }
                {
                  GList *direc;
                  for (direc = thestaff->voice_directives; direc; direc = direc->next)
                    {
                      DenemoDirective *directive = direc->data;
                      directive->widget = NULL;
                      widget_for_voice_directive (directive, thestaff->voicemenu);
                    }
                }
                g_list_free (gorig);
              }


            {
              GList *direc;
              for (direc = gui->movement->movementcontrol.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  directive->widget = NULL;
                  widget_for_movementcontrol_directive (directive);
                }
            }
            {
              GList *direc;
              for (direc = gui->movement->header.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  directive->widget = NULL;
                  widget_for_header_directive (directive);
                }
            }
            {
              GList *direc;
              for (direc = gui->movement->layout.directives; direc; direc = direc->next)
                {
                  DenemoDirective *directive = direc->data;
                  directive->widget = NULL;
                  widget_for_layout_directive (directive);
                }
            }

            cache_all ();


            gui->movement->smf = initial_smf;
            gui->movement->smfsync = -1;      //force recalculation of midi
            gui->movement->redo_invalid = initial_redo_invalid;
            gui->movement->undo_guard = initial_guard;        //we keep all the guards we had on entry which will be removed when
            gui->movement->changecount = initial_changecount;
            position_for_chunk (gui, chunk);// !!!!! this doesn't call goto_movement_staff_obj   //FIXME check return val
            setcurrents (Denemo.project->movement);
            if (!gui->movement->currentmeasure)
              {
                g_warning (
                "positioning after snapshot Bug in select.c  movem %d, staff %d, measure %d, obj = %d, appending %d, offend %d, leftmeas %d\n",
                      chunk->position.movement,
                      chunk->position.staff,
                      chunk->position.measure,
                      chunk->position.object,/**< 0 means no object */
                      chunk->position.appending,/**< if cursor is in appending position */
                      chunk->position.offend,/**< cursor is shown red in this case, obscure connection with appending */
                      chunk->position.leftmeasurenum /**< start at 1 */);
                movetoend (NULL, NULL);
              }
           //without this currentstaffnum is set wrongly, and moveviewport causes a crash.
           gui->movement->currentstaffnum = 1 + g_list_position (gui->movement->thescore, gui->movement->currentstaff);
           XesNeedRecalculating = TRUE;
           signal_structural_change (gui);
          }
        else
          {
            g_critical ("Movement does not exist in list of movements");
          }
      }
      break;
    case ACTION_NOOP:
      break;

    default:
      g_warning ("Unexpected undo case ");
    }
}




static void
position_warning (DenemoUndoData * chunk)
{
  g_warning ("Could not find position for undotype %d  movement %d staff %d measure %d object %d appending %d offend %d", chunk->action, chunk->position.movement, chunk->position.staff, chunk->position.measure, chunk->position.object, chunk->position.appending, chunk->position.offend);
  print_queue ("The undo queue was:", Denemo.project->movement->undodata);
  print_queue ("The redo queue was:", Denemo.project->movement->redodata);
}

static void
warn_no_more_undo (DenemoProject * gui)
{
  g_warning ("No more undo information at level %d guard %d ... resetting", gui->undo_level, gui->movement->undo_guard);
  gui->undo_level = 0;
  gui->movement->undo_guard = Denemo.prefs.disable_undo;
}



static void
free_queue (GQueue * queue)
{
  DenemoUndoData *chunk;
  //g_debug("before redo queue %p is %d empty\n", queue, g_queue_is_empty(queue));
  while ((chunk = (DenemoUndoData *) g_queue_pop_head (queue)))
    free_chunk (chunk);
  //g_debug("after redo queue %p is %d empty\n", queue, g_queue_is_empty(queue));
}

/**
 * undo
 * Undoes an insert, delete change of a DenemoObject, transferring the undo object to the redo queue and switching it between delete/insert
 * Undoes other changes to movement by returning to a snapshot.
 *
 * PARAM gui  the score (why??? this is per movement undo FIXME)
 */
static void
undo (DenemoProject * gui)
{

  DenemoUndoData *chunk = (DenemoUndoData *) g_queue_pop_head (gui->movement->undodata);
  if (chunk)
    {
      gui->movement->undo_guard++;
      //g_debug("undo %d\n", chunk->action);
      if (position_for_chunk (gui, chunk))
        {
          action_chunk (gui, &chunk);
        }
      else
        {
          position_warning (chunk);
          free_queue (gui->movement->redodata);
          free_queue (gui->movement->undodata);
          warn_no_more_undo (gui);      //returns guard to user preference and sets level 0
          return;
        }
      //g_debug("actioned undo now pushing %d\n", chunk->action);
      update_redo_info (gui->movement, chunk);
      gui->movement->undo_guard--;
      //g_debug("***undo guard after undo %d\n",  gui->movement->undo_guard);
      if (gui->undo_level > 0)
        undo (gui);
      score_status (gui, TRUE);
      if (gui->movement->currentmeasurenum > g_list_length (gui->movement->measurewidths))
        {
          g_warning ("Undo failed to set current measurenum %d out of %d", gui->movement->currentmeasurenum, g_list_length (gui->movement->measurewidths));
          gui->movement->currentmeasurenum = g_list_length (gui->movement->measurewidths);
        }
      //print_queue("Undo, queue: ", gui->movement->undodata);
    }
  else
    warn_no_more_undo (gui);

}



/**
 * redo
 * Takes objects from the redo queue and actions them, staged by ACTION_STAGE_START/END
 * Once actioned they are transferred back to the undo queue, with inverse transformation
 *
 * Input
 * scoreinfo - score data
 */
void
redo (DenemoProject * gui)
{
  DenemoUndoData *chunk = (DenemoUndoData *) g_queue_pop_head (gui->movement->redodata);
  if (chunk)
    {
      //g_debug("Before %s and %d\n", gui->movement->currentobject?"Obj":"noObj", gui->movement->cursor_x);
      gui->movement->undo_guard++;
      if (position_for_chunk (gui, chunk))
        {
          action_chunk (gui, &chunk);
        }
      else
        {
          position_warning (chunk);

        }
      update_undo_info (gui->movement, chunk);
      gui->movement->undo_guard--;
      //g_debug("After %s and %d\n", gui->movement->currentobject?"Obj":"noObj!!", gui->movement->cursor_x);
      if (gui->undo_level > 0)
        redo (gui);
      score_status (gui, TRUE);
    }
  else
    warn_no_more_undo (gui);
  cache_all ();
}




/**
 *  update_undo_info
 *
 *  Updates the undo list with current operation.
 *  Is passed score structure and undo_data structure
 *
 */
void
update_undo_info (DenemoMovement * si, DenemoUndoData * undo)
{
  //g_print ("Adding: Action %d at pos %d appending %d\n",  undo->action, undo->position.object, undo->position.appending);

  //  if (g_queue_get_length (si->undodata) == MAX_UNDOS)
  //    {
  //      tmp = g_queue_pop_tail (si->undodata);//FIXME freeing undo info, especially the object
  //      g_warning("Lost undo of %p %p", tmp, tmp->object);
  //   }

  g_queue_push_head (si->undodata, undo);
  si->redo_invalid = TRUE;
  // print_queue("\nUpdate Undo, queue:", si->undodata);
}


/**
 * update_redo_info
 *
 *  Updates the redo list with last undo operation.
 *  Is passed score structure and redo_data structure
 *  @param si pointer to the DenemoMovement structure
 *  @param redo redo data structure to prepend to the queue
g */

void
update_redo_info (DenemoMovement * si, DenemoUndoData * redo)
{
  //print_queue("Update redo ******************\nUndo queue:\n", si->undodata);
  //print_queue("Update redo ******************\nredo queue:\n", si->redodata);


  if (si->redo_invalid)
    {
      free_queue (si->redodata);
      si->redo_invalid = FALSE;
      //g_debug("queue = %p\n", si->redodata);
    }
  g_queue_push_head (si->redodata, redo);
}
