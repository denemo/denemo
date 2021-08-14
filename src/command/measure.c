/* measure.cpp
 * functions dealing with measures
 *
 * for Denemo, a gtk+ frontent to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee
 */

#include "display/accwidths.h"
#include "command/chord.h"
#include <denemo/denemo.h>
#include "display/drawingprims.h"
#include "command/measure.h"
#include "display/notewidths.h"
#include "command/object.h"
#include "command/staff.h"
#include "string.h"
#include "core/utils.h"
#include "core/cache.h"
#include "command/select.h"
#include "display/displayanimation.h"
#include "command/commandfuncs.h"
#include "export/exportmidi.h"

#define STEMDIFFERENCE 6
#define HALFSTEMDIFFERENCE 3


/**
 * Adds measures to the score at given position, and returns the
 * measurenode * to the first one in currentstaff
 * @param si the scoreinfo structure
 * @param pos position in staff to insert measures
 * @param nummeasures number of measures to insert
 * @param all append across all staffs
 * @return measurenode pointer to the first added in the current staff
 */
measurenode *
addmeasures (DenemoMovement * si, gint pos, guint nummeasures, gint all)
{
  staffnode *curstaff;
  guint i;
  DenemoMeasure *newmeasure = NULL;
  if(all)
        stage_undo (si, ACTION_STAGE_END);
  for (i = 0; i < nummeasures; i++)
    {
      if (all)
        {
          gint j;
          for (j = 1, curstaff = si->thescore; curstaff; j++, curstaff = curstaff->next)
            {
              store_for_undo_measure_create (si, j, pos);
              if (Denemo.project->movement->recording && Denemo.project->movement->recording->click_track_created && j==1)
				newmeasure = clone_measure ((DenemoMeasure*)g_list_last(staff_first_measure_node (curstaff))->data);//MIDi track: fill out the measure as duplicate clone_measure
			  else
				newmeasure = g_malloc0 (sizeof (DenemoMeasure));
				
              ((DenemoStaff *) curstaff->data)->themeasures = g_list_insert (staff_first_measure_node (curstaff), newmeasure, pos);
              ((DenemoStaff *) curstaff->data)->nummeasures++;
            }

        }
      else
        {
          store_for_undo_measure_create (si, si->currentstaffnum, pos);
           newmeasure = g_malloc0 (sizeof (DenemoMeasure)); //use NULL  originally
          ((DenemoStaff *) si->currentstaff->data)->themeasures = g_list_insert (staff_first_measure_node (si->currentstaff), newmeasure, pos);
          ((DenemoStaff *) si->currentstaff->data)->nummeasures++;
        }

      gint maxmeasures = 0;
      for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
        {
          maxmeasures = MAX (maxmeasures, ((DenemoStaff *) curstaff->data)->nummeasures);
        }

      if (g_list_length (si->measurewidths) < maxmeasures)
        {
          si->measurewidths = g_list_insert (si->measurewidths, GINT_TO_POINTER (si->measurewidth), pos);
        }


    }
    if (all)
        cache_all();
    else
        cache_staff (si->currentstaff);
  set_measure_transition (-20 * nummeasures, all);
  measurenode *ret = g_list_nth (staff_first_measure_node (si->currentstaff), pos);
//  displayhelper (Denemo.project);
 // score_status(Denemo.project, TRUE);
//check not returning NULL!!!!
  si->cursoroffend = FALSE;//This was left to the drawing routine to set, but it can happen that no draw occurs before the value is needed.

  if(all)
        stage_undo (si, ACTION_STAGE_START);
  if (ret)
    return ret;
  g_warning ("Add measures was going to return NULL");
  if (Denemo.project->movement->recording && Denemo.project->movement->recording->notes)
	exportmidi (NULL, Denemo.project->movement);//recorded notes will not be visible if they come after the old last measure until the timing of this measure is done.
  return g_list_last (staff_first_measure_node (si->currentstaff));
}

measurenode *
dnm_addmeasures (DenemoMovement * si, gint pos, guint nummeasures, gint all)
{
  return addmeasures (si, pos, nummeasures, all);
}


/**
 * Free a measures objects
 *
 */
void
freeobjlist (objnode *delobjs)
{
  if (delobjs)
    {
      /* Free all the Denemo objects */
      g_list_foreach (delobjs, (GFunc) freeobject, NULL);
      /* Free the object list itself */
      g_list_free (delobjs);
    }
}

DenemoMeasure * clone_measure (DenemoMeasure* m)
{ GList *g;
   DenemoMeasure *ret = g_malloc0 (sizeof (DenemoMeasure));
   memcpy (ret, m, sizeof (DenemoMeasure));
   ret->objects = NULL;
   for (g=m->objects;g;g=g->next)
    ret->objects = g_list_append (ret->objects, dnm_clone_object (g->data));
//the cache values will need recalculating depending on how the clone is used.
    return ret;
}

void free_measure (DenemoMeasure *m)
{
  g_list_foreach (m->objects, (GFunc)freeobject, NULL);  
  m->objects = NULL;
}
/**
 * staffremovemeasures
 * Contains common code to remove a measure from a staff
 *
 * @param curstaff the staff to remove the measure from
 * @param pos the position in the staff to remove the measure from
 *
 */
void
staffremovemeasures (staffnode * curstaff, guint pos)
{
  //g_debug ("In Staffremovemeasures\n");
  DenemoMovement *si = Denemo.project->movement;
  measurenode *firstmeasure;
  measurenode *delmeasure;

  firstmeasure = staff_first_measure_node (curstaff);
  delmeasure = g_list_nth (firstmeasure, pos);
  if (delmeasure)
    {

          //  g_debug ("Firstmeasure %x\t DelMeasure %x \t Position\n",
          //       firstmeasure, delmeasure, pos);


        DenemoUndoData *undo;
        if (!si->undo_guard)
            {
              undo = (DenemoUndoData *) g_malloc (sizeof (DenemoUndoData));
              undo->object = clone_measure ((DenemoMeasure*)delmeasure->data);
            }
       // freeobjlist (((DenemoMeasure*)delmeasure->data)->objects, NULL);

        free_measure (delmeasure->data);
        ((DenemoStaff *) curstaff->data)->themeasures = g_list_remove_link (firstmeasure, delmeasure); //FIXME DANGER
        g_free ((DenemoMeasure*)delmeasure->data);
        g_list_free_1 (delmeasure);
        ((DenemoStaff *) curstaff->data)->nummeasures--;
        if ( ((DenemoStaff *) curstaff->data)->themeasures != NULL)
            {//if the removed measures have a clef change in them the noteheights may need to change so...
            cache_staff (curstaff);
            staff_fix_note_heights (curstaff->data);
            }
        if (!si->undo_guard)
            {
              get_position (si, &undo->position);
              //!!!!!!!! that is setting the cursor position
              undo->position.staff = 1 + g_list_position (si->thescore, curstaff);
                //g_print ("Setting staff %d\n", undo->position.staff);
              undo->action = ACTION_MEASURE_DELETE;
              update_undo_info (si, undo);
            }
    }
 else
    g_warning ("Request to delete non-existent measure %d", pos);
}

/**
 * Remove measures at given position, and return an appropriate
 * currentmeasure
 *
 * @param si pointer to the scoreinfo structure
 * @param pos position to remove the measures from
 * @param nummeasures number of measures to remove
 * @param all remove from all staffs
 */
measurenode *
removemeasures (DenemoMovement * si, guint pos, guint nummeasures, gboolean all)
{
  staffnode *curstaff;
  measurenode *firstmeasure;
  GList *temp;
  guint totalmeasures = 0;
  guint i;

  if (nummeasures <= g_list_length (staff_first_measure_node ((staffnode *) si->currentstaff)) - pos)
    {
      if(all)
        stage_undo (si, ACTION_STAGE_END);
      for (i = 0; i < nummeasures; i++)
        {
          totalmeasures = 0;
          for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
            {
              if (curstaff == si->currentstaff || all)
                {
                  staffremovemeasures (curstaff, pos);
                  if (!staff_first_measure_node (curstaff))
                    {
                      ((DenemoStaff *) curstaff->data)->themeasures = g_list_append (NULL, g_malloc0(sizeof (DenemoMeasure)));
                      ((DenemoStaff *) curstaff->data)->nummeasures = 1;
                    }
                }
            }
          for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
            {
              totalmeasures = MAX (totalmeasures, ((DenemoStaff *) curstaff->data)->nummeasures);
            }

          if (totalmeasures <= (g_list_length (si->measurewidths) - 1))
            {
              /* And get rid of the no-longer-needed width data too */
              temp = g_list_nth (si->measurewidths, pos);
              si->measurewidths = g_list_remove_link (si->measurewidths, temp);
              g_list_free_1 (temp);
            }
        }
      set_measure_transition (20, all);
      all?  cache_all (): cache_staff (si->currentstaff);
      if(all)
        stage_undo (si, ACTION_STAGE_START);
    }
  else
    {
      g_warning (_("removemeasures: received request to delete more measures\
                   than exist.  Junking request."));
      return si->currentmeasure;
    }
  firstmeasure = staff_first_measure_node (si->currentstaff);
  if (pos == g_list_length (staff_first_measure_node ((staffnode *) si->currentstaff)))
    {
      /* That is, we deleted the last measure */
      si->currentmeasurenum--;
      return g_list_nth (firstmeasure, pos - 1);
    }
  else
    return g_list_nth (firstmeasure, pos);
}


/**
 * This function calculates the number of ticks per beat in a given
 * time signature
 */
gint
calcticksperbeat (gint time1, gint time2)
{
  gint ret = WHOLE_NUMTICKS / time2;

  /* If time1 is divisible by three and time2 is greater than 4;
   * e.g.,the time signature is 3/8, 15/16, etc., it's assumed that
   * the beat group is actually 3 times what's above */

  if (time2 > 4 && time1 % 3 == 0)
    ret *= 3;

  return ret;
}


/* looks at succeeding objects to see if the current object is the last chord in a grace
 returns ENDGRACE if it is */
static gint
is_end_grace (objnode * curobjnode)
{
  for (; curobjnode->next; curobjnode = curobjnode->next)
    {
      DenemoObject *obj = (DenemoObject *) curobjnode->next->data;
      if (obj->type == CHORD)
        {
          if (((chord *) obj->object)->is_grace)
            return 0;
          else
            return ENDGRACE;
        }
    }
  return ENDGRACE;
}


/**
 * This function goes through a measure and properly sets
 * durinticks and starttickofnextnote values for everything in
 * that measure, tuplets or no.
 * It also marks the end of grace groups
 *
 * It works out that this function can be called wherever
 * calculatebeamsandstemdirs is invoked, and would share code besides, so
 * that's precisely where it is invoked
 */
static void
settickvalsinmeasure (objnode * theobjs)
{
  gint numerator = 1, denominator = 1;
  objnode *curobjnode;
  DenemoObject *theobj;
  gint ticks_so_far = 0;
  gint basic_ticks_in_tuplet_group = 0;
  gboolean in_tuplet = FALSE;

  for (curobjnode = theobjs; curobjnode; curobjnode = curobjnode->next)
    {
      theobj = (DenemoObject *) curobjnode->data;
      theobj->starttick = ticks_so_far + (basic_ticks_in_tuplet_group * numerator / denominator);

      if (theobj->type == CHORD)
        {
          chord *thechord = ((chord *) theobj->object);
          if (in_tuplet)
            {
              if (!((chord *) theobj->object)->is_grace)
                {
                  set_tuplefied_numticks (theobj, numerator, denominator);
                  basic_ticks_in_tuplet_group += theobj->basic_durinticks;
                }  else
                {
                    if (!(thechord->is_grace & DURATION_SET))
                      theobj->durinticks = 0;
                    if(is_end_grace (curobjnode))
                      thechord->is_grace |= ENDGRACE;
                    else
                      thechord->is_grace &= (~ENDGRACE);    //and re-instate if needed
                }


            }
          else
            {

              thechord->is_grace &= (GRACED_NOTE | ACCIACCATURA | DURATION_SET);     //leave any fixed grace, changed by toggle.

              if (thechord->is_grace)
                {
                    if (!(thechord->is_grace & DURATION_SET))
                      theobj->durinticks = 0;
                    if(is_end_grace (curobjnode))
                      thechord->is_grace |= ENDGRACE;
                    else
                      thechord->is_grace &= (~ENDGRACE);    //and re-instate if needed
                }
              else
                theobj->durinticks = theobj->basic_durinticks;
              ticks_so_far += theobj->durinticks;
            }
        }
      else if (theobj->type == TUPOPEN)
        {
          in_tuplet = TRUE;
          numerator = ((tupopen *) theobj->object)->numerator;
          denominator = ((tupopen *) theobj->object)->denominator;
          /* basic_ticks_in_tuplet_group = 0; does not work when nested tuplets are used */
        }
      else if (theobj->type == TUPCLOSE)
        {
          in_tuplet = FALSE;
          ticks_so_far += ((basic_ticks_in_tuplet_group * numerator) / denominator);
          numerator = 1;
          denominator = 1;
          basic_ticks_in_tuplet_group = 0;
        }
      else if (theobj->type == LILYDIRECTIVE)
        ticks_so_far += theobj->durinticks;
      theobj->starttickofnextnote = ticks_so_far + (basic_ticks_in_tuplet_group * numerator / denominator);

      //this goes up too fast for grace notes...

      //g_debug("start tick next %d\n",       theobj->starttickofnextnote);
    }
}

/**
 * This function simply sets stem directions. It probably deals with
 * staves that have a fixed stem direction inefficiently, but this was
 * the easiest way to add things to the existing code.
 */
static void
setsdir (objnode * starter, objnode * ender, gint beamgroup_sum, gint beamgroup_number, gint beamgroup_highest, gint beamgroup_lowest, gint clef, gint stem_directive)
{
  objnode *curobjnode;
  DenemoObject *theobj;
  gint avgoffset = beamgroup_number ? beamgroup_sum / beamgroup_number : 0;
  gint avgheight = calculateheight (avgoffset, clef);
  gboolean is_stemup = TRUE;
  gint stemoffset;
  gint stemy;
#if 0
  {
    static gint count = 0;
    count++;
    g_debug ("Call %d ++++++++++++++++++++++++++++++++\n\
          Stem directive %s\n\
          Clef %d\n\
          ------------------------------\n", count, stem_directive == 2 ? "Neutral" : stem_directive == 1 ? "Down" : "Up", clef);
  }
#endif

  switch (stem_directive)
    {
    case DENEMO_STEMUP:
      is_stemup = TRUE;
      break;
    case DENEMO_STEMBOTH:
      is_stemup = avgheight > MID_STAFF_HEIGHT;
      break;
    case DENEMO_STEMDOWN:
      is_stemup = FALSE;
      break;
    }
  theobj = (DenemoObject *) starter->data;

  if (theobj->type == CHORD && (((chord *) theobj->object)->is_grace))
    is_stemup = TRUE;

  if (is_stemup)
    stemoffset = MAX (beamgroup_lowest + 7, beamgroup_highest + 5);
  else
    stemoffset = MIN (beamgroup_highest - 7, beamgroup_lowest - 5);
  stemy = calculateheight (stemoffset, clef);

  /* Okay; now that we've got everything calculated, just roll through
   * the measure and set stem heights. */

  for (curobjnode = starter; curobjnode != ender->next; curobjnode = curobjnode->next)
    {
      theobj = (DenemoObject *) curobjnode->data;
      if (theobj->type == CHORD)
        {
          if ((((chord *) theobj->object)->baseduration == 0) || (((chord *) theobj->object)->baseduration <-7))
            /* Whole notes are always laid out stemup */
            ((chord *) theobj->object)->is_stemup = TRUE;
          else
            {
              if (((chord *) theobj->object)->is_grace)
                ((chord *) theobj->object)->is_stemup = TRUE;
              else
                ((chord *) theobj->object)->is_stemup = is_stemup;

              ((chord *) theobj->object)->stemy = stemy;
            }
          findreversealigns (theobj);
        }
    }
}

/**
 * This function takes all these int *s in so that appropriate values
 * can be fed into the function when it's called again for the
 * next measure -- see staff_beams_and_stems_dirs for details
  */
void
calculatebeamsandstemdirs (DenemoMeasure *measure)
{
  if (measure == NULL)
    return;
  objnode * theobjs = measure->objects;
  DenemoObject *prevobj = NULL, *theobj;
  objnode *curobjnode, *starter = NULL;
  chord chordval;
  gint beatendsat, ticksperbeat;
  gint beamgroup_sum = 0;       /* Sum of mid_c_offsets in the beamgroup */
  gint beamgroup_number = 0;
  gint beamgroup_highest = 0;
  gint beamgroup_lowest = 0;


  gboolean isbeambreak;
  gint  theclef = measure->clef->type;
  gint  thetime1 = measure->timesig->time1;
  gint  thetime2 = measure->timesig->time2;
  gint  thestem_directive = measure->stemdir->type;
  gint next_clef = theclef;      /* Useful for when a clef intrudes
                                   mid-beamgroup */
  gint next_stem_directive = thestem_directive;

  if (theobjs==NULL)
    return;
#if 0
  {
    static gint count = 0;
    count++;
    gint stem = *stem_directive;
    g_debug ("Call calc %d #################################\n\
          Stem directive %s\n\
          Clef %d\n\
          ------------------------------\n", count, stem == 2 ? "Neutral" : stem == 1 ? "Down" : "Up", next_clef);
  }
#endif

  ticksperbeat = calcticksperbeat (thetime1, thetime2);
  settickvalsinmeasure (theobjs);
  beatendsat = ticksperbeat;

  for (curobjnode = theobjs; curobjnode; prevobj = theobj, curobjnode = curobjnode->next)
    {
      theobj = (DenemoObject *) curobjnode->data;
      isbeambreak = (theobj->type == CHORD) && (!((chord *) theobj->object)->notes || ((chord *) theobj->object)->is_grace);
      if (theobj->type != CHORD || isbeambreak)
        {
          /* A non-chord or rest always breaks up a beam group */
          /* LilyPond directives can have their own behaviour,
             starting with *not* forcing beam breaks */
          /* if(theobj->type != LILYDIRECTIVE) */
          {
            theobj->isstart_beamgroup = TRUE;
            theobj->isend_beamgroup = TRUE;
          }

          switch (theobj->type)
            {
            case CLEF:
              next_clef = ((clef *) theobj->object)->type;
              break;
            case STEMDIRECTIVE:
              next_stem_directive = ((stemdirective *) theobj->object)->type;
              break;
            default:
              break;
            }
        }
      else
        {
          /* Determine whether this is the start or end of another
           * beam group.  Quarter notes or longer automatically are. */
          if (((chord *) theobj->object)->baseduration <= 2)
            theobj->isstart_beamgroup = theobj->isend_beamgroup = TRUE;
          else                  /* otherwise... */
            {
              if (prevobj)
                theobj->isstart_beamgroup = prevobj->isend_beamgroup;
              else
                theobj->isstart_beamgroup = TRUE;

              /* Does this note occupy a beat boundary. i.e., it's dotted,
               * syncopated, whatever? If so, then it's its own beamgroup. */
              if (theobj->starttickofnextnote > beatendsat)
                theobj->isstart_beamgroup = theobj->isend_beamgroup = TRUE;
              /* Does it end exactly on the beat? Then it's the end of
               * the beamgroup */
              else if (theobj->starttickofnextnote == beatendsat)
                theobj->isend_beamgroup = TRUE;
              /* Is it the last note in the measure */
              else if (!curobjnode->next)
                theobj->isend_beamgroup = TRUE;
              /* Okay. So it's not the end of the beamgroup */
              else
                theobj->isend_beamgroup = FALSE;
            }                   /* End inner else */
        }                       /* End outer else */

      /* Update beatendsat to reflect the bit of music that's just
       * been tacked on */

      while (theobj->starttickofnextnote >= beatendsat)
        beatendsat += ticksperbeat;

      /* Backtrack a little bit -- we may not have known that prevobj
       * was the end of the preceding beamgroup until just now. If it
       * is, set the stem direction and such for the preceding
       * beamgroup. */
      if (prevobj && !prevobj->isend_beamgroup && theobj->isstart_beamgroup)
        {
          prevobj->isend_beamgroup = TRUE;
          setsdir (starter, curobjnode->prev, beamgroup_sum, beamgroup_number, beamgroup_highest, beamgroup_lowest, theclef, thestem_directive);
        }

      /* Now that we've determined this note's status, what to actually
       * do about it: */

      theclef = theobj->clef->type;
      thestem_directive = theobj->stemdir->type;

      if (theobj->isstart_beamgroup)
        {
          starter = curobjnode;
          beamgroup_sum = beamgroup_number = 0;
          beamgroup_highest = G_MININT;
          beamgroup_lowest = G_MAXINT;
        }
      if (theobj->type == CHORD)
        {
          chordval = *(chord *) theobj->object;
          beamgroup_sum += chordval.sum_mid_c_offset;
          beamgroup_number += g_list_length (chordval.notes);
          beamgroup_highest = MAX (beamgroup_highest, chordval.highestpitch);
          beamgroup_lowest = MIN (beamgroup_lowest, chordval.lowestpitch);
        }
      if (theobj->isend_beamgroup)
        {
          setsdir (starter, curobjnode, beamgroup_sum, beamgroup_number, beamgroup_highest, beamgroup_lowest, theclef, thestem_directive);
        }
    }                           /* End object loop */
}                               /* End function */





#define ACCS_TOO_CLOSE 10
/**
 * This function offsets accidentals that are near to each
 * other on the same chord.
 */
void
set_accidental_positions (DenemoObject * the_chord)
{
  GList *current;
  note *current_note;
  gint columns[ACCS_TOO_CLOSE];
  gint column_widths[ACCS_TOO_CLOSE];
  gint column_positions[ACCS_TOO_CLOSE];
  gint i;
  chord chordval = *(chord *) the_chord->object;
  gint baseduration = chordval.baseduration;
  if (baseduration<-7)
    baseduration = 0;
  if (baseduration < 0)
    baseduration = -baseduration;
  if (g_list_length (chordval.notes) > ACCS_TOO_CLOSE)
    return;

  baseduration = MAX (baseduration, 0);
  gint additional_space = ((!chordval.is_stemup && chordval.is_reversealigned) ? headwidths[MIN (baseduration, 2)] : 0);


  for (i = 0; i < ACCS_TOO_CLOSE; i++)
    {
      columns[i] = G_MAXINT;
      column_widths[i] = 0;
    }

  /* First pass through notes: assign accidentals to numerical
     columns: 0 -> closest to noteheads, ACCS_TOO_CLOSE - 1 ->
     furthest away.  Store this value in position_of_accidental, though
     it will be replaced fairly quickly.  */

  for (current = g_list_last (chordval.notes); current; current = current->prev)
    {
      current_note = (note *) current->data;
      if (current_note->showaccidental)
        {
          for (i = 0; columns[i] < current_note->mid_c_offset + ACCS_TOO_CLOSE; i++)
            ;
          current_note->position_of_accidental = i;
          columns[i] = current_note->mid_c_offset;
          column_widths[i] = MAX (column_widths[i], accwidths[current_note->enshift + 2]);
        }
    }

  /* Second pass: go through the notes again and replace
     position_of_accidental with a more useful value.  */

  column_positions[0] = (column_widths[0] + additional_space + EXTRABACKOFF);
  for (i = 1; i < ACCS_TOO_CLOSE; i++)
    column_positions[i] = (column_positions[i - 1] + column_widths[i] + EXTRABACKOFF);
  for (current = chordval.notes; current; current = current->next)
    {
      current_note = (note *) current->data;
      if (current_note->showaccidental)
        current_note->position_of_accidental = column_positions[current_note->position_of_accidental];
    }
}


#define UNSET -3
#define CONTRADICTED 3

/**
 * Calculate which accidentials should be shown
 * for each note of each chord of the measure whose list of objects is passed in
 * It also changes minpixelsalloted value of keysginatures to cope with the varying size (dependent on previous keysignature) by calling draw_key in dry-run mode.
 */
void
showwhichaccidentals (objnode * theobjs)
{
  if(theobjs==NULL) return;
  gint curkey;
  gint * initialaccs;
  gint whatpersisted[7];
  static gint initialaccsthischord[7] = { UNSET, UNSET, UNSET, UNSET, UNSET, UNSET, UNSET };
  gint accsthischord[7];
  gboolean freshthischord[7];
  gboolean contradicted[7];
  gint otn;                     /* offsettonumber */
  objnode *curobjnode;
  DenemoObject *theobj = (DenemoObject *) theobjs->data;;
  GList *curtone;
  note *thetone;
  gint ret[7];
  gint i;

  keysig *thekeysig = theobj->keysig;
  curkey = thekeysig->number;
  initialaccs = thekeysig->accs;

  memcpy (ret, initialaccs, SEVENGINTS);
  memcpy (whatpersisted, initialaccs, SEVENGINTS);

  for (curobjnode = theobjs; curobjnode; curobjnode = curobjnode->next)
    {
      theobj = (DenemoObject *) curobjnode->data;

      if (theobj->type == CHORD)
        {
          ((chord *) theobj->object)->hasanacc = FALSE;
          memcpy (accsthischord, initialaccsthischord, SEVENGINTS);
          memset (freshthischord, 0, SEVENGINTS);
          memset (contradicted, 0, SEVENGINTS);
          /* First loop through chord - looks for conflicting values
           * for the same note */
          for (curtone = ((chord *) theobj->object)->notes; curtone; curtone = curtone->next)
            {
              thetone = (note *) curtone->data;
              otn = offsettonumber (thetone->mid_c_offset);
              if (thetone->enshift != whatpersisted[otn])
                {
                  freshthischord[otn] = TRUE;
                  whatpersisted[otn] = thetone->enshift;
                }
              if (accsthischord[otn] == UNSET)
                accsthischord[otn] = thetone->enshift;
              else if (accsthischord[otn] != thetone->enshift)
                {
                  contradicted[otn] = TRUE;
                  whatpersisted[otn] = CONTRADICTED;
                }
            }                   /* End first loop through chord */
          /* Now loop through the chord again, setting note->showaccidental
           * appropriately */
          for (curtone = ((chord *) theobj->object)->notes; curtone; curtone = curtone->next)
            {
              thetone = (note *) curtone->data;
              otn = offsettonumber (thetone->mid_c_offset);
              if (contradicted[otn])
                /* We've got conflicting accidentals for the same pitch */
                thetone->showaccidental = ((chord *) theobj->object)->hasanacc = TRUE;
              else if (freshthischord[otn])
                /* A new accidental not present in the original chord */
                thetone->showaccidental = ((chord *) theobj->object)->hasanacc = TRUE;
              else
                thetone->showaccidental = FALSE;


              // FIXME - you should use a script to apply these directives & set hasnacc with that.
              if (thetone->directives && ((DenemoDirective *) thetone->directives->data)->postfix && (*((DenemoDirective *) thetone->directives->data)->postfix->str == '!' || *((DenemoDirective *) thetone->directives->data)->postfix->str == '?'))
                thetone->showaccidental = ((chord *) theobj->object)->hasanacc = (*((DenemoDirective *) thetone->directives->data)->postfix->str == '?') ? DENEMO_CAUTIONARY : DENEMO_REMINDER;

            }                   /* End second loop through chord */
          set_accidental_positions (theobj);
          setpixelmin (theobj);
        }                       /* End if chord */
      else if (theobj->type == KEYSIG)
        {
          for (i = 0; i < 7; i++)
            initialaccsthischord[i] = UNSET;
            memcpy (ret, ((keysig *) theobj->object)->accs, SEVENGINTS);
            memcpy (whatpersisted, ret, SEVENGINTS);
            theobj->minpixelsalloted = draw_key (NULL, 0, 0, ((keysig *) theobj->object)->number, curkey, 0, FALSE, (keysig *) theobj->object);
            curkey = ((keysig *) theobj->object)->number;
        }

    }                           /* End object loop */
  //memcpy (initialaccs, ret, SEVENGINTS);

}

/**
 *  Force and accidental to be shown on the score.
 *
 * @param theobj the DenemoObject to force the accidential on
 */
void
forceaccidentals (DenemoObject * theobj)
{
  GList *curtone;
  note *thetone;

  for (curtone = ((chord *) theobj->object)->notes; curtone; curtone = curtone->next)
    {
      thetone = (note *) curtone->data;
      thetone->showaccidental = TRUE;
    }
  ((chord *) theobj->object)->hasanacc = TRUE;
  set_accidental_positions (theobj);
  setpixelmin (theobj);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}

/**
 * Return the first object node of the given measure
 * @param mnode a measurenode
 * @return the first object node of the measure
 */
objnode *
measure_first_obj_node (measurenode * mnode)
{
  return mnode?(objnode *) ((DenemoMeasure*)mnode->data)->objects:NULL; //FIXME DANGER was expecting a node with NULL data for the first object in the case of an empty measure.
}

/**
 * Return the last object node of the given measure
 * @param mnode a measurenode
 * @return the last object node of the measure
 */
objnode *
measure_last_obj_node (measurenode * mnode)
{
  return g_list_last ((objnode *) ((DenemoMeasure*)mnode->data)->objects);
}
