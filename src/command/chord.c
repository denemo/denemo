/* chord.c
 *
 * functions which manipulate chords
 * For denemo, a gtk+ frontend to Lilypond, the GNU music typesetter
 *
 * (c) 2000-2005  Matthew Hiller, Adam Tee
 *
 */

#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include "command/object.h"
#include "command/chord.h"
#include "core/utils.h"
#include "audio/audiointerface.h"
#include "command/commandfuncs.h"

/* Calculates the height of a notehead */
static void
calcheight (gpointer data, gpointer user_data)
{
  note *thenote = (note *) data;
  gint dclef = GPOINTER_TO_INT (user_data);

  thenote->y = calculateheight (thenote->mid_c_offset, dclef);
}

/**
 *  Changes the position of the chord when a new clef
 *  is selected
 *
 */
void
newclefify (DenemoObject * thechord)
{
  if (((chord *) thechord->object)->notes == NULL)
    return;
  gint dclef = thechord->clef->type;
  g_list_foreach (((chord *) thechord->object)->notes, calcheight, GINT_TO_POINTER (dclef));
  ((chord *) thechord->object)->highesty = calculateheight (((chord *) thechord->object)->highestpitch, dclef);
  ((chord *) thechord->object)->lowesty = calculateheight (((chord *) thechord->object)->lowestpitch, dclef);
}

/**
 *  This function goes through a chord and checks to see how its notes are
 * laid out, and if it will have to push the display of any notes to the
 * "wrong" side of the stem as a result
 * the result is to set the is_reversealigned field of the chord
 * and the reversealign field of the note* structures that make up the chord.
 * thechord must have type CHORD on entry.
 */
void
findreversealigns (DenemoObject * thechord)
{
  GList *current;
  GList *previous;
  note *curnote;
  note *prevnote;

  ((chord *) thechord->object)->is_reversealigned = FALSE;
  if (((chord *) thechord->object)->notes)
    {
      if (((chord *) thechord->object)->is_stemup)
        {
          /* note clusters are painted left-right from bottom to top */
          previous = ((chord *) thechord->object)->notes;
          current = previous->next;
          prevnote = (note *) previous->data;
          prevnote->reversealign = FALSE;
          for (; current; previous = current, prevnote = curnote, current = current->next)
            {
              curnote = (note *) current->data;
              if ((prevnote->mid_c_offset == curnote->mid_c_offset - 1) || (prevnote->mid_c_offset == curnote->mid_c_offset))
                {
                  ((chord *) thechord->object)->is_reversealigned = TRUE;
                  curnote->reversealign = !prevnote->reversealign;
                }
              else
                curnote->reversealign = FALSE;
            }                   /* End for */
        }
      else
        {
          /* the stem's down
           * note clusters are painted right-left from top to bottom */
          previous = g_list_last (((chord *) thechord->object)->notes);
          current = previous->prev;
          prevnote = (note *) previous->data;
          prevnote->reversealign = FALSE;
          for (; current; previous = current, prevnote = curnote, current = current->prev)
            {
              curnote = (note *) current->data;
              if ((prevnote->mid_c_offset == curnote->mid_c_offset + 1) || (prevnote->mid_c_offset == curnote->mid_c_offset))
                {
                  curnote->reversealign = !prevnote->reversealign;
                  ((chord *) thechord->object)->is_reversealigned = TRUE;
                }
              else
                curnote->reversealign = FALSE;
            }                   /* End for */
        }                       /* End else */
    }
  setpixelmin (thechord);
}

/**
 * Allocate new chord from the heap
 * and do the basic initialisation
 *
 */
DenemoObject *
newchord (gint baseduration, gint numdots, int tied)
{
  DenemoObject *thechord = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  chord *newchord = (chord *) g_malloc0 (sizeof (chord));
  thechord->type = CHORD;
  thechord->isinvisible = FALSE;

  newchord->notes = NULL;
  newchord->dynamics = NULL;
  newchord->highestpitch = G_MININT;
  newchord->lowestpitch = G_MAXINT;
  newchord->baseduration = baseduration;
  newchord->numdots = numdots;
  newchord->sum_mid_c_offset = 0;
  newchord->numnotes = 0;
  newchord->is_tied = tied;
  newchord->is_reversealigned = FALSE;
  newchord->slur_begin_p = FALSE;
  newchord->slur_end_p = FALSE;

  newchord->crescendo_begin_p = FALSE;
  newchord->crescendo_end_p = FALSE;
  newchord->diminuendo_begin_p = FALSE;
  newchord->diminuendo_end_p = FALSE;
  newchord->hasanacc = FALSE;
  newchord->is_grace = FALSE;
  newchord->struck_through = FALSE;
  newchord->has_dynamic = FALSE;
  newchord->is_syllable = FALSE;
  newchord->center_lyric = FALSE;
  newchord->lyric = NULL;
  newchord->figure = NULL;
  thechord->object = newchord;

  set_basic_numticks (thechord);

  //g_debug ("Chord %d \n", ((chord *) (thechord->object))->baseduration);
  return thechord;
}

DenemoObject *
dnm_newchord (gint baseduration, gint numdots, int tied)
{
  return newchord (baseduration, numdots, tied);
}

/**
 * Set the invisible flag for the DenemoObject
 * @param thechord object to make invisible
 * @return the chord
 */
DenemoObject *
hidechord (DenemoObject * thechord)
{
  thechord->isinvisible = TRUE;

  return thechord;
}


/**
 * compare current note with pitch to be added
 *
 * if equal return FALSE(0) else return TRUE (1)
 */
#if 0
static gint
findcomparefunc (gconstpointer a, gconstpointer b)
{
  const note *anote = (note *) a;
  const int bnum = GPOINTER_TO_INT (b);

  if (anote->mid_c_offset == bnum)
    return 0;                   /* Identical */
  else
    return 1;                   /* Not identical */
}
#endif

/**
 * Compare two notes
 * used for sorting the currentchords
 * note list
 */
static gint
insertcomparefunc (gconstpointer a, gconstpointer b)
{
  const note *anote = (note *) a;
  const note *bnote = (note *) b;

  return anote->mid_c_offset - bnote->mid_c_offset;
}

/* modify the pitch of the note of a one note chord.
   (for multi-note chords delete and add a note, do not use this).
FIXME repeated calls to calculateheight
*/
void
modify_note (chord * thechord, gint mid_c_offset, gint enshift, gint dclef)
{
  note *thenote;
  if(thechord->notes == NULL)
    {
        g_critical ("modify_note called with rest");
        gdk_beep();
        return;
    }
  thenote = (note *) (thechord->notes->data);
  thenote->mid_c_offset = mid_c_offset;
  thenote->enshift = enshift;
  thenote->y = calculateheight (mid_c_offset, dclef);
  thechord->sum_mid_c_offset = mid_c_offset;    //Damned difficult to track this down - will not work if there are >1 notes in chord
  thechord->highestpitch = mid_c_offset;
  thechord->highesty = calculateheight (mid_c_offset, dclef);
  thechord->lowestpitch = mid_c_offset;
  thechord->lowesty = calculateheight (mid_c_offset, dclef);
  if (Denemo.project->last_source == INPUTKEYBOARD)
    {
      DenemoStaff *curstaffstruct = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
      if (Denemo.prefs.immediateplayback)
        {
          play_notes (DEFAULT_BACKEND, curstaffstruct->midi_port, curstaffstruct->midi_channel, thechord);
        }
    }
  displayhelper (Denemo.project);
}

/* Allocate a new note structure initializing the fields
 * caller must g_free
 */

static note *
new_note (gint mid_c_offset, gint enshift, gint dclef)
{
  note *newnote;
  if (enshift > 2)
    enshift = 2;
  if (enshift < -2)
    enshift = -2;
  newnote = (note *) g_malloc0 (sizeof (note));
  newnote->mid_c_offset = mid_c_offset;
  newnote->enshift = enshift;
  newnote->y = calculateheight (mid_c_offset, dclef);
  newnote->reversealign = FALSE;
  newnote->noteheadtype = DENEMO_NORMAL_NOTEHEAD;
  return newnote;
}

/**
 * Add note to the current chord
 * The note *will* get added if it is
 * present already
 * return note added
 */
note *
addtone (DenemoObject * thechord, gint mid_c_offset, gint enshift)
{ gint dclef = thechord->clef->type;
  note *newnote = new_note (mid_c_offset, (Denemo.project->movement?Denemo.project->movement->pending_enshift:0) + enshift, dclef);
  if(Denemo.project->movement) Denemo.project->movement->pending_enshift = 0;
  ((chord *) thechord->object)->notes = g_list_insert_sorted (((chord *) thechord->object)->notes, newnote, insertcomparefunc);
  if (mid_c_offset > ((chord *) thechord->object)->highestpitch)
    {
      ((chord *) thechord->object)->highestpitch = mid_c_offset;
      ((chord *) thechord->object)->highesty = calculateheight (mid_c_offset, dclef);
    }
  if (mid_c_offset < ((chord *) thechord->object)->lowestpitch)
    {
      ((chord *) thechord->object)->lowestpitch = mid_c_offset;
      ((chord *) thechord->object)->lowesty = calculateheight (mid_c_offset, dclef);
    }
  ((chord *) thechord->object)->sum_mid_c_offset += mid_c_offset;
  ((chord *) thechord->object)->numnotes++;

  return newnote;
}

void
dnm_addtone (DenemoObject * thechord, gint mid_c_offset, gint enshift)
{
  addtone (thechord, mid_c_offset, enshift);
}

/**
 * This function finds the node of the closest chord tone to n; in the
 * case of equally distant chord notes, it'll select the higher notes
 * of the two
 * returns NULL in the case of no notes
 */

/* I don't think that I could have quite done this with a g_list_find_custom */

static GList *
findclosest (GList * notes, gint n)
{
  GList *cur_tnode = notes;
  GList *next_tnode;
  note *cur_tone;
  note *next_tone;
  gint distance_from_cur;
  gint distance_from_next;

  if (!cur_tnode)
    return NULL;
  for (next_tnode = cur_tnode->next;; cur_tnode = next_tnode, next_tnode = cur_tnode->next)
    {
      cur_tone = (note *) cur_tnode->data;
      if (n <= cur_tone->mid_c_offset || !next_tnode)
        /* Aha! We have no other options */
        return cur_tnode;
      else
        {
          next_tone = (note *) next_tnode->data;
          if (cur_tone->mid_c_offset < n && n < next_tone->mid_c_offset)
            {
              distance_from_cur = n - cur_tone->mid_c_offset;
              distance_from_next = next_tone->mid_c_offset - n;
              if (distance_from_cur < distance_from_next)
                return cur_tnode;
              else
                return next_tnode;
            }
        }
    }                           /* End for loop */
}

objnode *
nearestnote (DenemoObject * thechord, gint mid_c_offset)
{
  if (thechord && thechord->object && thechord->type == CHORD && ((chord *) thechord->object)->notes)
    return findclosest (((chord *) thechord->object)->notes, mid_c_offset);
  else
    return NULL;
}

/**
 * Remove tone from current chord
 * return TRUE if a note is removed,
 * false if chord is a rest.
 */
gboolean
removetone (DenemoObject * thechord, gint mid_c_offset)
{
  GList *tnode;                 /* Tone node to remove */
  note *tone;
  gint dclef = thechord->clef->type;
  tnode = findclosest (((chord *) thechord->object)->notes, mid_c_offset);
  if (tnode)
    {
      tone = (note *) tnode->data;
      if (!tnode->next)         /* That is, we're removing the highest pitch */
        {
          if (tnode->prev)
            {
              ((chord *) thechord->object)->highestpitch = ((note *) tnode->prev->data)->mid_c_offset;
              ((chord *) thechord->object)->highesty = calculateheight (((chord *) thechord->object)->highestpitch, dclef);
            }
          else
            {
              ((chord *) thechord->object)->highestpitch = G_MININT;
              /* Had to take care of this somewhere - perhaps not needed - when passing through an edit with no notes...
                 ((chord *) thechord->object)->is_tied = FALSE; */
            }
        }
      if (!tnode->prev)         /* That is, we're removing the lowest pitch */
        {
          if (tnode->next)
            {
              ((chord *) thechord->object)->lowestpitch = ((note *) tnode->next->data)->mid_c_offset;
              ((chord *) thechord->object)->lowesty = calculateheight (((chord *) thechord->object)->lowestpitch, dclef);
            }
          else
            ((chord *) thechord->object)->lowestpitch = G_MAXINT;
        }
      ((chord *) thechord->object)->sum_mid_c_offset -= tone->mid_c_offset;

      /* Now that we no longer need any info in tnode or tone,
       * actually free stuff */

      g_free (tone);
      ((chord *) thechord->object)->notes = g_list_remove_link (((chord *) thechord->object)->notes, tnode);
      g_list_free_1 (tnode);
    }

  if (((chord *) thechord->object)->notes==NULL)
     ((chord *) thechord->object)->highesty = ((chord *) thechord->object)->lowesty = 0;
  return (gboolean) (intptr_t) tnode;
}

//void
//removeallnotes(DenemoObject * thechord) {
//  while(removetone(thechord, 0, 0))
//      ;
//}

/**
 * Set the accidental of note closest to mid_c_offset in
 * the currentchord accidental = -2 ... +2 for double flat to double sharp
 */
void
changeenshift (DenemoObject * thechord, gint mid_c_offset, gint accidental)
{
  GList *tnode;                 /* note node to inflect */
  note *tone;
  tnode = findclosest (((chord *) thechord->object)->notes, mid_c_offset);
  if (tnode)
    {
      tone = (note *) tnode->data;
      tone->enshift = accidental;
      displayhelper (Denemo.project);
    }
}


/**
 * Alter the pitch the  note closest to mid_c_offset in
 * the currentchord by setting
 * the accidental value
 * one sharper or flatter subject to a limit of double sharp/flat
 */
void
shiftpitch (DenemoObject * thechord, gint mid_c_offset, gint amount)
{
  GList *tnode;                 /* Tone node to inflect */
  note *tone;

  tnode = findclosest (((chord *) thechord->object)->notes, mid_c_offset);
  if (tnode)
    {
      tone = (note *) tnode->data;
    
     tone->enshift = tone->enshift + amount; // the , 2; here was a bug, why did the compiler ignore it?
     if (tone->enshift > 2)
        tone->enshift = 2;
    else if (tone->enshift < -2)
        tone->enshift = -2;
    }
}


/**
 * Change the duration of the current chord
 *
 */
void
changedur (DenemoObject * thechord, gint baseduration, gint numdots)
{
  gint current = ((chord *) thechord->object)->baseduration;
  if (current < 0)
    {
      if (((chord *) thechord->object)->directives)
        {
          GList *g;
          for (g = ((chord *) thechord->object)->directives; g; g = g->next)
            {
              DenemoDirective *directive = g->data;
              if (directive->prefix && (0 == (directive->override & DENEMO_OVERRIDE_AFFIX)))
                {
                  free_directive (directive);
                  ((chord *) thechord->object)->directives = g_list_remove (((chord *) thechord->object)->directives, directive);
                  break;        //there can only be one prefix replacing the duration, it would be tricky to remove more than one anyway as continuing the loop would be trick...
                }
            }
        }
    }
  ((chord *) thechord->object)->baseduration = baseduration;
  ((chord *) thechord->object)->numdots = numdots;

  set_basic_numticks (thechord);
}
//get the prevailing accidental for the current cursor height, that is the last accidental before the cursor at this height (from a note or keysig change), or the cached keysig accidental
gint get_cursoracc (void)
   {
        //g_print ("Get cursoracc with %d\n", Denemo.project->last_source);
	if (Denemo.project->last_source) 
		return 0; //INPUT_MIDI and AUDIO ignore cursoracc
	DenemoMovement *si = Denemo.project->movement;
	gint noteheight = si->staffletter_y;
	measurenode *meas = si->currentmeasure;
	objnode *obj = si->currentobject;
	if ((!si->cursor_appending) && obj)
		obj = obj->prev;//want the object before the cursor unless appending

    for (;obj;obj = obj->prev)
        {
          DenemoObject *curobj = (DenemoObject*)obj->data;
          if (curobj->type == CHORD)
            {
                chord *thechord = (chord*) curobj->object;
                GList *g;
                for (g = thechord->notes?thechord->notes:NULL;g;g=g->next)
                    {
                            note *thenote = (note*)g->data;
                            if (offsettonumber(thenote->mid_c_offset) == noteheight)
                                return thenote->enshift;
                    }
			}
		 else if (curobj->type == KEYSIG)
			  return curobj->keysig->accs [noteheight];
        }
    return ((DenemoMeasure *)meas->data)->keysig->accs [noteheight];// p *((DenemoMeasure *)(Denemo.project->movement->currentmeasure->data))->keysig
   }    
/**
 *  Set the number of dots on the chord
 *  if spillover is set then spillover single dotting (only).
 */
void
changenumdots (DenemoObject * thechord, gint number)
{
	DenemoMovement *si = Denemo.project->movement;
	chord *pchord = (chord *) thechord->object;
	gboolean inserting_midi = si->recording && (si->recording->type==DENEMO_RECORDING_MIDI) && si->recording->marked_onset;

	if ((!inserting_midi) && number==1 && Denemo.prefs.spillover && (pchord->numdots==0) && (pchord->baseduration>=0))
		{
		gint num = si->currentmeasurenum;
		gboolean tied = pchord->is_tied;
		gint enshift = Denemo.project->movement->pending_enshift;
		gint old_cursoracc = get_cursoracc ();
		pchord->is_tied = TRUE;
		insertion_point (si);
		if (num == si->currentmeasurenum)
			{
				pchord->is_tied = tied;//tie was not needed bale out
			}
		else //spillover
			{
				si->cursor_y = pchord->sum_mid_c_offset;
				si->staffletter_y = offsettonumber (si->cursor_y); //in case cursor was moved before dotting
				gint new_cursoracc = get_cursoracc ();
				Denemo.project->movement->pending_enshift = enshift + (old_cursoracc - new_cursoracc);
				dnm_insertnote (Denemo.project, pchord->baseduration + 1, INPUTNORMAL, FALSE);
				DenemoObject *newchord = (DenemoObject*)si->currentobject->data;
				if (newchord && (newchord->type==CHORD))
					newchord->isinvisible = thechord->isinvisible;
				return;
			}
		}
  ((chord *) thechord->object)->numdots = MAX (((chord *) thechord->object)->numdots + number, 0);
  set_basic_numticks (thechord);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}



static void
freenote (gpointer thenote)
{
  if (((note *) thenote)->directives)
    {
      free_directives (((note *) thenote)->directives);
      //g_list_free(thenote->directives);
    }
  g_free (thenote);
}


/**
 * Free the current chord
 */
void
freechord (DenemoObject * thechord)
{
  g_list_foreach (((chord *) thechord->object)->notes, (GFunc) freenote, NULL);
  g_list_free (((chord *) thechord->object)->notes);
  g_list_free (((chord *) thechord->object)->dynamics);
  if (((chord *) thechord->object)->lyric)
    g_string_free (((chord *) thechord->object)->lyric, FALSE); //FIXME memory leak????
  /* tone_node does not belong to the chord but belongs instead to the pitch recognition system */
  if (((chord *) thechord->object)->is_figure && ((chord *) thechord->object)->figure)
    g_string_free (((chord *) thechord->object)->figure, FALSE);        //FIXME memory leak????

  if (((chord *) thechord->object)->directives)
    {
      free_directives (((chord *) thechord->object)->directives);
      //g_list_free(((chord *) thechord->object)->directives);
    }
//FIXME we should free thechord->directives too if scripts fail to delete them
  g_free (thechord);
}



/**
 * Clone the current chord
 * used in the cut/copy/paste routine
 */
DenemoObject *
clone_chord (DenemoObject * thechord)
{
  DenemoObject *ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  GList *curtone;
  note *newnote;
  chord *curchord = (chord *) thechord->object;
  chord *clonedchord = (chord *) g_malloc0 (sizeof (chord));
  /* I'd use a g_list_copy here, only that won't do the deep copy of
   * the list data that I'd want it to */
  memcpy ((DenemoObject *) ret, (DenemoObject *) thechord, sizeof (DenemoObject));

  ret->object = NULL;
  ret->directives = NULL;       //currently the only pointers in DenemoObject
  memcpy ((chord *) clonedchord, curchord, sizeof (chord));
  clonedchord->directives = NULL;
  clonedchord->dynamics = NULL;
  clonedchord->tone_node = NULL;
  if (curchord->figure)
    clonedchord->figure = g_string_new (((GString *) curchord->figure)->str);
  else
    clonedchord->figure = NULL;
  clonedchord->is_figure = curchord->is_figure;
  if (curchord->fakechord)
    clonedchord->fakechord = g_string_new (((GString *) curchord->fakechord)->str);
  else
    clonedchord->fakechord = NULL;
  clonedchord->is_fakechord = curchord->is_fakechord;


  clonedchord->lyric = NULL;

/*   GList *g = curchord->directives; */
/*   for(;g;g=g->next) { */
/*     DenemoDirective *directive = (DenemoDirective *)g->data; */
/*     if(directive) */
/*       clonedchord->directives = g_list_append(clonedchord->directives, clone_directive(directive)); */
/*     else */
/*       g_warning("A Chord Directive list with NULL directive"); */
/*   } */

  clonedchord->directives = clone_directives (curchord->directives);

  clonedchord->notes = NULL;
  for (curtone = ((chord *) thechord->object)->notes; curtone; curtone = curtone->next)
    {
      newnote = (note *) g_malloc0 (sizeof (note));
      note *curnote = (note *) curtone->data;
      memcpy (newnote, curnote, sizeof (note));
      newnote->directives = clone_directives (curnote->directives);
      clonedchord->notes = g_list_append (clonedchord->notes, newnote);
    }
  ret->object = (chord *) clonedchord;
  /*
     g_debug ("Chord Base dur %d \tCloned Note base dur %d\n",
     ((chord *) thechord->object)->baseduration,
     ((chord *) ret->object)->baseduration);
     */
  return ret;
}
