/**
 * staffops.cpp
 * functions dealing with whole staffs
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller 
 */

#include "chordops.h"
#include "contexts.h"
#include <denemo/denemo.h>
#include "dialogs.h"
#include "measureops.h"
#include "moveviewport.h"
#include "objops.h"
#include "processstaffname.h"
#include "staffops.h"
#include <stdlib.h>
#include <string.h>
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "lilydirectives.h"
#include "displayanimation.h"
#include "selectops.h"
#include "utils.h"
#include "lyric.h"

/**
 * Return the first object node of the given measure
 * @param mnode a measurenode
 * @return the first object node of the measure
 */
objnode *
firstobjnode (measurenode * mnode)
{
  return (objnode *) mnode->data;
}

/**
 * Return the last object node of the given measure
 * @param mnode a measurenode
 * @return the last object node of the measure
 */
objnode *
lastobjnode (measurenode * mnode)
{
  return g_list_last ((objnode *) mnode->data);
}

/**
 * Return the first measure node of the given staffops
 * @param thestaff a staffnode
 * @return the first measure node of the staff
 */
measurenode *
firstmeasurenode (staffnode * thestaff)
{
  return ((DenemoStaff *) thestaff->data)->measures;
}

/**
 * Return the nth measure of the given staff
 * @param thestaff a staffnode
 * @param n the number of the measure to return
 * @return the nth measure of the staff
 */
measurenode *
nth_measure_node_in_staff (staffnode * thestaff, gint n)
{
  return g_list_nth (((DenemoStaff *) thestaff->data)->measures, n);
}

/**
 * Return the first object node in the given staff
 * @param thestaff a staffnode
 * @return the first object node of the staff
 */
objnode *
firstobjnodeinstaff (staffnode * thestaff)
{
  return firstobjnode (firstmeasurenode (thestaff));
}

/**
 * Reset si->currentprimarystaff based on current value of
 * si->currentstaff 
 * @param si a scoreinfo structure
 * @return none
 */
void
setcurrentprimarystaff (DenemoMovement * si)
{
  for (si->currentprimarystaff = si->currentstaff; !((DenemoStaff *) si->currentprimarystaff->data)->voicecontrol & DENEMO_PRIMARY; si->currentprimarystaff = si->currentprimarystaff->prev)
    ;
}

/**
 * Copies the staff data from a source staff to destination staff
 * @param src the source staff
 * @param dest the destination staff
 * @return none
 */
static void
copy_staff_bits (DenemoStaff * src, DenemoStaff * dest)
{
  dest->clef.type = src->clef.type;     //other fields - take care if dynamic
  dest->keysig.number = src->keysig.number;
  dest->keysig.isminor = src->keysig.isminor;
  memcpy (dest->keysig.accs, src->keysig.accs, SEVENGINTS);
  dest->timesig.time1 = src->timesig.time1;
  dest->timesig.time2 = src->timesig.time2;
  dest->volume = 127;
  dest->no_of_lines = 5;
  dest->transposition = 0;

  dest->space_above = 0;
  dest->space_below = 0;
  dest->context = DENEMO_NONE;
}

/**
 * Copies a staffs parameters from source to destination FIXME: only for a new voice - does not really copy
 * @param src the source staff
 * @param dest the destination staff
 * @return none
 */
static void
copy_staff_properties (DenemoStaff * src, DenemoStaff * dest)
{



  dest->midi_instrument = g_string_new (src->midi_instrument->str);
  dest->space_above = src->space_above;
  dest->space_below = src->space_below;
  dest->no_of_lines = src->no_of_lines;
  dest->transposition = src->transposition;

  dest->volume = src->volume;
  dest->voicecontrol = DENEMO_SECONDARY;
  beamsandstemdirswholestaff (dest);

}



/* copies a staff without its music data to another staff */
void
copy_staff (DenemoStaff * src, DenemoStaff * dest)
{
  dest->denemo_name = g_string_new (src->denemo_name->str);
  dest->lily_name = g_string_new (src->lily_name->str);
  dest->midi_instrument = g_string_new (src->midi_instrument->str);
  dest->transposition = src->transposition;

  dest->volume = src->volume;
  dest->voicecontrol = src->voicecontrol;
  dest->clef.type = src->clef.type;
  dest->leftmost_clefcontext = &dest->clef;

  dest->staff_directives = clone_directives (src->staff_directives);
  dest->voice_directives = clone_directives (src->voice_directives);
  dest->clef.directives = clone_directives (src->clef.directives);
  dest->keysig.directives = clone_directives (src->keysig.directives);
  dest->timesig.directives = clone_directives (src->timesig.directives);


  dest->keysig.number = src->keysig.number;
  dest->keysig.isminor = src->keysig.isminor;
  memcpy (dest->keysig.accs, src->keysig.accs, SEVENGINTS);
  dest->leftmost_keysig = &dest->keysig;

  dest->timesig.time1 = src->timesig.time1;
  dest->timesig.time2 = src->timesig.time2;

  dest->transposition = src->transposition;

  dest->space_above = 0;
  dest->space_below = 0;
  dest->context = src->context;
}

/**
 * Insert a new staff into the score
 * @param si the scoreinfo structure
 * @param thestaffstruct the staff to insert
 * @param action where to insert the new staff
 * @param addat the position to insert at
 * @return none
 */
static void
insert_staff (DenemoMovement * si, DenemoStaff * thestaffstruct, enum newstaffcallbackaction action, gint addat)
{
  si->thescore = g_list_insert (si->thescore, thestaffstruct, addat - 1);
  if (action != BEFORE)
    if (action != AFTER)
      {
        si->currentstaff = g_list_nth (si->thescore, addat - 1);
        si->currentstaffnum = addat;
        setcurrentprimarystaff (si);
        find_leftmost_staffcontext (thestaffstruct, si);
      }
  set_staff_transition (20);
}



/**
 * Create and insert a new staff into the score
 * @param si the scoreinfo structure
 * @param action the staffs type / where to insert it
 * @param context the staffs contexts
 * @return none
 */
void
newstaff (DenemoProject * gui, enum newstaffcallbackaction action, DenemoContext context)
{
  DenemoMovement *si = gui->si;
  g_assert (si != NULL);
  take_snapshot ();
  DenemoStaff *thestaffstruct = (DenemoStaff *) g_malloc (sizeof (DenemoStaff));

  if(!Denemo.non_interactive){
    thestaffstruct->staffmenu = (GtkMenu *) gtk_menu_new ();
    thestaffstruct->voicemenu = (GtkMenu *) gtk_menu_new ();
  }

  measurenode *themeasures = NULL;      /* Initial set of measures in staff */
  gint numstaffs = g_list_length (si->thescore);
  gint i, addat = 1;
  if (si->lily_file){
    g_free (thestaffstruct);
    return;          /* no code for this yet - just edit textually */
  }
  g_debug ("newstaff: Num staffs %d\n", numstaffs);
  if (numstaffs == 0)
    {
      action = INITIAL;

      thestaffstruct->clef.type = DENEMO_TREBLE_CLEF;
      thestaffstruct->keysig.number = 0;
      thestaffstruct->keysig.isminor = FALSE;
      memset (thestaffstruct->keysig.accs, 0, SEVENGINTS);
      thestaffstruct->timesig.time1 = 4;
      thestaffstruct->timesig.time2 = 4;
      thestaffstruct->volume = 127;
      thestaffstruct->no_of_lines = 5;
      thestaffstruct->transposition = 0;

      thestaffstruct->space_above = 20;
      thestaffstruct->space_below = 0;
      thestaffstruct->nummeasures = 1;
      thestaffstruct->midi_channel = 0;
#if 0
      si->measurewidths = g_list_append (si->measurewidths, GINT_TO_POINTER (si->measurewidth));
#else
      si->measurewidths = g_list_append (NULL, GINT_TO_POINTER (si->measurewidth));     //FIXME free old measurewidths
#endif
    }
  else
    {
      /* how did this work before? a new staff must have the same number of measures as the present one(s) */
      thestaffstruct->nummeasures = g_list_length (firstmeasurenode (si->thescore));
      copy_staff_bits ((DenemoStaff *) si->currentstaff->data, thestaffstruct);
      thestaffstruct->midi_channel = (numstaffs < 9 ? numstaffs : numstaffs + 1) & 0xF;
    }

  if (action == NEWVOICE)
    {
      thestaffstruct->voicecontrol = DENEMO_SECONDARY;
      thestaffstruct->nummeasures = g_list_length (firstmeasurenode (si->currentstaff));        //FIXME redundant
    }
  else
    {
      thestaffstruct->voicecontrol = DENEMO_PRIMARY;
    };

  for (i = 0; i < thestaffstruct->nummeasures; i++)
    {
      themeasures = g_list_append (themeasures, NULL);
    };

  if (action == INITIAL || action == ADDFROMLOAD)
    {
      si->currentmeasure = themeasures;
    }

#if 0
  // always go home for new staffs.
  else
    {
      si->currentmeasure = g_list_nth (themeasures, si->currentmeasurenum - 1);

    }
#endif

  /* Now fix the stuff that shouldn't be directly copied from
   * the current staff, if this staff was non-initial and that
   * was done to begin with */

  thestaffstruct->measures = themeasures;
  thestaffstruct->denemo_name = g_string_new (NULL);
  thestaffstruct->lily_name = g_string_new (NULL);

  //thestaffstruct->staff_prolog = NULL;
  // thestaffstruct->lyrics_prolog = NULL;
  //thestaffstruct->figures_prolog = NULL;
  //thestaffstruct->fakechords_prolog = NULL;
  thestaffstruct->context = context;
  if (action == NEWVOICE)
    g_string_sprintf (thestaffstruct->denemo_name, _("poly voice %d"), numstaffs + 1);
  else if (action == INITIAL)
    g_string_sprintf (thestaffstruct->denemo_name, _("Unnamed"));
  else
    g_string_sprintf (thestaffstruct->denemo_name, _("voice %d"), numstaffs + 1);
  set_lily_name (thestaffstruct->denemo_name, thestaffstruct->lily_name);
  thestaffstruct->midi_instrument = g_string_new ("");
  thestaffstruct->device_port = g_string_new ("NONE");
  thestaffstruct->leftmost_timesig = &thestaffstruct->timesig;

  /* In what position should the scrollbar be added?  */
  switch (action)
    {
    case INITIAL:
      addat = 1;
      break;
    case LAST:
    case ADDFROMLOAD:
      addat = numstaffs + 1;
      break;
    case BEFORE:
      addat = si->currentstaffnum;
      break;
    case AFTER:
    case NEWVOICE:
    case LYRICSTAFF:
    case FIGURESTAFF:
      addat = si->currentstaffnum + 1;
      break;
    case CHORDSTAFF:
      addat = si->currentstaffnum + 1;
      break;
    case FIRST:
      addat = 1;
      break;
    default:
      break;
    }

  if (action != INITIAL && action != ADDFROMLOAD)
    {
      if (action == NEWVOICE)
        {
          copy_staff_properties ((DenemoStaff *) si->currentstaff->data, thestaffstruct);
          set_lily_name (thestaffstruct->denemo_name, thestaffstruct->lily_name);       //this should be re-done if the denemo_name is reset.
          insert_staff (si, thestaffstruct, action, addat);
        }
      else
        {
          //      ret = staff_properties_change (&itp);
          //      if (ret)
          {
            /* 
               If staff_properties_change returns false,
               then the staff should probably be removed
               Fixed 09042005 Adam Tee 
             */
            insert_staff (si, thestaffstruct, action, addat);
            si->currentmeasurenum = 1;
            /*
               Reset leftmeasure num to 1 to be at the start of 
               the next staff. 
             */
            si->leftmeasurenum = 1;
          }
          //    else
          //      {
          /*
           *  Free the staff struct as it has not been inserted 
           *  into the score
           */
          //     g_free (thestaffstruct);
          //  }
        }
    }
  else                          // is INITIAL or ADDFROMLOAD
    {
      insert_staff (si, thestaffstruct, action, addat);
      si->leftmeasurenum = 1;
    }
  if ( addat==1)
       thestaffstruct->space_above = 20;
  //si->haschanged = TRUE;
}


gboolean
signal_structural_change (DenemoProject * gui)
{
  gui->layout_sync = gui->changecount;
  return TRUE;
}

/**
 * Remove the gui->si->currentstaff from the piece gui and reset si->currentstaff
 * if only one staff, inserts a new empty one
 * if interactive checks for custom_scoreblock
 * if a staff is deleted, updates the changecount
 * @param gui the DenemoProject structure
 * @return nothing
 */
void
deletestaff (DenemoProject * gui, gboolean interactive)
{
  DenemoMovement *si = gui->si;
  DenemoStaff *curstaffstruct = si->currentstaff->data;
  gboolean has_next = (si->currentstaff->next != NULL);
  (void) signal_structural_change (gui);
  if (si->currentstaff == NULL)
    return;

  gboolean give_info = FALSE;   //give info about removing matching context
  if (interactive && (curstaffstruct->context != DENEMO_NONE) && (!confirm (_("A context is set on this staff"), _("You will need to alter/delete the matching staff; Proceed?"))))
    return;
  if (interactive && (curstaffstruct->context != DENEMO_NONE))
    give_info = TRUE;
  take_snapshot ();
  gboolean isprimary = (gboolean) (curstaffstruct->voicecontrol & DENEMO_PRIMARY);
  //FIXME free_staff()

  free_directives (curstaffstruct->staff_directives);
  free_directives (curstaffstruct->timesig.directives);
  free_directives (curstaffstruct->keysig.directives);

  gtk_widget_destroy ((GtkWidget *) (curstaffstruct->staffmenu));
  gtk_widget_destroy ((GtkWidget *) (curstaffstruct->voicemenu));


  g_list_foreach (curstaffstruct->measures, freeobjlist, NULL);
  g_list_free (curstaffstruct->measures);
  g_string_free (curstaffstruct->denemo_name, FALSE);   //FIXME these should all be TRUE??
  g_string_free (curstaffstruct->lily_name, FALSE);
  g_string_free (curstaffstruct->midi_instrument, FALSE);
  // g_list_foreach (curstaffstruct->verses, (GFunc)destroy_parent, NULL);//FIXME it is enough to destroy the notebook, here we are only destroying the GtkTextViews
  if (curstaffstruct->verses)
    gtk_widget_destroy (gtk_widget_get_parent (gtk_widget_get_parent (curstaffstruct->verses->data)));

  g_free (curstaffstruct);




  if (si->currentstaff == g_list_last (si->thescore))
    si->currentstaffnum--;      //deleting the last, so the currentstaffnum must decrease
  else
    set_staff_transition (20);
  si->thescore = g_list_delete_link (si->thescore, si->currentstaff);
  if (si->thescore == NULL)
    {
      newstaff (gui, INITIAL, DENEMO_NONE);
    }
  si->currentstaff = g_list_nth (si->thescore, si->currentstaffnum - 1);


  if (isprimary && has_next)                // we deleted the primary, so the next one (which is present) must become the primary
    {
      ((DenemoStaff *) si->currentstaff->data)->voicecontrol = DENEMO_PRIMARY;
      si->currentprimarystaff = si->currentstaff;
    }
  else
    {
      setcurrentprimarystaff (si);
    }
  setcurrents (si);
  if (si->markstaffnum)
    calcmarkboundaries (si);
  if (gui->si->currentstaffnum < gui->si->top_staff)
    gui->si->top_staff = gui->si->currentstaffnum;
  show_lyrics ();
  update_vscrollbar (gui);

  displayhelper (gui);
  score_status (gui, TRUE);
  if (give_info)
    infodialog (_("The staff deleted had a start/end context; if you still have the staff with the matching end/start context\n then you should remove it (or its context) now.\nSee Staff->properties->context\nYou will not be able to print with miss-matched contexts."));
  return;
}

/**
 * Sets the beams and stem directions across the given staff
 * @param thestaff a staff structure
 * @return none
 */
void
beamsandstemdirswholestaff (DenemoStaff * thestaff)
{
  measurenode *curmeasure;
  gint nclef, time1, time2, stem_directive;

  nclef = thestaff->clef.type;
  time1 = thestaff->timesig.time1;
  time2 = thestaff->timesig.time2;
  stem_directive = DENEMO_STEMBOTH;

  for (curmeasure = thestaff->measures; curmeasure; curmeasure = curmeasure->next)
    {
      calculatebeamsandstemdirs ((objnode *) curmeasure->data, &nclef, &time1, &time2, &stem_directive);
    }
}

/**
 * Sets which accidentals to show across a staff on a key sig change
 * @param thestaff a staff stucture
 * @return none
 */
void
showwhichaccidentalswholestaff (DenemoStaff * thestaff)
{
  gint feed[7];
  gint feednum;
  measurenode *curmeasure;

  memcpy (feed, thestaff->keysig.accs, SEVENGINTS);
  feednum = thestaff->keysig.number;
  for (curmeasure = thestaff->measures; curmeasure; curmeasure = curmeasure->next)
    feednum = showwhichaccidentals ((objnode *) curmeasure->data, feednum, feed);
}

/**
 * Function to set the note positions on the given staff when there is a clef change
 * @param thestaff a staff structure
 * @return none 
 */
void
fixnoteheights (DenemoStaff * thestaff)
{
  gint nclef = thestaff->clef.type;
  //gint time1 = thestaff->stime1;//USELESS
  //gint time2 = thestaff->stime2;//USELESS
  //gint initialclef;//USELESS
  measurenode *curmeasure;
  objnode *curobj;
  DenemoObject *theobj;

  for (curmeasure = thestaff->measures; curmeasure; curmeasure = curmeasure->next)
    {
      //initialclef = nclef;
      for (curobj = (objnode *) curmeasure->data; curobj; curobj = curobj->next)
        {
          theobj = (DenemoObject *) curobj->data;
          switch (theobj->type)
            {
            case CHORD:
              newclefify (theobj, nclef);
              break;
            case TIMESIG:      //USELESS
              //time1 = ((timesig *) theobj->object)->time1;
              // time2 = ((timesig *) theobj->object)->time2;
              break;
            case CLEF:
              nclef = ((clef *) theobj->object)->type;
              break;
            default:
              break;
            }
        }                       /* End for */
    }                           /* End for */
  beamsandstemdirswholestaff (thestaff);
}



/**
 * Callback function to insert a staff in the initial position
 * @param action a Gtk Action
 * @param gui the DenemoProject structure
 * @return none
 */
void
newstaffinitial (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  while (gui->si->currentstaff && gui->si->currentstaff->prev)
    movetostaffup (NULL, NULL);
  newstaffbefore (action, NULL);
}

/**
 Callback function to insert a staff before the current staff
 * @param action a Gtk Action
 * @param gui the DenemoProject structure
 * @return none
 */
void
newstaffbefore (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  (void) signal_structural_change (gui);

  movetostart (NULL, NULL);
  newstaff (gui, BEFORE, DENEMO_NONE);
  if (gui->si->currentstaffnum >= gui->si->top_staff)
    gui->si->top_staff++;
  gui->si->currentstaffnum++;
  gui->si->bottom_staff++;
  set_bottom_staff (gui);
  move_viewport_down (gui);

  movetostaffup (NULL, NULL);
  displayhelper (gui);

}

/**
 * Callback function to insert a staff after the current staff
 * @param action a Gtk Action
 * @param gui the DenemoProject structure
 * @return none
 */
void
dnm_newstaffafter (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  if (!signal_structural_change (gui))
    return;
  movetostart (NULL, NULL);
  newstaff (gui, AFTER, DENEMO_NONE);
  set_bottom_staff (gui);
  update_vscrollbar (gui);
  movetostaffdown (NULL, NULL);
  displayhelper (gui);
}

/**
 * Callback function to insert a staff at the bottom of the score
 * @param action a Gtk Action
 * @param gui the DenemoProject structure
 * @return none
 */
void
newstafflast (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  while (gui->si->currentstaff && gui->si->currentstaff->next)
    movetostaffdown (NULL, NULL);
  dnm_newstaffafter (action, param);
}

/**
 * Callback function to add a new voice to the current staff
 * @param action a Gtk Action
 * @param gui the DenemoProject structure
 * @return none
 */
void
dnm_newstaffvoice (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  newstaff (gui, NEWVOICE, DENEMO_NONE);
  set_bottom_staff (gui);
  update_vscrollbar (gui);
  setcurrents (gui->si);
  if (gui->si->markstaffnum)
    calcmarkboundaries (gui->si);
  displayhelper (gui);
}
