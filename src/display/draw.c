/* draw.c
 * loop that draws all the items in the presently-displayed part of
 * the score
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include "display/calculatepositions.h"
#include "command/commandfuncs.h"
#include "command/contexts.h"
#include "command/lilydirectives.h"
#include "display/draw.h"               /* Which includes gtk.h */
#include "display/drawingprims.h"
#include "display/slurs.h"
#include "display/hairpin.h"
#include "command/staff.h"
#include "core/utils.h"
#include "export/exportlilypond.h"     /* to generate lily text for display */
#include "audio/pitchentry.h"
#include "command/lyric.h"
#include "audio/midi.h"
#include "display/displayanimation.h"
#include "ui/moveviewport.h"
#include "audio/audiointerface.h"

#define EXCL_WIDTH 3
#define EXCL_HEIGHT 13
#define SAMPLERATE (44100) /* arbitrary large figure used if no audio */
static DenemoObject *Startobj, *Endobj;
static gboolean layout_needed = TRUE;   //Set FALSE when further call to draw_score(NULL) is not needed.
static GList *MidiDrawObject;/* a chord used for drawing MIDI recorded notes on the score */

static gboolean last_tied = FALSE;
void
initialize_playhead (void)
{

  layout_needed = TRUE;
}

void
region_playhead (void)
{
  draw_score_area();
}


void
set_start_and_end_objects_for_draw (void)
{
  if (Denemo.project->movement->smf)
    {
      gdouble start = Denemo.project->movement->start_time;
      gdouble end = Denemo.project->movement->end_time;
      if ((end > 0.0) && (end < start))
        {
#ifdef SWAPPING_ENDS
            Denemo.project->movement->start_time = end;
            Denemo.project->movement->end_time = start;
            start = Denemo.project->movement->start_time;
            end = Denemo.project->movement->end_time;
#else
            Denemo.project->movement->end_time = -1.0;
#endif
        }
      Startobj = get_obj_for_end_time (Denemo.project->movement->smf, start/get_playback_speed() + 0.001);
      Endobj = Denemo.project->movement->end_time < 0.0 ? NULL : get_obj_for_start_time (Denemo.project->movement->smf, end/get_playback_speed() - 0.001);
    }
}



/**
 * scorearea_configure_event
 *
 * This function recaculates the number of measures that can be fit into
 * the display, and returns
 */
gint
scorearea_configure_event (G_GNUC_UNUSED GtkWidget * widget, G_GNUC_UNUSED GdkEventConfigure * event)
{
  DenemoProject *gui = Denemo.project;
  static gboolean init = FALSE;
  if (!init)
    {
      MidiDrawObject = g_list_append(NULL, newchord (0, 0, 0));
      chord *thechord = ((DenemoObject*)(MidiDrawObject->data))->object;
      init = TRUE;
    }

  if (gui == NULL)
    return TRUE;
  if (gui->movement == NULL)
    return TRUE;

  set_width_to_work_with (gui);
  nudgerightward (gui);
  nudge_downward (gui);
  return TRUE;
}

#define EXTRAFORSELECTRECT 2

/**
 *   Information to pass between the drawing functions
 */
struct infotopass
{
  clef *clef;
  gint key;
  gint curaccs[7];
  gint keyaccs[7];
  gint stem_directive;
  gint time1, time2;
  gint tickspermeasure;
  gint wholenotewidth;
  gint objnum;
  gint measurenum;
  gint staffnum;
  gboolean end;                 //if we have drawn the last measure
  gint top_y;
  gint last_gap;                //horizontal gap from last object
  gint markx1, markx2;
  gint marky1, marky2;
  gboolean line_end;            //set true when an object is drawn off the right hand edge
  gint tupletstart;             //x-coordinate where tuplet started, 0 if none
  gint tuplety;                 //y-coordinate of highest note within tuplet
  measurenode *curmeasure;
  GList *mwidthiterator;
  GSList *slur_stack;
  GSList *hairpin_stack;
  GtkWidget *verse;
  gint space_above;
  gint highy;                   /*(return) the highest y value drawn */
  gint lowy;                    /*(return) the lowest y value drawn */
  gint in_highy;                // FIXME these are passed in so that highy, lowy do not need to be passed back
  gint in_lowy;
  gboolean source_displayed;    //if pixbufs (sources) have been displayed for a staff - show no further staff content
  gboolean mark;                //whether the region is selected
  gint *left, *right;           //pointer into array, pointing to leftmost/rightmost measurenum for current system(line)
  gint *scale;                  //pointer into an array of scales - this entry is the percent horizontal scale applied to the current system
  GList *last_midi;             //last list of midi events for object at right of window
  DenemoObject *startobj;       //pointer values - if drawing such an object mark as playback start
  DenemoObject *endobj;         //pointer values - if drawing such an object mark as playback end
  gint startposition;           //x coordinate where to start playing
  gint endposition;             //x coordinate where to end playing
  gint playposition;            //x coordinate of currently played music
  gdouble leftmosttime;         //MIDI time of left most object on last system line displayed
  gdouble rightmosttime;        //MIDI time of last object  displayed
  GList *recordednote;//list of notes when recorded audio or MIDI is present
  gint currentframe; //current frame of audio being played. (current time converted to frames (at si->recording->samplerate) and slowed down)
  gboolean highlight_next_note;//the last CHORD was the one before the currently playing one.
  gboolean allow_duration_error; //do not indicate errors in duration of measure
  gdouble red, green, blue, alpha; //color of notes
  gboolean range;
  gint range_lo, range_hi;
};

/* count the number of syllables up to staff->leftmeasurenum */
static gint
count_syllables (DenemoStaff * staff, gint from)
{
  gint count = 0;
  gint i;
  GList *curmeasure = staff->themeasures;
  gboolean in_slur = FALSE;
  for (i = 1; curmeasure && (i < from); i++, curmeasure = curmeasure->next)
    {
      objnode *curobj;
      for (curobj = ((DenemoMeasure*)curmeasure->data)->objects; curobj; curobj = curobj->next)
        {
          DenemoObject *obj = curobj->data;

          if (obj->type == CHORD)
            {
              chord *thechord = ((chord *) obj->object);
              if (thechord->notes && !in_slur)
                count++;
              if (thechord->slur_begin_p)
                in_slur = TRUE;
              if (thechord->slur_end_p)
                in_slur = FALSE;
              if (thechord->is_tied && (!in_slur))
                count--;
            }
        }                       //for objs
    }                           //for measures
  if (in_slur)
    return -count;
  return count;
}

static void draw_note_onset(cairo_t *cr, double x, const gchar *glyph, gboolean mark)
{
    if(glyph) {
        drawlargetext_cr (cr, glyph, x, 20);
    } else
    {
                cairo_move_to (cr, x, 32);
                cairo_line_to (cr, x, 0);
                cairo_line_to (cr, x + 10, 32);
                cairo_fill (cr);

    }
   static gboolean on;

    if(mark)
        {
            on = !on;
            if(on)
                {
                  cairo_set_line_width (cr, 6.0 / Denemo.project->movement->zoom);
                  cairo_set_source_rgba (cr, 0, 1, 0, 0.40);
                  cairo_arc (cr, x + 10, 20, 20 / Denemo.project->movement->zoom, 0, 2 * M_PI);
                  cairo_stroke (cr);
                }
        }
}

/**
 *  draw_object
 *
 * Draws a single object in a measure within a staff.
 * @param curobj
 * @param x
 * @param y
 * @param gui
 * @param itp
 * @return excess ticks in the measure at this object. (Negative means still space).
 */
static gint
draw_object (cairo_t * cr, objnode * curobj, gint x, gint y, DenemoProject * gui, struct infotopass *itp)
{

  itp->highy = itp->lowy = 0;
  DenemoMovement *si = gui->movement;
  DenemoObject *mudelaitem = (DenemoObject *) curobj->data;

  //g_debug("draw obj %d %d\n", mudelaitem->x, y);
  //this is the selection being given a colored background
  if (cr)
    if (itp->mark)
      {
        cairo_save (cr);
        if (si->selection_is_copied)
            cairo_set_source_rgba (cr, 0.4, 0.8, 0.5, 0.7);
        else
            cairo_set_source_rgba (cr, 0.8, 0.8, 0.4, 0.7);
        cairo_rectangle (cr, x + mudelaitem->x, y, mudelaitem->minpixelsalloted, 80);
        cairo_fill (cr);
        cairo_restore (cr);
      }
  if (cr && (!Denemo.hovering_over_margin_up)
         && (!Denemo.hovering_over_margin_down)
         && (!Denemo.hovering_over_partname)
         && (!Denemo.hovering_over_clef)
         && (!Denemo.hovering_over_timesig)
         && (!Denemo.hovering_over_keysharpen)
         && (!Denemo.hovering_over_keyflatten)
         && (!Denemo.hovering_over_movement)
         && (!Denemo.hovering_over_right_arrow) 
         && (!Denemo.hovering_over_left_arrow)) //this is the hovered over object being given a colored background
    if ((Denemo.object_hovering_over==curobj) && ((si->currentobject != curobj) || ((si->currentobject == curobj) && si->cursor_appending)))
      {
        cairo_save (cr);
  // if curobj->next is NULL then we may be hovering over the barline we need to detect this in mousing.c and set object_hovering_over to a dummy barline object???, then draw here     
        cairo_set_source_rgba (cr, 0.5, 0.5, 0.9, 0.3);
        cairo_arc (cr, x + mudelaitem->x +  mudelaitem->minpixelsalloted/2, y + 20, mudelaitem->minpixelsalloted, 0.0, 2 * M_PI);
        cairo_fill (cr);
        cairo_set_source_rgba (cr, 0.2, 0.2, 0.9, 1);
        cairo_arc (cr, x + mudelaitem->x +  mudelaitem->minpixelsalloted/2, y + 20, mudelaitem->minpixelsalloted, 0.0, 2 * M_PI);
        cairo_stroke (cr);
          
        
        cairo_restore (cr);
      }
  

    if(Denemo.project->movement->playingnow && itp->highlight_next_note && (((Denemo.project->movement->playhead < mudelaitem->latest_time))))
        {
            itp->highlight_next_note = FALSE;
            if (cr)
                {
                  cairo_save (cr);
                  cairo_set_source_rgba (cr, 0.0, 0.2, 0.8, 0.5);
                  cairo_rectangle (cr, x + mudelaitem->x, y, 20, 80);
                  cairo_fill (cr);
                  cairo_restore (cr);
                }
        }

    if (Denemo.project->movement->playingnow && (!((Denemo.project->movement->playhead < mudelaitem->latest_time))))//For an unknown reason, this is the condition that the next note is being played...
        {
            itp->highlight_next_note = TRUE;
        }


  /* The current note, rest, etc. being painted */

  if (mudelaitem == Denemo.project->movement->playingnow)
    itp->playposition = x + mudelaitem->x;

  if (mudelaitem == itp->startobj) {
    itp->startposition = x + mudelaitem->x/* + mudelaitem->minpixelsalloted*/;
    // if(curobj->prev==NULL) g_debug("item %p at %d\n", curobj, x+mudelaitem->x), itp->startposition -= mudelaitem->minpixelsalloted;
    }

  if (mudelaitem == itp->endobj)
    itp->endposition = x + mudelaitem->x/* + mudelaitem->minpixelsalloted*/;



  if (cr)
    if (mudelaitem->type == CHORD && ((chord *) mudelaitem->object)->tone_node)
      cairo_set_source_rgb (cr, 231.0 / 255, 215.0 / 255, 39.0 / 255);  //thecolor = &yellow;



  switch (mudelaitem->type)
    {
    case CHORD:
      {
        chord *thechord = ((chord *) mudelaitem->object);
        if(cr)
           {
            cairo_save (cr);
            //if staff->range and thechord->highesty staff->range_hi ...
            if (thechord->notes && ((itp->range && ((thechord->highestpitch > itp->range_hi) || (thechord->lowestpitch < itp->range_lo)))))
                cairo_set_source_rgba (cr, 1.0, 0.0, 0.0, 1.0);
            else
                cairo_set_source_rgba (cr, itp->red, itp->green, itp->blue, itp->alpha);
           }
        gint highest = draw_chord (cr, curobj, x + mudelaitem->x, y,
                                   GPOINTER_TO_INT (itp->mwidthiterator->data),
                                   itp->curaccs, itp->mark, (si->currentobject == curobj));
        if(cr)
           {
            cairo_restore (cr);
           }
        if ((thechord->highesty) < itp->highy)
          itp->highy = thechord->highesty;
        itp->highy = MIN (itp->highy, highest);
        if ((thechord->lowesty) > itp->lowy + STAFF_HEIGHT)
          itp->lowy = thechord->lowesty - STAFF_HEIGHT;

 //display note onsets for source audio above relevant notes in top staff
 // if there are not enough notes to use up all the recorded note onsets only some recorded notes are shown.
         if(cr && si->recording && itp->recordednote && (itp->staffnum == si->top_staff))
            {
             GList *g = itp->recordednote;
             gint current = mudelaitem->earliest_time*si->recording->samplerate;
             gint next =  mudelaitem->latest_time*si->recording->samplerate;
             gint leadin =  si->recording->leadin;
             gint notewidth = 0;
             objnode *curobjnext = curobj->next;
             if(curobjnext){
                    DenemoObject *nextobj = (DenemoObject*)curobjnext->data;
                    notewidth = nextobj->x - mudelaitem->x;
             } else
             {
                    notewidth = GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE - mudelaitem->x;
             }

             /* draw the extent of the note */
            gint extra_width = (curobj->prev==NULL) ? SPACE_FOR_BARLINE:0; //first note has extra width to leave no gap in timing from end of last bar

            notewidth += extra_width;

            cairo_set_source_rgba (cr, 0.0, 0.2, 1.0, 1);
            cairo_move_to (cr, -extra_width + x + mudelaitem->x, 25);
            cairo_line_to (cr, -extra_width + x + mudelaitem->x, 20);
            cairo_line_to (cr, -extra_width + x + mudelaitem->x+ notewidth - 2, 20);
            cairo_line_to (cr, -extra_width + x + mudelaitem->x+ notewidth, 14);
            cairo_line_to (cr, -extra_width + x + mudelaitem->x+ notewidth, 22);
            cairo_line_to (cr, -extra_width + x + mudelaitem->x+2, 22);

            cairo_fill (cr);



            cairo_set_source_rgba (cr, 0.3, 0.3, 0.3, 0.5);

             while( g && (((gint)(((DenemoRecordedNote*)g->data)->timing) - leadin) < current))
                {
                    if(itp->measurenum == 1) {//represent onsets before score starts as single red onset mark 10 pixels before the first note. test g==itp->onset to avoid re-drawing
                        cairo_save (cr);
                        cairo_set_source_rgba (cr, 1.0, 0.0, 0.0, 1.0);
                        draw_note_onset (cr, x - 10, NULL, FALSE);
                        cairo_restore (cr);
                    }
                    g=g->next;
                }
            while( g && (((gint)(((DenemoRecordedNote*)g->data)->timing) - leadin) < next))
                {
                DenemoRecordedNote *midinote = (DenemoRecordedNote*)(g->data);
                gdouble fraction = (((gint)(midinote->timing) - leadin) - current) / (double)(next-current);
                gint pos;
                gchar *glyph;
                glyph = NULL;
                pos = notewidth * fraction;
                pos +=  mudelaitem->x;

                if(g==si->marked_onset)
                {
                    cairo_save (cr);
                    cairo_set_source_rgba (cr, 0, 0.5, 0, 1);//g_debug("marked offset %2.2f seconds ", midinote->timing/(double)si->recording->samplerate);
                } else
                if (si->playingnow)
                    {
                    (itp->currentframe < ((gint)(midinote->timing) - leadin)) ?
                        cairo_set_source_rgba (cr, 0.0, 0.2, 0.8, 0.8):
                        cairo_set_source_rgba (cr, 0.8, 0.2, 0.0, 0.8);
                    }


                //if MIDI RECORDING draw the pitch as a headless diamond note.
                if(si->recording->type==DENEMO_RECORDING_MIDI)
                    {

                        DenemoObject *midiobj = (DenemoObject*)(MidiDrawObject->data);
                        DenemoMeasure *curmeasure = si->currentmeasure->data;
                        midiobj->clef = curmeasure->clef;
                        midiobj->keysig = curmeasure->keysig;
                        midiobj->stemdir = curmeasure->stemdir;/// FIXME is there anything else

                        removetone ((DenemoObject*)(MidiDrawObject->data), 0);//there is only one note in the chord so any mid_c_offset will do
                        addtone (MidiDrawObject->data,  midinote->mid_c_offset + 7 * midinote->octave,  midinote->enshift);
                        chord *thechord = ((DenemoObject*)(MidiDrawObject->data))->object;
                        note *thenote = ((note*)(thechord->notes->data));
                        thenote->noteheadtype = DENEMO_DIAMOND_NOTEHEAD;
                        if(midinote->enshift)
                            thenote->showaccidental = TRUE;
                        thenote->position_of_accidental = 8;
                        //thechord->baseduration = midinote->duration;
                        //thechord->numdots = midinote->dots;
                        //set_basic_numticks (MidiDrawObject->data);
                        switch (midinote->duration) {
                            case 0:
                                glyph = midinote->dots?"𝅝•":"𝅝";
                                break;
                            case 1:
                                glyph = midinote->dots?"𝅗𝅥•":"𝅗𝅥";
                                break;
                            case 2:
                                glyph = midinote->dots?"𝅘𝅥•":"𝅘𝅥";
                                break;
                            case 3:
                                glyph = midinote->dots?"𝅘𝅥𝅮•":"𝅘𝅥𝅮";
                                break;
                            case 4:
                                glyph = midinote->dots?"𝅘𝅥𝅯•":"𝅘𝅥𝅯";
                                break;
                            case 5:
                                glyph = midinote->dots?"•𝅘𝅥𝅰":"𝅘𝅥𝅰";
                                break;
                            case 6:
                                glyph = midinote->dots?"•𝅘𝅥𝅱":"𝅘𝅥𝅱";
                                break;
                            case 7:
                                glyph = NULL;//we do not have a glyph for this yet
                                break;



                        }

                        if (g==si->marked_onset)
                        {
                            //midinote->measurenum = itp->measurenum;
                            //midinote->objnum = itp->objnum;
                        }

                        cairo_save (cr);
                        (g==si->marked_onset) ?cairo_set_source_rgba (cr, 0, 0.5, 0, 1):
                            cairo_set_source_rgba (cr, 0, 0, 0, 1);
                        draw_chord (cr, MidiDrawObject, pos + x -extra_width, y, 0, itp->curaccs, FALSE, FALSE);
                        cairo_restore (cr);
                    }

                draw_note_onset(cr, pos + x - extra_width, glyph, (g==si->marked_onset));

                if(g==si->marked_onset)
                    {//g_debug("fraction = %f; notewidth = %d ", fraction, notewidth);
                        cairo_restore (cr);
                    }
                if(si->marked_onset_position && ABS((gint)(pos + x - si->marked_onset_position))<20)
                    {
                    si->marked_onset = g;
                    si->marked_onset_position = 0; //g_debug("Found selected onset\n\n");
                    }

                //if(g==itp->onset) g_debug("First onset at %d %d %d %d\n", pos, x, si->marked_onset_position, notewidth);


                g = g->next;
                }
            itp->recordednote = g;//Search onwards for future onsets. Only notes on top staff are used for display of onsets.

         } //recording


        if (itp->tupletstart)
          itp->tuplety = MAX (0, MAX (itp->tuplety, MAX (-thechord->lowesty, -thechord->highesty)));

        if (thechord->is_fakechord)
          if (cr)
            draw_fakechord (cr, x + mudelaitem->x, y - 45, mudelaitem);

        if (si->currentstaffnum == itp->staffnum && itp->verse && thechord->notes)
          {
            if ((!thechord->is_grace) && (!last_tied) && (!itp->slur_stack)
                            && !find_directive (thechord->directives, "MoveRest"))
              {
                gchar *syllable = (gchar *) next_syllable ();
                if (cr)
                  if (syllable)
                    draw_lyric (cr, x + mudelaitem->x, y + itp->in_lowy, syllable);
              }
            last_tied = thechord->is_tied;
          }

        if (cr)
          if (thechord->dynamics)
            draw_dynamic (cr, x + mudelaitem->x, y, mudelaitem);

        if (cr)
          if (thechord->slur_end_p)
            draw_slur (cr, &(itp->slur_stack), x + mudelaitem->x + 5/*half note head??? */, y, thechord->highesty);
        if (thechord->slur_begin_p)
          itp->slur_stack = push_slur_stack (itp->slur_stack, x + mudelaitem->x, thechord->highesty);




        if (thechord->crescendo_begin_p)
          {
           if (thechord->diminuendo_end_p)
              {
               if (top_hairpin_stack (itp->hairpin_stack) == -1)
                    {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                    }
              if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 0);
                   //pop stack
              itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
             } else  if (thechord->crescendo_end_p)
              {
                if (top_hairpin_stack (itp->hairpin_stack) == -1)
                  {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                  }
                if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 1);
                   //pop stack
                itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
             }
            itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
          }
        else if (thechord->diminuendo_begin_p)
          {
            if (thechord->crescendo_end_p)
              {
                if (top_hairpin_stack (itp->hairpin_stack) == -1)
                  {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                  }
                if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 1);
                   //pop stack
                itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
             } else if (thechord->diminuendo_end_p)
              {
               if (top_hairpin_stack (itp->hairpin_stack) == -1)
                    {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                    }
              if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 0);
                   //pop stack
              itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
             }
            itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
          }
       else
          {
            if (thechord->crescendo_end_p)
              {
                 if (top_hairpin_stack (itp->hairpin_stack) == -1)
                  {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                  }
                if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 1);
                   //pop stack
                itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
              }
            else if (thechord->diminuendo_end_p)
              {
               if (top_hairpin_stack (itp->hairpin_stack) == -1)
                    {
                        itp->hairpin_stack = push_hairpin_stack (itp->hairpin_stack, -10);
                    }
               if (cr) draw_hairpin (cr, &(itp->hairpin_stack), x + mudelaitem->x, y, 0);
                   //pop stack
               itp->hairpin_stack = pop_hairpin_stack (itp->hairpin_stack);
              }
          }
        /* notice the following does not check is_figure but checks if figure is not VOID) */
        //if (!thechord->is_figure && thechord->figure)
        if (cr)
          if (thechord->figure)
            draw_figure (cr, x + mudelaitem->x, y + (thechord->lowesty / 2), mudelaitem);
        if (cr)
          if (!thechord->is_fakechord && thechord->fakechord)
            draw_fakechord (cr, x + mudelaitem->x, y - 45,
                            //y + (thechord->highesty / 4),
                            mudelaitem);
      }
      break;
    case TUPOPEN:
      itp->tupletstart = x + mudelaitem->x;
      if (cr)
        draw_tupbracket (cr, x + mudelaitem->x, y, mudelaitem, 0);
      break;
    case TUPCLOSE:
      if (cr)
        draw_tupbracket (cr, x + mudelaitem->x, y - itp->tuplety, mudelaitem, itp->tupletstart);
      itp->tupletstart = itp->tuplety = 0;
      break;
    case LILYDIRECTIVE:
      {
        DenemoDirective *directive = mudelaitem->object;
        if (cr)
          draw_lily_dir (cr, x + mudelaitem->x, y, 0, 0, mudelaitem, itp->mark, (si->currentobject == curobj));
        if ((directive->ty - 10) < itp->highy)
          itp->highy = directive->ty - 10 /* for height of text */ ;
        if ((directive->gy - 10) < itp->highy)
          itp->highy = directive->gy - 10 /* for height of text */ ;
          if(directive->tag && *directive->tag->str == '!')
            itp->allow_duration_error = TRUE;
      }
      break;
    case CLEF:
      itp->clef = ((clef *) mudelaitem->object);
      if (cr)
        {
          if (mudelaitem->isinvisible)
            {
              cairo_save (cr);
              cairo_set_source_rgb (cr, 231.0 / 255, 215.0 / 255, 39.0 / 255);  //thecolor = &yellow;cairo_  rgb yellow
            }
          draw_clef (cr, x + mudelaitem->x, y, itp->clef);
          if (mudelaitem->isinvisible)
            {
              cairo_restore (cr);
            }
        }

      break;
    case KEYSIG:
      if (cr)
        draw_key (cr, x + mudelaitem->x, y, ((keysig *) mudelaitem->object)->number, itp->key, itp->clef->type, TRUE, (keysig *) mudelaitem->object);
      itp->key = ((keysig *) mudelaitem->object)->number;
      memcpy (itp->keyaccs, ((keysig *) mudelaitem->object)->accs, SEVENGINTS);
      memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);

      break;
    case TIMESIG:
      itp->time1 = ((timesig *) mudelaitem->object)->time1;
      itp->time2 = ((timesig *) mudelaitem->object)->time2;
      if (cr)
        draw_timesig (cr, x + mudelaitem->x, y, itp->time1, itp->time2, (timesig *) mudelaitem->object);

      /* The following assumes no multiple simultaneous time signatures */
      itp->tickspermeasure = WHOLE_NUMTICKS * itp->time1 / itp->time2;
      itp->wholenotewidth = si->measurewidth * itp->time2 / itp->time1;
      break;
    case STEMDIRECTIVE:
      if (cr)
        draw_stem_directive (cr, x + mudelaitem->x, y, mudelaitem);
      itp->stem_directive = ((stemdirective *) mudelaitem->object)->type;
      break;
    default:
      /* Nothing */
      break;
    }

  gint extra = MAX (mudelaitem->minpixelsalloted,
                    space_after (mudelaitem->durinticks,
                                 itp->wholenotewidth));
  if (si->currentobject == curobj)
    {                           /* Draw the cursor */
      /* Determine if it needs to be red or not */
      if (si->cursor_appending || mudelaitem->type != CHORD)
        si->cursoroffend = (mudelaitem->starttickofnextnote >= itp->tickspermeasure);
      else
        si->cursoroffend = (mudelaitem->starttickofnextnote > itp->tickspermeasure);
      if (si->cursor_appending)
        {
          draw_cursor (cr, si, x + mudelaitem->x + extra, y, ((itp->curmeasure->next != NULL) && (objnode *) ((DenemoMeasure*)itp->curmeasure->next->data)->objects) ? -1 : 0 /*itp->last_gap */ , 0, mudelaitem->clef->type);

        }
      else
        {
          draw_cursor (cr, si, x + mudelaitem->x, y, itp->last_gap, mudelaitem->type == CHORD ? 0 : mudelaitem->minpixelsalloted, mudelaitem->clef->type);
        }
    }
  /* End cursor drawing */

  itp->last_gap = extra;
  /* Now quite possibly update the mark */

  if (si->selection.firststaffmarked == itp->staffnum && si->selection.firstmeasuremarked == itp->measurenum && si->selection.firstobjmarked == itp->objnum)
    itp->markx1 = x + mudelaitem->x - EXTRAFORSELECTRECT;

  if (si->selection.laststaffmarked == itp->staffnum && si->selection.lastmeasuremarked == itp->measurenum && si->selection.lastobjmarked == itp->objnum)
    itp->markx2 = x + mudelaitem->x + mudelaitem->minpixelsalloted + EXTRAFORSELECTRECT;

    //In page view we have to allow the last time to be the last recorded time for any object on the page, but empty measures on the lowest visible staff will cause the rightmost time to be set too early.
    //FIXME, use smf.c to calculate start and end times for each measure and consult that.
  if((Denemo.project->view == DENEMO_PAGE_VIEW) || (itp->rightmosttime < mudelaitem->latest_time*get_playback_speed()))
    {
      if (!(itp->measurenum == si->rightmeasurenum + 1)) //ignore partially drawn measures for computing whether we need to call page_viewport
        itp->rightmosttime = mudelaitem->latest_time*get_playback_speed();
    }
  return (mudelaitem->starttickofnextnote - itp->tickspermeasure);
}                              /* draw_object */

/**
 * Draws a single measure within a staff.
 * @param curmeasure pointer to the measure to draw
 * @param x
 * @param y
 * @param gui
 * @param itp
 * return nothing
 */
static void
draw_measure (cairo_t * cr, measurenode * curmeasure, gint x, gint y, DenemoProject * gui, struct infotopass *itp)
{
  static GString *mstring;
  gint last_type = -1;          //type of last object in measure
  gint extra_ticks = 0;         //number of ticks by which measure is over-full
  DenemoMovement *si = gui->movement;
  objnode *curobj;
  gboolean has_cursor = FALSE;
  /* initialization */
  if (!mstring)
    mstring = g_string_new (NULL);
  /* Set information about the state at the current measure,
     if necessary */

  memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);
  itp->wholenotewidth = si->measurewidth * itp->time2 / itp->time1;


  /*  paint the measure number at the preceding barline
   */

  if (cr)
    {

     DenemoMeasure *meas = (DenemoMeasure*)curmeasure->data;
     if (meas->measure_numbering_offset)
        {
          cairo_set_source_rgba (cr, 0.2, 0.1, 0.8, 0.7);
          g_string_sprintf (mstring, "%d", meas->measure_numbering_offset);
          drawlargetext_cr (cr, mstring->str, x - SPACE_FOR_BARLINE - 5, y + 2 * STAFF_HEIGHT - 10);
        }

      if (itp->measurenum > 1)
        {                       //don't draw first measure number, as it collides and is obvious anyway and is never typeset thus
          cairo_set_source_rgba (cr, 0, 0, 0, 0.5);
          g_string_sprintf (mstring, "%d", meas->measure_number);
          drawnormaltext_cr (cr, mstring->str, x - SPACE_FOR_BARLINE - 5, y - 3);
        }
    }


  // draw the cursor and set the side effects up if this didn't happen when drawing the currentobject because there isn't one - a blank measure
  if (!si->currentobject && (si->currentstaffnum == itp->staffnum && si->currentmeasurenum == itp->measurenum))
    {
      /* That is, the cursor's at the beginning of this blank measure */
      si->cursoroffend = FALSE;
      has_cursor = TRUE;
      draw_cursor (cr, si, x, y, 0, gui->mode, itp->clef->type);
    }

  curobj = (objnode *) ((DenemoMeasure*)curmeasure->data)->objects;
  /* These default values for the markx'es may be necessary down
   * the road */
  if (si->selection.firststaffmarked == itp->staffnum && si->selection.firstmeasuremarked == itp->measurenum)
    {
      if (!curobj)
        itp->markx1 = x - EXTRAFORSELECTRECT;
      else
        itp->markx1 = x + GPOINTER_TO_INT (itp->mwidthiterator->data);
    }
  if (si->selection.laststaffmarked == itp->staffnum && si->selection.lastmeasuremarked == itp->measurenum)
    {
      if (!curobj || (si->selection.lastobjmarked >= (gint) (g_list_length ((objnode *) curobj))))
        itp->markx2 = x + GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE + EXTRAFORSELECTRECT;
      else
        itp->markx2 = x;
    }



  gboolean not_marked = (!si->markstaffnum) || (si->selection.firststaffmarked > itp->staffnum) || (si->selection.laststaffmarked < itp->staffnum) || (si->selection.firstmeasuremarked > itp->measurenum) || (si->selection.lastmeasuremarked < itp->measurenum);

  gboolean definitely_marked = (!not_marked) && (si->selection.firstmeasuremarked < itp->measurenum) && (si->selection.lastmeasuremarked > itp->measurenum);
  gboolean in_firstmeas = (si->selection.firstmeasuremarked == itp->measurenum);
  gboolean in_lastmeas = (si->selection.lastmeasuremarked == itp->measurenum);
  /* Draw each mudelaitem */
  for (itp->objnum = 0; curobj; curobj = curobj->next, itp->objnum++)
    {
      itp->mark = (definitely_marked) || ((!not_marked) && ((in_firstmeas && !in_lastmeas && (si->selection.firstobjmarked <= itp->objnum)) || (in_lastmeas && !in_firstmeas && (si->selection.lastobjmarked >= itp->objnum)) || (in_lastmeas && in_firstmeas && (si->selection.firstobjmarked <= itp->objnum) && (si->selection.lastobjmarked >= itp->objnum))));


      if (cr)
        {
          //      if(itp->measurenum > si->rightmeasurenum+1)
          //        g_critical("Please advise the circumstance in which this happened");
          if (itp->measurenum == si->rightmeasurenum + 1)
            cairo_set_source_rgb (cr, 0.5, 0.5, 0.5);   //This draws light gray anything that will be repeated at the start of the next page.
          else
            cairo_set_source_rgb (cr, 0, 0, 0); //black;
        }                       // if cr

      extra_ticks = draw_object (cr, curobj, x, y, gui, itp);

      {
        DenemoObject *obj = (DenemoObject *) curobj->data;
        last_type = obj->type;
      }
      //itp->rightmosttime = curobj->latest_time;//we just want this for the rightmost object
    }                           // for each object
  if (cr)
    {
      cairo_save (cr);
      //marking the barline if within selection
      if (si->markstaffnum &&
/*        (curmeasure->data==NULL) && */
          (si->selection.firststaffmarked <= itp->staffnum) && (si->selection.laststaffmarked >= itp->staffnum) && (si->selection.firstmeasuremarked <= itp->measurenum) && (si->selection.lastmeasuremarked > itp->measurenum))
        {
          cairo_set_source_rgba (cr, 0.8, 0.8, 0.4, 0.7);
          cairo_rectangle (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data) - 10, y, 20, STAFF_HEIGHT + 1);

          cairo_fill (cr);
        }
      /* Indicate fill status  */
#define OPACITY (curmeasure == si->currentmeasure?0.3:0.8)
         if(itp->allow_duration_error)
            {
                extra_ticks = 0;
                itp->allow_duration_error = FALSE;
            }
      if (curmeasure->data)
        {
            //overfull or underfull measure indicator
          if (extra_ticks == -itp->tickspermeasure)
                extra_ticks = 0;//allow empty measures
          if(itp->time1 == 256) //Cadenza Time
                extra_ticks = 0;
          if (extra_ticks > 0)
                cairo_set_source_rgba (cr, 1.0, 0.6, 0.6, OPACITY);
          else if ((extra_ticks < 0)/* && curmeasure->next*/)
                cairo_set_source_rgba (cr, 0.6, 0.6, 1, OPACITY);

          if (((extra_ticks > 0) || (extra_ticks < 0)) &&
            ((curmeasure->next && curmeasure->next->data) || ((curmeasure->next!= NULL) && (!has_cursor))))

            {
              cairo_rectangle (cr, x, y, GPOINTER_TO_INT (itp->mwidthiterator->data), STAFF_HEIGHT + 1);
              cairo_fill (cr);
              extra_ticks > 0 ? cairo_set_source_rgb (cr, 1, 0, 0) : cairo_set_source_rgb (cr, 0, 0, 1);
            }
          else
            {
              cairo_set_source_rgb (cr, 0, 0, 0);
            }
        }
#undef OPACITY
      if (extra_ticks == 0)
        {
          cairo_set_source_rgb (cr, 0, 0, 0);
        }

      //draw the barline
      cairo_rectangle (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y - 0.5, 1.5, STAFF_HEIGHT + 1);
      cairo_fill (cr);

      if ((!curmeasure->next))
        {
          /* we've reached the end of the staff and should
           * draw the heavy part of double-barline unless there is a directive here in which case it takes responsibility for the type of barline */
          x += 3;
          if (last_type != LILYDIRECTIVE)
            {
              cairo_rectangle (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y - 0.5, 4, STAFF_HEIGHT + 1);
              cairo_fill (cr);
            }
          itp->end = TRUE;
          if(itp->startposition>-1 && itp->endposition<0)
            itp->endposition = x + GPOINTER_TO_INT (itp->mwidthiterator->data) + 5;//end play marker after last note if not elsewhere
        }
      else
        {
          itp->end = FALSE;
        }
      cairo_restore (cr);
    }                           //if cr

}

/**
 * Draws a single staff
 * TODO sort out graphics context for active polyphonic voice should
 * do it here
 * @param curstaffstruct pointer to the staff to draw
 * @param y    y position of the staff
 * @param gui   pointer to the DenemoProject structure
 * @param itp  pointer to the infotopass structure
 * return TRUE if the staff has had to made taller
 */
static gboolean
draw_staff (cairo_t * cr, staffnode * curstaff, gint y, DenemoProject * gui, struct infotopass *itp)
{
  DenemoStaff *thestaff = (DenemoStaff *) curstaff->data;
  gboolean repeat = FALSE;
  DenemoMovement *si = gui->movement;
  gint x = (gui->leftmargin+35), i;
    itp->red = (((thestaff->color)>>24)&0xFF)/255.0;
    itp->green = (((thestaff->color)>>16)&0xFF)/255.0;
    itp->blue = (((thestaff->color)>>8)&0xFF)/255.0;
    itp->alpha = (0xFF ^ ((thestaff->color)&0xFF))/255.0;
    itp->range = thestaff->range;
    itp->range_lo = thestaff->range_lo;
    itp->range_hi = thestaff->range_hi;
  // if(si->marked_onset_position)
    //g_debug("repeat"),repeat = TRUE;//we set up the marked onset with this, then need to repeat to draw it
  //g_debug("drawing staff %d at %d\n", itp->staffnum, y);
  gint nummeasures = g_list_length (thestaff->themeasures);

  //g_debug("Of %d current %d\n", nummeasures, itp->measurenum);
  if (itp->measurenum > nummeasures)
    cr = NULL;                  //no more drawing on this staff
  if (cr)
    {
      cairo_save (cr);

      //Draw vertical lines to bind the staffs of the system together
      if (curstaff->prev)
        {
          DenemoStaff *prev = (DenemoStaff *) (curstaff->prev->data);
          cairo_set_source_rgb (cr, 0, 0, 0);
          cairo_rectangle (cr, gui->leftmargin, y - STAFF_HEIGHT - prev->space_below - thestaff->space_above, 2, 2 * STAFF_HEIGHT + prev->space_below + thestaff->space_above);
          cairo_fill (cr);
        }
      if (curstaff->next)
        {
          DenemoStaff *next = (DenemoStaff *) (curstaff->next->data);
          //cairo_save(cr);
          cairo_set_source_rgb (cr, 0, 0, 0);
          cairo_rectangle (cr, gui->leftmargin, y, 2, 2 * STAFF_HEIGHT + next->space_above + thestaff->space_below);
          cairo_fill (cr);
        }

      if ((DenemoStaff *) si->currentstaff->data == thestaff)
        cairo_set_source_rgb (cr, 0, 0, 0);
      else
        cairo_set_source_rgb (cr, 0.3, 0.3, 0.3);
    }   //if cr




  if (!itp->line_end)
    {                           //not a continuation
      itp->clef = thestaff->leftmost_clefcontext;
      if (cr && !(thestaff->voicecontrol & DENEMO_SECONDARY))
        draw_clef (cr, gui->leftmargin, y, itp->clef);
      else if (cr)
        {
          cairo_save (cr);
          cairo_set_source_rgb (cr, 231.0 / 255, 215.0 / 255, 39.0 / 255);      //thecolor = &yellow;
          draw_clef (cr, gui->leftmargin, y, itp->clef);
          cairo_restore (cr);
        }


      itp->key = thestaff->leftmost_keysig->number;
      if (cr && !(thestaff->voicecontrol & DENEMO_SECONDARY))
        draw_key (cr, x, y, itp->key, 0, itp->clef->type, TRUE, thestaff->leftmost_keysig);
      x += si->maxkeywidth;
      itp->time1 = thestaff->leftmost_timesig->time1;
      itp->time2 = thestaff->leftmost_timesig->time2;
      if (cr && !(thestaff->voicecontrol & DENEMO_SECONDARY))
        {
          if (si->leftmeasurenum == 1)
            draw_timesig (cr, x + 5, y, itp->time1, itp->time2, thestaff->leftmost_timesig);
          else
            {
              cairo_save (cr);
              if (Denemo.hovering_over_left_arrow)
                {
                    cairo_rectangle (cr, x, y, 30, 40);
                    cairo_stroke (cr);
                    cairo_set_source_rgb (cr, 0.7, 1.0, 0.7);
                    cairo_rectangle (cr, x, y, 30, 40);
                    cairo_fill (cr);
                }
              cairo_set_source_rgb (cr, 0.1, 0.2, 1);
              drawtext_cr  (cr, "<", x, y + 30, 42.0);
              cairo_restore (cr);
            }
        }
      x += SPACE_FOR_TIME;
    }
  else
    {                           // a continuation line
      if (cr && !(thestaff->voicecontrol & DENEMO_SECONDARY))
        draw_clef (cr, gui->leftmargin, y, itp->clef);
      if (cr && !(thestaff->voicecontrol & DENEMO_SECONDARY))
        draw_key (cr, x, y, itp->key, 0, itp->clef->type, TRUE, thestaff->leftmost_keysig);
      x += si->maxkeywidth;
      x += SPACE_FOR_TIME;      // to allow the same margin ??
    }

  *itp->left = itp->measurenum > gui->movement->rightmeasurenum ? gui->movement->rightmeasurenum : itp->measurenum;
  memcpy (itp->keyaccs, thestaff->leftmost_keysig->accs, SEVENGINTS);


  itp->stem_directive = thestaff->leftmost_stem_directive;
  itp->tickspermeasure = WHOLE_NUMTICKS * itp->time1 / itp->time2;

  if (cr)
    {
      /* Draw staff name on first system */
      if (!itp->line_end)
        {
          gint staffname_offset = (thestaff->voicecontrol & DENEMO_PRIMARY) ? 26 : 12;
          gboolean color = (Denemo.hovering_over_partname && (si->currentstaffnum == itp->staffnum));
           if (si->leftmeasurenum == 1)//make a button of it if measure 1 is leftmost
             {
             
              cairo_save (cr);
              cairo_set_source_rgb (cr, 0.8, color?1:0.8, 0.8);

              cairo_rectangle (cr, gui->leftmargin + 5, y - staffname_offset - 1, 90, 15);
              cairo_fill (cr);

              
              cairo_set_source_rgb (cr, 0.0, 0.0, 0.0);
              if (color)
                {
                    cairo_rectangle (cr, gui->leftmargin + 5, y - staffname_offset - 2, 90, 16);
                    cairo_stroke (cr);
                }
              drawnormaltext_cr (cr, thestaff->denemo_name->str, gui->leftmargin /*KEY_MARGIN */ + 5 , y - staffname_offset + 10);
              if (thestaff->subpart)
                drawnormaltext_cr (cr, thestaff->subpart->str, gui->leftmargin +20/*KEY_MARGIN */ , y - staffname_offset + 24);
              cairo_restore(cr);
             }
           else
             {
              drawnormaltext_cr (cr, thestaff->denemo_name->str, gui->leftmargin - 10 /*KEY_MARGIN */ + 5 , y - staffname_offset + 10);
              if (thestaff->subpart)
                drawnormaltext_cr (cr, thestaff->subpart->str, gui->leftmargin + 20 /*KEY_MARGIN */ , y - staffname_offset + 24);
             }
            if(thestaff->hasfakechords)  drawnormaltext_cr (cr, _("Chord Symbols"), gui->leftmargin - 10 /*KEY_MARGIN */ , y - staffname_offset + 20 + 2 * STAFF_HEIGHT);
            if(thestaff->hasfigures)  drawnormaltext_cr (cr, _("Figured Bass"), gui->leftmargin - 10 /*KEY_MARGIN */ , y - staffname_offset + 20 + 2 * STAFF_HEIGHT);
            { cairo_save (cr);
              cairo_set_source_rgba (cr, 0.1, 0.6, 0.1, 0.5);
              if(thestaff->hidden)  drawnormaltext_cr (cr, _("Hidden when not the current staff."), gui->leftmargin +55, y - staffname_offset - 5 + 2 * STAFF_HEIGHT);
              cairo_restore(cr);
            }
        }
      else
        {
          cairo_save (cr);
          gint staffname_offset = (thestaff->voicecontrol & DENEMO_PRIMARY) ? 26 : 12;
          cairo_translate (cr, 2, (y - staffname_offset + 30));
          cairo_rotate (cr, -M_PI / 5.0);
          drawnormaltext_cr (cr, thestaff->denemo_name->str, 0, 0);
          if (thestaff->subpart)
                drawnormaltext_cr (cr, thestaff->subpart->str, 30, 20);
          cairo_restore (cr);
        }
    }                           //if cr



  x += transition_offset ();






  if ((gui->movement->smf) && (itp->startobj == NULL) && (itp->startposition <= 0) && (si->leftmeasurenum == 1))
    itp->startposition = x;


  /* Loop that will draw each measure. Basically a for loop, but was uglier
   * when written that way.  */
  itp->curmeasure = g_list_nth (thestaff->themeasures, itp->measurenum - 1);
  //g_debug("measurenum %d\nx=%d\n", itp->measurenum, x);

  //FIX in measure.c for case where si->measurewidths is too short
  itp->mwidthiterator = g_list_nth (si->measurewidths, itp->measurenum - 1);

  {
    //compute itp->leftmosttime here - the time at the start of this system

    objnode *curobj = itp->curmeasure ? (objnode *) ((DenemoMeasure*)itp->curmeasure->data)->objects : NULL;
    if (curobj)
      {
        DenemoObject *mudelaitem = (DenemoObject *) curobj->data;
        if (mudelaitem)
          itp->leftmosttime = mudelaitem->earliest_time;
        else
          itp->leftmosttime = 1000000.0;
      }
    //g_debug("Drawing staff %d leftmost time %f, measurenum %d\n",itp->staffnum, itp->leftmosttime, itp->measurenum);
  }

  if (!*itp->scale)
    *itp->scale = 100;
  if (cr)
    {
      cairo_scale (cr, 100.0 / (*itp->scale), 1.0);
      //cairo_scale(cr,(*itp->scale)/100.0,1.0);
    }

  gint scale_before = *itp->scale;
  itp->line_end = FALSE;
  cairo_t *saved_cr = NULL;
  if (itp->source_displayed)
    {                           //We have displayed source material below the last staff, so do not draw anymore staff music. We cannot simply skip the drawing routines however, because they determine the rightmost bar for mouse positioning, so we save and restore it.
      saved_cr = cr;
      cr = NULL;
    }

  while ((!itp->line_end) && itp->measurenum <= nummeasures)
    {

      if (x + GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE > (int) (get_widget_width (Denemo.scorearea) / gui->movement->zoom - (RIGHT_MARGIN + (gui->leftmargin+35) + si->maxkeywidth + SPACE_FOR_TIME)))
        itp->line_end = TRUE;

      itp->last_gap = 0;

      if (itp->measurenum == si->currentmeasurenum)
        x += measure_transition_offset (si->currentstaffnum == itp->staffnum);
      draw_measure (cr, itp->curmeasure, x, y, gui, itp);
 

      x += GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE;


      if (
#if 0
/* do not scale the non page views as it perturbs the display animation and anyway does not add anything */
           (Denemo.project->view != DENEMO_PAGE_VIEW && itp->line_end && itp->measurenum > si->rightmeasurenum) ||
#endif
           (Denemo.project->view == DENEMO_PAGE_VIEW && itp->line_end && itp->curmeasure->next))
        *itp->scale = (int) (100 * x / (get_widget_width (Denemo.scorearea) / gui->movement->zoom));
      else
        *itp->scale = 100;

      itp->curmeasure = itp->curmeasure->next;
      itp->mwidthiterator = itp->mwidthiterator->next;

      itp->measurenum++;
      //g_debug("line_end is %d, while itp->measurenum=%d and si->rightmeasurenum=%d\n",  itp->line_end, itp->measurenum, si->rightmeasurenum);
      if (!itp->line_end)
        {
          if (-itp->highy > itp->in_highy && -itp->highy < MAXEXTRASPACE)
            {
              //g_debug("With %d to change %d\n", -itp->highy, itp->in_highy);
              thestaff->space_above = -itp->highy;
              repeat = TRUE;
            }
          if (itp->lowy > itp->in_lowy && itp->lowy < MAXEXTRASPACE)
            {
              thestaff->space_below = itp->lowy;
              repeat = TRUE;
            }
        }
    }                           // end of loop drawing each measure
  if (scale_before != *itp->scale)
    repeat = TRUE;              /* the first system is already drawn, so it is too late to discover that we needed to scale it */
  *itp->right = itp->measurenum - 1;
  /* draw lines connecting the staves in this system at the left and right hand ends */
  if (cr)
    if (curstaff->prev)
      {
        DenemoStaff *prev = (DenemoStaff *) (curstaff->prev->data);
        cairo_set_source_rgb (cr, 0, 0, 0);
        cairo_rectangle (cr, x - SPACE_FOR_BARLINE, y - STAFF_HEIGHT - prev->space_below - thestaff->space_above, 2, 2 * STAFF_HEIGHT + prev->space_below + thestaff->space_above);
        cairo_fill (cr);
      }
  if (cr)
    if (curstaff->next)
      {
        DenemoStaff *next = (DenemoStaff *) (curstaff->next->data);
        //cairo_save(cr);
        cairo_set_source_rgb (cr, 0, 0, 0);
        cairo_rectangle (cr, x - SPACE_FOR_BARLINE, y, 2, 2 * STAFF_HEIGHT + next->space_above + thestaff->space_below);
        cairo_fill (cr);
      }
  /* end of draw lines connecting the staves in this system at the left and right hand ends */
  if (cr)
    if (itp->line_end)
      if (itp->measurenum > si->rightmeasurenum)
        if (!itp->end)
          {
             cairo_save (cr);
             gint xx = get_widget_width (Denemo.scorearea) / gui->movement->zoom - 20;
              if (Denemo.hovering_over_right_arrow)
                {
                    cairo_rectangle (cr, xx, y, 30, 40);
                    cairo_stroke (cr);
                    cairo_set_source_rgb (cr, 0.7, 1.0, 0.7);
                    cairo_rectangle (cr, xx, y, 30, 40);
                    cairo_fill (cr);
                }
              cairo_set_source_rgb (cr, 0.1, 0.2, 1);
              drawtext_cr  (cr, ">", xx, y + 30, 42.0);
              cairo_restore (cr);
          }

  if (cr)
    cairo_restore (cr);
  // if(itp->highy > title_highy)
  //  itp->highy = title_highy;

  /* now draw the staff lines, reset itp->slur_stack, and we're done */
  if (cr)
    {
      cairo_set_line_width (cr, 1.0);
      for (i = 1; i <= thestaff->no_of_lines; i++)
        {
          gint yy = y + 2 * LINE_SPACE + ((i % 2) ? (1) : (-1)) * (i / 2) * LINE_SPACE;
          cairo_move_to (cr, gui->leftmargin, yy);
          cairo_line_to (cr, (x * 100 / (*itp->scale)) - HALF_BARLINE_SPACE, yy);
        }
      cairo_stroke (cr);
    }

  /* Initialize the slur_stack for this staff. For the time being,
     slurs that begin and/or end after the portion of the music
     that is shown are not drawn. */
  if (itp->slur_stack)
    {
      g_slist_free (itp->slur_stack);
      itp->slur_stack = NULL;
    }
  if (saved_cr)
    cr = saved_cr;

  return repeat;
}


static void
print_system_separator (cairo_t * cr, gdouble position)
{
  //g_debug("At %f for %d\n", position, Denemo.scorearea->allocation.height);
#define SYSTEM_SEP (6)
  cairo_save (cr);
  cairo_set_source_rgb (cr, 0.5, 0.0, 0.0);
  cairo_rectangle (cr, 0, position - SYSTEM_SEP / 2, get_widget_width (Denemo.scorearea) / Denemo.project->movement->zoom, SYSTEM_SEP);

  cairo_set_source_rgb (cr, 0.7, 0.0, 0.0);
  cairo_fill (cr);
#undef SYSTEM_SEP
  cairo_restore (cr);
}

typedef enum colors
{ BLACK, RED, GREEN } colors;
static void
draw_playback_marker (cairo_t * cr, gint color, gint pos, gint yy, gint line_height)
{
  if (!Denemo.prefs.playback_controls)
    return;
  //g_debug("drawing marker %x at %d %d %d\n", color, pos, yy, line_height);
  cairo_save (cr);
  cairo_set_line_width (cr, 4.0);
  switch (color)
    {
    case BLACK:
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.5);
      break;
    case GREEN:
      cairo_set_source_rgba (cr, 0.0, 1.0, 0.0, 0.5);
      break;
    case RED:
      cairo_set_source_rgba (cr, 1.0, 0.0, 0.0, 0.5);
      break;
    }
  cairo_move_to (cr, pos, yy - STAFF_HEIGHT);
  cairo_line_to (cr, pos, yy - STAFF_HEIGHT + line_height);
  cairo_stroke (cr);
  switch (color)
    {
    case GREEN:
      drawlargetext_cr (cr, _("Playback Start"), pos - 100, yy);
      break;
    case RED:
      drawlargetext_cr (cr, _("Playback End"), pos - 100, yy);
      break;
    }
  cairo_restore (cr);
}

static void
draw_playback_markers (cairo_t * cr, struct infotopass *itp, gint yy, gint line_height)
{
static gboolean off;
  if (itp->startposition > 0)
    {
      if (!(Denemo.dragging_start_playback_marker && Denemo.prefs.cursor_highlight && off))
        draw_playback_marker (cr, GREEN, itp->startposition, yy, line_height);
      off = !off; //this function is called twice for each draw (for start and then end) flashing off and on requires this to toggle once only.
    }
  itp->startposition = -1;

  if (itp->endposition > 0)
      {
      if (!(Denemo.dragging_end_playback_marker && Denemo.prefs.cursor_highlight && off))
        draw_playback_marker (cr, RED, itp->endposition, yy, line_height);
    }

  itp->endposition = -1;
}

void
draw_score_area(){
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw (Denemo.scorearea);
}

#define MAX_FLIP_STAGES (Denemo.prefs.animation_steps>0?Denemo.prefs.animation_steps:1)
static gboolean
schedule_draw (gint * flip_count)
{
  if (*flip_count == -1)
    return FALSE;
  *flip_count += 1;
  //g_debug("flip count %d\n", *flip_count);
  if (*flip_count > MAX_FLIP_STAGES)
    {
      return FALSE;
    }
  draw_score_area();
  return TRUE;
}


/**
 * This actually draws the score, staff-by-staff
 * @param cr cairo context to draw with.
 * @param gui pointer to the DenemoProject structure
 * returns whether the height of the drawing area was sufficient to draw everything
 */
gboolean
draw_score (cairo_t * cr)
{                               //g_debug("draw_score %p\n", cr);
  if (cr == NULL) return TRUE;//no longer need to side effect the data while doing a dummy draw, however cr is set NULL during draw if no more drawing is needed, so we can't remove all the if (cr) conditionals...
  staffnode *curstaff;
  gint y = 0;
  struct infotopass itp;
  gboolean repeat = FALSE;
  gdouble leftmost = 10000000.0;
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  gint line_height = get_widget_height (Denemo.scorearea) * gui->movement->system_height / gui->movement->zoom;
  static gint flip_count;       //passed to a timer to indicate which stage of animation of page turn should be used when re-drawing, -1 means not animating 0+ are the stages
  
  last_tied = FALSE;
  /* Initialize some fields in itp */

  //g_debug("Printing for %d\n", flip_count);
  itp.slur_stack = NULL;
  itp.hairpin_stack = NULL;
  itp.source_displayed = FALSE;
  itp.highy = 0;                //in case there are no objects...
  itp.lowy = 0;
  itp.last_gap = 0;
  itp.last_midi = NULL;
  itp.playposition = -1;
  itp.startposition = -1;
  itp.endposition = -1;
  itp.startobj = itp.endobj = NULL;
  itp.tupletstart = itp.tuplety = 0;
  itp.recordednote = si->recording?si->recording->notes:NULL;
  itp.currentframe = (get_playback_time()/get_playback_speed())*(si->recording?si->recording->samplerate:SAMPLERATE);
  itp.allow_duration_error = FALSE;

  if (Denemo.hidden_staff_heights)  g_list_free (Denemo.hidden_staff_heights);
  Denemo.hidden_staff_heights = NULL;
  if (gui->movement->smf)
    {
      itp.startobj = Startobj;
      //g_debug("start %p\n", itp.startobj);

      itp.endobj = Endobj;
      //g_debug("Start time %p %f end time %p %f\n", itp.startobj, si->start_time, itp.endobj, si->end_time);
    }
  if (cr)
    cairo_translate (cr, movement_transition_offset (), 0);
  /* The colour for staff lines and such is black. */
  if (cr)
    cairo_set_source_rgb (cr, 0, 0, 0);

  if (cr)
    cairo_scale (cr, gui->movement->zoom, gui->movement->zoom);
  if (cr)
    cairo_translate (cr, 0.5, 0.5);
    /*draw a flag in the corner for accessing whole-movement settings */
 if (cr)
        {
         if (Denemo.hovering_over_movement)
                {
                    cairo_set_source_rgb (cr, 0.7, 1.0, 0.7);
                    cairo_rectangle (cr, 0, 0, 25, 25);
                    cairo_fill (cr);
                }
            cairo_set_source_rgb (cr, 0, 0, 0);
            cairo_rectangle (cr, 0, 0, 25, 25);
            cairo_stroke (cr);
 
            drawlargetext_cr (cr, "⚐", 2, 20);

        }

 if (cr && (si->leftmeasurenum == 1))
  {
    GList *h, *g;
    gint count;
    y = si->staffspace / 4;
    g_list_free_full (gui->braces, g_free);
    gui->braces = NULL;
    for (curstaff = si->thescore, count = 1;curstaff;curstaff=curstaff->next, count++)
        {
        DenemoStaff* staff = (DenemoStaff*)curstaff->data;
        gchar *context;
        if (staff->hidden  && (si->currentstaffnum != count)) continue;
          for (g = staff->staff_directives; g; g = g->next)
            {
              DenemoDirective *directive = g->data;
              if (directive->override & DENEMO_OVERRIDE_AFFIX)
                {
                    context =  directive->tag->str;
                   if (g_str_has_suffix (context, "Start"))
                    {
                       DenemoBrace *brace = (DenemoBrace *) g_malloc0 (sizeof(DenemoBrace));
                       brace->starty = y + staff->space_above; //g_print ("brace start %d |", brace->starty);
                       brace->curly = g_strrstr (context, "Piano") || g_strrstr (context, "Grand");
                       brace->startstaff = count;
                       gui->braces = g_list_prepend (gui->braces, brace);
                   }
                   else if (g_str_has_suffix (context, "End"))
                    {
                    gint number_of_ends = 1;
                    if(directive->data)
                        number_of_ends = atoi (directive->data->str);
                    if (number_of_ends<0 || (number_of_ends>10)) number_of_ends = 1;//sanity check on data in directive
                    while (number_of_ends--)
                        for (h=gui->braces;h;h=h->next)
                            {
                                DenemoBrace *brace = (DenemoBrace *) h->data;
                                if (brace->endstaff)
                                    continue;
                                brace->endstaff = count;
                                brace->endy = y + staff->space_above + si->staffspace/4;//g_print ("Brace end %d||", brace->endy);
                                break;
                            }
                    }
                }
            }
        y += staff->space_above +staff->space_below + si->staffspace;
        }

   // terminate all un-ended braces

    for(g=gui->braces;g;g=g->next)
          {
              DenemoBrace *brace = (DenemoBrace *) g->data;
              if (brace->endstaff == 0) {
                brace->endstaff = count-1;// g_print ("Unended staff terminated at staff %d with %d; ", count-1, y);
                brace->endy = y; //bottom staff value;
              }
          }
    gint off_screen = 0;
    gui->leftmargin = BASIC_LEFT_MARGIN + BRACEWIDTH*g_list_length (gui->braces);
    if(gui->braces)
        {
        for (count=1, curstaff = si->thescore;curstaff && (count<si->top_staff);curstaff=curstaff->next, count++)
            {
            DenemoStaff* staff = (DenemoStaff*)curstaff->data;
            if(count==1) off_screen = si->staffspace / 4;
            off_screen += staff->space_above + staff->space_below + si->staffspace;
        }

    }
    //draw all braces
    for(count=1, g=gui->braces;g;g=g->next, count++) {
            DenemoBrace *brace = (DenemoBrace *) g->data;
            cairo_save (cr);
            draw_staff_brace (cr, brace->curly, (count*BRACEWIDTH),  (brace->starty - off_screen),
                 (brace->endy-brace->starty) + (off_screen?40:15));
            cairo_restore (cr);
    }
 }
 else gui->leftmargin = 20;


  y = 0;

  /* Draw each staff */
  for (itp.staffnum = si->top_staff, curstaff = g_list_nth (si->thescore, si->top_staff - 1), (y += si->staffspace / 4); curstaff && itp.staffnum <= si->bottom_staff;  curstaff = curstaff->next, itp.staffnum++)
    {
      DenemoStaff *staff = (DenemoStaff *) curstaff->data;
      g_slist_free (itp.hairpin_stack);//clear any cresc or dim started but not finished; these can just be off-screen, they need not be in error.
      itp.hairpin_stack = NULL;
      if(cr) if (staff->hidden && (si->currentstaffnum != itp.staffnum))
          {
            static GString *hidden_text;
            if (hidden_text == NULL) 
                hidden_text = g_string_new (_("Hidden "));
            static gint height;
            gint top_height = 10; //height to put line indicating hidden staff when it is the top staff
            if (height == 0)
                height = (itp.staffnum == si->top_staff)? top_height :y - 35;
            Denemo.hidden_staff_heights = g_list_append (Denemo.hidden_staff_heights, GINT_TO_POINTER (height));   
            g_string_append_printf (hidden_text, "%s ", staff->denemo_name->str);
            {
                DenemoStaff *nextstaff = curstaff->next ? ((DenemoStaff *) curstaff->next->data) : NULL;
                if ((nextstaff==NULL) || ((!nextstaff->hidden) || (si->currentstaffnum == (1 + itp.staffnum))))
                    {
                        cairo_save (cr);
                        cairo_set_source_rgba (cr, 0.0, 0.5, 0.5, 0.6);
                        
                        if (height == top_height) //the top and possibly other staffs are hidden
                            {
                                cairo_rectangle (cr, 300, 5, get_widget_width (Denemo.scorearea) / Denemo.project->movement->zoom - 120, 3);
                                cairo_fill (cr);
                                cairo_set_source_rgba (cr, 1.0, 0.5, 0.5, 1);
                                drawnormaltext_cr (cr, hidden_text->str, 120, height);
                                
                            }
                        else 
                            {
                                cairo_rectangle (cr, 20, height, get_widget_width (Denemo.scorearea) / Denemo.project->movement->zoom - 120, 3);
                                cairo_fill (cr);
                                cairo_set_source_rgba (cr, 1.0, 0.5, 0.5, 1);
                                drawlargetext_cr (cr,hidden_text->str,  80, y - 35);
                               // drawlargetext_cr (cr, _("Hidden"), 80, y - 10);
                            }
                       g_string_assign (hidden_text, _("Hidden "));
                       height = 0;
                       cairo_restore (cr);
                    }
                }
            continue;
          }
      itp.verse = verse_get_current_view (staff);
      if (si->currentstaffnum == itp.staffnum)
        y += staff_transition_offset ();


      if (curstaff && staff->voicecontrol & DENEMO_PRIMARY)
        y += staff->space_above;

      //g_print("Incrementing vertically %d\n", y);
      itp.space_above = staff->space_above;
      gint top_y = (si->staffspace / 4) + itp.space_above;

      itp.top_y = top_y;
      //itp.y = y;
      gint highy = staff->space_above;
      gint lowy = staff->space_below;

      itp.in_highy = highy, itp.in_lowy = lowy;
      itp.highy = 0;            //do not pass on extra_space from one staff to the next
      if (flip_count <= 0)
      if (cr)
          {
            cairo_save (cr);
            cairo_set_source_rgb (cr, 0.7, 0.7, 0.7);
            cairo_rectangle (cr, 0, y, 20/*BASIC LEFT_MARGIN*/, STAFF_HEIGHT);
            cairo_fill (cr);
            cairo_set_source_rgb (cr, 0.7, 1.0, 0.7);
            if (Denemo.hovering_over_margin_up && (si->currentstaffnum == itp.staffnum))
                cairo_rectangle (cr, 0, y, 20/*BASIC LEFT_MARGIN*/, STAFF_HEIGHT/2 /*staff edit */ ), cairo_fill (cr);
            else if (Denemo.hovering_over_margin_down && (si->currentstaffnum == itp.staffnum))
                cairo_rectangle (cr, 0, y + STAFF_HEIGHT/2, 20/*BASIC LEFT_MARGIN*/, STAFF_HEIGHT/2 /*staff edit */ ), cairo_fill (cr);;
            
           
            
            cairo_set_source_rgb (cr, 0, 0, 0);
            if (Denemo.hovering_over_margin_up && (si->currentstaffnum == itp.staffnum))
                cairo_rectangle (cr, 0, y, 20/*BASIC LEFT_MARGIN*/, STAFF_HEIGHT/2 /*staff edit */ ),cairo_stroke (cr);
           else if (Denemo.hovering_over_margin_down && (si->currentstaffnum == itp.staffnum))
                cairo_rectangle (cr, 0, y + STAFF_HEIGHT/2, 20/*BASIC LEFT_MARGIN*/, STAFF_HEIGHT/2 /*staff edit */ ), cairo_stroke (cr);
            
            gint staffnumber = 1 + g_list_position (si->thescore,curstaff);
            gchar *number = g_strdup_printf ("%d", staffnumber);
            if(staffnumber>9)
                drawnormaltext_cr (cr, number, 0, y + STAFF_HEIGHT - 2);
            else
                drawlargetext_cr (cr, number, 0, y + STAFF_HEIGHT - 2);
            g_free (number);
            drawnormaltext_cr (cr, "⚐", 5, y + STAFF_HEIGHT/2 - 6);
            cairo_restore (cr);

            if (si->leftmeasurenum == 1 && !(staff->voicecontrol & DENEMO_SECONDARY))
              {
                /* draw background of clef, keysig, timesig */
                gint key = gui->movement->maxkeywidth;
                gint cmajor = key ? 0 : 5;      //allow some area for keysig in C-major
                cairo_save (cr);

                cairo_set_source_rgb (cr, 0.7, Denemo.hovering_over_clef?1.0:0.7, 0.7);
                if (si->currentstaffnum == itp.staffnum)
                    cairo_rectangle (cr, gui->leftmargin, y, (gui->leftmargin+35) - gui->leftmargin - cmajor, STAFF_HEIGHT);  /*clef edit */
                cairo_fill (cr);
                
                cairo_set_source_rgb (cr, 0.7, Denemo.hovering_over_timesig?1.0:0.7, 0.7);
                cairo_rectangle (cr, (gui->leftmargin+35) + key + cmajor, y, SPACE_FOR_TIME - cmajor, STAFF_HEIGHT);      /*timesig edit */
                cairo_fill (cr);

                cairo_set_source_rgba (cr, Denemo.hovering_over_keysharpen?0.0:0.2, Denemo.hovering_over_keysharpen?0.0:0.2, Denemo.hovering_over_keysharpen?1.0:0.9, 0.5);
                cairo_rectangle (cr, (gui->leftmargin+35) - cmajor, y, key + 2 * cmajor, STAFF_HEIGHT / 2);       /*keysig sharpen edit */
                cairo_fill (cr);
                
                
                if (Denemo.hovering_over_keysharpen)
                    {
                    cairo_set_source_rgb (cr, 0, 0, 0);
                    cairo_set_line_width (cr, 3);
                    cairo_rectangle (cr, (gui->leftmargin+35) - cmajor -2 , y-2, key + 2 * cmajor+4, STAFF_HEIGHT / 2 +4);
                    cairo_stroke (cr);
                    }

                //cairo_set_source_rgb (cr, 1, 0.5, 0.5);
                cairo_set_source_rgba (cr, Denemo.hovering_over_keyflatten?1.0:0.8, Denemo.hovering_over_keyflatten?0.0:0.4, Denemo.hovering_over_keyflatten?0.0:0.4, 0.5);

                cairo_rectangle (cr, (gui->leftmargin+35) - cmajor, y + STAFF_HEIGHT / 2, key + 2 * cmajor, STAFF_HEIGHT / 2);    /*keysig flatten edit */
                cairo_fill (cr);
                if (Denemo.hovering_over_keyflatten)
                    {
                    cairo_set_source_rgb (cr, 0, 0, 0);
                    cairo_set_line_width (cr, 3);
                    cairo_rectangle (cr, (gui->leftmargin+35) - cmajor -2, y + STAFF_HEIGHT / 2 -2, key + 2 * cmajor +4, STAFF_HEIGHT / 2 +4);
                    cairo_stroke (cr);
                    }


                cairo_restore (cr);
              }
          }                     //if cr
      if (si->currentstaffnum == itp.staffnum)
        {

          gint count = count_syllables (staff, si->leftmeasurenum);
          //g_print ("Count syllables from %d yields %d last_tied \n", si->leftmeasurenum, count, last_tied);
          if (count < 0)
            {
              count = -count;
              itp.slur_stack = push_slur_stack (itp.slur_stack, 0, 0);
            }
          reset_lyrics (staff, count);
        }


      itp.measurenum = si->leftmeasurenum;
      itp.line_end = FALSE;
      itp.left = &gui->lefts[0];
      itp.right = &gui->rights[0];
      itp.scale = &gui->scales[0];



      if (draw_staff (flip_count > 0 ? NULL : cr, curstaff, y, gui, &itp))
        repeat = TRUE;

      if (cr)
        draw_playback_markers (cr, &itp, y, line_height);

      gint system_num;
      system_num = 1;
      //g_debug("Drawn staffnum %d, at %d %s.\n", itp.staffnum,  y, itp.line_end?" another line":"End");

      if (cr)
        if (itp.staffnum == si->top_staff)
          print_system_separator (cr, line_height * system_num);

      system_num++;

      // This block prints out continuations of the staff just printed
      {
        int yy;
        yy = y + line_height;
        itp.left++;
        itp.right++;
        itp.scale++;
        if (Denemo.project->movement->playingnow && itp.measurenum >= si->rightmeasurenum)
          itp.line_end = FALSE; //don't print whole lines of grayed out music during playback

        while (((itp.left - gui->lefts) < DENEMO_MAX_SYSTEMS - 1) && itp.line_end && (yy < (get_widget_height (Denemo.scorearea) / gui->movement->zoom)))
          {
            if (cr)
              if (itp.staffnum == si->top_staff)
                print_system_separator (cr, line_height * system_num);
            system_num++;
            if (draw_staff (cr, curstaff, yy, gui, &itp))
              repeat = TRUE;
            if (itp.staffnum == si->top_staff)  //single criterion for all staffs on whether to draw next page
              leftmost = MIN (leftmost, itp.leftmosttime);
            if (cr)
              draw_playback_markers (cr, &itp, yy, line_height);
            yy += line_height;
            itp.left++;
            itp.right++;
            itp.scale++;

          }                     //end while printing out all the systems for this staff

        //g_debug("staff num %d measure %d playhead %f left time %f\nheight %d system_num %d\n", itp.staffnum,itp.measurenum, si->playhead, itp.leftmosttime, yy, system_num);

        si->rightmost_time = itp.rightmosttime;//g_debug("Setting rightmost time to %f\n", si->rightmost_time);

        if ((system_num > 2) && Denemo.project->movement->playingnow && (si->playhead > leftmost) && itp.measurenum <= g_list_length (((DenemoStaff *) curstaff->data)->themeasures) /*(itp.measurenum > (si->rightmeasurenum+1)) */ )
          {
            //put the next line of music at the top with a break marker
            itp.left = &gui->lefts[0];
            itp.right = &gui->rights[0];
            itp.scale = &gui->scales[0];
            if (cr)
              {
                cairo_save (cr);
                cairo_set_source_rgb (cr, 0.0, 0.0, 1.0);       //Strong Blue Line to break pages
                cairo_rectangle (cr, 0, line_height - 10, get_widget_width (Denemo.scorearea) / Denemo.project->movement->zoom, 10);
                cairo_fill (cr);
                cairo_restore (cr);
              }

            itp.line_end = FALSE;       //to force print of timesig
            if (itp.measurenum > (si->rightmeasurenum + 1))
              itp.measurenum = si->rightmeasurenum + 1;
            gdouble flip;
            flip = 1.0;
            if ((itp.staffnum == si->top_staff) && (flip_count == -1))
              {
                flip = 0.1;
                flip_count = 0;
                //g_debug("Adding timeout");
                g_timeout_add (1000 / MAX_FLIP_STAGES, (GSourceFunc) schedule_draw, &flip_count);
              }
            //g_debug("drawing %d\n", flip_count);
            if (flip_count > 0 && flip_count < MAX_FLIP_STAGES)
              flip = flip_count / (gdouble) MAX_FLIP_STAGES;
            if (cr)
              {
                cairo_translate (cr, get_widget_width (Denemo.scorearea) * (1 - flip) * 0.5 / Denemo.project->movement->zoom, 0.0);
                cairo_scale (cr, flip, 1.0);

                if (draw_staff (flip_count > 0 ? cr : NULL, curstaff, y, gui, &itp))
                  repeat = TRUE;
                cairo_scale (cr, 1 / flip, 1.0);
                cairo_translate (cr, -get_widget_width (Denemo.scorearea) * (1 - flip) * 0.5 / Denemo.project->movement->zoom, 0.0);
              }
            //draw_break_marker();
          }
        else
          {
            if (flip_count != -1)
              repeat = TRUE;
            //g_debug("Repeating %d\n", repeat);
            flip_count = -1;
          }
       //   if(itp.rightmosttime != si->rightmost_time)
        //      g_debug("Resetting %f %f? ",itp.rightmosttime, si->rightmost_time);
        // itp.rightmosttime = si->rightmost_time;//We want to ignore the rightmost_time of the flipped over top system that belongs to the next page

      }                         //end of block printing continuations
      *itp.left = 0;            //To signal end of valid systems

      if ((!curstaff->next) || ((DenemoStaff *) curstaff->next->data)->voicecontrol & DENEMO_PRIMARY)
        {
          if (itp.verse)
            {
              y += LYRICS_HEIGHT;
            }
        if (curstaff->next)
        {
          DenemoStaff *next = (DenemoStaff *) (curstaff->next->data);
          y += (si->staffspace - next->space_shorten + staff->space_below);
        } else
         y += (si->staffspace + staff->space_below);
      }

    }                           // for all the staffs

  //g_debug("Right most time %f\n", si->rightmost_time);
  //  if(itp.last_midi)
  //  si->rightmost_time = get_midi_off_time(itp.last_midi);


  return repeat;

  /* And we're done */
}




static gint
draw_callback (cairo_t * cr)
{
  DenemoProject *gui = Denemo.project;

  //g_debug("expose\n");
  if ((!Denemo.project) || (!Denemo.project->movement) || (!Denemo.project->movement->currentmeasure))
    {
      g_warning ("Cannot draw!");
      return TRUE;
    }

  /* Layout the score. */
  if (layout_needed)
    if (draw_score (NULL))
      {
        set_bottom_staff (gui);
        update_vscrollbar (gui);
      }
  layout_needed = TRUE;
  if(Denemo.project->movement->playingnow)
    gtk_widget_queue_draw (Denemo.playbackview);

  /* Clear with an appropriate background color. */
  if (Denemo.project->input_source != INPUTKEYBOARD && Denemo.project->input_source != INPUTMIDI && (Denemo.prefs.overlays || (Denemo.project->input_source == INPUTAUDIO)) && pitch_entry_active (gui))
    {
      cairo_set_source_rgb (cr, 0xAD/255.0, 0xD8/255.0, 0xE6/255.0);  //light blue
    }
  else if (gtk_widget_has_focus (Denemo.scorearea) && gtk_widget_is_focus (Denemo.scorearea))
    {
      if (Denemo.project->input_source == INPUTMIDI && (Denemo.keyboard_state == GDK_LOCK_MASK || Denemo.keyboard_state == GDK_SHIFT_MASK))      //listening to MIDI-in
        cairo_set_source_rgb (cr, 0.9, 0.85, 1.0);
      else if (Denemo.project->input_source == INPUTMIDI && Denemo.keyboard_state == GDK_CONTROL_MASK)      //checking pitches
        cairo_set_source_rgb (cr, 0.85, 1.0, 0.9);
      else
        cairo_set_source_rgb (cr, ((0xFF0000 & Denemo.color) >> 16) / 255.0, ((0xFF00 & Denemo.color) >> 8) / 255.0, ((0xFF & Denemo.color)) / 255.0);
    }

  else
    {
      cairo_set_source_rgb (cr, 0.8, 0.8, 0.8); //gray background when key strokes are not being received.
    }
  cairo_paint (cr);
  /* Draw the score. */
  draw_score (cr);
  return TRUE;
}

void
update_drawing_cache (void)
{
  if(Denemo.non_interactive)
    return;
  draw_score (NULL);
}

/**
 * Here we have the function that actually draws the score. Note that
 * it does not clip intelligently at all
 */
#if GTK_MAJOR_VERSION==3
gint
scorearea_draw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return draw_callback (cr);
}
#else
gint
scorearea_draw_event (GtkWidget * widget, GdkEventExpose * event)
{
  if (widget == NULL)
    {
      draw_score (NULL);
      return TRUE;
    }
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  draw_callback (cr);
  cairo_destroy (cr);
  return TRUE;
}
#endif
