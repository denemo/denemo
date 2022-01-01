/* drawnotes.cpp
 *
 * Functions for drawing notes and rests
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

#include "core/utils.h"         /* Includes <gdk.h> */
#include "command/lilydirectives.h"
#include "command/scorelayout.h"
#include "display/drawingprims.h"
#include "display/notewidths.h"
#include <math.h>
#include "display/slurs.h"

gint restwidths[SMALLESTDURATION + 1] = { WHOLEREST_WIDTH, HALFREST_WIDTH, QUARTERREST_WIDTH, EIGHTHREST_WIDTH,
  SIXTEENTHREST_WIDTH, THIRTYSECONDREST_WIDTH, SIXTYFOURTHREST_WIDTH, HUNDREDTWENTYEIGHTHREST_WIDTH, TWOHUNDREDFIFTYSIXTHREST_WIDTH
};

gint headwidths[3] = { WHOLEHEAD_WIDTH, HALFHEAD_WIDTH, NOTEHEAD_WIDTH
};



static void
draw_selection_shading (cairo_t * cr, DenemoDirective * directive, gdouble x, gdouble y, gdouble diameter)
{
  if (directive == Denemo.project->movement->directive_on_clipboard)
    {
      cairo_save (cr);
      cairo_set_source_rgba (cr, 0.4, 0.8, 0.5, 0.7);
      cairo_arc (cr, x, y - 4, 2 * diameter, 0.0, 2 * M_PI);    //FIXME put these adjustments back into the caller code and pass diameter and y as final values
      cairo_fill (cr);
      cairo_restore (cr);
    }
}

/**
 * draw_dots
 * This draws dots after rests or notes
 *
 */
static void
draw_dots (cairo_t * cr, gint xstart, gint ystart, gint numdots)
{
  xstart += 5;
  for (; numdots; numdots--, xstart += 6)
    {
      cairo_arc (cr, xstart, ystart, 2.5, 0.0, 2 * M_PI);
      cairo_fill (cr);
    }
}

/**
 * draw_rest
 * This function actually draws a rest onto the backing pixmap
 *
 */
static void
draw_rest (cairo_t * cr, gint duration, gint numdots, gint xx, gint y, DenemoGraphic * override_rest, gint gx, gint gy)
{
  static gint restoffsets[SMALLESTDURATION + 1] = { WHOLEREST_OFFSETFROMTOP, HALFREST_OFFSETFROMTOP,
    QUARTERREST_OFFSETFROMTOP, EIGHTHREST_OFFSETFROMTOP,
    SIXTEENTHREST_OFFSETFROMTOP, THIRTYSECONDREST_OFFSETFROMTOP,
    SIXTYFOURTHREST_OFFSETFROMTOP,
    HUNDREDTWENTYEIGHTHREST_OFFSETFROMTOP,
    TWOHUNDREDFIFTYSIXTHREST_OFFSETFROMTOP
  };

  static gunichar rest_char[SMALLESTDURATION + 1] = { 0x20, 0x21, 0x27, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2d      /*note no glyph for this one! */
  };
  if (duration > SMALLESTDURATION)
	duration = 0;
  if (override_rest)
    drawbitmapinverse_cr (cr, override_rest, xx + gx + restwidths[0] - override_rest->width / 2, y + restoffsets[0] + gy - override_rest->height / 2, FALSE);
  else
    drawfetachar_cr (cr, rest_char[duration], xx, y + restoffsets[duration]);
  /* Now draw any trailing dots and we're done */

  draw_dots (cr, xx + restwidths[duration], y + restoffsets[duration] - HALF_LINE_SPACE, numdots);
}

/**
 *  draw_notehead
 *  This function actually draws the note onto the backing pixmap
 *
 */
static void
draw_notehead (cairo_t * cr, note * thenote, gint duration, gint numdots, gint xx, gint y, gint * accs, gint is_stemup, DenemoGraphic * override_notehead, gint gx, gint gy, gboolean at_cursor, gboolean percussion)
{
  /* Adam's changed this code; it used to be that these arrays only had
     three elements.  The change has defeated what had been semi-elegance;
     grrr.  */

  static gint headwidths[7] = { WHOLEHEAD_WIDTH, HALFHEAD_WIDTH,
    NOTEHEAD_WIDTH, DIAMOND_WIDTH,
    CROSS_WIDTH, HARMONIC_WIDTH, DIAMOND_WIDTH,
  };

  static gunichar head_char[7] = { 0x54, 0x55, 0x56, 0x93, 0x64, 0x92, 0x56
  };
  gint height = thenote->y;
  gint noteheadtype = 1;
  gint enshift = thenote->enshift;
  gint pitch = offsettonumber (thenote->mid_c_offset);

  if (thenote->noteheadtype == DENEMO_NORMAL_NOTEHEAD)
    {
      noteheadtype = MIN (duration, 2); /* Index of relevant notehead */
    }
  else if (thenote->noteheadtype == DENEMO_CROSS_NOTEHEAD)
    {
      noteheadtype = 4;         /* Index of relevant notehead */
    }
  else if (thenote->noteheadtype == DENEMO_HARMONIC_NOTEHEAD)
    {
      noteheadtype = 5;
    }
  else if (thenote->noteheadtype == DENEMO_BLACK_NOTEHEAD)
    {
      noteheadtype = 2;
    }
  if (duration < 0)
    {
        if (duration < -7)
          noteheadtype = 0;
        else
          {
             cairo_set_source_rgba (cr, 0.2, 0.2, 0.8, 1.0);
             noteheadtype = MIN (-duration, 2);
          }
    }

  /* Draw the accidental, if necessary.  Note that this has to be
     done before xx is modified, as the the value in
     position_of_accidental already accounts for any reverse-alignment
     in the chord.  */
  if (thenote->showaccidental && !percussion)
    {
      if (cr)
        {
          draw_accidental (cr, xx - thenote->position_of_accidental, y + height, enshift);
          if (thenote->showaccidental == DENEMO_REMINDER)
            drawtext_cr (cr, "!", xx - thenote->position_of_accidental - 8.0, y + height, 22.0);
          if (thenote->showaccidental == DENEMO_CAUTIONARY)
            {
              drawtext_cr (cr, "(", xx - thenote->position_of_accidental - 8.0, y + height, 22.0);
              drawtext_cr (cr, ")", xx - thenote->position_of_accidental + 6.0, y + height, 22.0);      //there are actual widths in accwidths.h which could be used instead of 6.0
            }
        }
      accs[pitch] = enshift;
    }

  if (thenote->reversealign)
    {
      if (is_stemup)
        xx += headwidths[noteheadtype];
      else
        xx -= headwidths[noteheadtype];
    }
  //OVERRIDE_GRAPHIC on the note directive just affects the head, everything else -  accidentals dots stems beams ...are controlled by chord/chord-directives. Even the notehead is set by the chord directives if override_notehead is set
  if (cr)
    if (!(get_override (thenote->directives) & DENEMO_OVERRIDE_GRAPHIC))
      {
        if (override_notehead)
          {
            //g_debug("drawing a chord override graphic at %d %d\n",  xx+gx-override_notehead->width/2,  y+height+gy-override_notehead->height/2);
            drawbitmapinverse_cr (cr, override_notehead, xx + gx - override_notehead->width / 2, y + height + gy - override_notehead->height / 2, FALSE);
          }
        else
          {
            if (is_stemup)
              drawfetachar_cr (cr, head_char[noteheadtype], xx, y + height);
            else
              drawfetachar_cr (cr, head_char[noteheadtype], xx - 0.5, y + height);
          }
      }

  gint maxwidth = headwidths[noteheadtype];

  /* any display for note directives */
  if (cr)
    {
        gint thiswidth = draw_for_directives (cr, thenote->directives, xx, y + thenote->y, at_cursor);
        maxwidth = MAX (thiswidth, maxwidth);
    }
  if (cr)
    {
      /* Now draw any trailing dots */
      if ((height % LINE_SPACE) == 0)
        draw_dots (cr, xx + maxwidth, y + height - HALF_LINE_SPACE, numdots);
      else
        draw_dots (cr, xx + maxwidth, y + height, numdots);
    }
}

/**
 * Draw the legder lines on the score
 * Modified RTS 2011, ledger lines must not coalesce, must project from noteheads and stems
 * there is not much room, hence the sleight of hand
 */
void
draw_ledgers (cairo_t * cr, gint greaterheight, gint lesserheight, gint xx, gint y, gint width)
{
  int ledgerheight;

#define EXTRA_ON_LEDGER 1.5

  cairo_set_line_width (cr, 1.0);
  /* Draw the top ledger lines */
  for (ledgerheight = 2*LINE_SPACE; ledgerheight >= greaterheight; ledgerheight -= LINE_SPACE)
    {
      cairo_move_to (cr, xx + ((ledgerheight == greaterheight) ? (-EXTRA_ON_LEDGER) : (-2)), ledgerheight + y);
      cairo_line_to (cr, xx + width + ((ledgerheight == greaterheight) ? EXTRA_ON_LEDGER : (-2)), ledgerheight + y);
    }

  /* Almost identically, draw the bottom ones */
  for (ledgerheight =  2*LINE_SPACE; ledgerheight <= lesserheight; ledgerheight += LINE_SPACE)
    {
      cairo_move_to (cr, xx + ((ledgerheight == lesserheight) ? -EXTRA_ON_LEDGER : 2), ledgerheight + y);
      cairo_line_to (cr, xx + width + ((ledgerheight == lesserheight) ? EXTRA_ON_LEDGER : 2), ledgerheight + y);
    }

  cairo_stroke (cr);
}

/**
 * Draw the chord object on the score
 * returns the highest y value drawn at
 */
gint
draw_chord (cairo_t * cr, objnode * curobj, gint xx, gint y, gint mwidth, gint * accs, gboolean selected, gboolean at_cursor)
{
  gint highest = 0;
  static gunichar upstem_char[SMALLESTDURATION + 1] = { 0, 0, 0, 0xb9, 0xba,
    0xbb,
    0xbc,
    0xbd,
    0xbd
  };

  static gunichar downstem_char[SMALLESTDURATION + 1] = { 0, 0, 0, 0xbe, 0xc1,
    0xc2,
    0xc3,
    0xc4,
    0xc4
  };

  DenemoObject *prevmuditem = (DenemoObject *) (curobj->prev ? curobj->prev->data : NULL);
  DenemoObject *mudelaitem = (DenemoObject *) curobj->data;
  DenemoObject *nextmuditem = (DenemoObject *) (curobj->next ? curobj->next->data : NULL);
  chord thechord = *(chord *) mudelaitem->object;
  gint noteheadtype;
  gint duration = thechord.baseduration;
   if (duration < 0)
    {
      duration = -duration;
      noteheadtype = 2; //3;!!!! instead of diamond it does reversealign
    }
   else
    noteheadtype = MIN (duration, 2);
  
  gint i;
  gint beampainty, arcwidth;

  gint prevbaseduration, nextbaseduration;
  GList *curnode;
  gint is_grace = (thechord.notes ? thechord.is_grace : 0);
  DenemoGraphic *override_notehead = NULL;      //overriding notehead to be used for all notes of chord unless built-in or overriden
  gint gx = 0, gy = 0;          //positioning for overriding notehead
  if (cr)
    {
      cairo_save (cr);
      cairo_set_line_width (cr, 1.0);
      if (is_grace)
        {
          note *thenote = (note *) thechord.notes->data;
          cairo_translate (cr, xx, y + thenote->y);
          cairo_scale (cr, 0.8, 0.8);
          cairo_translate (cr, -xx, -(y + thenote->y));
        }
      //g_debug("Invisible is %d\n", mudelaitem->isinvisible);
      if (mudelaitem->isinvisible)
        {
          if (selected)
            cairo_set_source_rgb (cr, 231.0 / 255, 1, 39.0 / 255);
          else if (Denemo.project->movement->recording)
            cairo_set_source_rgba (cr, 180.0 / 255, 160.0 / 255, 32.0 / 255, 0.4);      // yellow for non printing
          else
            cairo_set_source_rgb (cr, 180.0 / 255, 160.0 / 255, 32.0 / 255);    // yellow for non printing

        }

      if (thechord.slur_begin_p)
        draw_slur_start (cr, xx, y);

      if (thechord.slur_end_p)
        draw_slur_end (cr, xx, y);

      if (thechord.crescendo_begin_p)
        drawlargetext_cr (cr, "𝆒", xx - 10, y + STAFF_HEIGHT + 10);  //FIXME the cresc musical sign is too small, hence largetext, but | is then too large
      if (thechord.crescendo_end_p)
        drawlargetext_cr (cr, "𝆒.", xx + 10, y + STAFF_HEIGHT + 10);

      if (thechord.diminuendo_begin_p)
        drawlargetext_cr (cr, "𝆓", xx - 10, y + STAFF_HEIGHT + 10);
      if (thechord.diminuendo_end_p)
        drawlargetext_cr (cr, "𝆓·", xx + 10, y + STAFF_HEIGHT + 10);
    }

  gboolean override_chord_graphic = FALSE;
  if (cr)
    {
      GList *g;
      gint count = 0;
      for (g = thechord.directives; g; g = g->next)
        {
          DenemoDirective *directive = (DenemoDirective *) g->data;
          guint layout = selected_layout_id ();
          gdouble only = (directive->layouts && !wrong_layout (directive, layout)) ? 0.5 : 0.0;
          gdouble exclude = (directive->layouts && wrong_layout (directive, layout)) ? 0.9 : 0.0;
          if (wrong_layout (directive, layout))
            exclude = 0.9;
          if (exclude > 0.0 || only > 0.0)
            {
              cairo_save (cr);
              cairo_set_source_rgba (cr, 0.4 + exclude - only / 2, 0.5 + only, 0.4 - only / 2, at_cursor ? 1.0 : 0.7);
              //cairo_set_source_rgba (cr, 0.4 + exclude, 0.5 + only, 0.4, at_cursor ? 1.0 : 0.5); green is too pale.
            }
          if (directive->graphic)
            {

              if ((directive->override & DENEMO_OVERRIDE_GRAPHIC))
                override_chord_graphic = TRUE;

              if ((directive->override & DENEMO_OVERRIDE_GRAPHIC))
                {
                  if (directive->override & DENEMO_ALT_OVERRIDE)
                    {
                      gx = directive->gx;
                      gy = directive->gy;
                      override_notehead = directive->graphic;   //will be used to draw all the notes/the rest
                    }
                  else
                    {
                      draw_selection_shading (cr, directive, xx + directive->gx, y + STAFF_HEIGHT + 40 + directive->gy - 4, MAX (directive->graphic->width, 8.0));
                      drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx - directive->graphic->width / 2, y + STAFF_HEIGHT + 40 + directive->gy - directive->graphic->height / 2, FALSE);
                    }
                }
              else
                {               //this directive's graphic does not override entire chord (other ones may)
                  if (directive->override & DENEMO_ALT_OVERRIDE)
                    {           //ALT_OVERRIDE makes the positioning stem sensitive
                      //FIXME - use count to stack up multiple markings
                      gdouble yval = (thechord.is_stemup ? (y + thechord.lowesty + 8 + count + directive->gy) : (y + thechord.highesty - 8 - count - directive->gy));
                      draw_selection_shading (cr, directive, xx + directive->gx, yval - 4, MAX (directive->graphic->width, 8.0));
                      drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx - directive->graphic->width / 2 + 4, yval - directive->graphic->height / 2, thechord.is_stemup);
                      if (!thechord.is_stemup)
                        highest = ((y + thechord.highesty + directive->gy - 16 - 2 * count) - directive->graphic->height / 2);

                    }
                  else
                    {
                      if (directive->override & DENEMO_OVERRIDE_ABOVE)
                        {
                          gint posy;
                          if (thechord.highesty < 0)
                            posy = y - 14 + thechord.highesty - count + directive->gy;
                          else
                            posy = y + 1 - count - STAFF_HEIGHT / 2 + directive->gy;
                          draw_selection_shading (cr, directive, xx + directive->gx, posy - 4, MAX (directive->graphic->width, 8.0));
                          drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx - directive->graphic->width / 2, posy + directive->graphic->height / 2, FALSE);
                        }
                      else
                        {
                          draw_selection_shading (cr, directive, xx + directive->gx, y + STAFF_HEIGHT + 8 + thechord.lowesty + count + directive->gy - 4, MAX (directive->graphic->width, 8.0));
                          drawbitmapinverse_cr (cr, directive->graphic, xx + directive->gx - directive->graphic->width / 2, y + STAFF_HEIGHT + 8 + thechord.lowesty + count + directive->gy - directive->graphic->height / 2, FALSE);
                        }
                    }
                }
            }
          if (directive->display)
            {
#define MAXLEN (8)
              gchar c = 0;      //if it is a long string only show it all when cursor is on it also only display from first line
              gchar *p;
              for (p = directive->display->str; *p; p++)
                {
                  if (*p == '\n' || (!at_cursor && (p - directive->display->str) > MAXLEN))
                    {
                      c = *p;
                      *p = 0;
                      break;
                    }
                }

              draw_selection_shading (cr, directive, xx + directive->tx + 4, y + ((thechord.highesty > -10) ? -10 : thechord.highesty) - 8 - 4 + count + directive->ty, 8);
              drawnormaltext_cr (cr, directive->display->str, xx + directive->tx, y + ((thechord.highesty > -10) ? -10 : thechord.highesty) - 8 + count + directive->ty);

              highest = y + ((thechord.highesty > -10) ? -10 : thechord.highesty) - 8 + count + directive->ty - 10 /*for height of text */ ;
              if (c)
                {
                  *p = c;
                }
            }
          count += 16;
          if (exclude > 0.0 || only > 0.0)
            cairo_restore (cr);
        }                       //for each chord directive
    }                           //if drawing do chord directives
  if ((!override_chord_graphic) || (override_chord_graphic && override_notehead))
    {
      if (!thechord.notes /* a rest */ )
        {
          if (cr)
            draw_rest (cr, MAX (duration, 0), thechord.numdots, xx, y, override_notehead, gx, gy);
        }
      else
        {
          /* Draw the noteheads and accidentals */
          for (curnode = thechord.notes; curnode; curnode = curnode->next)
            {
              note *thenote = (note *) curnode->data;
              draw_notehead (cr, thenote, thechord.baseduration, thechord.numdots, xx, y, accs, thechord.is_stemup, override_notehead, gx, gy, at_cursor, mudelaitem->isinvisible);
            }
        }
    }
  if (!cr)
    return highest;
  /* Now the stem and beams. This is complicated. */
  if (thechord.notes /* not a rest */ )
    {
      if (thechord.is_stemup)
        {
          if (!override_chord_graphic)
            {
              if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
                {
                  if (duration >= 3)
                    /* Up-pointing stem pixmap */
                    drawfetachar_cr (cr, upstem_char[duration], xx + NOTEHEAD_WIDTH, thechord.highesty + y + 3 - (duration == 6 ? EXTRA_STEM_HEIGHT : STEM_HEIGHT));
                }
              else if (nextmuditem && !mudelaitem->isend_beamgroup)
                {
                  /* Draw the thin beam across the gap */
                  cairo_rectangle (cr, xx + headwidths[noteheadtype] - 1, y + thechord.stemy, nextmuditem->x - mudelaitem->x, THINBEAM_HEIGHT);
                  cairo_fill (cr);
                  if (mudelaitem->isstart_beamgroup || !prevmuditem)
                    prevbaseduration = 0;
                  else
                    prevbaseduration = ((chord *) prevmuditem->object)->baseduration;
                  nextbaseduration = ((chord *) nextmuditem->object)->baseduration;
                  for (i = 4, beampainty = thechord.stemy + FIRSTBEAMSPACE; i <= duration; i++, beampainty += SUBSQBEAMSPACE)
                    {
                      if (nextbaseduration >= i)
                        {
                          /* Draw a thick beam across the gap */
                          cairo_rectangle (cr, xx + headwidths[noteheadtype] - 1, y + beampainty, nextmuditem->x - mudelaitem->x, THICKBEAM_HEIGHT);
                          cairo_fill (cr);

                        }
                      else if (prevbaseduration < i)
                        {
                          /* Draw a stub to the right of the staff */
                          cairo_rectangle (cr, xx + headwidths[noteheadtype] - 1, y + beampainty, STUB_WIDTH, THICKBEAM_HEIGHT);
                          cairo_fill (cr);
                        }
                    }           /* end for loop */
                }               /* end drawing for non-end-beamgroup notes */
              else
                {               /* We're at the end of a beamgroup */
                  if (prevmuditem)
                    for (i = MAX (((chord *) prevmuditem->object)->baseduration + 1, 4), beampainty = thechord.stemy + FIRSTBEAMSPACE + (SUBSQBEAMSPACE * (i - 4)); i <= duration; i++, beampainty += SUBSQBEAMSPACE)
                      {
                        /* Draw a stub to the left of the staff */
                        cairo_rectangle (cr, xx + headwidths[noteheadtype] - 1 - STUB_WIDTH, y + beampainty, STUB_WIDTH, THICKBEAM_HEIGHT);
                        cairo_fill (cr);
                      }
                }

              if (duration > 0)
                {
                  /* Vertical line */
                  cairo_move_to (cr, xx + headwidths[noteheadtype], thechord.stemy + y);
                  cairo_line_to (cr, xx + headwidths[noteheadtype], thechord.lowesty + y - 2);
                  if (is_grace & ACCIACCATURA)
                    {
                      cairo_set_line_width (cr, 2.0);
                      cairo_move_to (cr, xx + headwidths[noteheadtype] - 3, thechord.lowesty + y - 2 - 4);
                      cairo_line_to (cr, xx + headwidths[noteheadtype] + 6, thechord.stemy + y);
                    }
                  cairo_stroke (cr);
                }
            }                   //if graphic not overrriden

          /* Now draw the tie, if appropriate */
          if (thechord.is_tied)
            {
              if (nextmuditem)
                arcwidth = nextmuditem->x - mudelaitem->x;
              else
                arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;

              cairo_set_line_width (cr, 2.0);
              cairo_move_to (cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13);
              cairo_rel_curve_to (cr, arcwidth / 3, -8, arcwidth * 2 / 3, -8, arcwidth, 0);
              cairo_stroke (cr);
            }
        }                       /* End stemup stuff */
      else
        {

          if (!override_chord_graphic)
            {

              /* chord is stemdown */
              if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
                {
                  if (duration >= 3)
                    /* Down-pointing stem */
                    drawfetachar_cr (cr, downstem_char[duration], xx, thechord.lowesty + y + (duration == 6 ? EXTRA_STEM_HEIGHT : STEM_HEIGHT));
                }
              else if ((nextmuditem) && !mudelaitem->isend_beamgroup)
                {
                  /* Draw the thin beam across the gap */
                  cairo_rectangle (cr, xx, y + thechord.stemy - THINBEAM_HEIGHT + 1, nextmuditem->x - mudelaitem->x, THINBEAM_HEIGHT);
                  cairo_fill (cr);

                  if (mudelaitem->isstart_beamgroup || !prevmuditem)
                    prevbaseduration = 0;
                  else
                    prevbaseduration = ((chord *) prevmuditem->object)->baseduration;

                  nextbaseduration = ((chord *) nextmuditem->object)->baseduration;
                  for (i = 4, beampainty = thechord.stemy - FIRSTBEAMSPACE - THICKBEAM_HEIGHT + 1; i <= duration; i++, beampainty -= SUBSQBEAMSPACE)
                    {
                      if (nextbaseduration >= i)
                        {
                          /* Draw a thick beam across the gap */
                          cairo_rectangle (cr, xx, y + beampainty, nextmuditem->x - mudelaitem->x, THICKBEAM_HEIGHT);
                          cairo_fill (cr);

                        }
                      else if (prevbaseduration < i)
                        {
                          /* Draw a stub to the right of the staff */
                          cairo_rectangle (cr, xx, y + beampainty, STUB_WIDTH, THICKBEAM_HEIGHT);
                          cairo_fill (cr);
                        }
                    }           /* End for loop */
                }               /* End drawing for non-end-beamgroup notes */
              else
                {               /* We're at the end of a beamgroup */
                  if (prevmuditem)      //sanity check
                    for (i = MAX (((chord *) prevmuditem->object)->baseduration, 4), beampainty = thechord.stemy - FIRSTBEAMSPACE - THICKBEAM_HEIGHT + 1 - (SUBSQBEAMSPACE * (i - 4)); i <= duration; i++, beampainty -= SUBSQBEAMSPACE)
                      {
                        /* Draw a stub to the left of the staff */
                        cairo_rectangle (cr, xx - STUB_WIDTH, y + beampainty, STUB_WIDTH, THICKBEAM_HEIGHT);
                        cairo_fill (cr);
                      }
                }

              if (duration > 0)
                {
                  /* Vertical line */
                  cairo_move_to (cr, xx, thechord.highesty + y + 2);
                  cairo_line_to (cr, xx, thechord.stemy + y);
                  cairo_stroke (cr);
                }
            }
          /* Now draw the tie, if appropriate */
          if (thechord.is_tied)
            {
              if (nextmuditem)
                arcwidth = nextmuditem->x - mudelaitem->x;
              else
                arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
              cairo_set_line_width (cr, 2.0);
              cairo_move_to (cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13);
              cairo_rel_curve_to (cr, arcwidth / 3, -8, arcwidth * 2 / 3, -8, arcwidth, 0);
              cairo_stroke (cr);
            }
        }
      /* End stemdown stuff */


      if (is_grace)
        cairo_restore (cr);
      draw_ledgers (cr, thechord.highesty, thechord.lowesty, xx, y, headwidths[noteheadtype]);
      if (is_grace)
        cairo_save (cr);

    }                           /* end if not a rest draw stems etc */

  if (cr)
    cairo_restore (cr);
  return highest;
}
