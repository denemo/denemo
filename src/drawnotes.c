/* drawnotes.cpp
 *
 * Functions for drawing notes and rests
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999, 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

#include "utils.h"		/* Includes <gdk.h> */
#include "drawingprims.h"
#include "notewidths.h"
#include "gcs.h"

/* Include the bitmaps; they'll actually be made into GdkBitmaps with the
 * GDK create_from_xbm_d functions. Doing things this way circumvents the need
 * to install the pixmaps into a /usr/share directory of some kind before
 * the program can be run from anywhere on the system. */

#include "../pixmaps/feta26-noteheads-0.xbm"
#include "../pixmaps/feta26-noteheads-1.xbm"
#include "../pixmaps/feta26-noteheads-2.xbm"
#include "../pixmaps/feta26-noteheads-1diamond.xbm"
#include "../pixmaps/feta26-noteheads-2cross.xbm"
#include "../pixmaps/feta26-noteheads-2harmonic.xbm"
#include "../pixmaps/feta26-flags-u3.xbm"
#include "../pixmaps/feta26-flags-u4.xbm"
#include "../pixmaps/feta26-flags-u5.xbm"
#include "../pixmaps/feta26-flags-u6.xbm"
#include "../pixmaps/feta26-flags-d3.xbm"
#include "../pixmaps/feta26-flags-d4.xbm"
#include "../pixmaps/feta26-flags-d5.xbm"
#include "../pixmaps/feta26-flags-d6.xbm"
#include "../pixmaps/feta26-flags-ugrace.xbm"
#include "../pixmaps/feta26-flags-dgrace.xbm"
#include "../pixmaps/feta26-rests-0.xbm"
#include "../pixmaps/feta26-rests-1.xbm"
#include "../pixmaps/feta26-rests-2.xbm"
#include "../pixmaps/feta26-rests-3.xbm"
#include "../pixmaps/feta26-rests-4.xbm"
#include "../pixmaps/feta26-rests-5.xbm"
#include "../pixmaps/feta26-rests-6.xbm"

#include "../pixmaps/feta26-scripts-ufermata.xbm"
#include "../pixmaps/feta26-scripts-dfermata.xbm"
#include "../pixmaps/feta26-scripts-staccato.xbm"
#include "../pixmaps/feta26-scripts-umarcato.xbm"
#include "../pixmaps/feta26-scripts-dmarcato.xbm"
#include "../pixmaps/feta26-scripts-accent.xbm"
#include "../pixmaps/feta26-scripts-tenuto.xbm"
#include "../pixmaps/feta26-scripts-ustaccatissimo.xbm"
#include "../pixmaps/feta26-scripts-dstaccatissimo.xbm"
#include "../pixmaps/feta26-scripts-trill.xbm"
#include "../pixmaps/feta26-scripts-turn.xbm"
#include "../pixmaps/feta26-scripts-mordent.xbm"
#include "../pixmaps/feta26-scripts-downbow.xbm"
#include "../pixmaps/feta26-scripts-upbow.xbm"
#include "../pixmaps/feta26-scripts-upedalheel.xbm"
#include "../pixmaps/feta26-scripts-dpedalheel.xbm"
#include "../pixmaps/feta26-scripts-upedaltoe.xbm"
#include "../pixmaps/feta26-scripts-dpedaltoe.xbm"
#include "../pixmaps/feta26-scripts-coda.xbm"
#include "../pixmaps/feta26-scripts-flageolet.xbm"
#include "../pixmaps/feta26-scripts-open.xbm"
#include "../pixmaps/feta26-scripts-prallmordent.xbm"
#include "../pixmaps/feta26-scripts-prallprall.xbm"
#include "../pixmaps/feta26-scripts-prall.xbm"
#include "../pixmaps/feta26-scripts-reverseturn.xbm"
#include "../pixmaps/feta26-scripts-segno.xbm"
#include "../pixmaps/feta26-scripts-sforzato.xbm"
#include "../pixmaps/feta26-scripts-stopped.xbm"
#include "../pixmaps/feta26-scripts-thumb.xbm"
#include "../pixmaps/feta26-scripts-trilelement.xbm"
#include "../pixmaps/feta26-scripts-trill-element.xbm"
#include "../pixmaps/feta26-scripts-upprall.xbm"
#include "../pixmaps/feta26-scripts-arpeggio.xbm"

gint restwidths[SMALLESTDURATION + 1] =
  { WHOLEREST_WIDTH, HALFREST_WIDTH, QUARTERREST_WIDTH, EIGHTHREST_WIDTH,
  SIXTEENTHREST_WIDTH, THIRTYSECONDREST_WIDTH, SIXTYFOURTHREST_WIDTH
};
gint headwidths[3] = { WHOLEHEAD_WIDTH, HALFHEAD_WIDTH, NOTEHEAD_WIDTH
};

/**
 * draw_dots
 * This draws dots after rests or notes 
 *
 */
static void
draw_dots (GdkPixmap * pixmap, GdkGC * gc,
	   gint xstart, gint ystart, gint numdots)
{
  gint thinrecty = ystart - 2, shortrecty = ystart - 1;

  xstart += 2;
  for (; numdots; numdots--, xstart += 6)
    {
      /* Thin rectangle */
      gdk_draw_rectangle (pixmap, gc, TRUE, xstart + 1, thinrecty, 2, 4);
      /* Short rectangle */
      gdk_draw_rectangle (pixmap, gc, TRUE, xstart, shortrecty, 4, 2);
    }
}

/**
 * draw_rest
 * This function actually draws a rest onto the backing pixmap 
 *
 */
void
draw_rest (GdkPixmap * pixmap, GdkGC * gc,
	   gint duration, gint numdots, gint xx, gint y)
{
  static GdkBitmap *rests[SMALLESTDURATION + 1] = { NULL, NULL, NULL, NULL,
    NULL, NULL, NULL
  };
  static gint restheights[SMALLESTDURATION + 1] =
    { WHOLEREST_HEIGHT, HALFREST_HEIGHT, QUARTERREST_HEIGHT,
    EIGHTHREST_HEIGHT,
    SIXTEENTHREST_HEIGHT, THIRTYSECONDREST_HEIGHT, SIXTYFOURTHREST_HEIGHT
  };
  static gint restoffsets[SMALLESTDURATION + 1] =
    { WHOLEREST_OFFSETFROMTOP, HALFREST_OFFSETFROMTOP,
    QUARTERREST_OFFSETFROMTOP, EIGHTHREST_OFFSETFROMTOP,
    SIXTEENTHREST_OFFSETFROMTOP, THIRTYSECONDREST_OFFSETFROMTOP,
    SIXTYFOURTHREST_OFFSETFROMTOP
  };

  if (!rests[0])
    {
      rests[0] = bitmaphelper (NULL, feta26_rests_0);
      rests[1] = bitmaphelper (NULL, feta26_rests_1);
      rests[2] = bitmaphelper (NULL, feta26_rests_2);
      rests[3] = bitmaphelper (NULL, feta26_rests_3);
      rests[4] = bitmaphelper (NULL, feta26_rests_4);
      rests[5] = bitmaphelper (NULL, feta26_rests_5);
      rests[6] = bitmaphelper (NULL, feta26_rests_6);
    }
  drawbitmapinverse (pixmap, gc, rests[duration],
		     xx, y + restoffsets[duration],
		     restwidths[duration], restheights[duration]);
  /* Now draw any trailing dots and we're done */
  draw_dots (pixmap, gc, xx + restwidths[duration],
	     y + restoffsets[duration] + restheights[duration] / 2, numdots);
}

/**
 *  draw_notehead
 *  This function actually draws the note onto the backing pixmap 
 *
 */
void
draw_notehead (GdkPixmap * pixmap, GdkGC * gc,
	       note * thenote, gint duration, gint numdots,
	       gint xx, gint y, gint * accs, gint is_stemup)
{
  /* Adam's changed this code; it used to be that these arrays only had
     three elements.  The change has defeated what had been semi-elegance;
     grrr.  */

  static GdkBitmap *heads[6] = { NULL, NULL, NULL, NULL, NULL, NULL };
  static gint headwidths[6] = { WHOLEHEAD_WIDTH, HALFHEAD_WIDTH,
    NOTEHEAD_WIDTH, DIAMOND_WIDTH,
    CROSS_WIDTH, HARMONIC_WIDTH,
  };
  static gint headheights[6] = { WHOLEHEAD_HEIGHT, HALFHEAD_HEIGHT,
    NOTEHEAD_HEIGHT, DIAMOND_HEIGHT,
    CROSS_HEIGHT, HARMONIC_HEIGHT
  };
  static gint headsemiheights[6] =
    { WHOLEHEAD_SEMI_HEIGHT, HALFHEAD_SEMI_HEIGHT, NOTEHEAD_SEMI_HEIGHT,
    DIAMOND_SEMI_HEIGHT, CROSS_SEMI_HEIGHT, HARMONIC_SEMI_HEIGHT
  };
  gint height = thenote->y;
  gint noteheadtype = 1;
  gint enshift = thenote->enshift;
  gint pitch = offsettonumber (thenote->mid_c_offset);

  if (thenote->noteheadtype == DENEMO_NORMAL_NOTEHEAD)
    {
      noteheadtype = MIN (duration, 2);	/* Index of relevant notehead */
    }
  else if (thenote->noteheadtype == DENEMO_CROSS_NOTEHEAD)
    {
      noteheadtype = 4;		/* Index of relevant notehead */
    }
  else if (thenote->noteheadtype == DENEMO_HARMONIC_NOTEHEAD)
    {
      noteheadtype = 5;
    }
  else if (thenote->noteheadtype == DENEMO_DIAMOND_NOTEHEAD)
    {
      noteheadtype = 3;
    }

  if (!heads[0])
    {
      heads[0] = bitmaphelper (NULL, feta26_noteheads_0);
      heads[1] = bitmaphelper (NULL, feta26_noteheads_1);
      heads[2] = bitmaphelper (NULL, feta26_noteheads_2);
      heads[3] = bitmaphelper (NULL, feta26_noteheads_1diamond);
      heads[4] = bitmaphelper (NULL, feta26_noteheads_2cross);
      heads[5] = bitmaphelper (NULL, feta26_noteheads_2harmonic);
    }

  /* Draw the accidental, if necessary.  Note that this has to be
     done before xx is modified, as the the value in
     position_of_accidental already accounts for any reverse-alignment
     in the chord.  */
  if (thenote->showaccidental)
    {
      draw_accidental (pixmap, gc, xx - thenote->position_of_accidental,
		       y + height, enshift);
      accs[pitch] = enshift;
    }

  if (thenote->reversealign)
    if (is_stemup)
      xx += headwidths[noteheadtype];
    else
      xx -= headwidths[noteheadtype];


  drawbitmapinverse (pixmap, gc, heads[noteheadtype],
		     xx, y + height - headsemiheights[noteheadtype],
		     headwidths[noteheadtype], headheights[noteheadtype]);

  /* Now draw any trailing dots */
  if ((height % LINE_SPACE) == 0)
    draw_dots (pixmap, gc, xx + headwidths[noteheadtype],
	       y + height - HALF_LINE_SPACE, numdots);
  else
    draw_dots (pixmap, gc, xx + headwidths[noteheadtype],
	       y + height, numdots);

  /* any display for attached LilyPond */
  if(thenote->display){
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
    PangoLayout *layout = pango_layout_new (context);
    PangoFontDescription *desc = pango_font_description_from_string (FONT);
        pango_layout_set_text (layout,
			   thenote->display->str,
			   -1);
    pango_layout_set_font_description (layout, desc);
    gdk_draw_layout (pixmap, gc, xx, y+STAFF_HEIGHT+20, layout);
  }

}

/**
 * Draw the legder lines on the score
 *
 */
void
draw_ledgers (GdkPixmap * pixmap, GdkGC * gc,
	      gint greaterheight, gint lesserheight,
	      gint xx, gint y, gint width)
{
  int ledgerheight;

#define EXTRA_ON_LEDGER 3

  /* Draw the top ledger lines */
  for (ledgerheight = -LINE_SPACE; ledgerheight >= greaterheight;
       ledgerheight -= LINE_SPACE)
    gdk_draw_line (pixmap, gc, xx - EXTRA_ON_LEDGER, ledgerheight + y,
		   xx + width + EXTRA_ON_LEDGER, ledgerheight + y);

  /* Almost identically, draw the bottom ones */
  for (ledgerheight = STAFF_HEIGHT + LINE_SPACE;
       ledgerheight <= lesserheight; ledgerheight += LINE_SPACE)
    gdk_draw_line (pixmap, gc, xx - 2, ledgerheight + y, xx + width + 2,
		   ledgerheight + y);
}

/**
 * Draw the chord object on the score
 *
 */
void
draw_chord (GdkPixmap * pixmap, GdkGC * gc, objnode * curobj, gint xx, gint y,
	    gint mwidth, gint * accs, gboolean selected)
{

  static GdkBitmap *upstems[SMALLESTDURATION + 1] =
    { NULL, NULL, NULL, NULL, NULL, NULL, NULL };
  static GdkBitmap *downstems[SMALLESTDURATION + 1];
  static GdkBitmap *graceflags[2];
  static gint stemheights[SMALLESTDURATION + 1] =
    { 0, 0, 0, EIGHTHSTEM_HEIGHT, SIXTEENTHSTEM_HEIGHT,
    THIRTYSECONDSTEM_HEIGHT,
    SIXTYFOURTHSTEM_HEIGHT
  };



  DenemoObject *prevmuditem =
    (DenemoObject *) (curobj->prev ? curobj->prev->data : NULL);
  DenemoObject *mudelaitem = (DenemoObject *) curobj->data;
  DenemoObject *nextmuditem =
    (DenemoObject *) (curobj->next ? curobj->next->data : NULL);
  chord thechord = *(chord *) mudelaitem->object;
  gint duration = thechord.baseduration;
  gint noteheadtype = MIN (duration, 2);

  /* Change those two so that they're cached instead */
  gint i;
  gint beampainty, arcwidth;

  gint prevbaseduration, nextbaseduration;
  GList *curnode;


  if (!upstems[3])
    {
      upstems[3] = bitmaphelper (NULL, feta26_flags_u3);
      upstems[4] = bitmaphelper (NULL, feta26_flags_u4);
      upstems[5] = bitmaphelper (NULL, feta26_flags_u5);
      upstems[6] = bitmaphelper (NULL, feta26_flags_u6);
      downstems[3] = bitmaphelper (NULL, feta26_flags_d3);
      downstems[4] = bitmaphelper (NULL, feta26_flags_d4);
      downstems[5] = bitmaphelper (NULL, feta26_flags_d5);
      downstems[6] = bitmaphelper (NULL, feta26_flags_d6);
      graceflags[0] = bitmaphelper (NULL, feta26_flags_ugrace);
      graceflags[1] = bitmaphelper (NULL, feta26_flags_dgrace);

    }

#if 0
  if (thechord.is_highlighted)
    gc = gcs_bluegc ();
#endif
  if (mudelaitem->isinvisible)
    gc = selected?gcs_bluegc():gcs_yellowgc ();

  if (!thechord.notes)		/* We have a rest here */
    draw_rest (pixmap, gc, duration, thechord.numdots, xx, y);
  else
    {
      /* Draw the noteheads and accidentals */

      for (curnode = thechord.notes; curnode; curnode = curnode->next)
	draw_notehead (pixmap, gc, (note *) curnode->data, duration,
		       thechord.numdots, xx, y, accs, thechord.is_stemup);

      /* Now the stem and beams. This is complicated. */

      if (thechord.is_stemup)
	{
	  if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
	    {
	      if (duration >= 3)
		/* Up-pointing stem pixmap */
		drawbitmapinverse (pixmap, gc, upstems[duration],
				   xx + NOTEHEAD_WIDTH - 1,
				   thechord.highesty + y
				   - (duration == 6 ? EXTRA_STEM_HEIGHT
				      : STEM_HEIGHT),
				   STEM_WIDTH, stemheights[duration]);
	    }
	  else if (!mudelaitem->isend_beamgroup)
	    {
	      /* Draw the thin beam across the gap */
	      gdk_draw_rectangle (pixmap, gc, TRUE,
				  xx + headwidths[noteheadtype] - 1,
				  y + thechord.stemy,
				  nextmuditem->x - mudelaitem->x,
				  THINBEAM_HEIGHT);
	      if (mudelaitem->isstart_beamgroup)
		prevbaseduration = 0;
	      else
		prevbaseduration =
		  ((chord *) prevmuditem->object)->baseduration;
	      nextbaseduration =
		((chord *) nextmuditem->object)->baseduration;
	      for (i = 4, beampainty = thechord.stemy + FIRSTBEAMSPACE;
		   i <= thechord.baseduration;
		   i++, beampainty += SUBSQBEAMSPACE)
		{
		  if (nextbaseduration >= i)
		    /* Draw a thick beam across the gap */
		    gdk_draw_rectangle (pixmap, gc, TRUE,
					xx + headwidths[noteheadtype] - 1,
					y + beampainty,
					nextmuditem->x - mudelaitem->x,
					THICKBEAM_HEIGHT);
		  else if (prevbaseduration < i)
		    /* Draw a stub to the right of the staff */
		    gdk_draw_rectangle (pixmap, gc, TRUE,
					xx + headwidths[noteheadtype] - 1,
					y + beampainty, STUB_WIDTH,
					THICKBEAM_HEIGHT);
		}		/* end for loop */
	    }			/* end drawing for non-end-beamgroup notes */
	  else
	    {			/* We're at the end of a beamgroup */
	      for (i = MAX (((chord *) prevmuditem->object)->baseduration + 1,
			    4),
		   beampainty = thechord.stemy + FIRSTBEAMSPACE +
		   (SUBSQBEAMSPACE * (i - 4));
		   i <= thechord.baseduration;
		   i++, beampainty += SUBSQBEAMSPACE)
		{
		  /* Draw a stub to the left of the staff */
		  gdk_draw_rectangle (pixmap, gc, TRUE,
				      xx + headwidths[noteheadtype] - 1 -
				      STUB_WIDTH, y + beampainty, STUB_WIDTH,
				      THICKBEAM_HEIGHT);
		}
	    }

	  if (duration > 0)
	    /* Vertical line */
	    gdk_draw_line (pixmap, gc, xx + headwidths[noteheadtype] - 1,
			   thechord.stemy + y,
			   xx + headwidths[noteheadtype] - 1,
			   thechord.lowesty + y);

	  /* Now draw the tie, if appropriate */
	  if (thechord.is_tied)
	    {
	      if (nextmuditem)
		arcwidth = nextmuditem->x - mudelaitem->x;
	      else
		arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	      gdk_draw_arc (pixmap, gc, FALSE,
			    xx + headwidths[noteheadtype] / 2,
			    y + thechord.lowesty + 3,
			    arcwidth, 8, 64 * 180, 64 * 180);
	    }
	}			/* End stemup stuff */
      else
	{			/* chord is stemdown */
	  if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
	    {
	      if (duration >= 3)
		/* Down-pointing stem */
		drawbitmapinverse (pixmap, gc, downstems[duration],
				   xx,
				   thechord.lowesty + y
				   + (duration == 6 ? EXTRA_STEM_HEIGHT
				      : STEM_HEIGHT)
				   - stemheights[duration],
				   STEM_WIDTH, stemheights[duration]);
	    }
	  else if (!mudelaitem->isend_beamgroup)
	    {
	      /* Draw the thin beam across the gap */
	      gdk_draw_rectangle (pixmap, gc, TRUE, xx,
				  y + thechord.stemy - THINBEAM_HEIGHT + 1,
				  nextmuditem->x - mudelaitem->x,
				  THINBEAM_HEIGHT);
	      if (mudelaitem->isstart_beamgroup)
		prevbaseduration = 0;
	      else
		prevbaseduration =
		  ((chord *) prevmuditem->object)->baseduration;

	      nextbaseduration =
		((chord *) nextmuditem->object)->baseduration;
	      for (i = 4, beampainty =
		   thechord.stemy - FIRSTBEAMSPACE - THICKBEAM_HEIGHT + 1;
		   i <= thechord.baseduration;
		   i++, beampainty -= SUBSQBEAMSPACE)
		{
		  if (nextbaseduration >= i)
		    /* Draw a thick beam across the gap */
		    gdk_draw_rectangle (pixmap, gc, TRUE, xx,
					y + beampainty,
					nextmuditem->x - mudelaitem->x,
					THICKBEAM_HEIGHT);
		  else if (prevbaseduration < i)
		    /* Draw a stub to the right of the staff */
		    gdk_draw_rectangle (pixmap, gc, TRUE, xx, y + beampainty,
					STUB_WIDTH, THICKBEAM_HEIGHT);
		}		/* End for loop */
	    }			/* End drawing for non-end-beamgroup notes */
	  else
	    {			/* We're at the end of a beamgroup */
	      for (i = MAX (((chord *) prevmuditem->object)->baseduration + 1,
			    4),
		   beampainty = thechord.stemy - FIRSTBEAMSPACE -
		   THICKBEAM_HEIGHT + 1 -
		   (SUBSQBEAMSPACE * (i - 4));
		   i <= thechord.baseduration;
		   i++, beampainty += SUBSQBEAMSPACE)
		{
		  /* Draw a stub to the left of the staff */
		  gdk_draw_rectangle (pixmap, gc, TRUE, xx - STUB_WIDTH,
				      y + beampainty, STUB_WIDTH,
				      THICKBEAM_HEIGHT);
		}
	    }

	  if (duration > 0)
	    /* Vertical line */
	    gdk_draw_line (pixmap, gc, xx, thechord.highesty + y, xx,
			   thechord.stemy + y);
	  /* Now draw the tie, if appropriate */
	  if (thechord.is_tied)
	    {
	      if (nextmuditem)
		arcwidth = nextmuditem->x - mudelaitem->x;
	      else
		arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	      gdk_draw_arc (pixmap, gc, FALSE,
			    xx + headwidths[noteheadtype] / 2,
			    y + thechord.highesty - 13,
			    arcwidth, 8, 0, 64 * 180);
	    }
	}
      /* End stemdown stuff */

      draw_articulations (pixmap, gc, thechord, xx, y);

/*#ifdef DEBUG
      g_print ("(lower height) lowest y %d, (greater height) highest y %d\n", thechord.lowesty,
	       thechord.highesty);
#endif*/

      draw_ledgers (pixmap, gc, thechord.highesty, thechord.lowesty, xx, y,
		    headwidths[noteheadtype]);


      if(thechord.display) {
	PangoContext *context =
	  gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
	PangoLayout *layout = pango_layout_new (context);
	PangoFontDescription *desc = pango_font_description_from_string (FONT);
        pango_layout_set_text (layout,
			       thechord.display->str,
			       -1);
	pango_layout_set_font_description (layout, desc);
	gdk_draw_layout (pixmap, gc, xx, y+STAFF_HEIGHT+40, layout);
      }

      
	

    }				/* end else */

}



/**
 * Draw the articulations on the score
 *
 */
void
draw_articulations (GdkPixmap * pixmap, GdkGC * gc,
		    chord thechord, gint xx, gint y)
{
  static GdkBitmap *options[25];
  static GdkBitmap *noteoptions[3];
  static GdkBitmap *strings[2];
  static GdkBitmap *organ[4];
  /* extra y position for staccato, accent, tenuto, fermata when stacked */
  gint extra = 0;
  GList *tmp;

  if (!options[0])
    {
      noteoptions[0] = bitmaphelper (NULL, feta26_scripts_trill);
      noteoptions[1] = bitmaphelper (NULL, feta26_scripts_turn);
      noteoptions[2] = bitmaphelper (NULL, feta26_scripts_mordent);
      options[0] = bitmaphelper (NULL, feta26_scripts_ufermata);
      options[1] = bitmaphelper (NULL, feta26_scripts_dfermata);
      options[2] = bitmaphelper (NULL, feta26_scripts_accent);
      options[3] = bitmaphelper (NULL, feta26_scripts_accent);
      options[4] = bitmaphelper (NULL, feta26_scripts_staccato);
      options[5] = bitmaphelper (NULL, feta26_scripts_tenuto);
      options[6] = bitmaphelper (NULL, feta26_scripts_ustaccatissimo);
      options[7] = bitmaphelper (NULL, feta26_scripts_dstaccatissimo);
      options[8] = bitmaphelper (NULL, feta26_scripts_umarcato);
      options[9] = bitmaphelper (NULL, feta26_scripts_dmarcato);
      options[10] = bitmaphelper (NULL, feta26_scripts_coda);
      options[11] = bitmaphelper (NULL, feta26_scripts_flageolet);
      options[12] = bitmaphelper (NULL, feta26_scripts_open);
      options[13] = bitmaphelper (NULL, feta26_scripts_prallmordent);
      options[14] = bitmaphelper (NULL, feta26_scripts_prallprall);
      options[15] = bitmaphelper (NULL, feta26_scripts_prall);
      options[16] = bitmaphelper (NULL, feta26_scripts_reverseturn);
      options[17] = bitmaphelper (NULL, feta26_scripts_segno);
      options[18] = bitmaphelper (NULL, feta26_scripts_sforzato);
      options[19] = bitmaphelper (NULL, feta26_scripts_stopped);
      options[20] = bitmaphelper (NULL, feta26_scripts_thumb);
      options[21] = bitmaphelper (NULL, feta26_scripts_trilelement);
      options[22] = bitmaphelper (NULL, feta26_scripts_trill_element);
      options[23] = bitmaphelper (NULL, feta26_scripts_upprall);
      options[24] = bitmaphelper (NULL, feta26_scripts_arpeggio);
      strings[0] = bitmaphelper (NULL, feta26_scripts_downbow);
      strings[1] = bitmaphelper (NULL, feta26_scripts_upbow);
      organ[0] = bitmaphelper (NULL, feta26_scripts_upedalheel);
      organ[1] = bitmaphelper (NULL, feta26_scripts_dpedalheel);
      organ[2] = bitmaphelper (NULL, feta26_scripts_upedaltoe);
      organ[3] = bitmaphelper (NULL, feta26_scripts_dpedaltoe);
    }
  /* Fermata, Accents, Staccato, tenuto stuff.  */

  /* The following code ought to see some serious reorganization.
     (It's another chunk of code that uses arrays in a situation
     where arrays aren't elegant -- rather, they're just confusing.
     And how 'bout some nesting of these if statements?)  */

  /* Calculate the offset required when stacking symbols */
  extra = calc_offset (thechord, thechord.is_stemup);

  /* These rely on notehead position and stem-ends 
   * Usually are above the stave except tenuto and 
   * staccato
   */
  for (tmp = thechord.ornamentlist; tmp; tmp = tmp->next)
    {
#ifdef DEBUG
      g_print ("ornament %d\n", *(enum ornament *) (tmp->data));
#endif
      if (*(enum ornament *) tmp->data == (enum ornament) STACCATO)
	drawbitmapinverse (pixmap, gc, options[4],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra, STACATTO, STACATTO);
      if (*(enum ornament *) tmp->data == (enum ornament) TENUTO)
	drawbitmapinverse (pixmap, gc,
			   options[5], xx,
			   y + extra, TENUTO_WIDTH, TENUTO_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) STACCATISSIMO)
	drawbitmapinverse (pixmap, gc, options[6],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra, STACCATISSIMO_WIDTH,
			   STACCATISSIMO_HEIGHT);

      if (*(enum ornament *) tmp->data == (enum ornament) D_ACCENT)
	drawbitmapinverse (pixmap, gc, options[2],
			   xx + 2, y + extra, ACCENT_WIDTH, ACCENT_HEIGHT);

      else if (*(enum ornament *) tmp->data == (enum ornament) MARCATO)
	drawbitmapinverse (pixmap, gc, options[8],
			   xx + 2, y + extra, MARCATO_WIDTH, MARCATO_HEIGHT);


      /* 
       * Always should appear above the note(s) they
       * effect.
       */
      if (*(enum ornament *) tmp->data == (enum ornament) FERMATA)
	drawbitmapinverse (pixmap, gc,
			   options[0], xx - FERMATA_WIDTH / 4,
			   y + extra, FERMATA_WIDTH, FERMATA_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) CODA)
	drawbitmapinverse (pixmap, gc,
			   options[10], xx - CODA_WIDTH / 4,
			   y + extra, CODA_WIDTH, CODA_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL)
	drawbitmapinverse (pixmap, gc,
			   noteoptions[0], xx - TRILL_WIDTH / 4,
			   y + extra, TRILL_WIDTH, TRILL_HEIGHT);

      else if (*(enum ornament *) tmp->data == (enum ornament) TURN)
	drawbitmapinverse (pixmap, gc,
			   noteoptions[1], xx - TURN_WIDTH / 4,
			   y + extra, TURN_WIDTH, TURN_HEIGHT);

      else if (*(enum ornament *) tmp->data == (enum ornament) MORDENT)
	drawbitmapinverse (pixmap, gc,
			   noteoptions[2], xx - MORDENT_WIDTH / 4,
			   y + extra, MORDENT_WIDTH, MORDENT_HEIGHT);

      if (*(enum ornament *) tmp->data == (enum ornament) DBOW)
	drawbitmapinverse (pixmap, gc,
			   strings[0], xx, y + extra, DBOW_WIDTH,
			   DBOW_HEIGHT);
      else if (*(enum ornament *) tmp->data == (enum ornament) UBOW)
	drawbitmapinverse (pixmap, gc,
			   strings[1], xx, y + extra, UBOW_WIDTH,
			   UBOW_HEIGHT);

      if (*(enum ornament *) tmp->data == (enum ornament) RHEEL)
	{
	  drawbitmapinverse (pixmap, gc,
			     organ[1], xx, y + extra, HEEL_WIDTH,
			     HEEL_HEIGHT);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LHEEL)
	{
	  drawbitmapinverse (pixmap, gc,
			     organ[0], xx, y + extra, HEEL_WIDTH,
			     HEEL_HEIGHT);
	}

      if (*(enum ornament *) tmp->data == (enum ornament) RTOE)
	{
	  drawbitmapinverse (pixmap, gc,
			     organ[3], xx, y + extra, TOE_WIDTH, TOE_HEIGHT);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LTOE)
	{
	  drawbitmapinverse (pixmap, gc,
			     organ[2], xx, y + extra, TOE_WIDTH, TOE_HEIGHT);
	}
      if (*(enum ornament *) tmp->data == (enum ornament) D_ARPEGGIO)
	drawbitmapinverse (pixmap, gc,
			   options[24], xx, y + extra, ARPEGGIO_WIDTH,
			   ARPEGGIO_HEIGHT);

      if (*(enum ornament *) tmp->data == (enum ornament) UPPRALL)
	drawbitmapinverse (pixmap, gc, options[23],
			   xx, y + extra, UPPRALL_WIDTH, UPPRALL_HEIGHT);

      if (*(enum ornament *) tmp->data == (enum ornament) FLAGEOLET)
	drawbitmapinverse (pixmap, gc, options[11],
			   xx, y + extra, FLAGEOLET_SIZE, FLAGEOLET_SIZE);
      if (*(enum ornament *) tmp->data == (enum ornament) OPEN)
	drawbitmapinverse (pixmap, gc, options[12],
			   xx, y + extra, OPEN_WIDTH, OPEN_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLMORDENT)
	drawbitmapinverse (pixmap, gc, options[13],
			   xx, y + extra, PRALLMORDENT_WIDTH,
			   PRALLMORDENT_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLPRALL)
	drawbitmapinverse (pixmap, gc, options[14],
			   xx, y + extra, PRALLPRALL_WIDTH,
			   PRALLPRALL_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALL)
	drawbitmapinverse (pixmap, gc, options[15],
			   xx, y + extra, PRALL_WIDTH, PRALL_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) REVERSETURN)
	drawbitmapinverse (pixmap, gc, options[16],
			   xx, y + extra, REVERSETURN_WIDTH,
			   REVERSETURN_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) SEGNO)
	drawbitmapinverse (pixmap, gc, options[17],
			   xx, y + extra, SEGNO_WIDTH, SEGNO_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) SFORZATO)
	drawbitmapinverse (pixmap, gc, options[18],
			   xx, y + extra, SFORZATO_WIDTH, SFORZATO_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) STOPPED)
	drawbitmapinverse (pixmap, gc, options[19],
			   xx, y + extra, STOPPED_SIZE, STOPPED_SIZE);
      if (*(enum ornament *) tmp->data == (enum ornament) THUMB)
	drawbitmapinverse (pixmap, gc, options[20],
			   xx, y + extra, THUMB_WIDTH, THUMB_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILLELEMENT)
	drawbitmapinverse (pixmap, gc, options[21],
			   xx, y + extra, TRILLELEMENT_WIDTH,
			   TRILLELEMENT_HEIGHT);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL_ELEMENT)
	drawbitmapinverse (pixmap, gc, options[22],
			   xx, y + extra, TRILL_ELEMENT_WIDTH,
			   TRILL_ELEMENT_HEIGHT);

    }				//end for loop

}

/** 
 * Calculate the offset required for the given ornament
 *
 */
gint
calc_offset (chord thechord, gint stemdir)
{
  gint offset = 0;
  GList *tmp;

  for (tmp = thechord.ornamentlist; tmp; tmp = tmp->next)
    {
      if (*(enum ornament *) tmp->data == (enum ornament) FERMATA ||
	  *(enum ornament *) tmp->data == (enum ornament) TURN ||
	  *(enum ornament *) tmp->data == (enum ornament) TRILL ||
	  *(enum ornament *) tmp->data == (enum ornament) MORDENT)
	offset = -LINE_SPACE - 7;
      else if (*(enum ornament *) tmp->data == (enum ornament) STACCATO ||
	       *(enum ornament *) tmp->data == (enum ornament) TENUTO
	       && stemdir)
	offset = thechord.lowesty + 10;
      else if (*(enum ornament *) tmp->data == (enum ornament) STACCATO ||
	       *(enum ornament *) tmp->data == (enum ornament) TENUTO)
	offset = thechord.highesty - 10;
      else if (*(enum ornament *) tmp->data == (enum ornament) D_ACCENT
	       && stemdir)
	offset = thechord.lowesty + 10;
      else if (*(enum ornament *) tmp->data == (enum ornament) MARCATO ||
	       *(enum ornament *) tmp->data == (enum ornament) D_ACCENT)
	offset = -thechord.highesty - 6;
    }

  return offset;
}
