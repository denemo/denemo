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


gint restwidths[SMALLESTDURATION + 1] =
  { WHOLEREST_WIDTH, HALFREST_WIDTH, QUARTERREST_WIDTH, EIGHTHREST_WIDTH,
  SIXTEENTHREST_WIDTH, THIRTYSECONDREST_WIDTH, SIXTYFOURTHREST_WIDTH, HUNDREDTWENTYEIGHTHREST_WIDTH, TWOHUNDREDFIFTYSIXTHREST_WIDTH
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
  static gint restoffsets[SMALLESTDURATION + 1] =
    { WHOLEREST_OFFSETFROMTOP, HALFREST_OFFSETFROMTOP,
    QUARTERREST_OFFSETFROMTOP, EIGHTHREST_OFFSETFROMTOP,
    SIXTEENTHREST_OFFSETFROMTOP, THIRTYSECONDREST_OFFSETFROMTOP,
    SIXTYFOURTHREST_OFFSETFROMTOP,
    HUNDREDTWENTYEIGHTHREST_OFFSETFROMTOP,
    TWOHUNDREDFIFTYSIXTHREST_OFFSETFROMTOP
  };

  static gunichar rest_char[SMALLESTDURATION + 1] =
    { 0x20, 0x21, 0x27, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2e
  };

  drawfetachar (pixmap, gc, rest_char[duration],
		     xx, y + restoffsets[duration]);
  /* Now draw any trailing dots and we're done */
  draw_dots (pixmap, gc, xx + restwidths[duration],
	     y + restoffsets[duration] , numdots);
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

  static gint headwidths[6] = { WHOLEHEAD_WIDTH, HALFHEAD_WIDTH,
    NOTEHEAD_WIDTH, DIAMOND_WIDTH,
    CROSS_WIDTH, HARMONIC_WIDTH,
  };

  static gunichar head_char[6] =
    { 0x54, 0x55, 0x56, 0x58, 0x64, 0x92
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
  if(!(get_override(thenote->directives)&DENEMO_OVERRIDE_GRAPHIC)) {
    drawfetachar (pixmap, gc, head_char[noteheadtype],
		       xx, y + height);
    /* Now draw any trailing dots */
    if ((height % LINE_SPACE) == 0)
      draw_dots (pixmap, gc, xx + headwidths[noteheadtype],
		 y + height - HALF_LINE_SPACE, numdots);
    else
      draw_dots (pixmap, gc, xx + headwidths[noteheadtype],
		 y + height, numdots);
  }
  
  /* any display for attached LilyPond */
 { GList *g = thenote->directives;
 GString *gstr=g_string_new("");
 gint count=10;
  for(;g;g=g->next, count+=10) {
    DenemoDirective *directive = (DenemoDirective *)g->data;
    if(directive->graphic) {
      gint width, height;
      gdk_drawable_get_size(GDK_DRAWABLE(directive->graphic), &width, &height);
      drawbitmapinverse (pixmap, gc, (GdkBitmap *)directive->graphic,
			 xx+directive->gx+count,  y+thenote->y+directive->gy/*thechord.highesty*/, width, height);

    }
    if(directive->display) {
      //      g_string_append(gstr, directive->display->str);
      PangoContext *context =
	gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
      PangoLayout *layout = pango_layout_new (context);
      PangoFontDescription *desc = pango_font_description_from_string (FONT);
      pango_layout_set_text (layout,
			     directive->display->str,
			     -1);
      pango_layout_set_font_description (layout, desc);
      gdk_draw_layout (pixmap, gc, xx+directive->tx+count, y+thenote->y+directive->ty, layout);
    }
  }
#if 0
  if(gstr->len) {
      PangoContext *context =
	gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
      PangoLayout *layout = pango_layout_new (context);
      PangoFontDescription *desc = pango_font_description_from_string (FONT);
      pango_layout_set_text (layout,
			     gstr->str,
			     -1);
      pango_layout_set_font_description (layout, desc);
      gdk_draw_layout (pixmap, gc, xx+20, y+thenote->y, layout);
      g_string_free(gstr, TRUE);
  }
#endif
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

  static gunichar upstem_char[SMALLESTDURATION + 1] =
    { 0, 0, 0, 0xb9, 0xba,
      0xbb,
      0xbc,
      0xbd,
      0xbd
  };

  static gunichar downstem_char[SMALLESTDURATION + 1] =
    { 0, 0, 0, 0xbe, 0xc1,
      0xc2,
      0xc3,
      0xc4,
      0xc4
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


#if 0
  if (thechord.is_highlighted)
    gc = gcs_bluegc ();
#endif
  if (mudelaitem->isinvisible)
    gc = selected?gcs_bluegc():gcs_yellowgc ();

  if (!thechord.notes/* a rest */) {
     if( !(get_override(thechord.directives)&DENEMO_OVERRIDE_GRAPHIC))
       draw_rest (pixmap, gc, duration, thechord.numdots, xx, y);  
  }  else {
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
	      drawfetachar (pixmap, gc, upstem_char[duration],
				 xx + NOTEHEAD_WIDTH,
				 thechord.highesty + y + 3
				 - (duration == 6 ? EXTRA_STEM_HEIGHT
				    : STEM_HEIGHT));
	  }
	else if (nextmuditem && !mudelaitem->isend_beamgroup)
	  {
	    /* Draw the thin beam across the gap */
	    gdk_draw_rectangle (pixmap, gc, TRUE,
				xx + headwidths[noteheadtype] - 1,
				y + thechord.stemy,
				nextmuditem->x - mudelaitem->x,
				THINBEAM_HEIGHT);
	    if (mudelaitem->isstart_beamgroup || !prevmuditem)
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
	    if (prevmuditem)
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
	    cairo_t *cr = gdk_cairo_create( pixmap );
	    if (nextmuditem)
	      arcwidth = nextmuditem->x - mudelaitem->x;
	    else
	      arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	    cairo_set_line_width( cr, 1.0 );
	    cairo_move_to( cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13 );
	    cairo_rel_curve_to( cr, arcwidth/3, -8, arcwidth*2/3, -8, arcwidth, 0 );
	    cairo_stroke( cr );
	    cairo_destroy( cr );
//	    gdk_draw_arc (pixmap, gc, FALSE,
//			  xx + headwidths[noteheadtype] / 2,
//			  y + thechord.lowesty + 3,
//			  arcwidth, 8, 64 * 180, 64 * 180);
	  }
      }			/* End stemup stuff */
    else
      {			/* chord is stemdown */
	if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
	  {
	    if (duration >= 3)
	      /* Down-pointing stem */
	      drawfetachar (pixmap, gc, downstem_char[duration],
				 xx + 1,
				 thechord.lowesty + y 
				 + (duration == 6 ? EXTRA_STEM_HEIGHT
				    : STEM_HEIGHT)
				 );
	  }
	else if ((nextmuditem) && !mudelaitem->isend_beamgroup)
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
	    if(prevmuditem)//sanity check
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
	    cairo_t *cr = gdk_cairo_create( pixmap );

	    if (nextmuditem)
	      arcwidth = nextmuditem->x - mudelaitem->x;
	    else
	      arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	    cairo_set_line_width( cr, 1.0 );
	    cairo_move_to( cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13 );
	    cairo_rel_curve_to( cr, arcwidth/3, -8, arcwidth*2/3, -8, arcwidth, 0 );
	    cairo_stroke( cr );
	    cairo_destroy( cr );

//	    gdk_draw_arc (pixmap, gc, FALSE,
//			  xx + headwidths[noteheadtype] / 2,
//			  y + thechord.highesty - 13,
//			  arcwidth, 8, 0, 64 * 180);
	  }
      }
    /* End stemdown stuff */
    
    draw_articulations (pixmap, gc, thechord, xx, y); 
    draw_ledgers (pixmap, gc, thechord.highesty, thechord.lowesty, xx, y,
		  headwidths[noteheadtype]);  
    
  }				/* end else if there are notes in the chord*/
  { GList *g = thechord.directives;
  gint count = 0;
  for(;g;g=g->next) {
    DenemoDirective *directive = (DenemoDirective *)g->data;
    if(directive->graphic) {
      gint width, height;
      gdk_drawable_get_size(GDK_DRAWABLE(directive->graphic), &width, &height);
      drawbitmapinverse (pixmap, gc, (GdkBitmap*)directive->graphic,
			 xx+directive->gx, y+directive->gy, width, height);
    }
    if(directive->display) {
      PangoContext *context =
	gdk_pango_context_get_for_screen (gdk_drawable_get_screen (pixmap));
      PangoLayout *layout = pango_layout_new (context);
      PangoFontDescription *desc = pango_font_description_from_string (FONT);
      pango_layout_set_text (layout,
			     directive->display->str,
			     -1);
      pango_layout_set_font_description (layout, desc);
      gdk_draw_layout (pixmap, gc, xx+directive->tx, y+STAFF_HEIGHT+40+count+directive->ty, layout);
      count += 16;
    }
  } //for each chord directive
  }//block displaying chord directives
}



/**
 * Draw the articulations on the score
 *
 */
void
draw_articulations (GdkPixmap * pixmap, GdkGC * gc,
		    chord thechord, gint xx, gint y)
{

  static gunichar options_char[25] = { 0x81, 0x82, 0x8a, 0x8a, 0x8c, 0x8f, 0x8d, 0x8e,
	                               0x92, 0x93, 0xa1, 0x9f, 0x94, 0xaf, 0xae, 0xac, 
				       0x98, 0xa0, 0x8a, 0x95, 0x89, 0xab, 0xa8, 0xb0,
				       0xa7 };

  static gunichar noteoptions_char[3] = { 0x9a, 0x99, 0xad };
  static gunichar strings_char[2] = { 0x97, 0x96 };
  static gunichar organ_char[4] = { 0x9b, 0x9c, 0x9d, 0x9e };

  /* extra y position for staccato, accent, tenuto, fermata when stacked */
  gint extra = 0;
  GList *tmp;

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
	drawfetachar (pixmap, gc, options_char[4],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TENUTO)
	drawfetachar (pixmap, gc,
			   options_char[5], xx,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) STACCATISSIMO)
	drawfetachar (pixmap, gc, options_char[6],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) D_ACCENT)
	drawfetachar (pixmap, gc, options_char[2],
			   xx + 2, y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) MARCATO)
	drawfetachar (pixmap, gc, options_char[8],
			   xx + 2, y + extra);


      /* 
       * Always should appear above the note(s) they
       * effect.
       */
      if (*(enum ornament *) tmp->data == (enum ornament) FERMATA)
	drawfetachar (pixmap, gc,
			   options_char[0], xx - FERMATA_WIDTH / 4,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) CODA)
	drawfetachar (pixmap, gc,
			   options_char[10], xx - CODA_WIDTH / 4,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL)
	drawfetachar (pixmap, gc,
			   noteoptions_char[0], xx - TRILL_WIDTH / 4,
			   y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) TURN)
	drawfetachar (pixmap, gc,
			   noteoptions_char[1], xx - TURN_WIDTH / 4,
			   y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) MORDENT)
	drawfetachar (pixmap, gc,
			   noteoptions_char[2], xx - MORDENT_WIDTH / 4,
			   y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) DBOW)
	drawfetachar (pixmap, gc,
			   strings_char[0], xx, y + extra);
      else if (*(enum ornament *) tmp->data == (enum ornament) UBOW)
	drawfetachar (pixmap, gc,
			   strings_char[1], xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) RHEEL)
	{
	  drawfetachar (pixmap, gc,
			     organ_char[1], xx, y + extra);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LHEEL)
	{
	  drawfetachar (pixmap, gc,
			     organ_char[0], xx, y + extra);
	}

      if (*(enum ornament *) tmp->data == (enum ornament) RTOE)
	{
	  drawfetachar (pixmap, gc,
			     organ_char[3], xx, y + extra);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LTOE)
	{
	  drawfetachar (pixmap, gc,
			     organ_char[2], xx, y + extra);
	}
      if (*(enum ornament *) tmp->data == (enum ornament) D_ARPEGGIO)
	drawfetachar (pixmap, gc,
			   options_char[24], xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) UPPRALL)
	drawfetachar (pixmap, gc, options_char[23],
			   xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) FLAGEOLET)
	drawfetachar (pixmap, gc, options_char[11],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) OPEN)
	drawfetachar (pixmap, gc, options_char[12],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLMORDENT)
	drawfetachar (pixmap, gc, options_char[13],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLPRALL)
	drawfetachar (pixmap, gc, options_char[14],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALL)
	drawfetachar (pixmap, gc, options_char[15],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) REVERSETURN)
	drawfetachar (pixmap, gc, options_char[16],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) SEGNO)
	drawfetachar (pixmap, gc, options_char[17],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) SFORZATO)
	drawfetachar (pixmap, gc, options_char[18],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) STOPPED)
	drawfetachar (pixmap, gc, options_char[19],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) THUMB)
	drawfetachar (pixmap, gc, options_char[20],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILLELEMENT)
	drawfetachar (pixmap, gc, options_char[21],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL_ELEMENT)
	drawfetachar (pixmap, gc, options_char[22],
			   xx, y + extra);

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
