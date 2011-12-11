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
#include <math.h>

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
draw_dots (cairo_t *cr,
	   gint xstart, gint ystart, gint numdots)
{
  gint thinrecty = ystart - 2, shortrecty = ystart - 1;

  xstart += 4;
  for (; numdots; numdots--, xstart += 6)
    {
      cairo_arc( cr, xstart, ystart, 1.5, 0.0, 2*M_PI );
      cairo_fill( cr );
    }
}

/**
 * draw_rest
 * This function actually draws a rest onto the backing pixmap 
 *
 */
static void
draw_rest (cairo_t *cr,
	   gint duration, gint numdots, gint xx, gint y, DenemoGraphic *override_rest, gint gx, gint gy)
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
    { 0x20, 0x21, 0x27, 0x29, 0x2a, 0x2b, 0x2c, 0x2d, 0x2d/*note no glyph for this one! */ 
  };
  if(override_rest)
    drawbitmapinverse_cr ( cr, override_rest,
			       xx+gx+ restwidths[0]-override_rest->width/2,  y+restoffsets[0]+gy-override_rest->height/2, FALSE);
  else
    drawfetachar_cr (cr, rest_char[duration],
		     xx, y + restoffsets[duration]);
  /* Now draw any trailing dots and we're done */

  draw_dots (cr, xx + restwidths[duration],
	     y + restoffsets[duration]  - HALF_LINE_SPACE, numdots);
}

/**
 *  draw_notehead
 *  This function actually draws the note onto the backing pixmap 
 *
 */
static void
draw_notehead (cairo_t *cr,
	       note * thenote, gint duration, gint numdots,
	       gint xx, gint y, gint * accs, gint is_stemup, gboolean invisible, DenemoGraphic *override_notehead, gint gx, gint gy)
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
  if(duration<0) noteheadtype = 0;

  /* Draw the accidental, if necessary.  Note that this has to be
     done before xx is modified, as the the value in
     position_of_accidental already accounts for any reverse-alignment
     in the chord.  */
  if (thenote->showaccidental)
    {
      if(cr)draw_accidental (cr, xx - thenote->position_of_accidental,
		       y + height, enshift);
      accs[pitch] = enshift;
    }

  if (thenote->reversealign) {
    if (is_stemup)
      xx += headwidths[noteheadtype];
    else
      xx -= headwidths[noteheadtype];
  }
  //OVERRIDE_GRAPHIC on the note directive just affects the head, everything else -  accidentals dots stems beams ...are controlled by chord/chord-directives. Even the notehead is set by the chord directives if override_notehead is set
  if(cr)if(!(get_override(thenote->directives)&DENEMO_OVERRIDE_GRAPHIC)) {
    if(override_notehead) {
      //g_print("drawing a chord override graphic at %d %d\n",  xx+gx-override_notehead->width/2,  y+height+gy-override_notehead->height/2);
      drawbitmapinverse_cr ( cr, override_notehead,
			       xx+gx-override_notehead->width/2,  y+height+gy-override_notehead->height/2, FALSE);
    }
    else {
    if (is_stemup)
      drawfetachar_cr ( cr, head_char[noteheadtype], xx, y + height);
    else
      drawfetachar_cr ( cr, head_char[noteheadtype], xx-0.5, y + height);
    }
  }
  
  gint maxwidth = headwidths[noteheadtype];
  
  /* any display for note directives */
  if(cr)
    maxwidth = MAX(draw_for_directives(cr, thenote->directives, xx, y+thenote->y), maxwidth);
  if(cr){
    /* Now draw any trailing dots */
    if ((height % LINE_SPACE) == 0)
      draw_dots (cr, xx + maxwidth,
		 y + height - HALF_LINE_SPACE, numdots);
    else
      draw_dots (cr, xx + maxwidth,
		 y + height, numdots);
  }
}

/**
 * Draw the legder lines on the score
 * Modified RTS 2011, ledger lines must not coalesce, must project from noteheads and stems
 * there is not much room, hence the sleight of hand
 */
void
draw_ledgers (cairo_t *cr,
	      gint greaterheight, gint lesserheight,
	      gint xx, gint y, gint width)
{
  int ledgerheight;

#define EXTRA_ON_LEDGER 1.5

  cairo_set_line_width( cr, 1.0 );
  /* Draw the top ledger lines */
  for (ledgerheight = -LINE_SPACE; ledgerheight >= greaterheight;
       ledgerheight -= LINE_SPACE)
  {
    cairo_move_to( cr, xx + ((ledgerheight==greaterheight)? (-EXTRA_ON_LEDGER): (-2)), ledgerheight + y );
    cairo_line_to( cr, xx + width + ((ledgerheight==greaterheight)? EXTRA_ON_LEDGER: (-2)), ledgerheight + y );
  }

  /* Almost identically, draw the bottom ones */
  for (ledgerheight = STAFF_HEIGHT + LINE_SPACE;
       ledgerheight <= lesserheight; ledgerheight += LINE_SPACE)
  {
    cairo_move_to( cr, xx + ((ledgerheight==lesserheight)?-EXTRA_ON_LEDGER:2), ledgerheight + y );
    cairo_line_to( cr, xx + width + ((ledgerheight==lesserheight)?EXTRA_ON_LEDGER:2), ledgerheight + y );
  }

  cairo_stroke( cr );
}

/**
 * Draw the chord object on the score
 *
 */
gint
draw_chord ( cairo_t *cr, objnode * curobj, gint xx, gint y,
	    gint mwidth, gint * accs, gboolean selected)
{
  gint highest = 0;
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
  if(duration<0) noteheadtype = 0;

  gint i;
  gint beampainty, arcwidth;

  gint prevbaseduration, nextbaseduration;
  GList *curnode;
  gboolean is_grace = thechord.is_grace && thechord.notes;
  DenemoGraphic *override_notehead = NULL;//overriding notehead to be used for all notes of chord unless built-in or overriden
  gint gx=0, gy=0;//positioning for overriding notehead
  if(cr) {
  cairo_save(cr);
  cairo_set_line_width( cr, 1.0 );



 if (thechord.slur_begin_p)
   draw_slur_start(cr, xx, y);

 if (thechord.slur_end_p)
   draw_slur_end(cr, xx, y);





  if(is_grace) {
    note *thenote = (note *) thechord.notes->data;
    cairo_translate( cr, xx, y+thenote->y);
    cairo_scale( cr, 0.8, 0.8);
    cairo_translate( cr, -xx, -(y+thenote->y));
  }
  //  g_print("Invisble is %d\n", mudelaitem->isinvisible); 
  if (mudelaitem->isinvisible) {
    if (selected)
      cairo_set_source_rgb( cr, 231.0/255 , 1, 39.0/255 ); 
    else
      cairo_set_source_rgb( cr, 180.0/255, 160.0/255, 32.0/255 );// yellow for non printing
  }

  }

  gboolean override_chord_graphic = FALSE;
  if(cr)
    { GList *g = thechord.directives;
      gint count = 0;
      for(;g;g=g->next) {
	DenemoDirective *directive = (DenemoDirective *)g->data;
	
	if(directive->graphic) {
	  
	  if((directive->override&DENEMO_OVERRIDE_GRAPHIC))
	    override_chord_graphic = TRUE;

	  if( (directive->override&DENEMO_OVERRIDE_GRAPHIC) ) {
	    if(directive->override&DENEMO_ALT_OVERRIDE) {
	      gx=directive->gx;
	      gy=directive->gy;
	      override_notehead = directive->graphic;//will be used to draw all the notes/the rest
	    } else {
	      drawbitmapinverse_cr (cr, directive->graphic, 
				    xx+directive->gx-directive->graphic->width/2, 
				    y + STAFF_HEIGHT+40+directive->gy  - directive->graphic->height/2, FALSE);
				      
	    }
	  } else { //this directive's graphic does not override entire chord (other ones may)
	    if(directive->override&DENEMO_ALT_OVERRIDE) {//ALT_OVERRIDE makes the positioning stem sensitive
	    //FIXME - use count to stack up multiple markings
	      drawbitmapinverse_cr (cr, directive->graphic, 
				    xx+directive->gx-directive->graphic->width/2 + 4, 
				    (thechord.is_stemup? (y + 8 + thechord.lowesty + directive->gy):(y + thechord.highesty - 8 - directive->gy))  - directive->graphic->height/2, thechord.is_stemup);
	      if(!thechord.is_stemup)
		highest =  ((y + thechord.highesty-directive->gy - 8) - directive->graphic->height/2);
	      
	    } else {
	      drawbitmapinverse_cr (cr, directive->graphic, 
				    xx+directive->gx-directive->graphic->width/2, 
				    y + STAFF_HEIGHT+ 8 + thechord.lowesty+count+directive->gy  - directive->graphic->height/2, FALSE);	      
	    }
	  }
	}
	if(directive->display) {
	  drawnormaltext_cr (cr, directive->display->str, xx+directive->tx, y + ((thechord.highesty>-10)?-10:thechord.highesty) - 8 +count+directive->ty );
	  highest = y + ((thechord.highesty>-10)?-10:thechord.highesty) - 8 +count+directive->ty;
	  count += 16;
	}
      } //for each chord directive
    }//if drawing do chord directives
  if( (!override_chord_graphic) || (override_chord_graphic && override_notehead)) {
    if (!thechord.notes/* a rest */) {
      if(cr)draw_rest (cr, MAX(duration, 0), thechord.numdots, xx, y, override_notehead, gx, gy);
    }  else {
      /* Draw the noteheads and accidentals */
      for (curnode = thechord.notes; curnode; curnode = curnode->next){
	note *thenote = (note *)curnode->data;
	draw_notehead (cr, thenote, duration,
		       thechord.numdots, xx, y, accs, thechord.is_stemup, mudelaitem->isinvisible, override_notehead, gx, gy); 
      }
    }
  }
  if(!cr) return highest;
      /* Now the stem and beams. This is complicated. */
      if (thechord.notes/* not a rest */) {
	if (thechord.is_stemup)
	  {
if(!override_chord_graphic) {
	if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
	  {
	    if (duration >= 3)
	      /* Up-pointing stem pixmap */
	      drawfetachar_cr (cr, upstem_char[duration],
			       xx + NOTEHEAD_WIDTH,
			       thechord.highesty + y + 3
			       - (duration == 6 ? EXTRA_STEM_HEIGHT
				  : STEM_HEIGHT));
	  }
	else if (nextmuditem && !mudelaitem->isend_beamgroup)
	  {
	    /* Draw the thin beam across the gap */
	    cairo_rectangle (cr,
			     xx + headwidths[noteheadtype] - 1,
			     y + thechord.stemy,
			     nextmuditem->x - mudelaitem->x,
			     THINBEAM_HEIGHT);
	    cairo_fill(cr);
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
		if (nextbaseduration >= i) {
		  /* Draw a thick beam across the gap */
		  cairo_rectangle (cr,
				   xx + headwidths[noteheadtype] - 1,
				   y + beampainty,
				   nextmuditem->x - mudelaitem->x,
				   THICKBEAM_HEIGHT);
		  cairo_fill(cr);
		  
		} else if (prevbaseduration < i) {
		  /* Draw a stub to the right of the staff */
		  cairo_rectangle (cr,
				   xx + headwidths[noteheadtype] - 1,
				   y + beampainty, STUB_WIDTH,
				   THICKBEAM_HEIGHT);
		  cairo_fill( cr );
		}
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
		  cairo_rectangle (cr,
				   xx + headwidths[noteheadtype] - 1 -
				   STUB_WIDTH, y + beampainty, STUB_WIDTH,
				   THICKBEAM_HEIGHT);
		  cairo_fill(cr);
		}
	  }
	
	if (duration > 0) {
	  /* Vertical line */
	  cairo_move_to( cr, xx + headwidths[noteheadtype], thechord.stemy + y );
	  cairo_line_to( cr, xx + headwidths[noteheadtype], thechord.lowesty + y - 2 );
	  cairo_stroke( cr );
	}
 } //if graphic not overrriden	
	
	/* Now draw the tie, if appropriate */
	if (thechord.is_tied)
	  {
	    if (nextmuditem)
	      arcwidth = nextmuditem->x - mudelaitem->x;
	    else
	      arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	    
	    cairo_set_line_width( cr, 1.0 );
	    cairo_move_to( cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13 );
	    cairo_rel_curve_to( cr, arcwidth/3, -8, arcwidth*2/3, -8, arcwidth, 0 );
	    cairo_stroke( cr );
	  }
      }			/* End stemup stuff */
    else
      {

if(!override_chord_graphic) {

			/* chord is stemdown */
	if (mudelaitem->isstart_beamgroup && mudelaitem->isend_beamgroup)
	  {
	    if (duration >= 3)
	      /* Down-pointing stem */
	      drawfetachar_cr (cr, downstem_char[duration],
			       xx,
			       thechord.lowesty + y 
			       + (duration == 6 ? EXTRA_STEM_HEIGHT
				  : STEM_HEIGHT)
			       );
	  }
	else if ((nextmuditem) && !mudelaitem->isend_beamgroup)
	  {
	    /* Draw the thin beam across the gap */
	    cairo_rectangle( cr, xx, y + thechord.stemy - THINBEAM_HEIGHT + 1,
			     nextmuditem->x - mudelaitem->x,
			     THINBEAM_HEIGHT);
	    cairo_fill( cr );
	    
	    if (mudelaitem->isstart_beamgroup || !prevmuditem)
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
		if (nextbaseduration >= i) {
		  /* Draw a thick beam across the gap */
		  cairo_rectangle( cr, xx, y + beampainty,
				   nextmuditem->x - mudelaitem->x,
				   THICKBEAM_HEIGHT);
		  cairo_fill( cr );
		  
		} else if (prevbaseduration < i) {
		  /* Draw a stub to the right of the staff */
		  cairo_rectangle (cr, xx, y + beampainty,
				   STUB_WIDTH, THICKBEAM_HEIGHT);
		  cairo_fill(cr);
		}
	      }		/* End for loop */
	  }			/* End drawing for non-end-beamgroup notes */
	else
	  {			/* We're at the end of a beamgroup */
	    if(prevmuditem)//sanity check
	      for (i = MAX (((chord *) prevmuditem->object)->baseduration,
			    4),
		     beampainty = thechord.stemy - FIRSTBEAMSPACE - 
		     THICKBEAM_HEIGHT + 1 -
		     (SUBSQBEAMSPACE * (i - 4));
		   i <= thechord.baseduration;
		   i++, beampainty -= SUBSQBEAMSPACE)
		{
		  /* Draw a stub to the left of the staff */
		  cairo_rectangle (cr, xx - STUB_WIDTH,
				   y + beampainty, STUB_WIDTH,
				   THICKBEAM_HEIGHT);
		  cairo_fill(cr);
		}
	  }
	
	if (duration > 0) {
	  /* Vertical line */
	  cairo_move_to( cr, xx, thechord.highesty + y + 2 );
	  cairo_line_to( cr, xx, thechord.stemy + y );
	  cairo_stroke( cr );
	}
 }
	/* Now draw the tie, if appropriate */
	if (thechord.is_tied)
	  {
	    if (nextmuditem)
	      arcwidth = nextmuditem->x - mudelaitem->x;
	    else
	      arcwidth = mwidth - mudelaitem->x + SPACE_FOR_BARLINE;
	    cairo_set_line_width( cr, 1.0 );
	    cairo_move_to( cr, xx + headwidths[noteheadtype] / 2, y + thechord.highesty - 13 );
	    cairo_rel_curve_to( cr, arcwidth/3, -8, arcwidth*2/3, -8, arcwidth, 0 );
	    cairo_stroke( cr );
	  }
      }
  /* End stemdown stuff */
  
  draw_articulations (cr, thechord, xx, y); 
  if(is_grace) cairo_restore(cr);
  draw_ledgers (cr, thechord.highesty, thechord.lowesty, xx, y,
		headwidths[noteheadtype]);
  if(is_grace) cairo_save(cr);
  
  }				/* end if not a rest draw stems etc */

cairo_restore (cr);

return highest;
}



/**
 * Draw the articulations on the score
 *
 */
void
draw_articulations (cairo_t * cr,
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
#if 0
      g_print ("ornament %d\n", *(enum ornament *) (tmp->data));
#endif
      if (*(enum ornament *) tmp->data == (enum ornament) STACCATO)
	drawfetachar_cr (cr, options_char[4],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TENUTO)
	drawfetachar_cr (cr,
			   options_char[5], xx,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) STACCATISSIMO)
	drawfetachar_cr (cr, options_char[6],
			   xx + NOTEHEAD_WIDTH / 2,
			   y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) D_ACCENT)
	drawfetachar_cr (cr, options_char[2],
			   xx + 2, y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) MARCATO)
	drawfetachar_cr (cr, options_char[8],
			   xx + 2, y + extra);


      /* 
       * Always should appear above the note(s) they
       * effect.
       */
      if (*(enum ornament *) tmp->data == (enum ornament) FERMATA)
	drawfetachar_cr (cr,
			   options_char[0], xx - FERMATA_WIDTH / 4,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) CODA)
	drawfetachar_cr (cr,
			   options_char[10], xx - CODA_WIDTH / 4,
			   y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL)
	drawfetachar_cr (cr,
			   noteoptions_char[0], xx - TRILL_WIDTH / 4,
			   y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) TURN)
	drawfetachar_cr (cr,
			   noteoptions_char[1], xx - TURN_WIDTH / 4,
			   y + extra);

      else if (*(enum ornament *) tmp->data == (enum ornament) MORDENT)
	drawfetachar_cr (cr,
			   noteoptions_char[2], xx - MORDENT_WIDTH / 4,
			   y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) DBOW)
	drawfetachar_cr (cr,
			   strings_char[0], xx, y + extra);
      else if (*(enum ornament *) tmp->data == (enum ornament) UBOW)
	drawfetachar_cr (cr,
			   strings_char[1], xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) RHEEL)
	{
	  drawfetachar_cr (cr,
			     organ_char[1], xx, y + extra);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LHEEL)
	{
	  drawfetachar_cr (cr,
			     organ_char[0], xx, y + extra);
	}

      if (*(enum ornament *) tmp->data == (enum ornament) RTOE)
	{
	  drawfetachar_cr (cr,
			     organ_char[3], xx, y + extra);
	}
      else if (*(enum ornament *) tmp->data == (enum ornament) LTOE)
	{
	  drawfetachar_cr (cr,
			     organ_char[2], xx, y + extra);
	}
      if (*(enum ornament *) tmp->data == (enum ornament) D_ARPEGGIO)
	drawfetachar_cr (cr,
			   options_char[24], xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) UPPRALL)
	drawfetachar_cr (cr, options_char[23],
			   xx, y + extra);

      if (*(enum ornament *) tmp->data == (enum ornament) FLAGEOLET)
	drawfetachar_cr (cr, options_char[11],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) OPEN)
	drawfetachar_cr (cr, options_char[12],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLMORDENT)
	drawfetachar_cr (cr, options_char[13],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALLPRALL)
	drawfetachar_cr (cr, options_char[14],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) PRALL)
	drawfetachar_cr (cr, options_char[15],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) REVERSETURN)
	drawfetachar_cr (cr, options_char[16],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) SEGNO)
	drawfetachar_cr (cr, options_char[17],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) SFORZATO)
	drawfetachar_cr (cr, options_char[18],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) STOPPED)
	drawfetachar_cr (cr, options_char[19],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) THUMB)
	drawfetachar_cr (cr, options_char[20],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILLELEMENT)
	drawfetachar_cr (cr, options_char[21],
			   xx, y + extra);
      if (*(enum ornament *) tmp->data == (enum ornament) TRILL_ELEMENT)
	drawfetachar_cr (cr, options_char[22],
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
      else if (( *(enum ornament *) tmp->data == (enum ornament) STACCATO ||
	         *(enum ornament *) tmp->data == (enum ornament) TENUTO      )
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
