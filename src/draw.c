/* draw.c
 * loop that draws all the items in the presently-displayed part of
 * the score
 *  
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include <string.h>
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "draw.h"		/* Which includes gtk.h */
#include "drawingprims.h"
#include "gcs.h"
#include "slurs.h"
#include "hairpin.h"
#include "staffops.h"
#include "utils.h"
#include "../pixmaps/toomany.xbm"
#include "exportlilypond.h"	/* to generate lily text for display */
#include "lyparserfuncs.h"	/* to generate lily text for display */
#include "pitchentry.h"
/**
 * defines function to choose the correct 
 * depending upon the GTK version
 *
 */
#define REDEXCL_WIDTH 3
#define REDEXCL_HEIGHT 13

/**
 * scorearea_configure_event
 *
 * This function just creates a backing pixmap of the appropriate
 * size, recaculates the number of measures that can be fit into
 * the display, and returns 
 */
gint
scorearea_configure_event (GtkWidget * widget, GdkEventConfigure * event)
{
  DenemoGUI *gui = Denemo.gui;
  /* Create a new backing pixmap of the appropriate size */
  if (gui->pixmap)
    gdk_pixmap_unref (gui->pixmap);

  gui->pixmap = gdk_pixmap_new (widget->window,
				widget->allocation.width,
				widget->allocation.height, -1);

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
  gint clef;
  gint key;
  gint curaccs[7];
  gint keyaccs[7];
  gint stem_directive;
  gint time1, time2;
  gint tickspermeasure;
  gint wholenotewidth;
  gint objnum;
  gint measurenum;//would need measurenum_adj to allow control of numbering after pickup etc...
  gint staffnum;
  gint top_y;
  gint y;
  gint markx1, markx2;
  gint marky1, marky2;
  /*GString *dynamic; */
  GtkWidget *widget;
  measurenode *curmeasure;
  GList *mwidthiterator;
  GSList *slur_stack;
  GSList *hairpin_stack;
  GdkGC *gc;
  gboolean haslyrics;
  gint space_above;
  gint highy;/*(return) the highest y value drawn*/
  gint lowy;/*(return) the lowest y value drawn*/
  gint in_highy; // FIXME these are passed in so that highy, lowy do not need to be passed back
  gint in_lowy;
  gboolean mark;//whether the region is selected
};

/**
 *  draw_object
 *
 * Draws a single object in a measure within a staff. 
 * @param curobj
 * @param x
 * @param y
 * @param gui
 * @param itp 
 * @return whether the DenemoObject is off the end of the measure or not 
 */
static gboolean
draw_object (objnode * curobj, gint x, gint y,
	     DenemoGUI * gui, struct infotopass *itp)
{
  static GdkGC *blackgc = NULL;
  static GdkGC *redgc = NULL;
  static GdkGC *greengc = NULL;
  static gboolean init=FALSE;
  static GdkColor white, black, blue, green, yellow;
  if(!init) {
    gdk_color_parse ("white", &white);
    gdk_colormap_alloc_color (gdk_colormap_get_system (), &white, TRUE, TRUE);
    gdk_color_parse ("black", &black);
    gdk_colormap_alloc_color (gdk_colormap_get_system (), &black, TRUE, TRUE);
    gdk_color_parse ("blue", &blue);
    gdk_colormap_alloc_color (gdk_colormap_get_system (), &blue, TRUE, TRUE);
    gdk_color_parse ("green", &green);
    gdk_colormap_alloc_color (gdk_colormap_get_system (), &green, TRUE, TRUE);
    gdk_color_parse ("yellow", &yellow);
    gdk_colormap_alloc_color (gdk_colormap_get_system (), &yellow, TRUE, TRUE);
    init = TRUE;
  }
  itp->highy = itp->lowy = 0;
  DenemoScore *si = gui->si;
  DenemoObject *mudelaitem = (DenemoObject *) curobj->data;
  /* The current note, rest, etc. being painted */
  gint extra;

  if (!blackgc)
    blackgc = gcs_blackgc ();

  if (!redgc)
    redgc = gcs_redgc ();
  if (!greengc)
    greengc = gcs_greengc ();
  /* Should we set cursor-context info before drawing? */

  /************ FIXME the drawing is side-effecting the DenemoScore si here *******************/
  if (si->currentobject == curobj)
    {
      si->cursorclef = itp->clef;
      if (!si->cursor_appending)
	memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
    }

  {
    
    GdkColor *thecolor;
    if(mudelaitem->type==CHORD && ((chord *) mudelaitem->object)->tone_node)
      thecolor = &yellow;
    else
      thecolor =/* (mudelaitem->isinvisible) ? &white :*/ itp->mark?&blue:&black;
    gdk_gc_set_foreground (blackgc, thecolor);
  }


  switch (mudelaitem->type)
    {
    case CHORD:
       if (((chord *) mudelaitem->object)->is_figure && ((chord *) mudelaitem->object)->figure)
      //if (((chord *) mudelaitem->object)->is_figure)
	//draw_figure (gui->pixmap, itp->gc,
	//	     gtk_style_get_font (itp->widget->style),
	//	     x + mudelaitem->x, y, mudelaitem);
  
	draw_figure (gui->pixmap, itp->gc,
		     gtk_style_get_font (itp->widget->style),
		     x + mudelaitem->x,
		     y + (((chord *) mudelaitem->object)->lowesty / 2),
		     mudelaitem);
  
      else
	{
	  draw_chord (gui->pixmap, itp->gc, curobj, x + mudelaitem->x, y,
		      GPOINTER_TO_INT (itp->mwidthiterator->data),
		      itp->curaccs, itp->mark);
	  if((((chord *) mudelaitem->object)->highesty) < itp->highy)
 	    itp->highy  = ((chord *) mudelaitem->object)->highesty/*, g_print("setting highy %d\n", itp->highy)*/;

	  if((((chord *) mudelaitem->object)->lowesty) > itp->lowy+STAFF_HEIGHT)
	    itp->lowy  = ((chord *) mudelaitem->object)->lowesty-STAFF_HEIGHT;




	}
       if (((chord *) mudelaitem->object)->is_fakechord)
	draw_fakechord (gui->pixmap, itp->gc,
		     gtk_style_get_font (itp->widget->style),
		     x + mudelaitem->x, 
		     y - 45,
		     //y - (((chord *) mudelaitem->object)->highesty ), 
		     mudelaitem);
       else
	{
	  draw_chord (gui->pixmap, itp->gc, curobj, x + mudelaitem->x, y,
		      GPOINTER_TO_INT (itp->mwidthiterator->data),
		      itp->curaccs, itp->mark);
	}

      if (((chord *) mudelaitem->object)->lyric)
	{
	  draw_lyric (gui->pixmap, itp->gc,
		      gtk_style_get_font (itp->widget->style),
		      x + mudelaitem->x,
		      y + ((chord *) mudelaitem->object)->lowesty,
		      mudelaitem);
	  itp->haslyrics = TRUE;
	}

      if (((chord *) mudelaitem->object)->dynamics)
	draw_dynamic (gui->pixmap, itp->gc,
		      gtk_style_get_font (itp->widget->style),
		      x + mudelaitem->x, y, mudelaitem);

      if (((chord *) mudelaitem->object)->slur_end_p)
	draw_slur (gui->pixmap, itp->gc, &(itp->slur_stack),
		   x + mudelaitem->x, y);
      if (((chord *) mudelaitem->object)->slur_begin_p)
	itp->slur_stack =
	  push_slur_stack (itp->slur_stack, x + mudelaitem->x);

      if (((chord *) mudelaitem->object)->crescendo_begin_p)
	itp->hairpin_stack =
	  push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
      else if (((chord *) mudelaitem->object)->diminuendo_begin_p)
	itp->hairpin_stack =
	  push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
      if (((chord *) mudelaitem->object)->crescendo_end_p)
	{
	  if (top_hairpin_stack (itp->hairpin_stack) <= -1)
	    {
#if 0
	      //this is only the visible part of the cresc, the start may be off screen
	      ((chord *) mudelaitem->object)->crescendo_end_p = FALSE;
	      warningdialog
		("Crescendo end without a corresponding start\n"
		 "removing the crescendo end");
#endif
	    }
	  draw_hairpin (gui->pixmap, itp->gc, &(itp->hairpin_stack),
			x + mudelaitem->x, y, 1);
	}
      else if (((chord *) mudelaitem->object)->diminuendo_end_p)
	{
	  if (top_hairpin_stack (itp->hairpin_stack) <= -1)
	    {
#if 0
	      //this is only the visible part of the dim, the start may be off screen
	      ((chord *) mudelaitem->object)->diminuendo_end_p = FALSE;
	      warningdialog
		("Diminuendo end without a corresponding start\n"
		 "removing the diminuendo end");
#endif
	    }
	  draw_hairpin (gui->pixmap, itp->gc, &(itp->hairpin_stack),
			x + mudelaitem->x, y, 0);
	}
	/* notice the following does not check is_figure but checks if figure is not VOID) */      
      if (!((chord *) mudelaitem->object)->is_figure && ((chord *) mudelaitem->object)->figure)
      //if (((chord *) mudelaitem->object)->figure)
        draw_figure (gui->pixmap, itp->gc,
		     gtk_style_get_font (itp->widget->style),
		     x + mudelaitem->x,
		     y + (((chord *) mudelaitem->object)->lowesty / 2),
		     mudelaitem);
      if (!((chord *) mudelaitem->object)->is_fakechord && ((chord *) mudelaitem->object)->fakechord) 
	draw_fakechord (gui->pixmap, itp->gc,
		     gtk_style_get_font (itp->widget->style),
		     x + mudelaitem->x,
		     y - 45,
		     //y + (((chord *) mudelaitem->object)->highesty / 4),
		     mudelaitem);

      break;
    case TUPOPEN:
    case TUPCLOSE:
      draw_tupbracket (gui->pixmap, itp->gc,
		       gtk_style_get_font (itp->widget->style),
		       x + mudelaitem->x, y, mudelaitem);
      break;
    case LILYDIRECTIVE:
      // if(si->markstaffnum) not available
      draw_lily_dir(gui->pixmap, itp->gc,
		       gtk_style_get_font (itp->widget->style),
		       x + mudelaitem->x, y, itp->in_highy, itp->in_lowy, mudelaitem, itp->mark);  
      break;
    case CLEF:
      draw_clef (gui->pixmap, itp->gc, x + mudelaitem->x, y,
		 itp->clef = ((clef *) mudelaitem->object)->type);
      if (si->currentobject == curobj && si->cursor_appending)
	si->cursorclef = itp->clef;
      break;
    case KEYSIG:
      draw_key (gui->pixmap, itp->gc, x + mudelaitem->x, y,
		((keysig *) mudelaitem->object)->number, itp->key,
		itp->clef, TRUE);
      itp->key = ((keysig *) mudelaitem->object)->number;
      memcpy (itp->keyaccs, ((keysig *) mudelaitem->object)->accs,
	      SEVENGINTS);
      memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);
      if (si->currentmeasure == itp->curmeasure)
	/* We're in the current measure */
	memcpy (si->nextmeasureaccs, itp->keyaccs, SEVENGINTS);
      break;
    case TIMESIG:
      draw_timesig (gui->pixmap, itp->gc,
		    gtk_style_get_font (itp->widget->style),
		    x + mudelaitem->x, y, itp->time1 =
		    ((timesig *) mudelaitem->object)->time1, itp->time2 =
		    ((timesig *) mudelaitem->object)->time2);
      if (si->currentmeasure == itp->curmeasure)
	{
	  /* This is the current measure */
	  si->cursortime1 = itp->time1;
	  si->cursortime2 = itp->time2;
	}
      /* The following assumes no multiple simultaneous time signatures */
      itp->tickspermeasure = WHOLE_NUMTICKS * itp->time1 / itp->time2;
      break;
    case STEMDIRECTIVE:
      draw_stem_directive (gui->pixmap, itp->gc,
			   gtk_style_get_font (itp->widget->style),
			   x + mudelaitem->x, y, mudelaitem);
      itp->stem_directive = ((stemdirective *) mudelaitem->object)->type;
      break;
    case GRACE_START:
    case GRACE_END:
      draw_gracebracket (gui->pixmap, itp->gc,
			 gtk_style_get_font (itp->widget->style),
			 x + mudelaitem->x, y, mudelaitem);
      break;
    case BARLINE:
      {
	gint top_y = (si->staffspace / 4) + itp->space_above;
	x += GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE;
	g_print ("possible y values top_y %d, y %d itp->y %d\n", itp->top_y,
		 y, itp->y);
	drawbarline (gui->pixmap, itp->gc, x, itp->y, itp->y + STAFF_HEIGHT,
		     ((barline *) mudelaitem->object)->type);
#ifdef DEBUG
	g_print ("Draw barline\n");
#endif
      }
      break;
    default:
      /* Nothing */
      break;
    }
  if (si->currentobject == curobj)
    {				/* Draw the cursor */
      /* Determine if it needs to be red or not */
      if (si->cursor_appending || mudelaitem->type != CHORD)
	si->cursoroffend =
	  (mudelaitem->starttickofnextnote >= itp->tickspermeasure);
      else
	si->cursoroffend =
	  (mudelaitem->starttickofnextnote > itp->tickspermeasure);
      if (si->cursor_appending)
	{
	  extra = MAX (mudelaitem->minpixelsalloted,
		       space_after (mudelaitem->durinticks,
				    itp->wholenotewidth));
	  draw_cursor (gui->pixmap, si, x + mudelaitem->x + extra, y,
		       gui->mode, si->cursorclef);
	  memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
	}
      else
	{
	  draw_cursor (gui->pixmap, si, x + mudelaitem->x, y, gui->mode,
		       si->cursorclef);
	}
    }
      /* End cursor drawing */


  /* Now quite possibly update the mark */

  if (si->firststaffmarked == itp->staffnum
      && si->firstmeasuremarked == itp->measurenum
      && si->firstobjmarked == itp->objnum)
    itp->markx1 = x + mudelaitem->x - EXTRAFORSELECTRECT;
  if (si->laststaffmarked == itp->staffnum
      && si->lastmeasuremarked == itp->measurenum
      && si->lastobjmarked == itp->objnum)
    itp->markx2 = x + mudelaitem->x + mudelaitem->minpixelsalloted
      + EXTRAFORSELECTRECT;



  gdk_gc_set_foreground (blackgc, &black);
  //g_print("returning with %d\n", itp->highy);
  /* And give a return value and we're done */
  return (mudelaitem->starttickofnextnote > itp->tickspermeasure);
} /* draw_object */

/**
 * Draws a single measure within a staff. 
 * @param curmeasure pointer to the measure to draw
 * @param x
 * @param y
 * @param gui
 * @param itp
 */
void
draw_measure (measurenode * curmeasure, gint x, gint y,
	      DenemoGUI * gui, struct infotopass *itp)
{
  static GdkPixmap *redexclaim = NULL;
  static GdkGC *redgc;
  static GString *mstring;
  static PangoContext *context;
  static PangoLayout *layout;
  PangoFontDescription *desc;
  gboolean offend = FALSE;
  DenemoScore *si = gui->si;
  objnode *curobj;
  if (!context)
    {
      context =
	gdk_pango_context_get_for_screen (gdk_drawable_get_screen
					  (gui->pixmap));
      layout = pango_layout_new (context);
    }
  /* initialization */
  if (!redexclaim)
    {
      redexclaim = bitmaphelper (itp->widget, toomany);
      mstring = g_string_new (NULL);
      redgc = gcs_redgc ();
    }

  /* Set information about the state at the current measure,
     if necessary */

  memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);
  itp->wholenotewidth = si->measurewidth * itp->time2 / itp->time1;
  if (curmeasure == si->currentmeasure)
    {
      si->curmeasureclef = itp->clef;
      memcpy (si->curmeasureaccs, itp->keyaccs, SEVENGINTS);
      memcpy (si->nextmeasureaccs, itp->keyaccs, SEVENGINTS);
      si->curmeasurekey = itp->key;
      si->curmeasure_stem_directive = itp->stem_directive;
      si->cursortime1 = itp->time1;
      si->cursortime2 = itp->time2;
    }

  /* If this is the current staff, paint the measure number at the
   * preceding barline */

  if (si->currentstaffnum == itp->staffnum)
    {
      g_string_sprintf (mstring, "%d", itp->measurenum);
      pango_layout_set_text (layout, mstring->str, -1);
      desc = pango_font_description_from_string (FONT);
      pango_layout_set_font_description (layout, desc);
      pango_font_description_free (desc);
      gdk_draw_layout (gui->pixmap, itp->gc, x - SPACE_FOR_BARLINE,
		       y - 12, layout);

      //gdk_draw_text (si->pixmap, mnumfont, itp->gc, x - SPACE_FOR_BARLINE,
      // y - 2, mstring->str, mstring->len);
      if (si->currentmeasurenum == itp->measurenum && !si->currentobject)
	{
	  /* That is, the cursor's at the beginning of this blank measure */
	  si->cursoroffend = FALSE;
	  draw_cursor (gui->pixmap, si, x, y, gui->mode, itp->clef);
	  memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
	  si->cursorclef = itp->clef;
	}
    }
  curobj = (objnode *) curmeasure->data;
  /* These default values for the markx'es may be necessary down
   * the road */
  if (si->firststaffmarked == itp->staffnum
      && si->firstmeasuremarked == itp->measurenum)
    {
      if (!curobj)
	itp->markx1 = x - EXTRAFORSELECTRECT;
      else
	itp->markx1 = x + GPOINTER_TO_INT (itp->mwidthiterator->data);
    }
  if (si->laststaffmarked == itp->staffnum
      && si->lastmeasuremarked == itp->measurenum)
    {
      if (!curobj
	  || (si->lastobjmarked >=
	      (gint) (g_list_length ((objnode *) curobj))))
	itp->markx2 =
	  x + GPOINTER_TO_INT (itp->mwidthiterator->data) +
	  SPACE_FOR_BARLINE + EXTRAFORSELECTRECT;
      else
	itp->markx2 = x;
    }



  gboolean not_marked = (!si->markstaffnum) ||
    (si->firststaffmarked >itp->staffnum) || 
    (si->laststaffmarked < itp->staffnum) ||
    (si->firstmeasuremarked > itp->measurenum) ||
    (si->lastmeasuremarked < itp->measurenum);

  gboolean definitely_marked = (!not_marked) &&
    (si->firstmeasuremarked < itp->measurenum) &&
    (si->lastmeasuremarked > itp->measurenum);
  gboolean in_firstmeas = (si->firstmeasuremarked == itp->measurenum);
  gboolean in_lastmeas = (si->lastmeasuremarked == itp->measurenum);
  /* Draw each mudelaitem */
  for (itp->objnum = 0; curobj; curobj = curobj->next, itp->objnum++) {
    itp->mark =  (definitely_marked) || 
      ((!not_marked) && ((in_firstmeas && !in_lastmeas &&
			 (si->firstobjmarked <= itp->objnum)) ||
		       (in_lastmeas && !in_firstmeas &&
		        (si->lastobjmarked >= itp->objnum)) ||
			 (in_lastmeas && in_firstmeas && 
			  (si->firstobjmarked <= itp->objnum) && 
			  (si->lastobjmarked >= itp->objnum)))
       );
    offend = draw_object (curobj, x, y, gui, itp);
  }
  /* Paint the red exclamation point, if necessary */
  if (offend)
    drawbitmapinverse (gui->pixmap, redgc, redexclaim,
		       x, y - 8 - REDEXCL_HEIGHT,
		       REDEXCL_WIDTH, REDEXCL_HEIGHT);
}

/**
 * Draws a single staff 
 * TODO sort out graphics context for active polyphonic voice should 
 * do it here
 * @param curstaffstruct pointer to the staff to draw
 * @param y    y poisition of the staff
 * @param gui   pointer to the DenemoGUI structure
 * @param itp  pointer to the infotopass structure
 */
static void
draw_staff (DenemoStaff * curstaffstruct, gint y,
	    DenemoGUI * gui, struct infotopass *itp)
{
  PangoContext *context;
  PangoLayout *layout;
  PangoFontDescription *desc;
  static GdkGC *blackgc;
  static GdkGC *graygc;

  DenemoScore *si = gui->si;
  gint x, i;
  GdkGC *gc;
  context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (gui->pixmap));
  layout = pango_layout_new (context);

  if (!blackgc)
    {
      blackgc = gcs_blackgc ();
      graygc = gcs_slategraygc ();
    }
  if ((DenemoStaff *) si->currentstaff->data == curstaffstruct)
    gc = blackgc;
  else
    gc = graygc;



  draw_clef (gui->pixmap, gc, LEFT_MARGIN, y,
	     itp->clef = curstaffstruct->leftmost_clefcontext);
  x = KEY_MARGIN;
  draw_key (gui->pixmap, gc, x, y,
	    itp->key = curstaffstruct->leftmost_keysigcontext,
	    0, itp->clef, TRUE);
  memcpy (itp->keyaccs, curstaffstruct->leftmost_keyaccs, SEVENGINTS);
  x += si->maxkeywidth;

  draw_timesig (gui->pixmap, gc, gtk_style_get_font (itp->widget->style), x,
		y, itp->time1 =
		curstaffstruct->leftmost_time1context, itp->time2 =
		curstaffstruct->leftmost_time2context);
  x += SPACE_FOR_TIME;
  itp->stem_directive = curstaffstruct->leftmost_stem_directive;
  itp->tickspermeasure = WHOLE_NUMTICKS * itp->time1 / itp->time2;
  if (si->firststaffmarked == itp->staffnum)
    itp->marky1 = y - EXTRAFORSELECTRECT;
  if (si->laststaffmarked == itp->staffnum)
    itp->marky2 = y + STAFF_HEIGHT + EXTRAFORSELECTRECT;

  gint buffer = (curstaffstruct->voicenumber == 1) ? 24 :
    (curstaffstruct->voicenumber == 2
     || curstaffstruct->voicenumber == 3) ? 12 : 0;

  /* Draw staff name */
  pango_layout_set_text (layout, curstaffstruct->denemo_name->str, -1);
  desc = pango_font_description_from_string (FONT);
  pango_layout_set_font_description (layout, desc);


  gdk_draw_layout (gui->pixmap, gc, KEY_MARGIN, y - buffer, layout);
  gint title_highy = 0;
  if(curstaffstruct->staff_directives) {
    GList *g = curstaffstruct->staff_directives;
    gint count=1;
    for(;g;g=g->next, count++) {
      DenemoDirective* directive = g->data;
        if(directive->display) { 
    pango_layout_set_text (layout,
			   directive->display->str,
			   -1);
    pango_layout_set_font_description (layout, desc);
    gint ypos = y-buffer+directive->ty - count*10;
    gdk_draw_layout (gui->pixmap, gc, KEY_MARGIN + directive->tx, ypos, layout);
    //g_print("high %d %d\n", itp->highy, -ypos);
    if(title_highy > ypos) title_highy = ypos;
  }
    }

  }
  //FIXME voice directives...

  pango_font_description_free (desc);

  /* Loop that will draw each measure. Basically a for loop, but was uglier
   * when written that way.  */
  itp->measurenum = si->leftmeasurenum;
  itp->curmeasure =
    g_list_nth (curstaffstruct->measures, itp->measurenum - 1);


  //FIX in measureops.c for case where si->measurewidths is too short
  itp->mwidthiterator = g_list_nth (si->measurewidths, itp->measurenum - 1);



  itp->gc = gc;
  while (itp->measurenum <= si->rightmeasurenum+1
	 && itp->measurenum <= g_list_length (curstaffstruct->measures))
    {
      if(itp->measurenum == si->rightmeasurenum+1)
	itp->gc = gcs_slategraygc ();
      draw_measure (itp->curmeasure, x, y, gui, itp);
      x += GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE;
      itp->curmeasure = itp->curmeasure->next;
      itp->mwidthiterator = itp->mwidthiterator->next;
      itp->measurenum++;
    }

  if(itp->highy > title_highy)
    itp->highy = title_highy;

  /* now draw the staff lines, reset itp->slur_stack, and we're done */
  for (i = 0; i < curstaffstruct->no_of_lines; i++, y += LINE_SPACE)
    gdk_draw_line (gui->pixmap, blackgc, LEFT_MARGIN, y,
		   x - HALF_BARLINE_SPACE, y);
  /* Initialize the slur_stack for this staff. For the time being,
     slurs that begin and/or end after the portion of the music
     that is shown are not drawn. */
  if (itp->slur_stack)
    {
      g_slist_free (itp->slur_stack);
      itp->slur_stack = NULL;
    }
}

/**
 * Draw the score directives and return the amount of vertical space they took up.

*/
static gint
draw_score_directives (void) {
  GList *g = Denemo.gui->lilycontrol.directives;
  gint y = 0;
  if(g==NULL)
    return 0;
  PangoContext *context =
    gdk_pango_context_get_for_screen (gdk_drawable_get_screen (Denemo.gui->pixmap));
  PangoLayout *layout = pango_layout_new (context);
  PangoFontDescription *desc = pango_font_description_from_string (FONT);
  for(;g;g=g->next) {
   DenemoDirective *directive = g->data;
    if(directive->display) {
      pango_layout_set_text (layout,
			     directive->display->str,
			     -1);
      
      pango_layout_set_font_description (layout, desc);
      y += directive->ty;
      gdk_draw_layout (Denemo.gui->pixmap, gcs_blackgc(), directive->tx, y, layout);
    }
  }
  pango_font_description_free (desc);
  return y;
}

/**
 * This actually draws the score, staff-by-staff 
 * @param widget pointer to the parent widget
 * @param gui pointer to the DenemoGUI structure
 * returns whether the height of the drawing area was sufficient to draw everything
 */
static gboolean
draw_score (GtkWidget * widget, DenemoGUI * gui)
{
  staffnode *curstaff;
  gint x, y=0, i;
  struct infotopass itp;
  static GdkGC *blackgc = NULL;
  static GdkGC *bluegc;
  static GdkGC *graygc;
  GList *mwidthiterator;
  gboolean repeat = FALSE;
  DenemoScore *si = gui->si;

  /* One-time inits */
  if (!blackgc)
    {
      blackgc = gcs_blackgc ();
      bluegc = gcs_bluegc ();
      graygc = gcs_graygc ();
    }

  /* Initialize some fields in itp */

  itp.widget = widget;// this is used for widget->style, it is spuriously passed to the macro bitmaphelper
  itp.slur_stack = NULL;
  itp.hairpin_stack = NULL;
  itp.haslyrics = FALSE;
  itp.highy = 0;//in case there are no objects...
  y = 0;
  //draw score title etc above top staff, if it is visible and if desired by score directives
  if(si->top_staff==1) {
    y = draw_score_directives();
    curstaff = si->thescore;
    gint space = ((DenemoStaff *) curstaff->data)->space_above;
    if(space<y) 
      space = ((DenemoStaff *) curstaff->data)->space_above = y;
    gdk_draw_rectangle (gui->pixmap, gcs_lightbluegc(), TRUE, 0, 0, KEY_MARGIN, 10); 
  }
  /* Draw each staff */
  for ((itp.staffnum = si->top_staff,
	curstaff = g_list_nth (si->thescore, si->top_staff - 1),
	y += si->staffspace / 4);
       curstaff && itp.staffnum <= si->bottom_staff; itp.staffnum++)
    {
      if (curstaff && ((DenemoStaff *) curstaff->data)->voicenumber == 1)
	y += ((DenemoStaff *) curstaff->data)->space_above;
      itp.space_above = ((DenemoStaff *) curstaff->data)->space_above;
      gint top_y = (si->staffspace / 4) + itp.space_above;
    
      itp.top_y = top_y;
      itp.y = y;
      gint highy = ((DenemoStaff *) curstaff->data)->space_above;
      gint lowy =  ((DenemoStaff *) curstaff->data)->space_below;

      itp.in_highy = highy, itp.in_lowy = lowy;
      itp.highy = 0;//do not pass on extra_space from one staff to the next

      gdk_draw_rectangle (gui->pixmap, gcs_lightbluegc(), TRUE, 0, y, LEFT_MARGIN, STAFF_HEIGHT/*staff edit*/);
      if(si->leftmeasurenum==1) {
	/* draw background of clef, keysig, timesig */
	gint key = gui->si->maxkeywidth;
	gint cmajor = key?0:5;//allow some area for keysig in C-major
	gdk_draw_rectangle (gui->pixmap, gcs_graygc(), TRUE, LEFT_MARGIN,y,KEY_MARGIN-LEFT_MARGIN - cmajor,STAFF_HEIGHT);/*clef edit*/
	gdk_draw_rectangle (gui->pixmap, gcs_lightbluegc(), TRUE, KEY_MARGIN-cmajor,y,key+2*cmajor,STAFF_HEIGHT);/*keysig edit*/
	gdk_draw_rectangle (gui->pixmap, gcs_graygc(), TRUE, KEY_MARGIN+key+cmajor,y,SPACE_FOR_TIME-cmajor,STAFF_HEIGHT);/*timesig edit*/

            
	       }
      draw_staff ((DenemoStaff *) curstaff->data, y, gui, &itp);

      //IN FACT itp.highy is only set by one measure, it is reset to zero in the measure loop
      if(-itp.highy>highy && -itp.highy<MAXEXTRASPACE) //FIXME this should be done before draw_staff returns
	/*g_print("setting space above %d staff %d\n", -itp.highy, itp.staffnum),*/((DenemoStaff *) curstaff->data)->space_above = -itp.highy, repeat=TRUE;
      if(itp.lowy>lowy && itp.lowy<MAXEXTRASPACE)
	((DenemoStaff *) curstaff->data)->space_below = itp.lowy, repeat=TRUE;


        /* Now draw the barlines between the measures, across all the staffs */
       mwidthiterator = g_list_nth (si->measurewidths, si->leftmeasurenum - 1);
       for (x = KEY_MARGIN + si->maxkeywidth + 
	      SPACE_FOR_TIME - HALF_BARLINE_SPACE,
	      i = si->leftmeasurenum;
	    i <= si->rightmeasurenum; mwidthiterator = mwidthiterator->next, 
	      i++)
	 {
	   gint top = y + STAFF_HEIGHT; 

	   x += GPOINTER_TO_INT (mwidthiterator->data) + SPACE_FOR_BARLINE;
	   
	   if (!mwidthiterator->next) /* Last measure - draw double-barline */
	     x -= 3;
	   gdk_draw_line (gui->pixmap, graygc, x, top, x, y);
	   
	   if (!mwidthiterator->next)
	     {
	       /* Again, we've reached the end of the score and should
		* draw a double-barline */
	       x += 3;
	       gdk_draw_rectangle (gui->pixmap, blackgc, TRUE, x,
				   y, 4,
				   STAFF_HEIGHT+1);
	     }
	   
	 }

       if ( itp.staffnum < si->bottom_staff
	   &&    ((DenemoStaff *) curstaff->next->data)->voicenumber !=2)
	 {
	   if (itp.haslyrics) {
	     if(!((DenemoStaff *) curstaff->data)->haslyrics)
	       ((DenemoStaff *) curstaff->data)->haslyrics = TRUE;//, repeat=TRUE;
	     y += LYRICS_HEIGHT;
	   }
	   y +=
	     (si->staffspace + ((DenemoStaff *) curstaff->data)->space_below);
	 }
       itp.haslyrics = FALSE;
       curstaff = curstaff->next;
    }// for all the staffs


 
  /* Draw the selection rectangle */
  if (si->markstaffnum)
    draw_selection (gui->pixmap, bluegc, itp.markx1, itp.marky1, itp.markx2,
		    itp.marky2);
  return repeat;

  /* And we're done */
}

/**
 * Here we have the function that actually draws the score. Note that
 * it does not clip intelligently at all 
 */

gint
scorearea_expose_event (GtkWidget * widget, GdkEventExpose * event)
{

DenemoGUI *gui = Denemo.gui;
  do{
  /* Clear the backing pixmap */
  if(Denemo.gui->input_source!=INPUTKEYBOARD &&
     (Denemo.prefs.overlays || (Denemo.gui->input_source==INPUTAUDIO))
     && pitch_entry_active(gui)) {
    gdk_draw_rectangle (gui->pixmap,
			gcs_lightbluegc(),
			TRUE,
			0, 0,
			widget->allocation.width, widget->allocation.height);

  } else {
  if (GTK_WIDGET_IS_SENSITIVE (gui->scorearea) )
    gdk_draw_rectangle (gui->pixmap,
			widget->style->white_gc,
			TRUE,
			0, 0,
			widget->allocation.width, widget->allocation.height);
  else
    gdk_draw_rectangle (gui->pixmap,
			widget->style->bg_gc[0],
			TRUE,
			0, 0,
			widget->allocation.width, widget->allocation.height);
  }

  /* Draw the score */
  } while(draw_score (widget, gui));

  /* Now actually draw the backing pixmap onto the drawing area */

  gdk_draw_pixmap (gui->scorearea->window,
		   gui->scorearea->style->black_gc,
		   gui->pixmap,
		   0, 0, 0, 0,
		   gui->scorearea->allocation.width,
		   gui->scorearea->allocation.height);

  return TRUE;
}




/**
 * create an editable display of the passed str at the bottom 
 * of the drawing area 
 */
 /*
    void
    display_string (gchar * str, DenemoGUI *gui)
    {

    if (GTK_WIDGET_VISIBLE (gui->scorearea))     not in toplevel 
    set_text_node (NULL, gui);


    }
  */
