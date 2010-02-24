/* draw.c
 * loop that draws all the items in the presently-displayed part of
 * the score
 *  
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include <stdio.h>
#include <string.h>
#include <math.h>
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
#include "exportlilypond.h"	/* to generate lily text for display */
#include "lyparserfuncs.h"	/* to generate lily text for display */
#include "pitchentry.h"
#include "lyric.h"
#include "midi.h"

/**
 * defines function to choose the correct 
 * depending upon the GTK version
 *
 */
#define EXCL_WIDTH 3
#define EXCL_HEIGHT 13

#if 0
static GdkGC *  bluegc;
static GdkGC *  redgc;
static GdkGC *  graygc;
static GdkGC *  slategraygc;
static GdkGC *  lightbluegc;
static GdkGC *  blackgc;
static GdkGC *greengc = NULL;
#endif
GdkPixbuf *StaffPixbuf, *StaffPixbufSmall;

static void      
create_tool_pixbuf(void) {
  GtkWidget *widget = gtk_button_new();
  StaffPixbuf = gtk_widget_render_icon (widget, GTK_STOCK_PROPERTIES, GTK_ICON_SIZE_BUTTON, "denemo");
  StaffPixbufSmall = gtk_widget_render_icon (widget, GTK_STOCK_PROPERTIES, GTK_ICON_SIZE_MENU, "denemo");
}


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
  if(gui==NULL)
    return TRUE;
  if(gui->si==NULL)
    return TRUE;
  static gboolean init = FALSE;
  if(!init) {
    create_tool_pixbuf();
  }
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
  clef* clef;
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
  //gint y;
  gint markx1, markx2;
  gint marky1, marky2;
  gboolean line_end;//set true when an object is drawn off the right hand edge

  measurenode *curmeasure;
  GList *mwidthiterator;
  GSList *slur_stack;
  GSList *hairpin_stack;
  GdkGC *gc;
  GtkWidget * verse;
  gint space_above;
  gint highy;/*(return) the highest y value drawn*/
  gint lowy;/*(return) the lowest y value drawn*/
  gint in_highy; // FIXME these are passed in so that highy, lowy do not need to be passed back
  gint in_lowy;
  gboolean mark;//whether the region is selected
  gint *left, *right;//location of right and left measurenum for current system(line)
  GList *last_midi;//last list of midi events for object at right of window
  DenemoObject *startobj;//pointer values - if drawing such an object mark as playback start
  DenemoObject *endobj;//pointer values - if drawing such an object mark as playback end
  gint startposition;//x coordinate where to start playing
  gint endposition;//x coordinate where to end playing
  gint playposition;//x coordinate of currently played music
};

/* count the number of syllables up to staff->leftmeasurenum */
static gint count_syllables(DenemoStaff *staff, gint from) {
  gint count = 0;
  gint i;
  GList *curmeasure = staff->measures;
  gboolean in_slur = FALSE;
  for(i=1;curmeasure && (i<from);i++, curmeasure = curmeasure->next) {
    objnode *curobj;
    for(curobj = curmeasure->data;curobj;curobj=curobj->next) {
      DenemoObject *obj = curobj->data;
      
      if(obj->type==CHORD) {
	  chord *thechord = ((chord *) obj->object);
	  if(thechord->notes && !in_slur)
	    count++;
	  if (thechord->slur_begin_p)
	    in_slur = TRUE;
	  if (thechord->slur_end_p)
	    in_slur = FALSE;
	  if (thechord->is_tied)
	    count--;
      }
    }//for objs
  }//for measures
  if(in_slur)
    return -count;
  return count;
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
draw_object (cairo_t *cr, objnode * curobj, gint x, gint y,
	     DenemoGUI * gui, struct infotopass *itp)
{


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

  //FIXME too much save and restore for trifling reasons...
  if(itp->mark) {
    cairo_save(cr);
    cairo_set_source_rgb( cr, 0.5, 0.5, 1.0 );
    cairo_rectangle (cr, x+mudelaitem->x, y, 20, 80 );
    cairo_fill(cr);
    cairo_restore(cr);
  }













  /* The current note, rest, etc. being painted */
  gint extra;
  if(mudelaitem == Denemo.gui->si->playingnow)
    itp->playposition = x + mudelaitem->x;
  
  if(mudelaitem == itp->startobj)
    itp->startposition = x + mudelaitem->x +  mudelaitem->minpixelsalloted;
  if(mudelaitem == itp->endobj)
    itp->endposition = x + mudelaitem->x;

  //g_print("item %p draw at %d\n", mudelaitem, itp->playposition);
  // if (!greengc)
  //   greengc = gcs_greengc ();
  /* Should we set cursor-context info before drawing? */

  /************ FIXME the drawing is side-effecting the DenemoScore si here *******************/
  if (si->currentobject == curobj)
    {
      si->cursorclef = itp->clef->type;
      if (!si->cursor_appending)
	memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
    }

  
  if(mudelaitem->type==CHORD && ((chord *) mudelaitem->object)->tone_node)
    cairo_set_source_rgb( cr, 1.0, 1.0, 0 );//thecolor = &yellow;
  
  if(mudelaitem->midi_events) {
    itp->last_midi = mudelaitem->midi_events;
  }

  switch (mudelaitem->type)
    {
    case CHORD:
      { chord *thechord = ((chord *) mudelaitem->object);
       if (thechord->is_figure && thechord->figure)
      //if (thechord->is_figure)
	//draw_figure (gui->pixmap, itp->gc,
	//	     gtk_style_get_font (itp->widget->style),
	//	     x + mudelaitem->x, y, mudelaitem);
  
	draw_figure ( cr,
		     x + mudelaitem->x,
		     y + (thechord->lowesty / 2),
		     mudelaitem);
  
      else
	{
	  draw_chord ( cr, curobj, x + mudelaitem->x, y,
		      GPOINTER_TO_INT (itp->mwidthiterator->data),
		      itp->curaccs, itp->mark);
	  if((thechord->highesty) < itp->highy)
 	    itp->highy  = thechord->highesty/*, g_print("setting highy %d\n", itp->highy)*/;

	  if((thechord->lowesty) > itp->lowy+STAFF_HEIGHT)
	    itp->lowy  = thechord->lowesty-STAFF_HEIGHT;




	}
       if (thechord->is_fakechord)
	draw_fakechord (cr,
		     x + mudelaitem->x, 
		     y - 45,
		     //y - (thechord->highesty ), 
		     mudelaitem);
       else
	{
	  draw_chord ( cr, curobj, x + mudelaitem->x, y,
		      GPOINTER_TO_INT (itp->mwidthiterator->data),
		      itp->curaccs, itp->mark);
	}

       if (si->currentstaffnum==itp->staffnum 
	   && itp->verse && thechord->notes   
	   && !itp->slur_stack
	   && !thechord->is_tied)
	{
	  gchar *syllable = (gchar *) next_syllable(0);
	  if(syllable)
	    draw_lyric (cr,
			x + mudelaitem->x,
			y + itp->in_lowy,
			syllable);
	}

      if (thechord->dynamics)
	draw_dynamic (cr,
		      x + mudelaitem->x, y, mudelaitem);

      if (thechord->slur_end_p)
	draw_slur (cr, &(itp->slur_stack),
		   x + mudelaitem->x, y);
      if (thechord->slur_begin_p)
	itp->slur_stack =
	  push_slur_stack (itp->slur_stack, x + mudelaitem->x);

      if (thechord->crescendo_begin_p)
	itp->hairpin_stack =
	  push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
      else if (thechord->diminuendo_begin_p)
	itp->hairpin_stack =
	  push_hairpin_stack (itp->hairpin_stack, x + mudelaitem->x);
      if (thechord->crescendo_end_p)
	{
	  if (top_hairpin_stack (itp->hairpin_stack) <= -1)
	    {
#if 0
	      //this is only the visible part of the cresc, the start may be off screen
	      thechord->crescendo_end_p = FALSE;
	      warningdialog
		("Crescendo end without a corresponding start\n"
		 "removing the crescendo end");
#endif
	    }
	  draw_hairpin (cr, &(itp->hairpin_stack),
			x + mudelaitem->x, y, 1);
	}
      else if (thechord->diminuendo_end_p)
	{
	  if (top_hairpin_stack (itp->hairpin_stack) <= -1)
	    {
#if 0
	      //this is only the visible part of the dim, the start may be off screen
	      thechord->diminuendo_end_p = FALSE;
	      warningdialog
		("Diminuendo end without a corresponding start\n"
		 "removing the diminuendo end");
#endif
	    }
	  draw_hairpin (cr, &(itp->hairpin_stack),
			x + mudelaitem->x, y, 0);
	}
	/* notice the following does not check is_figure but checks if figure is not VOID) */      
      if (!thechord->is_figure && thechord->figure)
      //if (thechord->figure)
        draw_figure (cr,
		     x + mudelaitem->x,
		     y + (thechord->lowesty / 2),
		     mudelaitem);
      if (!thechord->is_fakechord && thechord->fakechord) 
	draw_fakechord (cr,
		     x + mudelaitem->x,
		     y - 45,
		     //y + (thechord->highesty / 4),
		     mudelaitem);
      }
      break;
    case TUPOPEN:
    case TUPCLOSE:
      draw_tupbracket (cr,
		       x + mudelaitem->x, y, mudelaitem);
      break;
    case LILYDIRECTIVE:
      // if(si->markstaffnum) not available
      draw_lily_dir(cr,
		       x + mudelaitem->x, y, itp->in_highy, itp->in_lowy, mudelaitem, itp->mark);  
      break;
    case CLEF:
      draw_clef (cr, x + mudelaitem->x, y,
		 itp->clef = ((clef *) mudelaitem->object));
      if (si->currentobject == curobj && si->cursor_appending)
	si->cursorclef = itp->clef->type;//FIXME drawing is side-effecting the data, presumably to economize on searching for the prevailing clef at the cursor.
      break;
    case KEYSIG:
      draw_key (cr, x + mudelaitem->x, y,
		((keysig *) mudelaitem->object)->number, itp->key,
		itp->clef->type, TRUE);
      itp->key = ((keysig *) mudelaitem->object)->number;
      memcpy (itp->keyaccs, ((keysig *) mudelaitem->object)->accs,
	      SEVENGINTS);
      memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);
      if (si->currentmeasure == itp->curmeasure)
	/* We're in the current measure */
	memcpy (si->nextmeasureaccs, itp->keyaccs, SEVENGINTS);
      break;
    case TIMESIG:
      draw_timesig (cr,
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
      draw_stem_directive (cr,
			   x + mudelaitem->x, y, mudelaitem);
      itp->stem_directive = ((stemdirective *) mudelaitem->object)->type;
      break;
    case GRACE_START:
    case GRACE_END:
      draw_gracebracket (cr,
			 x + mudelaitem->x, y, mudelaitem);
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
	  draw_cursor (cr, si, x + mudelaitem->x + extra, y,
		       gui->mode, si->cursorclef);
	  memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
	}
      else
	{
	  draw_cursor (cr, si, x + mudelaitem->x, y, gui->mode,
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

  //gdk_gc_set_foreground (blackgc, &black);

  //  g_print("obj at %d %d\n",  x + mudelaitem->x + mudelaitem->minpixelsalloted, (int)(gui->scorearea->allocation.width/gui->si->zoom - (RIGHT_MARGIN + KEY_MARGIN + si->maxkeywidth + SPACE_FOR_TIME)));
  //  itp->line_end = itp->markx2 > (int)(gui->scorearea->allocation.width/gui->si->zoom - (RIGHT_MARGIN + KEY_MARGIN + si->maxkeywidth + SPACE_FOR_TIME));



  //g_print("returning with %d\n", itp->highy);
  /* And give a return value and we're done */
  return (mudelaitem->starttickofnextnote - itp->tickspermeasure);
} /* draw_object */

/**
 * Draws a single measure within a staff. 
 * @param curmeasure pointer to the measure to draw
 * @param x
 * @param y
 * @param gui
 * @param itp
 * return TRUE if measure has correct number of beats
 */
static gboolean
draw_measure (cairo_t *cr, measurenode * curmeasure, gint x, gint y,
	      DenemoGUI * gui, struct infotopass *itp)
{
  static GString *mstring;
  gint extra_ticks = 0;//number of ticks by which measure is over-full
  DenemoScore *si = gui->si;
  objnode *curobj;
  /* initialization */
  if (!mstring)
    mstring = g_string_new (NULL);
  /* Set information about the state at the current measure,
     if necessary */

  memcpy (itp->curaccs, itp->keyaccs, SEVENGINTS);
  itp->wholenotewidth = si->measurewidth * itp->time2 / itp->time1;
  if (curmeasure == si->currentmeasure)
    {
      si->curmeasureclef = itp->clef->type;
      memcpy (si->curmeasureaccs, itp->keyaccs, SEVENGINTS);
      memcpy (si->nextmeasureaccs, itp->keyaccs, SEVENGINTS);
      si->curmeasurekey = itp->key;
      si->curmeasure_stem_directive = itp->stem_directive;
      si->cursortime1 = itp->time1;
      si->cursortime2 = itp->time2;
    }

  /*  paint the measure number at the preceding barline 
      - do not do measure 1 as it clashes with the name (and is not needed) */

  if(itp->measurenum>1) {
    g_string_sprintf (mstring, "%d", itp->measurenum);
    drawnormaltext_cr (cr, mstring->str, x - SPACE_FOR_BARLINE, y - 12);
  }


  // draw the cursor and set the side effects up if this didn't happen when drawing the currentobject
  if (!si->currentobject && (si->currentstaffnum == itp->staffnum && si->currentmeasurenum == itp->measurenum))
    {
      /* That is, the cursor's at the beginning of this blank measure */
      si->cursoroffend = FALSE; 
      draw_cursor (cr, si, x, y, gui->mode, itp->clef->type);
      memcpy (si->cursoraccs, itp->curaccs, SEVENGINTS);
      si->cursorclef = itp->clef->type;     
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


    
    if(itp->measurenum >= si->rightmeasurenum+1)
      cairo_set_source_rgb( cr, 0.5,0.5,0.5 );//This draws light gray anything that may be only partially visible.
    else 
      //  if(itp->mark)
      //	cairo_set_source_rgb( cr, 0, 0, 1.0 );//blue
      //  else
	cairo_set_source_rgb( cr, 0, 0, 0 );//black;
    extra_ticks = draw_object (cr, curobj, x, y, gui, itp);    
  }
  /* Paint the exclamation point, if necessary */
  cairo_save(cr);
  if( extra_ticks > 0 )
    cairo_set_source_rgb( cr, 1.0, 0, 0 );
  else
    cairo_set_source_rgb( cr, 0, 0, 1 );
  if(extra_ticks != 0) {
    drawlargetext_cr( cr, "!", x, y - 8 );
    //cairo_set_source_rgb( cr, 0.5, 0.5, 0.5 );
  } else
    cairo_set_source_rgb( cr, 0, 0, 0 );
  //draw the barline
#if 0
  //This barline changes appearance depending on where the cursor is
  cairo_move_to (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y+STAFF_HEIGHT);
  cairo_line_to (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y);
  cairo_stroke (cr);
#else
  cairo_rectangle (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y-0.5, 1.5, STAFF_HEIGHT+1);
  cairo_fill(cr);
#endif

  if (!curmeasure->next)
    {
      /* we've reached the end of the score and should
       * draw the heavy part of double-barline at regular position */
      x += 3;
      cairo_rectangle (cr, x + GPOINTER_TO_INT (itp->mwidthiterator->data), y-0.5, 4, STAFF_HEIGHT+1);
      cairo_fill(cr);
    }	
  cairo_restore(cr);


  return extra_ticks!=0;
}

/**
 * Draws a single staff 
 * TODO sort out graphics context for active polyphonic voice should 
 * do it here
 * @param curstaffstruct pointer to the staff to draw
 * @param y    y position of the staff
 * @param gui   pointer to the DenemoGUI structure
 * @param itp  pointer to the infotopass structure
 * return TRUE if the staff has had to made taller
 */
static gboolean
draw_staff (cairo_t *cr, staffnode * curstaff, gint y,
	    DenemoGUI * gui, struct infotopass *itp)
{
  DenemoStaff *thestaff = (DenemoStaff*)curstaff->data;
  gboolean repeat = FALSE;
  DenemoScore *si = gui->si;
  gint x  = KEY_MARGIN, i;
  //g_print("drawing staff %d at %d\n", itp->staffnum, y);
  cairo_save(cr);
  //Draw vertical lines to bind the staffs of the system together
      if(curstaff->prev)
	{
	  DenemoStaff *prev = (DenemoStaff *)(curstaff->prev->data);	  
	  cairo_set_source_rgb( cr, 0, 0, 0);
	  cairo_rectangle (cr, LEFT_MARGIN, y - STAFF_HEIGHT - prev->space_below - thestaff->space_above, 2, 2*STAFF_HEIGHT + prev->space_below + thestaff->space_above);
	  cairo_fill(cr);	 
	}
      if(curstaff->next)
	{
	  DenemoStaff *next = (DenemoStaff *)(curstaff->next->data);
	  cairo_save(cr);
	  cairo_set_source_rgb( cr, 0, 0, 0);
	  cairo_rectangle (cr, LEFT_MARGIN, y, 2, 2*STAFF_HEIGHT + next->space_above + thestaff->space_below);
	  cairo_fill(cr);	 
	}

  if ((DenemoStaff *) si->currentstaff->data == thestaff)
    cairo_set_source_rgb( cr, 0,0,0 );
  else
    cairo_set_source_rgb( cr, 0.3,0.3,0.3 );


  if(!itp->line_end) {//not a continuation
    draw_clef (cr, LEFT_MARGIN, y,
	       itp->clef = thestaff->leftmost_clefcontext);
    draw_key (cr, x, y,
	      itp->key = thestaff->leftmost_keysig->number,
	      0, itp->clef->type, TRUE);
    x += si->maxkeywidth;
    draw_timesig (cr, x,
		  y, itp->time1 =
		  thestaff->leftmost_timesig->time1, itp->time2 =
		  thestaff->leftmost_timesig->time2);
    x += SPACE_FOR_TIME;

  } else {
    draw_clef (cr, LEFT_MARGIN, y, itp->clef);
    draw_key (cr, x, y,
	      itp->key, 0, itp->clef->type, TRUE);
    x += si->maxkeywidth;
    x += SPACE_FOR_TIME;// to allow the same margin ??
  }

  *itp->left = itp->measurenum;
  memcpy (itp->keyaccs, thestaff->leftmost_keysig->accs, SEVENGINTS);


  itp->stem_directive = thestaff->leftmost_stem_directive;
  itp->tickspermeasure = WHOLE_NUMTICKS * itp->time1 / itp->time2;


  /* Draw staff name on first system */
  if(!itp->line_end) {
    gint staffname_offset = (thestaff->voicenumber == 1) ? 24 :
      (thestaff->voicenumber == 2
       || thestaff->voicenumber == 3) ? 12 : 0;
    drawnormaltext_cr( cr, thestaff->denemo_name->str, KEY_MARGIN, y - staffname_offset+10 );
  } else {
 cairo_save(cr);
   gint staffname_offset = (thestaff->voicenumber == 1) ? 24 :
      (thestaff->voicenumber == 2
       || thestaff->voicenumber == 3) ? 12 : 0;
   cairo_translate(cr, 2, (y - staffname_offset+30) );
   cairo_rotate( cr,- M_PI/5.0 );
    drawnormaltext_cr( cr, thestaff->denemo_name->str, 0,0 );
 cairo_restore(cr);
  }

  cairo_save(cr);
  /* Loop that will draw each measure. Basically a for loop, but was uglier
   * when written that way.  */

  itp->curmeasure =
    g_list_nth (thestaff->measures, itp->measurenum - 1);
  // g_print("measurenum %d\nx=%d\n", itp->measurenum, x);

  //FIX in measureops.c for case where si->measurewidths is too short
  itp->mwidthiterator = g_list_nth (si->measurewidths, itp->measurenum - 1);

  // g_print("Width is %d\n", itp->mwidthiterator->data);

  //itp->gc = gc;
  itp->line_end = FALSE;
  while ( (!itp->line_end)  //       itp->measurenum <= si->rightmeasurenum+1
	 && itp->measurenum <= g_list_length (thestaff->measures))
    {
      


      if( x + GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE >
	  (int) (gui->scorearea->allocation.width/gui->si->zoom - (RIGHT_MARGIN + KEY_MARGIN + si->maxkeywidth + SPACE_FOR_TIME)))
	if(itp->curmeasure->next) {
	  itp->line_end=TRUE;
	  //continue; continue is not good here, you cannot see things that Denemo thinks you can on the right.
	}
      
      draw_measure (cr, itp->curmeasure, x, y, gui, itp);

      x += GPOINTER_TO_INT (itp->mwidthiterator->data) + SPACE_FOR_BARLINE;
      itp->curmeasure = itp->curmeasure->next;
      itp->mwidthiterator = itp->mwidthiterator->next;
      itp->measurenum++;
      //g_print("line_end is %d, while itp->measurenum=%d and si->rightmeasurenum=%d\n",  itp->line_end, itp->measurenum, si->rightmeasurenum);
      if(!itp->line_end) {
	if(-itp->highy>itp->in_highy && -itp->highy<MAXEXTRASPACE){
	  thestaff->space_above = -itp->highy;
	  repeat = TRUE;
	}
	if(itp->lowy>itp->in_lowy && itp->lowy<MAXEXTRASPACE){
	  thestaff->space_below = itp->lowy;
	  repeat=TRUE;
	}
      }			      
    }

  *itp->right = itp->measurenum-1;
  
      if(curstaff->prev)
	{
	  DenemoStaff *prev = (DenemoStaff *)(curstaff->prev->data);	  
	  cairo_set_source_rgb( cr, 0, 0, 0);
	  cairo_rectangle (cr, x - SPACE_FOR_BARLINE, y - STAFF_HEIGHT - prev->space_below - thestaff->space_above, 2, 2*STAFF_HEIGHT + prev->space_below + thestaff->space_above);
	  cairo_fill(cr);	 
	}
      if(curstaff->next)
	{
	  DenemoStaff *next = (DenemoStaff *)(curstaff->next->data);
	  cairo_save(cr);
	  cairo_set_source_rgb( cr, 0, 0, 0);
	  cairo_rectangle (cr, x - SPACE_FOR_BARLINE, y, 2, 2*STAFF_HEIGHT + next->space_above + thestaff->space_below);
	  cairo_fill(cr);	 
	}


  cairo_restore(cr);
  // if(itp->highy > title_highy)
  //  itp->highy = title_highy;

  /* now draw the staff lines, reset itp->slur_stack, and we're done */
  for (i = 0; i < thestaff->no_of_lines; i++, y += LINE_SPACE) {
    cairo_set_line_width( cr, 1.0 );
    cairo_move_to( cr, LEFT_MARGIN, y );
    cairo_line_to( cr, x - HALF_BARLINE_SPACE, y );
    cairo_stroke( cr );
  }

 

  /* Initialize the slur_stack for this staff. For the time being,
     slurs that begin and/or end after the portion of the music
     that is shown are not drawn. */
  if (itp->slur_stack)
    {
      g_slist_free (itp->slur_stack);
      itp->slur_stack = NULL;
    }
  cairo_restore(cr);
  return repeat;
}
static void
print_system_separator (cairo_t *cr, gdouble position){
  //g_print("At %f for %d\n", position, Denemo.gui->scorearea->allocation.height);
#define SYSTEM_SEP (6)
  cairo_save(cr);
  cairo_set_source_rgb( cr, 0.5, 0.0, 0.0 );
  cairo_rectangle (cr, 0, position-SYSTEM_SEP/2, Denemo.gui->scorearea->allocation.width/Denemo.gui->si->zoom, SYSTEM_SEP);
  cairo_set_source_rgb( cr, 0.7, 0.0, 0.0 );
  cairo_fill(cr);
#undef SYSTEM_SEP
  cairo_restore(cr);
}

typedef enum colors {BLACK, RED, GREEN} colors;
static void draw_playback_marker(cairo_t *cr, gint color, gint pos, gint yy, gint line_height) {
  //g_print("drawing marker %x at %d %d %d\n", color, pos, yy, line_height);
  cairo_save(cr);
  cairo_set_line_width( cr, 4.0 );
  switch(color) {
  case BLACK:
    cairo_set_source_rgb( cr, 0.0, 0.0, 0.0 );
    break;
  case GREEN:
    cairo_set_source_rgb( cr, 0.0, 1.0, 0.0 );
    break;
  case RED:
    cairo_set_source_rgb( cr, 1.0, 0.0, 0.0 );
    break;
  }
  cairo_move_to( cr, pos, yy-STAFF_HEIGHT );
  cairo_line_to( cr, pos, yy-STAFF_HEIGHT+line_height);
  cairo_stroke( cr );
  cairo_restore(cr);

}

static void draw_playback_markers(cairo_t *cr, struct infotopass *itp, gint yy, gint line_height) {

  if(itp->playposition>-1)
    draw_playback_marker(cr, BLACK, itp->playposition, yy, line_height);
  itp->playposition = -1;
  
  if(itp->startposition>0)
    draw_playback_marker(cr, GREEN, itp->startposition, yy, line_height);
  itp->startposition = -1;
  
  if(itp->endposition>0)
    draw_playback_marker(cr, RED, itp->endposition, yy, line_height);
  itp->endposition = -1;
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
  gint y=0;
  struct infotopass itp;
  gboolean repeat = FALSE;
  DenemoScore *si = gui->si;
  gint line_height = gui->scorearea->allocation.height*gui->si->system_height/gui->si->zoom;

  /* Initialize some fields in itp */


  itp.slur_stack = NULL;
  itp.hairpin_stack = NULL;

  itp.highy = 0;//in case there are no objects...
  itp.lowy = 0;
  itp.last_midi = NULL;
  itp.playposition = -1;
  itp.startposition = -1;
  itp.endposition = -1;
  itp.startobj =  itp.endobj = NULL;
  y = 0;

  if(gui->si->smf) {
    itp.startobj =
      get_obj_for_start_time(gui->si->smf, gui->si->start_time);
    itp.endobj =
      get_obj_for_end_time(gui->si->smf, gui->si->end_time);
    //g_print("Start time %p %f end time %p %f\n", itp.startobj, si->start_time, itp.endobj, si->end_time);
  }
  cairo_t *cr = gdk_cairo_create( gui->pixmap );

  cairo_scale( cr, gui->si->zoom, gui->si->zoom );
  cairo_translate( cr, 0.5, 0.5 );
  //cairo_rotate( cr, M_PI/6.0 );

  /* Draw each staff */
  for (itp.staffnum = si->top_staff,
	 curstaff = g_list_nth (si->thescore, si->top_staff - 1),
	 (y += si->staffspace / 4);
       curstaff && itp.staffnum <= si->bottom_staff; 
       itp.staffnum++) {
    DenemoStaff *staff = (DenemoStaff *) curstaff->data;
    itp.verse = staff->currentverse?staff->currentverse->data:NULL;
    GdkPixbuf *StaffDirectivesPixbuf = (si->currentstaffnum==itp.staffnum)?StaffPixbuf:StaffPixbufSmall;
      

    if (curstaff && staff->voicenumber == 1)
      y += staff->space_above;

    //g_print("Incrementing vertically %d\n", y);
    itp.space_above = staff->space_above;
    gint top_y = (si->staffspace / 4) + itp.space_above;
    
    itp.top_y = top_y;
    //itp.y = y;
    gint highy = staff->space_above;
    gint lowy =  staff->space_below;

    itp.in_highy = highy, itp.in_lowy = lowy;
    itp.highy = 0;//do not pass on extra_space from one staff to the next

    cairo_save(cr);
    cairo_set_source_rgb( cr, 0.5, 0.5, 1.0 );
    cairo_rectangle (cr, 0, y, LEFT_MARGIN, STAFF_HEIGHT/*staff edit*/);
    cairo_fill(cr);
    cairo_restore(cr);

      if(staff->staff_directives){

	guint width = gdk_pixbuf_get_width( GDK_PIXBUF(StaffDirectivesPixbuf));
	guint height = gdk_pixbuf_get_height( GDK_PIXBUF(StaffDirectivesPixbuf));
	cairo_save( cr );
	gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(StaffDirectivesPixbuf), 0,y );
	cairo_rectangle( cr,0,y, width, height );
	cairo_fill( cr );
	cairo_restore( cr );
	//gdk_draw_pixbuf(gui->pixmap, NULL, StaffDirectivesPixbuf,  0,0, 0,y, width, height, GDK_RGB_DITHER_NONE,0,0/*staff edit*/);
      }
      if(staff->voice_directives) {

	guint width = gdk_pixbuf_get_width( GDK_PIXBUF(StaffDirectivesPixbuf));
	guint height = gdk_pixbuf_get_height( GDK_PIXBUF(StaffDirectivesPixbuf));
	cairo_save( cr );
	gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(StaffDirectivesPixbuf), 0,y+STAFF_HEIGHT/2 );
	cairo_rectangle( cr,0,y+STAFF_HEIGHT/2, width, height );
	cairo_fill( cr );
	cairo_restore( cr );
	//gdk_draw_pixbuf(gui->pixmap, NULL, StaffDirectivesPixbuf,  0,0, 0,y + STAFF_HEIGHT/2, width, height, GDK_RGB_DITHER_NONE,0,0/*staff edit*/);
      }
    
    


    if(si->leftmeasurenum==1) {
      /* draw background of clef, keysig, timesig */
      gint key = gui->si->maxkeywidth;
      gint cmajor = key?0:5;//allow some area for keysig in C-major
      cairo_save(cr);

      cairo_set_source_rgb( cr, 0.7, 0.7, 0.7 );
      cairo_rectangle (cr, LEFT_MARGIN,y,KEY_MARGIN-LEFT_MARGIN - cmajor,STAFF_HEIGHT);/*clef edit*/
      cairo_rectangle (cr, KEY_MARGIN+key+cmajor,y,SPACE_FOR_TIME-cmajor,STAFF_HEIGHT);/*timesig edit*/
      cairo_fill(cr);

      cairo_set_source_rgb( cr, 0.7, 0.7, 1 );
      cairo_rectangle (cr, KEY_MARGIN-cmajor,y,key+2*cmajor,STAFF_HEIGHT);/*keysig edit*/
      cairo_fill(cr);

      cairo_restore(cr);
    }
    if(si->currentstaffnum==itp.staffnum) {

      gint count =  count_syllables(staff, si->leftmeasurenum);
      if(count<0) {
	count = -count;
	itp.slur_stack =
	  push_slur_stack (itp.slur_stack, 0);
      }
      reset_lyrics(staff, count);
    }
    
  
    itp.measurenum = si->leftmeasurenum;
    itp.line_end = FALSE;
    itp.left = &gui->lefts[0];
    itp.right = &gui->rights[0];


    repeat = draw_staff (cr, curstaff, y, gui, &itp);

    draw_playback_markers(cr, &itp, y, line_height);
    
    gint system_num;
    system_num = 1;
    // g_print("Drawn staffnum %d, at %d %s.\n", itp.staffnum,  y, itp.line_end?" another line":"End");

    if (itp.staffnum==si->top_staff)
      print_system_separator (cr, line_height*system_num++);


    // This block prints out continuations of the staff just printed
    // we are going to have to store an x-count for each continuation so that mousing.c will know
    // the correct value. LEFT_MARGIN only works for all same value notes...
    {
    int yy;
    yy = y + line_height;
    itp.left++;
    itp.right++;


    while(((itp.left-gui->lefts)<DENEMO_MAX_SYSTEMS-1) && itp.line_end && (yy<(gui->scorearea->allocation.height/gui->si->zoom))) {
      if (itp.staffnum==si->top_staff)
	print_system_separator (cr, line_height*system_num++);

      if(draw_staff (cr, curstaff, yy, gui, &itp))
	repeat = TRUE;
      draw_playback_markers(cr, &itp, yy, line_height);

      // g_print("Drawn successively staffnum %d, at %d %s. Aloc %d,%d yy now %d line height %d\n", itp.staffnum,  yy, itp.line_end?" another line":"End", gui->scorearea->allocation.width, gui->scorearea->allocation.height, yy, line_height);
     
      yy += line_height;
      itp.left++;
      itp.right++;
    }
    }//end of block printing continuations
    *itp.left=0;//To signal end of valid systems

    if ( (!curstaff->next)
	 ||    ((DenemoStaff *) curstaff->next->data)->voicenumber !=2)
      {
	if (itp.verse) {
	  y += LYRICS_HEIGHT;
	}
	y +=
	  (si->staffspace + staff->space_below);
      }
    curstaff = curstaff->next;
  }// for all the staffs


  if(itp.last_midi)
    si->rightmost_time = get_midi_off_time(itp.last_midi);

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
 if((!Denemo.gui->si)||(!Denemo.gui->si->currentmeasure)){
   g_warning("Cannot draw!\n");
   return TRUE;
 }

 do{
   /* Clear the backing pixmap */
   if(Denemo.gui->input_source!=INPUTKEYBOARD && Denemo.gui->input_source!=INPUTMIDI &&
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


