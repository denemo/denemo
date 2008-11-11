/* commandfuncs.c
 * functions invoked by user keypresses in score area
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */

#include <string.h>
#include "commandfuncs.h"
#include "calculatepositions.h"
#include "chordops.h"
#include "contexts.h"
#include "draw.h"
#include "measureops.h"
#include "midi.h"
#include "objops.h"
#include "moveviewport.h"
#include "selectops.h"
#include "staffops.h"
#include "utils.h"
#include "tupletops.h"
#include "graceops.h"
#include "instrumentname.h"
#include "file.h"
#include "exportlilypond.h"
#include "exportxml.h"
#include "prefops.h"
#include "keyresponses.h"
#include "view.h"
/**
 * Macro to get the current DenemoObject
 */
#define declarecurmudelaobj \
DenemoObject *curmudelaobj = \
       (DenemoObject *) ( (si->currentobject && si->currentobject->data) ? \
       ((((DenemoObject *)si->currentobject->data)->type == TUPCLOSE) ? \
       (si->currentobject->prev?si->currentobject->prev->data:si->currentobject->data) : si->currentobject->data) : NULL)


/**
 * Move the current rhythm on to the next one
 * FIXME re-arrange the list of rhythms each time as well so that it
 * is easy to alternate rhythms.
 */
void nextrhythm(DenemoGUI *gui) {
  if(!gui->rhythms)
    return;
  if(gui->currhythm==NULL)
    gui->currhythm = g_list_last(gui->rhythms);
#if 0
  gtk_widget_set_state(((RhythmPattern *)gui->currhythm->data)->button, GTK_STATE_NORMAL);
#else

  {GdkColor thecolor;
    gdk_color_parse ("gray", &thecolor);
    gtk_widget_modify_bg (gtk_tool_button_get_label_widget(GTK_TOOL_BUTTON(((RhythmPattern *)gui->currhythm->data)->button)), GTK_STATE_NORMAL, &thecolor);
  }

#endif
  if(gui->currhythm->next)
    gui->currhythm = gui->currhythm->next;
  else
    gui->currhythm = gui->rhythms;
#define g  (gui->rstep)

  g = ((RhythmPattern*)gui->currhythm->data)->rsteps;


#define CURRP ((RhythmPattern *)gui->currhythm->data)  
  if(((RhythmElement*)g->data)->icon) {
    GtkWidget *label = LABEL(CURRP->button);
    //g_print("markup is %s\n", ((RhythmElement*)g->data)->icon);
    gtk_label_set_markup(GTK_LABEL(label),((RhythmElement*)g->data)->icon);
  }
#if 0
  gtk_widget_set_state(GTK_WIDGET(((RhythmPattern *)gui->currhythm->data)->button), GTK_STATE_PRELIGHT);
#else
  highlight_rhythm((RhythmPattern *)gui->currhythm->data);
#endif
  //g_print("selected active\n");
#undef CURRP
#undef g
}




/**
 * Helper function for calculating the 
 * beam and stem direction
 * I think it calculates the beam and stem directions for the si->currentmeasure
 * on the assumption that si->curmeasureclef has been set to the appropriate value
 * before the function is called. Likewise for 
 * si->cursortime1/2 and si->curmeasure_stem_directive.
 * the calculated values are stored in the objects in the measure - the fields set are:
 * isstart/end_beamgroup, is_stemup, stemy, is_reversealigned. reversealign, minpixelsalloted, space_before
 * si->curmeasureclef is also set: it is set to the value prevailing at the end of the measure.
 */
void
beamandstemdirhelper (DenemoScore * si)
{
  calculatebeamsandstemdirs
    ((objnode *) si->currentmeasure->data, &(si->curmeasureclef),
     &(si->cursortime1), &(si->cursortime2),
     &(si->curmeasure_stem_directive));
}


/**
 * Set si->current* variables from currentmeasurenum
 * the current object is set to the first in the measure
 * the selection is updated
 *
 */
void
setcurrents (DenemoScore * si)
{
  if (((DenemoStaff *) si->currentstaff->data)->nummeasures >=
      si->currentmeasurenum)
    {
      si->currentmeasure =
	g_list_nth (firstmeasurenode (si->currentstaff),
		    si->currentmeasurenum - 1);
    }
  else
    {
#ifdef DEBUG
      g_print ("Setting measure to %d which is last in Staff\n",
	       ((DenemoStaff *) si->currentstaff->data)->nummeasures);
#endif
      si->currentmeasure =
	g_list_nth (firstmeasurenode (si->currentstaff),
		    ((DenemoStaff *) si->currentstaff->data)->nummeasures -
		    1);
      si->currentmeasurenum =
	((DenemoStaff *) si->currentstaff->data)->nummeasures;

    }

  si->cursor_x = 0;
  si->currentobject = (objnode *) si->currentmeasure->data;
  if (si->currentobject)
    si->cursor_appending = FALSE;
  else
    si->cursor_appending = TRUE;
  calcmarkboundaries (si);
}

/**
 * Push the score to the left off the 
 * displayed portion
 */
void
nudgerightward (DenemoGUI * gui)
{
  set_rightmeasurenum (gui->si);
  while (gui->si->currentmeasurenum > gui->si->rightmeasurenum)
    {
      gui->si->leftmeasurenum++;
      set_rightmeasurenum (gui->si);
    }
  find_leftmost_allcontexts (gui->si);
  update_hscrollbar (gui);
}

/**
 * Push the score upwards off the displayed
 * portion
 */
void
nudge_downward (DenemoGUI * gui)
{
  set_bottom_staff (gui);

  while (gui->si->currentstaffnum > gui->si->bottom_staff)
    {
      gui->si->top_staff++;
      set_bottom_staff (gui);
    }
  update_vscrollbar (gui);
}

/**
 * Set the width available for drawing the measures of the movements from the width of the working scorearea widget
 */
void
set_width_to_work_with (DenemoGUI * gui)
{
  GList *g; 
  for(g = gui->movements;g;g=g->next)
 ((DenemoScore*) g->data)->widthtoworkwith
    = (gui->scorearea->allocation.width
       - (RIGHT_MARGIN + KEY_MARGIN + gui->si->maxkeywidth + SPACE_FOR_TIME));
}

/**
 * Change the basic width of each measure
 */
void
adjustmeasurewidth (DenemoScore * si, gint amount)
{
  si->measurewidth += amount;
  if (si->measurewidth < 10)
    si->measurewidth = 10;
  if (si->widthtoworkwith < si->measurewidth + SPACE_FOR_BARLINE)
    si->measurewidth = si->widthtoworkwith - SPACE_FOR_BARLINE;
  find_xes_in_all_measures (si);
  /*nudgerightward (si); */
}

/**
 * Determines whether the closer jump will be up or down 
 */
gint
jumpcursor (gint cursor_y, gint fromnote, gint tonote)
{
  int distance;

  distance = (tonote - fromnote + 7) % 7;
  if (distance <= 3)		/* an upward jump is good */
    return cursor_y + distance;
  else				/* jump down */
    return cursor_y - 7 + distance;
}

/**
 *  General function for inserting a DenemoObject
 *  into the score
 */
void
object_insert (DenemoGUI * gui, DenemoObject * mudela_obj_new)
{
  DenemoScore *si = gui->si;
  unre_data *undo = (unre_data *) g_malloc (sizeof (unre_data));

  declarecurmudelaobj;

  /* First, check to see if the operation would add something before an
   * indicator of a time signature change. This would be bad, so don't
   * allow it to happen */
  if (curmudelaobj && curmudelaobj->type == TIMESIG && !si->cursor_appending)
    {
      si->cursor_x++;
      if (si->currentobject->next)
	si->currentobject = si->currentobject->next;
      else
	si->cursor_appending = TRUE;
    }

  si->currentmeasure->data =
    g_list_insert ((objnode *) si->currentmeasure->data,
		   mudela_obj_new, si->cursor_x);

  /* update undo information */
  undo->position = si->cursor_x;
  undo->measurenum = si->currentmeasurenum;
  undo->staffnum = si->currentstaffnum;
  undo->object = dnm_clone_object (mudela_obj_new);
  undo->action = ACTION_INSERT;


  si->cursor_x++;
  if (si->cursor_appending)
    si->currentobject = g_list_last ((objnode *) si->currentmeasure->data);
  else
    si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				    si->cursor_x);

  update_undo_info (si, undo);

  score_status(gui, TRUE);
  si->markstaffnum = 0;
}

/**
 *  Change the y position of each staff
 */
void
adjuststaffheight (DenemoScore * si, gint amount)
{
  si->staffspace += amount;
  if (si->staffspace < 2 * STAFF_HEIGHT)
    si->staffspace = 2 * STAFF_HEIGHT;
  /*nudge_downward (si); */
}

/**
 * Move an entire measure to the left
 *
 */
void
measureleft (DenemoGUI * gui)
{
  if (!gui->si->cursor_x && gui->si->currentmeasure->prev)
    {
      gui->si->currentmeasurenum--;
      isoffleftside (gui);
    }
  setcurrents (gui->si);
}

/**
 * Move an entire measure to the right
 *
 */
void
measureright (DenemoGUI * gui)
{
  if (gui->si->currentmeasure->next)
    {
      gui->si->currentmeasurenum++;
      isoffrightside (gui);
      setcurrents (gui->si);
    }
}


/**
 * swap the current movement for the previous one in the list of movements
 *
 */
gboolean
swapmovements (GtkAction *action, gpointer param)
{
  DenemoGUI  *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  GList *this = g_list_find( gui->movements, gui->si);
  if(this->prev) {
    GList * prev = this->prev;
    GList * prevv = prev->prev;
    GList * next = this->next;
    if(next)
      next->prev = prev;
    if(prevv)
      prevv->next = this;
    else
      gui->movements = this;
    this->next = prev;
    this->prev = prevv;
    prev->next = next;
    prev->prev = this;
    gchar *str = g_strdup_printf("This movement is now number %d in the score", 1+g_list_index(gui->movements, gui->si));
    infodialog(str);
    g_free(str);
  } else
    warningdialog("There is no previous movement to swap with");
}
/**
 * 
 *
 */
gboolean
swapstaffs (GtkAction *action, gpointer param)
{
  DenemoGUI  *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  if (gui->si->currentstaff && gui->si->currentstaff->prev)
    {
      DenemoStaff *temp;
      temp = gui->si->currentstaff->data;
      if(temp->context==DENEMO_NONE ||
	confirm("A context is set on this staff", "You will need to alter the staff->properties->context of this and the previous staff; Proceed?")) {
	  
	  gui->si->currentstaff->data = gui->si->currentstaff->prev->data;
	  gui->si->currentstaff->prev->data = temp;
	  gui->si->currentstaffnum--;
	  gui->si->currentstaff = gui->si->currentstaff->prev;
	  setcurrentprimarystaff (gui->si);
	  setcurrents (gui->si);
	  move_viewport_up (gui);
	  score_status(gui, TRUE);
	  displayhelper(gui);
	  return TRUE;
	}
    }
  else
    warningdialog("There is no previous staff to swap with");
  return FALSE;
}

/**
 * 
 *
 */
gboolean
splitstaffs (GtkAction *action, gpointer param)
{
  DenemoGUI  *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  if (gui->si->currentstaff && gui->si->currentstaff->next)
    {
      DenemoStaff *thestaff = (DenemoStaff *)gui->si->currentstaff->data;
      DenemoStaff *nextstaff = (DenemoStaff *)gui->si->currentstaff->next->data;
      if((thestaff->voicenumber==1) && (nextstaff->voicenumber!=1))
	nextstaff->voicenumber=1;
      else
	warningdialog("There is no voice below this one on this staff");
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_up (gui);
      score_status(gui, TRUE);
      displayhelper(gui);
      return TRUE;
    }
  else
    warningdialog("There is no voice to split from");
  return FALSE;
}


/**
 * 
 *
 */
gboolean
joinstaffs (GtkAction *action, gpointer param)
{
  DenemoGUI  *gui = Denemo.gui;
  if(!confirm_insertstaff_custom_scoreblock(gui))
    return;
  if (gui->si->currentstaff && gui->si->currentstaff->prev)
    {
      DenemoStaff *thestaff = (DenemoStaff *)gui->si->currentstaff->data;
      DenemoStaff *prevstaff = (DenemoStaff *)gui->si->currentstaff->prev->data;
      thestaff->voicenumber=2;
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_up (gui);
      score_status(gui, TRUE);
      displayhelper(gui);
      return TRUE;
    }
  else
    warningdialog("There is no staff above to move this staff into");
  return FALSE;
}


/**
 * Move si->currentstaff up an one voice, return TRUE if successful
 *
 */
gboolean
voiceup (DenemoGUI * gui)
{
  if(!gui->si->currentstaff)
    return FALSE;
  if (gui->si->currentstaff && (((DenemoStaff *)(gui->si->currentstaff->data))->voicenumber==2))  {
      gui->si->currentstaffnum--;
      gui->si->currentstaff = gui->si->currentstaff->prev;
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_down (gui);
      return TRUE;
    } else
      warningdialog("This is the first voice");
  return FALSE;
}

/**
 * Move si->currentstaff up an one staff/voice, return TRUE if successful
 *
 */
gboolean
staffup (DenemoGUI * gui)
{
  if(!gui->si->currentstaff)
    return FALSE;
  while (((DenemoStaff *)(gui->si->currentstaff->data))->voicenumber!=1)
    voiceup(gui);
  if (gui->si->currentstaff->prev)
    {
      gui->si->currentstaffnum--;
      gui->si->currentstaff = gui->si->currentstaff->prev;
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_up (gui);
      return TRUE;
    }else
      warningdialog("This is the first staff");
  return FALSE;
}


/**
 * Move si->currentstaff down an one staff/voice, return TRUE if successful
 *
 */
gboolean
voicedown (DenemoGUI * gui)
{
  if(!gui->si->currentstaff)
    return FALSE;
  if (gui->si->currentstaff->next && ((DenemoStaff *)(gui->si->currentstaff->next->data))->voicenumber==2) {
      gui->si->currentstaffnum++;
      gui->si->currentstaff = gui->si->currentstaff->next;
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_down (gui);
      return TRUE;
    } else
      warningdialog("This is the last voice");
  return FALSE;
}
/**
 * Move si->currentstaff down an one staff/voice, return TRUE if successful
 *
 */
gboolean
staffdown (DenemoGUI * gui)
{
  if(!gui->si->currentstaff)
    return FALSE;
  while (gui->si->currentstaff->next && ((DenemoStaff *)(gui->si->currentstaff->next->data))->voicenumber==2)
    voicedown(gui);
  if (gui->si->currentstaff->next)
    {
      gui->si->currentstaffnum++;
      gui->si->currentstaff = gui->si->currentstaff->next;
      setcurrentprimarystaff (gui->si);
      setcurrents (gui->si);
      move_viewport_down (gui);
    }else
      warningdialog("This is the last staff");
}



/**
 * move the cursor one position to the left
 *
 */
void
cursorleft (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  g_debug("cursorleft: cursorpos %d\n", si->cursor_x);
  if (!si->cursor_x)
    {
      /* also the only situation where si->currentobject == NULL */
      if (si->currentmeasure->prev)
	{
	  g_debug("Currentmeasure prev == TRUE");
	  /* Go to end of preceding measure */
	  si->cursor_appending = TRUE;
	  si->currentmeasure = si->currentmeasure->prev;
	  si->currentmeasurenum--;
	  isoffleftside (gui);
	  si->currentobject =
	    g_list_last ((objnode *) si->currentmeasure->data);
	  /* The preceding statement will set currentobject to
	   * NULL if appropriate */
	  si->cursor_x = g_list_length ((objnode *) si->currentmeasure->data);
	  /* Despite appearances, there is not an off-by-one error in the
	   * preceding command */
	}
    }
  else if (si->cursor_appending)
    {
      /* Can back off from appending */
      si->cursor_appending = FALSE;
      si->cursor_x--;
    }
  else
    {
      /* Can go back in the measure */
      if (si->currentobject && si->currentobject->prev)
	{
	  si->currentobject = si->currentobject->prev;
	  si->cursor_x--;
	}
    }
  calcmarkboundaries (si);
  write_status(gui);
}

/**
 * move the cursor one position to the right
 * returns TRUE if the cursor has moved.
 */
gboolean
cursorright (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  if (si->cursor_appending && si->currentmeasure->next)
    {
      /* Go to the next measure */
      si->currentmeasure = si->currentmeasure->next;
      si->currentmeasurenum++;
      isoffrightside (gui);
      si->currentobject = (objnode *) si->currentmeasure->data;
      si->cursor_x = 0;
      if (si->currentobject)
	si->cursor_appending = FALSE;
    }
  else if (si->currentobject)
    {
      /* See if we should go to appending position. If not, go to the
       * next note (if possible) */
      if (!si->cursor_appending && !si->currentobject->next)
	{
	  /* Go to appending position */
	  si->cursor_appending = TRUE;
	  si->cursor_x++;
	}
      else if (si->currentobject->next)
	{
	  si->currentobject = si->currentobject->next;
	  si->cursor_x++;
	}
    }
  calcmarkboundaries (si);
  write_status(gui);
  return si->currentobject || (!si->cursor_appending) || si->currentmeasure->next;
}

/**
 * Move the cursor up one position 
 */
void
cursorup (DenemoGUI * gui)
{
  gui->si->cursor_y++;
  gui->si->staffletter_y = (gui->si->staffletter_y + 1) % 7;
  //g_print ("Cursor Y Position %d\n", gui->si->cursor_y);
}

/**
 * Move the cursor down one position 
 */
void
cursordown (DenemoGUI * gui)
{
  gui->si->cursor_y--;
  gui->si->staffletter_y = (gui->si->staffletter_y + 6) % 7;
  //g_print ("Cursor Y Position %d\n", gui->si->cursor_y);
}

/**
 * shiftcursor: FIXME change the name of this function!
 * Mode sensitive note actions:
 * In Classic mode: Move the cursor to a given note value nearest the current cursor
 * In Edit mode: Change the current note (or insert if none)
 * In Insert mode: Insert a note at the cursor
 */
void
shiftcursor (DenemoGUI  *gui, gint note_value)
{
  gint oldstaffletter_y = gui->si->staffletter_y;
  gint oldcursor_y = gui->si->cursor_y;
  gui->si->staffletter_y = note_value;
  gui->si->cursor_y = jumpcursor (gui->si->cursor_y, oldstaffletter_y,
				  gui->si->staffletter_y);
  int mid_c_offset = gui->si->cursor_y;
  if((gui->mode & INPUTRHYTHM)&&!(gui->mode&INPUTINSERT)){
    warningdialog("Rhythm only mode - enter durations, not notes!");
    return;
  }
     
  /* in edit mode edit the current note name */
  if((gui->mode & INPUTEDIT) && gui->si->currentobject) {
    DenemoObject *theobj =  (DenemoObject *)(gui->si->currentobject->data);
    if(theobj->type == CHORD && ((chord*)theobj->object)->notes) {
      if(g_list_length( ((chord*)theobj->object)->notes)>1) {/* multi-note chord - remove and add a note */
	gui->si->cursor_y = oldcursor_y;
	tonechange(gui->si, TRUE);
	gui->si->cursor_y = mid_c_offset;
	tonechange(gui->si, FALSE);
      } else {/* single-note chord - change the note */
      gint dclef = find_prevailing_clef(gui->si);	    
      modify_note((chord*)theobj->object, mid_c_offset, gui->si->curmeasureaccs[note_value], dclef);
      showwhichaccidentals ((objnode *) gui->si->currentmeasure->data,
			    gui->si->curmeasurekey, gui->si->curmeasureaccs);
      }
      score_status(gui, TRUE);
    }  
  } else
    /* in INSERT (or EDIT with no currentobject) we insert a note using the next step of the rhythm pattern */
#define g  (gui->rstep)
  if((gui->mode&(INPUTEDIT|INPUTINSERT)) && g) {
      GList *start = g;
      GList *h;
      do {
	if(g) {
	  for(h = ((RhythmElement*)g->data)->functions;h;h=h->next) {
	    insertion_point (gui->si);	
	    gui->si->cursoroffend = FALSE;
	    ((GtkFunction)h->data)(gui);
	    displayhelper(gui);
	  }
	  h = ((RhythmElement*)g->data)->functions;
	  g = g->next;/* list is circular */
	}
      } while(g!=start && modifier_code(h->data));
#define CURRP ((RhythmPattern *)gui->currhythm->data)    
      if(((RhythmElement*)g->data)->icon) {/* singletons do not have icon */
	GtkWidget *label = LABEL(CURRP->button);
	//g_print("Markup is %s\n", ((RhythmElement*)g->data)->icon);
	gtk_label_set_markup(GTK_LABEL(label),((RhythmElement*)g->data)->icon);
      }
      score_status(gui, TRUE);
    }
#undef CURRP
#undef g
}


void
insert_rhythm_pattern(DenemoGUI  *gui) {
#define g  (gui->rstep)
  if((gui->mode&(INPUTEDIT)) && g) {
    GList *start = g;
    GList *h;
    do {
      if(g) {
	for(h = ((RhythmElement*)g->data)->functions;h;h=h->next) {
	  insertion_point (gui->si);
	  gui->si->cursoroffend = FALSE;
	  if(!code_is_a_duration(modifier_code(h->data)))
	    cursorleft(gui);
	  ((GtkFunction)h->data)(gui);
	  if(!code_is_a_duration(modifier_code(h->data)))
	    cursorright(gui);
	  displayhelper(gui);
	}
	h = ((RhythmElement*)g->data)->functions;
	g = g->next;/* list is circular */
      }
    } while(g!=start);
  }
#undef g
}

/**
 * insertion_point()
 * chooses/creates a good insertion point.
 * if the cursor is at the end of a full measure:
 *      creates a new measure and makes it the current one.
 * if the cursor is at the end of a full measure before an empty measure:
 *      it makes that empty measure current. 
 * 
 */
void
insertion_point (DenemoScore * si)
{

  gboolean next_measure;

  /* First, check to see if the insertion'll cause the cursor to
   * jump to the next measure. (Denemo will implicitly create it
   * if it doesn't exist already.) */

  next_measure = si->cursoroffend && si->cursor_appending
    && (!si->currentmeasure->next || !si->currentmeasure->next->data);

  g_debug ("next_measure %d\n", next_measure);
  if (next_measure)
    {
      if (!si->currentmeasure->next)
	{
	  gboolean all = TRUE;//add to all measures
	  g_debug ("Appending a new measure\n");

	  /* Add a measure and make it currentmeasure */
	  if(!(all && si->currentstaff && si->currentstaff && g_list_length(((DenemoStaff *) si->currentstaff->data)->measures) == g_list_length(si->measurewidths)))
	    all = FALSE; // add only to current staff if it is shorter than some other staff
	  si->currentmeasure =
	    dnm_addmeasures (si, si->currentmeasurenum, 1, all);
	}
      else
	si->currentmeasure = si->currentmeasure->next;
      /* Now the stuff that needs to be done for each case */
      si->currentmeasurenum++;
      si->currentobject = (objnode *) si->currentmeasure->data;
      si->cursor_x = 0;
      memcpy (si->cursoraccs, si->nextmeasureaccs, SEVENGINTS);
      memcpy (si->curmeasureaccs, si->nextmeasureaccs, SEVENGINTS);
      si->curmeasureclef = si->cursorclef;
    }
}

/**
 * Insert a chord into the score
 * @param si pointer to the scoreinfo structure
 * @param duration the duration of the chord to insert
 * @param mode the current input mode
 * @param rest specifies a note is a rest
 */
void
dnm_insertchord (DenemoGUI * gui, gint duration, input_mode mode, 
		 gboolean rest)
{
  DenemoScore *si = gui->si;
  DenemoObject *mudela_obj_new;
  int prognum;
  DenemoStaff *curstaffstruct;

  if((mode & INPUTEDIT) && !si->cursor_appending) {
    changeduration(si, duration);
    return;
  }
  insertion_point (si);
  
  /* Now actually create the chord */
  mudela_obj_new = newchord (duration, 0, 0);
  if ((mode & INPUTNORMAL) && (rest != TRUE))
    addtone (mudela_obj_new, si->cursor_y, si->cursoraccs[si->staffletter_y],
	     si->cursorclef);
  if ((mode & INPUTBLANK) || (gui->mode & INPUTBLANK))
    mudela_obj_new->isinvisible = TRUE;

  if (si->is_grace_mode)
    ((chord *) mudela_obj_new->object)->is_grace = TRUE;


  /* Insert the new note into the score.  Note that while we may have
     added a measure above, object_insert will invoke nudgerightward,
     which will in turn invoke update_hscrollbar, so we
     don't need to invoke that here.  */

  object_insert (gui, mudela_obj_new);
  curstaffstruct = (DenemoStaff *) si->currentstaff->data;
  prognum = select_program (curstaffstruct->midi_instrument->str);
  /**/playnotes (Denemo.prefs.immediateplayback, *(chord *) mudela_obj_new->object,
     prognum);
}

/**
 * Insert tuplet into the score
 * @param si pointer to the scoreinfo structure
 * @param type the type of tuplet to insert
 */
void
dnm_inserttuplet (DenemoGUI * gui, tuplet_type type)
{
  DenemoScore *si = gui->si;
  DenemoObject *mudela_obj_new;

  insertion_point (si);
  switch (type)
    {
    case DUPLET:
      mudela_obj_new = newtupopen (3, 2);
      break;
    case TRIPLET:
      mudela_obj_new = newtupopen (2, 3);
      break;
    case QUADTUPLET:
      mudela_obj_new = newtupopen (3, 4);
      break;
    case QUINTUPLET:
      mudela_obj_new = newtupopen (4, 5);
      break;
    case SEXTUPLET:
      mudela_obj_new = newtupopen (4, 6);
      break;
    case SEPTUPLET:
      mudela_obj_new = newtupopen (4, 7);
      break;
    default:
      mudela_obj_new = newtupopen (2, 3);
      break;
    }
  //g_print ("Cursor pos %d (Before tup open)\n", si->cursor_x);
  object_insert (gui, mudela_obj_new);
  //g_print ("Cursor pos %d (After tup open, before tup close)\n",	   si->cursor_x);
  /* Add the closing bracket */
  object_insert (gui, newtupclose ());
  //g_print ("Cursor pos %d (After tup close)\n", si->cursor_x);
  si->cursor_x--;
  //g_print ("Cursor pos %d( After move back)\n", si->cursor_x);

  si->currentobject = si->currentobject->prev;
  si->cursor_appending = FALSE;
}

/**
 * Insert grace note into the score
 * @param si pointer to the scoreinfo structure
 */
void
insertgrace (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  DenemoObject *mudela_obj_new;
  insertion_point (si);


  mudela_obj_new = newgracestart ();

  object_insert (gui, mudela_obj_new);

  object_insert (gui, newgraceend ());
  si->cursor_x--;
  si->currentobject =
    g_list_nth ((objnode *) si->currentmeasure->data, si->cursor_x);
  si->cursor_appending = FALSE;
  si->is_grace_mode = TRUE;
}

/**
 * Change the duration of the current note/rest
 * @param si pointer to the scoreinfo structure
 * @param duration the duration to change the current CHORD
 * object to
 */
void
changeduration (DenemoScore * si, gint duration)
{
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      changedur (curmudelaobj, duration, 0);
    }
}

/**
 *  tonechange 
 * If REMOVE delete the note closest to si->cursor_y in a ~si->currentobject
 * else add a note at si->cursor_y to the ~si->currentobject
 * FIXME ~si->currentobject in this comment means the thing gotten by the macro declaremudelaobj. This macro is a horrible hack induced by trying to be clever with tuplets - enforcing pairing of begin/end. tonechange 
 * @param si pointer to the scoreinfo structure
 * @param remove whether to remove note or not
 */
gboolean
tonechange (DenemoScore * si, gboolean remove)
{
  declarecurmudelaobj;
  int prognum;
  gboolean ret = FALSE;
  DenemoStaff *curstaffstruct;
  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      if (remove == TRUE)
	ret = removetone (curmudelaobj, si->cursor_y/*mid_c_offset*/, si->cursorclef/*dclef*/);
      else
	ret = (gboolean)addtone (curmudelaobj, si->cursor_y/* mid_c_offset*/,
		 si->cursoraccs[si->staffletter_y]/* enshift */, si->cursorclef /*dclef*/);

      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}
      curstaffstruct = (DenemoStaff *) si->currentstaff->data;
      prognum = select_program (curstaffstruct->midi_instrument->str);
      /*playnotes (si->prefs->immediateplayback,
       *(chord *) curmudelaobj->object, prognum);*/
    }
  return ret;
}

/**
 * Helper function that contains calls to all the display 
 * update functions
 *
 * @param gui pointer to the DenemoGUI structure
 */
void
displayhelper (DenemoGUI * gui)
{
  DenemoScore *si = gui->si;
  beamandstemdirhelper (si);
  showwhichaccidentals ((objnode *) si->currentmeasure->data,
			si->curmeasurekey, si->curmeasureaccs);
  find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
		       si->cursortime2);
  nudgerightward (gui);
  set_bottom_staff(gui);
  write_status(gui);
  if ((gui->mode&(INPUTRHYTHM)) && si->currentobject && (((DenemoObject*)(si->currentobject->data))->type==CHORD)&& ((DenemoObject*)(si->currentobject->data))->starttickofnextnote>= WHOLE_NUMTICKS * si->cursortime1/si->cursortime2)
    gdk_beep(); //Signal new measures in Edit mode to catch out of step entry

  /*gtk_widget_draw (gui->scorearea, NULL);*/
  gtk_widget_queue_draw (gui->scorearea);
}

/**
 * Increment the enharmonic shift of the tone closest to the cursor.
 * @param si pointer to the DenemoScore structure
 * @param direction, +ve for sharp -ve for flat
 */

void
incrementenshift (DenemoGUI * gui, gint direction)
{
  DenemoScore *si = gui->si;
  declarecurmudelaobj;
  int prognum;
  DenemoStaff *curstaffstruct;
  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      shiftpitch (curmudelaobj, si->cursor_y, direction>0);
      showwhichaccidentals ((objnode *) si->currentmeasure->data,
			    si->curmeasurekey, si->curmeasureaccs);
      find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
			   si->cursortime2);
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}
      curstaffstruct = (DenemoStaff *) si->currentstaff->data;
      prognum = select_program (curstaffstruct->midi_instrument->str);
      /* playnotes (si->prefs->immediateplayback,
       *(chord *) curmudelaobj->object, prognum);*/

      unre_data *data = (unre_data *) g_malloc (sizeof (unre_data));
      data->object = curmudelaobj;
      data->position = si->cursor_x;
      data->measurenum = si->currentmeasurenum;
      data->staffnum = si->currentstaffnum;
      data->action = ACTION_CHANGE;
      update_undo_info (si, data);
      score_status(gui, TRUE);
    }
}
/**
 * Set the enharmonic shift of the tone closest to the cursor.
 * @param si pointer to the DenemoScore structure
 * @param enshift -2 .. +2 for double flat to double sharp FIXME make this a system wide enum
 */

void
setenshift (DenemoScore * si, gint enshift)
{
  declarecurmudelaobj;
  int prognum;
  DenemoStaff *curstaffstruct;
  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      changeenshift (curmudelaobj, si->cursor_y, enshift);
      showwhichaccidentals ((objnode *) si->currentmeasure->data,
			    si->curmeasurekey, si->curmeasureaccs);
      find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
			   si->cursortime2);
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}
      curstaffstruct = (DenemoStaff *) si->currentstaff->data;
      prognum = select_program (curstaffstruct->midi_instrument->str);
      /* playnotes (si->prefs->immediateplayback,
       *(chord *) curmudelaobj->object, prognum);*/

      unre_data *data = (unre_data *) g_malloc (sizeof (unre_data));
      data->object = curmudelaobj;
      data->position = si->cursor_x;
      data->measurenum = si->currentmeasurenum;
      data->staffnum = si->currentstaffnum;
      data->action = ACTION_CHANGE;
      update_undo_info (si, data);
    }
}

/**
 * Change the stemdirection of the current chord object
 * by a given amount
 * @param si  pointer to the scoreinfo structure
 * @param amount the stem direction change to make
 */
void
change_stem_directive (DenemoScore * si, enum stemdirections amount)
{
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    {
      switch (amount)
	{
	case DENEMO_STEMDOWN:
	  ((stemdirective *) curmudelaobj->object)->type = DENEMO_STEMDOWN;
	  break;
	case DENEMO_STEMUP:
	  ((stemdirective *) curmudelaobj->object)->type = DENEMO_STEMUP;
	  break;
	default:
	  ((stemdirective *) curmudelaobj->object)->type = DENEMO_STEMBOTH;
	  break;
	}

      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 * Change the number of dots on the current chord
 *
 * @param si pointer to the scoreinfo structure
 * @param amount the number of dots to add/remove
 */
void
changedots (DenemoScore * si, gint amount)
{
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      changenumdots (curmudelaobj, amount);

      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}
    }
}

/**
 *  Insert measure into the score at the current position
 *
 * @param si pointer to the scoreinfo structure
 * @param number of measures to insert
 */
void
dnm_insertmeasures (DenemoScore * si, gint number)
{
  si->currentmeasure = dnm_addmeasures (si, si->currentmeasurenum - 1, number, 1);
  si->cursor_x = 0;
  si->cursor_appending = TRUE;
  si->currentobject = NULL;
  set_rightmeasurenum (si);
  si->markstaffnum = 0;
  calcmarkboundaries (si);
  /* update_hscrollbar (si); */
}

/**
 * Add measure to the end of the score
 * 
 * @param si pointer to the scoreinfo structure
 * @param number the number of measures to append
 */
void
appendmeasures (DenemoScore * si, gint number)
{
  dnm_addmeasures (si, g_list_length (firstmeasurenode (si->currentstaff)),
	       number, FALSE);
  /* Reset these two variables because si->currentmeasure and
   * si->currentobject may now be pointing to dead data */
  si->currentmeasure =
    g_list_nth (firstmeasurenode (si->currentstaff),
		si->currentmeasurenum - 1);
  si->currentobject =
    g_list_nth ((objnode *) si->currentmeasure->data,
		si->cursor_x - (si->cursor_appending == TRUE));
  set_rightmeasurenum (si);
  /*update_hscrollbar (si); */
}

void
appendmeasurestoentirescore (DenemoScore * si, gint number)
{
  dnm_addmeasures (si, g_list_length (firstmeasurenode (si->currentstaff)),
	       number, TRUE);
  /* Reset these two variables because si->currentmeasure and
   * si->currentobject may now be pointing to dead data */
  si->currentmeasure =
    g_list_nth (firstmeasurenode (si->currentstaff),
		si->currentmeasurenum - 1);
  si->currentobject =
    g_list_nth ((objnode *) si->currentmeasure->data,
		si->cursor_x - (si->cursor_appending == TRUE));
  set_rightmeasurenum (si);
  /* update_hscrollbar (si); */

}





/**
 * Delete staff wrapper to delete the preceding staff 
 * 
 * @param action pointer to the GtkAction event
 * @param gui pointer to the DenemoGUI structure
 */
void
delete_staff_before (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if (staffup(gui)) {
    deletestaff (gui, TRUE);
  }
}

/**
 * Delete staff wrapper to delete the next staff 
 * 
 * @param action pointer to the GtkAction event
 * @param gui pointer to the DenemoGUI structure
 */
void
delete_staff_after (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  if (staffdown(gui)) {
    deletestaff (gui, TRUE);
  }
}

/**
 * Delete staff wrapper to delete current staff
 * 
 * @param action pointer to the GtkAction event
 * @param gui pointer to the DenemoGUI structure
 */
void
delete_staff_current (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  deletestaff (gui, TRUE);
}



/**
 * Delete mesasure from score 
 * @param gui pointer to the DenemoGUI structure
 * @return none
 *
 * This is only a wrapper for the real dnm_deletemeasure
 * function.
 */
void deletemeasure(DenemoGUI * gui)
{
	dnm_deletemeasure(gui->si);
	isoffleftside (gui);
	displayhelper(gui);
}
/**
 * Delete mesasure from all staffs of score 
 * @param gui pointer to the DenemoGUI structure
 * @return none
 *

 */
void deletemeasureallstaffs(DenemoGUI * gui)
{
  DenemoScore *si=gui->si;
  si->currentmeasure =
    removemeasures (si, si->currentmeasurenum - 1, 1, TRUE);
  setcurrents (si);
  score_status(gui, TRUE);
  si->markstaffnum = 0;
  isoffleftside (gui);
  displayhelper(gui);
}



/**
 * Delete measure from the score
 *
 * TODO remove measure from current staff 
 * rather than the entire score
 * @param gui pointer to the DenemoGUI structure
 * @return none
 */
void
dnm_deletemeasure (DenemoScore * si)
{
  si->currentmeasure =
    removemeasures (si, si->currentmeasurenum - 1, 1, FALSE);
  
  /* In case that was the last measure we just deleted, which'd cause
   * the current measure to be the left of what's displayed */

  setcurrents (si);
  si->markstaffnum = 0;


}

/**
 * Remove current object from the score
 *@param cur_measure pointer to the current measure
 * @param cur_objnode pointer to the current object
 * @return none
 */
static void
remove_object (measurenode * cur_measure, objnode * cur_objnode)
{
  if(cur_measure->data) {
    cur_measure->data = g_list_remove_link ((objnode *) cur_measure->data,
					  cur_objnode);
    freeobject ((DenemoObject *) cur_objnode->data);
    g_list_free_1 (cur_objnode);
  }
}


/**
 * Reset the cursor stats 
 * @param si pointer to the scoreinfo structure
 * @return none
 */
static void
reset_cursor_stats (DenemoScore * si)
{
  si->currentobject = g_list_nth ((objnode *) si->currentmeasure->data,
				  si->cursor_x);
  if (!si->currentobject)
    {
      si->currentobject = g_list_last ((objnode *) si->currentmeasure->data);
      si->cursor_appending = TRUE;
    }
}

/**
 * Helper to remove the current object an reset cursor stats
 * @param si pointer to the scoreinfo structure
 * @return none
 */
static void
delete_object_helper (DenemoScore * si)
{
  remove_object (si->currentmeasure, si->currentobject);
  reset_cursor_stats (si);
}


/**
 * Helper to remove a object from the score
 * @param gui pointer to the DenemoGUI structure
 * @return none.
 *
 */
void deleteobject(DenemoGUI *gui)
{
	dnm_deleteobject(gui->si);
}

/**
 * Function to delete object from the score
 * @param gui - pointer to the DenemoGUI structure
 * @return none
 */
void
dnm_deleteobject (DenemoScore * si)
{
  declarecurmudelaobj;
  staffnode *curstaff;
  measurenode *curmeasure;
  //g_print ("dnm_deleteobject undo/redo mode %d\n", si->undo_redo_mode);
  if(curmudelaobj==NULL)
    return;
  /* when tone_store is active, act on that, not the staff itself */

  if (((DenemoStaff*)si->currentstaff->data)->tone_store) {
    if(si->currentobject &&
       ((DenemoObject*)(si->currentobject->data))->type==CHORD){
      if(delete_tone(si, ((DenemoObject*)(si->currentobject->data))->object))
	return;
    }
  }

  if(curmudelaobj->type==LILYDIRECTIVE && ((lilydirective *)curmudelaobj->object)->locked)
    if(!confirm("This LilyPond insert is locked","Really delete it?"))
      return;


  if (si->undo_redo_mode == UNDO)
    {
      unre_data *undo = (unre_data *) g_malloc (sizeof (unre_data));
      undo->staffnum = si->currentstaffnum;
      undo->measurenum = si->currentmeasurenum;
      undo->position = si->cursor_x;
      undo->object = dnm_clone_object (curmudelaobj);
      undo->action = ACTION_DELETE;
      update_undo_info (si, undo);
    }



  if (!si->cursor_appending)
    {
      switch (curmudelaobj->type)
	{
	case CHORD:
	  delete_object_helper (si);

	  break;
	case TUPOPEN:
	case TUPCLOSE:
	  /* TODO - add code that will automatically delete a tupbracket's
	   * corresponding bracket */

	  delete_object_helper (si);

	  break;
	case CLEF:
	  delete_object_helper (si);
	  fixnoteheights ((DenemoStaff *) si->currentstaff->data);
	  beamsandstemdirswholestaff ((DenemoStaff *) si->currentstaff->data);
	  find_xes_in_all_measures (si);
	  break;
	case KEYSIG:
	  /* Doesn't automatically delete sibling key signatures, though
	   * I probably will have it do so soon */
	  delete_object_helper (si);
	  beamsandstemdirswholestaff ((DenemoStaff *) si->currentstaff->data);
	  showwhichaccidentalswholestaff ((DenemoStaff *) si->currentstaff->
					  data);
	  find_xes_in_all_measures (si);
	  break;
	case TIMESIG:
	  /* For time signatures, deletion is linked to all
	   * the staffs on the score */
	  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
	    {
	      curmeasure = g_list_nth (firstmeasurenode (curstaff),
				       si->currentmeasurenum - 1);
	      if(curmeasure){
		remove_object (curmeasure, (objnode *) curmeasure->data);
		beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
	      }
	    }
	  reset_cursor_stats (si);
	  find_xes_in_all_measures (si);
	  break;
	case STEMDIRECTIVE:
	  delete_object_helper (si);
	  beamsandstemdirswholestaff ((DenemoStaff *) si->currentstaff->data);
	  find_xes_in_all_measures (si);
	  break;
	case DYNAMIC:
	  delete_object_helper (si);

	  break;
	case LILYDIRECTIVE:
	  delete_object_helper (si);
	  //displayhelper (gui);
	  break;
	case GRACE_START:
	case GRACE_END:
	  delete_object_helper (si);

	  break;
	case LYRIC:
	case FIGURE:
	  delete_object_helper (si);

	  break;
	case BARLINE:
	  //    case COMMENT:
	case MEASUREBREAK:
	  break;
	}
      si->markstaffnum = 0;
    }
}

/**
 * Insert cloned chordobject into the 
 * score
 * @param si - pointer to the scoreinfo structure
 * @return none
 */
void
insertclone (DenemoGUI *gui)
{
  DenemoScore *si = gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    object_insert (gui, dnm_clone_object (curmudelaobj));
}


void tolastobject (DenemoGUI *gui)
{
  while (gui->si->currentobject && (gui->si->currentobject->next))
    {
      gui->si->currentobject = gui->si->currentobject->next;
      gui->si->cursor_x++;
    }
}

/**
 * Move cursor to the end of the score 
 * @param action - Gtk Action event 
 * @param gui - pointer to the DenemoGUI structure
 * @return none
 */
void
toend (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  gui->si->currentmeasurenum = gui->si->leftmeasurenum =
    gui->si->rightmeasurenum =
    g_list_length (((DenemoStaff *) gui->si->currentstaff->data)->measures);
  setcurrents (gui->si);
  tolastobject(gui);
  cursorright(gui);
  find_leftmost_allcontexts (gui->si);
  update_hscrollbar (gui);
  displayhelper (gui);
}

/**
 * Move the cursor to the beginning of the score 
 * @param action - Gtk Action event
 * @param gui - pointer to the DenemoGUI structure
 * @return none
*/
void
tohome (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  gui->si->currentmeasurenum = gui->si->leftmeasurenum = 1;
  set_rightmeasurenum (gui->si);
  setcurrents (gui->si);
  find_leftmost_allcontexts (gui->si);
  update_hscrollbar (gui);
  /*gtk_widget_draw (gui->scorearea, NULL);*/
  gtk_widget_queue_draw (gui->scorearea);
}


/**
 * Insert stem directive,  absolute stemdirection for the 
 * entire staff or until a new stem directive in added
 * 
 * @param action Gtk Action event
 * @param gui pointer to the DenemoGUI structure
 * @return none
 */
void
stem_directive_insert (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  object_insert (gui, dnm_stem_directive_new (DENEMO_STEMBOTH));
  /* This sets beams and stem directions in the measure, but that's
   * not sufficient */

  displayhelper (gui);


}

/**
 * Toggle start_slur flag for the current chord
 * 
 * @param gui pointer to the DenemoGUI structure
 * @return none
 */
void
toggle_begin_slur (DenemoGUI *gui)
{
  DenemoScore *si = gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->slur_begin_p
	= !((chord *) curmudelaobj->object)->slur_begin_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 *  Force a cautionary accidental
 * @param si pointer to the scoreinfo structure
 * @return none
 */
void
caution (DenemoScore * si)
{
  declarecurmudelaobj;

  forceaccidentals (curmudelaobj);
  find_xes_in_measure (si, si->currentmeasurenum, si->cursortime1,
		       si->cursortime2);

  if (curmudelaobj->user_string)
    {
      g_free (curmudelaobj->user_string);
      curmudelaobj->user_string = NULL;
    }
}

/**
 * Toggle end_slur flag for the current chord
 * @param gui pointer to the DenemoGUI structure
 * @return none
 */
void
toggle_end_slur (DenemoGUI *gui)
{
  DenemoScore *si = gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->slur_end_p
	= !((chord *) curmudelaobj->object)->slur_end_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 * Toggle start crescendo flag for current chord
 * @param si pointer to the scoreinfo structure
 * @return none
 */
void
toggle_start_crescendo (DenemoGUI * gui)
{
  DenemoScore *si = (DenemoScore *) gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->crescendo_begin_p
	= !((chord *) curmudelaobj->object)->crescendo_begin_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 * Toggle end crescendo flag for current chord
 * @param si pointer to the scoreinfo structure
 * @return none
 */
void
toggle_end_crescendo (DenemoGUI *gui)
{
  DenemoScore *si = (DenemoScore *) gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->crescendo_end_p
	= !((chord *) curmudelaobj->object)->crescendo_end_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 * Toggle start diminuendo flag for current chord
 * @param si pointer to the scoreinfo structure
 * @return none
 */
void
toggle_start_diminuendo (DenemoGUI *gui)
{
  DenemoScore *si = (DenemoScore *) gui->si;	
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->diminuendo_begin_p
	= !((chord *) curmudelaobj->object)->diminuendo_begin_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}

/**
 * Toggle end diminuendo flag for current chord
 * @param si pointer to the scoreinfo structure
 * @return none
 */
void
toggle_end_diminuendo (DenemoGUI *gui)
{
  DenemoScore *si = (DenemoScore *) gui->si;
  declarecurmudelaobj;

  if (curmudelaobj && curmudelaobj->type == CHORD)
    {
      ((chord *) curmudelaobj->object)->diminuendo_end_p
	= !((chord *) curmudelaobj->object)->diminuendo_end_p;
      if (curmudelaobj->user_string)
	{
	  g_free (curmudelaobj->user_string);
	  curmudelaobj->user_string = NULL;
	}

    }
}



/**
 * Autosave timeout function - saves the current score after the current
 * timeout has expired
 * @param si pointer to the scoreinfo structure
 * @return none
 */
gboolean
auto_save_document_timeout (DenemoGUI *gui)
{
  /* first check that this timer has not been left running after destruction of the gui */
  if(g_list_find(Denemo.guis, gui)==NULL) {
    warningdialog("Timer left running");
    return FALSE; /* turns off the timer */
  }
  DenemoScore *si = gui->si;
  g_print ("Autosaving\n");
  if (!gui->autosavename)
    {
      g_warning ("gui->autosavename not set\n");
      /*gui->autosavename = g_string_new (dir);*/
      gui->autosavename = g_string_new (locatedotdenemo ());
      if (si->lily_file)
	gui->autosavename = g_string_append (gui->autosavename, "/autosave.ly");
      else
	gui->autosavename =
	  g_string_append (gui->autosavename, "/autosave.denemo");
    }
  //g_print ("Auto save file name %s\n", gui->autosavename->str);
  if (si->lily_file)
    {
      exportlilypond (gui->autosavename->str, gui, TRUE);
    }
  else
    {
      exportXML (gui->autosavename->str, gui, 0, 0);
    }

  return TRUE;
}
