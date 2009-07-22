/* keyresponses.c
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <string.h>

#include "kbd-custom.h"
#include "keyresponses.h"
#include "articulations.h"
#include "barline.h"
#include "utils.h"
#include "tupletops.h"
#include "view.h"







static GdkEventKey ** divert_key_event;/* Non null if key events are being intercepted by a function
					* (which is running a gtk_mail_loop() for this reason).
					* return TRUE if a key press successfully captured
					* in which case the kyval, state pair are returned
					*/

gboolean intercept_scorearea_keypress (GdkEventKey *pevent) {
  if(divert_key_event) {
    warningdialog("Recursive key capture not possible!");/* we could make a stack of them instead ... */
    return FALSE;
  }
  GdkEventKey *event;
  divert_key_event = &event;
  gtk_main();
  divert_key_event = NULL;
  // *keyval = event->keyval;
  // *state = dnm_sanitize_key_state(event);
  memcpy(pevent, event, sizeof(GdkEventKey));
  return TRUE;
}



static guint lock_mask(gint keyval) {

  if((keyval==GDK_Shift_L)||
     (keyval==GDK_Shift_R))
    return GDK_SHIFT_MASK;

  if(keyval==GDK_Caps_Lock)
    return GDK_LOCK_MASK;
    
  if((keyval==GDK_Control_L)||
     (keyval==GDK_Control_R))
    return GDK_CONTROL_MASK;


  if(keyval==GDK_Alt_L)
    return GDK_MOD1_MASK;



  if(keyval==GDK_Num_Lock)
    return GDK_MOD2_MASK;

     /*penguin/windows */
  if((keyval==GDK_Super_L)||
     (keyval==GDK_Super_R))
    return GDK_MOD4_MASK;

  if(keyval==GDK_Alt_R || GDK_ISO_Level3_Shift )
    return GDK_MOD5_MASK;

  return 0;

}



/**
 * keyrelease event callback 
 * sets cursor if a modifier
 */

gint
scorearea_keyrelease_event (GtkWidget * widget, GdkEventKey * event)
{
  // set_cursor_for(keyrelease_modify(event->state), event->keyval);
  gint state;
  if((event->keyval==GDK_Caps_Lock) || (event->keyval==GDK_Num_Lock))
    return TRUE;
  state = (lock_mask(event->keyval)^event->state);
  set_cursor_for(state);
  return TRUE;
}
/**
 * keypress event callback 
 * looks up the key press and executes the correct function
 */

gint
scorearea_keypress_event (GtkWidget * widget, GdkEventKey * event)
{
  DenemoGUI *gui = Denemo.gui;
  keymap *the_keymap = Denemo.map;
  if(divert_key_event && !isModifier(event)) {
    *divert_key_event = event;
    //g_object_ref(event); FIXME do we need to keep it around?
    gtk_main_quit();
    return TRUE;
  }
  gint state;
  state = (lock_mask(event->keyval)^event->state);
  if(state || ((event->keyval==GDK_Caps_Lock) || (event->keyval==GDK_Num_Lock)))
    set_cursor_for(state); // MUST LOOK AHEAD to state after keypress HERE CATCH modifiers and set the cursor for them.....
  /* Look up the keystroke in the keymap and execute the appropriate
   * function */
  gint command_idx = lookup_command_for_keyevent(event);
  if (command_idx != -1) {
    const gchar *command_name =
      lookup_name_from_idx (the_keymap, command_idx);
    if (command_name) {
      Denemo.last_keyval = event->keyval;
      Denemo.last_keystate =  dnm_sanitize_key_state(event);
      execute_callback_from_name(the_keymap, command_name);
      gui = Denemo.gui;
      displayhelper (gui);
      gtk_widget_draw (gui->scorearea, NULL);   
      return TRUE;
    } else {
      g_warning("No action %i has no name", command_idx);
    }
  }
  return TRUE;
}

/**
 * Reduce the measure width by 10 pixels
 */
void
adjust_measure_less_width_key (DenemoGUI * gui)
{
  adjustmeasurewidth (gui->si, -10);
}

/**
 * Enlarge the measure width by 10 pixels
 */
void
adjust_measure_more_width_key (DenemoGUI * gui)
{
  adjustmeasurewidth (gui->si, 10);
}

/**
 * Reduce the staff height by 10 pixels
 */
void
adjust_staff_less_height_key (DenemoGUI * gui)
{
  adjuststaffheight (gui->si, -10);
}

/**
 * Enlarge the staff height by 10 pixels
 */
void
adjust_staff_more_height_key (DenemoGUI * gui)
{
  adjuststaffheight (gui->si, 10);

}

/**
 * Remove selection marker
 */
void
unset_selection_key (DenemoGUI * gui)
{
  gui->si->markstaffnum = 0;
}

/**
 * Insert quarter note into score
 */
void
insert_chord_key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 2, gui->mode, FALSE);
}

/**
 * Goto the nearest a
 *
 */
void
go_to_A_key (DenemoGUI * gui)
{
  shiftcursor (gui, 5);
}

/**
 * Goto the nearest b
 *
 */
void
go_to_B_key (DenemoGUI * gui)
{
  shiftcursor (gui, 6);
}

/**
 * Goto the nearest c
 *
 */
void
go_to_C_key (DenemoGUI * gui)
{
  shiftcursor (gui, 0);
}

/**
 * Goto the nearest d
 *
 */
void
go_to_D_key (DenemoGUI * gui)
{
  shiftcursor (gui, 1);
}

/**
 * Goto the nearest e
 *
 */
void
go_to_E_key (DenemoGUI * gui)
{
  shiftcursor (gui, 2);
}

/**
 * Goto the nearest f
 *
 */
void
go_to_F_key (DenemoGUI * gui)
{
  shiftcursor (gui, 3);
}

/**
 * Goto the nearest g
 *
 */
void
go_to_G_key (DenemoGUI * gui)
{
  shiftcursor (gui, 4);
}


/**
 * Move cursor by amount
 */
static void
octave_shift_key (DenemoGUI * gui, gint amount)
{
  if (((DenemoStaff*)gui->si->currentstaff->data)->tone_store) {
    return;//FIXME create a function modify_tone, like delete_tone in pitchentry.c to do this sort of thing
  } else {
    if(gui->mode&(INPUTEDIT))
      {
	if(gui->si->currentobject) {
	  objnode *thenote = nearestnote (gui->si->currentobject->data, gui->si->cursor_y);
	  if(thenote) {
	    note copy = *((note *) thenote->data);
	    notechange(gui->si, TRUE);
	    gui->si->cursor_y = copy.mid_c_offset + amount;
	    notechange(gui->si, FALSE);
	    changeenshift(gui->si->currentobject->data, gui->si->cursor_y, copy.enshift);
	  }
	}
      }
    else
      gui->si->cursor_y += amount;
  }
}
/**
 * Move cursor an octave up
 */
void
octave_up_key (DenemoGUI * gui)
{
  octave_shift_key(gui, 7);
}




/**
 * Move cursor an octave down
 */
void
octave_down_key (DenemoGUI * gui)
{
  octave_shift_key (gui, -7);
}

/**
 * change to read only mode
 * only score traversal available FIXME wrong name
 */
void
default_mode (DenemoGUI * gui)
{
  gui->mode ^= TRAVERSE;
  if(gui->mode & TRAVERSE)
    gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id,
			"Read Only");
  g_print ("Mode %d\n", gui->mode);
  displayhelper (gui);
}




/**
 * Toggle into rest mode
 * 
 */
void
rest_toggle_key (DenemoGUI * gui)
{
GtkAction *mode = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/Rest");
  gtk_action_activate(mode);

}

/**
 * Toggle blank mode FIXME bitfields!!!
 *
 */
void
toggle_blank (DenemoGUI * gui)
{
GtkAction *mode = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/Blank");
  gtk_action_activate(mode);
}

/**
 * Add measure to end of staff / score
 */
void
append_measure_key (DenemoGUI * gui)
{
  appendmeasures (gui->si, 1);
}

void
append_measure_score (DenemoGUI * gui)
{
  appendmeasurestoentirescore (gui->si, 1);
}

/** 
 * Insert measure at the current 
 */
void
insert_measure_key (DenemoGUI * gui)
{
  dnm_insertmeasures (gui->si, 1);
}


void
insert_chord_0key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 0, gui->mode, FALSE);

}

void
insert_chord_1key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 1, gui->mode, FALSE);
}

void
insert_chord_2key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 2, gui->mode, FALSE);
}

void
insert_chord_3key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 3, gui->mode, FALSE);
}

void
insert_chord_4key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 4, gui->mode, FALSE);
}

void
insert_chord_5key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 5, gui->mode, FALSE);
}

void
insert_chord_6key (DenemoGUI * gui)
{

    dnm_insertchord (gui, 6, gui->mode, FALSE);
}

void
insert_rest_0key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 0, INPUTREST, TRUE);

}

void
insert_rest_1key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 1, INPUTREST, TRUE);
}

void
insert_rest_2key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 2, INPUTREST, TRUE);
}

void
insert_rest_3key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 3, INPUTREST, TRUE);
}

void
insert_rest_4key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 4, INPUTREST, TRUE);
}

void
insert_rest_5key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 5, INPUTREST, TRUE);
}

void
insert_rest_6key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 6, INPUTREST, TRUE);
}


void
insert_blankchord_0key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 0, INPUTBLANK, FALSE);
}

void
insert_blankchord_1key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 1, INPUTBLANK, FALSE);

}

void
insert_blankchord_2key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 2, INPUTBLANK, FALSE);

}

void
insert_blankchord_3key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 3, INPUTBLANK, FALSE);

}

void
insert_blankchord_4key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 4, INPUTBLANK, FALSE);

}

void
insert_blankchord_5key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 5, INPUTBLANK, FALSE);

}

void
insert_blankchord_6key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 6, INPUTBLANK, FALSE);

}



void
insert_duplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, DUPLET);
}

void
insert_triplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, TRIPLET);
}

void
start_triplet (DenemoGUI * gui)
{
  insertion_point (gui->si);
  object_insert (gui, newtupopen(2,3));
}
void
end_tuplet (DenemoGUI * gui)
{
  object_insert (gui, newtupclose ());
}
void
insert_quadtuplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, QUADTUPLET);
}

void
insert_quintuplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, QUINTUPLET);
}

void
insert_sextuplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, SEXTUPLET);
}

void
insert_septuplet (DenemoGUI * gui)
{
  dnm_inserttuplet (gui, SEPTUPLET);
}



gboolean
add_tone_key (DenemoGUI * gui)
{
 return notechange (gui->si, FALSE);

}

gboolean
remove_tone_key (DenemoGUI * gui)
{
  return notechange (gui->si, TRUE);

}

void
deletepreviousobject(DenemoGUI * gui)
{

  /* remove the object preceding the cursor, within the current measure */
  if (gui->si->cursor_x)
    {
      /* Then move the cursor back */
      cursorleft (NULL);
      /* And delete */
      deleteobject (gui);
    }
  else
    {/* go to the previous measure and start deleting there */
      if(gui->si->currentmeasure->prev) {
	measureleft(NULL);
	while (gui->si->currentobject && (gui->si->currentobject->next))
	  {
	    gui->si->currentobject = gui->si->currentobject->next;
	    gui->si->cursor_x++;
	  }
	cursorright(NULL);
	if(gui->si->currentobject)
	  deletepreviousobject(gui);
      }
    }
  /* if you are following a rhythmic pattern then backup the pattern */
#define g  (gui->rstep)
  if((gui->mode&(INPUTEDIT) && g)) 
    {
#define CURRP ((RhythmPattern *)gui->currhythm->data) 
      g = g->prev; /* list is circular - should we stop at beginning? */
      if(((RhythmElement*)g->data)->icon) {
	GtkWidget *label = LABEL(CURRP->button);
	gtk_label_set_markup(GTK_LABEL(label),((RhythmElement*)g->data)->icon);
      }
#undef CURRP
#undef g
    }
}

void
sharpen_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (gui->si, DENEMO_STEMUP);
  else
    incrementenshift (gui, 1);
}
void
stem_up (DenemoGUI * gui)
{
  sharpen_key(gui);
}
void
flatten_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (gui->si, DENEMO_STEMDOWN);
  else
    incrementenshift (gui, -1);
}
void
stem_down (DenemoGUI * gui)
{
  flatten_key(gui);
}
/* insert a duplicate note and tie to it */
void
tie_notes_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  if (curmudelaobj && curmudelaobj->type == CHORD &&
      ((chord *) curmudelaobj->object)->notes)
    {
      insertion_point (gui->si);
      object_insert (gui, dnm_clone_object (curmudelaobj));
      ((chord *) curmudelaobj->object)->is_tied = TRUE;
    }
}

void
add_dot_key (DenemoGUI * gui)
{
  changedots (gui->si, 1);
}

void
remove_dot_key (DenemoGUI * gui)
{
  changedots (gui->si, -1);
}


void
add_accent (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  addornament (curmudelaobj, D_ACCENT);
  /* if (curmudelaobj && curmudelaobj->type == CHORD &&
     ((chord *) curmudelaobj->object)->notes)
     ((chord *)curmudelaobj->object)->ornamentlist = 
     insert_ornament_list(D_ACCENT,
     ((chord *)curmudelaobj->object)->ornamentlist); */

}

void
add_fermata (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  addornament (curmudelaobj, FERMATA);


}

void
add_staccato (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  addornament (curmudelaobj, STACCATO);



}

void
add_tenuto (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  addornament (curmudelaobj, TENUTO);
}


void
add_trill (DenemoGUI * gui)
{
/*  note *curr;*/
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  addornament (curmudelaobj, TRILL);

}

void
add_turn (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, TURN);
}

void
add_mordent (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, MORDENT);

}

void
add_staccatissimo (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, STACCATISSIMO);

}

void
add_coda (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, CODA);


}


void
add_flageolet (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  addornament (curmudelaobj, FLAGEOLET);

}

void
add_open (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, OPEN);

}

void
add_prallmordent (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, PRALLMORDENT);
}

void
add_prallprall (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, PRALLPRALL);
}

void
add_prall (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, PRALL);
}

void
add_reverseturn (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, REVERSETURN);
}

void
add_segno (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, SEGNO);
}

void
add_sforzato (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, SFORZATO);
}

void
add_stopped (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, STOPPED);
}

void
add_thumb (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, THUMB);
}

void
add_trillelement (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, TRILLELEMENT);
}

void
add_trill_element (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, TRILLELEMENT);
}

void
add_upprall (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, UPPRALL);
}

void
add_arpeggio (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  addornament (curmudelaobj, D_ARPEGGIO);
}



void
set_grace (DenemoGUI * gui)
{
  insertgrace (gui);
}

void
force_cautionary (DenemoGUI * gui)
{
  DenemoObject *theobj =
    gui->si->currentobject ? (DenemoObject *) gui->si->currentobject->
    data : NULL;
  if (theobj && theobj->type == CHORD)
    caution (gui->si);

}

void
change_pitch (DenemoGUI * gui)
{
  if (gui->mode & INPUTEDIT)
    {
      //DenemoObject *theobj =
      //  si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
      notechange (gui->si, TRUE);
      notechange (gui->si, FALSE);
    }
  // addtone(theobj, si->cursor_y, si->cursoraccs[si->staffletter_y],
  //       si->cursorclef);

}


void
insert_doublebar (DenemoGUI * gui)
{
  object_insert (gui, newbarline (barlinefromname (_("Double"))));
}

void
insert_endbar (DenemoGUI * gui)
{
  object_insert (gui, newbarline (barlinefromname (_("End"))));
}

void
insert_openrepeat (DenemoGUI * gui)
{
  object_insert (gui, newbarline (barlinefromname (_("Open Repeat"))));
}

void
insert_closerepeat (DenemoGUI * gui)
{
  object_insert (gui, newbarline (barlinefromname (_("Close Repeat"))));
}

void
insert_opencloserepeat (DenemoGUI * gui)
{
  object_insert (gui, newbarline (barlinefromname (_("Double"))));
}


/**
 * Wrapper function to create new treble clef and insert into the score
 */
void
newcleftreble (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_TREBLE_CLEF));
}

/**
 * Wrapper function to create new bass clef and insert into the score
 */
void
newclefbass (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_BASS_CLEF));
}

/**
 * Wrapper function to create new alto clef and insert into the score
 */
void
newclefalto (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_ALTO_CLEF));
}

/**
 * Wrapper function to create new treble_8 clef and insert into the score
 */
void
newclefg8 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_G_8_CLEF));
}

/**
 * Wrapper function to create new tenor clef and insert into the score
 */
void
newcleftenor (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_TENOR_CLEF));
}

/**
 * Wrapper function to create new soprano clef and insert into the score
 */
void
newclefsoprano (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_SOPRANO_CLEF));
}

/**
 * Wrapper function to create new 4/4 time sig and insert into the score
 */
void
newtimesig44 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (4, 4));
}

/**
 * Wrapper function to create new 2/4 time sig and insert into the score
 */
void
newtimesig24 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (2, 4));
}

/**
 * Wrapper function to create new 3/4 time sig and insert into the score
 */
void
newtimesig34 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (3, 4));
}

/**
 * Wrapper function to create new 6/4 time sig and insert into the score
 */
void
newtimesig64 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (6, 4));
}

/**
 * Wrapper function to create new 5/4 time sig and insert into the score
 */
void
newtimesig54 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (5, 4));
}

/**
 * Wrapper function to create new 3/8 time sig and insert into the score
 */
void
newtimesig38 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (3, 8));
}

/**
 * Wrapper function to create new 6/8 time sig and insert into the score
 */
void
newtimesig68 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (6, 8));
}

/**
 * Wrapper function to create new 9/8 time sig and insert into the score
 */
void
newtimesig98 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (9, 8));
}

/**
 * Wrapper function to create new 12/8 time sig and insert into the score
 */
void
newtimesig128 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (12, 8));
}

/**
 * Wrapper function to create new 2/2 time sig and insert into the score
 */
void
newtimesig22 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (2, 2));
}

/**
 * Wrapper function to create new 3/2 time sig and insert into the score
 */
void
newtimesig32 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (3, 2));
}

/**
 * Wrapper function to create new 4/2 time sig and insert into the score
 */
void
newtimesig42 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newtimesigobj (4, 2));
}



void
newkeysigcmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (0, 1, 0));
}

void
newkeysiggmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (1, 1, 0));
}

void
newkeysigdmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (2, 1, 0));
}

void
newkeysigamaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (3, 1, 0));
}

void
newkeysigemaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (4, 1, 0));
}

void
newkeysigbmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (5, 1, 0));
}

void
newkeysigfsharpmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (6, 1, 0));
}

void
newkeysigcsharpmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (7, 1, 0));
}

void
newkeysigfmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-1, 1, 0));
}

void
newkeysigbflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-2, 1, 0));
}

void
newkeysigeflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-3, 1, 0));
}

void
newkeysigaflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-4, 1, 0));
}

void
newkeysigdflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-5, 1, 0));
}

void
newkeysiggflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-6, 1, 0));
}

void
newkeysigcflatmaj (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-7, 1, 0));
}

void
newkeysigamin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (0, 0, 0));
}

void
newkeysigemin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (1, 0, 0));
}

void
newkeysigbmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (2, 0, 0));
}

void
newkeysigfsharpmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (3, 0, 0));
}

void
newkeysigcsharpmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (4, 0, 0));
}

void
newkeysiggsharpmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (5, 0, 0));
}

void
newkeysigdsharpmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (6, 0, 0));
}

void
newkeysigasharpmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (7, 0, 0));
}

void
newkeysigdmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-1, 0, 0));
}

void
newkeysiggmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-2, 0, 0));
}

void
newkeysigcmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-3, 0, 0));
}

void
newkeysigfmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-4, 0, 0));
}

void
newkeysigbflatmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-5, 0, 0));
}

void
newkeysigeflatmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-6, 0, 0));
}

void
newkeysigaflatmin (DenemoGUI * gui)
{
  object_insert (gui, dnm_newkeyobj (-7, 0, 0));
}


//Functions to set the initial key signature
void
setkeysigcmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 1);
  //displayhelper(si);
}

void
setkeysiggmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 1);
  //displayhelper(si);
}

void
setkeysigdmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 1);
}

void
setkeysigamaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 1);
}

void
setkeysigemaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 1);
}

void
setkeysigbmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 1);
}

void
setkeysigfsharpmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 1);
}

void
setkeysigcsharpmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 1);
}

void
setkeysigfmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 1);
}

void
setkeysigbflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 1);
}

void
setkeysigeflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 1);
}

void
setkeysigaflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 1);
}

void
setkeysigdflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 1);
}

void
setkeysiggflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 1);
}

void
setkeysigcflatmaj (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 1);
}

void
setkeysigamin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 0);
}

void
setkeysigemin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 0);
}

void
setkeysigbmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 0);
}

void
setkeysigfsharpmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 0);
}

void
setkeysigcsharpmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 0);
}

void
setkeysiggsharpmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 0);
}

void
setkeysigdsharpmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 0);
}

void
setkeysigasharpmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 0);
}

void
setkeysigdmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 0);
}

void
setkeysiggmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 0);
}

void
setkeysigcmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 0);
}

void
setkeysigfmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 0);
}

void
setkeysigbflatmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 0);
}

void
setkeysigeflatmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 0);
}

void
setkeysigaflatmin (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 0);
}


void
settimesig22 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 2, 2, TRUE);
}

void
settimesig42 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 4, 2, TRUE);
}

void
settimesig32 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 3, 2, TRUE);
}

void
settimesig44 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 4, 4, TRUE);
}

void
settimesig54 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 5, 4, TRUE);
}

void
settimesig24 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 2, 4, TRUE);
}

void
settimesig34 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 3, 4, TRUE);
}

void
settimesig68 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 6, 8, TRUE);
}

void
settimesig128 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 12, 8, TRUE);
}

void
settimesig38 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 3, 8, TRUE);
}

void
settimesig98 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 9, 8, TRUE);
}

void
settimesig64 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (gui->si, curstaff, 6, 4, TRUE);
}

void
setcleftreble (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_TREBLE_CLEF);
}

void
setclefbass (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_BASS_CLEF);
}

void
setclefg8 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_G_8_CLEF);
}

void
setclefalto (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_ALTO_CLEF);
}

void
setcleftenor (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_TENOR_CLEF);
}

void
setclefsoprano (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_SOPRANO_CLEF);
}

