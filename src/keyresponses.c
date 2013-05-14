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
#include "commandfuncs.h"
#if GTK_MAJOR_VERSION==3
#include <gdk/gdkkeysyms-compat.h>      //FIXME Look for something more gtk3 like
#endif
static GdkEventKey **divert_key_event;  /* Non null if key events are being intercepted by a function
                                         * (which is running a gtk_mail_loop() for this reason).
                                         * return TRUE if a key press successfully captured
                                         * in which case the kyval, state pair are returned
                                         */

static gint divert_key_id = 0;
gboolean
intercept_scorearea_keypress (GdkEventKey * pevent)
{
  if (divert_key_event)
    {
      warningdialog ("Recursive key capture not possible!");    /* we could make a stack of them instead ... */
      return FALSE;
    }
  GdkEventKey *event;
  divert_key_id = Denemo.gui->id;
  divert_key_event = &event;
  gtk_main ();
  divert_key_event = NULL;
  // *keyval = event->keyval;
  // *state = dnm_sanitize_key_state(event);
  memcpy (pevent, event, sizeof (GdkEventKey));
  return TRUE;
}



static guint
lock_mask (gint keyval)
{

  if ((keyval == GDK_Shift_L) || (keyval == GDK_Shift_R))
    return GDK_SHIFT_MASK;

  if (keyval == GDK_Caps_Lock)
    return GDK_LOCK_MASK;

  if ((keyval == GDK_Control_L) || (keyval == GDK_Control_R))
    return GDK_CONTROL_MASK;


  if (keyval == GDK_Alt_L)
    return GDK_MOD1_MASK;



  if (keyval == GDK_Num_Lock)
    return GDK_MOD2_MASK;

  /*penguin/windows */
  if ((keyval == GDK_Super_L) || (keyval == GDK_Super_R))
    return GDK_MOD4_MASK;

  if (keyval == GDK_Alt_R || GDK_ISO_Level3_Shift)
    return GDK_MOD5_MASK;

  return 0;

}

static guint
klock_mask (gint keyval)
{

  if ((keyval == GDK_Shift_L) || (keyval == GDK_Shift_R))
    return GDK_SHIFT_MASK;

  if ((keyval == GDK_Control_L) || (keyval == GDK_Control_R))
    return GDK_CONTROL_MASK;


  if (keyval == GDK_Alt_L)
    return GDK_MOD1_MASK;


  /*penguin/windows */
  if ((keyval == GDK_Super_L) || (keyval == GDK_Super_R))
    return GDK_MOD4_MASK;

  if (keyval == GDK_Alt_R || GDK_ISO_Level3_Shift)
    return GDK_MOD5_MASK;

  return 0;

}

static guint
llock_mask (gint keyval)
{
  if (keyval == GDK_Caps_Lock)
    return GDK_LOCK_MASK;


  if (keyval == GDK_Num_Lock)
    return GDK_MOD2_MASK;

  return 0;

}


/**
 * keyrelease event callback 
 * sets cursor if a modifier
 */

gint
scorearea_keyrelease_event (GtkWidget * widget, GdkEventKey * event)
{
  Denemo.keyboard_state ^= (0xf & klock_mask (event->keyval));
  if ((event->keyval == GDK_Alt_L) || (event->keyval == GDK_Alt_R))
    {
      if ((Denemo.keyboard_state & CHORD_MASK)) //At least one note has been entered in a chord
        next_editable_note ();
      Denemo.keyboard_state &= ~CHORD_MASK;
    }
  set_midi_in_status ();
  // g_print("release %x state %x\n", Denemo.keyboard_state, event->state);
  // set_cursor_for(keyrelease_modify(event->state), event->keyval);
  gint state;
  if ((event->keyval == GDK_Caps_Lock) || (event->keyval == GDK_Num_Lock))
    return TRUE;
  state = (lock_mask (event->keyval) ^ event->state);

  set_cursor_for (state);
  return TRUE;
}

/* perform the command of the given name and store the event that triggered it */
static gchar *
perform_command (const gchar * command_name, GdkEventKey * event)
{
  Denemo.last_keyval = event->keyval;
  Denemo.last_keystate = dnm_sanitize_key_state (event);
  call_out_to_guile ("(define DenemoKeypressActivatedCommand #t)");
  execute_callback_from_name (Denemo.map, command_name);
  call_out_to_guile ("(define DenemoKeypressActivatedCommand #f)");
  // note gui = Denemo.gui; may have changed as a result of executing the command
#ifdef TESTING_REPEATED_XPOSITION_UPDATE
  if (Denemo.gui->si)
    displayhelper (Denemo.gui);
#endif
  return NULL;
}

//return the value of perform_command if executed or "" if keypress is part of a two-key shortcut, or NULL toherwise
gchar *
process_key_event (GdkEventKey * event, gchar * perform_command ())
{
  DenemoGUI *gui = Denemo.gui;
  keymap *the_keymap = Denemo.map;
  // g_print("\n********\nCaps Lock %x?\n\n********\nShifted %x?\n", event->state&GDK_LOCK_MASK,          event->state&GDK_SHIFT_MASK     );
  {
    gint state;
    state = (lock_mask (event->keyval) ^ event->state);
    if (state || ((event->keyval == GDK_Caps_Lock) || (event->keyval == GDK_Num_Lock)))
      set_cursor_for (state);   // MUST LOOK AHEAD to state after keypress HERE CATCH modifiers and set the cursor for them.....
  }
  dnm_clean_event (event);


  if (isModifier (event))
    return NULL;

  /* Look up the keystroke in the keymap and execute the appropriate
   * function */
  static GString *prefix_store = NULL;
  if (!prefix_store)
    prefix_store = g_string_new ("");

  gint command_idx = lookup_command_for_keyevent (event);
  if ((prefix_store->len == 0) && (command_idx != -1))
    {
      const gchar *command_name = lookup_name_from_idx (the_keymap, command_idx);
      if (command_name)
        {
          if (Denemo.prefs.learning)
            {
              gchar *name = dnm_accelerator_name (event->keyval, event->state);
              KeyStrokeShow (name, command_idx, TRUE);
              g_free (name);
            }
          if (Denemo.ScriptRecording)
						if (idx_has_callback (the_keymap, command_idx))
							{
								append_scheme_call ((gchar *) command_name);
							}  
          //g_print("Single Key shortcut %s invokes %s\n", dnm_accelerator_name(event->keyval, event->state), command_name);
          return perform_command (command_name, event);
        }
      else
        {
          g_warning ("Error: action %i has no name", command_idx);
          return NULL;
        }
    }

  /*  we create a store for the prefix char and look to see if it is populated when a keystroke is received. If it is populated, we try for the two-key combination, {???else we try for the single key, and if that fails populate the store. OR if it fails clear store}. If the two-key combination works we clear the store. If the two-key combination fails we try for the single key, if that succeeds we clear the store if it fails we set the store to the unresolved keystroke.  */

  gchar *ret = NULL;
  if (prefix_store->len)
    {
      gchar *name = dnm_accelerator_name (event->keyval, event->state);
      //g_print("second key %s\n", name);
      g_string_append_printf (prefix_store, "%c%s", ',', name);
      command_idx = lookup_command_for_keybinding_name (Denemo.map, prefix_store->str);

      if (command_idx != -1)
        {
          const gchar *command_name = lookup_name_from_idx (the_keymap, command_idx);
          if (command_name)
            {
              if (Denemo.prefs.learning)
                {
                  KeyStrokeShow (prefix_store->str, command_idx, FALSE);
                }
					if (Denemo.ScriptRecording)
							if (idx_has_callback (the_keymap, command_idx))
								{
									append_scheme_call ((gchar *) command_name);
								}            
              ret = perform_command (command_name, event);
            }
        }
      else
        {                       //Two key name was not a binding
          ret = NULL;
          write_status (Denemo.gui);
          if ((Denemo.gui->view != DENEMO_MENU_VIEW) || Denemo.prefs.learning)
            {
							Denemo.prefs.learning = TRUE;
              KeyStrokeDecline (prefix_store->str);
            }
          toggle_to_drawing_area (TRUE);        //restore menus, in case the user is lost and needs to look up a keypress
          if (Denemo.gui->view != DENEMO_MENU_VIEW)
						toggle_to_drawing_area (TRUE);
        }
      g_string_assign (prefix_store, "");
      Denemo.continuations = NULL;
      return ret;
    }
  else
    {                           //no prefix stored 
      gchar *name = dnm_accelerator_name (event->keyval, event->state); //FIXME free name

      if ((Denemo.continuations = (GList *) g_hash_table_lookup (Denemo.map->continuations_table, name)))
        {
          GList *g;
          GString *continuations = g_string_new ("");
          for (g = Denemo.continuations; g; g = g->next)
            g_string_append_printf (continuations, "%s%s", (gchar *) g->data, ", or ");
          g_string_printf (prefix_store, "Prefix Key %s, waiting for key %stype Esc to abort", name, continuations->str);
          g_string_free (continuations, TRUE);
          gtk_statusbar_pop (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id);
          gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id, prefix_store->str);
          g_string_assign (prefix_store, name);
          if (Denemo.prefs.learning)
            {
              KeyStrokeAwait (name);
            }
          return "";            //continuation available
        }
      else
        {
          if ((Denemo.gui->view != DENEMO_MENU_VIEW) || Denemo.prefs.learning)
            {
							Denemo.prefs.learning = TRUE;
              KeyStrokeDecline (name);
            }
          toggle_to_drawing_area (TRUE);  //restore menus, in case the user is lost and needs to look up a keypress
          if (Denemo.gui->view != DENEMO_MENU_VIEW)
						toggle_to_drawing_area (TRUE);
        }
      return NULL;
    }
  return NULL;
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

  Denemo.keyboard_state |= (0xf & klock_mask (event->keyval));
  Denemo.keyboard_state ^= llock_mask (event->keyval);
  // if((event->keyval==GDK_Alt_L)||(event->keyval==GDK_Alt_R))
  //  Denemo.keyboard_state |= CHORD_MASK;
  set_midi_in_status ();

  //g_print("press Denemo %x state %x klock %x\n", Denemo.keyboard_state, event->state, klock_mask(event->keyval));

  // g_print("State eored %x\n", (lock_mask(event->keyval)^event->state));
  if (divert_key_event && !isModifier (event) && divert_key_id == Denemo.gui->id)
    {
      dnm_clean_event (event);
      *divert_key_event = event;
      //g_object_ref(event); FIXME do we need to keep it around?
      gtk_main_quit ();
      return TRUE;              //*is* reached main loop exits to the caller of the loop when it next gains control
    }

  (void) process_key_event (event, perform_command);
  return TRUE;                  //I think this means do not run any other handlers after this.
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
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (gui, 5);
}

/**
 * Goto the nearest b
 *
 */
void
go_to_B_key (DenemoGUI * gui)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (gui, 6);
}

/**
 * Goto the nearest c
 *
 */
void
go_to_C_key (DenemoGUI * gui)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
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
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (gui, 2);
}

/**
 * Goto the nearest f
 *
 */
void
go_to_F_key (DenemoGUI * gui)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (gui, 3);
}

/**
 * Goto the nearest g
 *
 */
void
go_to_G_key (DenemoGUI * gui)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (gui, 4);
}


/**
 * Move cursor by amount or in EDIT mode change the note by the amount
 */
static void
octave_shift_key (DenemoGUI * gui, gint amount)
{
  if (((DenemoStaff *) gui->si->currentstaff->data)->tone_store)
    {
      return;                   //FIXME create a function modify_tone, like delete_tone in pitchentry.c to do this sort of thing
    }
  else
    {
      if (gui->mode & (INPUTEDIT))
        {
          if (gui->si->currentobject)
            {
              objnode *thenote = nearestnote (gui->si->currentobject->data, gui->si->cursor_y);
              if (thenote)
                {
                  note copy = *((note *) thenote->data);
                  GList *direcs = ((note *) thenote->data)->directives;
                  store_for_undo_change (gui->si, gui->si->currentobject->data);
                  gui->si->undo_guard++;
                  delete_chordnote (gui);       //does not delete the directives.
                  gui->si->cursor_y = copy.mid_c_offset + amount;
                  insert_chordnote (gui);
                  changeenshift (gui->si->currentobject->data, gui->si->cursor_y, copy.enshift);
                  thenote = nearestnote (gui->si->currentobject->data, gui->si->cursor_y);
                  if (thenote)
                    ((note *) thenote->data)->directives = direcs;
                  gui->si->undo_guard--;
                  score_status (gui, TRUE);
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
  octave_shift_key (gui, 7);
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
  if (gui->mode & TRAVERSE)
    gtk_statusbar_push (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id, "Read Only");
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
  gtk_action_activate (mode);

}

/**
 * Toggle blank mode FIXME bitfields!!!
 *
 */
void
toggle_blank (DenemoGUI * gui)
{
  GtkAction *mode = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/Blank");
  gtk_action_activate (mode);
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
insert_chord_7key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 7, gui->mode, FALSE);
}

void
insert_chord_8key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 8, gui->mode, FALSE);
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
insert_rest_7key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 7, INPUTREST, TRUE);
}

void
insert_rest_8key (DenemoGUI * gui)
{
  dnm_insertchord (gui, 8, INPUTREST, TRUE);
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
insert_blankchord_7key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 7, INPUTBLANK, FALSE);

}

void
insert_blankchord_8key (DenemoGUI * gui)
{

  dnm_insertchord (gui, 8, INPUTBLANK, FALSE);

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
  object_insert (gui, newtupopen (2, 3));
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
  return insert_chordnote (gui);
}

gboolean
remove_tone_key (DenemoGUI * gui)
{
  return delete_chordnote (gui);
}

void
deletepreviousobject (DenemoGUI * gui)
{

  /* remove the object preceding the cursor, within the current measure */
  if (gui->si->cursor_x)
    {
      /* Then move the cursor back */
      movecursorleft (NULL);
      /* And delete */
      deleteobject (gui);
      /* if you are following a rhythmic pattern then backup the pattern */
#define g  (gui->rstep)
      if ((gui->mode & (INPUTEDIT) && g))
        {
#define CURRP ((RhythmPattern *)gui->currhythm->data)
          g = g->prev;          /* list is circular - should we stop at beginning? */
          if (gui->cstep)
            {
              gui->cstep = gui->cstep->prev ? gui->cstep->prev : g_list_last (CURRP->clipboard)->data;
            }
          if (((RhythmElement *) g->data)->icon)
            {
              GtkWidget *label = LABEL (CURRP->button);
              gtk_label_set_markup (GTK_LABEL (label), ((RhythmElement *) g->data)->icon);
            }
#undef CURRP
#undef g
        }

    }
  else
    {                           /* go to the previous measure, go to end of it, and start deleting there */
      if (gui->si->currentmeasure->prev)
        {
          DenemoScriptParam param;

          do
            {
              movetomeasureleft (&param);
              //go to end
              while (gui->si->currentobject && (gui->si->currentobject->next))
                {
                  gui->si->currentobject = gui->si->currentobject->next;
                  gui->si->cursor_x++;
                }
            }
          while (param.status && !gui->si->currentobject);


          if (gui->si->currentobject)
            {
              movecursorright (NULL);
              deletepreviousobject (gui);
            }
        }
    }
}

void
sharpen_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (gui->si, DENEMO_STEMUP);
  else
    incrementenshift (gui, 1);
}

void
stem_up (DenemoGUI * gui)
{
  sharpen_key (gui);
}

void
flatten_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (gui->si, DENEMO_STEMDOWN);
  else
    incrementenshift (gui, -1);
}

void
pending_sharpen (DenemoGUI * gui)
{
  Denemo.gui->si->pending_enshift++;
  if (Denemo.gui->si->pending_enshift > 2)
    Denemo.gui->si->pending_enshift = 2;
}

void
pending_flatten (DenemoGUI * gui)
{
  Denemo.gui->si->pending_enshift--;
  if (Denemo.gui->si->pending_enshift < -2)
    Denemo.gui->si->pending_enshift = -2;
}

void
stem_down (DenemoGUI * gui)
{
  flatten_key (gui);
}

/* insert a duplicate note and tie to it */
void
tie_notes_key (DenemoGUI * gui)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  if (curmudelaobj && curmudelaobj->type == CHORD && ((chord *) curmudelaobj->object)->notes)
    {

      insertion_point (gui->si);
      object_insert (gui, dnm_clone_object (curmudelaobj));
      movecursorleft (NULL);
      movecursorleft (NULL);
      toggle_tie (NULL, NULL);
      movecursorright (NULL);
      movecursorright (NULL);
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
force_cautionary (DenemoGUI * gui)
{
  DenemoObject *theobj = gui->si->currentobject ? (DenemoObject *) gui->si->currentobject->data : NULL;
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
      delete_chordnote (gui);
      insert_chordnote (gui);
    }
  // addtone(theobj, si->cursor_y, si->cursoraccs[si->staffletter_y],
  //       si->cursorclef);

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
 * Wrapper function to create new bass_8 clef and insert into the score
 */
void
newcleff8 (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_F_8_CLEF));
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
 * Wrapper function to create new french clef and insert into the score
 */
void
newcleffrench (DenemoGUI * gui)
{
  object_insert (gui, dnm_newclefobj (DENEMO_FRENCH_CLEF));
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
setcleff8 (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_F_8_CLEF);
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

void
setcleffrench (DenemoGUI * gui)
{
  DenemoStaff *curstaff = (DenemoStaff *) gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (gui->si, curstaff, DENEMO_FRENCH_CLEF);
}
