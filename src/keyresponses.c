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
#include "kbd-custom.h"
#include "audiointerface.h"
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
      warningdialog (_("Recursive key capture not possible!"));    /* we could make a stack of them instead ... */
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
  execute_callback_from_name (command_name);
  call_out_to_guile ("(define DenemoKeypressActivatedCommand #f)");
  // note Denemo.gui = Denemo.gui; may have changed as a result of executing the command
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
          if (Denemo.prefs.immediateplayback)
              play_note (DEFAULT_BACKEND, 0, 9, 61, 300, 127 * Denemo.gui->si->master_volume);
          //gtk_statusbar_pop (GTK_STATUSBAR (Denemo.statusbar), Denemo.status_context_id);
          gtk_label_set_text (GTK_LABEL (Denemo.statuslabel), prefix_store->str);
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
adjust_measure_less_width_key (DenemoScriptParam *param)
{
  adjustmeasurewidth (Denemo.gui->si, -10);
}

/**
 * Enlarge the measure width by 10 pixels
 */
void
adjust_measure_more_width_key (DenemoScriptParam *param)
{
  adjustmeasurewidth (Denemo.gui->si, 10);
}

/**
 * Reduce the staff height by 10 pixels
 */
void
adjust_staff_less_height_key (DenemoScriptParam *param)
{
  adjuststaffheight (Denemo.gui->si, -10);
}

/**
 * Enlarge the staff height by 10 pixels
 */
void
adjust_staff_more_height_key (DenemoScriptParam *param)
{
  adjuststaffheight (Denemo.gui->si, 10);

}

/**
 * Remove selection marker
 */
void
unset_selection_key (DenemoScriptParam *param)
{
  Denemo.gui->si->markstaffnum = 0;
}

/**
 * Insert quarter note into score
 */
void
insert_chord_key (DenemoScriptParam *param)
{
  dnm_insertchord (Denemo.gui, 2, Denemo.gui->mode, FALSE);
}

void
go_to_key(gchar note, DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, ((note + 5 - 'A') % 7));
}

/**
 * Goto the nearest a
 *
 */
void
go_to_A_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 5);
}

/**
 * Goto the nearest b
 *
 */
void
go_to_B_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 6);
}

/**
 * Goto the nearest c
 *
 */
void
go_to_C_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 0);
}

/**
 * Goto the nearest d
 *
 */
void
go_to_D_key (DenemoScriptParam *param)
{
  shiftcursor (Denemo.gui, 1);
}

/**
 * Goto the nearest e
 *
 */
void
go_to_E_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 2);
}

/**
 * Goto the nearest f
 *
 */
void
go_to_F_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 3);
}

/**
 * Goto the nearest g
 *
 */
void
go_to_G_key (DenemoScriptParam *param)
{
  Denemo.gui->last_source = INPUTKEYBOARD;
  shiftcursor (Denemo.gui, 4);
}


/**
 * Move cursor by amount or in EDIT mode change the note by the amount
 */
static void
octave_shift_key (DenemoScriptParam *param, gint amount)
{
  if (((DenemoStaff *) Denemo.gui->si->currentstaff->data)->tone_store)
    {
      return;                   //FIXME create a function modify_tone, like delete_tone in pitchentry.c to do this sort of thing
    }
  else
    {
      if (Denemo.gui->mode & (INPUTEDIT))
        {
          if (Denemo.gui->si->currentobject)
            {
              objnode *thenote = nearestnote (Denemo.gui->si->currentobject->data, Denemo.gui->si->cursor_y);
              if (thenote)
                {
                  note copy = *((note *) thenote->data);
                  GList *direcs = ((note *) thenote->data)->directives;
                  store_for_undo_change (Denemo.gui->si, Denemo.gui->si->currentobject->data);
                  Denemo.gui->si->undo_guard++;
                  delete_chordnote (Denemo.gui);       //does not delete the directives.
                  Denemo.gui->si->cursor_y = copy.mid_c_offset + amount;
                  insert_chordnote (Denemo.gui);
                  changeenshift (Denemo.gui->si->currentobject->data, Denemo.gui->si->cursor_y, copy.enshift);
                  thenote = nearestnote (Denemo.gui->si->currentobject->data, Denemo.gui->si->cursor_y);
                  if (thenote)
                    ((note *) thenote->data)->directives = direcs;
                  Denemo.gui->si->undo_guard--;
                  score_status (Denemo.gui, TRUE);
                }
            }
        }
      else
        Denemo.gui->si->cursor_y += amount;
    }
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw(Denemo.scorearea);
}

/**
 * Move cursor an octave up
 */
void
octave_up_key (DenemoScriptParam *param)
{
  octave_shift_key (param, 7);
}




/**
 * Move cursor an octave down
 */
void
octave_down_key (DenemoScriptParam *param)
{
  octave_shift_key (param, -7);
}

/**
 * change to read only mode
 * only score traversal available FIXME wrong name
 */
void
default_mode (DenemoScriptParam *param)
{
  Denemo.gui->mode ^= TRAVERSE;
  if (Denemo.gui->mode & TRAVERSE)
    gtk_label_set_text (GTK_LABEL (Denemo.statuslabel), "Read Only");
  g_print ("Mode %d\n", Denemo.gui->mode);
  displayhelper (Denemo.gui);
}




/**
 * Toggle into rest mode
 * 
 */
void
rest_toggle_key (DenemoScriptParam *param)
{
  GtkAction *mode = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/Rest");
  gtk_action_activate (mode);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

/**
 * Toggle blank mode FIXME bitfields!!!
 *
 */
void
toggle_blank (DenemoScriptParam *param)
{
  GtkAction *mode = gtk_ui_manager_get_action (Denemo.ui_manager, "/MainMenu/ModeMenu/Blank");
  gtk_action_activate (mode);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

/**
 * Add measure to end of staff / score
 */
void
append_measure_key (DenemoScriptParam *param)
{
  appendmeasures (Denemo.gui->si, 1);
}

void
append_measure_score (DenemoScriptParam *param)
{
  appendmeasurestoentirescore (Denemo.gui->si, 1);
}

/** 
 * Insert measure at the current 
 */
void
insert_measure_key (DenemoScriptParam *param)
{
  dnm_insertmeasures (Denemo.gui->si, 1);
}

void
insert_chord_xkey (gint duration, DenemoScriptParam *param)
{
  dnm_insertchord (Denemo.gui, duration, Denemo.gui->mode, FALSE);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

void
insert_chord_0key (DenemoScriptParam *param)
{
  insert_chord_xkey(0, param);
}

void
insert_chord_1key (DenemoScriptParam *param)
{
  insert_chord_xkey(1, param);
}

void
insert_chord_2key (DenemoScriptParam *param)
{
  insert_chord_xkey(2, param);
}

void
insert_chord_3key (DenemoScriptParam *param)
{
  insert_chord_xkey(3, param);
}

void
insert_chord_4key (DenemoScriptParam *param)
{
  insert_chord_xkey(4, param);
}

void
insert_chord_5key (DenemoScriptParam *param)
{
  insert_chord_xkey(5, param);
}

void
insert_chord_6key (DenemoScriptParam *param)
{
  insert_chord_xkey(6, param);
}

void
insert_chord_7key (DenemoScriptParam *param)
{
  insert_chord_xkey(7, param);
}

void
insert_chord_8key (DenemoScriptParam *param)
{
  insert_chord_xkey(8, param);
}

void
insert_rest_xkey(gint duration, DenemoScriptParam* param)
{
  dnm_insertchord (Denemo.gui, duration, INPUTREST, TRUE);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}


void
insert_rest_0key (DenemoScriptParam *param)
{
  insert_rest_xkey(0, param);
}

void
insert_rest_1key (DenemoScriptParam *param)
{
  insert_rest_xkey(1, param);
}

void
insert_rest_2key (DenemoScriptParam *param)
{
  insert_rest_xkey(2, param);
}

void
insert_rest_3key (DenemoScriptParam *param)
{
  insert_rest_xkey(3, param);
}

void
insert_rest_4key (DenemoScriptParam *param)
{
  insert_rest_xkey(4, param);
}

void
insert_rest_5key (DenemoScriptParam *param)
{
  insert_rest_xkey(5, param);
}

void
insert_rest_6key (DenemoScriptParam *param)
{
  insert_rest_xkey(6, param);
}

void
insert_rest_7key (DenemoScriptParam *param)
{
  insert_rest_xkey(7, param);
}

void
insert_rest_8key (DenemoScriptParam *param)
{
  insert_rest_xkey(8, param);
}

void
insert_blankchord_xkey (gint duration, DenemoScriptParam *param)
{
  dnm_insertchord (Denemo.gui, duration, INPUTBLANK, FALSE);
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

void
insert_blankchord_0key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(0, param);
}

void
insert_blankchord_1key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(1, param);
}

void
insert_blankchord_2key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(2, param);
}

void
insert_blankchord_3key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(3, param);
}

void
insert_blankchord_4key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(4, param);
}

void
insert_blankchord_5key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(5, param);
}

void
insert_blankchord_6key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(6, param);
}

void
insert_blankchord_7key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(7, param);
}

void
insert_blankchord_8key (DenemoScriptParam *param)
{
  insert_blankchord_xkey(8, param);
}


void
insert_duplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, DUPLET);
}

void
insert_triplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, TRIPLET);
}

void
start_triplet (DenemoScriptParam *param)
{
  insertion_point (Denemo.gui->si);
  object_insert (Denemo.gui, newtupopen (2, 3));
}

void
end_tuplet (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, newtupclose ());
}

void
insert_quadtuplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, QUADTUPLET);
}

void
insert_quintuplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, QUINTUPLET);
}

void
insert_sextuplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, SEXTUPLET);
}

void
insert_septuplet (DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.gui, SEPTUPLET);
}



gboolean
add_tone_key (DenemoScriptParam *param)
{
  return insert_chordnote (Denemo.gui);
}

gboolean
remove_tone_key (DenemoScriptParam *param)
{
  return delete_chordnote (Denemo.gui);
}

void
deletepreviousobject (DenemoScriptParam *param)
{

  /* remove the object preceding the cursor, within the current measure */
  if (Denemo.gui->si->cursor_x)
    {
      /* Then move the cursor back */
      movecursorleft (NULL);
      /* And delete */
      deleteobject (Denemo.gui);
      /* if you are following a rhythmic pattern then backup the pattern */
#define g  (Denemo.gui->rstep)
      if ((Denemo.gui->mode & (INPUTEDIT) && g))
        {
#define CURRP ((RhythmPattern *)Denemo.gui->currhythm->data)
          g = g->prev;          /* list is circular - should we stop at beginning? */
          if (Denemo.gui->cstep)
            {
              Denemo.gui->cstep = Denemo.gui->cstep->prev ? Denemo.gui->cstep->prev : g_list_last (CURRP->clipboard)->data;
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
      if (Denemo.gui->si->currentmeasure->prev)
        {
          DenemoScriptParam param;

          do
            {
              movetomeasureleft (&param);
              //go to end
              while (Denemo.gui->si->currentobject && (Denemo.gui->si->currentobject->next))
                {
                  Denemo.gui->si->currentobject = Denemo.gui->si->currentobject->next;
                  Denemo.gui->si->cursor_x++;
                }
            }
          while (param.status && !Denemo.gui->si->currentobject);


          if (Denemo.gui->si->currentobject)
            {
              movecursorright (NULL);
              deletepreviousobject (NULL);
            }
        }
    }
}

void
sharpen_key (DenemoScriptParam *param)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (Denemo.gui->si->currentobject ? Denemo.gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (Denemo.gui->si, DENEMO_STEMUP);
  else
    incrementenshift (Denemo.gui, 1);
}

void
stem_up (DenemoScriptParam *param)
{
  sharpen_key (param);
}

void
flatten_key (DenemoScriptParam *param)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (Denemo.gui->si->currentobject ? Denemo.gui->si->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (Denemo.gui->si, DENEMO_STEMDOWN);
  else
    incrementenshift (Denemo.gui, -1);
}

void
pending_sharpen (DenemoScriptParam *param)
{
  Denemo.gui->si->pending_enshift++;
  if (Denemo.gui->si->pending_enshift > 2)
    Denemo.gui->si->pending_enshift = 2;
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

void
pending_flatten (DenemoScriptParam *param)
{
  Denemo.gui->si->pending_enshift--;
  if (Denemo.gui->si->pending_enshift < -2)
    Denemo.gui->si->pending_enshift = -2;
  displayhelper (Denemo.gui);
  score_status(Denemo.gui, TRUE);
}

void
stem_down (DenemoScriptParam *param)
{
  flatten_key (param);
}

/* insert a duplicate note and tie to it */
void
tie_notes_key (DenemoScriptParam *param)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (Denemo.gui->si->currentobject ? Denemo.gui->si->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  if (curmudelaobj && curmudelaobj->type == CHORD && ((chord *) curmudelaobj->object)->notes)
    {

      insertion_point (Denemo.gui->si);
      object_insert (Denemo.gui, dnm_clone_object (curmudelaobj));
      movecursorleft (NULL);
      movecursorleft (NULL);
      toggle_tie (NULL, NULL);
      movecursorright (NULL);
      movecursorright (NULL);
    }
}

void
add_dot_key (DenemoScriptParam *param)
{
  changedots (Denemo.gui->si, 1);
}

void
remove_dot_key (DenemoScriptParam *param)
{
  changedots (Denemo.gui->si, -1);
}


void
force_cautionary (DenemoScriptParam *param)
{
  DenemoObject *theobj = Denemo.gui->si->currentobject ? (DenemoObject *) Denemo.gui->si->currentobject->data : NULL;
  if (theobj && theobj->type == CHORD)
    caution (Denemo.gui->si);
}

void
change_pitch (DenemoScriptParam *param)
{
  if (Denemo.gui->mode & INPUTEDIT)
    {
      //DenemoObject *theobj =
      //  si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
      delete_chordnote (Denemo.gui);
      insert_chordnote (Denemo.gui);
    }
  // addtone(theobj, si->cursor_y, si->cursoraccs[si->staffletter_y],
  //       si->cursorclef);

}




/**
 * Wrapper function to create new treble clef and insert into the score
 */
void
newcleftreble (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_TREBLE_CLEF));
}

/**
 * Wrapper function to create new bass clef and insert into the score
 */
void
newclefbass (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_BASS_CLEF));
}

/**
 * Wrapper function to create new alto clef and insert into the score
 */
void
newclefalto (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_ALTO_CLEF));
}

/**
 * Wrapper function to create new treble_8 clef and insert into the score
 */
void
newclefg8 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_G_8_CLEF));
}

/**
 * Wrapper function to create new bass_8 clef and insert into the score
 */
void
newcleff8 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_F_8_CLEF));
}

/**
 * Wrapper function to create new tenor clef and insert into the score
 */
void
newcleftenor (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_TENOR_CLEF));
}

/**
 * Wrapper function to create new soprano clef and insert into the score
 */
void
newclefsoprano (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_SOPRANO_CLEF));
}

/**
 * Wrapper function to create new french clef and insert into the score
 */
void
newcleffrench (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newclefobj (DENEMO_FRENCH_CLEF));
}

/**
 * Wrapper function to create new 4/4 time sig and insert into the score
 */
void
newtimesig44 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (4, 4));
}

/**
 * Wrapper function to create new 2/4 time sig and insert into the score
 */
void
newtimesig24 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (2, 4));
}

/**
 * Wrapper function to create new 3/4 time sig and insert into the score
 */
void
newtimesig34 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (3, 4));
}

/**
 * Wrapper function to create new 6/4 time sig and insert into the score
 */
void
newtimesig64 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (6, 4));
}

/**
 * Wrapper function to create new 5/4 time sig and insert into the score
 */
void
newtimesig54 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (5, 4));
}

/**
 * Wrapper function to create new 3/8 time sig and insert into the score
 */
void
newtimesig38 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (3, 8));
}

/**
 * Wrapper function to create new 6/8 time sig and insert into the score
 */
void
newtimesig68 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (6, 8));
}

/**
 * Wrapper function to create new 9/8 time sig and insert into the score
 */
void
newtimesig98 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (9, 8));
}

/**
 * Wrapper function to create new 12/8 time sig and insert into the score
 */
void
newtimesig128 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (12, 8));
}

/**
 * Wrapper function to create new 2/2 time sig and insert into the score
 */
void
newtimesig22 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (2, 2));
}

/**
 * Wrapper function to create new 3/2 time sig and insert into the score
 */
void
newtimesig32 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (3, 2));
}

/**
 * Wrapper function to create new 4/2 time sig and insert into the score
 */
void
newtimesig42 (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newtimesigobj (4, 2));
}



void
newkeysigcmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (0, 1, 0));
}

void
newkeysiggmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (1, 1, 0));
}

void
newkeysigdmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (2, 1, 0));
}

void
newkeysigamaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (3, 1, 0));
}

void
newkeysigemaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (4, 1, 0));
}

void
newkeysigbmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (5, 1, 0));
}

void
newkeysigfsharpmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (6, 1, 0));
}

void
newkeysigcsharpmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (7, 1, 0));
}

void
newkeysigfmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-1, 1, 0));
}

void
newkeysigbflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-2, 1, 0));
}

void
newkeysigeflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-3, 1, 0));
}

void
newkeysigaflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-4, 1, 0));
}

void
newkeysigdflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-5, 1, 0));
}

void
newkeysiggflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-6, 1, 0));
}

void
newkeysigcflatmaj (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-7, 1, 0));
}

void
newkeysigamin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (0, 0, 0));
}

void
newkeysigemin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (1, 0, 0));
}

void
newkeysigbmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (2, 0, 0));
}

void
newkeysigfsharpmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (3, 0, 0));
}

void
newkeysigcsharpmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (4, 0, 0));
}

void
newkeysiggsharpmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (5, 0, 0));
}

void
newkeysigdsharpmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (6, 0, 0));
}

void
newkeysigasharpmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (7, 0, 0));
}

void
newkeysigdmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-1, 0, 0));
}

void
newkeysiggmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-2, 0, 0));
}

void
newkeysigcmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-3, 0, 0));
}

void
newkeysigfmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-4, 0, 0));
}

void
newkeysigbflatmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-5, 0, 0));
}

void
newkeysigeflatmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-6, 0, 0));
}

void
newkeysigaflatmin (DenemoScriptParam *param)
{
  object_insert (Denemo.gui, dnm_newkeyobj (-7, 0, 0));
}


//Functions to set the initial key signature
void
setkeysigcmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 1);
  //displayhelper(si);
}

void
setkeysiggmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 1);
  //displayhelper(si);
}

void
setkeysigdmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 1);
}

void
setkeysigamaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 1);
}

void
setkeysigemaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 1);
}

void
setkeysigbmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 1);
}

void
setkeysigfsharpmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 1);
}

void
setkeysigcsharpmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 1);
}

void
setkeysigfmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 1);
}

void
setkeysigbflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 1);
}

void
setkeysigeflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 1);
}

void
setkeysigaflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 1);
}

void
setkeysigdflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 1);
}

void
setkeysiggflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 1);
}

void
setkeysigcflatmaj (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 1);
}

void
setkeysigamin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 0);
}

void
setkeysigemin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 0);
}

void
setkeysigbmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 0);
}

void
setkeysigfsharpmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 0);
}

void
setkeysigcsharpmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 0);
}

void
setkeysiggsharpmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 0);
}

void
setkeysigdsharpmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 0);
}

void
setkeysigasharpmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 0);
}

void
setkeysigdmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 0);
}

void
setkeysiggmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 0);
}

void
setkeysigcmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 0);
}

void
setkeysigfmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 0);
}

void
setkeysigbflatmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 0);
}

void
setkeysigeflatmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 0);
}

void
setkeysigaflatmin (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 0);
}


void
settimesig22 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 2, 2, TRUE);
}

void
settimesig42 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 4, 2, TRUE);
}

void
settimesig32 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 3, 2, TRUE);
}

void
settimesig44 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 4, 4, TRUE);
}

void
settimesig54 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 5, 4, TRUE);
}

void
settimesig24 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 2, 4, TRUE);
}

void
settimesig34 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 3, 4, TRUE);
}

void
settimesig68 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 6, 8, TRUE);
}

void
settimesig128 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 12, 8, TRUE);
}

void
settimesig38 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 3, 8, TRUE);
}

void
settimesig98 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 9, 8, TRUE);
}

void
settimesig64 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialtimesig (Denemo.gui->si, curstaff, 6, 4, TRUE);
}

void
setcleftreble (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_TREBLE_CLEF);
}

void
setclefbass (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_BASS_CLEF);
}

void
setclefg8 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_G_8_CLEF);
}

void
setcleff8 (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_F_8_CLEF);
}

void
setclefalto (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_ALTO_CLEF);
}

void
setcleftenor (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_TENOR_CLEF);
}

void
setclefsoprano (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_SOPRANO_CLEF);
}

void
setcleffrench (DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.gui->si->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.gui->si, curstaff, DENEMO_FRENCH_CLEF);
}

/*******************************************************************************
* DURATION COMMANDS
******************************************************************************/

void InsertRest(gint duration){
  highlight_rest(Denemo.gui, duration);
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTINSERT|INPUTREST;
  insert_chord_xkey(duration, NULL);
  Denemo.gui->mode = mode;
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void InsertDur(gint duration){
  highlight_duration(Denemo.gui, duration);
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTINSERT|INPUTNORMAL;
  insert_chord_xkey(duration, NULL);
  Denemo.gui->mode = mode;
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void ChangeDur(gint duration){
  gint mode = Denemo.gui->mode;
  gboolean appending = Denemo.gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  Denemo.gui->mode = INPUTEDIT|INPUTNORMAL;
  insert_chord_xkey(duration, NULL);
  Denemo.gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void SetDur(gint duration){
  highlight_duration(Denemo.gui, duration);
}

void Dur (gint duration) {
  DenemoGUI *gui = Denemo.gui;
 if(Denemo.gui->mode&INPUTINSERT)
   highlight_duration(Denemo.gui, duration);
 else
 if( !(Denemo.gui->mode&INPUTRHYTHM) && (Denemo.gui->mode&INPUTEDIT) && (!Denemo.gui->si->cursor_appending))
   ChangeDur (duration);
else {
 insert_chord_xkey(duration, NULL);
   highlight_duration(Denemo.gui, duration);
  score_status(Denemo.gui, TRUE);
 displayhelper(Denemo.gui);
 }
}

/*******************************************************************************
* NOTE COMMANDS
******************************************************************************/

void ChangeTo(gchar note){
  DenemoGUI *gui = Denemo.gui;
  gboolean appending = Denemo.gui->si->cursor_appending;
  if(appending)
    movecursorleft(NULL);
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTEDIT|INPUTNORMAL;
  go_to_key(note, NULL);
  Denemo.gui->mode = mode;
  if(appending)
    movecursorright(NULL);
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void MoveTo(gchar note){
  DenemoGUI *gui = Denemo.gui;
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_key(note, NULL);
  Denemo.gui->mode = mode;
  displayhelper(Denemo.gui);
}

void Insert(gchar note){
  DenemoGUI *gui = Denemo.gui;
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_key(note, NULL);
  Denemo.gui->mode = mode;
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void AddNote(gchar note){
  DenemoGUI *gui = Denemo.gui;
  movecursorright(NULL);
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTINSERT|INPUTNORMAL;
  go_to_key(note, NULL);
  Denemo.gui->mode = mode;
  movecursorleft(NULL);
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void Add(gchar note){
  DenemoGUI *gui = Denemo.gui;
  gint mode = Denemo.gui->mode;
  Denemo.gui->mode = INPUTCLASSIC|INPUTNORMAL;
  go_to_key(note, NULL);
  add_tone_key(NULL);
  Denemo.gui->mode = mode;
  score_status(Denemo.gui, TRUE);
  displayhelper(Denemo.gui);
}

void Dur0(GtkAction *action, gpointer param) {
  Dur(0);
}
void ChangeDur0(GtkAction *action, gpointer param){
  ChangeDur(0);
}
void InsertDur0(GtkAction *action, gpointer param){
  InsertDur(0);
}
void InsertRest0(GtkAction *action, gpointer param){
  InsertRest(0);
}
void SetDur0(GtkAction *action, gpointer param){
  SetDur(0);
}
void Dur1(GtkAction *action, gpointer param) {
  Dur(1);
}
void ChangeDur1(GtkAction *action, gpointer param){
  ChangeDur(1);
}
void InsertDur1(GtkAction *action, gpointer param){
  InsertDur(1);
}
void InsertRest1(GtkAction *action, gpointer param){
  InsertRest(1);
}
void SetDur1(GtkAction *action, gpointer param){
  SetDur(1);
}
void Dur2(GtkAction *action, gpointer param) {
  Dur(2);
}
void ChangeDur2(GtkAction *action, gpointer param){
  ChangeDur(2);
}
void InsertDur2(GtkAction *action, gpointer param){
  InsertDur(2);
}
void InsertRest2(GtkAction *action, gpointer param){
  InsertRest(2);
}
void SetDur2(GtkAction *action, gpointer param){
  SetDur(2);
}
void Dur3(GtkAction *action, gpointer param) {
  Dur(3);
}
void ChangeDur3(GtkAction *action, gpointer param){
  ChangeDur(3);
}
void InsertDur3(GtkAction *action, gpointer param){
  InsertDur(3);
}
void InsertRest3(GtkAction *action, gpointer param){
  InsertRest(3);
}
void SetDur3(GtkAction *action, gpointer param){
  SetDur(3);
}
void Dur4(GtkAction *action, gpointer param) {
  Dur(4);
}
void ChangeDur4(GtkAction *action, gpointer param){
  ChangeDur(4);
}
void InsertDur4(GtkAction *action, gpointer param){
  InsertDur(4);
}
void InsertRest4(GtkAction *action, gpointer param){
  InsertRest(4);
}
void SetDur4(GtkAction *action, gpointer param){
  SetDur(4);
}
void Dur5(GtkAction *action, gpointer param) {
  Dur(5);
}
void ChangeDur5(GtkAction *action, gpointer param){
  ChangeDur(5);
}
void InsertDur5(GtkAction *action, gpointer param){
  InsertDur(5);
}
void InsertRest5(GtkAction *action, gpointer param){
  InsertRest(5);
}
void SetDur5(GtkAction *action, gpointer param){
  SetDur(5);
}
void Dur6(GtkAction *action, gpointer param) {
  Dur(6);
}
void ChangeDur6(GtkAction *action, gpointer param){
  ChangeDur(6);
}
void InsertDur6(GtkAction *action, gpointer param){
  InsertDur(6);
}
void InsertRest6(GtkAction *action, gpointer param){
  InsertRest(6);
}
void SetDur6(GtkAction *action, gpointer param){
  SetDur(6);
}
void Dur7(GtkAction *action, gpointer param) {
  Dur(7);
}
void ChangeDur7(GtkAction *action, gpointer param){
  ChangeDur(7);
}
void InsertDur7(GtkAction *action, gpointer param){
  InsertDur(7);
}
void InsertRest7(GtkAction *action, gpointer param){
  InsertRest(7);
}
void SetDur7(GtkAction *action, gpointer param){
  SetDur(7);
}
void Dur8(GtkAction *action, gpointer param) {
  Dur(8);
}
void ChangeDur8(GtkAction *action, gpointer param){
  ChangeDur(8);
}
void InsertDur8(GtkAction *action, gpointer param){
  InsertDur(8);
}
void InsertRest8(GtkAction *action, gpointer param){
  InsertRest(8);
}
void SetDur8(GtkAction *action, gpointer param){
  SetDur(8);
}
void InsertA(GtkAction *action, gpointer param){
  Insert('A');
}
void AddNoteA(GtkAction *action, gpointer param){
  AddNote('A');
}
void AddA(GtkAction *action, gpointer param){
  Add('A');
}
void ChangeToA(GtkAction *action, gpointer param){
  ChangeTo('A');
}
void MoveToA(GtkAction *action, gpointer param){
  MoveTo('A');
}
void InsertB(GtkAction *action, gpointer param){
  Insert('B');
}
void AddNoteB(GtkAction *action, gpointer param){
  AddNote('B');
}
void AddB(GtkAction *action, gpointer param){
  Add('B');
}
void ChangeToB(GtkAction *action, gpointer param){
  ChangeTo('B');
}
void MoveToB(GtkAction *action, gpointer param){
  MoveTo('B');
}
void InsertC(GtkAction *action, gpointer param){
  Insert('C');
}
void AddNoteC(GtkAction *action, gpointer param){
  AddNote('C');
}
void AddC(GtkAction *action, gpointer param){
  Add('C');
}
void ChangeToC(GtkAction *action, gpointer param){
  ChangeTo('C');
}
void MoveToC(GtkAction *action, gpointer param){
  MoveTo('C');
}
void InsertD(GtkAction *action, gpointer param){
  Insert('D');
}
void AddNoteD(GtkAction *action, gpointer param){
  AddNote('D');
}
void AddD(GtkAction *action, gpointer param){
  Add('D');
}
void ChangeToD(GtkAction *action, gpointer param){
  ChangeTo('D');
}
void MoveToD(GtkAction *action, gpointer param){
  MoveTo('D');
}
void InsertE(GtkAction *action, gpointer param){
  Insert('E');
}
void AddNoteE(GtkAction *action, gpointer param){
  AddNote('E');
}
void AddE(GtkAction *action, gpointer param){
  Add('E');
}
void ChangeToE(GtkAction *action, gpointer param){
  ChangeTo('E');
}
void MoveToE(GtkAction *action, gpointer param){
  MoveTo('E');
}
void InsertF(GtkAction *action, gpointer param){
  Insert('F');
}
void AddNoteF(GtkAction *action, gpointer param){
  AddNote('F');
}
void AddF(GtkAction *action, gpointer param){
  Add('F');
}
void ChangeToF(GtkAction *action, gpointer param){
  ChangeTo('F');
}
void MoveToF(GtkAction *action, gpointer param){
  MoveTo('F');
}
void InsertG(GtkAction *action, gpointer param){
  Insert('G');
}
void AddNoteG(GtkAction *action, gpointer param){
  AddNote('G');
}
void AddG(GtkAction *action, gpointer param){
  Add('G');
}
void ChangeToG(GtkAction *action, gpointer param){
  ChangeTo('G');
}
void MoveToG(GtkAction *action, gpointer param){
  MoveTo('G');
}