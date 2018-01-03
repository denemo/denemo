/* keyresponses.c
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */

#include <string.h>
#include "core/kbd-custom.h"
#include "command/keyresponses.h"
#include "core/utils.h"
#include "command/tuplet.h"
#include "core/view.h"
#include "command/commandfuncs.h"
#include "core/kbd-custom.h"
#include "audio/audiointerface.h"
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
  divert_key_id = Denemo.project->id;
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
     if(!Denemo.keyboard_state_locked)
          {
              Denemo.keyboard_state ^= (0xf & klock_mask (event->keyval));
              if ((event->keyval == GDK_Alt_L) || (event->keyval == GDK_Alt_R))
                {
                  if ((Denemo.keyboard_state & CHORD_MASK)) //At least one note has been entered in a chord
                    next_insert_or_editable_note ();
                  Denemo.keyboard_state &= ~CHORD_MASK;
                }
              set_midi_in_status ();
        }
  //g_print("release %x state %x\n", Denemo.keyboard_state, event->state);
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
  // note Denemo.project = Denemo.project; may have changed as a result of executing the command
#ifdef TESTING_REPEATED_XPOSITION_UPDATE
  if (Denemo.project->movement)
    displayhelper (Denemo.project);
#endif
  return NULL;
}

//return the value of perform_command if executed or "" if keypress is part of a two-key shortcut, or NULL toherwise
gchar *
process_key_event (GdkEventKey * event, gchar * perform_command ())
{
  keymap *the_keymap = Denemo.map;
  //g_debug("\n********\nCaps Lock %x?\n\n********\nShifted %x?\n", event->state&GDK_LOCK_MASK,          event->state&GDK_SHIFT_MASK     );
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
            {
                append_scheme_call ((gchar *) command_name);
            }
          //g_debug("Single Key shortcut %s invokes %s\n", dnm_accelerator_name(event->keyval, event->state), command_name);
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
      //g_debug("second key %s\n", name);
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
                {
                append_scheme_call ((gchar *) command_name);
                }
              ret = perform_command (command_name, event);
            }
        }
      else
        {                       //Two key name was not a binding
          ret = NULL;
          write_status (Denemo.project);
          if ((Denemo.project->view != DENEMO_MENU_VIEW) || Denemo.prefs.learning)
            {
                            Denemo.prefs.learning = TRUE;
              KeyStrokeDecline (prefix_store->str);
            }
          toggle_to_drawing_area (TRUE);        //restore menus, in case the user is lost and needs to look up a keypress
          if (Denemo.project->view != DENEMO_MENU_VIEW)
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
            g_string_append_printf (continuations, "%s%s", (gchar *) g->data, _(", or "));
          g_string_printf (prefix_store, _( "Prefix Key %s, waiting for key %stype Esc to abort"), name, continuations->str);
          g_string_free (continuations, TRUE);
          if (Denemo.prefs.immediateplayback)
              play_note (DEFAULT_BACKEND, 0, 9, 61, 300, 127 * Denemo.project->movement->master_volume);
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
          if ((Denemo.project->view != DENEMO_MENU_VIEW) || Denemo.prefs.learning)
            {
                            Denemo.prefs.learning = TRUE;
              KeyStrokeDecline (name);
            }
          toggle_to_drawing_area (TRUE);  //restore menus, in case the user is lost and needs to look up a keypress
          if (Denemo.project->view != DENEMO_MENU_VIEW)
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
      if(!Denemo.keyboard_state_locked)
          {
              Denemo.keyboard_state |= (0xf & klock_mask (event->keyval));
              Denemo.keyboard_state ^= llock_mask (event->keyval);
              // if((event->keyval==GDK_Alt_L)||(event->keyval==GDK_Alt_R))
              //  Denemo.keyboard_state |= CHORD_MASK;
              set_midi_in_status ();
          }
  //g_print("press Denemo %x state %x klock %x\n", Denemo.keyboard_state, event->state, klock_mask(event->keyval));

  //g_debug("State eored %x\n", (lock_mask(event->keyval)^event->state));
  if (divert_key_event && !isModifier (event) && divert_key_id == Denemo.project->id)
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
adjust_measure_less_width_key (DenemoAction* action, DenemoScriptParam *param)
{
  adjustmeasurewidth (Denemo.project->movement, -10);
}

/**
 * Enlarge the measure width by 10 pixels
 */
void
adjust_measure_more_width_key (DenemoAction* action, DenemoScriptParam *param)
{
  adjustmeasurewidth (Denemo.project->movement, 10);
}

/**
 * Reduce the staff height by 10 pixels
 */
void
adjust_staff_less_height_key (DenemoAction* action, DenemoScriptParam *param)
{
  adjuststaffheight (Denemo.project->movement, -10);
}

/**
 * Enlarge the staff height by 10 pixels
 */
void
adjust_staff_more_height_key (DenemoAction* action, DenemoScriptParam *param)
{
  adjuststaffheight (Denemo.project->movement, 10);

}

/**
 * Remove selection marker
 */
void
unset_selection_key (DenemoScriptParam *param)
{
  Denemo.project->movement->markstaffnum = 0;
}


static void move_to_note_by_command (gchar note)
{
    
  Denemo.project->last_source = INPUTKEYBOARD;
  move_to_pitch (Denemo.project, ((note + 5 - 'A') % 7));  
}

static void
insert_note_by_command(gchar note)
{
  Denemo.project->last_source = INPUTKEYBOARD;
  insert_pitch (Denemo.project, ((note + 5 - 'A') % 7));
}


static void change_or_append_x_key (gint key)
{
  //move_to_pitch (Denemo.project, key);
  //gint enshift = get_cursoracc();g_print ("Cursoracc for change_or_append_x_key is %d\n", enshift);
  Denemo.project->last_source = INPUTKEYBOARD;
  edit_or_append_pitch (key, FALSE); 
    
}

/**
 * Goto the nearest a
 *
 */
void
change_or_append_A_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (5);
}

/**
 * Goto the nearest b
 *
 */
void
change_or_append_B_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (6);
}

/**
 * Goto the nearest c
 *
 */
void
change_or_append_C_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (0);
}

/**
 * Goto the nearest d
 *
 */
void
change_or_append_D_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (1);
}

/**
 * Goto the nearest e
 *
 */
void
change_or_append_E_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (2);
}

/**
 * Goto the nearest f
 *
 */
void
change_or_append_F_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (3);
}

/**
 * Goto the nearest g
 *
 */
void
change_or_append_G_key (DenemoAction* action, DenemoScriptParam *param)
{
  change_or_append_x_key (4);
}


/**
 * Move cursor by amount or in EDIT mode change the note by the amount
 */
static void
octave_shift_key (DenemoScriptParam *param, gint amount)
{
  if (((DenemoStaff *) Denemo.project->movement->currentstaff->data)->tone_store)
    {
      return;                   //FIXME create a function modify_tone, like delete_tone in pitchentry.c to do this sort of thing
    }
  else
    {
      if (Denemo.project->mode & (INPUTEDIT))
        {
          if (Denemo.project->movement->currentobject)
            {
              objnode *thenote = nearestnote (Denemo.project->movement->currentobject->data, Denemo.project->movement->cursor_y);
              if (thenote)
                {
                  note copy = *((note *) thenote->data);
                  GList *direcs = ((note *) thenote->data)->directives;
                  store_for_undo_change (Denemo.project->movement, Denemo.project->movement->currentobject->data);
                  Denemo.project->movement->undo_guard++;
                  delete_chordnote (Denemo.project);       //does not delete the directives.
                  Denemo.project->movement->cursor_y = copy.mid_c_offset + amount;
                  insert_chordnote (Denemo.project);
                  changeenshift (Denemo.project->movement->currentobject->data, Denemo.project->movement->cursor_y, copy.enshift);
                  thenote = nearestnote (Denemo.project->movement->currentobject->data, Denemo.project->movement->cursor_y);
                  if (thenote)
                    ((note *) thenote->data)->directives = direcs;
                    
                  Denemo.project->movement->staffletter_y  = offsettonumber (Denemo.project->movement->cursor_y);
                    
                  Denemo.project->movement->undo_guard--;
                  score_status (Denemo.project, TRUE);
                }
            }
        }
      else
        Denemo.project->movement->cursor_y += amount;
    }
  if(!Denemo.non_interactive)
    gtk_widget_queue_draw(Denemo.scorearea);
}

/**
 * Move cursor an octave up
 */
void
octave_up_key (DenemoAction* action, DenemoScriptParam *param)
{
  octave_shift_key (param, 7);
}




/**
 * Move cursor an octave down
 */
void
octave_down_key (DenemoAction* action, DenemoScriptParam *param)
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
  Denemo.project->mode ^= TRAVERSE;
  if (Denemo.project->mode & TRAVERSE)
    gtk_label_set_text (GTK_LABEL (Denemo.statuslabel), "Read Only");
  g_debug ("Mode %d\n", Denemo.project->mode);
  displayhelper (Denemo.project);
}





/**
 * Add measure to end of staff / score
 */
void
append_measure_key (DenemoAction* action, DenemoScriptParam *param)
{
  appendmeasures (Denemo.project->movement, 1);
}

void
append_measure_score (DenemoAction* action, DenemoScriptParam *param)
{
  appendmeasurestoentirescore (Denemo.project->movement, 1);
}

/**
 * Insert measure at the current
 */
void
insert_measure_key (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_insertmeasures (Denemo.project->movement, 1);
}

static void
insert_chord_xdur (gint duration, DenemoScriptParam *param)
{
  dnm_insertnote (Denemo.project, duration, Denemo.project->mode, FALSE);//gets it wrong
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}

void
insert_chord_0key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(0, param);
}

void
insert_chord_1key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(1, param);
}

void
insert_chord_2key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(2, param);
}

void
insert_chord_3key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(3, param);
}

void
insert_chord_4key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(4, param);
}

void
insert_chord_5key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(5, param);
}

void
insert_chord_6key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(6, param);
}

void
insert_chord_7key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(7, param);
}

void
insert_chord_8key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_chord_xdur(8, param);
}

void
insert_rest_xkey(gint duration, DenemoScriptParam* param)
{
  dnm_insertnote (Denemo.project, duration, INPUTREST, TRUE);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}


void
insert_rest_0key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(0, param);
}

void
insert_rest_1key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(1, param);
}

void
insert_rest_2key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(2, param);
}

void
insert_rest_3key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(3, param);
}

void
insert_rest_4key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(4, param);
}

void
insert_rest_5key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(5, param);
}

void
insert_rest_6key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(6, param);
}

void
insert_rest_7key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(7, param);
}

void
insert_rest_8key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_rest_xkey(8, param);
}

void
insert_blankchord_xkey (gint duration, DenemoScriptParam *param)
{
  dnm_insertnote (Denemo.project, duration, INPUTBLANK, FALSE);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}

void
insert_blankchord_0key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(0, param);
}

void
insert_blankchord_1key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(1, param);
}

void
insert_blankchord_2key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(2, param);
}

void
insert_blankchord_3key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(3, param);
}

void
insert_blankchord_4key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(4, param);
}

void
insert_blankchord_5key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(5, param);
}

void
insert_blankchord_6key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(6, param);
}

void
insert_blankchord_7key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(7, param);
}

void
insert_blankchord_8key (DenemoAction* action, DenemoScriptParam *param)
{
  insert_blankchord_xkey(8, param);
}

gboolean
add_note_to_chord (DenemoAction* action, DenemoScriptParam *param)
{
  return insert_chordnote (Denemo.project);
}

gboolean
delete_note_from_chord (DenemoAction* action, DenemoScriptParam *param)
{
  return delete_chordnote (Denemo.project);
}

// Note this function is RECURSIVE.
// deletes the object before the cursor. If the measure is empty it moves to the end of the previous measure and calls itself.
// after deleting an object it backs-up on any rhythm pattern being followed.
void
deletepreviousobject (DenemoAction* action, DenemoScriptParam *param)
{

  /* remove the object preceding the cursor, within the current measure */
  if (Denemo.project->movement->cursor_x)
    {
      /* Then move the cursor back */
      movecursorleft (NULL, NULL);
      DenemoObject *curObj = (DenemoObject*)Denemo.project->movement->currentobject->data;
      gboolean anote = (curObj->type == CHORD) && ((chord*)curObj->object)->notes;
      /* And delete */
      deleteobject (NULL, NULL);
      /* if you are following a rhythmic pattern then backup the pattern */
      if ((Denemo.project->mode & (INPUTEDIT) && Denemo.project->rstep))
        {
          if (anote)
            Denemo.project->rstep = Denemo.project->rstep->prev;          /* rstep list of elements is circular */
          if (Denemo.project->cstep)
            {
              RhythmPattern *cursnip = (RhythmPattern *)Denemo.project->currhythm->data;
              Denemo.project->cstep = Denemo.project->cstep->prev ? Denemo.project->cstep->prev : g_list_last (Denemo.project->cstep);// cstep list of DenemoObjects is not circular
            }
          if (((RhythmElement *) Denemo.project->rstep->data)->highlightlabel)
            {
                RhythmPattern *cursnip = (RhythmPattern *)Denemo.project->currhythm->data;
                set_rhythm_label (cursnip, ((RhythmElement *) Denemo.project->rstep->data)->highlightlabel);
            }
        }

    }
  else
    {                           /* go to the previous measure, go to end of it, and start deleting there */
      if (Denemo.project->movement->currentmeasure->prev)
        {
          DenemoScriptParam param;

          do
            {
              movetomeasureleft (NULL, &param);
              //go to end
              while (Denemo.project->movement->currentobject && (Denemo.project->movement->currentobject->next))
                {
                  Denemo.project->movement->currentobject = Denemo.project->movement->currentobject->next;
                  Denemo.project->movement->cursor_x++;
                }
            }
          while (param.status && !Denemo.project->movement->currentobject);


          if (Denemo.project->movement->currentobject)
            {
              movecursorright (NULL, NULL);
              deletepreviousobject (NULL, NULL);//RECURSIVE!!
            }
        }
    }
}

void
sharpen_note (DenemoAction* action, DenemoScriptParam *param)
{
    incrementenshift (Denemo.project, 1);
}
void
set_stem_up (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (Denemo.project->movement->currentobject ? Denemo.project->movement->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (Denemo.project->movement, DENEMO_STEMUP);
}


void
flatten_note (DenemoAction* action, DenemoScriptParam *param)
{
    incrementenshift (Denemo.project, -1);
}
void
set_stem_down (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoObject *curmudelaobj = (DenemoObject *) (Denemo.project->movement->currentobject ? Denemo.project->movement->currentobject->data : NULL);

  if (curmudelaobj && curmudelaobj->type == STEMDIRECTIVE)
    change_stem_directive (Denemo.project->movement, DENEMO_STEMDOWN);
}


void
pending_sharpen (DenemoAction* action, DenemoScriptParam *param)
{
  Denemo.project->movement->pending_enshift++;
  if (Denemo.project->movement->pending_enshift > 2)
    Denemo.project->movement->pending_enshift = 2;
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}

void
pending_flatten (DenemoAction* action, DenemoScriptParam *param)
{
  Denemo.project->movement->pending_enshift--;
  if (Denemo.project->movement->pending_enshift < -2)
    Denemo.project->movement->pending_enshift = -2;
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}



/* insert a duplicate note and tie to it */
void
tie_notes_key (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoObject *curObj = (DenemoObject *) (Denemo.project->movement->currentobject ? Denemo.project->movement->currentobject->data : NULL);

  /* Equals - toggle whether this note is tied */
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->notes)
    {
        if (Denemo.project->movement->cursor_appending)
            {
              DenemoMovement *si = Denemo.project->movement;
              gint tickspermeasure =  WHOLE_NUMTICKS * ((DenemoMeasure*)si->currentmeasure->data)->timesig->time1 / ((DenemoMeasure*)si->currentmeasure->data)->timesig->time2;
              si->cursoroffend = (curObj->starttickofnextnote >= tickspermeasure);
              insertion_point (Denemo.project->movement);
              object_insert (Denemo.project, dnm_clone_object (curObj));
              movecursorleft (NULL, NULL);
              movecursorleft (NULL, NULL);
              toggle_tie (NULL, NULL);
              movecursorright (NULL, NULL);
              movecursorright (NULL, NULL);
          } else
          {
              object_insert (Denemo.project, dnm_clone_object (curObj));
              movecursorleft (NULL, NULL);
              ((chord *) ((DenemoObject *)Denemo.project->movement->currentobject->data)->object)->is_tied = 1;
              movecursorright (NULL, NULL);
              movecursorright (NULL, NULL);
          }
    }
}

void
add_dot_key (DenemoAction* action, DenemoScriptParam *param)
{
  changedots (Denemo.project->movement, 1);
}

void
remove_dot_key (DenemoAction* action, DenemoScriptParam *param)
{
  changedots (Denemo.project->movement, -1);
}


void
force_cautionary (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoObject *theobj = Denemo.project->movement->currentobject ? (DenemoObject *) Denemo.project->movement->currentobject->data : NULL;
  if (theobj && theobj->type == CHORD)
    caution (Denemo.project->movement);
}

void
change_pitch (DenemoAction* action, DenemoScriptParam *param)
{
  if (Denemo.project->mode & INPUTEDIT)
    {
      //DenemoObject *theobj =
      //  si->currentobject ? (DenemoObject *) si->currentobject->data : NULL;
      delete_chordnote (Denemo.project);
      insert_chordnote (Denemo.project);
    }
}

/*******************************************************************************
* DURATION COMMANDS
******************************************************************************/

void InsertRest(gint duration){
  highlight_rest(Denemo.project, duration);
  dnm_insertnote (Denemo.project, duration, INPUTREST, TRUE);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void InsertDur(gint duration){
  highlight_duration(Denemo.project, duration);
  dnm_insertnote (Denemo.project, duration,  INPUTNORMAL, FALSE);
  displayhelper (Denemo.project);
  score_status(Denemo.project, TRUE);
}

void ChangeDur(gint duration){
  
  gboolean appending = Denemo.project->movement->cursor_appending;
  if(appending)
    movecursorleft(NULL, NULL);
  change_duration(duration);
  if(appending)
    movecursorright(NULL, NULL);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void SetDur(gint duration){
  highlight_duration(Denemo.project, duration);
}

//called by the commands (d-0) (d-1) etc
static void Dur (gint duration) {
    insert_chord_xdur(duration, NULL);
    highlight_duration(Denemo.project, duration);
    score_status(Denemo.project, TRUE);
    displayhelper(Denemo.project);
}

/*******************************************************************************
* NOTE COMMANDS
******************************************************************************/
static void change_note_to (gchar note)//CHECK executing (d-ChangeToA)
{
  gint mode = Denemo.project->mode;
  Denemo.project->last_source = INPUTKEYBOARD;
  edit_or_append_pitch (((note + 5 - 'A') % 7), FALSE);
  Denemo.project->mode = mode;
}


void ChangeTo(gchar note){
  gboolean appending = Denemo.project->movement->cursor_appending;
  if(appending)
    movecursorleft(NULL, NULL);

  change_note_to (note);
  if(appending)
    movecursorright(NULL, NULL);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void MoveTo(gchar note){
  move_to_note_by_command (note);
  displayhelper(Denemo.project);
}

void Insert(gchar note){

  insert_note_by_command (note);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void AddNote(gchar note){
  movecursorright(NULL, NULL);
  insert_note_by_command (note);
  movecursorleft(NULL, NULL);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void Add(gchar note){ //AddA etc
  Denemo.project->last_source = INPUTKEYBOARD;
  move_to_pitch (Denemo.project, ((note + 5 - 'A') % 7));
  insert_chordnote (Denemo.project);
  score_status(Denemo.project, TRUE);
  displayhelper(Denemo.project);
}

void Dur0(DenemoAction *action, gpointer param) {
  Dur(0);
}
void ChangeDur0(DenemoAction *action, gpointer param){
  ChangeDur(0);
}
void InsertDur0(DenemoAction *action, gpointer param){
  InsertDur(0);
}
void InsertRest0(DenemoAction *action, gpointer param){
  InsertRest(0);
}
void SetDur0(DenemoAction *action, gpointer param){
  SetDur(0);
}
void Dur1(DenemoAction *action, gpointer param) {
  Dur(1);
}
void ChangeDur1(DenemoAction *action, gpointer param){
  ChangeDur(1);
}
void InsertDur1(DenemoAction *action, gpointer param){
  InsertDur(1);
}
void InsertRest1(DenemoAction *action, gpointer param){
  InsertRest(1);
}
void SetDur1(DenemoAction *action, gpointer param){
  SetDur(1);
}
void Dur2(DenemoAction *action, gpointer param) {
  Dur(2);
}
void ChangeDur2(DenemoAction *action, gpointer param){
  ChangeDur(2);
}
void InsertDur2(DenemoAction *action, gpointer param){
  InsertDur(2);
}
void InsertRest2(DenemoAction *action, gpointer param){
  InsertRest(2);
}
void SetDur2(DenemoAction *action, gpointer param){
  SetDur(2);
}
void Dur3(DenemoAction *action, gpointer param) {
  Dur(3);
}
void ChangeDur3(DenemoAction *action, gpointer param){
  ChangeDur(3);
}
void InsertDur3(DenemoAction *action, gpointer param){
  InsertDur(3);
}
void InsertRest3(DenemoAction *action, gpointer param){
  InsertRest(3);
}
void SetDur3(DenemoAction *action, gpointer param){
  SetDur(3);
}
void Dur4(DenemoAction *action, gpointer param) {
  Dur(4);
}
void ChangeDur4(DenemoAction *action, gpointer param){
  ChangeDur(4);
}
void InsertDur4(DenemoAction *action, gpointer param){
  InsertDur(4);
}
void InsertRest4(DenemoAction *action, gpointer param){
  InsertRest(4);
}
void SetDur4(DenemoAction *action, gpointer param){
  SetDur(4);
}
void Dur5(DenemoAction *action, gpointer param) {
  Dur(5);
}
void ChangeDur5(DenemoAction *action, gpointer param){
  ChangeDur(5);
}
void InsertDur5(DenemoAction *action, gpointer param){
  InsertDur(5);
}
void InsertRest5(DenemoAction *action, gpointer param){
  InsertRest(5);
}
void SetDur5(DenemoAction *action, gpointer param){
  SetDur(5);
}
void Dur6(DenemoAction *action, gpointer param) {
  Dur(6);
}
void ChangeDur6(DenemoAction *action, gpointer param){
  ChangeDur(6);
}
void InsertDur6(DenemoAction *action, gpointer param){
  InsertDur(6);
}
void InsertRest6(DenemoAction *action, gpointer param){
  InsertRest(6);
}
void SetDur6(DenemoAction *action, gpointer param){
  SetDur(6);
}
void Dur7(DenemoAction *action, gpointer param) {
  Dur(7);
}
void ChangeDur7(DenemoAction *action, gpointer param){
  ChangeDur(7);
}
void InsertDur7(DenemoAction *action, gpointer param){
  InsertDur(7);
}
void InsertRest7(DenemoAction *action, gpointer param){
  InsertRest(7);
}
void SetDur7(DenemoAction *action, gpointer param){
  SetDur(7);
}
void Dur8(DenemoAction *action, gpointer param) {
  Dur(8);
}
void ChangeDur8(DenemoAction *action, gpointer param){
  ChangeDur(8);
}
void InsertDur8(DenemoAction *action, gpointer param){
  InsertDur(8);
}
void InsertRest8(DenemoAction *action, gpointer param){
  InsertRest(8);
}
void SetDur8(DenemoAction *action, gpointer param){
  SetDur(8);
}
void InsertA(DenemoAction *action, gpointer param){
  Insert('A');
}
void AddNoteA(DenemoAction *action, gpointer param){
  AddNote('A');
}
void AddA(DenemoAction *action, gpointer param){
  Add('A');
}
void ChangeToA(DenemoAction *action, gpointer param){
  ChangeTo('A');
}
void MoveToA(DenemoAction *action, gpointer param){
  MoveTo('A');
}
void InsertB(DenemoAction *action, gpointer param){
  Insert('B');
}
void AddNoteB(DenemoAction *action, gpointer param){
  AddNote('B');
}
void AddB(DenemoAction *action, gpointer param){
  Add('B');
}
void ChangeToB(DenemoAction *action, gpointer param){
  ChangeTo('B');
}
void MoveToB(DenemoAction *action, gpointer param){
  MoveTo('B');
}
void InsertC(DenemoAction *action, gpointer param){
  Insert('C');
}
void AddNoteC(DenemoAction *action, gpointer param){
  AddNote('C');
}
void AddC(DenemoAction *action, gpointer param){
  Add('C');
}
void ChangeToC(DenemoAction *action, gpointer param){
  ChangeTo('C');
}
void MoveToC(DenemoAction *action, gpointer param){
  MoveTo('C');
}
void InsertD(DenemoAction *action, gpointer param){
  Insert('D');
}
void AddNoteD(DenemoAction *action, gpointer param){
  AddNote('D');
}
void AddD(DenemoAction *action, gpointer param){
  Add('D');
}
void ChangeToD(DenemoAction *action, gpointer param){
  ChangeTo('D');
}
void MoveToD(DenemoAction *action, gpointer param){
  MoveTo('D');
}
void InsertE(DenemoAction *action, gpointer param){
  Insert('E');
}
void AddNoteE(DenemoAction *action, gpointer param){
  AddNote('E');
}
void AddE(DenemoAction *action, gpointer param){
  Add('E');
}
void ChangeToE(DenemoAction *action, gpointer param){
  ChangeTo('E');
}
void MoveToE(DenemoAction *action, gpointer param){
  MoveTo('E');
}
void InsertF(DenemoAction *action, gpointer param){
  Insert('F');
}
void AddNoteF(DenemoAction *action, gpointer param){
  AddNote('F');
}
void AddF(DenemoAction *action, gpointer param){
  Add('F');
}
void ChangeToF(DenemoAction *action, gpointer param){
  ChangeTo('F');
}
void MoveToF(DenemoAction *action, gpointer param){
  MoveTo('F');
}
void InsertG(DenemoAction *action, gpointer param){
  Insert('G');
}
void AddNoteG(DenemoAction *action, gpointer param){
  AddNote('G');
}
void AddG(DenemoAction *action, gpointer param){
  Add('G');
}
void ChangeToG(DenemoAction *action, gpointer param){
  ChangeTo('G');
}
void MoveToG(DenemoAction *action, gpointer param){
  MoveTo('G');
}
