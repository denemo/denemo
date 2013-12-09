/* scoreops.cpp
 * functions dealing with the whole score

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee*/

#include <denemo/denemo.h>
#include <stdio.h>
#include <string.h>
#include "config.h"
#include "staffops.h"
#include "scoreops.h"
#include "prefops.h"
#include "selectops.h"
#include "objops.h"
#include "lyric.h"
#include "audiointerface.h"
#include "sourceaudio.h"
#include "utils.h"
#include "view.h"
#include "commandfuncs.h"
#include "moveviewport.h"
#include "contexts.h"
#include "midi.h"
#include "displayanimation.h"
#include "lilydirectives.h"
#include "exportmidi.h"
#include "calculatepositions.h"

#define MOVEMENT_WIDTH (10)


/**
 * Create new DenemoScore with no staff
 * set it as gui->si but do not add it to the movements list.
 */
void
point_to_empty_movement (DenemoProject * gui)
{
  DenemoScore *newscore = (DenemoScore *) g_malloc0 (sizeof (DenemoScore));
  init_score (newscore, gui);
  if (!Denemo.non_interactive && gui->si && gui->si->buttonbox)
    gtk_widget_hide (gui->si->buttonbox);
  g_static_mutex_lock (&smfmutex);
  gui->si = newscore;
  g_static_mutex_unlock (&smfmutex);
  if(!Denemo.non_interactive)
    gtk_widget_show (gui->si->buttonbox);

}

/**
 * Create new DenemoScore with one empty staff
 * set it as gui->si but do not add it to the movements list.
 */
void
point_to_new_movement (DenemoProject * gui)
{
  point_to_empty_movement (gui);
  newstaff (gui, INITIAL, DENEMO_NONE);
  gui->si->undo_guard = Denemo.prefs.disable_undo;
}

static void select_movement (gint movementnum) {
	goto_movement_staff_obj (NULL, movementnum, 0, 0, 0);
	set_movement_selector (Denemo.project);
	displayhelper (Denemo.project);
	write_status (Denemo.project);	
}

#define NUM_MOVEMENTS_TO_SHOW (2*5)
void set_movement_selector (DenemoProject *gui)
{
  GtkWidget *button;
  GList *g;
  gint i;
  
  if(gui->movements_selector)
	gtk_widget_destroy (gui->movements_selector);
  gui->movements_selector = (GtkWidget*)gtk_hbox_new(FALSE,1);
  gtk_box_pack_start(GTK_BOX(gui->buttonbox), gui->movements_selector,  FALSE, TRUE, 0);
  gtk_widget_show (gui->movements_selector);
  gint num_movements = 1;
  gint last = 1, first = 1;
  if(gui->movements) {
	  gint current = g_list_index (gui->movements, gui->si) + 1;
	  num_movements = g_list_length(gui->movements);
	  first = current - NUM_MOVEMENTS_TO_SHOW/2;
	  if(first<1) first = 1;
	  last = first + NUM_MOVEMENTS_TO_SHOW -1;
	  
	  if(last>num_movements) 
	  {
		  last = num_movements;
		  first = num_movements - NUM_MOVEMENTS_TO_SHOW;
		  if(first<1) first = 1;
		}
  }
  for (g=gui->movements, i=1;g;g=g->next, i++)
	{
		if(i<first)
			continue;
		if(i>last)
			continue;
		button = gtk_button_new_with_label("");
		if (g->data == gui->si)
		{
			gchar *text = g_strdup_printf("<span foreground=\"blue\"><i><b>%d</b></i></span>", i);
			GtkWidget *label_widget = gtk_bin_get_child(GTK_BIN(button));
			gtk_label_set_use_markup (GTK_LABEL(label_widget), TRUE);
			gtk_label_set_markup (GTK_LABEL (label_widget), text);
			g_free(text);
			text = g_strdup_printf(_("This is the current movement number %d\nClick on another button to change movements"), i);
			gtk_widget_set_tooltip_text (button, text);
			g_free(text);
		} else 
		{
			gchar *more = "";
			if((last<num_movements) && (i==last))
				more = "+...";
			gchar *text = g_strdup_printf("%d%s", i, more);
			gtk_button_set_label (button, text);
			g_free(text);
			text = g_strdup_printf(_("Click to switch to movement number %d"), i);
			gtk_widget_set_tooltip_text (button, text);
			g_free(text);
		}
		
		
		gtk_widget_set_can_focus (button, FALSE);
		gtk_widget_show(button);
		g_signal_connect_swapped (G_OBJECT(button), "clicked", G_CALLBACK (select_movement), (gpointer) i);
		gtk_box_pack_start (GTK_BOX(gui->movements_selector), button,  FALSE, TRUE, 0);
	}
	if(i==2) 
		gtk_widget_hide (gui->movements_selector);
}
static void
new_movement (GtkAction * action, DenemoScriptParam * param, gboolean before)
{
  DenemoProject *gui = Denemo.project;
  gint pos = g_list_index (gui->movements, gui->si);
  append_new_movement (action, param);
  DenemoScore *newsi = g_list_last (gui->movements)->data;
  gui->movements = g_list_delete_link (gui->movements, g_list_last (gui->movements));
  gui->movements = g_list_insert (gui->movements, newsi, before ? pos : pos + 1);
  newsi->currentmovementnum = 1 + g_list_index (gui->movements, newsi);
  setcurrentprimarystaff (gui->si);
  gui->movements_selector = NULL;
  set_movement_selector (gui);
  
  rewind_audio ();
  write_status (gui);
}


static void
append_movement (GtkAction * action, gpointer param, gboolean populate)
{
  DenemoProject *gui = Denemo.project;
  DenemoScore *source_movement = gui->si;
  GList *g;
  (void) signal_structural_change (gui);

  if (gui->si->lyricsbox)
    gtk_widget_hide (gui->si->lyricsbox);
  point_to_empty_movement (gui);
  for (g = source_movement->thescore; g; g = g->next)
    {
      DenemoStaff *source_staff = g->data;
      newstaff (gui, LAST, DENEMO_NONE);
      if (!populate)
        break;
      GList *dest = g_list_last (gui->si->thescore);
      DenemoStaff *dest_staff = dest->data;
      copy_staff (source_staff, dest_staff);
    }
  gui->movements = g_list_append (gui->movements, gui->si);
  gui->si->currentmovementnum = 1 + g_list_index (gui->movements, gui->si);
  set_movement_selector (gui);
  gui->si->undo_guard = Denemo.prefs.disable_undo;
  call_out_to_guile ("(d-DirectivePut-header-postfix \"SuppressTitleRepeats\" \"title = ##f\ninstrument = ##f\n\")");
  set_width_to_work_with (gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  find_leftmost_allcontexts (gui->si);
  set_bottom_staff (gui);
  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (Denemo.scorearea);
  g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
  g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
  displayhelper (gui);
  score_status (gui, TRUE);
}

//copies staff structure to new movement
void
append_new_movement (GtkAction * action, DenemoScriptParam * param)
{
  append_movement (action, param, TRUE);
}

//does not copy staff structure to new movement
void
append_blank_movement (void)
{
  append_movement (NULL, NULL, FALSE);
}




void
insert_movement_before (GtkAction * action, DenemoScriptParam * param)
{
  new_movement (action, param, TRUE);
}

void
insert_movement_after (GtkAction * action, DenemoScriptParam * param)
{
  new_movement (action, param, FALSE);
}

static void
terminate_playback (void)
{
  if (is_playing ())
    midi_stop ();
  g_thread_yield ();            //FIXME find a better way of ensuring playing is finished - in principle the user could start playing again
  if (is_playing ())
    terminate_playback ();
  //g_print("Terminated %d\n", is_playing());
}

void
reset_movement_numbers (DenemoProject * gui)
{
  GList *g;
  gint i;
  for (i = 1, g = gui->movements; g; g = g->next, i++)
    {
      DenemoScore *si = g->data;
      si->currentmovementnum = i;
    }
}

void
delete_movement (GtkAction * action, DenemoScriptParam* param)
{
  DenemoProject *gui = Denemo.project;
  terminate_playback ();
  (void) signal_structural_change (gui);
  GString *primary = g_string_new (""), *secondary = g_string_new ("");
  if (gui->movements == NULL || (g_list_length (gui->movements) == 1))
    {
      g_string_assign (primary, _("This is the only movement"));
      g_string_assign (secondary, _("Delete it and start over?"));
      if (confirm (primary->str, secondary->str))
        {
          gchar *name = g_strdup (gui->filename->str);
          deletescore (NULL, gui);
          g_string_assign (gui->filename, name);
          g_free (name);
        }
    }
  else
    {                           // more than one movement
      gint num = g_list_index (gui->movements, gui->si);
      g_string_printf (primary, _("This is movement #%d"), num + 1);
      g_string_assign (secondary, _("Delete entire movement?"));
      if (confirm (primary->str, secondary->str))
        {
          free_score (gui);
          DenemoScore *si = gui->si;
          GList *g = g_list_find (gui->movements, si)->next;
          if (g == NULL)
            g = g_list_find (gui->movements, si)->prev;
          gui->si = g->data;
          gui->movements = g_list_remove (gui->movements, (gpointer) si);
        }
    }
  reset_movement_numbers (gui);
  g_string_free (primary, TRUE);
  g_string_free (secondary, TRUE);
  set_movement_selector (gui);
  set_movement_transition (-MOVEMENT_WIDTH);
  displayhelper (gui);
  score_status (gui, TRUE);
}

/* go to the movement voice measure staff and object numbers (starting from 1)
 movementnum<=0 means current movement
possible_gui is really a flag interactive or not
*/
gboolean
goto_movement_staff_obj (DenemoProject * possible_gui, gint movementnum, gint staffnum, gint measurenum, gint objnum)
{
  DenemoProject *gui;
  terminate_playback ();
  if (possible_gui == NULL)
    gui = Denemo.project;
  else
    gui = possible_gui;
  if (movementnum > 0)
    {
      GList *this = g_list_nth (gui->movements, movementnum - 1);
      if (this == NULL)
        {
          if (possible_gui)
            warningdialog (_("No such movement"));
          return FALSE;
        }
      gtk_widget_hide (gui->si->buttonbox);
      gui->si = this->data;
      gtk_widget_show (gui->si->buttonbox);
    }


  if (!moveto_currentstaffnum (gui, staffnum))
    {
      if (possible_gui)
        warningdialog (_("No such voice"));
      return FALSE;
    }
  if (!moveto_currentmeasurenum (gui, measurenum))
    {
      if (possible_gui)
        warningdialog (_("No such measure"));
      return FALSE;
    }
  //cursor_x is zero and we are on the first, if any, object
  if (gui->si->currentobject == NULL && objnum)
    return FALSE;               //asking for an object in an empty measure

  while (--objnum > 0 && gui->si->currentobject && gui->si->currentobject->next)
    {
      gui->si->currentobject = gui->si->currentobject->next;
      gui->si->cursor_x++;
    }
  // g_print("objnum %d\n",objnum);
  if (objnum > 0)
    {
      gui->si->cursor_x++;      // objnum;
      if (!gui->si->currentobject)
        return FALSE;
      gui->si->cursor_appending = TRUE;
    }
  set_movement_selector (gui);
  write_status (gui);
  move_viewport_up (gui);
  move_viewport_down (gui);
  gtk_widget_queue_draw (Denemo.scorearea);
  return TRUE;
}

void
PopPosition (GtkAction * action, DenemoScriptParam * param)
{
  DenemoPosition *pos = pop_position ();
  DenemoScriptParam dummy;
  if (action || !param)
    param = &dummy;
  if (pos == NULL)
    {
      param->status = FALSE;
      return;
    }
  param->status = goto_movement_staff_obj (NULL, pos->movement, pos->staff, pos->measure, pos->object);
  if (param->status)
    {
      Denemo.project->si->cursor_appending = pos->appending;
      Denemo.project->si->cursoroffend = pos->offend;
    }
  g_free (pos);
}

void
PushPosition (GtkAction * action, DenemoScriptParam * param)
{
  push_position ();
}

void
PopPushPosition (GtkAction * action, DenemoScriptParam * param)
{
  DenemoPosition *pos = pop_position ();
  DenemoScriptParam dummy;
  if (action || !param)
    param = &dummy;
  if (pos)
    {
      push_position ();
      param->status = goto_movement_staff_obj (NULL, pos->movement, pos->staff, pos->measure, pos->object);
      if (param->status)
        {
          Denemo.project->si->cursor_appending = pos->appending;
          Denemo.project->si->cursoroffend = pos->offend;
        }
    }
  else
    param->status = FALSE;
}


/**
 * Move to the next movement
 * @param action - Gtk Action event
 * @param gui - pointer to the DenemoProject structure
 * @return none
*/
void
next_movement (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  terminate_playback ();
  GList *this = g_list_find (gui->movements, gui->si);
  this = this->next;
  if (param)
    param->status = TRUE;
  if (this == NULL)
    {
      if (param)
        param->status = FALSE;
      else
        warningmessage (_("This is the last movement"));
      return;
    }
  gtk_widget_hide (gui->si->buttonbox);
  if (gui->si->lyricsbox)
    gtk_widget_hide (gui->si->lyricsbox);
  gui->si = this->data;
  if (gui->si->lyricsbox && Denemo.prefs.lyrics_pane)
    gtk_widget_show (gui->si->lyricsbox);       //toggle_lyrics_view(NULL, NULL);
  gtk_widget_show (gui->si->buttonbox);
  set_master_tempo (gui->si, 1.0);
  set_movement_selector (gui);
  //!!!!!!!!updatescoreinfo (gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  find_leftmost_allcontexts (gui->si);
  set_bottom_staff (gui);

  update_hscrollbar (gui);
  update_vscrollbar (gui);
  g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
  g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
  write_status (gui);
  //gtk_widget_draw (Denemo.scorearea, NULL);//KLUDGE FIXME see staffup/down
  set_movement_transition (-MOVEMENT_WIDTH);
  gtk_widget_queue_draw (Denemo.scorearea);
  draw_score (NULL);
}

/**
 * Move to the previous movement
 * @param action - Gtk Action event
 * @param gui - pointer to the DenemoProject structure
 * @return none
*/
void
prev_movement (GtkAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  terminate_playback ();
  GList *this = g_list_find (gui->movements, gui->si);
  this = this->prev;
  if (param)
    param->status = TRUE;
  if (this == NULL)
    {
      if (param)
        param->status = FALSE;
      else
        warningmessage (_("This is the first movement"));
      return;
    }
  gtk_widget_hide (gui->si->buttonbox);
  if (gui->si->lyricsbox)
    gtk_widget_hide (gui->si->lyricsbox);
  gui->si = this->data;
  if (gui->si->lyricsbox)
    gtk_widget_show (gui->si->lyricsbox);       //toggle_lyrics_view(NULL, NULL);
  gtk_widget_show (gui->si->buttonbox);
  set_master_tempo (gui->si, 1.0);
   set_movement_selector (gui);
  //!!!!!!!!!!!!!updatescoreinfo (gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  set_bottom_staff (gui);

  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (Denemo.scorearea);
  g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
  g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
  write_status (gui);
  //gtk_widget_draw (Denemo.scorearea, NULL);//KLUDGE FIXME see staffup/down
  set_movement_transition (MOVEMENT_WIDTH);

  gtk_widget_queue_draw (Denemo.scorearea);
  draw_score (NULL);
}

/**
 * Initialise scoreinfo structure 
 *
 * @param si pointer to the scoreinfo structure to initialise
 */
void
init_score (DenemoScore * si, DenemoProject * gui)
{
  gchar *dir = (gchar *) get_user_data_dir (TRUE);

  si->readonly = 0;
  si->leftmeasurenum = si->rightmeasurenum = 1;
  si->top_staff = si->bottom_staff = 1;
  si->measurewidth = DENEMO_INITIAL_MEASURE_WIDTH;
  si->measurewidths = NULL;
  si->staffspace = DENEMO_INITIAL_STAFF_HEIGHT;
  si->thescore = NULL;
  si->currentstaffnum = 1;
  si->currentmeasurenum = 1;
  si->currentobject = NULL;
  si->cursor_x = 0;
  si->cursor_y = 0;
  si->staffletter_y = 0;
  si->cursor_appending = TRUE;
  si->zoom = Denemo.prefs.zoom > 0 ? Denemo.prefs.zoom / 100.0 : 1.0;
  si->preview_zoom = 1.0;
  si->system_height = Denemo.prefs.system_height > 0 ? Denemo.prefs.system_height / 100.0 : 1.0;

  si->cursoroffend = FALSE;
  si->cursortime1 = si->cursortime2 = 4;
  si->markstaffnum = 0;
  si->markmeasurenum = 0;
  si->markcursor_x = 0;
  si->maxkeywidth = 0;
  si->has_figures = FALSE;
  si->has_fakechords = FALSE;
  /*playback purposes */
  si->tempo = 120;
  si->start = 0;
  si->end = 0;
  si->stafftoplay = 0;
  si->start_time = 0;
  si->end_time = -1.0;          //ie unset
  if (Denemo.project->si)
    set_master_volume (si, 1.0);
  else
    si->master_volume = 1.0;


  si->tempo_change_time = 0.0;
  if (Denemo.project->si)
    set_master_tempo (si, 1.0);
  else
    si->master_tempo = 1.0;
  si->savebuffer = NULL;
  si->bookmarks = NULL;

  if (gui->filename == NULL)
    {
      gui->filename = g_string_new ("");
      gui->autosavename = g_string_new (dir);
      gui->autosavename = g_string_append (gui->autosavename, "/autosave.denemo");
    }
  if (gui->tabname == NULL)
    gui->tabname = g_string_new ("");
  si->curlilynode = 0;          /* the node of the lily parse tree on display in 
                                   textwindow */
  si->lily_file = 0;            /* root of lily file parse, see lilyparser.y etc  */




  /* Undo/redo initialisation */
  si->undodata = g_queue_new ();
  si->redodata = g_queue_new ();
  si->undo_guard = 1;           //do not collect undo information until file is loaded

  if(!Denemo.non_interactive){
    si->buttonbox = gtk_hbox_new (FALSE, 1);
    set_movement_selector (gui);
    gtk_widget_set_tooltip_text (si->buttonbox, _("A button bar that can be populated by Movement titles and other user generated buttons.\nGenerally by clicking the button you can edit the title or value or execute the action of the button"));
    gtk_box_pack_end (GTK_BOX (gui->buttonboxes), si->buttonbox, FALSE, TRUE, 0);
    gtk_widget_show (si->buttonbox);
    gtk_widget_set_can_focus (si->buttonbox, FALSE);
    //GTK_WIDGET_UNSET_FLAGS(si->buttonbox, GTK_CAN_FOCUS);
  }
}

static gboolean
delete_all_staffs (DenemoProject * gui)
{
  DenemoScore *si = gui->si;
  gint i;
  for (i = g_list_length (si->thescore); i > 0; i--)
    {
      si->currentstaffnum = i;
      si->currentstaff = g_list_nth (si->thescore, i - 1);
      deletestaff (gui, FALSE);
    }
  return TRUE;
}


static GList *
extract_verses (GList * verses)
{
  //g_warning("extract_verses not tested!!!!!!!");
  GList *ret = NULL;
  GList *g;
  for (g = verses; g; g = g->next)
    {
      GtkTextView *srcVerse = g->data;
      ret = g_list_append (ret, get_text_from_view (GTK_WIDGET (srcVerse)));
    }
  return ret;
}

/**
 * FIXME there is a muddle here between DenemoScore and DenemoProject
 * frees the data in the passed scoreinfo stucture 
 *
 * @param si pointer to the scoreinfo structure to free
 */
void
free_score (DenemoProject * gui)
{
  delete_all_staffs (gui);
  delete_directives (&gui->si->layout.directives);
  delete_directives (&gui->si->header.directives);
  free_midi_data (gui->si);
  if (gui->si->buttonbox)
    {
      gtk_widget_destroy (gui->si->buttonbox);
      gui->si->buttonbox = NULL;
    }
  if (gui->si->lyricsbox)
    {
      gtk_widget_destroy (gui->si->lyricsbox);
      gui->si->lyricsbox = NULL;
    }
  reset_lyrics (NULL, 0);
  g_queue_free (gui->si->undodata);
  g_queue_free (gui->si->redodata);
}

DenemoScore *
clone_movement (DenemoScore * si)
{
  DenemoScore *newscore = (DenemoScore *) g_malloc0 (sizeof (DenemoScore));
  memcpy (newscore, si, sizeof (DenemoScore));

  GList *g;
  newscore->measurewidths = NULL;
  if (newscore->sources)
    {
      g_warning ("Undo will lose source screenshots\n");
      newscore->sources = NULL;
    }
  for (g = si->measurewidths; g; g = g->next)
    newscore->measurewidths = g_list_append (newscore->measurewidths, g->data);
  newscore->playingnow = NULL;

  for (newscore->thescore = NULL, g = si->thescore; g; g = g->next)
    {
      DenemoStaff *thestaff = (DenemoStaff *) g_malloc (sizeof (DenemoStaff));
      DenemoStaff *srcStaff = (DenemoStaff *) g->data;
      // copy_staff(srcStaff, thestaff);!!!!!! does not copy e.g. no of lines ... need proper clone code.
      memcpy (thestaff, srcStaff, sizeof (DenemoStaff));

      thestaff->denemo_name = g_string_new (srcStaff->denemo_name->str);
      thestaff->lily_name = g_string_new (srcStaff->lily_name->str);
      thestaff->midi_instrument = g_string_new (srcStaff->midi_instrument->str);

      thestaff->clef.directives = clone_directives (srcStaff->clef.directives);
      thestaff->keysig.directives = clone_directives (srcStaff->keysig.directives);
      thestaff->timesig.directives = clone_directives (srcStaff->timesig.directives);

      if (srcStaff->leftmost_clefcontext == &srcStaff->clef)
        thestaff->leftmost_clefcontext = &thestaff->clef;
      else
        // has to be fixed up after the measures are done..., so do the whole thing after???
        //                                                         likewise keysig timesig
        g_warning ("Not doing clef context yet..."), thestaff->leftmost_clefcontext = &thestaff->clef;

      if (srcStaff->leftmost_timesig == &srcStaff->timesig)
        thestaff->leftmost_timesig = &thestaff->timesig;
      else
        // has to be fixed up after the measures are done..., so do the whole thing after???
        //                                                         likewise keysig timesig
        g_warning ("Not doing timesig context yet..."), thestaff->leftmost_timesig = &thestaff->timesig;

      if (srcStaff->leftmost_keysig == &srcStaff->keysig)
        thestaff->leftmost_keysig = &thestaff->keysig;
      else
        // has to be fixed up after the measures are done..., so do the whole thing after???
        //                                                         likewise keysig timesig
        g_warning ("Not doing keysig context yet..."), thestaff->leftmost_keysig = &thestaff->keysig;



      thestaff->denemo_name = g_string_new (srcStaff->denemo_name->str);
      thestaff->lily_name = g_string_new (srcStaff->lily_name->str);
      thestaff->midi_events = NULL;     //cached data

      thestaff->staff_directives = clone_directives (srcStaff->staff_directives);
      {
        GList *direc;
        for (direc = thestaff->staff_directives; direc; direc = direc->next)
          {
            DenemoDirective *directive = direc->data;
            directive->widget = NULL;
            //      widget_for_staff_directive(directive);
          }
      }
      thestaff->voice_directives = clone_directives (srcStaff->voice_directives);
      {
        GList *direc;
        for (direc = thestaff->voice_directives; direc; direc = direc->next)
          {
            DenemoDirective *directive = direc->data;
            directive->widget = NULL;
            //      widget_for_voice_directive(directive);
          }
      }

      newscore->lyricsbox = NULL;
      thestaff->verses = extract_verses (srcStaff->verses);

      if (srcStaff->currentverse)
        thestaff->currentverse = g_list_nth (thestaff->verses, g_list_position (srcStaff->verses, srcStaff->currentverse));

      newscore->thescore = g_list_append (newscore->thescore, thestaff);
      if (g == si->currentprimarystaff)
        newscore->currentprimarystaff = newscore->thescore;
      if (g == si->currentstaff)
        newscore->currentstaff = newscore->thescore;
      newscore->currentmeasure = newscore->currentobject = thestaff->measures = NULL;
      GList *h;
      for (h = srcStaff->measures; h; h = h->next)
        {
          objnode *theobjs = h->data;
          GList *newmeasure = NULL;
          GList *i;
          for (i = theobjs; i; i = i->next)
            {
              DenemoObject *theobj = (DenemoObject *) i->data;
              DenemoObject *newobj = dnm_clone_object (theobj);
              newmeasure = g_list_append (newmeasure, newobj);
              if (i == si->currentobject)
                                                                                                                                        /*g_print("current object %x\n", g_list_last(newmeasure)), */ newscore->currentobject = g_list_last (newmeasure);
                                                                                                                                        //???
            }
          thestaff->measures = g_list_append (thestaff->measures, newmeasure);
          if (h == si->currentmeasure)
                                                                                                                                                /*g_print("current measure %x\n", g_list_last(thestaff->measures)), */ newscore->currentmeasure = g_list_last (thestaff->measures);
                                                                                                                                                //???
        }
    }



  newscore->movementcontrol.directives = clone_directives (si->movementcontrol.directives);
  newscore->layout.directives = clone_directives (si->layout.directives);
  newscore->header.directives = clone_directives (si->header.directives);



  /*


     smfsync
     savebuffer
     bookmarks
     Instruments


   */
  newscore->markstaffnum = 0;   //Do not clone the selection


  return newscore;
}




/**
 * Recalculates the stored information about a movement
 * either gui->si or if that does exist yet, gui->movements->data, the first movement.(FIXME)
 *
 * @param gui pointer to the gui structure
 */
void
updatescoreinfo (DenemoProject * gui)
{
  staffnode *curstaff;
  DenemoScore *si;
  GList *g = gui->movements;
  if (g)
    si = g->data;
  else
    si = gui->si;
  do
    {
      for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
        {
          beamsandstemdirswholestaff ((DenemoStaff *) curstaff->data);
          showwhichaccidentalswholestaff ((DenemoStaff *) curstaff->data);
        }
      find_xes_in_all_measures (si);
      find_leftmost_allcontexts (si);

      si->currentstaff = si->thescore;
      si->currentmeasure = firstmeasurenode (si->currentstaff);
      si->currentobject = firstobjnode (si->currentmeasure);
      if (!si->currentobject)
        si->cursor_appending = TRUE;
      else
        si->cursor_appending = FALSE;
      si->leftmeasurenum = si->currentstaffnum = si->currentmeasurenum = 1;
    }
  while (g && (g = g->next) && (si = g->data));
  score_status (gui, FALSE);
}

/**
 * Delete the movements of the given score and create a new one
 * with one movement and empty music data, no title
 * This is the action for the d-New command
 */
void
deletescore (GtkWidget * widget, DenemoProject * gui)
{
  free_movements (gui);
  score_status (gui, FALSE);
  if (gui->filename)
    {
      g_string_free (gui->filename, TRUE);
      g_string_free (gui->autosavename, TRUE);
      gui->filename = NULL;
      set_title_bar (gui);
    }
  if (gui->tabname)
    {
      g_string_free (gui->tabname, TRUE);
      gui->tabname = NULL;
    }

  point_to_new_movement (gui);
  gui->movements = g_list_append (gui->movements, gui->si);
  reset_movement_numbers (gui);
  set_width_to_work_with (gui);
  set_rightmeasurenum (gui->si);
  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (Denemo.scorearea);
  g_signal_emit_by_name (G_OBJECT (Denemo.hadjustment), "changed");
  g_signal_emit_by_name (G_OBJECT (Denemo.vadjustment), "changed");
  force_lily_refresh (gui);
}
