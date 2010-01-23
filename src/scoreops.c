/* scoreops.cpp
 * functions dealing with the whole score

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee*/

#include <denemo/denemo.h>
#include <stdio.h>
#include "config.h"
#include "staffops.h"
#include "scoreops.h"
#include "prefops.h"
#include "selectops.h"
#define INITIAL_WHOLEWIDTH 160
#define INITIAL_STAFFHEIGHT 100

static remove_all_staffs(DenemoScore * si);


/**
 * Create new DenemoScore with no staff
 * set it as gui->si but do not add it to the movements list.
 */
void
point_to_empty_movement (DenemoGUI *gui)
{
  DenemoScore *newscore = (DenemoScore *) g_malloc0 (sizeof (DenemoScore));
  init_score (newscore, gui);
  if(gui->si && gui->si->buttonbox)
     gtk_widget_hide(gui->si->buttonbox);
  gui->si = newscore;
  gtk_widget_show(gui->si->buttonbox);
}
/**
 * Create new DenemoScore with one empty staff
 * set it as gui->si but do not add it to the movements list.
 */
void
point_to_new_movement (DenemoGUI *gui)
{
  point_to_empty_movement(gui);
  newstaff (gui, INITIAL, DENEMO_NONE);
}

static void
new_movement(GtkAction *action, DenemoScriptParam *param, gboolean before) {
  DenemoGUI *gui = Denemo.gui;
  gint pos = g_list_index(gui->movements, gui->si);
  append_new_movement(action, param);
  DenemoScore *newsi = g_list_last(gui->movements)->data;
  gui->movements = g_list_delete_link(gui->movements, g_list_last(gui->movements));
  gui->movements = g_list_insert(gui->movements, newsi, before?pos:pos+1);
  setcurrentprimarystaff (gui->si);
  write_status(gui);
}


void
append_new_movement(GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *source_movement = gui->si;
  GList *g;
  point_to_empty_movement(gui);
  for(g=source_movement->thescore;g;g=g->next) {
    DenemoStaff *source_staff = g->data;
    newstaff (gui, LAST, DENEMO_NONE);
    GList *dest = g_list_last(gui->si->thescore);
    DenemoStaff *dest_staff = dest->data;
    copy_staff(source_staff, dest_staff);
  }
  gui->movements = g_list_append(gui->movements, gui->si);

  set_width_to_work_with(gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  find_leftmost_allcontexts (gui->si);
  set_bottom_staff (gui);
  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (gui->scorearea);
  gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
  gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
  displayhelper(gui);
  score_status(gui, TRUE); 
}


void
insert_movement_before(GtkAction *action, DenemoScriptParam *param) {
  new_movement(action, param, TRUE);
}
void
insert_movement_after(GtkAction *action, DenemoScriptParam *param) {
  new_movement(action, param, FALSE);
}

void
delete_movement(GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  if(!confirm_deletestaff_custom_scoreblock(gui))
    return;
  GString *primary = g_string_new(""), *secondary = g_string_new("");
  if(gui->movements==NULL || (g_list_length(gui->movements)==1)) {
    g_string_assign(primary, _("This is the only movement"));
    g_string_assign(secondary, _("Delete it and start over?"));
    if(confirm(primary->str, secondary->str)) {
      gchar *name = g_strdup(gui->filename->str);
      deletescore (NULL, gui);
      g_string_assign(gui->filename, name);
      g_free(name);
    }
  } else { // more than one movement
    gint num = g_list_index(gui->movements, gui->si);
    g_string_printf(primary,_("This is movement #%d"), num+1);
    g_string_assign(secondary,_("Delete entire movement?"));
    if( confirm(primary->str, secondary->str)) {
      free_score(gui);
      DenemoScore *si= gui->si;
      GList *g = g_list_find(gui->movements, si)->next;
      if(g==NULL)
	g = g_list_find(gui->movements, si)->prev;
      gui->si = g->data;
      gui->movements = g_list_remove(gui->movements, (gpointer)si);
    }
  }
  g_string_free(primary, TRUE);
  g_string_free(secondary, TRUE);
  displayhelper(gui);
  score_status(gui, TRUE);
}

/* go to the movement voice measure staff and object numbers (starting from 1) */
gboolean
goto_movement_staff_obj (DenemoGUI * gui, gint movementnum, gint staffnum, gint measurenum, gint objnum)
{
  GList *this = g_list_nth(gui->movements, movementnum-1);

  if(this==NULL){
    warningdialog(_("No such movement"));
    return FALSE;
  }
  gtk_widget_hide(gui->si->buttonbox);
  gui->si = this->data;
  gtk_widget_show(gui->si->buttonbox);
  if(!set_currentstaffnum (gui, staffnum))
    {
      warningdialog(_("No such voice"));
      return FALSE;
    }
  if(!set_currentmeasurenum(gui, measurenum))
  {
    warningdialog(_("No such measure"));
    return FALSE;
  }
  while(--objnum>0  && gui->si->currentobject && gui->si->currentobject->next) {
    gui->si->currentobject = gui->si->currentobject->next;
    gui->si->cursor_x++;
  }
  if(objnum)
    return FALSE;
  write_status(gui);
#if 0
  //something bad here re-sets the staff to 1
  updatescoreinfo (gui);
  set_rightmeasurenum (gui->si);
  find_leftmost_allcontexts (gui->si);
  set_bottom_staff (gui);
#endif
  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
  gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
  gtk_widget_queue_draw (gui->scorearea);
  return TRUE;
}

void PopPosition (GtkAction * action, DenemoScriptParam *param) {
  DenemoPosition *pos = pop_position();
  DenemoScriptParam dummy;
  if(action || !param)
    param = &dummy;
  if(pos==NULL) {
    param->status = FALSE;
    return;
  }
  param->status = goto_movement_staff_obj(Denemo.gui, pos->movement, pos->staff, pos->measure, pos->object);
  g_free(pos);
}

void PushPosition (GtkAction * action, DenemoScriptParam *param) {
  push_position();
}

void PopPushPosition (GtkAction * action, DenemoScriptParam *param) {
  DenemoPosition *pos = pop_position();
  DenemoScriptParam dummy;
  if(action || !param)
    param = &dummy;
  if(pos) {
    push_position();
    param->status = goto_movement_staff_obj(Denemo.gui, pos->movement, pos->staff, pos->measure, pos->object);
  } else
    param->status = FALSE;
}


/**
 * Move to the next movement
 * @param action - Gtk Action event
 * @param gui - pointer to the DenemoGUI structure
 * @return none
*/
void
next_movement (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GList *this = g_list_find( gui->movements, gui->si);
  this = this->next;
  if(param)
    param->status = TRUE;
  if(this==NULL){
    if(param)
      param->status = FALSE;
    else
      warningmessage(_("This is the last movement"));
    return;
  }
  gtk_widget_hide(gui->si->buttonbox);
  gui->si = this->data;
  gtk_widget_show(gui->si->buttonbox);
  //!!!!!!!!updatescoreinfo (gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  find_leftmost_allcontexts (gui->si);
  set_bottom_staff (gui);

  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
  gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
  gtk_widget_draw (gui->scorearea, NULL);//KLUDGE FIXME see staffup/down
}

/**
 * Move to the next movement
 * @param action - Gtk Action event
 * @param gui - pointer to the DenemoGUI structure
 * @return none
*/
void
prev_movement (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GList *this = g_list_find( gui->movements, gui->si);
  this = this->prev;
  if(param)
    param->status = TRUE;
  if(this==NULL){
    if(param)
      param->status = FALSE;
    else
      warningmessage(_("This is the first movement"));
    return;
  }
  gtk_widget_hide(gui->si->buttonbox);
  gui->si = this->data; 
  gtk_widget_show(gui->si->buttonbox);

  //!!!!!!!!!!!!!updatescoreinfo (gui);
  //FIXME duplicate code
  set_rightmeasurenum (gui->si);
  set_bottom_staff (gui);

  update_hscrollbar (gui);
  update_vscrollbar (gui);
  gtk_widget_queue_draw (gui->scorearea);
  gtk_signal_emit_by_name (GTK_OBJECT (gui->hadjustment), "changed");
  gtk_signal_emit_by_name (GTK_OBJECT (gui->vadjustment), "changed");
 gtk_widget_draw (gui->scorearea, NULL);//KLUDGE FIXME see staffup/down
}
/**
 * Initialise scoreinfo structure 
 *
 * @param si pointer to the scoreinfo structure to initialise
 */
void
init_score (DenemoScore * si, DenemoGUI *gui)
{
  gchar *dir = (gchar*)locatedotdenemo ();

  si->readonly = 0;
  si->leftmeasurenum = si->rightmeasurenum = 1;
  si->top_staff = si->bottom_staff = 1;
  si->measurewidth = INITIAL_WHOLEWIDTH;
  si->measurewidths = NULL;
  si->staffspace = INITIAL_STAFFHEIGHT;
  si->thescore = NULL;
  si->currentstaffnum = 1;
  si->currentmeasurenum = 1;
  si->currentobject = NULL;
  si->cursor_x = 0;
  si->cursor_y = 0;
  si->staffletter_y = 0;
  si->cursor_appending = TRUE;
  si->zoom = Denemo.prefs.zoom>0?Denemo.prefs.zoom/100.0:1.0;
  si->system_height = Denemo.prefs.system_height>0?Denemo.prefs.system_height/100.0:1.0;

  si->cursoroffend = FALSE;
  si->cursortime1=si->cursortime2=4;
  si->markstaffnum = 0;
  si->markmeasurenum = 0;
  si->markcursor_x = 0;
  si->maxkeywidth = 0;
  si->has_figures = FALSE;
  si->has_fakechords = FALSE;
  /*playback purposes */
  si->tempo = 60;
  si->start = 0;
  si->end = 0;
  si->stafftoplay = 0;

  si->savebuffer = NULL;
  si->bookmarks = NULL;

  if(gui->filename==NULL) {
    gui->filename = g_string_new ("");
    gui->autosavename = g_string_new (dir);
    gui->autosavename = g_string_append (gui->autosavename, "/autosave.denemo");
  }
  si->curlilynode = 0;	 /* the node of the lily parse tree on display in 
			    textwindow */
  si->lily_file = 0;   /* root of lily file parse, see lilyparser.y etc  */




  /* Undo/redo initialisation */
  si->undodata = g_queue_new ();
  si->redodata = g_queue_new ();
  si->undo_redo_mode = UNDO;
  
  si->buttonbox = gtk_hbox_new(FALSE, 1);
  gtk_box_pack_end (GTK_BOX (gui->buttonboxes), si->buttonbox, FALSE, TRUE,
		      0);
  gtk_widget_show (si->buttonbox);
  GTK_WIDGET_UNSET_FLAGS(si->buttonbox, GTK_CAN_FOCUS);
}

static delete_all_staffs(DenemoGUI * gui) {
  DenemoScore *si=gui->si;
  gint i;
  for (i = g_list_length (si->thescore); i>0; i--)
    {
      si->currentstaffnum = i;
      si->currentstaff = g_list_nth (si->thescore, i - 1);
      deletestaff (gui, FALSE);
    }
  return TRUE;
}

/**
 * frees the data in the passed scoreinfo stucture 
 *
 * @param si pointer to the scoreinfo structure to free
 */
void
free_score (DenemoGUI * gui)
{
  delete_all_staffs(gui);
  delete_directives(&gui->si->layout.directives);
  delete_directives(&gui->si->header.directives);
  free_midi_data(gui->si);
  if(gui->si->buttonbox) {
    gtk_widget_destroy(gui->si->buttonbox);
    gui->si->buttonbox = NULL;
  }
  if(gui->si->lyricsbox) {
    gtk_widget_destroy(gui->si->lyricsbox);
    gui->si->lyricsbox = NULL;
  }
  reset_lyrics(NULL, 0);
  g_queue_free (gui->si->undodata);
  g_queue_free (gui->si->redodata);
}

