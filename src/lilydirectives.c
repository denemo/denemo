/* lilydirectives.cpp 
 * Implements lilydirectives which are not notes 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * A Tee  (c) 2000-2005
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"


struct callbackdata
{
  DenemoGUI *gui;
  gchar *string;
  gboolean locked;
};

/**
 * If the curObj is a chord with a note at the cursor position 
 * return that note, else return NULL
 */
static note *
findnote(DenemoObject *curObj, gint cursory) {
  note *curnote = NULL;
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->notes ) {
    GList *notes = ((chord *) curObj->object)->notes;
    for(;notes; notes = notes->next){
      curnote =  (note*)notes->data;
      //g_print("comparing %d and %d\n", cursory, curnote->y);
      if(cursory == curnote->mid_c_offset)
	break;
      curnote = NULL;
   }

  }
     return curnote;
}
/**
 * Insert the lilypond directive into the score
 *
 */
static void
insertdirective (GtkWidget * widget, gpointer data)
{

  struct callbackdata *cbdata = (struct callbackdata *) data;
  DenemoGUI* gui = cbdata->gui;
  DenemoScore *si = gui->si;
  note *curnote=NULL;/* a note in a chord */
  lilydirective *lilyobj=NULL; /* a lily directive object */
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  gchar *directivestring = cbdata->string;
  if (curObj && curObj->type == LILYDIRECTIVE)
    g_string_assign((lilyobj=(lilydirective *) curObj->object)->directive, directivestring);
  else
    if((curnote = findnote(curObj, gui->si->cursor_y)) != NULL) {
      if(curnote->directive)
	g_string_assign(curnote->directive, directivestring);
      else
	curnote->directive = g_string_new(directivestring);
      score_status(gui, TRUE);
    }
    else {  
      DenemoObject *lily = lily_directive_new (directivestring);
      object_insert (gui, lily);
      if(*directivestring=='%') {//append newline if directive starts with a LilyPond comment indicator
	lilyobj = (lilydirective *) lily->object;
	g_string_append(lilyobj->directive,"\n");
      }
      displayhelper(gui);
    }
  if(lilyobj)
    lilyobj->locked = cbdata->locked;
}
static void  toggle_locked(GtkWidget *widget, gboolean *locked) {
  //g_print("Called with %d\n", *locked);
  *locked = !*locked;
}
/**
 * Lilypond directive.  Allows user to insert a lilypond directive 
 * before the current cursor position
 * or (if the cursor is on a note) attach one to the note, 
 * or edit the current lilypond directive
 */
void
lily_directive (GtkAction * action, DenemoGUI *gui)
{
  gchar *string;
  gchar *current = NULL;
  DenemoScore * si = gui->si;
  static struct callbackdata cbdata;
  
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  lilydirective *lilyobj=NULL;
  cbdata.locked = FALSE;
  if (curObj && curObj->type == LILYDIRECTIVE && ((lilydirective *) curObj->object)->directive)
	{
		current = ((GString *) (lilyobj = (lilydirective *) curObj->object)->directive)->str;
		cbdata.locked = lilyobj->locked;
	}
  note *curnote = findnote(curObj, gui->si->cursor_y);
  if(curnote && curnote->directive)
   	current = curnote->directive->str;
  GtkToggleButton *button = NULL;
  if(!curnote) {
    button = gtk_check_button_new_with_label("locked");
    g_signal_connect(button, "toggled",  G_CALLBACK (toggle_locked), &cbdata.locked);
    if(cbdata.locked)
      gtk_toggle_button_set_active (button, cbdata.locked), cbdata.locked=TRUE;//FIXME how is this supposed to be done?
  }
  string = string_dialog_entry_with_widget(gui, curnote?"Postfix LilyPond":"Insert LilyPond", curnote?"Give LilyPond text to postfix to note of chord":"Give LilyPond text to insert", current, button);
  
  cbdata.gui = gui;
  cbdata.string = string;

  if (string){ 
    insertdirective (NULL, &cbdata);
    displayhelper (gui);
  }

  g_free(string);

}

#if 0
void
rehearsal_mark (GtkAction * action, DenemoGUI *gui)
{
DenemoObject *lily = lily_directive_new (" \\mark \\default ");
 object_insert (gui, lily);
 if (!gui->si->cursor_appending){
   gui->si->cursor_x--;
   gui->si->currentobject = g_list_nth ((objnode *) gui->si->currentmeasure->data, gui->si->cursor_x);
 }
 lily_directive (action, gui);
}
#endif


void  attach_set_accel_callback (gpointer data, GtkAction *action, DenemoGUI *gui);
void
myactivate (GtkAction * action, DenemoGUI *gui)
{
  // the proxy list is NULL until the menu item is first called...
  //BUT if you first activate it with right button ....

  GSList *h = gtk_action_get_proxies (action);//FIXME this can't be needed what is a proxy?
   for(;h;h=h->next) {
     attach_set_accel_callback(h->data, action, gui);
   }
  gchar *text = (gchar*)g_object_get_data(G_OBJECT(action), "lilypond");
DenemoObject *lily = lily_directive_new (text);
 object_insert (gui, lily);
 if (!gui->si->cursor_appending){
   gui->si->cursor_x--;
   gui->si->currentobject = g_list_nth ((objnode *) gui->si->currentmeasure->data, gui->si->cursor_x);
 }
 lily_directive (action, gui);
}
