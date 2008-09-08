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
  gboolean attach;/* whether the LilyPond is to be postfixed to note (else should be a DenemoObject) */
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
insertdirective (GtkWidget * widget, struct callbackdata *cbdata)
{
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
    if(cbdata->attach && (curnote = findnote(curObj, gui->si->cursor_y)) != NULL) {
      if(curnote->directive)
	g_string_assign(curnote->directive, directivestring);
      else
	curnote->directive = g_string_new(directivestring);      
    }
    else {  
      DenemoObject *lily = lily_directive_new (directivestring);
      object_insert (gui, lily);
      lilyobj = (lilydirective *) lily->object;
    }
  if(lilyobj) {
    lilyobj->locked = cbdata->locked;
    if(*directivestring=='%') {//append newline if directive starts with a LilyPond comment indicator
      g_string_append(lilyobj->directive,"\n");
    }
  }
  score_status(gui, TRUE);
  displayhelper(gui);
}
static void  toggle_locked(GtkWidget *widget, gboolean *locked) {
  //g_print("Called with %d\n", *locked);
  *locked = !*locked;
}
/**
 * Lilypond directive.  Allows user to insert a lilypond directive 
 * before the current cursor position
 * or (if ATTACH is true) attach one to the note, 
 * or edit the current lilypond directive
 */
static void
lily_directive (DenemoGUI *gui, gboolean attach, gchar *init)
{
  gchar *string;
  gchar *current = NULL;
  DenemoScore * si = gui->si;
  note *curnote = NULL;
  static struct callbackdata cbdata;
  
  if(init==NULL) {
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  if(attach)
    curnote = findnote(curObj, gui->si->cursor_y);
  if(attach && curnote==NULL) {
    warningdialog("You must put the cursor on a note to postfix LilyPond");//FIXME find a note and ask
    return;
  }
  cbdata.attach = attach;  
  lilydirective *lilyobj=NULL;
  cbdata.locked = FALSE;
  if (curObj && curObj->type == LILYDIRECTIVE && ((lilydirective *) curObj->object)->directive)
	{
		current = ((GString *) (lilyobj = (lilydirective *) curObj->object)->directive)->str;
		cbdata.locked = lilyobj->locked;
	}
  if(curnote && curnote->directive)
   	current = curnote->directive->str;
  GtkToggleButton *button = NULL;
  if(!curnote) {
    button = (GtkToggleButton *)gtk_check_button_new_with_label("locked");
    g_signal_connect(button, "toggled",  G_CALLBACK (toggle_locked), &cbdata.locked);
    if(cbdata.locked)
      gtk_toggle_button_set_active (button, cbdata.locked), cbdata.locked=TRUE;//FIXME how is this supposed to be done?
  }
  string = string_dialog_entry_with_widget(gui, curnote?"Postfix LilyPond":"Insert LilyPond", curnote?"Give LilyPond text to postfix to note of chord":"Give LilyPond text to insert", current, GTK_WIDGET(button));
  } else
    string = g_strdup(init);

  cbdata.gui = gui;
  cbdata.string = string;

  if (string){ 
    insertdirective (NULL, &cbdata);
    displayhelper (gui);
  }

  g_free(string);

}

/**
 * Lilypond directive insert.  Allows user to insert a lilypond directive 
 * before the current cursor position
 * or edit the current lilypond directive
 */
void
lily_directive_insert (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;

  lily_directive (gui, FALSE, action?NULL: ((GString*)param)->str);
}

/**
 * Lilypond directive attach.  Allows user to attach a lilypond directive 
 * to the current note
 * or edit the current lilypond directive
 */
void
lily_directive_postfix (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  lily_directive (gui, TRUE, action?NULL: ((GString*)param)->str);
}
