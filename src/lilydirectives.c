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
  gchar *directive;
  gchar *prefix;
  gchar *display;
  gint minpixels;
  gint spacebefore;
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
 * Insert the lilypond directive object or attaches LilyPond to a note
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
  gchar *directivestring = cbdata->directive;
  gchar *prefixstring = cbdata->prefix;
  gchar *displaystring = cbdata->display;
  if(cbdata->attach) {//ATTACH
    if((curnote = findnote(curObj, gui->si->cursor_y)) != NULL) {
      if(curnote->directive && directivestring)
	g_string_assign(curnote->directive, directivestring);
      else
	curnote->directive = g_string_new(directivestring); 
     if(curnote->prefix && prefixstring)
	g_string_assign(curnote->prefix, prefixstring);
      else
	curnote->prefix = g_string_new(prefixstring); 
    if(curnote->display && displaystring)
	g_string_assign(curnote->display, displaystring);
      else
	curnote->display = g_string_new(displaystring); 
    } else
      g_warning("Attach without note");
  } else { //INSERT
    if (curObj && curObj->type == LILYDIRECTIVE) {
      g_string_assign((lilyobj=(lilydirective *) curObj->object)->directive, directivestring);
      curObj->minpixelsalloted = cbdata->minpixels;
    }  else {  
      DenemoObject *lily = lily_directive_new (directivestring);
      object_insert (gui, lily);
      lilyobj = (lilydirective *) lily->object;
      lily->minpixelsalloted = cbdata->minpixels;// g_print("min pixels %d\n", lily->minpixelsalloted);
    }
    if(lilyobj) {
      lilyobj->locked = cbdata->locked;
      if(*directivestring=='%') {//append newline if directive starts with a LilyPond comment indicator
	g_string_append(lilyobj->directive,"\n");
      }
      if(cbdata->display) {
	if(lilyobj->display)
	  g_string_assign(lilyobj->display, cbdata->display);
	else
	  lilyobj->display = g_string_new(cbdata->display);
      }
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
lily_directive (DenemoGUI *gui, gboolean attach, gchar *init, gchar *prefix, gchar *display, gchar *minpixels)
{
  gchar *directivestring;
  gchar *current = NULL;
  gchar *displaystring = NULL;
  gchar *prefixstring = NULL;
  gint current_minpixels = 8;//used to be set in setpixelmin() in utils.c
  DenemoScore * si = gui->si;
  note *curnote = NULL;
  static struct callbackdata cbdata;
  cbdata.attach = attach; 
  if(!(init || prefix || display)) {
    DenemoObject *curObj = (DenemoObject *) si->currentobject ?
      (DenemoObject *) si->currentobject->data : NULL;
    if(attach)
      curnote = findnote(curObj, gui->si->cursor_y);
    if(attach && curnote==NULL) {
      warningdialog("You must put the cursor on a note to postfix LilyPond");//FIXME find a note and ask
      return;
    }
    lilydirective *lilyobj=NULL;
    cbdata.locked = FALSE;
    /* Edit if on a lilydirective otherwise insert. But do not edit called from scheme with a "lily" value to insert */
    if (curObj && curObj->type == LILYDIRECTIVE && ((lilydirective *) curObj->object)->directive) {
      current = ((GString *) (lilyobj = (lilydirective *) curObj->object)->directive)->str;
      if( ((GString *) (lilyobj = (lilydirective *) curObj->object)->display))
	displaystring = ((GString *) (lilyobj = (lilydirective *) curObj->object)->display)->str;
      cbdata.locked = lilyobj->locked;
    }
    if(attach) {
      if(curnote->directive)
   	current = curnote->directive->str;
      if(curnote->prefix)
	prefixstring = curnote->prefix->str;
      if(curnote->display)
	displaystring = curnote->display->str;
    }


    GtkToggleButton *button = NULL;
    if(!attach) {
      button = (GtkToggleButton *)gtk_check_button_new_with_label("locked");
      g_signal_connect(button, "toggled",  G_CALLBACK (toggle_locked), &cbdata.locked);
      if(cbdata.locked)
	gtk_toggle_button_set_active (button, cbdata.locked), cbdata.locked=TRUE;//FIXME how is this supposed to be done?
    }
    directivestring = string_dialog_entry_with_widget(gui, curnote?"Postfix LilyPond":"Insert LilyPond", curnote?"Give LilyPond text to postfix to note of chord":"Give LilyPond text to insert", current, GTK_WIDGET(button));
    if(attach)
      prefixstring = string_dialog_entry(gui, "Attach LilyPond", "Give text to place before the note", prefix);
    displaystring =  string_dialog_entry(gui, "Insert LilyPond", "Give Display text if required", displaystring);
  } else {// called with initialization string
    if(init)
      directivestring = g_strdup(init);
    if(display)
      displaystring = g_strdup(display);
    if(minpixels)
      current_minpixels = atoi(minpixels);
    if(prefix)
      prefixstring = prefix;
  }

  cbdata.gui = gui;
  cbdata.directive = directivestring;
  cbdata.prefix = prefixstring;
  cbdata.display = displaystring;
  cbdata.minpixels = current_minpixels;

  if (directivestring || prefixstring || displaystring){ 
    insertdirective (NULL, &cbdata);
    displayhelper (gui);
  }

  g_free(directivestring);
  g_free(displaystring);
  g_free(prefixstring);

}


static void
get_lily_parameter(gchar *query, DenemoScriptParam *param) {
  DenemoObject *curObj = (DenemoObject *) Denemo.gui->si->currentobject ?
    (DenemoObject *) Denemo.gui->si->currentobject->data : NULL;
  param->status = curObj && curObj->type==LILYDIRECTIVE;
#define ASSIGN_PARAM(field)  if(!strcmp(#field, query))\
  g_string_assign(param->string, lilyobj->field->str);
  if(param->status)
    {
      lilydirective *lilyobj = (lilydirective *) curObj->object;
      ASSIGN_PARAM(directive);
      ASSIGN_PARAM(display);
      if(!strcmp("minpixels", query))
	g_string_printf(param->string, "%d", curObj->minpixelsalloted);
    }
#undef ASSIGN_PARAM
}
/**
 * Lilypond directive insert.  Allows user to insert a lilypond directive 
 * before the current cursor position
 * or edit the current lilypond directive
 */
void
lily_directive_insert (GtkAction *action, DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_3PARAMS(action, param, directive, display, minpixels);
  //g_print("query is %s\n", query);
  if(query) {
    get_lily_parameter(*query?query:"directive", param);
    return;
  }  
  if(directive && !display)
    display = directive;

  lily_directive (gui, FALSE, directive, NULL, display, minpixels);
}
/**
 * Lilypond directive attach.  Allows user to attach a lilypond directive 
 * to the current note
 * or edit the current lilypond directive
 */
void
lily_directive_attach (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_3PARAMS(action, param, directive, display, prefix);
  lily_directive (gui, TRUE, directive, prefix, display, NULL);
}
