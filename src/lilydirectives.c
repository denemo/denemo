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

static void  toggle_locked(GtkWidget *widget, gboolean *locked) {
  //g_print("Called with %d\n", *locked);
  *locked = !*locked;
}


typedef enum attach_type {ATTACH_NOTE, ATTACH_CHORD} attach_type;
/**
 * Lilypond directive.  Allows user to insert a lilypond directive 
 * before the current cursor position
 * or (if ATTACH is true) attach one to the note, 
 * or edit the current lilypond directive
 */
static void
attach_lily_directive (attach_type attach, gchar *init, gchar *prefix, gchar *display, gboolean interactive)
{
  gchar *prefixstring=NULL, *current=NULL, *displaystring=NULL;
  DenemoGUI *gui = Denemo.gui;
  DenemoScore * si = gui->si;
  note *curnote = NULL;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  if(curObj==NULL) {  
    if(interactive)
      warningdialog("You must put the cursor on a chord to attach LilyPond");//FIXME find a note and ask
    return;
  }
  chord *thechord = NULL;
  thechord = (chord *)curObj->object;
  if(curObj->type!=CHORD) {  
    if(interactive)
      warningdialog("You must put the cursor on a chord to attach LilyPond");
    return;
  }
    
  curnote = findnote(curObj, gui->si->cursor_y);
  if(attach==ATTACH_NOTE && (curnote==NULL)) {  
    if(interactive)
      warningdialog("You must put the cursor on a note to attach LilyPond to the note");//FIXME find a note and ask
    return;
  }
 
  if(interactive) {
    switch(attach) {
    case ATTACH_CHORD:
      if(thechord->postfix)
	current = thechord->postfix->str;
      if(thechord->prefix)
	prefixstring = thechord->prefix->str;
      if(thechord->display)
	displaystring = thechord->display->str;
      break;;
      break;
    case ATTACH_NOTE:
      if(curnote->postfix)
	current = curnote->postfix->str;
      if(curnote->prefix)
	prefixstring = curnote->prefix->str;
      if(curnote->display)
	displaystring = curnote->display->str;
      break;
    default:
      break;
    }  
    prefixstring = string_dialog_entry(gui, "Attach LilyPond", "Give text to place before the note", prefixstring);
    current = string_dialog_entry(gui, curnote?"Attach LilyPond to Note":"Attach LilyPond to Chord", curnote?"Give LilyPond text to postfix to note of chord":"Give LilyPond text to postfix to chord", current);
    displaystring =  string_dialog_entry(gui, "Attach LilyPond", "Give Display text if required", displaystring);
  } else {//not interactive
    if(prefix)
      prefixstring = g_strdup(prefix);
    if(init)
      current = g_strdup(init);
    if(display)
      displaystring = g_strdup(display);
  }
  switch(attach) {
#define STRINGASSIGN(obj, field, val) \
     if(obj->field && val && *val) g_string_assign(obj->field, val);\
     else obj->field=g_string_new(val);
                     
  case ATTACH_CHORD:
    STRINGASSIGN(thechord, postfix, current);
    STRINGASSIGN(thechord, prefix, prefixstring);
    STRINGASSIGN(thechord, display, displaystring);
    break;
  case ATTACH_NOTE:
    STRINGASSIGN( curnote, postfix, current);
    STRINGASSIGN(curnote, prefix, prefixstring);
    STRINGASSIGN(curnote, display, displaystring);
    break;
  default:
    break;
#undef STRINGASSIGN
  } 
  score_status(gui, TRUE);
  displayhelper (gui);
  g_free(current);
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


static
void insert_lily_directive_now(gchar *directive, gchar *display, gboolean locked, gint minpixels) {
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  lilydirective *lilyobj=NULL; /* a lily directive object */
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
    if (curObj && curObj->type == LILYDIRECTIVE) {
      g_string_assign((lilyobj=(lilydirective *) curObj->object)->directive, directive);
      curObj->minpixelsalloted = minpixels;
    }  else {  
      DenemoObject *lily = lily_directive_new (directive);
      object_insert (gui, lily);
      lilyobj = (lilydirective *) lily->object;
      lily->minpixelsalloted = minpixels;// g_print("min pixels %d\n", lily->minpixelsalloted);
    }
    if(lilyobj) {
      lilyobj->locked = locked;
      if(*directive=='%') {//append newline if directive starts with a LilyPond comment indicator
	g_string_append(lilyobj->directive,"\n");
      }
      if(display) {
	if(lilyobj->display)
	  g_string_assign(lilyobj->display, display);
	else
	  lilyobj->display = g_string_new(display);
      }
    }
    score_status(gui, TRUE);
    displayhelper(gui);
}

static
gboolean get_lily_directive(gchar **directive, gchar **display, gboolean *locked) {
  DenemoGUI *gui = Denemo.gui;
  GtkToggleButton *button = NULL;
  button = (GtkToggleButton *)gtk_check_button_new_with_label("locked");
  g_signal_connect(button, "toggled",  G_CALLBACK (toggle_locked), locked);
  if(*locked)
    gtk_toggle_button_set_active (button, *locked), *locked=TRUE;//FIXME how is this supposed to be done?
  *directive = string_dialog_entry_with_widget(gui, "Insert LilyPond", "Give LilyPond text to insert", *directive, GTK_WIDGET(button));
  if(!*directive)
    return FALSE;
  *display =  string_dialog_entry(gui, "Insert LilyPond", "Give Display text if required", *display);
  return TRUE;
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
  gboolean locked = FALSE;
  if(directive && !display)
    display = directive;
  if(action) {
     if(get_lily_directive(&directive, &display, &locked))
       insert_lily_directive_now(directive, display, locked, 8);
  } else {
    insert_lily_directive_now(directive, display, locked, atoi(minpixels));
  }
}
/**
 * Lilypond directive attach to note.  Allows user to attach a lilypond directive 
 * to the current note
 * or edit it
 */
void
lily_directive_attach_note (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_3PARAMS(action, param, postfix, display, prefix);
  attach_lily_directive (ATTACH_NOTE, postfix, prefix, display, action!=NULL);
}
/**
 * Lilypond directive attach to chord.  Allows user to attach a lilypond directive 
 * to the current chord
 * or edit it
 */
void
lily_directive_attach_chord (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_3PARAMS(action, param, postfix, display, prefix);
  attach_lily_directive (ATTACH_CHORD, postfix, prefix, display, action!=NULL);
}
