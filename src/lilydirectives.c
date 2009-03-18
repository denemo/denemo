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
#include "lilydirectives.h"
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"
#include "prefops.h"


/**
 * If the curObj is a chord with a note(s)
 * return the first note at or below cursory, or the last note 
 * else return NULL
 */
static note *
findnote(DenemoObject *curObj, gint cursory) {
  note *curnote = NULL;
  if (curObj && curObj->type == CHORD && ((chord *) curObj->object)->notes ) {
    GList *notes = ((chord *) curObj->object)->notes;
    for(;notes; notes = notes->next){
      curnote =  (note*)notes->data;
      //g_print("comparing %d and %d\n", cursory, curnote->y);
      if(cursory <= curnote->mid_c_offset)
	break;
   }

  }
     return curnote;
}

static void  toggle_locked(GtkWidget *widget, gboolean *locked) {
  //g_print("Called with %d\n", *locked);
  *locked = !*locked;
}

/* lookup a directive tagged with TAG in a list DIRECTIVES and return it.
   if TAG is NULL return the first directive
   else return NULL */
static DenemoDirective *find_directive(GList *directives, gchar *tag) {
  DenemoDirective *directive = NULL;
  if(tag) {
    GList *g;
    for(g=directives;g;g=g->next){
      directive = (DenemoDirective *)g->data;
      if(directive->tag && !strcmp(tag, directive->tag->str))
	return directive;
      directive = NULL;
    }
  } else
    directive = (DenemoDirective *)directives->data;
  return directive;
}

static void
free_directive(DenemoDirective *directive) {

  g_free(directive);//FIXME free the GStrings, do nothing to the graphic which may be shared.

}
static gboolean 
delete_directive(GList **directives, gchar *tag) {
  DenemoDirective *directive = NULL;
  if(tag) {
    GList *g;
    for(g=*directives;g;g=g->next){
      directive = (DenemoDirective *)g->data;
      if(directive->tag && !strcmp(tag, directive->tag->str)){
	*directives = g_list_remove(*directives, directive);
	free_directive(directive);
	return TRUE;
      }
    }
  }
  return FALSE;
}

/* free a list of directives and set to NULL */
void
delete_directives(GList **directives) {
  DenemoDirective *directive = NULL; 
  if(directives)
  while(*directives){
    directive = (DenemoDirective *)(*directives)->data;
    *directives = g_list_remove(*directives, directive);
    free_directive(directive);
  }
}



static DenemoDirective*
new_directive(gchar *tag){
  DenemoDirective *directive = (DenemoDirective*)g_malloc0(sizeof(DenemoDirective));
  if(tag)
    directive->tag = g_string_new(tag);
  return directive;
}

static  DenemoObject *findobj(void) {
  DenemoGUI *gui = Denemo.gui;
  DenemoScore * si = gui->si;
  note *curnote = NULL;
  return (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  }

typedef enum attach_type {ATTACH_NOTE, ATTACH_CHORD} attach_type;
/**
 * Denemo directive attach or edit.  
if interactive: Allows user to attach a lilypond directive 
else attache the passed strings as lilypond directive
attachment is to chord ( attach is ATTACH_CHORD) or to the note at the cursor
 */
static void
attach_directive (attach_type attach, gchar *postfix, gchar *prefix, gchar *display, gchar *tag, gboolean interactive)
{
  gchar *prefixstring=NULL, *postfixstring=NULL, *displaystring=NULL;
  DenemoGUI *gui = Denemo.gui;
  DenemoScore * si = gui->si;
  note *curnote = NULL;
  DenemoObject *curObj = findobj();
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
 
  // setup directive to be data from thechord->directives or curnote->directives which has matching tag, or first if tag is NULL.
  DenemoDirective *directive=NULL;
    switch(attach) {
    case  ATTACH_CHORD:
      if(thechord->directives==NULL) {
	directive = new_directive(tag);
	thechord->directives = g_list_append(NULL, directive);
      } else {
	directive = find_directive(thechord->directives, tag);
	if(directive == NULL) {
	  if(tag) {
	    directive = new_directive(tag);
	    thechord->directives = g_list_append(thechord->directives, directive);
	  }
	}
      }
      break;
     case  ATTACH_NOTE:
      if(curnote->directives==NULL) {
	directive = new_directive(tag);
	curnote->directives = g_list_append(NULL, directive);
      } else {
	directive = find_directive(curnote->directives, tag);
	if(directive == NULL) {
	  if(tag) {
	    directive = new_directive(tag);
	    curnote->directives = g_list_append(curnote->directives, directive);
	  }
	}
      }
      break;
    default:
      g_warning("Error in attach type");
      return;
    }

  if(interactive) {
      if(directive->postfix)
	postfixstring = directive->postfix->str;
      if(directive->prefix)
	prefixstring = directive->prefix->str;
      if(directive->display)
	displaystring = directive->display->str;

    prefixstring = string_dialog_entry(gui, "Attach LilyPond", "Give text to place before the note", prefixstring);
    postfixstring = string_dialog_entry(gui, curnote?"Attach LilyPond to Note":"Attach LilyPond to Chord", curnote?"Give LilyPond text to postfix to note of chord":"Give LilyPond text to postfix to chord", postfixstring);
    displaystring =  string_dialog_entry(gui, "Attach LilyPond", "Give Display text if required", displaystring);
  } else {//not interactive
    if(prefix)
      prefixstring = g_strdup(prefix);
    if(postfix)
      postfixstring = g_strdup(postfix);
    if(display)
      displaystring = g_strdup(display);
  }

#define STRINGASSIGN(field, val) \
     if(val && *val) {\
     if(directive->field) g_string_assign(directive->field, val);\
     else directive->field=g_string_new(val);}
    STRINGASSIGN(postfix, postfixstring);
    STRINGASSIGN(prefix, prefixstring);
    STRINGASSIGN(display, displaystring);

#undef STRINGASSIGN

  score_status(gui, TRUE);
  displayhelper (gui);
  g_free(postfixstring);
  g_free(displaystring);
  g_free(prefixstring);
}

static void create_directives(GList **directives, gchar *tag) {
  *directives = g_list_append(NULL, new_directive(tag));
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
      ASSIGN_PARAM(postfix);
      ASSIGN_PARAM(display);
      if(!strcmp("minpixels", query))
	g_string_printf(param->string, "%d", curObj->minpixelsalloted);
    }
#undef ASSIGN_PARAM
}


static
void insert_lily_directive(gchar *postfix, gchar *display, gboolean locked, gint minpixels) {
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  DenemoObject *lily;
  lilydirective *lilyobj=NULL; /* a lily directive object */
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  if(postfix==NULL)
    postfix="";
  gboolean is_new = FALSE;
    if (curObj && curObj->type == LILYDIRECTIVE) {
      g_string_assign((lilyobj=(lilydirective *) curObj->object)->postfix, postfix);
      curObj->minpixelsalloted = minpixels;
    }  else {  
      lily = lily_directive_new (postfix);
      is_new= TRUE;
      lilyobj = (lilydirective *) lily->object;
      lily->minpixelsalloted = minpixels;// g_print("min pixels %d\n", lily->minpixelsalloted);
    }
    if(lilyobj) {
      lilyobj->locked = locked;
      if(*postfix=='%') {//append newline if directive starts with a LilyPond comment indicator
	g_string_append(lilyobj->postfix,"\n");
      }
      if(display) {
	if(lilyobj->display)
	  g_string_assign(lilyobj->display, display);
	else
	  lilyobj->display = g_string_new(display);
      }
    }
    if(is_new)
      object_insert (gui, lily);
    score_status(gui, TRUE);
    displayhelper(gui);
}


/* Run a dialog to get a lily directive from the user
 the values returned must be freed by the caller */
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

/* return the directive tagged tag if present at cursor postion
 if tag is NULL, return any directive at current position*/
static
DenemoDirective *get_standalone_directive(gchar *tag){
    DenemoObject *curObj = (DenemoObject *) Denemo.gui->si->currentobject ?
      (DenemoObject *)  Denemo.gui->si->currentobject->data : NULL;
    if (curObj && curObj->type == LILYDIRECTIVE) {
      DenemoDirective *ret = (DenemoDirective *)curObj->object;
      if(tag==NULL)
	return ret;
      if(ret && ret->tag && strcmp(tag, ret->tag->str))
	 ret = NULL;	
      return ret;
    }
    return NULL;
}
/**
 * Denemo directive insert/Edit.  Allows user to insert a Denemo directive 
 * before the current cursor position
 * or edit the current Denemo directive
 */
void
standalone_directive (GtkAction *action, DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_4PARAMS(action, param, directive, postfix, display, minpixels);
  //g_print("query is %s\n", query);
  if(directive)
    postfix = directive;//support for simpler syntax directive=xxx instead of postfix=xxx
  if(query) {
    get_lily_parameter(*query?query:"postfix", param);
    return;
  }
  gboolean locked = FALSE;
  if(postfix && !display)
    display = postfix;
  if(action) {
    DenemoObject *curObj = (DenemoObject *) gui->si->currentobject ?
      (DenemoObject *) gui->si->currentobject->data : NULL;
    if (curObj && curObj->type == LILYDIRECTIVE) {
      lilydirective *lilyobj=(lilydirective *) curObj->object;
      postfix = lilyobj->postfix? lilyobj->postfix->str:NULL;
      display = lilyobj->display? lilyobj->display->str:NULL;
    }
    if(get_lily_directive(&postfix, &display, &locked))
      insert_lily_directive(postfix, display, locked, 8);
  } else {
    insert_lily_directive(postfix, display, locked, minpixels?atoi(minpixels):8);
  }
}
/**
 * callback for AttachLilyToNote (command name is historical)
 * note_directive DenemoDirective attach to chord.  The DenemoDirective is tagged with TAG
 * and attached the note below cursor in current chord
 * if one tagged with TAG already exists, it edits it with the passed values. 
 * Only postfix, prefix and display can be set with this call (for backward compatibility)
 */
void
note_directive (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_4PARAMS(action, param, postfix, display, prefix, tag);
  attach_directive (ATTACH_NOTE, postfix, prefix, display, tag, action!=NULL);
}
/**
 * callback for AttachLilyToChord (command name is historical)
 * chord_directive DenemoDirective attach to chord.  The DenemoDirective is tagged with TAG
 * and attached the current chord
 * if one tagged with TAG already exists, it edits it with the passed values. 
 * Only postfix, prefix and display can be set with this call (for backward compatibility)
 */
void
chord_directive (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_4PARAMS(action, param, postfix, display, prefix, tag);
  attach_directive (ATTACH_CHORD, postfix, prefix, display, tag, action!=NULL);
}



static DenemoObject *get_chordobject(void) {
  chord *thechord = NULL;
  DenemoObject *curObj = findobj();
  if(curObj==NULL)
    return NULL;
  thechord = (chord *)curObj->object;
  if(curObj->type!=CHORD) {  
    return NULL;
  }
  return curObj;
}
static chord *get_chord(void) {
  chord *thechord = NULL;
  DenemoObject *curObj = get_chordobject();
  if(curObj==NULL)
    return NULL;
  return (chord *)curObj->object;
}

static DenemoLilyControl *get_score(void) {
  return &Denemo.gui->lilycontrol;
}


static note *get_note(void) {
  DenemoGUI *gui = Denemo.gui;
  note *curnote = NULL;
  chord *thechord = NULL;
  DenemoObject *curObj = get_chordobject();
  if(curObj==NULL)
    return NULL;
  return findnote(curObj, gui->si->cursor_y);
}


static DenemoStaff *get_staff(void) {
  if(Denemo.gui->si->currentstaff==NULL)
    return NULL;
  return Denemo.gui->si->currentstaff->data;
}
#define get_voice get_staff


static
DenemoDirective *get_note_directive(gchar *tag) {
  note *curnote = get_note();
  if(curnote==NULL || (curnote->directives==NULL))
    return NULL;
  return find_directive(curnote->directives, tag);
}

static
DenemoDirective *get_chord_directive(gchar *tag) {
DenemoObject *curObj = get_chordobject();
  if(curObj==NULL)
    return NULL;
  chord *thechord = (chord *)curObj->object;
  if(thechord->directives==NULL)
    return NULL;
  return find_directive(thechord->directives, tag);
}

static
DenemoDirective *get_score_directive(gchar *tag) {

  return find_directive(Denemo.gui->lilycontrol.directives, tag);
}

static
DenemoDirective *get_staff_directive(gchar *tag) {
  if(Denemo.gui->si->currentstaff==NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  //FIXME return NULL if not primary staff
  if(curstaff==NULL || curstaff->staff_directives==NULL)
    return NULL;
  return find_directive(curstaff->staff_directives, tag);
}
static
DenemoDirective *get_voice_directive(gchar *tag) {
  if(Denemo.gui->si->currentstaff==NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  if(curstaff==NULL || curstaff->voice_directives==NULL)
    return NULL;
  return find_directive(curstaff->voice_directives, tag);
}

gboolean
delete_staff_directive(gchar *tag) {
  if(Denemo.gui->si->currentstaff==NULL)
    return;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  if(curstaff==NULL || curstaff->staff_directives==NULL)
    return;
  return delete_directive(&curstaff->staff_directives, tag);
}
gboolean
delete_voice_directive(gchar *tag) {
  if(Denemo.gui->si->currentstaff==NULL)
    return FALSE;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  if(curstaff==NULL || curstaff->voice_directives==NULL)
    return FALSE;
  return delete_directive(&curstaff->voice_directives, tag);
}
gboolean delete_note_directive(gchar *tag) {
  note *curnote = get_note();
  if(curnote==NULL || (curnote->directives==NULL))
    return FALSE;
  DenemoDirective *directive = get_note_directive(tag);
  if(directive==NULL)
    return FALSE;
  return delete_directive(&curnote->directives, tag);
}
gboolean delete_chord_directive(gchar *tag) {
DenemoObject *curObj = get_chordobject();
  if(curObj==NULL)
    return FALSE;
  chord *thechord = (chord *)curObj->object;
  if(thechord->directives==NULL)
    return FALSE;
  DenemoDirective *directive = get_chord_directive(tag);
  if(directive==NULL)
    return FALSE;
  return delete_directive(&thechord->directives, tag);
}

gboolean delete_score_directive(gchar *tag) {

  DenemoDirective *directive = get_score_directive(tag);
  if(directive==NULL)
    return FALSE;
  return delete_directive(&Denemo.gui->lilycontrol.directives, tag);
}

#define GET_STR_FIELD_FUNC(what, field)\
gchar *\
what##_directive_get_##field(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive && directive->field)\
    return directive->field->str;\
  return NULL;\
}
//typdefs to make the macros defined below pick up the right structure for staff, voice and score as chord & note do
typedef DenemoStaff staff;
typedef DenemoStaff voice;
typedef DenemoLilyControl score;
#define PUT_STR_FIELD_FUNC_NAME(what, field, name)\
gboolean \
what##_directive_put_##field(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  if(directive->field)\
    g_string_assign(directive->field, value);\
  else\
    directive->field = g_string_new(value);\
  return TRUE;\
}

#define PUT_STR_FIELD_FUNC(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, directives)
#define PUT_STR_FIELD_FUNCS(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, staff_directives)
#define PUT_STR_FIELD_FUNCV(what, field) PUT_STR_FIELD_FUNC_NAME(what, field, voice_directives)



GET_STR_FIELD_FUNC(chord, prefix)
GET_STR_FIELD_FUNC(chord, postfix)
GET_STR_FIELD_FUNC(chord, display)

PUT_STR_FIELD_FUNC(chord, prefix)
PUT_STR_FIELD_FUNC(chord, postfix)
PUT_STR_FIELD_FUNC(chord, display)


GET_STR_FIELD_FUNC(note, prefix)
GET_STR_FIELD_FUNC(note, postfix)
GET_STR_FIELD_FUNC(note, display)

PUT_STR_FIELD_FUNC(note, prefix)
PUT_STR_FIELD_FUNC(note, postfix)
PUT_STR_FIELD_FUNC(note, display)

PUT_STR_FIELD_FUNC(score, prefix)
PUT_STR_FIELD_FUNC(score, postfix)
PUT_STR_FIELD_FUNC(score, display)

GET_STR_FIELD_FUNC(standalone, prefix)
GET_STR_FIELD_FUNC(standalone, postfix)
GET_STR_FIELD_FUNC(standalone, display)

GET_STR_FIELD_FUNC(score, prefix)
GET_STR_FIELD_FUNC(score, postfix)
GET_STR_FIELD_FUNC(score, display)

GET_STR_FIELD_FUNC(staff, prefix)
GET_STR_FIELD_FUNC(staff, postfix)
GET_STR_FIELD_FUNC(staff, display)

PUT_STR_FIELD_FUNCS(staff, prefix)
PUT_STR_FIELD_FUNCS(staff, postfix)
PUT_STR_FIELD_FUNCS(staff, display)

GET_STR_FIELD_FUNC(voice, prefix)
GET_STR_FIELD_FUNC(voice, postfix)
GET_STR_FIELD_FUNC(voice, display)

PUT_STR_FIELD_FUNCV(voice, prefix)
PUT_STR_FIELD_FUNCV(voice, postfix)
PUT_STR_FIELD_FUNCV(voice, display)
#undef staff
#undef GET_STR_FIELD_FUNC
#undef PUT_STR_FIELD_FUNC

#define PUT_INT_FIELD_FUNC_NAME(what, field, name)\
gboolean \
what##_directive_put_##field(gchar *tag, gint value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->name==NULL)\
       create_directives (&current->name, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->name = g_list_append(current->name, directive);\
    }\
  directive->field = value;\
  return TRUE;\
}
#define PUT_INT_FIELD_FUNC(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, directives)
#define PUT_INT_FIELD_FUNCS(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, staff_directives)
#define PUT_INT_FIELD_FUNCV(what, field)  PUT_INT_FIELD_FUNC_NAME(what, field, voice_directives)


#define GET_INT_FIELD_FUNC(what, field)\
gint \
what##_directive_get_##field(gchar *tag) {\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive)\
    return directive->field;\
  return 0;\
}




     /* block which can be copied for new int fields */
PUT_INT_FIELD_FUNC(note, minpixels)
PUT_INT_FIELD_FUNC(chord, minpixels)
PUT_INT_FIELD_FUNCS(staff, minpixels)
PUT_INT_FIELD_FUNCV(voice, minpixels)
PUT_INT_FIELD_FUNC(score, minpixels)
     //standalone needs different code for "put" see STANDALONE_PUT* below
GET_INT_FIELD_FUNC(note, minpixels)
GET_INT_FIELD_FUNC(chord, minpixels)
GET_INT_FIELD_FUNC(staff, minpixels)
GET_INT_FIELD_FUNC(voice, minpixels)
GET_INT_FIELD_FUNC(score, minpixels)
GET_INT_FIELD_FUNC(standalone, minpixels)
  /* end block which can be copied for new int fields */


PUT_INT_FIELD_FUNC(note, override)
PUT_INT_FIELD_FUNC(chord, override)
PUT_INT_FIELD_FUNCS(staff, override)
PUT_INT_FIELD_FUNCV(voice, override)
PUT_INT_FIELD_FUNC(score, override)
     //standalone has nothing to override
GET_INT_FIELD_FUNC(note, override)
GET_INT_FIELD_FUNC(chord, override)
GET_INT_FIELD_FUNC(staff, override)
GET_INT_FIELD_FUNC(voice, override)
GET_INT_FIELD_FUNC(score, override)




PUT_INT_FIELD_FUNC(note, x)
PUT_INT_FIELD_FUNC(chord, x)
PUT_INT_FIELD_FUNCS(staff, x)
PUT_INT_FIELD_FUNCV(voice, x)

GET_INT_FIELD_FUNC(note, x)
GET_INT_FIELD_FUNC(chord, x)
GET_INT_FIELD_FUNC(staff, x)
GET_INT_FIELD_FUNC(voice, x)
 
PUT_INT_FIELD_FUNC(note, y)
PUT_INT_FIELD_FUNC(chord, y)
PUT_INT_FIELD_FUNCS(staff, y)
PUT_INT_FIELD_FUNCV(voice, y)

GET_INT_FIELD_FUNC(note, y)
GET_INT_FIELD_FUNC(chord, y)
GET_INT_FIELD_FUNC(staff, y)
GET_INT_FIELD_FUNC(voice, y)
 

PUT_INT_FIELD_FUNC(note, tx)
PUT_INT_FIELD_FUNC(chord, tx)
PUT_INT_FIELD_FUNCS(staff, tx)
PUT_INT_FIELD_FUNCV(voice, tx)
GET_INT_FIELD_FUNC(note, tx)
GET_INT_FIELD_FUNC(chord, tx)
GET_INT_FIELD_FUNC(staff, tx)
GET_INT_FIELD_FUNC(voice, tx)

PUT_INT_FIELD_FUNC(note, ty)
PUT_INT_FIELD_FUNC(chord, ty)
PUT_INT_FIELD_FUNCS(staff, ty)
PUT_INT_FIELD_FUNCV(voice, ty)
GET_INT_FIELD_FUNC(note, ty)
GET_INT_FIELD_FUNC(chord, ty)
GET_INT_FIELD_FUNC(staff, ty)
GET_INT_FIELD_FUNC(voice, ty)


PUT_INT_FIELD_FUNC(note, gx)
PUT_INT_FIELD_FUNC(chord, gx)
PUT_INT_FIELD_FUNCS(staff, gx)
PUT_INT_FIELD_FUNCV(voice, gx)
GET_INT_FIELD_FUNC(note, gx)
GET_INT_FIELD_FUNC(chord, gx)
GET_INT_FIELD_FUNC(staff, gx)
GET_INT_FIELD_FUNC(voice, gx)


PUT_INT_FIELD_FUNC(note, gy)
PUT_INT_FIELD_FUNC(chord, gy)
PUT_INT_FIELD_FUNCS(staff, gy)
PUT_INT_FIELD_FUNCV(voice, gy)
GET_INT_FIELD_FUNC(note, gy)
GET_INT_FIELD_FUNC(chord, gy)
GET_INT_FIELD_FUNC(staff, gy)
GET_INT_FIELD_FUNC(voice, gy)


GET_INT_FIELD_FUNC(standalone, x)
GET_INT_FIELD_FUNC(standalone, y)

GET_INT_FIELD_FUNC(standalone, tx)
GET_INT_FIELD_FUNC(standalone, ty)

GET_INT_FIELD_FUNC(standalone, gx)
GET_INT_FIELD_FUNC(standalone, gy)


GET_INT_FIELD_FUNC(score, x)
GET_INT_FIELD_FUNC(score, y)

GET_INT_FIELD_FUNC(score, tx)
GET_INT_FIELD_FUNC(score, ty)

GET_INT_FIELD_FUNC(score, gx)
GET_INT_FIELD_FUNC(score, gy)

PUT_INT_FIELD_FUNC(score, x)
PUT_INT_FIELD_FUNC(score, y)

PUT_INT_FIELD_FUNC(score, tx)
PUT_INT_FIELD_FUNC(score, ty)

PUT_INT_FIELD_FUNC(score, gx)
PUT_INT_FIELD_FUNC(score, gy)
PUT_INT_FIELD_FUNC(score, width)
PUT_INT_FIELD_FUNC(score, height)



     /* width and height of graphic (if any), read only */
GET_INT_FIELD_FUNC(note, width)
GET_INT_FIELD_FUNC(chord, width)
GET_INT_FIELD_FUNC(staff, width)
GET_INT_FIELD_FUNC(voice, width)
GET_INT_FIELD_FUNC(standalone, width)
GET_INT_FIELD_FUNC(score, width)
GET_INT_FIELD_FUNC(note, height)
GET_INT_FIELD_FUNC(chord, height)
GET_INT_FIELD_FUNC(staff, height)
GET_INT_FIELD_FUNC(voice, height)
GET_INT_FIELD_FUNC(standalone, height)
GET_INT_FIELD_FUNC(score, height)

#undef PUT_INT_FIELD_FUNC
#undef GET_INT_FIELD_FUNC

     //note I think you cannot change the graphic once you have set it.
#define PUT_GRAPHIC_NAME(what, directives) gboolean \
what##_directive_put_graphic(gchar *tag, gchar *value) {\
  what *current = get_##what();\
  if(current==NULL) return FALSE;\
  if(current->directives==NULL)\
       create_directives (&current->directives, tag);\
  DenemoDirective *directive = get_##what##_directive(tag);\
  if(directive==NULL){\
    directive=new_directive(tag);\
    current->directives = g_list_append(current->directives, directive);\
    }\
  loadGraphicItem(value, &directive->graphic, &directive->width, &directive->height);\
  if(directive->graphic_name)\
     g_string_assign(directive->graphic_name, value);\
  else\
      directive->graphic_name = g_string_new(value);\
  return TRUE;\
}

#define PUT_GRAPHIC(what) PUT_GRAPHIC_NAME(what, directives)
#define PUT_GRAPHICS(what) PUT_GRAPHIC_NAME(what, staff_directives)
#define PUT_GRAPHICV(what) PUT_GRAPHIC_NAME(what, voice_directives)


PUT_GRAPHIC(chord);
PUT_GRAPHIC(note);
PUT_GRAPHIC(score);
PUT_GRAPHICS(staff);
PUT_GRAPHICV(voice);
#undef PUT_GRAPHIC

gboolean
standalone_directive_put_graphic(gchar *tag, gchar *value) {
  DenemoDirective *directive = get_standalone_directive(tag);
  if(directive && directive->graphic) {
    
    // directive->graphic = NULL; FIXME should we do this...
    //g_string_free(directive->graphic_name, TRUE);
  }
  if(!directive) {
   	DenemoObject *obj = lily_directive_new (" ");
        directive = (DenemoDirective*)obj->object;
        directive->tag = g_string_new(tag);
	object_insert(Denemo.gui, obj);
  }
  if( loadGraphicItem(value, &directive->graphic, &directive->width, &directive->height)) {
    if(directive->graphic_name)
      g_string_assign(directive->graphic_name, value);
    else
      directive->graphic_name = g_string_new(value);
    return TRUE;
  } else 
    return FALSE;
}



#define STANDALONE_PUT_STR_FIELD_FUNC(field)\
gboolean \
standalone_directive_put_##field(gchar *tag, gchar *value) {\
  DenemoDirective *directive = get_standalone_directive(tag);\
  if(directive && directive->field)\
    g_string_assign(directive->field, value);\
  else if(directive)\
    directive->field = g_string_new(value);\
  else {\
	DenemoObject *obj = lily_directive_new (" ");\
        directive = (DenemoDirective*)obj->object;\
        directive->tag = g_string_new(tag);\
        directive->field = g_string_new(value);\
	object_insert(Denemo.gui, obj);\
   }\
  return TRUE;\
}

STANDALONE_PUT_STR_FIELD_FUNC(prefix);
STANDALONE_PUT_STR_FIELD_FUNC(postfix);
STANDALONE_PUT_STR_FIELD_FUNC(display);




#undef STANDALONE_PUT_STR_FIELD_FUNC

#define STANDALONE_PUT_INT_FIELD_FUNC(field)\
gboolean \
standalone_directive_put_##field(gchar *tag, gint value) {\
  DenemoDirective *directive = get_standalone_directive(tag);\
  if(directive)\
    directive->field = value;\
  else {\
        DenemoObject *obj = lily_directive_new (" ");\
        directive = (DenemoDirective*)obj->object;\
        directive->tag = g_string_new(tag);\
        directive->field = value;\
	object_insert(Denemo.gui, obj);\
   }\
  return TRUE;\
}

//STANDALONE_PUT_INT_FIELD_FUNC(minpixels); special case
STANDALONE_PUT_INT_FIELD_FUNC(x);
STANDALONE_PUT_INT_FIELD_FUNC(y);
STANDALONE_PUT_INT_FIELD_FUNC(tx);
STANDALONE_PUT_INT_FIELD_FUNC(ty);
STANDALONE_PUT_INT_FIELD_FUNC(gx);
STANDALONE_PUT_INT_FIELD_FUNC(gy);

#undef STANDALONE_PUT_INT_FIELD_FUNC
gboolean 
standalone_directive_put_minpixels(gchar *tag, gint value) {
  DenemoDirective *directive = get_standalone_directive(tag);
  if(directive){
    directive->minpixels = value;//This field is not actually useful for standalone directives.
    DenemoObject *obj = findobj();
    obj->minpixelsalloted = value;
  }
  else {
        DenemoObject *obj = lily_directive_new (" ");
        directive = (DenemoDirective*)obj->object;
        directive->tag = g_string_new(tag);
        obj->minpixelsalloted = directive->minpixels = value;
	object_insert(Denemo.gui, obj);
   }
  return TRUE;
}

/* returns the path to a script file for editing a directive created by command of name commandname 
   a local one takes precedence over the system one
   caller must g_free the result */
static gchar *
get_script_file(gchar *commandname) {
  gchar *name = g_strconcat(commandname, ".scm", NULL);
  gchar* filename = g_build_filename (locatedotdenemo(), "actions", "editscripts", name, NULL);
  if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    g_free(filename);
    filename = g_build_filename(get_data_dir(), "actions", "editscripts", name, NULL);
    if(g_file_test(filename, G_FILE_TEST_EXISTS))
      return filename;
    g_free(filename);
    return NULL;
  }
  return filename;
}

static gboolean
script_file_exists(gchar *commandname){
  commandname = get_script_file(commandname);
  if(commandname==NULL) return FALSE;
  g_free(commandname);
  return TRUE;
}

static gboolean
tag_choice(GtkWidget *widget, DenemoDirective **response) {
  if( gtk_toggle_button_get_active(GTK_TOGGLE_BUTTON(widget)))
    *response = g_object_get_data(G_OBJECT(widget), "choice");
  return TRUE;
}


/* pack radio buttons for directive choice */
static gint
pack_buttons(GtkWidget *vbox, GList *directives, DenemoDirective **response) {
  GList *g;
  gint count;
  GtkWidget *widget, *widget2;
  for(count=0, g=directives;g;g=g->next) {
    DenemoDirective *directive = (DenemoDirective *) g->data;
    if (directive->tag && script_file_exists(directive->tag->str)){
      count++;
      if(*response==NULL)
	*response = directive;
      if(g==directives) {
	widget =   gtk_radio_button_new_with_label(NULL, directive->tag->str);
	g_object_set_data(G_OBJECT(widget), "choice", directive);
	g_signal_connect(G_OBJECT(widget), "toggled", G_CALLBACK(tag_choice), response);
	gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);
      } else {
	widget2  =   gtk_radio_button_new_with_label_from_widget(GTK_RADIO_BUTTON (widget), directive->tag->str);
	g_object_set_data(G_OBJECT(widget2), "choice", directive);
	g_signal_connect(G_OBJECT(widget2), "toggled", G_CALLBACK(tag_choice), response);
	gtk_box_pack_start (GTK_BOX (vbox), widget2, FALSE, TRUE, 0);
      }
    }
  }
  return count;
}


/* let the user choose from a list of directives */
static
DenemoDirective *select_directive(gchar *instr, GList *directives) {

  GtkWidget *dialog = gtk_dialog_new_with_buttons ("Select Directive",
                                        GTK_WINDOW (Denemo.window),
                                        (GtkDialogFlags) (GTK_DIALOG_MODAL |
                                                       GTK_DIALOG_DESTROY_WITH_PARENT),
                                        GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
                                        GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
                                        NULL);
  GtkWidget *vbox = gtk_vbox_new(FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), vbox,
		      TRUE, TRUE, 0);
  
  DenemoDirective *response = NULL;
  GList *g;
  gint count;//count tagged directives
  GtkWidget *widget, *widget2;
  widget = gtk_label_new(instr);
  gtk_box_pack_start (GTK_BOX (vbox), widget, FALSE, TRUE, 0);
  count = pack_buttons(vbox, directives, &response);

  if(count>0) {    
    gtk_widget_show_all (dialog);
    if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_REJECT){ 
      response = NULL;
    }
  }
  gtk_widget_destroy (dialog);
  if(response && response->tag)
    g_print("Came back with response %s\n", response->tag->str);
  return response;
}


/* let the user choose from the directives at the cursor */
static 
DenemoDirective *get_directive(void) {
  DenemoDirective *directive = get_standalone_directive(NULL);
  if(directive)
    return directive;
  note *curnote = get_note();
  if(curnote==NULL)
    return NULL;//if we allow chord directives on rests this must change.
  gchar *name = mid_c_offsettolily(curnote->mid_c_offset, curnote->enshift);
  if(curnote->mid_c_offset == Denemo.gui->si->cursor_y)
    if(curnote->directives) {
      gchar *instr = g_strdup_printf("Select a directive attached to the note \"%s\"", name);
      directive = select_directive(instr, curnote->directives);
      g_free(instr);
      if(directive)
	return directive;
    }
  if(directive==NULL) {
  // not exactly on a note, offer any chord directives
    gchar *instr = "Select a directive attached to the chord";
    chord *curchord = get_chord();
    if(curchord && curchord->directives) {
      directive = select_directive(instr, curchord->directives);
    } 
  }
  if(directive==NULL)//try nearest note
    if(curnote->directives && curnote->mid_c_offset != Denemo.gui->si->cursor_y) {
      gchar *instr = g_strdup_printf("Select a directive attached to the note \"%s\"", name);
      directive = select_directive(instr, curnote->directives);
      g_free(instr);
      if(directive && (g_list_length(curnote->directives)==1)) {
	/* seek confirmation of the choice of this directive since it is on a note not pointed at and
	   has been chosen automatically. */
	gchar *name = mid_c_offsettolily(curnote->mid_c_offset, curnote->enshift);
	gchar *msg = g_strdup_printf("Edit the directive %s on note \"%s\"?", directive->tag->str, name);

	if(!confirm("Edit Directive", msg))
	  directive = NULL;
	g_free(name);
	g_free(msg);
      }
    }
  g_free(name);
  return directive;
}





void edit_object(GtkAction *action,  DenemoScriptParam *param) {
  DenemoObject *obj = findobj();
  if(obj==NULL){
      warningdialog("No object here to edit");
    return;
  }
  switch(obj->type){
  case LILYDIRECTIVE:
    edit_object_directive(action, param);
    return;
  case CLEF:
     {
      popup_menu( "/ClefEditPopup");
    }  
     return;
  case KEYSIG:
     {
      popup_menu( "/KeyEditPopup");
    }  
     return;
  case TIMESIG:
     {
      popup_menu( "/TimeEditPopup");
    }  
     return;
  case CHORD:
    {
      popup_menu( "/NoteEditPopup");
    }
    return;

  case STEMDIRECTIVE:
     {
      popup_menu( "/StemControlPopup");
    }  
     return;

  default:
    warningdialog("No method for editing this type of object");
    return;
  }
}

static void
edit_directive(DenemoDirective *directive) {
  gchar *name = g_strconcat(directive->tag->str, ".scm", NULL);
  gchar* filename = g_build_filename (locatedotdenemo(), "actions", "editscripts", name, NULL);
  if(!g_file_test(filename, G_FILE_TEST_EXISTS)) {
    g_free(filename);
    filename = g_build_filename(get_data_dir(), "actions", "editscripts", name, NULL);
    if(!g_file_test(filename, G_FILE_TEST_EXISTS))
      warningdialog("No editing script provided, perhaps edit in LilyPond window?");
    return;
  }
  GError *error = NULL;
  gchar *script;
  if(g_file_get_contents (filename, &script, NULL, &error)) {
    call_out_to_guile(script);
    g_free(script);
  }
}

/**
 * callback for EditDirective 
 */
void edit_object_directive(GtkAction *action,  DenemoScriptParam *param) {
  //g_print("Edit directive called\n");
  DenemoDirective *directive = get_directive();
  g_print("Got directive %p\n", directive);
  if(directive==NULL)
    return;
  if(directive->tag == NULL)
    warningdialog("Use the old Other->Insert LilyPond command");
  else {
    edit_directive(directive);

  }
}
static
DenemoDirective *select_score_directive(void) {
  if(Denemo.gui->lilycontrol.directives==NULL)
    return NULL;
  return select_directive("Select a score directive", Denemo.gui->lilycontrol.directives);
}

static
DenemoDirective *select_staff_directive(void) {
  if(Denemo.gui->si->currentstaff==NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  //FIXME return NULL if not primary staff
  if(curstaff==NULL || curstaff->staff_directives==NULL)
    return NULL;
  return select_directive("Select a staff directive", curstaff->staff_directives);
}
static
DenemoDirective *select_voice_directive(void) {
  if(Denemo.gui->si->currentstaff==NULL)
    return NULL;
  DenemoStaff *curstaff = Denemo.gui->si->currentstaff->data;
  if(curstaff==NULL || curstaff->voice_directives==NULL)
    return NULL;
  return select_directive("Select a voice directive", curstaff->voice_directives);
}


/**
 * callback for EditVoiceDirective 
 */
void edit_voice_directive(GtkAction *action,  DenemoScriptParam *param) {
  //g_print("Edit directive called\n");
  DenemoDirective *directive = select_voice_directive();
  g_print("Got directive %p\n", directive);
  if(directive==NULL)
    return;
  if(directive->tag == NULL)
    warningdialog("Use the LilyPond window");
  else {
    edit_directive(directive);

  }
}

/**
 * callback for EditStaffDirective 
 */
void edit_staff_directive(GtkAction *action,  DenemoScriptParam *param) {
  //g_print("Edit directive called\n");
  DenemoDirective *directive = select_staff_directive();
  g_print("Got directive %p\n", directive);
  if(directive==NULL)
    return;
  if(directive->tag == NULL)
    warningdialog("Use the LilyPond window");
  else {
    edit_directive(directive);

  }
}

/**
 * callback for EditScoreDirective 
 */
void edit_score_directive(GtkAction *action,  DenemoScriptParam *param) {
  DenemoDirective *directive = select_score_directive();
  if(directive==NULL)
    return;
  if(directive->tag == NULL)
    warningdialog("Use the LilyPond window");
  else {
    edit_directive(directive);

  }
}

