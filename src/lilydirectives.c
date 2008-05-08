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
  GtkWidget *string;
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
  note *curnote;
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  gchar *directivestring = (gchar *) cbdata->string;
  if (curObj && curObj->type == LILYDIRECTIVE)
    ((lilydirective *) curObj->object)->directive = g_string_new(directivestring);//FIXME memory leak of old directive
  else
    if((curnote = findnote(curObj, gui->si->cursor_y)) != NULL) {
      curnote->directive = g_string_new(directivestring);//FIXME memory leak of old directive
      score_status(gui, TRUE);
    }
    else    
      object_insert (gui, lily_directive_new (directivestring)), displayhelper(gui);
}

/**
 * Lilypond directive.  Allows user to insert a lilypond directive 
 * to the score at the current cursor position
 */
void
lily_directive (GtkAction * action, DenemoGUI *gui)
{
  gchar *string;
  gchar *PreValue = NULL;
  DenemoScore * si = gui->si;
  static struct callbackdata cbdata;
  
  DenemoObject *curObj = (DenemoObject *) si->currentobject ?
    (DenemoObject *) si->currentobject->data : NULL;
  
  if (curObj && curObj->type == LILYDIRECTIVE && ((lilydirective *) curObj->object)->directive)
	{
		PreValue = ((GString *) ((lilydirective *) curObj->object)->directive)->str;
	}
  note *curnote = findnote(curObj, gui->si->cursor_y);
  
  if(curnote && curnote->directive)
   	PreValue = curnote->directive->str;

  string = string_dialog_entry(gui, "Insert LilyDirective", "Insert Lilydirective followed by Enter key", PreValue);
  
  cbdata.gui = gui;
  cbdata.string = string;

  if (string){ 
    insertdirective (NULL, &cbdata);
    displayhelper (gui);
  }

  g_free(string);

}
