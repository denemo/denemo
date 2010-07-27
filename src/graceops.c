/* graceops.cpp
 *
 * functions which manipulate grace notes
 * For denemo, a gtk+ frontend to Lilypond, the GNU music typesetter
 *
 * (c) 2000, 2001, 2002 Adam Tee 
 *
 */
#include "graceops.h"
#include <denemo/denemo.h>
#include <string.h>
#include "utils.h"


/**
 * Create a new grace start object
 *
 */
DenemoObject *
newgracestart ()
{
  DenemoObject *thegrace;
  grace *newgrace = (grace *) g_malloc (sizeof (grace));
  thegrace = (DenemoObject *) g_malloc (sizeof (DenemoObject));

  thegrace->type = GRACE_START;
  newgrace->on_beat = FALSE;
  thegrace->object = newgrace;
  set_basic_numticks (thegrace);
  setpixelmin (thegrace);

  return thegrace;
}

/**
 * Create a new grace end object
 *
 */
DenemoObject *
newgraceend ()
{
  DenemoObject *thegrace;

  thegrace = (DenemoObject *) g_malloc (sizeof (DenemoObject));

  thegrace->type = GRACE_END;


  set_basic_numticks (thegrace);
  setpixelmin (thegrace);

  return thegrace;
}

void
toggle_grace (GtkAction *action, DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  GET_1PARAM(action, param, grace);
 DenemoObject *curmudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);
 if(curmudelaobj && (curmudelaobj->type==CHORD)) {
   if(query)
     param->status =  ((chord *)curmudelaobj->object)->is_grace, g_string_assign(param->string, "gracenote");
   else
     ((chord *)curmudelaobj->object)->is_grace ^= GRACED_NOTE;
   //g_print("now %x\n",  ((chord *)curmudelaobj->object)->is_grace);
 }
}
