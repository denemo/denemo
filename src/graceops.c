/* graceops.cpp
 *
 * functions which manipulate grace notes
 * For denemo, a gtk+ frontend to Lilypond, the GNU music typesetter
 *
 * (c) 2000, 2001, 2002 Adam Tee 
 *
 */
#include "graceops.h"
#include "selectops.h"
#include "commandfuncs.h"
#include <denemo/denemo.h>
#include <string.h>
#include "utils.h"



void
toggle_grace (GtkAction * action, DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  GET_1PARAM (action, param, grace);
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  if (curmudelaobj && (curmudelaobj->type == CHORD))
    {
      if (query)
        param->status = ((chord *) curmudelaobj->object)->is_grace, g_string_assign (param->string, "gracenote");
      else
        {
          store_for_undo_change (si, curmudelaobj);
          ((chord *) curmudelaobj->object)->is_grace ^= GRACED_NOTE;
          displayhelper (Denemo.gui);
        }
      //g_print("now %x\n",  ((chord *)curmudelaobj->object)->is_grace);
    }
}

void
toggle_acciaccatura (GtkAction * action, DenemoScriptParam * param)
{
  DenemoGUI *gui = Denemo.gui;
  DenemoScore *si = gui->si;
  GET_1PARAM (action, param, grace);
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->si->currentobject ? gui->si->currentobject->data : NULL);
  if (curmudelaobj && (curmudelaobj->type == CHORD))
    {
      if (query)
        param->status = ((chord *) curmudelaobj->object)->is_grace, g_string_assign (param->string, "acciaccatura");
      else
        {
          store_for_undo_change (si, curmudelaobj);
          ((chord *) curmudelaobj->object)->is_grace ^= ACCIACCATURA;
          displayhelper (Denemo.gui);
        }
      //g_print("now %x\n",  ((chord *)curmudelaobj->object)->is_grace);
    }
}
