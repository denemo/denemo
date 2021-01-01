/* grace.cpp
 *
 * functions which manipulate grace notes
 * For denemo, a gtk+ frontend to Lilypond, the GNU music typesetter
 *
 * (c) 2000, 2001, 2002 Adam Tee
 *
 */
#include "command/grace.h"
#include "command/select.h"
#include "command/commandfuncs.h"
#include <denemo/denemo.h>
#include <string.h>
#include "core/utils.h"



void
toggle_grace (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  GET_1PARAM (action, param, grace);
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->movement->currentobject ? gui->movement->currentobject->data : NULL);
  if (curmudelaobj && (curmudelaobj->type == CHORD))
    {
      if (query)
        param->status = ((chord *) curmudelaobj->object)->is_grace, g_string_assign (param->string, "gracenote");
      else
        {
          store_for_undo_change (si, curmudelaobj);
          ((chord *) curmudelaobj->object)->is_grace ^= GRACED_NOTE;
          displayhelper (Denemo.project);
          score_status (Denemo.project, TRUE);
        }
      //g_debug("now %x\n",  ((chord *)curmudelaobj->object)->is_grace);
    }
}

void
toggle_acciaccatura (DenemoAction * action, DenemoScriptParam * param)
{
  DenemoProject *gui = Denemo.project;
  DenemoMovement *si = gui->movement;
  GET_1PARAM (action, param, grace);
  DenemoObject *curmudelaobj = (DenemoObject *) (gui->movement->currentobject ? gui->movement->currentobject->data : NULL);
  if (curmudelaobj && (curmudelaobj->type == CHORD))
    {
      if (query)
        param->status = ((chord *) curmudelaobj->object)->is_grace, g_string_assign (param->string, "acciaccatura");
      else
        {
          store_for_undo_change (si, curmudelaobj);
          ((chord *) curmudelaobj->object)->is_grace ^= ACCIACCATURA;
          displayhelper (Denemo.project);
          score_status (Denemo.project, TRUE);
        }
      //g_debug("now %x\n",  ((chord *)curmudelaobj->object)->is_grace);
    }
}
