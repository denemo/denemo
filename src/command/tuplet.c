/* tuplet.cpp
 * Set Tuplet options
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee Matthew Hiller
 */

#include <gtk/gtk.h>
#include <stdlib.h>
#include <string.h>
#include "command/tuplet.h"
#include "command/chord.h"
#include "command/contexts.h"
#include <denemo/denemo.h>
#include "command/staff.h"
#include "core/utils.h"
#include "display/draw.h"
#include "command/measure.h"
#include "audio/midi.h"
#include "command/object.h"
#include "command/commandfuncs.h"

DenemoObject *
tuplet_open_new (gint numerator, gint denominator)
{
  DenemoObject *tuplet;
  tupopen *newtup = (tupopen *) g_malloc (sizeof (tupopen));
  tuplet = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  tuplet->type = TUPOPEN;
  newtup->numerator = numerator;
  newtup->denominator = denominator;

  tuplet->object = newtup;
  set_basic_numticks (tuplet);
  setpixelmin (tuplet);
  return tuplet;
}

DenemoObject *
tuplet_close_new ()
{
  DenemoObject *tuplet;
  tupopen *newtup = (tupopen *) g_malloc (sizeof (tupopen));    //avoids a null object
  tuplet = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  tuplet->type = TUPCLOSE;
  tuplet->object = newtup;      //avoids a null object
  set_basic_numticks (tuplet);
  setpixelmin (tuplet);
  return tuplet;
}

void
duplet_insert (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, DUPLET);
}

void
triplet_insert (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, TRIPLET);
}

void
triplet_start (DenemoAction* action, DenemoScriptParam *param)
{
  insertion_point (Denemo.project->movement);
  object_insert (Denemo.project, tuplet_open_new (2, 3));
}

void
tuplet_end (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, tuplet_close_new ());
}

void
insert_quadtuplet (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, QUADTUPLET);
}

void
quintuplet_insert (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, QUINTUPLET);
}

void
sextuplet_insert (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, SEXTUPLET);
}

void
septuplet_insert (DenemoAction* action, DenemoScriptParam *param)
{
  dnm_inserttuplet (Denemo.project, SEPTUPLET);
}
