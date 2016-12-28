#include "command/clef.h"
#include "command/commandfuncs.h"
#include "core/utils.h"

/**
 * Create a new clef object
 * @param type clef type to create
 *
 * @return the clef
 */
DenemoObject *
clef_new (enum clefs type)
{
  DenemoObject *ret;
  clef *newclef = (clef *) g_malloc (sizeof (clef));
  ret = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  ret->type = CLEF;
  newclef->type = type;
  ret->object = newclef;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

/**
 * Wrapper function to create new treble clef and insert into the score
 */
void
clef_new_treble (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_TREBLE_CLEF));
}

/**
 * Wrapper function to create new bass clef and insert into the score
 */
void
clef_new_bass (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_BASS_CLEF));
}

/**
 * Wrapper function to create new alto clef and insert into the score
 */
void
clef_new_alto (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_ALTO_CLEF));
}

/**
 * Wrapper function to create new treble_8 clef and insert into the score
 */
void
clef_new_g8 (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_G_8_CLEF));
}

/**
 * Wrapper function to create new bass_8 clef and insert into the score
 */
void
clef_new_f8 (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_F_8_CLEF));
}

/**
 * Wrapper function to create new tenor clef and insert into the score
 */
void
clef_new_tenor (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_TENOR_CLEF));
}

/**
 * Wrapper function to create new soprano clef and insert into the score
 */
void
clef_new_soprano (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_SOPRANO_CLEF));
}

/**
 * Wrapper function to create new french clef and insert into the score
 */
void
clef_new_french (DenemoAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_FRENCH_CLEF));
}


void
clef_set_treble (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_TREBLE_CLEF);
}

void
clef_set_bass (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_BASS_CLEF);
}

void
clef_set_g8 (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_G_8_CLEF);
}

void
clef_set_f8 (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_F_8_CLEF);
}

void
clef_set_alto (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_ALTO_CLEF);
}

void
clef_set_tenor (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_TENOR_CLEF);
}

void
clef_set_soprano (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_SOPRANO_CLEF);
}

void
clef_set_french (DenemoAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_FRENCH_CLEF);
}
