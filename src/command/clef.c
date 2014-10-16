#include "command/clef.h"

/**
 * Create a new clef object
 * @param type clef type to create 
 *
 * @return the clef 
 */
DenemoObject *
clef_new (enum clefs type)
{
  DenemoStaff *thestaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  gboolean invisible = (thestaff->voicecontrol & DENEMO_SECONDARY);
  DenemoObject *ret;
  clef *newclef = (clef *) g_malloc (sizeof (clef));
  ret = (DenemoObject *) g_malloc (sizeof (DenemoObject));
  ret->type = CLEF;
  ret->isinvisible = invisible;
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
clef_new_treble (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_TREBLE_CLEF));
}

/**
 * Wrapper function to create new bass clef and insert into the score
 */
void
clef_new_bass (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_BASS_CLEF));
}

/**
 * Wrapper function to create new alto clef and insert into the score
 */
void
clef_new_alto (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_ALTO_CLEF));
}

/**
 * Wrapper function to create new treble_8 clef and insert into the score
 */
void
clef_new_g8 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_G_8_CLEF));
}

/**
 * Wrapper function to create new bass_8 clef and insert into the score
 */
void
clef_new_f8 (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_F_8_CLEF));
}

/**
 * Wrapper function to create new tenor clef and insert into the score
 */
void
clef_new_tenor (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_TENOR_CLEF));
}

/**
 * Wrapper function to create new soprano clef and insert into the score
 */
void
clef_new_soprano (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_SOPRANO_CLEF));
}

/**
 * Wrapper function to create new french clef and insert into the score
 */
void
clef_new_french (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, clef_new (DENEMO_FRENCH_CLEF));
}


void
clef_set_treble (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_TREBLE_CLEF);
}

void
clef_set_bass (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_BASS_CLEF);
}

void
clef_set_g8 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_G_8_CLEF);
}

void
clef_set_f8 (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_F_8_CLEF);
}

void
clef_set_alto (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_ALTO_CLEF);
}

void
clef_set_tenor (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_TENOR_CLEF);
}

void
clef_set_soprano (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_SOPRANO_CLEF);
}

void
clef_set_french (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  if (curstaff)
    dnm_setinitialclef (Denemo.project->movement, curstaff, DENEMO_FRENCH_CLEF);
}
