#include "keysig.h"

/**
 * Create a new keysignature object
 * 
 * @param number number of keysignature
 * @param isminor signifies if the key sig should be minor
 * @param mode    description of the keys mode
 * @return the key signature
 */
DenemoObject *
dnm_newkeyobj (gint number, gint isminor, gint mode)
{
  DenemoObject *ret;
  keysig *key_sig = (keysig *) g_malloc (sizeof (keysig));
  ret = (DenemoObject *) g_malloc0 (sizeof (DenemoObject));
  ret->type = KEYSIG;
  ret->isinvisible = FALSE;
  g_debug ("Number %d \t IsMinor %d \t Mode %d\n", number, isminor, mode);

  key_sig->mode = mode;
  key_sig->number = number;
  key_sig->isminor = isminor;

  if (isminor == 2)
    set_modeaccs (key_sig->accs, number, mode);
  else
    initkeyaccs (key_sig->accs, number);

  ret->object = key_sig;
  set_basic_numticks (ret);
  setpixelmin (ret);
  return ret;
}

void
keysig_new_cmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (0, 1, 0));
}

void
keysig_new_gmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (1, 1, 0));
}

void
keysig_new_dmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (2, 1, 0));
}

void
keysig_new_amaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (3, 1, 0));
}

void
keysig_new_emaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (4, 1, 0));
}

void
keysig_new_bmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (5, 1, 0));
}

void
keysig_new_fsharpmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (6, 1, 0));
}

void
keysig_new_csharpmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (7, 1, 0));
}

void
keysig_new_fmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-1, 1, 0));
}

void
keysig_new_bflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-2, 1, 0));
}

void
keysig_new_eflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-3, 1, 0));
}

void
keysig_new_aflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-4, 1, 0));
}

void
keysig_new_dflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-5, 1, 0));
}

void
keysig_new_gflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-6, 1, 0));
}

void
keysig_new_cflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-7, 1, 0));
}

void
keysig_new_amin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (0, 0, 0));
}

void
keysig_new_emin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (1, 0, 0));
}

void
keysig_new_bmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (2, 0, 0));
}

void
keysig_new_fsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (3, 0, 0));
}

void
keysig_new_csharpmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (4, 0, 0));
}

void
keysig_new_gsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (5, 0, 0));
}

void
keysig_new_dsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (6, 0, 0));
}

void
keysig_new_asharpmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (7, 0, 0));
}

void
keysig_new_dmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-1, 0, 0));
}

void
keysig_new_gmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-2, 0, 0));
}

void
keysig_new_cmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-3, 0, 0));
}

void
keysig_new_fmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-4, 0, 0));
}

void
keysig_new_bflatmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-5, 0, 0));
}

void
keysig_new_eflatmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-6, 0, 0));
}

void
keysig_new_aflatmin (GtkAction* action, DenemoScriptParam *param)
{
  object_insert (Denemo.project, dnm_newkeyobj (-7, 0, 0));
}


//Functions to set the initial key signature
void
keysig_set_cmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 1);
  //displayhelper(si);
}

void
keysig_set_gmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 1);
  //displayhelper(si);
}

void
keysig_set_dmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 1);
}

void
keysig_set_amaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 1);
}

void
keysig_set_emaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 1);
}

void
keysig_set_bmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 1);
}

void
keysig_set_fsharpmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 1);
}

void
keysig_set_csharpmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 1);
}

void
keysig_set_fmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 1);
}

void
keysig_set_bflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 1);
}

void
keysig_set_eflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 1);
}

void
keysig_set_aflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 1);
}

void
keysig_set_dflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 1);
}

void
keysig_set_gflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 1);
}

void
keysig_set_cflatmaj (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 1);
}

void
keysig_set_amin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 0, 0);
}

void
keysig_set_emin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 1, 0);
}

void
keysig_set_bmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 2, 0);
}

void
keysig_set_fsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 3, 0);
}

void
keysig_set_csharpmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 4, 0);
}

void
keysig_set_gsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 5, 0);
}

void
keysig_set_dsharpmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 6, 0);
}

void
keysig_set_asharpmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, 7, 0);
}

void
keysig_set_dmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -1, 0);
}

void
keysig_set_gmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -2, 0);
}

void
keysig_set_cmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -3, 0);
}

void
keysig_set_fmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -4, 0);
}

void
keysig_set_bflatmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -5, 0);
}

void
keysig_set_eflatmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -6, 0);
}

void
keysig_set_aflatmin (GtkAction* action, DenemoScriptParam *param)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, -7, 0);
}

