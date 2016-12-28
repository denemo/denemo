#include "keysig.h"
#include "command/commandfuncs.h"
#include "command/object.h"
#include "core/utils.h"

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
keysig_new_maj(gint number){
  object_insert (Denemo.project, dnm_newkeyobj (number, 1, 0));
}

void
keysig_new_cmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(0);
}

void
keysig_new_gmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(1);
}

void
keysig_new_dmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(2);
}

void
keysig_new_amaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(3);
}

void
keysig_new_emaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(4);
}

void
keysig_new_bmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(5);
}

void
keysig_new_fsharpmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(6);
}

void
keysig_new_csharpmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(7);
}

void
keysig_new_fmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-1);
}

void
keysig_new_bflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-2);
}

void
keysig_new_eflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-3);
}

void
keysig_new_aflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-4);
}

void
keysig_new_dflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-5);
}

void
keysig_new_gflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-6);
}

void
keysig_new_cflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_maj(-7);
}

void
keysig_new_min(gint number)
{
  object_insert (Denemo.project, dnm_newkeyobj (number, 0, 0));
}

void
keysig_new_amin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (0);
}

void
keysig_new_emin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (1);
}

void
keysig_new_bmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (2);
}

void
keysig_new_fsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (3);
}

void
keysig_new_csharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (4);
}

void
keysig_new_gsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (5);
}

void
keysig_new_dsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (6);
}

void
keysig_new_asharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (7);
}

void
keysig_new_dmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-1);
}

void
keysig_new_gmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-2);
}

void
keysig_new_cmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-3);
}

void
keysig_new_fmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-4);
}

void
keysig_new_bflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-5);
}

void
keysig_new_eflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-6);
}

void
keysig_new_aflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_new_min (-7);
}


//Functions to set the initial key signature
void
keysig_set_maj(gint number)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, number, 1);
}

void
keysig_set_cmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (0);
}

void
keysig_set_gmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (1);
}

void
keysig_set_dmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (2);
}

void
keysig_set_amaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (3);
}

void
keysig_set_emaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (4);
}

void
keysig_set_bmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (5);
}

void
keysig_set_fsharpmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (6);
}

void
keysig_set_csharpmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (7);
}

void
keysig_set_fmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-1);
}

void
keysig_set_bflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-2);
}

void
keysig_set_eflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-3);
}

void
keysig_set_aflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-4);
}

void
keysig_set_dflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-5);
}

void
keysig_set_gflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-6);
}

void
keysig_set_cflatmaj (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_maj (-7);
}

void
keysig_set_min(gint number)
{
  DenemoStaff *curstaff = (DenemoStaff *) Denemo.project->movement->currentstaff->data;
  dnm_setinitialkeysig (curstaff, number, 0);
}

void
keysig_set_amin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (0);
}

void
keysig_set_emin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (1);
}

void
keysig_set_bmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (2);
}

void
keysig_set_fsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (3);
}

void
keysig_set_csharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (4);
}

void
keysig_set_gsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (5);
}

void
keysig_set_dsharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (6);
}

void
keysig_set_asharpmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (7);
}

void
keysig_set_dmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-1);
}

void
keysig_set_gmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-2);
}

void
keysig_set_cmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-3);
}

void
keysig_set_fmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-4);
}

void
keysig_set_bflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-5);
}

void
keysig_set_eflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-6);
}

void
keysig_set_aflatmin (DenemoAction* action, DenemoScriptParam *param)
{
  keysig_set_min (-7);
}

