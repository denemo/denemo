#ifndef __KEYSIG_H__
#define __KEYSIG_H__

#include <denemo/denemo.h>

void keysig_new_maj(gint number);
void keysig_new_cmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_gmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_dmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_amaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_emaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_bmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_fsharpmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_csharpmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_fmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_bflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_eflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_aflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_dflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_gflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_cflatmaj (DenemoAction* action, DenemoScriptParam *param);

void keysig_new_min(gint number);
void keysig_new_amin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_emin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_bmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_fsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_csharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_gsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_dsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_asharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_dmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_gmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_cmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_fmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_bflatmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_eflatmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_new_aflatmin (DenemoAction* action, DenemoScriptParam *param);

void keysig_set_maj(gint number);
void keysig_set_cmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_gmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_dmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_amaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_emaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_bmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_fsharpmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_csharpmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_fmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_bflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_eflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_aflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_dflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_gflatmaj (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_cflatmaj (DenemoAction* action, DenemoScriptParam *param);

void keysig_set_min(gint number);
void keysig_set_amin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_emin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_bmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_fsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_csharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_gsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_dsharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_asharpmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_dmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_gmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_cmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_fmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_bflatmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_eflatmin (DenemoAction* action, DenemoScriptParam *param);
void keysig_set_aflatmin (DenemoAction* action, DenemoScriptParam *param);

#endif
