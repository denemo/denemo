#ifndef __KEYSIG_H__
#define __KEYSIG_H__

#include <denemo/denemo.h>

void keysig_new_cmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_gmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_dmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_amaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_emaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_bmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_fsharpmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_csharpmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_fmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_bflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_eflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_aflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_dflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_gflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_new_cflatmaj (GtkAction* action, DenemoScriptParam *param);

void keysig_new_amin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_emin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_bmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_fsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_csharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_gsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_dsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_asharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_dmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_gmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_cmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_fmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_bflatmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_eflatmin (GtkAction* action, DenemoScriptParam *param);
void keysig_new_aflatmin (GtkAction* action, DenemoScriptParam *param);

void keysig_set_cmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_gmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_dmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_amaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_emaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_bmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_fsharpmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_csharpmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_fmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_bflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_eflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_aflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_dflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_gflatmaj (GtkAction* action, DenemoScriptParam *param);
void keysig_set_cflatmaj (GtkAction* action, DenemoScriptParam *param);

void keysig_set_amin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_emin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_bmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_fsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_csharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_gsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_dsharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_asharpmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_dmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_gmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_cmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_fmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_bflatmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_eflatmin (GtkAction* action, DenemoScriptParam *param);
void keysig_set_aflatmin (GtkAction* action, DenemoScriptParam *param);

#endif