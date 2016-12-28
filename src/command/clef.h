#ifndef __CLEF_H__
#define __CLEF_H__

#include <denemo/denemo.h>

void clef_new_treble (DenemoAction* action, DenemoScriptParam *param);
void clef_new_bass (DenemoAction* action, DenemoScriptParam *param);
void clef_new_g8 (DenemoAction* action, DenemoScriptParam *param);
void clef_new_alto (DenemoAction* action, DenemoScriptParam *param);
void clef_new_tenor (DenemoAction* action, DenemoScriptParam *param);
void clef_new_soprano (DenemoAction* action, DenemoScriptParam *param);
void clef_new_french (DenemoAction* action, DenemoScriptParam *param);
void clef_new_f8 (DenemoAction* action, DenemoScriptParam *param);

void clef_set_treble (DenemoAction* action, DenemoScriptParam *param);
void clef_set_bass (DenemoAction* action, DenemoScriptParam *param);
void clef_set_g8 (DenemoAction* action, DenemoScriptParam *param);
void clef_set_alto (DenemoAction* action, DenemoScriptParam *param);
void clef_set_tenor (DenemoAction* action, DenemoScriptParam *param);
void clef_set_soprano (DenemoAction* action, DenemoScriptParam *param);
void clef_set_french (DenemoAction* action, DenemoScriptParam *param);
void clef_set_f8 (DenemoAction* action, DenemoScriptParam *param);

#endif
