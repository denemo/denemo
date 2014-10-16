#ifndef __CLEF_H__
#define __CLEF_H__

#include <denemo/denemo.h>

void clef_new_treble (GtkAction* action, DenemoScriptParam *param);
void clef_new_bass (GtkAction* action, DenemoScriptParam *param);
void clef_new_g8 (GtkAction* action, DenemoScriptParam *param);
void clef_new_alto (GtkAction* action, DenemoScriptParam *param);
void clef_new_tenor (GtkAction* action, DenemoScriptParam *param);
void clef_new_soprano (GtkAction* action, DenemoScriptParam *param);
void clef_new_french (GtkAction* action, DenemoScriptParam *param);
void clef_new_f8 (GtkAction* action, DenemoScriptParam *param);

void clef_set_treble (GtkAction* action, DenemoScriptParam *param);
void clef_set_bass (GtkAction* action, DenemoScriptParam *param);
void clef_set_g8 (GtkAction* action, DenemoScriptParam *param);
void clef_set_alto (GtkAction* action, DenemoScriptParam *param);
void clef_set_tenor (GtkAction* action, DenemoScriptParam *param);
void clef_set_soprano (GtkAction* action, DenemoScriptParam *param);
void clef_set_french (GtkAction* action, DenemoScriptParam *param);
void clef_set_f8 (GtkAction* action, DenemoScriptParam *param);

#endif