/* tupletops.h
 * 
 * tuplet function prototypes
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Adam Tee, Matthew Hiller
 */

#include <denemo/denemo.h>

DenemoObject *tuplet_open_new (gint numerator, gint denominator);
DenemoObject *tuplet_close_new ();

void duplet_insert (GtkAction* action, DenemoScriptParam *param);
void triplet_insert (GtkAction* action, DenemoScriptParam *param);
void triplet_start (GtkAction* action, DenemoScriptParam *param);
void tuplet_end (GtkAction* action, DenemoScriptParam *param);
void insert_quadtuplet (GtkAction* action, DenemoScriptParam *param);
void quintuplet_insert (GtkAction* action, DenemoScriptParam *param);
void sextuplet_insert (GtkAction* action, DenemoScriptParam *param);
void septuplet_insert (GtkAction* action, DenemoScriptParam *param);
