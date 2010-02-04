/* selectops.h
 * selecting, cutting, copying, and pasting music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */

#include <denemo/denemo.h>

#ifndef DENEMO_SELECTOPS
#define DENEMO_SELECTOPS

enum drag_selection_type
{
  NO_DRAG,
  NORMAL_SELECT,
  WHOLE_MEASURES,
  WHOLE_STAFFS
};
 
DenemoObjType  get_clip_obj_type(gint staff, gint object);
gboolean insert_clip_obj(gint staff, gint object);
void clearbuffer ();

void saveselection(DenemoScore *si);

void copytobuffer (DenemoScore *si);

void cuttobuffer (DenemoScore *si);



void
set_mark (DenemoGUI *gui);

void
unset_mark (DenemoGUI *gui);

void
copywrapper (GtkAction *action, DenemoScriptParam *param);

void
cutwrapper (GtkAction *action, DenemoScriptParam *param);

void
pastewrapper (GtkAction *action, DenemoScriptParam *param);

void
mark_boundaries_helper (DenemoScore *si, gint mark_staff,
			gint mark_measure, gint mark_object, gint point_staff,
			gint point_measure, gint point_object,
			enum drag_selection_type type);

void
calcmarkboundaries (DenemoScore *si);

void
saveselwrapper(GtkAction *action, DenemoScriptParam *param);
void undowrapper(GtkAction *action, gpointer param);
void redowrapper(GtkAction *action, gpointer param);
void update_undo_info(DenemoScore *si, unre_data *undo);
void update_redo_info(DenemoScore *si, unre_data *redo);
void goto_mark (GtkAction *action,  DenemoScriptParam *param);
void goto_selection_start (GtkAction *action,  DenemoScriptParam *param);

DenemoPosition *pop_position(void);
void push_position(void);


DenemoObject *get_mark_object(void);
DenemoObject *get_point_object(void);
void save_selection(DenemoScore *si);
void restore_selection(DenemoScore *si);
gboolean in_selection(DenemoScore *si);

void
swap_point_and_mark(GtkAction *action, gpointer param);

#endif
