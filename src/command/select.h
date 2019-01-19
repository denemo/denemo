/* select.h
 * Undoing, selecting, cutting, copying, and pasting music
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller 2010 Richard Shann */

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

DenemoObjType get_clip_obj_type (gint staff, gint object);
gboolean insert_clip_obj (gint staff, gint object);

/* clear the Primary Denemo Clipboard */
void clearbuffer (void);

/* destroy the passed clipboard */
void free_clipboard (GList * clipboard);

void saveselection (DenemoMovement * si);

void copytobuffer (DenemoMovement * si);

void delete_selection (void);


gboolean mark_status (void);

void set_mark (DenemoAction * action, DenemoScriptParam * param);

void unset_mark (DenemoAction * action, DenemoScriptParam * param);
void set_point (DenemoAction * action, DenemoScriptParam * param);

void copywrapper (DenemoAction * action, DenemoScriptParam * param);

void cutwrapper (DenemoAction * action, DenemoScriptParam * param);

void pastewrapper (DenemoAction * action, DenemoScriptParam * param);



void calcmarkboundaries (DenemoMovement * si);

void saveselwrapper (DenemoAction * action, DenemoScriptParam * param);
void undowrapper (DenemoAction * action, DenemoScriptParam * param);
void redowrapper (DenemoAction * action, DenemoScriptParam * param);
void update_undo_info (DenemoMovement * si, DenemoUndoData * undo);
void update_redo_info (DenemoMovement * si, DenemoUndoData * redo);
void store_for_undo_change (DenemoMovement * si, DenemoObject * obj);
gboolean take_snapshot (void);
void stage_undo (DenemoMovement * si, action_type type);

void goto_mark (DenemoAction * action, DenemoScriptParam * param);
void goto_selection_start (DenemoAction * action, DenemoScriptParam * param);

DenemoPosition *pop_position (void);
void push_position (void);
void get_position (DenemoMovement * si, DenemoPosition * pos);

DenemoObject *get_mark_object (void);
DenemoObject *get_point_object (void);
void save_selection (DenemoMovement * si);
void restore_selection (DenemoMovement * si);
gboolean in_selection (DenemoMovement * si);

void swap_point_and_mark (DenemoAction * action, DenemoScriptParam * param);

GList *pop_off_clipboard (void);

gboolean pop_clipboard (gint count);

void push_clipboard (void);

gint get_staffs_in_clipboard (void);

gchar *get_last_change (DenemoMovement * si);

void insert_clipboard (GList * clipboard);
gint get_clip_objs (gint m);
void store_for_undo_measure_create (DenemoMovement * si, gint staffnum, gint measurenum);
#endif
