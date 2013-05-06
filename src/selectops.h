/* selectops.h
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

void saveselection (DenemoScore * si);

void copytobuffer (DenemoScore * si);

void delete_selection (void);


gboolean mark_status (void);

void set_mark (DenemoGUI * gui);

void unset_mark (DenemoGUI * gui);
void set_point (DenemoGUI * gui);

void copywrapper (GtkAction * action, DenemoScriptParam * param);

void cutwrapper (GtkAction * action, DenemoScriptParam * param);

void pastewrapper (GtkAction * action, DenemoScriptParam * param);



void calcmarkboundaries (DenemoScore * si);

void saveselwrapper (GtkAction * action, DenemoScriptParam * param);
void undowrapper (GtkAction * action, gpointer param);
void redowrapper (GtkAction * action, gpointer param);
void update_undo_info (DenemoScore * si, DenemoUndoData * undo);
void update_redo_info (DenemoScore * si, DenemoUndoData * redo);
void store_for_undo_change (DenemoScore * si, DenemoObject * obj);
gboolean take_snapshot (void);
void stage_undo (DenemoScore * si, action_type type);

void goto_mark (GtkAction * action, DenemoScriptParam * param);
void goto_selection_start (GtkAction * action, DenemoScriptParam * param);

DenemoPosition *pop_position (void);
void push_position (void);
void get_position (DenemoScore * si, DenemoPosition * pos);

DenemoObject *get_mark_object (void);
DenemoObject *get_point_object (void);
void save_selection (DenemoScore * si);
void restore_selection (DenemoScore * si);
gboolean in_selection (DenemoScore * si);

void swap_point_and_mark (GtkAction * action, gpointer param);

GList *pop_off_clipboard (void);

gboolean pop_clipboard (void);

void push_clipboard (void);

gint get_staffs_in_clipboard (void);

gchar *get_last_change (DenemoScore * si);

#endif
