/* staff.h
 * Header file for functions dealing with whole staffs

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */


#ifndef __STAFFOPS_H__
#define __STAFFOPS_H__
#include <denemo/denemo.h>

gboolean signal_structural_change (DenemoProject * project);

enum newstaffcallbackaction
{
  INITIAL,
  FIRST,
  LAST,
  ADDFROMLOAD,
  BEFORE,
  AFTER,
  CURRENT,
  NEWVOICE,
  LYRICSTAFF,
  FIGURESTAFF,
  CHORDSTAFF
};

gchar *difference_of_staffs (DenemoStaff *s1, DenemoStaff *s2);

measurenode *staff_first_measure_node (staffnode * thestaff);

measurenode *staff_nth_measure_node (staffnode * thestaff, gint n);

void staff_set_current_primary (DenemoMovement * movement);

/* default context shall be DENEMO_NONE */
DenemoStaff* staff_new (DenemoProject * gui, enum newstaffcallbackaction action, DenemoContext context);

void staff_delete (DenemoProject * gui, gboolean interactive);

void staff_beams_and_stems_dirs (DenemoStaff * thestaff);

void staff_show_which_accidentals (DenemoStaff * thestaff);

void staff_fix_note_heights (DenemoStaff * thestaff);

void staff_new_initial (DenemoAction * action, DenemoScriptParam * param);

void staff_new_before (DenemoAction * action, DenemoScriptParam * param);

void staff_new_after (DenemoAction * action, DenemoScriptParam * param);

void staff_new_voice (DenemoAction * action, DenemoScriptParam * param);

void staff_new_last (DenemoAction * action, DenemoScriptParam * param);

void staff_copy (DenemoStaff * src, DenemoStaff * dest, gboolean copy_all);

#endif /* __STAFFOPS_H__ */
