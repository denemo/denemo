/* staff.h
 * Header file for functions dealing with whole staffs

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller */


#ifndef __STAFFOPS_H__
#define __STAFFOPS_H__
#include <denemo/denemo.h>

enum newstaffcallbackaction
{ INITIAL, FIRST, LAST, ADDFROMLOAD, BEFORE, AFTER, CURRENT,
  NEWVOICE, LYRICSTAFF, FIGURESTAFF, CHORDSTAFF
};

struct newstaffinfotopass
{
  DenemoProject *gui;
  DenemoStaff *staff;
  guint addat;
};

objnode *firstobjnode (measurenode * mnode);

objnode *lastobjnode (measurenode * mnode);

measurenode *firstmeasurenode (staffnode * thestaff);

measurenode *nth_measure_node_in_staff (staffnode * thestaff, gint n);

objnode *firstobjnodeinstaff (staffnode * thestaff);

//void initkeysig (gint *accs, gint number);

void setcurrentprimarystaff (DenemoMovement * si);

void newstaff (DenemoProject * gui, enum newstaffcallbackaction action, DenemoContext context);     /* default context shall be DENEMO_NONE */

void deletestaff (DenemoProject * gui, gboolean interactive);

void beamsandstemdirswholestaff (DenemoStaff * thestaff);

void showwhichaccidentalswholestaff (DenemoStaff * thestaff);

void fixnoteheights (DenemoStaff * thestaff);

void newstaffinitial (GtkAction * action, DenemoScriptParam * param);

void newstaffbefore (GtkAction * action, DenemoScriptParam * param);

void dnm_newstaffafter (GtkAction * action, DenemoScriptParam * param);


void dnm_newstaffvoice (GtkAction * action, DenemoScriptParam * param);

void newstafflast (GtkAction * action, DenemoScriptParam * param);

gboolean signal_structural_change (DenemoProject * gui);

void copy_staff (DenemoStaff * src, DenemoStaff * dest);

#endif /* __STAFFOPS_H__ */
