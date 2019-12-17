/* measure.h
 * header file for functions dealing with measures
 *
 * for Denemo, a gtk+ frontent to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller
 */
#ifndef MEASURE_H
#define MEASURE_H

#include <denemo/denemo.h>

measurenode *addmeasures (DenemoMovement * si, gint pos, guint nummeasures, gint all);

void freeobjlist (GList *objs);

measurenode *removemeasures (DenemoMovement * si, guint pos, guint nummeasures, gboolean all);

void calculatebeamsandstemdirs (DenemoMeasure *m);

void showwhichaccidentals (objnode * theobjs);

void forceaccidentals (DenemoObject * theobj);

objnode *measure_first_obj_node (measurenode * mnode);

objnode *measure_last_obj_node (measurenode * mnode);

DenemoMeasure *clone_measure (DenemoMeasure *m);

void free_measure (DenemoMeasure *m);
#endif
