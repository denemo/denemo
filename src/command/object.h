/* object.h
 * header file for operations to objects
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller
 */
#ifndef OBJOPS_H
#define OBJOPS_H

#include <denemo/denemo.h>

DenemoObject *get_object (void);

gchar *compare_objects  (GList *curmeasure1, GList *curobj1, gint *pmeasurenum1, gint *pobjnum1, GList *curmeasure2, GList *curobj2, gint *pmeasurenum2, gint *pobjnum2);

void freeobject (DenemoObject * mudobj);

void display_current_object (void);

void update_object_info (void);

void edit_object (void);
void edit_score_properties (void);
void edit_movement_properties (void);

void edit_staff_properties (void);

void edit_voice_properties (void);

void set_modeaccs (gint * accs, gint number, gint mode);

void initkeyaccs (gint * accs, gint number);

//void dnm_setinitialkeysig (DenemoStaff * curstaff, gint tokey, gint type);

DenemoObject *newmeasurebreakobject ();

DenemoObject *newstaffbreakobject ();

DenemoObject *lily_directive_new (gchar * type);

DenemoObject *directive_object_new (DenemoDirective * directive);

DenemoObject *dynamic_new (gchar * type);

DenemoDirective *clone_directive (DenemoDirective * directive);

GList *clone_directives (GList * directives);

void free_directives (GList * directives);

void free_directive (DenemoDirective * directive);

void free_directive_data (DenemoDirective * directive);
#endif
