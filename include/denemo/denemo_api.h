/*
 * denemo_api.h
 *
 * Contains public functions available in libdenemo
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */

#ifndef DENEMO_APIH
#define DENEMO_APIH

#include "denemo_types.h"

void dnm_insertnote (DenemoProject  *gui, gint duration, input_mode mode,
              gboolean rest);
void dnm_insertmeasures (DenemoMovement  *si, gint number);
void dnm_deleteobject (DenemoMovement *si);
void dnm_deletemeasure (DenemoMovement *si);


void dnm_addornament(DenemoObject *obj, Ornament orn);
void
dnm_setinitialclef(DenemoMovement *si, DenemoStaff *curstaffstruct,
           enum clefs clef);
void
dnm_setinitialtimesig (DenemoMovement *si, DenemoStaff * curstaffstruct,
               gint time1,gint time2, gboolean all_staves);
void
dnm_insert_timesig (DenemoMovement * si, DenemoStaff * curstaffstruct, gint time1, gint time2);

void dnm_setinitialkeysig(DenemoStaff *curstaff, gint tokey, gint type);
void dnm_set_denemo_name (GString *lily_name, GString *denemo_name);




void dnm_addtone (DenemoObject * thechord, gint mid_c_offset, gint enshift);
DenemoObject * dnm_newchord (gint baseduration, gint numdots, int tied);
measurenode * dnm_addmeasures (DenemoMovement * si, gint pos, guint nummeasures, gint all);
/* create one DenemoObject of type not known to denemo otherwise
*  no object is associated. */

void dnm_chooseorcfile (GtkWidget * widget, struct cs_callback *data);

gchar *
dnm_get_temp_filename (const gchar * name);


DenemoObject *
dnm_newobj(DenemoObjType type);

DenemoObject *
dnm_lyric_new(gchar *type, gint position, gboolean syllable);

DenemoObject *
dnm_clone_object (DenemoObject * orig);

DenemoObject *
dnm_stem_directive_new (enum stemdirections type);


DenemoObject *
dnm_newkeyobj (gint number, gint isminor, gint mode); /* default mode: 0 */

DenemoObject *
dnm_newtimesigobj (gint time1, gint time2);

DenemoObject *
clef_new (enum clefs type);

#endif //DENEMO_APIH
