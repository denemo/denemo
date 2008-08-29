/* pitchentry.h
 * function prototypes for responses to pitchrecognition from audio in
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c)2007 Richard Shann
 */

#ifndef PITCHENTRY_H
#define PITCHENTRY_H

#include <gtk/gtk.h>
#include <denemo/denemo.h>

#include "selectops.h"
#include "chordops.h"
#include "objops.h"


gint setup_pitch_recognition(DenemoGUI *gui);
gint stop_pitch_recognition(void);
void start_pitch_recognition(DenemoGUI *gui);
gboolean pitch_recognition_system_active(void);
gboolean delete_tone(DenemoScore *si, chord *thechord);
void clear_overlay(GtkAction *action, gpointer param);
#endif //PITCHENTRY_H
