/* pitchentry.h
 * function prototypes for interface to audio/midi in
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


gint setup_pitch_input (void);
gint stop_pitch_input (void);
void start_pitch_input (void);
gboolean pitch_recognition_system_active (void);
gboolean delete_tone (DenemoScore * si, chord * thechord);
gboolean apply_tones (DenemoScore * si);
void clear_overlay (GtkAction * action, gpointer param);
void store_pitch (double pitch);
double get_pitch (void);
void set_sharper (GtkAction * action, gpointer param);
void set_flatter (GtkAction * action, gpointer param);
gchar *determine_interval (gint bass, gint harmony, gboolean * status);
gchar *sharpest (void);
gchar *flattest (void);

void signal_measure_end (void);
gboolean pitch_entry_active (DenemoGUI * gui);
GtkWidget *get_enharmonic_frame (void);
GtkWidget *get_temperament_combo (void);
void reset_temperament (void);
gchar *get_fents_string (void);
gchar *get_sharpest (void);
gchar *get_flattest (void);
gchar *get_temperament_name (void);
void set_enharmonic_position (gint position);
void notenum2enharmonic (gint notenum, gint * poffset, gint * penshift, gint * poctave);
gchar *get_cents_string (void);
void set_tuning (void);
void adjust_tonal_center (gint * accs);
gint get_enharmonic_position (void);
gboolean check_interval (gint step1, gint enshift1, gint step2, gint enshift2);
gboolean check_midi_intervals (GList * midichord);
#endif //PITCHENTRY_H
