/* midirecord.h

 * (c) 2021 Richard Shann
 */
#ifndef MIDIRECORD_H
#define MIDIRECORD_H

void new_midi_recording (void);
void delete_last_recorded_note (void);
void record_midi (gchar * buf);
void delete_recording (void);
//gdouble get_recording_start_time (void);
void play_recorded_midi (void);
void pause_recording_midi (void);
gboolean toggle_midi_record (void);
void advance_marked_midi (gint steps);
void synchronize_recording (void);
void scale_recording (gdouble scale);
gboolean midi_track_present (void);
#endif //MIDIRECORD_H
