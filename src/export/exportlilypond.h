/* exportlilypond.h
 * Header file for lilypond generation

 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000-2005 Matthew Hiller, Adam Tee, 2011 Richard Shann
 */
#ifndef EXPORTMUDELAH
#define EXPORTMUDELAH

#include <gtk/gtk.h>
#include <stdio.h>

//real TAB causes mismatch between LilyPond error columns and Gtks
#define TAB "        "

#define LILYPOND_SYMBOL_DEFINITIONS \
    "\nCompactChordSymbols = {}\n#(define DenemoTransposeStep 0)\n#(define DenemoTransposeAccidental 0)\nDenemoGlobalTranspose = \\void {}\ntitledPiece = {}\nAutoBarline = {}\nAutoEndMovementBarline = \\bar \"|.\"\n"
void create_lilywindow (void);
void exportlilypond (gchar * thefilename, DenemoProject * gui, gboolean all_movements);

void export_lilypond_parts (char *filename, DenemoProject * gui);
void export_lilypond_part (char *filename, DenemoProject * gui, gboolean all_movements);

/* generate the LilyPond for the current part, all movements, into the LilyPond textview window */
void generate_lilypond_part (void);
void make_scoreblock_editable (void);
/* generate lilypond text for the object passed in - the string should
   be g_freed by the caller when finished with*/
gchar *generate_lily (objnode * obj);
void refresh_lily_cb (DenemoAction * action, DenemoProject * gui);
void force_lily_refresh (DenemoProject * gui);
void toggle_lily_visible_cb (DenemoAction * action, gpointer param);

void custom_lily_cb (DenemoAction * action, gpointer param);

void delete_lily_cb (DenemoAction * action, gpointer param);
void set_lily_error (gint line, gint column);
void highlight_lily_error ();
gboolean goto_lilypond_position (gint line, gint column);
DenemoObject *get_object_at_lilypond (gint line, gint col);

void set_initiate_scoreblock (DenemoMovement * si, GString * scoreblock);
gchar *get_lilypond_for_clef (clef * theclef);
gchar *get_lilypond_for_keysig (struct keysig *key);
gchar *get_lilypond_for_timesig (timesig * time);
gchar *get_lilypond_paper (void);
const gchar *get_prevailing_clef_as_lilypond (void);
const gchar *get_prevailing_keysig_as_lilypond (void);
const gchar *get_prevailing_timesig_as_lilypond (void);
void set_voice_termination (GString * str, DenemoStaff * curstaffstruct);
void set_staff_termination (GString * str, DenemoStaff * curstaffstruct);
void set_staff_finalize (GString * str, DenemoStaff * curstaffstruct);
void set_voice_definition (GString * str, DenemoStaff * curstaffstruct, gchar * denemo_name);
void set_staff_definition (GString * str, DenemoStaff * curstaffstruct);
gint get_cursor_offset (void);
void init_lilypond_buffer (void);
#endif
