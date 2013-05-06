#ifndef EXTERNAL_H
#define EXTERNAL_H

void ext_init (void);
void ext_quit (void);
/*gboolean exists_temp_filename (const gchar* name);*/
gchar *get_temp_filename (const gchar * name);
GPid spawn_external (const GString * cmdline);

/* GPID_UNREF contains the default value for an unreferenced GPid*/
#ifdef G_OS_WIN32
static const GPid GPID_UNREF = NULL;
#define GPID_UNREF_VALUE (NULL)
#else
static const GPid GPID_UNREF = -1;
#define GPID_UNREF_VALUE (-1)
#endif

/* define your externals here:
 * these are indexes of ext_pifiles.
 * functions ext_* cand reach desired pid filename
 * with gchar* filename = ext_pifiles[EXT_MYAPP];
 */
//#define EXT_MIDI 0
//#define EXT_CSOUND 1

#endif
