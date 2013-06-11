/* denemo.h
 *
 * the important data structures and defines for denemo, a gtk+ frontend to
 * Lilypond, the GNU music typesetter
 *
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 *
 */

#ifndef DENEMO_DATASTRUCTURES
#define DENEMO_DATASTRUCTURES
#ifdef __cplusplus
extern "C" {
#endif

/* Include the internationalization stuff.  */
#include <libintl.h>
#include <locale.h>

#define _(String) gettext (String)
#ifndef gettext_noop
# define gettext_noop(String) String
#endif
#define N_(String) gettext_noop (String)


#include <glib.h>
#include <gmodule.h>

#include "denemo_api.h"

#define LILYPOND_VERSION "2.8.7" /* version of lilypond that we output */

/*Set maximum number of undos*/
#define MAX_UNDOS 50

/* This unit of memory size is used all over the place.  */
#define SEVENGINTS (sizeof (gint) * 7)

#define CHECKING_MASK (GDK_CONTROL_MASK) 
#define ADDING_MASK (GDK_MOD1_MASK)  //Alt
#define CHORD_MASK (1<<25)  //Not used by GDK

#define DENEMO_INITIAL_MEASURE_WIDTH 160
#define DENEMO_INITIAL_STAFF_HEIGHT 100

#define DENEMO_FILE_SUFFIX  ".denemo"
#define XML_EXT             ".xml"
#define SCM_EXT             ".scm"
#define LILYPOND_EXT        ".ly"

#define DENEMO_NO_COMMAND (-1) /* command id for no command */

#define g_malloc(a) g_malloc0(a) /* for extensible debuggable code */
#if 0
  //use this to detect bad access to G_OBJECT
#define g_object_set_data(a,b,c) (G_IS_OBJECT((a))? g_object_set_data((a),(b),(c)):((gpointer)fprintf(stderr,"Bad G_OBJECT at %s line %d\n",__FILE__, __LINE__), NULL))
#define g_object_get_data(a,b) (G_IS_OBJECT(a)? g_object_get_data((a),(b)):((gpointer)fprintf(stderr,"Bad G_OBJECT at %s line %d\n",__FILE__, __LINE__), NULL))
#define gtk_action_get_name(a) (GTK_IS_ACTION(a)? gtk_action_get_name(a):((gpointer)fprintf(stderr,"Bad GTK_ACTION at %s line %d\n",__FILE__, __LINE__), NULL))


#endif
#if 0
  // use these to test for bad frees.
#define g_free
#define g_list_free
#define g_error_free
#define gtk_tree_path_free
#define g_string_free
#endif

#define DEFAULT_COMMANDS "Default.commands"
#define DEFAULT_KEYBINDINGS "Default.shortcuts"
#ifdef G_OS_WIN32
#define PREFS_FILE "denemorcV2"
#else
#define PREFS_FILE "denemorc"
#endif
  
#ifdef G_OS_WIN32
#define mswin g_print
#else
#define mswin
#endif

extern const gchar *None;
extern const gchar *Jack;
extern const gchar *Portaudio;
extern const gchar *Fluidsynth;

#ifdef __cplusplus
}
#endif

#endif  /* #ifndef DENEMO_DATASTRUCTURES  */
