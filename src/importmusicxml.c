/* 
 * importmusicxml.c
 *
 * Functions for importing a MusicXML file
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (C)  2010 Richard Shann
 * 
 * License: this file may be used under the FSF GPL version 3 or later
 */
#include <denemo/denemo.h>
#include "prefops.h"            //for locatedotdenemo()
gint
mxmlinput (gchar * filename, DenemoGUI * gui)
{
  GError *err = NULL;
#ifdef G_OS_WIN32
  gchar *rootdir = g_win32_get_package_installation_directory (NULL, NULL);
  gchar *script = g_build_filename (rootdir, "bin", "musicxml2ly.py", NULL);
  gchar *argv[] = {
    "python.exe",
    script,
    "-o", "denemoconvert",
    filename,
    NULL
  };
#else
  gchar *argv[] = {
    "musicxml2ly",
    "-o", "denemoconvert",
    filename,
    NULL
  };
#endif
  g_spawn_sync (locatedotdenemo (),     /* dir */
                argv, NULL,     /* env */
                G_SPAWN_SEARCH_PATH, NULL,      /* child setup func */
                NULL,           /* user data */
                NULL,           /* stdout */
                NULL,           /* stderr */
                NULL, &err);

  if (err)
    {
      g_warning ("Conversion to LilyPond gave message %s\n", err->message);
      return -1;
    }
  gchar *output;
  output = g_build_filename (locatedotdenemo (), "denemoconvert.ly", NULL);
  gboolean ret = lyinput (output, gui);
  g_free (output);
  return ret;
}
