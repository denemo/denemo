/*
 * external.c
 *
 * functions that deals with external commmands
 * such as play, playmidi, lpr...
 *
 * this is part of the GNU Denemo,
 * Copyright (c) 2000-2006 Adam Tee
 */

#include <denemo/denemo.h>
#include "core/prefops.h"
#include <signal.h>
#include "core/external.h"
#include "core/utils.h"


/* give a filepath string pointing to the user's
 * Denemo temporary directory.
 *
 * if constname is NULL, basename is random.
 * else, basename is the given constname.
 *
 * returns: the filepath
 */
gchar *
get_temp_filename (const gchar * name)
{
  gchar *path = NULL;
  if (get_user_data_dir (FALSE) == NULL)
    return NULL;
  if (name != NULL)
    path = g_build_filename (get_user_data_dir (FALSE), name, NULL);
  else
    path = g_build_filename (get_user_data_dir (FALSE), "bla", NULL);

  g_debug ("temp filename: %s\n", path);
  return path;
}

gchar *
dnm_get_temp_filename (const gchar * name)
{
  return get_temp_filename (name);
}


/*
 * spawn an external shell command line with
 * its arguments and returns immediately.
 *
 * additional options can be put in cmdline,
 * separated by spaces or tabs.
 *
 * return: PID of spawned process.
 */
GPid
spawn_external (const GString * cmdline)
{
  GError *err = NULL;           /* force implicit g_malloc */
  gchar **argv;
  GPid pid = GPID_UNREF;
  gboolean ok;

  if (!cmdline->str)
    return GPID_UNREF;

  /*  argv = build_argv (cmdline->str); */
  if (!g_shell_parse_argv (cmdline->str, NULL, &argv, &err))
    {
      g_message (_("Could not parse command line: %s"), err->message);
      g_error_free (err);
      return pid;
    }
  ok = g_spawn_async (NULL,     /* dir */
                      argv, NULL,       /* env */
                      G_SPAWN_SEARCH_PATH,      /* | G_SPAWN_DO_NOT_REAP_CHILD, */
                      NULL,     /* child setup func */
                      NULL,     /* user data passed to setup */
                      &pid,     /* child pid */
                      &err);

  if (!ok)
    {
      g_warning ("error spawning pid %d: %s", pid, err->message);
      g_error_free (err);
    }
  /*  free_argv (argv); */
  g_strfreev (argv);
  return pid;
}
