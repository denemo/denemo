/* 
 * external.c
 * 
 * functions that deals with external commmands
 * such as csound, play, playmidi, lpr...
 *
 * this is part of the GNU Denemo, 
 * Copyright (c) 2000-2006 Adam Tee
 */

#include <denemo/denemo.h>
#include "prefops.h"
#include <signal.h>
#include "external.h"


#if 0
/* test it a given temp filename points to an 
 * existing file in the dotdenemo directory.
 */
gboolean
exists_temp_filename (const GString * name)
{
  GString *path;

  path = g_string_new (locatedotdenemo ());
  g_string_append (path, "/");
  g_string_append (path, name->str);
  return g_file_test (name->str, G_FILE_TEST_EXISTS);
}

/* parse spaces and tabs as field delimitors, return an argv
 * suitable for use without the need of system()
 * FIXME: let delimitors "as is" if they are inside quotations mark
 */

/* this function is disabled with recent GLib: 
 * use g_shell_parse_argv() instead */

static gchar **
build_argv (gchar * cmd)
{
  gint i = 0;			/* index in cmd */
  gint argi = 0;		/* arg index in vector */
  gint vlen = 0;		/* vector length in args */
  gchar **argv = NULL;

  argv = (gchar **) g_malloc (sizeof (gchar *));
  argv[0] = 0;			/* vlen = 0 */

  if (!cmd)
    return NULL;

  while (cmd[i] != 0)
    {
      gint ci = 0;		/* char index in current arg */
      gint alen = 0;		/* arg length in chars */

      if (argi >= (vlen - 2))	/* ensure to not overflow */
	argv = (gchar **) realloc (argv, (vlen += 32) * sizeof (gchar *));
      argv[argi] = g_malloc (sizeof (gchar));
      argv[argi] = 0;		/* curently, alen=0 */
      while (cmd[i] == ' ' || cmd[i] == '\t' || cmd[i] == '\n')
	i++;			/* crop */
      while (cmd[i] != ' ' && cmd[i] != '\t' && cmd[i] != '\n' && cmd[i] != 0)
	{
	  if (ci >= (alen - 2))	/* ensure to not overflow */
	    argv[argi] =
	      (gchar *) realloc (argv[argi], (alen += 32) * sizeof (gchar));
	  argv[argi][ci] = cmd[i];
	  ci++, i++;
	}
      argv[argi][ci] = 0;	/* close string */
      argi++;
      if (cmd[i])
	i++;
#ifdef DEBUG
      fprintf (stderr, "argv[%d]=#%s#\n", (argi - 1), argv[argi - 1]);
#endif
    }
  argv[argi] = NULL;		/* close args list */
  return argv;
}

/* free memory that was allocated by build_argv */
/* this function is disabled with recent GLib: 
 * use g_strfreev() instead */
static void
free_argv (gchar ** av)
{
  int i = 0;

  if (!av)
    return;
  while (av[i])
    {
      free (av[i++]);
    }
}
#endif

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
  if(locatedotdenemo()==NULL)
    return NULL;
  if (name != NULL)
    path = g_build_filename (locatedotdenemo (), name, NULL);
  else
    path = tempnam (locatedotdenemo (), NULL);

#ifdef DEBUG
  fprintf (stderr, "temp filename: %s\n", path);
#endif
  return path;
}

gchar *
dnm_get_temp_filename (const gchar * name)
{
	return get_temp_filename (name);
}

/* init pid files */
void
ext_init (void)
{
  gchar *pidpath;
  FILE *fp;
  int i;

  for (i=0; ext_pidfiles[i] != NULL; i++)
    {
      pidpath = get_temp_filename (ext_pidfiles[i]);
      if(pidpath==NULL)
	continue;
      fp = fopen (pidpath, "wb+");
#ifdef HAVE_FILE_LOCKS
      flockfile(fp); /* wait Denemo unlock it */
#endif
      fwrite (&GPID_UNREF, sizeof(GPid), 1, fp);
#ifdef HAVE_FILE_LOCKS
      funlockfile(fp);
#endif
      fclose (fp);

      g_free (pidpath);
    }
}

/* invalid pid files */
void
ext_quit (void)
{
  gchar *pidpath;
  FILE *fp;
  GPid pid;
  int i = 0;
  
  while (ext_pidfiles[i] != NULL)
    {
      pidpath = get_temp_filename (ext_pidfiles[i]);
      if ((fp = fopen (pidpath, "rb+")) == NULL)
       {
         g_warning ("ext_quit: Open failed on %s", pidpath);
         perror (pidpath);
         break;
       }
#ifdef HAVE_FILE_LOCKS
      flockfile(fp); /* wait Denemo unlock it */
#endif
      fread (&pid, sizeof(GPid), 1, fp);

      g_debug ("ext_quit: read pid %d\n", pid);

      if (pid != GPID_UNREF)
        kill_process (pid);

      rewind (fp);
      fwrite (&GPID_UNREF, sizeof(GPid), 1, fp);
#ifdef HAVE_FILE_LOCKS
      funlockfile(fp);
#endif
      fclose (fp);

      g_free (pidpath);
      i++;
    }
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
  GError *err = NULL; /* force implicit g_malloc */
  gchar **argv;
  GPid pid = GPID_UNREF;
  gboolean ok;

  if (!cmdline->str)
    return GPID_UNREF;

  /*  argv = build_argv (cmdline->str);*/
  if(!g_shell_parse_argv(cmdline->str, NULL, &argv, &err))
    {
      g_message(_("Could not parse command line: %s\n"),
		err->message);
      g_error_free(err);
      return pid;
    }
  ok = g_spawn_async (NULL,	/* dir */
		      argv, NULL,	/* env */
		      G_SPAWN_SEARCH_PATH,  /* | G_SPAWN_DO_NOT_REAP_CHILD, */
		      NULL,	/* child setup func */
		      NULL,	/* user data passed to setup */
		      &pid,	/* child pid */
		      &err);

  if (!ok)
    {
      g_warning ("error spawning pid %d: %s", pid, err->message);
      g_error_free (err);
    }
  /*  free_argv (argv); */
  g_strfreev(argv);
  return pid;
}
