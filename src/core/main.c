/* main.c
 * sets up the GUI and connects the main callback functions.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Matthew Hiller, Adam Tee
 */
#ifdef HAVE_CONFIG_H
#include "config.h"
#endif

#ifdef HAVE_GETOPT_H
#include <getopt.h>
#endif

#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>
#include <gtk/gtk.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
#include <wait.h>
#endif
#ifdef HAVE_SYS_RESOURCE_H
#include <sys/resource.h>
#endif

#include "denemo/denemo.h"
#include "core/importxml.h"
#include <sys/types.h>
#include <dirent.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libguile.h>
#include <librsvg/rsvg.h>
#include "core/view.h"
#include "core/exportxml.h"
#include "core/utils.h"
#include "core/keyboard.h"

struct DenemoRoot Denemo;

#ifdef HAVE_SIGCHLD
/* Code by Erik Mouw, taken directly from the gtk+ FAQ */

/**
 * signal handler to be invoked when child processes _exit() without
 * having to wait for them
 */
static void
sigchld_handler (G_GNUC_UNUSED gint num)
{
  sigset_t set, oldset;
  pid_t pid;
  gint status, exitstatus;


  /* block other incoming SIGCHLD signals */
  sigemptyset (&set);
  sigaddset (&set, SIGCHLD);
  sigprocmask (SIG_BLOCK, &set, &oldset);

  /* wait for child */
  while ((pid = waitpid ((pid_t) - 1, &status, WNOHANG)) > 0)
    {
      if (WIFEXITED (status))
        {
          exitstatus = WEXITSTATUS (status);


          fprintf (stderr, _("Parent: child exited, pid = %d, exit status = %d\n"), (int) pid, exitstatus);
        }
      else if (WIFSIGNALED (status))
        {
          exitstatus = WTERMSIG (status);

          fprintf (stderr, _("Parent: child terminated by signal %d, pid = %d\n"), exitstatus, (int) pid);
        }
      else if (WIFSTOPPED (status))
        {
          exitstatus = WSTOPSIG (status);

          fprintf (stderr, _("Parent: child stopped by signal %d, pid = %d\n"), exitstatus, (int) pid);
        }
      else
        {
          fprintf (stderr, _("Parent: child exited magically, pid = %d\n"), (int) pid);
        }
    }

  /* re-install the signal handler (some systems need this) */
  signal (SIGCHLD, sigchld_handler);

  /* and unblock it */
  sigemptyset (&set);
  sigaddset (&set, SIGCHLD);
  sigprocmask (SIG_UNBLOCK, &set, &oldset);
}
#endif /* HAVE_SIGCHLD */

static void
append_to_path (gchar * path, gchar * extra, ...)
{
  va_list ap;
  va_start(ap, extra);

  gchar *path_string = (gchar *) g_getenv (path);
  if (!path_string){
    if(extra){
      path_string = g_strdup (extra);
      extra = va_arg(ap, gchar*);
    }
  }

  while(extra){
    path_string = g_strconcat (path_string, G_SEARCHPATH_SEPARATOR_S, extra, NULL);
    extra = va_arg(ap, gchar*);
  }

  g_setenv (path, path_string, TRUE);
  g_debug ("%s is %s\n", path, path_string);
  va_end(ap);
}

static gchar **
process_command_line (int argc, char **argv, gboolean gtkstatus)
{
  GError *error = NULL;
  GOptionContext *context;
  gchar* scheme_script_name = NULL;
  gboolean version = FALSE;
  gchar **filenames = NULL;

  GOptionEntry entries[] =
  {
    { "scheme-path",         'i', 0, G_OPTION_ARG_FILENAME, &Denemo.scheme_file, _("Process scheme commands in pathtofile on file open"), _("path") },
    { "scheme-script-name",  's', 0, G_OPTION_ARG_STRING, &scheme_script_name, _("Process scheme commands from system file on file open"), _("file")  },
    { "scheme",              'a', 0, G_OPTION_ARG_STRING, &Denemo.scheme_commands, _("Process the scheme on startup"), _("scheme") },
    { "fatal-scheme-errors", 'e', 0, G_OPTION_ARG_NONE, &Denemo.fatal_scheme_errors, _("Abort on scheme errors"), NULL },
    { "silent",              'm', 0, G_OPTION_ARG_NONE, &Denemo.silent, _("Don't log any message"), NULL },
    { "verbose",             'V', 0, G_OPTION_ARG_NONE, &Denemo.verbose, _("Display every messages"), NULL },
    { "non-interactive",     'n', 0, G_OPTION_ARG_NONE, &Denemo.non_interactive, _("Launch Denemo without GUI"), NULL },
    { "version",             'v', 0, G_OPTION_ARG_NONE, &version,  _("Print version information and exit"), NULL },
    { "audio-options",       'A', G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &Denemo.prefs.audio_driver,_("Audio driver options"), _("options") },
    { "midi-options",        'M', G_OPTION_FLAG_HIDDEN, G_OPTION_ARG_NONE, &Denemo.prefs.midi_driver, _("Midi driver options"), _("options") },
    { G_OPTION_REMAINING,    0,   0, G_OPTION_ARG_FILENAME_ARRAY, &filenames, NULL, _("[FILE]...") },
    { NULL }
  };
  const gchar* subtitle = _(" ");
  gchar *header = g_strconcat (_("GNU Denemo version"), " ", VERSION, "\n",
                        _("Denemo is a graphical music notation editor.\n"
                          "It uses GNU Lilypond for music typesetting.\n"
                          "Denemo is part of the GNU project."), NULL);
  const gchar* footer = _("Report bugs to http://www.denemo.org\n"
                          "GNU Denemo, a free and open music notation editor");

  context = g_option_context_new (subtitle);
  g_option_context_set_summary (context, header);
  g_free(header);
  g_option_context_set_description (context, footer);
  g_option_context_add_main_entries (context, entries, GETTEXT_PACKAGE);
  if(gtkstatus)
    g_option_context_add_group (context, gtk_get_option_group (TRUE));
  if (!g_option_context_parse (context, &argc, &argv, &error))
    g_error ("Option parsing failed: %s", error->message);

  if(version)
  {
    gchar *message = g_strconcat (
    _("GNU Denemo version"), " ", VERSION, "\n",
    _("Gtk versions") , " runtime: %u.%u.%u, compiled against: %u.%u.%u, \n",
    _("© 1999-2005, 2009 Matthew Hiller, Adam Tee, and others, 2010-2015 Richard Shann, Jeremiah Benham, Nils Gey and others.\n"),
    _("This program is provided with absolutely NO WARRANTY; see the file COPYING for details.\n"),
    _("This software may be redistributed and modified under the terms of the GNU General Public License; again, see the file COPYING for details.\n"),
    NULL);
    g_print(message, gtk_major_version, gtk_minor_version, gtk_micro_version, GTK_MAJOR_VERSION, GTK_MINOR_VERSION, GTK_MICRO_VERSION);
    g_free(message);
    exit(EXIT_SUCCESS);
  }

  if(scheme_script_name)
    Denemo.scheme_file = g_build_filename (get_system_data_dir (), COMMANDS_DIR, scheme_script_name, NULL);

  if(Denemo.prefs.audio_driver)
    g_string_ascii_down (Denemo.prefs.audio_driver);

  if(Denemo.prefs.midi_driver)
    g_string_ascii_down (Denemo.prefs.midi_driver);

#ifdef HAVE_SIGCHLD
  //signal (SIGCHLD, sigchld_handler);
#endif

  //Set command line mode if gtk could not be initialized
  if(!gtkstatus)
    Denemo.non_interactive = TRUE;

  return filenames;
}

static void
localization_init()
{
  setlocale (LC_ALL, "");
  bindtextdomain(GETTEXT_PACKAGE, get_system_locale_dir ());
  bind_textdomain_codeset(GETTEXT_PACKAGE, "UTF-8");
  textdomain(GETTEXT_PACKAGE);
}

static void
init_environment()
{
#ifdef G_OS_WIN32
  gchar *prefix = g_win32_get_package_installation_directory (NULL, NULL);
  gchar *guile = g_build_filename (prefix, "share", "guile", NULL);
  gchar *guile_1_8 = g_build_filename (guile, "1.8", NULL);
  gchar *lilypond_current_scm = g_build_filename (prefix, "share", "lilypond", "current", "scm", NULL);
  gchar *denemo_scm = g_build_filename (get_system_data_dir (), COMMANDS_DIR, NULL);
  gchar *denemo_modules_scm = g_build_filename (get_system_data_dir (), COMMANDS_DIR, "denemo-modules", NULL);
  if (g_file_test (guile, G_FILE_TEST_EXISTS))
    {
      gchar *guile_path = g_strconcat (guile, ";", guile_1_8, ";", denemo_scm, ";", denemo_modules_scm, ";", lilypond_current_scm, NULL);
      //FIXME TRUE means we overwrite any installed version of lilyponds scm, FALSE risks not putting denemos scm in the path...
      g_setenv ("GUILE_LOAD_PATH", guile_path, TRUE);
      g_info ("Setting GUILE_LOAD_PATH=%s\n", guile_path);
    }
  else
    warningdialog (_("You may need to set GUILE_LOAD_PATH to the directory where you have ice9 installed\n"));
  g_setenv ("PANGO_PREFIX", prefix, TRUE);
  g_setenv ("PANGO_MODULE_VERSION", "1.6.0", TRUE);
  g_setenv ("PANGO_SO_EXTENSION", ".dll", TRUE);
  g_info ("Setting PANGO_PREFIX=%s\n", prefix);

  g_setenv ("GTK_MODULE_VERSION", "2.10.0", TRUE);
  g_setenv ("GTK_SO_EXTENSION", ".dll", TRUE);
  g_setenv ("GTK_PREFIX", prefix, TRUE);
  g_info ("Setting GTK_PREFIX=%s\n", prefix);

  gchar *fc_path = g_build_filename (prefix, "etc", "fonts", NULL);
  g_setenv ("FONTCONFIG_PATH", fc_path, TRUE);
  g_info ("Setting FONTCONFIG_PATH=%s\n", fc_path);
  gchar *fc_file = g_build_filename (fc_path, "fonts.conf", NULL);
  g_setenv ("FONTCONFIG_FILE", fc_file, TRUE);
  g_info ("Setting FONTCONFIG_FILE=%s\n", fc_file);


  //gchar *program_files = g_getenv ("PROGRAMFILES");
  gchar *path = g_getenv ("PATH");
  gchar *lilypond_path = g_build_filename (prefix, "bin", NULL);
  gchar *lib_path = g_build_filename (prefix, "lib", NULL);
  path = g_strconcat (path, ";", lilypond_path, ";", lib_path, NULL);

  g_setenv ("PATH", path, TRUE);
  g_info ("PATH set to %s\n", path);
  gchar *lilypond_data_path = g_build_filename (prefix, "share", "lilypond", "current", NULL);
  g_setenv ("LILYPOND_DATA_PATH", lilypond_data_path, FALSE);
  g_info ("LILYPOND_DATA_PATH will be %s if not already set", lilypond_data_path);

  append_to_path ("GUILE_LOAD_PATH", get_system_data_dir (), NULL);

#else
  add_font_directory (g_build_filename (get_system_data_dir (), "fonts", NULL));

  GList* dirs = NULL;
  dirs = g_list_append(dirs, g_build_filename (PACKAGE_SOURCE_DIR, COMMANDS_DIR, NULL));
  dirs = g_list_append(dirs, g_build_filename (get_system_data_dir (), COMMANDS_DIR, NULL));
  gchar* data_dir = find_dir_for_file ("denemo.scm", dirs);

  append_to_path ("GUILE_LOAD_PATH",
                  g_build_filename (data_dir, NULL),
                  g_build_filename (data_dir, "denemo-modules", NULL),
                  NULL);

#endif /* end of else not windows */
  g_setenv ("LILYPOND_VERBOSE", "1", FALSE);

  gchar *fontpath = NULL;
  fontpath = find_denemo_file(DENEMO_DIR_FONTS, "feta.ttf");
  if(fontpath)
      add_font_file (fontpath);
  else
    g_info("Did not find feta.ttf - perhaps installed in system");
  g_free(fontpath);

  fontpath = find_denemo_file(DENEMO_DIR_FONTS,  "Denemo.ttf");
  if(fontpath)
    add_font_file (fontpath);
  else
    g_info("Did not find Denemo.ttf - perhaps installed in system");
  g_free(fontpath);

  fontpath = find_denemo_file(DENEMO_DIR_FONTS,  "emmentaler.ttf");
  if(fontpath)
     add_font_file (fontpath);
  else
    g_info("Did not find emmentaler.ttf - perhaps installed in system");
  g_free(fontpath);

  g_setenv ("LYEDITOR", "denemoclient %(line)s %(column)s", FALSE);
}


//check  .denemo-xxx directory already exists set Denemo.old_user_data_dir to it if so.
static void check_if_upgrade (void) {
    if(get_user_data_dir (FALSE)==NULL) {
        guint32 ver_maj, ver_min, ver_mic, sofar=0;
        guint32 this_maj, this_min, this_mic, this_ver;
        guint32 foundmaj=0, foundmin=0, foundmic=0;
        const gchar *name;
        gchar *dotdenemodirname = NULL;
        sscanf (PACKAGE_VERSION, "%u.%u.%u", &this_maj, &this_min, &this_mic);
        this_ver = (this_maj<<16) + (this_min<<8) + this_mic;//allows for version numbers up to 256
        GDir *dir = g_dir_open (g_get_home_dir (), 0, NULL);
        if(dir==NULL)
            {
            g_warning ("Cannot find home directory");
            return;
            }
        while ((name = g_dir_read_name (dir))) {
            gchar *filename = g_build_filename (g_get_home_dir (), name, NULL);
            if(g_file_test (filename, G_FILE_TEST_IS_DIR)) {
                guint32 val;
                ver_maj=ver_min=ver_mic=0;
                sscanf (name, ".denemo-%u.%u.%u", &ver_maj, &ver_min, &ver_mic);
                //g_debug (" %u %u %u\n", ver_maj, ver_min, ver_mic);
                val = (ver_maj<<16) + (ver_min<<8) + ver_mic;
                if(val) g_debug("name %s", name);
                if (val>this_ver) {
                    g_warning ("Downgrade of Denemo version. Ignoring");
                    return;
                }
            g_free(filename);
            if(val>sofar)
                {
                  g_free (dotdenemodirname);
                  dotdenemodirname = g_strdup (name);
                  sofar = val;
                }
            }
        }
        if(sofar) {
            Denemo.old_user_data_dir = g_build_filename (g_get_home_dir (), dotdenemodirname, NULL);
            g_free(dotdenemodirname);
        }
    }
}

/* main_log_handler:
 * Message handler. How log levels should be used:
 * ERROR:    Fatal error that makes the program stop.
 * CRITICAL: Error that don't makes the program stop, but should make a test
 *           fail
 * WARNING:  Warns the user (or developper) about unwanted, but non fatal stuffs
 * MESSAGE:  Regular messages about program execution.
 * INFO:     Further information that may interest users, when launched with
 *           --verbose. Thoses messages may be relevant to repport bugs, but
 *           non interesting ortherwise.
 * DEBUG:    Debug information that may interest developpers, when compiled with
 *           -DDEBUG or configured with --enable-debug, and launched with
 *           --verbose
 */
static void
main_log_handler(const gchar *log_domain,
                 GLogLevelFlags log_level,
                 const gchar *message,
                 gpointer user_data ){
  if(Denemo.silent)
    return;

  char* color = NULL;
  char* level = NULL;
  char* endcolor = "\033[0m";
  FILE* stream = stdout;
  char* prev = NULL;
  char* next = NULL;
  char* msg = NULL;

  if(log_level & G_LOG_LEVEL_ERROR){
    color = "\033[1;41m";
    level = "ERROR";
    stream = stderr;
  }
  else if (log_level & G_LOG_LEVEL_CRITICAL){
    color = "\033[1;31m";
    level = "CRITICAL";
    stream = stderr;
  }
  else if (log_level & G_LOG_LEVEL_WARNING){
    color = "\033[0;33m";
    level = "WARNING";
    stream = stderr;
  }
  else if (log_level & G_LOG_LEVEL_MESSAGE){
    color = "\033[0;32m";
    level = "MESSAGE";
  }
  else if (Denemo.verbose && (log_level & G_LOG_LEVEL_INFO)){
    color = "\033[0;34m";
    level = "INFO";
  }

#ifdef DEBUG
  else if (Denemo.verbose && (log_level & G_LOG_LEVEL_DEBUG)){
    color = "\033[0;35m";
    level = "DEBUG";
  }
#endif

#ifdef G_OS_WIN32
  color = "";
  level = "";
#endif

  if(color != NULL && level != NULL){
    msg = g_strdup(message);
    //Displays colored header
    g_fprintf(stream, "%s%6s - %-8s%s: ", color, log_domain, level, endcolor);

    //Add some tab
    prev = msg;
    while(next = strchr(prev, '\n')){
      *next = '\0';
      g_fprintf(stream, "%s\n                   ", prev);
      prev = next+1;
    }
    g_fprintf(stream, "%s\n", prev);
    g_free(msg);
  }
  if(log_level & G_LOG_FLAG_FATAL)
    abort();
}

int
main (int argc, char *argv[])
{
  gchar** files = NULL;
  gboolean gtk_status = FALSE;

  g_log_set_default_handler (main_log_handler, NULL);

  if(!(gtk_status = gtk_init_check (&argc, &argv)))
    g_message(_("Could not start graphical interface."));

  files = process_command_line (argc, argv, gtk_status);

#ifdef G_OS_WIN32
gint num_paths;
GtkIconTheme *theme = gtk_icon_theme_get_default ();
gtk_icon_theme_get_search_path (theme, NULL, &num_paths);
if(num_paths)
  {
    gchar **paths = (gchar**)g_malloc (sizeof(gchar*) * num_paths);
    gtk_icon_theme_get_search_path (theme, &paths, &num_paths);
    for (;num_paths; num_paths--)
      g_print ("Icon theme path %s\n", paths[num_paths - 1]);
  }
 else 
  g_warning ("No default path to icons");
#endif

  /* initialization of directory relocatability */
  initdir ();

  check_if_upgrade();
  init_environment();


  localization_init();

  //register_stock_items ();
  scm_with_guile (inner_main, files);


  return 0;
}
