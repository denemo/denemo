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
#include "importxml.h"
#include <sys/types.h>
#include <dirent.h>
#include <glib.h>
#include <glib/gstdio.h>
#include <libguile.h>
#include "view.h"
#include "exportxml.h"
#include "runsilent.h"
#include "utils.h"
#include "keyboard.h"

struct DenemoRoot Denemo;


/* just a simple check, if the user has never run denemo before
   better, keep this for whole first session? */
gboolean first_time_user(void) {
  gchar *filename = g_build_filename(locatedotdenemo(), "actions", NULL);
  gboolean ret = !g_file_test (filename, G_FILE_TEST_EXISTS);
  g_free(filename);
  return ret;
}

gboolean uses_default_commandset(void) {
  gchar *filename = g_build_filename(locatedotdenemo(), "actions", "Default.commands", NULL);
  gboolean ret = !g_file_test (filename, G_FILE_TEST_EXISTS);
  g_free(filename);
  return ret;
}







#ifdef HAVE_SIGCHLD
/* Code by Erik Mouw, taken directly from the gtk+ FAQ */

/**
 * signal handler to be invoked when child processes _exit() without
 * having to wait for them 
 */
static void
sigchld_handler (gint num)
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


          fprintf (stderr,
                   _("Parent: child exited, pid = %d, exit status = %d\n"),
                   (int) pid, exitstatus);
        }
      else if (WIFSIGNALED (status))
        {
          exitstatus = WTERMSIG (status);

          fprintf (stderr,
                   _("Parent: child terminated by signal %d, pid = %d\n"),
                   exitstatus, (int) pid);
        }
      else if (WIFSTOPPED (status))
        {
          exitstatus = WSTOPSIG (status);

          fprintf (stderr,
                   _("Parent: child stopped by signal %d, pid = %d\n"),
                   exitstatus, (int) pid);
        }
      else
        {
          fprintf (stderr,
                   _("Parent: child exited magically, pid = %d\n"),
                   (int) pid);
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

#if GTK_MAJOR_VERSION > 1
/**
 * Segmentation fault dialog warning the cannot continue
 *
 */
void
segdialog (gchar * sigtype, gchar * message)
{
  GtkWidget *dialog;
  dialog = gtk_message_dialog_new (NULL, GTK_DIALOG_DESTROY_WITH_PARENT,
                                   GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
                                   "%s : %s", sigtype, message);
  gtk_dialog_run (GTK_DIALOG (dialog));
  gtk_widget_destroy (dialog);
}
#endif /* GTK_MAJOR_VERSION > 1 */
static gchar *pidfile;
static void remove_pid_file(void) {
FILE *fp = fopen(pidfile, "w");
if(fp) {
  fprintf(fp,"%d", 0);
  fclose(fp);
  }
}
/**
 * SIGUSR1 Handler to record the LilyPond text position the user has clicked on
 *
 */
static volatile gboolean position_data;

void
denemo_client (int sig)
{
//register that a new location is available, to be picked up in an idle callback
  position_data = TRUE;
}

/**
 * SIGSEGV Handler to do nice things if denemo bombs
 *
 */

 gboolean check_for_position(void) {
if(position_data) {
  static gchar *filename = NULL;
  if(filename==NULL)
    filename = g_build_filename(locatedotdenemo(), "lylocation.txt", NULL);
  FILE *fp = fopen(filename, "r");
  if(fp) {
    gint line, col;
    gint error = fscanf(fp, "%d %d", &line, &col);
    fclose(fp);
    g_print("line %d column %d\n", line, col);
    position_data = FALSE;
    //set_lily_error(line, col, Denemo.gui);
    //highlight_lily_error(Denemo.gui);
    goto_lilypond_position(line, col);
    }
  } 
  return TRUE;//keep going
}
void
denemo_signal_handler (int sig)
{
  GList *tmp = NULL;
  DenemoGUI *gui;
  static int already_in_segfault = 0;
  if (already_in_segfault)
    exit (1);
  else
    already_in_segfault = 1;


  g_print ("\nNo of tabs : %d this code only saves one of them however\n", g_list_length (Denemo.guis));

  if (g_list_length (Denemo.guis) == 1)
    {
      gui = (DenemoGUI *) Denemo.guis->data;
      g_debug ("si is %p", gui);
      gchar *filename = g_build_filename(locatedotdenemo (), 
                                         "crashrecovery.denemo", NULL);
      gui->si->markstaffnum = 0;
      if (gui->si->lily_file)
        exportlilypond (filename, gui, TRUE);
      else
        exportXML (filename, gui, 0, 0);


    }
  else
    {
      int i = 0;
      for (tmp = Denemo.guis; tmp && g_list_length (tmp) > 1; tmp = tmp->next)
        {
          gui = (DenemoGUI *) tmp->data;
          gchar *filename = g_build_filename(locatedotdenemo (), 
                                             "crashrecovery", NULL);
          char t[5];
          sprintf (t, "%d", i);
          strncat (filename, t, strlen (t));
          strcat (filename, ".denemo");
	  gui->si->markstaffnum = 0;
          if (gui->si->lily_file)
            exportlilypond (filename, gui, TRUE);
          else
            exportXML (filename, gui, 0, 0);
          i++;
        }
    }

  exit (1);
}

/**
 * Handler used to print debug messages.
 */
void
debug_handler (const gchar *log_domain, GLogLevelFlags log_level,
               const gchar *message, gpointer user_data)
{
  //g_debug ("%s",message);
}

static
void append_to_path(gchar *path, gchar *extra) {
  gchar * the_path = (gchar*)g_getenv (path);
  if(the_path)
#ifdef G_OS_WIN32
    the_path = g_strconcat(the_path, ";", extra, NULL);
#else
    the_path = g_strconcat(the_path, ":", extra, NULL);
#endif
  else
    the_path = g_strdup(extra);  
  g_setenv (path, the_path, TRUE);
  g_print("%s is %s\n", path, the_path);
}



/**
 * Main function
 *
 */
int
main (int argc, char *argv[])
{

//#ifdef G_OS_WIN32
//  /* workaround necessary for compilation on Cygwin */
//  g_set_print_handler ((GPrintFunc)printf);
//#endif
  
   /* set the default handler for debug messages */
  //FIXME this does not work
  g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, debug_handler, NULL);


  /* initialization of directory relocatability */
  initdir();
#ifdef G_OS_WIN32
  gchar *prefix = g_win32_get_package_installation_directory (NULL, NULL);
  gchar *guile = g_build_filename (prefix, "share", "guile", NULL);
  gchar *guile_1_8 = g_build_filename (guile, "1.8", NULL);
  gchar *lilypond_current_scm = g_build_filename (prefix, "share", "lilypond", "current", "scm", NULL);
  gchar *denemo_scm = g_build_filename (prefix, "share", "denemo", "actions", NULL);
  gchar *denemo_modules_scm = g_build_filename (prefix, "share", "denemo", "actions", "denemo-modules", NULL);
  if (g_file_test (guile, G_FILE_TEST_EXISTS))
    {
      gchar *guile_path = g_strconcat (guile, ";", guile_1_8, ";", denemo_scm, ";", denemo_modules_scm,";",lilypond_current_scm, NULL);
      g_setenv ("GUILE_LOAD_PATH", guile_path, TRUE);//FIXME TRUE means we overwrite any installed version of lilyponds scm, FALSE risks not putting denemos scm in the path...
      g_print ("Setting GUILE_LOAD_PATH=%s\n", guile_path);
    }
  else
    warningdialog ("You may need to set GUILE_LOAD_PATH to the directory where you have ice9 installed\n");
  g_setenv ("PANGO_PREFIX", prefix, TRUE);
  g_setenv ("PANGO_MODULE_VERSION", "1.6.0", TRUE);
  g_setenv ("PANGO_SO_EXTENSION", ".dll", TRUE);
  g_print ("Setting PANGO_PREFIX=%s\n", prefix);

  g_setenv ("GTK_MODULE_VERSION", "2.10.0", TRUE);
  g_setenv ("GTK_SO_EXTENSION", ".dll", TRUE);
  g_setenv ("GTK_PREFIX", prefix, TRUE);
  g_print ("Setting GTK_PREFIX=%s\n", prefix);

  gchar *fc_path = g_build_filename (prefix, "etc","fonts", NULL);
  g_setenv ("FONTCONFIG_PATH", fc_path, TRUE);
  g_print ("Setting FONTCONFIG_PATH=%s\n", fc_path);
  gchar *fc_file = g_build_filename (fc_path, "fonts.conf", NULL);
  g_setenv ("FONTCONFIG_FILE", fc_file, TRUE);
  g_print ("Setting FONTCONFIG_FILE=%s\n", fc_file);


  gchar *program_files =  g_getenv("PROGRAMFILES");
  gchar *path = g_getenv ("PATH");
  gchar *lilypond_path = g_build_filename(prefix, "bin", NULL);
  gchar *lib_path = g_build_filename(prefix, "lib", NULL);
  path = g_strconcat (path,";", lilypond_path, ";", lib_path, NULL);

  g_setenv ("PATH", path, TRUE);
  g_print("PATH set to %s\n", path);
  gchar *lilypond_data_path = g_build_filename (prefix, "share", "lilypond", "current", NULL);
  g_setenv ("LILYPOND_DATA_PATH", lilypond_data_path, FALSE);
  g_print("LILYPOND_DATA_PATH will be %s if not already set", lilypond_data_path);
  gchar *fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "feta.ttf", NULL);
  g_setenv ("LILYPOND_VERBOSE", "1", FALSE);
  add_font_file(fontpath);
  fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "Denemo.ttf", NULL);
  add_font_file(fontpath);
  fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "emmentaler.ttf", NULL);
  add_font_file(fontpath);

  append_to_path ("GUILE_LOAD_PATH", g_build_filename(prefix, "share", "denemo", NULL));

#else
  gchar *prefix = g_build_filename (get_prefix_dir(), NULL);
  add_font_directory (g_build_filename (get_data_dir(), "/fonts", NULL));

  gchar *fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "feta.ttf", NULL);
  g_setenv ("LILYPOND_VERBOSE", "1", FALSE);
  add_font_file(fontpath);
  fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "Denemo.ttf", NULL);
  add_font_file(fontpath);
  fontpath = g_build_filename (prefix, "share", "fonts", "truetype","denemo", "emmentaler.ttf", NULL);
  add_font_file(fontpath);

  append_to_path("GUILE_LOAD_PATH", g_build_filename(prefix, "share", "denemo", "actions", NULL));  
  append_to_path("GUILE_LOAD_PATH", g_build_filename(prefix, "share", "denemo", "actions", "denemo-modules", NULL));  

#endif /* end of else not windows */

  g_setenv ("LYEDITOR", "denemoclient %(line)s %(column)s", FALSE);
  GError *error = NULL;
  /* glib/gtk initialization */
  if (!g_thread_supported ()){
      g_thread_init(NULL);
  }
  gdk_threads_init();

  /* acquire gdk lock */
  gdk_threads_enter();

  gtk_init (&argc, &argv);


  /* locale initialization */
  //setlocale (LC_CTYPE, "");
  //setlocale (LC_MESSAGES, "");
  setlocale (LC_ALL, "");
  //gtk_set_locale ();
  bindtextdomain (PACKAGE, get_locale_dir ());
  bind_textdomain_codeset (PACKAGE, "UTF-8");
  textdomain (PACKAGE);

  //register_stock_items ();


  //g_print("Calling scm boot guile with %d and %p\n", argc, argv);
  scm_boot_guile (argc, argv, inner_main, NULL);

  /* release gdk lock */
  gdk_threads_leave();

  return 0;
}

gchar * process_command_line(int argc, char**argv) {

  gint opts;
  GDir *dir=NULL;
  gchar *filename;
  GError *error = NULL;
  gchar *commandsetfile=NULL;
  /* parse command line and display help messages */
  gchar *helptext  = g_strconcat (_("\nGNU Denemo version "), VERSION, ".\n\n",
                                 _("\
Usage: denemo [OPTION]... [FILE]\n\n\
Run denemo, optionally starting with FILE\n\n\
Denemo is a graphical music notation editor.\n\
It uses GNU Lilypond for music typesetting\n\
Denemo is part of the GNU project.\n\n\
Options:\n\
  -h,--help             print this help and exit\n\
  -c file               use commandset found in system file\n\
  -k file               use commandset found in local file (in ~/.denemo)\n\
  -i pathtofile         process scheme commands in pathtofile on file open\n\
  -s filename           process scheme commands from system file on file open\n\
  -a scheme             process the scheme on startup\n\
  -n                    non-interactive. No GUI.\n\
  -v,--version          print version number and exit\n\n\n\
Report bugs to http://www.denemo.org\n"), NULL) ;

  gchar *copytext = _("(c) 1999-2005, 2009 Matthew Hiller, Adam Tee, and others, 2010-2011 Richard Shann, Jeremiah Benham, Nils Gey and others.\n\n\n"
"This program is provided with absolutely NO WARRANTY; see\n"
"the file COPYING for details.\n\n"
"This software may be redistributed and modified under the\n"
"terms of the GNU General Public License; again, see the file\n"
"COPYING for details.\n\n");


#ifdef HAVE_GETOPT_H
  static struct option long_options[] = {
    {"help", no_argument, NULL, 'h'},
    {"version", no_argument, NULL, 'v'},
    {NULL, 0, NULL, 0}
  };
#endif

  char const *optstring = "s:hi:vc:k:a:nA:M:";

#ifdef HAVE_GETOPT_H
  while ((opts = getopt_long (argc, argv, optstring, long_options, NULL)) != -1)
#else
  while ((opts = getopt (argc, argv, optstring)) != -1)
#endif
    {
//      g_print("opt %c has %s\n", opts, argv[optind]);

      switch (opts) {
        case 'h':
          g_print ("%s", helptext);
          exit (0);
        case 'v':
          g_print (_("\nGNU Denemo version "));
          g_print (VERSION ".\n\n");
          g_print ("%s", copytext);
          exit (0);
        case 's':
          Denemo.scheme_file = g_build_filename(get_data_dir(), "actions",  optarg, NULL);
          break;
        case 'a':
          Denemo.scheme_commands = g_strdup(optarg);
          break;
        case 'i':
          Denemo.scheme_file = g_strdup(optarg);
          break;
        case 'c':
          commandsetfile = g_build_filename(get_data_dir(), "actions",  optarg, NULL);
          break;
        case 'k':
          commandsetfile = g_build_filename(locatedotdenemo(), "actions",  optarg, NULL);
          break;
        case 'n':
          Denemo.non_interactive = TRUE;
          break;
        case 'A':
          g_string_assign(Denemo.prefs.audio_driver, optarg);
          g_string_ascii_down(Denemo.prefs.audio_driver);
          break;
        case 'M':
          g_string_assign(Denemo.prefs.midi_driver, optarg);
          g_string_ascii_down(Denemo.prefs.midi_driver);
          break;
      }
    }

  g_print (_("\nGNU Denemo, a free and open music notation editor\n"));
  g_print ("%s", copytext);

  g_free (helptext);


  /* Set up the signal handlers */

  //  signal (SIGSEGV, denemo_signal_handler);
#if 0
//it seems that GtkPrintOperation uses this signal (SIGUSR1) so this code interferes with printing
    {
      __pid_t pid = getpid();
      pidfile = g_build_filename(locatedotdenemo(), "pid", NULL);
      FILE *fp = fopen(pidfile, "w");
      if(fp) {
        fprintf(fp, "%d\n", pid);
        fclose(fp);
        g_atexit((GVoidFunc)remove_pid_file);
        struct sigaction act = { denemo_client, 0, SA_SIGINFO};
        sigaction (SIGUSR1, &act, NULL);
        g_idle_add((GSourceFunc)check_for_position, NULL);
      }
    }
#endif
#ifdef HAVE_SIGCHLD
  signal (SIGCHLD, sigchld_handler);
#endif

    if (optind < argc)
      return argv[optind];
    else
      return NULL;


}
