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
#include "midiseq.h"

struct DenemoRoot Denemo;
midi_seq *sq;
#include "view.h"
#include "exportxml.h"
#include "runsilent.h"
#include "utils.h"
#include "alsaplayback.h"

int openfile (gchar * name);

static const GtkStockItem denemo_stock_items[] = {
  {"denemo-staccato", N_("Staccato"), (GdkModifierType) 0, 0, NULL},
  {"denemo-staccatissimo", N_("Staccatissimo"), (GdkModifierType) 0, 0, NULL},
  {"denemo-marcato", N_("Marcato"), (GdkModifierType) 0, 0, NULL},
  {"denemo-accent", N_("Accent"), (GdkModifierType) 0, 0, NULL},
  {"denemo-fermata", N_("Fermata"), (GdkModifierType) 0, 0, NULL},
  {"denemo-coda", N_("Coda"), (GdkModifierType) 0, 0, NULL},
  {"denemo-tenuto", N_("Tenuto"), (GdkModifierType) 0, 0, NULL},
  {"denemo-turn", N_("Turn"), (GdkModifierType) 0, 0, NULL},
  {"denemo-reverse-turn", N_("Reverse turn"), (GdkModifierType) 0, 0, NULL},
  {"denemo-trill", N_("Trill"), (GdkModifierType) 0, 0, NULL},
  {"denemo-mordent", N_("Mordent"), (GdkModifierType) 0, 0, NULL},
  {"denemo-up-bow", N_("Up bow"), (GdkModifierType) 0, 0, NULL},
  {"denemo-down-bow", N_("Down bow"), (GdkModifierType) 0, 0, NULL},
  {"denemo-rheel", N_("Right heel"), (GdkModifierType) 0, 0, NULL},
  {"denemo-lheel", N_("Left heel"), (GdkModifierType) 0, 0, NULL},
  {"denemo-ltoe", N_("Left toe"), (GdkModifierType) 0, 0, NULL},
  {"denemo-rtoe", N_("Right toe"), (GdkModifierType) 0, 0, NULL},
  {"denemo-whole-note", N_("Whole note"), (GdkModifierType) 0, 0, NULL},
  {"denemo-half-note", N_("Half note"), (GdkModifierType) 0, 0, NULL},
  {"denemo-quarter-note", N_("Quarter note"), (GdkModifierType) 0, 0, NULL},
  {"denemo-eight-note", N_("Eight note"), (GdkModifierType) 0, 0, NULL},
  {"denemo-sixteenth-note", N_("Sixteenth note"), (GdkModifierType) 0, 0,
   NULL},
  {"denemo-whole-rest", N_("Whole rest"), (GdkModifierType) 0, 0, NULL},
  {"denemo-half-rest", N_("Half rest"), (GdkModifierType) 0, 0, NULL},
  {"denemo-quarter-rest", N_("Quarter rest"), (GdkModifierType) 0, 0, NULL},
  {"denemo-eight-rest", N_("Eigth rest"), (GdkModifierType) 0, 0, NULL},
  {"denemo-sixteenth-rest", N_("Sixteenth rest"), (GdkModifierType) 0, 0,
   NULL},
  {"denemo-prall", N_("Prall"), (GdkModifierType) 0, 0, NULL},
  {"denemo-flageolet", N_("Flageolet"), (GdkModifierType) 0, 0, NULL},
  {"denemo-prallmordent", N_("PrallMordent"), (GdkModifierType) 0, 0, NULL},
  {"denemo-prallprall", N_("PrallPrall"), (GdkModifierType) 0, 0, NULL},
  {"denemo-open", N_("Open"), (GdkModifierType) 0, 0, NULL},
  {"denemo-segno", N_("Segno"), (GdkModifierType) 0, 0, NULL},
  {"denemo-stopped", N_("Stopped"), (GdkModifierType) 0, 0, NULL},
  {"denemo-thumb", N_("Thumb"), (GdkModifierType) 0, 0, NULL},
  {"denemo-upprall", N_("Upprall"), (GdkModifierType) 0, 0, NULL},
  {"denemo-arpeggio", N_("Arpeggio"), (GdkModifierType) 0, 0, NULL}
};

static void
register_stock_icon (GtkIconFactory * icon_factory, const gchar * stock_id,
                     const gchar * file)
{
  static gboolean warned;
  GtkIconSet *icon_set;
  GdkPixbuf *pixbuf;
  GError *error = NULL;
  gchar *path = g_build_filename (get_data_dir (), "pixmaps", file, NULL);

  g_debug ("path is %s\n", path);
  pixbuf = gdk_pixbuf_new_from_file (path, &error);
  if(!warned)
  if(error != NULL)
    {
      g_warning (_("Could not load specified pixbuf:\n%s\n"),
                 error->message);
      g_error_free (error);
      warningdialog("some icons will not display properly, but program will run ok.");
      warned = TRUE;
    }
  if(pixbuf != NULL) {
    icon_set = gtk_icon_set_new_from_pixbuf (pixbuf);
    g_object_unref (pixbuf);
    gtk_icon_factory_add (icon_factory, stock_id, icon_set);
    g_free (path);
  } 
  //FIXME  path may leak
}

// removes the accel of a standard stock item. We use this because we want to
// handle all the keybindings in denemo's keymap.
// Also, removes the _ in the label of the accel so that no mnemonic is defined
static void
clean_stock_item(const gchar *stock_id)
{
    gint i, j;
    GtkStockItem stock;
    gchar *label;
    if (gtk_stock_lookup(stock_id, &stock)) {
        label = g_strdup(stock.label);
        if (stock.label != NULL) {
            i = 0;
            j = 0;
            while (stock.label[i]) {
                if (stock.label[i] == '_')
                    i++;
                else {
                    label[j] = stock.label[i];
                    i++;
                    j++;
                }
            }
            label[j] = '\0';
        }
        stock.label = label;
        stock.keyval = 0;
        stock.modifier = 0;
        gtk_stock_add(&stock, 1);
        g_free(label);
    }
}

static void
register_stock_items ()
{
  GtkIconFactory *icon_factory;

  /* Load stock items that denemo defines*/
  gtk_stock_add_static (denemo_stock_items,
                        G_N_ELEMENTS (denemo_stock_items));

  /* Load stock icons for the new stock items*/
  icon_factory = gtk_icon_factory_new ();
  gtk_icon_factory_add_default (icon_factory);
  register_stock_icon (icon_factory, "denemo-staccato", "staccato.svg");
  register_stock_icon (icon_factory, "denemo-staccatissimo",
		       "staccatissimo.svg");
  register_stock_icon (icon_factory, "denemo-marcato", "marcato.svg");
  register_stock_icon (icon_factory, "denemo-accent", "accent.svg");	 
  register_stock_icon (icon_factory, "denemo-fermata", "fermata.svg");
  register_stock_icon (icon_factory, "denemo-tenuto", "tenuto.svg");
  register_stock_icon (icon_factory, "denemo-turn", "turn.svg");
  register_stock_icon (icon_factory, "denemo-reverse-turn",
		       "reverse-turn.svg");	 
  register_stock_icon (icon_factory, "denemo-trill", "trill.svg");
  register_stock_icon (icon_factory, "denemo-mordent", "mordent.svg");
  register_stock_icon (icon_factory, "denemo-up-bow", "upbow.svg");
  register_stock_icon (icon_factory, "denemo-down-bow", "downbow.svg");
	 
  register_stock_icon (icon_factory, "denemo-rheel", "rheel.svg");
  register_stock_icon (icon_factory, "denemo-lheel", "lheel.svg");
  register_stock_icon (icon_factory, "denemo-rtoe", "rtoe.svg");
  register_stock_icon (icon_factory, "denemo-ltoe", "ltoe.svg");
	 
  register_stock_icon (icon_factory, "denemo-whole-note", "icon-note-0.svg");
  register_stock_icon (icon_factory, "denemo-half-note", "icon-note-1.svg");
  register_stock_icon (icon_factory, "denemo-quarter-note",
		       "icon-note-2.svg");
  register_stock_icon (icon_factory, "denemo-eight-note", "icon-note-3.svg");
  register_stock_icon (icon_factory, "denemo-sixteenth-note",
		       "icon-note-4.svg");	 
  register_stock_icon (icon_factory, "denemo-whole-rest", "icon-rest-0.svg");
  register_stock_icon (icon_factory, "denemo-half-rest", "icon-rest-1.svg");
  register_stock_icon (icon_factory, "denemo-quarter-rest",
		       "icon-rest-2.svg");
  register_stock_icon (icon_factory, "denemo-eight-rest", "icon-rest-3.svg");
  register_stock_icon (icon_factory, "denemo-sixteenth-rest",
		       "icon-rest-4.svg");

  //New Ornaments added here, loding XBM's until svgs have been generated.
  register_stock_icon (icon_factory, "denemo-coda",
                       "feta26-scripts-coda.xbm");
  register_stock_icon (icon_factory, "denemo-prall",
                       "feta26-scripts-prall.xbm");
  register_stock_icon (icon_factory, "denemo-flageolet",
                       "feta26-scripts-flageolet.xbm");
  register_stock_icon (icon_factory, "denemo-prallmordent",
                       "feta26-scripts-prallmordent.xbm");
  register_stock_icon (icon_factory, "denemo-prallprall",
                       "feta26-scripts-prallprall.xbm");
  register_stock_icon (icon_factory, "denemo-open",
                       "feta26-scripts-open.xbm");
  register_stock_icon (icon_factory, "denemo-segno",
                       "feta26-scripts-segno.xbm");
  register_stock_icon (icon_factory, "denemo-stopped",
                       "feta26-scripts-stopped.xbm");
  register_stock_icon (icon_factory, "denemo-thumb",
                       "feta26-scripts-thumb.xbm");
  register_stock_icon (icon_factory, "denemo-upprall",
                       "feta26-scripts-upprall.xbm");
  register_stock_icon (icon_factory, "denemo-arpeggio",
                       "feta26-scripts-arpeggio.xbm");

  //remove accelerators from some gtk standard stock items
  clean_stock_item(GTK_STOCK_NEW);
  clean_stock_item(GTK_STOCK_OPEN);
  clean_stock_item(GTK_STOCK_SAVE);
  clean_stock_item(GTK_STOCK_CLOSE);
  clean_stock_item(GTK_STOCK_QUIT);
  clean_stock_item(GTK_STOCK_CUT);
  clean_stock_item(GTK_STOCK_COPY);
  clean_stock_item(GTK_STOCK_PASTE);
  clean_stock_item(GTK_STOCK_GOTO_FIRST);
  clean_stock_item(GTK_STOCK_GOTO_LAST);
  clean_stock_item(GTK_STOCK_ADD);
  clean_stock_item(GTK_STOCK_REMOVE);
  clean_stock_item(GTK_STOCK_FIND);

  g_object_unref (icon_factory);
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

/**
 * SIGSEGV Handler to do nice things if denemo bombs
 *
 */
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


  g_print ("\nNo of displays : %d\n", g_list_length (Denemo.guis));

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
#ifdef DEBUG
  g_print (message);
#endif
}





/**
 * Main function
 *
 */
int
main (int argc, char *argv[])
{
  gint opts;
  GError *error = NULL;
  DenemoGUI *gui = (DenemoGUI *) g_malloc0 (sizeof (DenemoGUI));

//#ifdef G_OS_WIN32
//  /* workaround necessary for compilation on Cygwin */
//  g_set_print_handler ((GPrintFunc)printf);
//#endif


  /* set the default handler for debug messages */
  g_log_set_handler (NULL, G_LOG_LEVEL_DEBUG, debug_handler, NULL);


  /* initialization of directory relocatability */
  initdir();


  /* locale initialization */
  //setlocale (LC_CTYPE, "");
  //setlocale (LC_MESSAGES, "");
  setlocale (LC_ALL, "");
  //gtk_set_locale ();
  bindtextdomain (PACKAGE, get_locale_dir ());
  bind_textdomain_codeset (PACKAGE, "UTF-8");
  textdomain (PACKAGE);


  /* parse command line and display help messages */
  gchar *helptext = g_strconcat (_("\nGNU Denemo version "), VERSION, ".\n\n",
                                 _("\
Usage: denemo [OPTION]... [FILE]\n\n\
Run denemo, opening save file FILE\n\n\
Denemo is a graphical music notation editor. It produces save files\n\
in GNU Lilypond input format (suitable for immediate typesetting with GNU\n\
Lilypond) and Adam Tee's JTF file format. Denemo is part of the GNU\n\
project.\n\n\
Options:\n\
  -h,--help             print this help and exit\n\
  -s,--silent           lets just start with silent lilypond conversion\n\
  -v,--version          print version number and exit\n\n\n\
Report bugs to bug-denemo@gnu.org\n"), NULL);

  gchar *copytext = _("\
(c) 1999-2005 Matthew Hiller, Adam Tee, and others\n\n\n\
This program is provided with absolutely NO WARRANTY; see\n\
the file COPYING for details.\n\n\
This software may be redistributed and modified under the\n\
terms of the GNU General Public License; again, see the file\n\
COPYING for details.\n\n");

#ifdef HAVE_GETOPT_H
  static struct option long_options[] = {
    {"help", no_argument, NULL, 'h'},
    {"version", no_argument, NULL, 'v'}
  };
#endif

#ifdef HAVE_GETOPT_H
  while ((opts = getopt_long (argc, argv, "shvt:", long_options, NULL)) != -1)
#else
  while ((opts = getopt (argc, argv, "shvt:")) != -1)
#endif
    {
      if (opts == 'h')
        {
          g_print (helptext);
          exit (0);
        }
      else if (opts == 's')
        {
          g_print (copytext);
          silentconversion (argv[optind], gui);
          exit (0);
        }
      else if (opts == 'v')
        {
          g_print (_("\nGNU Denemo version "));
          g_print (VERSION ".\n\n");
          g_print (copytext);
          exit (0);
        }
    }

  g_print (_("\nGNU Denemo, a gtk+ frontend for GNU Lilypond\n"));
  g_print (copytext);

  g_free (helptext);


  /* gtk initialization */
  gtk_init (&argc, &argv);
  /* adapt stock items to denemo */
  register_stock_items ();
  /* Following calls were made previously in newview. However I think they are
   * global, and should be done once and for all when the application opens
   */
  /* Initialize preferences */
  initprefs();
  /* read history file */
  readHistory();
  /* Set up the keymap */
  //init_keymap();
  
  /* audio initialization */
  ext_init ();                  /* external players (midi...) */
  midi_init ();                 /* internal (not working yet) */
  //DenemoPrefs prefs;
  //  readxmlprefs("src/denemorc", &prefs);

#ifdef HAVEALSA
  if (NULL == (sq = midi_seq_new ("Denemo")))
    {
      g_print ("Sequencer Error.\n");
    }
#endif


  /* Set up the signal handler */
  signal (SIGSEGV, denemo_signal_handler);
#ifdef HAVE_SIGCHLD
  signal (SIGCHLD, sigchld_handler);
#endif


 
    scm_boot_guile (argc, argv, inner_main, NULL);
  return 0;
}


int
openfile (gchar * name)
{
  GList *tmp = g_list_nth (Denemo.guis, 0);

  DenemoGUI *gui = (DenemoGUI *) tmp->data;
  gint result = open_for_real (name, gui, FALSE, FALSE);
  //gui->si->readonly = FALSE;
  //si->readonly = TRUE;
  return result;
}
