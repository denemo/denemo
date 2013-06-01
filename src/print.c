/* Print.c
 * 
 * printing support for GNU Denemo
 * outputs to an evince widget, or to a pdf or png file
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Adam Tee, 2009, 2010, 2011 Richard Shann
 */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <glib/gstdio.h>
#include <errno.h>
#include <denemo/denemo.h>
#include <evince-view.h>

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_WAIT_H
#include <wait.h>
#endif

#include "print.h"
#include "prefops.h"
#include "exportlilypond.h"
#include "utils.h"
#include "view.h"
#include "external.h"
#include "scorelayout.h"
#include "lilydirectives.h"

#if GTK_MAJOR_VERSION==3
typedef enum
{
  GDK_RGB_DITHER_NONE,
  GDK_RGB_DITHER_NORMAL,
  GDK_RGB_DITHER_MAX
} GdkRgbDither;
#endif

typedef struct lilyversion
{
  gint major;
  gint minor;
} lilyversion;

static gint LilyPond_stderr = -1;       //A file descriptor to pipe for LilyPond's stderr
static GError *lily_err = NULL;
static gint changecount = -1;   //changecount when the printfile was last created FIXME multiple tabs are muddled
static GPid previewerpid = GPID_NONE;

static gchar *thumbnailsdirN = NULL;
static gchar *thumbnailsdirL = NULL;

static gboolean retypeset (void);

printstatus*
get_print_status(){
  static printstatus PrintStatus = { GPID_NONE, 0, 0, 4, 4, 4, 4, TYPESET_ALL_MOVEMENTS, 0, 0, {NULL, NULL} , {NULL, NULL}, {NULL, NULL} };
  return &PrintStatus;
}

WysiwygInfo*
get_wysiwig_info(){
  static WysiwygInfo Ww;                   //Wysywyg information
  return &Ww;
}

static void
advance_printname ()
{
  if (get_print_status()->printbasename[0] == NULL)
    {
      get_print_status()->printbasename[0] = g_build_filename (locateprintdir (), "denemoprintA", NULL);
      get_print_status()->printbasename[1] = g_build_filename (locateprintdir (), "denemoprintB", NULL);
      get_print_status()->printname_pdf[0] = g_strconcat (get_print_status()->printbasename[0], ".pdf", NULL);
      get_print_status()->printname_ly[0] = g_strconcat (get_print_status()->printbasename[0], ".ly", NULL);
      get_print_status()->printname_pdf[1] = g_strconcat (get_print_status()->printbasename[1], ".pdf", NULL);
      get_print_status()->printname_ly[1] = g_strconcat (get_print_status()->printbasename[1], ".ly", NULL);
    }

  get_print_status()->cycle = !get_print_status()->cycle;
  /*gint success =*/ g_unlink (get_print_status()->printname_pdf[get_print_status()->cycle]);
  //g_print("Removed old pdf file %s %d\n",get_print_status()->printname_pdf[get_print_status()->cycle], success);
}


/*** 
 * make sure lilypond is in the path defined in the preferences
 */
/* UNUSED
gboolean
check_lilypond_path (DenemoGUI * gui)
{

  gchar *lilypath = g_find_program_in_path (Denemo.prefs.lilypath->str);
  if (lilypath == NULL)
    {
      // show a warning dialog
      GtkWidget *dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
                                                  GTK_DIALOG_DESTROY_WITH_PARENT,
                                                  GTK_MESSAGE_WARNING,
                                                  GTK_BUTTONS_OK,
                                                  _("Could not find %s"),
                                                  Denemo.prefs.lilypath->str);
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog), _("Please edit lilypond path " "in the preferences."));
      gtk_dialog_run (GTK_DIALOG (dialog));

      // free the memory and return
      gtk_widget_destroy (dialog);
      return 0;
    }
  else
    return 1;
}
*/

static int
version_check (lilyversion base, lilyversion installed)
{
  if (base.major > installed.major)
    return LESSER;
  if (base.major < installed.major)
    return GREATER;
  if (base.minor == installed.minor)
    return SAME;
  if (base.minor > installed.minor)
    return LESSER;
  if (base.minor < installed.minor)
    return GREATER;

  /* if none of the above something is wrong */
  return -1;
}

static lilyversion
string_to_lilyversion (char *string)
{
  lilyversion version = { 2, 0 };
  char **token;
  const char delimiters[] = ".";
  if (string == NULL || *string == 0)
    return version;
  /* split string */
  token = g_strsplit (string, delimiters, 2);

  /* get major version number */
  if (token[0])
    version.major = atoi (token[0]);
  /* get minor version number */
  if (token[1])
    version.minor = atoi (token[1]);
  g_strfreev (token);
  //intf("\nstring_to_lilyversion() major = %d minor = %d\n",version.major, version.minor);
  return version;
}

/* UNUSED
static gchar *
regex_parse_version_number (const gchar * str)
{
  GRegex *regex = NULL;
  GMatchInfo *match_info;
  GString *lilyversion = g_string_new ("");

  regex = g_regex_new ("\\d.\\d\\d", 0, 0, NULL);
  g_regex_match (regex, str, 0, &match_info);

  if (g_match_info_matches (match_info))
    {
      g_string_append (lilyversion, g_match_info_fetch (match_info, 0));
    }

  g_match_info_free (match_info);
  g_regex_unref (regex);
  return g_string_free (lilyversion, FALSE);
}
*/

gchar *
get_lily_version_string (void)
{
  return INSTALLED_LILYPOND_VERSION;
}

int
check_lily_version (gchar * version)
{
  gchar *version_string = get_lily_version_string ();
  lilyversion installed_version = string_to_lilyversion (version_string);
  lilyversion check_version = string_to_lilyversion (version);
  return version_check (check_version, installed_version);
}


/* returns the base name (/tmp/Denemo????/denemoprint usually) used as a base
   filepath for printing.
   The returned string should not be freed.
*/

static gchar *
get_printfile_pathbasename (void)
{
  if (get_print_status()->printbasename[0] == NULL)
    advance_printname ();
  return get_print_status()->printbasename[get_print_status()->cycle];
}

/* truncate epoint after 20 lines replacing the last three chars in that case with dots */
static void
truncate_lines (gchar * epoint)
{
  gint i;
  for (i = 0; i < 20 && *epoint; i++)
    {
      while (*epoint && *epoint != '\n')
        epoint++;
      if (*epoint)
        epoint++;
    }
  if (epoint)
    *epoint-- = '\0';
  /* replace last three chars with ... This is always possible if epoint is not NULL */
  if (*epoint)
    for (i = 3; i > 0; i--)
      *epoint-- = '.';
}

/***
 * Run the command line convert-ly to get the lilypond output 
 * current with the version running on the users computer
 *
 */
/* UNUSED
void
convert_ly (gchar * lilyfile)
{
  GError *err = NULL;
#ifdef G_OS_WIN32
  gchar *conv_argv[] = {
    "python" "convert-ly.py",
    "-e",
    lilyfile,
    NULL
  };
#else
  gchar *conv_argv[] = {
    "convert-ly",
    "-e",
    lilyfile,
    NULL
  };
#endif
  g_spawn_sync (locateprintdir (),      // dir 
                conv_argv, NULL,        // env 
                G_SPAWN_SEARCH_PATH, NULL,      // child setup func 
                NULL,           // user data 
                NULL,           // stdout 
                NULL,           // stderr 
                NULL, &err);

  if (err != NULL)
    {
      g_warning ("%s", err->message);
      if (err)
        g_error_free (err);
      err = NULL;
    }
}
*/
                                                  
static void
process_lilypond_errors (gchar * filename)
{
  get_print_status()->invalid = 0;
  if (LilyPond_stderr == -1)
    return;
  gchar *basename = g_path_get_basename (filename);
  gchar *filename_colon = g_strdup_printf ("%s.ly%s", basename, ":");
  g_free (basename);
  gchar *epoint = NULL;
#define bufsize (100000)
  gchar *bytes = g_malloc0 (bufsize);
  gint numbytes = read (LilyPond_stderr, bytes, bufsize - 1);
  close (LilyPond_stderr);
  LilyPond_stderr = -1;
#undef bufsize

  if (numbytes == -1)
    {
      g_free(filename_colon);
      g_free (bytes);
      return;
    }
  //g_print("\nLilyPond error messages\n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8>< %s \n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><\n", bytes);
  epoint = g_strstr_len (bytes, strlen (bytes), filename_colon);
  if (epoint)
    {
      gint line, column;
      gint cnv = sscanf (epoint + strlen (filename_colon), "%d:%d", &line, &column);
      truncate_lines (epoint);  /* truncate epoint if it has too many lines */
      if (cnv == 2)
        {
          line--;               /* make this 0 based */
          if (line >= gtk_text_buffer_get_line_count (Denemo.textbuffer))
            warningdialog (_("Spurious line number")), line = 0;
          /* gchar *errmsg = g_strdup_printf("Error at line %d column %d %d", line,column, cnv); */
          /*     warningdialog(errmsg); */
          console_output (epoint);
          if (Denemo.textbuffer)
            {
              set_lily_error (line + 1, column);
            }
          goto_lilypond_position (line + 1, column);
          get_print_status()->invalid = 2;      //print_is_valid = FALSE;
          if (Denemo.printarea)
            gtk_widget_queue_draw (Denemo.printarea);
          // FIXME this causes a lock-up     warningdialog("Typesetter detected errors. Cursor is position on the error point.\nIf in doubt delete and re-enter the measure.");
        }
      else
        {
          set_lily_error (0, 0);
          warningdialog (epoint);
        }
    }
  else
    set_lily_error (0, 0); /* line 0 meaning no line */
  highlight_lily_error ();
  g_free (filename_colon);
  if (lily_err != NULL)
    {
      if (*bytes)
        console_output (bytes);
      warningdialog (_("Could not execute lilypond - check Edit->preferences->externals->lilypond setting\nand lilypond installation"));
      g_warning ("%s", lily_err->message);
      if (lily_err)
        g_error_free (lily_err);
      lily_err = NULL;
    }
  g_free (bytes);
}

static void
open_viewer (gint status, gchar * filename, gboolean is_png)
{
  if (get_print_status()->printpid == GPID_NONE)
    return;
  GError *err = NULL;
  gchar *printfile;
  gchar **arguments;
  progressbar_stop ();
  g_spawn_close_pid (get_print_status()->printpid);
  get_print_status()->printpid = GPID_NONE;
  //normal_cursor();
  process_lilypond_errors (filename);
#ifndef G_OS_WIN32
  //status check seems to fail on windows, and errors are not highlighted for windows.
  if (status)
    {
      g_warning /* a warning dialog causes deadlock in threaded version of program */ ("LilyPond engraver failed - See highlighting in LilyPond window (open the LilyPond window and right click to print)");
    }
  else
#endif
    {

      if (is_png)
        printfile = g_strconcat (filename, ".png", NULL);
      else
        printfile = g_strconcat (filename, ".pdf", NULL);


      if (!g_file_test (printfile, G_FILE_TEST_EXISTS))
        {
          //FIXME use filename in message
          g_warning ("Failed to find %s, check permissions", (gchar *) printfile);
          g_free (printfile);
          return;
        }
      gchar *png[] = {
        Denemo.prefs.imageviewer->str,
        printfile,
        NULL
      };
      gchar *pdf[] = {
        Denemo.prefs.pdfviewer->str,
        printfile,
        NULL
      };
      if (is_png)
        {

          arguments = png;
        }
      else
        {

          arguments = pdf;
        }
      if ((!is_png && (Denemo.prefs.pdfviewer->len == 0)) || (is_png && (Denemo.prefs.imageviewer->len == 0)))
        {
          gboolean ok = run_file_association (printfile);
          if (!ok)
            {
              err = g_error_new (G_FILE_ERROR, -1, "Could not run file assoc for %s", is_png ? ".png" : ".pdf");
              g_warning ("Could not run the file association for a %s file\n", is_png ? ".png" : ".pdf");
            }
        }
      else
        {
          g_spawn_async_with_pipes (locateprintdir (),  /* dir */
                                    arguments, NULL,    /* env */
                                    G_SPAWN_SEARCH_PATH,        /* search in path for executable */
                                    NULL,       /* child setup func */
                                    NULL,       /* user data */
                                    &previewerpid,      /* FIXME &pid see g_spawn_close_pid(&pid) */
                                    NULL, NULL, NULL, &err);
        }
      if (err != NULL)
        {
          if (Denemo.prefs.pdfviewer->len)
            {
              g_warning ("Failed to find %s", Denemo.prefs.pdfviewer->str);
              warningdialog (_("Cannot display: Check Edit->Preferences->externals\nfor your PDF viewer"));
            }
          else
            warningdialog (err->message);
          g_warning ("%s", err->message);
          if (err)
            g_error_free (err);
          err = NULL;
        }
      g_free (printfile);
    }
}


static void
open_pngviewer (G_GNUC_UNUSED GPid pid, gint status, gchar * filename)
{
  open_viewer (status, filename, TRUE);
}

static void
open_pdfviewer (G_GNUC_UNUSED GPid pid, gint status, gchar * filename)
{
  open_viewer (status, filename, FALSE);
}

static gint
run_lilypond (gchar ** arguments)
{
  gint error = 0;
  if (get_print_status()->background == STATE_NONE)
    progressbar ("Denemo Typesetting");

  if (get_print_status()->printpid != GPID_NONE)
    {
      if (confirm (_("Already doing a print"), _("Kill that one off and re-start?")))
        {
          if (get_print_status()->printpid != GPID_NONE)        //It could have died while the user was making up their mind...
            kill_process (get_print_status()->printpid);
          get_print_status()->printpid = GPID_NONE;
        }
      else
        {
          warningdialog (_("Cancelled"));
          error = -1;
          return error;
        }
    }
  if (lily_err)
    {
      g_warning ("Old error message from launching lilypond still present - message was %s\nDiscarding...\n", lily_err->message);
      g_error_free (lily_err);
      lily_err = NULL;
    }

  gboolean lilypond_launch_success = g_spawn_async_with_pipes (locateprintdir (),       /* dir */
                                                               arguments,
                                                               NULL,    /* env */
                                                               G_SPAWN_SEARCH_PATH | G_SPAWN_DO_NOT_REAP_CHILD,
                                                               NULL,    /* child setup func */
                                                               NULL,    /* user data */
                                                               &get_print_status()->printpid,
                                                               NULL,
                                                               NULL,    /* stdout */
#ifdef G_OS_WIN32
                                                               NULL,
#else
                                                               &LilyPond_stderr,        /* stderr */
#endif
                                                               &lily_err);
  if (lily_err)
    {
      g_warning ("Error launching lilypond! Message is %s\n", lily_err->message);
      g_error_free (lily_err);
      lily_err = NULL;
      error = -1;
    }
  if (!lilypond_launch_success)
    {
      g_warning ("Error executing lilypond. Perhaps Lilypond is not installed or its path is not correctly configured.");
      error = -1;
    }
  if (error)
    progressbar_stop ();

  return error;
}

gboolean
stop_lilypond ()
{
  if (get_print_status()->printpid != GPID_NONE)
    {
      kill_process (get_print_status()->printpid);
      get_print_status()->printpid = GPID_NONE;
    }
  return FALSE;                 //do not call again
}

static void
generate_lilypond (gchar * lilyfile, gboolean part_only, gboolean all_movements)
{
  DenemoGUI *gui = Denemo.gui;
  if (part_only)
    export_lilypond_part (lilyfile, gui, all_movements);
  else
    exportlilypond (lilyfile, gui, all_movements);
}

static void
run_lilypond_for_pdf (gchar * filename, gchar * lilyfile)
{
  /*arguments to pass to lilypond to create a pdf for printing */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };
  run_lilypond (arguments);
}

static unsigned
file_get_mtime (gchar * filename)
{
  struct stat thebuf;
  g_stat (filename, &thebuf);
  unsigned mtime = thebuf.st_mtime;
  // g_print("the mt is %u %u\n", mtime, thebuf.st_mtim.tv_nsec);
  return mtime;
}

/*  create pdf of current score, optionally restricted to voices/staffs whose name match the current one.
 *  generate the lilypond text (on disk)
 *  Fork and run lilypond
 */
static void
create_pdf (gboolean part_only, gboolean all_movements)
{
  advance_printname ();
  gchar *filename = get_print_status()->printbasename[get_print_status()->cycle];
  gchar *lilyfile = get_print_status()->printname_ly[get_print_status()->cycle];
  g_remove (lilyfile);
  get_print_status()->invalid = 0;
  generate_lilypond (lilyfile, part_only, all_movements);
  run_lilypond_for_pdf (filename, lilyfile);
}


/** 
 * Dialog function used to select measure range 
 *
 */

void
printrangedialog (DenemoGUI * gui)
{
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  GtkWidget *from_measure;
  GtkWidget *to_measure;

  dialog = gtk_dialog_new_with_buttons (_("Print Excerpt Range"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), GTK_STOCK_OK, GTK_RESPONSE_ACCEPT, GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  gint max_measure = g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  label = gtk_label_new (_("Print from Measure"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  from_measure = gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure), (gdouble) gui->si->selection.firstmeasuremarked);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_measure = gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  //  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure), (gdouble) gui->si->selection.lastmeasuremarked);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->selection.firstmeasuremarked = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
      gui->si->selection.lastmeasuremarked = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
      //gtk_widget_destroy (dialog);
    }
  else
    {
      gui->si->selection.firstmeasuremarked = gui->si->selection.lastmeasuremarked = 0;
    }
  if (gui->si->selection.firstmeasuremarked)
    {
      gui->si->markstaffnum = gui->si->selection.firststaffmarked = 1;
      gui->si->selection.laststaffmarked = g_list_length (gui->si->thescore);
    }

  gtk_widget_destroy (dialog);
}



static void
rm_temp_files (gchar * file, gpointer free_only)
{
  //g_print("\n%s Deleting temp file %s\n",free_only?"Not":"", file);
  if (!free_only)
    g_remove (file);
  g_free (file);
}

static void
print_finished (GPid pid, gint status, G_GNUC_UNUSED GList * filelist)
{
  if (get_print_status()->printpid == GPID_NONE)
    return;
  open_pdfviewer (pid, status, (gchar *) get_printfile_pathbasename ());
  g_debug ("print finished\n");
  changecount = Denemo.gui->changecount;
  progressbar_stop ();          //FIXME this is already done in open_pdfviewer()
}


void
printpng_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, GList * filelist)
{
  g_debug ("printpng_finished\n");
  g_list_foreach (filelist, (GFunc) rm_temp_files, FALSE);
  g_list_free (filelist);
  g_spawn_close_pid (get_print_status()->printpid);
  get_print_status()->printpid = GPID_NONE;
  progressbar_stop ();
  infodialog ("Your png file has now been created");
}

static void
printpdf_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, GList * filelist)
{
  if (filelist)
    {
      g_list_foreach (filelist, (GFunc) rm_temp_files, FALSE);
      g_list_free (filelist);
    }
  g_spawn_close_pid (get_print_status()->printpid);
  get_print_status()->printpid = GPID_NONE;
  progressbar_stop ();
  infodialog ("Your pdf file has now been created");
}

static void
prepare_preview (GPid pid, gint status, GList * filelist)
{
  open_pngviewer (pid, status, (gchar *) get_printfile_pathbasename ());
  printpng_finished (pid, status, (GList *) filelist);
}

/**
 * Does all the export pdf work.
 * calls exportmudela and then  
 * runs lilypond to a create a filename.pdf
 *
 *  @param filename filename to save score to
 *  @param finish callback after creating png or if NULL, wait for finish before returning.
 *  @param gui pointer to the DenemoGUI structure
 */
void
export_png (gchar * filename, GChildWatchFunc finish, DenemoGUI * gui)
{
  gchar *basename;
  gchar *lilyfile;
  gchar *epsfile;
  gchar *epsfile2;
  gchar *texfile;
  gchar *texifile;
  gchar *countfile;

  GList *filelist = NULL;

  /* get the intended resolution of the png */
  gchar *resolution = g_strdup_printf ("-dresolution=%d", (int) Denemo.prefs.resolution);

  /* create temp file names */
  basename = get_printfile_pathbasename ();
  lilyfile = g_strconcat (basename, ".ly", NULL);
  epsfile = g_strconcat (filename, ".eps", NULL);
  epsfile2 = g_strconcat (filename, "-1.eps", NULL);
  texfile = g_strconcat (filename, "-systems.tex", NULL);
  texifile = g_strconcat (filename, "-systems.texi", NULL);
  countfile = g_strconcat (filename, "-systems.count", NULL);

  /* create a list of files that need to be deleted */
  filelist = g_list_append (filelist, lilyfile);
  filelist = g_list_append (filelist, epsfile);
  filelist = g_list_append (filelist, epsfile2);
  filelist = g_list_append (filelist, texfile);
  filelist = g_list_append (filelist, texifile);
  filelist = g_list_append (filelist, countfile);

  /* generate the lilypond file */
  gui->lilysync = G_MAXUINT;
  exportlilypond (lilyfile, gui, finish == (GChildWatchFunc) printpng_finished ? TRUE : FALSE);
  /* create arguments needed to pass to lilypond to create a png */

  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-dbackend=eps",
    resolution,
    "-o",
    filename,
    lilyfile,
    NULL
  };

  /* generate the png file */
  if (finish)
    {
      gint error = run_lilypond (arguments);
      if (!error)
        g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) finish, (gchar *) filelist);
    }
  else
    {
      GError *err = NULL;
      g_spawn_sync (locateprintdir (),  /* dir */
                    arguments, NULL,    /* env */
                    G_SPAWN_SEARCH_PATH, NULL,  /* child setup func */
                    NULL,       /* user data */
                    NULL,       /* stdout */
                    NULL,       /* stderr */
                    NULL, &err);
      //These are in tmpdir and can be used for the .eps file, so don't delete them   g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
      g_list_free (filelist);
    }
}

/**
 * Does all the export pdf work.
 * calls exportmudela and then  
 * runs lilypond to a create a filename.pdf
 *
 *	@param filename filename to save score to
 *  @param gui pointer to the DenemoGUI structure
 */
void
export_pdf (gchar * filename, DenemoGUI * gui)
{
  gchar *basename;
  gchar *lilyfile;
  gchar *psfile;
  GList *filelist = NULL;

  basename = get_printfile_pathbasename ();
  lilyfile = g_strconcat (basename, ".ly", NULL);
  psfile = g_strconcat (filename, ".ps", NULL);

  /* create list of files that will need to be deleted */
  filelist = g_list_append (filelist, lilyfile);
  filelist = g_list_append (filelist, psfile);


  /* generate the lilypond file */
  exportlilypond (lilyfile, gui, TRUE);
  /* create arguments to pass to lilypond to create a pdf */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };
  /* generate the pdf file */

  gint error = run_lilypond (arguments);
  if (error)
    {
      g_spawn_close_pid (get_print_status()->printpid);
      get_print_status()->printpid = GPID_NONE;
      return;
    }

  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printpdf_finished, filelist);
}


static gboolean
initialize_typesetting (void)
{
  return call_out_to_guile ("(InitializeTypesetting)");
}



// Displaying Print Preview

static void
busy_cursor (void)
{
  static GdkCursor *busycursor = NULL;
  if(!busycursor)
    busycursor = gdk_cursor_new (GDK_WATCH);
  if (gtk_widget_get_window (Denemo.printarea))
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), busycursor);
}

/*UNUSED
static void
drag_cursor (void)
{
  static GdkCursor *dragcursor = NULL;
  if(!dragcursor)  
    dragcursor = gdk_cursor_new (GDK_CROSS);
  if (gtk_widget_get_window (Denemo.printarea))
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), dragcursor);
}
*/

static void
normal_cursor (void)
{
  static GdkCursor *arrowcursor = NULL;
  if(!arrowcursor)
    arrowcursor = gdk_cursor_new (GDK_RIGHT_PTR);
  if (gtk_widget_get_window (Denemo.printarea))
    gdk_window_set_cursor (gtk_widget_get_window (Denemo.printarea), arrowcursor);
}

/*void                user_function                      (EvPrintOperation       *evprintoperation,
                                                        GtkPrintOperationResult arg1,
                                                        gpointer                user_data)             : Run Last */
static void
printop_done (EvPrintOperation * printop, G_GNUC_UNUSED GtkPrintOperationResult arg1, GtkPrintSettings ** psettings)
{
  if (*psettings)
    g_object_unref (*psettings);
  *psettings = ev_print_operation_get_print_settings (printop);
  g_object_ref (*psettings);
  //g_print("Came away with uri %s\n", gtk_print_settings_get(*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI));
  set_current_scoreblock_uri (g_strdup (gtk_print_settings_get (*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI)));
  if (get_print_status()->background & STATE_PAUSED)
    {
      if (Denemo.prefs.typesetrefresh)
        get_print_status()->updating_id = g_timeout_add (Denemo.prefs.typesetrefresh, (GSourceFunc) retypeset, NULL);
      else
        get_print_status()->updating_id = g_idle_add ((GSourceFunc) retypeset, NULL);
      get_print_status()->background &= ~STATE_PAUSED;
    }
  call_out_to_guile ("(FinalizePrint)");
}

static gboolean
libevince_print (void)
{
  GError *err = NULL;
  gchar *filename = get_print_status()->printname_pdf[get_print_status()->cycle];
  gchar *uri = g_filename_to_uri (filename, NULL, &err);

  if (err)
    {
      g_warning ("Malformed filename %s\n", filename);
      return -1;
    }

  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if (err)
    {
      g_warning ("Trying to print the pdf file %s gave an error: %s", uri, err->message);
      if (err)
        g_error_free (err);
      err = NULL;
      return -1;
    }
  else
    {
      static GtkPrintSettings *settings;
      if (settings == NULL)
        settings = gtk_print_settings_new ();
      EvPrintOperation *printop = ev_print_operation_new (doc);
      g_signal_connect (printop, "done", G_CALLBACK (printop_done), &settings);
      gtk_print_settings_set (settings, GTK_PRINT_SETTINGS_OUTPUT_URI, get_output_uri_from_scoreblock ());
      ev_print_operation_set_print_settings (printop, settings);

      if (get_print_status()->updating_id)
        {
          get_print_status()->background |= STATE_PAUSED;
          g_source_remove (get_print_status()->updating_id);    //if this is not turned off the print preview thread hangs until it is.
          get_print_status()->updating_id = 0;
        }

      ev_print_operation_run (printop, NULL);
    }
  return 0;
}

gboolean
print_typeset_pdf (void)
{
  return libevince_print ();
}

static void
set_printarea_doc (EvDocument * doc)
{
  EvDocumentModel *model;

  model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");     //there is no ev_view_get_model(), when there is use it
  if (model == NULL)
    {
      model = ev_document_model_new_with_document (doc);
      ev_view_set_model ((EvView *) Denemo.printarea, model);
      g_object_set_data (G_OBJECT (Denemo.printarea), "model", model);  //there is no ev_view_get_model(), when there is use it
    }
  else
    {
      g_object_unref (ev_document_model_get_document (model));  //FIXME check if this releases the file lock on windows.s
      ev_document_model_set_document (model, doc);
    }
  ev_document_model_set_dual_page (model, GPOINTER_TO_INT (g_object_get_data (G_OBJECT (Denemo.printarea), "Duplex")));
  get_wysiwig_info()->Mark.width = 0;            //indicate that there should no longer be any Mark placed on the score
}

static void
get_window_position (gint * x, gint * y)
{
  GtkAdjustment *adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printhscrollbar));
  *x = (gint) gtk_adjustment_get_value (adjust);
  adjust = gtk_range_get_adjustment (GTK_RANGE (Denemo.printvscrollbar));
  *y = gtk_adjustment_get_value (adjust);
}

//setting up Denemo.pixbuf so that parts of the pdf can be dragged etc.
static void
get_window_size (gint * w, gint * h)
{
  GdkWindow *window;
  if (!GTK_IS_LAYOUT (Denemo.printarea))
    window = gtk_widget_get_window (GTK_WIDGET (Denemo.printarea));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
  if (window)
    {
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      //      gdouble staffsize = atof(Denemo.gui->lilycontrol.staffsize->str);
      //      if(staffsize<1) staffsize = 20.0;
      //      scale *= (staffsize/4);//Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit       
#if GTK_MAJOR_VERSION==2
      gdk_drawable_get_size (window, w, h);
#else
      *w = gdk_window_get_width (window);
      *h = gdk_window_get_height (window);
#endif
      *w *= scale;
      *h *= scale;

    }
}

//setting up Denemo.pixbuf so that parts of the pdf can be dragged etc.
static void
set_denemo_pixbuf (gint x, gint y)
{
  GdkWindow *window;
  GdkPixbuf *pixbuf;
  if (!GTK_IS_LAYOUT (Denemo.printarea))
    window = gtk_widget_get_window (GTK_WIDGET (Denemo.printarea));
  else
    window = gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
  if (window)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      x -= xx;
      y -= yy;
#define GROB_SIZE 20            // a rough amount to drag grobs around recognizably
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      gint grob_size = GROB_SIZE * (staffsize / 20.0);
      x -= scale * grob_size / 2;
      y -= scale * grob_size / 2;
      if (x < 0)
        x = 0;
      if (y < 0)
        y = 0;
#if GTK_MAJOR_VERSION==2
      gint width, height;
      gdk_drawable_get_size (window, &width, &height);
      pixbuf = gdk_pixbuf_get_from_drawable (NULL, window, NULL /*gdk_colormap_get_system () */ ,
                                             (gint) (x), (gint) (y), 0, 0, scale * grob_size, scale * grob_size);
#else
      pixbuf = gdk_pixbuf_get_from_window (window, (gint) (x), (gint) (y), scale * grob_size, scale * grob_size);
#endif
      if (Denemo.pixbuf)
        g_object_unref (Denemo.pixbuf);
      Denemo.pixbuf = gdk_pixbuf_add_alpha (pixbuf, TRUE, 255, 255, 255);
      g_object_unref (pixbuf);
    }
}

//draw a circle to mark a dragging point
static void
place_spot (cairo_t * cr, gint x, gint y)
{
  cairo_move_to (cr, x, y);
  cairo_arc (cr, x, y, PRINTMARKER / 4, 0.0, 2 * M_PI);
  cairo_fill (cr);
}

//over-draw the evince widget with padding etc ...
static gboolean
overdraw_print (cairo_t * cr)
{
  gint x, y;

  get_window_position (&x, &y);

  // gint width, height;
//  width = gdk_pixbuf_get_width( GDK_PIXBUF(Denemo.pixbuf));
  // height = gdk_pixbuf_get_height( GDK_PIXBUF(Denemo.pixbuf));

  // cairo_scale( cr, Denemo.gui->si->preview_zoom, Denemo.gui->si->preview_zoom );
  cairo_translate (cr, -x, -y);
//  gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(Denemo.pixbuf), -x, -y);
  cairo_save (cr);

  if ((get_wysiwig_info()->Mark.width > 0.0) && (get_wysiwig_info()->stage != WaitingForDrag) && (get_wysiwig_info()->stage != DraggingNearEnd) && (get_wysiwig_info()->stage != DraggingFarEnd))
    {
      cairo_set_source_rgba (cr, 0.5, 0.5, 1.0, 0.5);
      cairo_rectangle (cr, get_wysiwig_info()->Mark.x - PRINTMARKER / 2, get_wysiwig_info()->Mark.y - PRINTMARKER / 2, PRINTMARKER, PRINTMARKER);
      cairo_fill (cr);
    }
  if (get_print_status()->invalid /*!print_is_valid */ )
    {
      gchar *headline, *explanation;
      switch (get_print_status()->invalid)
        {
        case 1:
          headline = _("Possibly Invalid");
          explanation = _("Cursor not moved.");
          break;
        case 2:
          headline = _("Check Score.");
          explanation = _("Cursor may have moved to error point in the score.");
          break;
        case 3:
          headline = _("INVALID!");
          explanation = _("LilyPond could not typeset this score.");
          break;
        }
      cairo_set_source_rgba (cr, 0.5, 0.0, 0.0, 0.4);
      cairo_set_font_size (cr, 48.0);
      cairo_move_to (cr, 50, 50);
      cairo_show_text (cr, headline);
      cairo_set_font_size (cr, 18.0);
      cairo_move_to (cr, 50, 80);
      cairo_show_text (cr, explanation);
    }
  if (get_print_status()->updating_id && (get_print_status()->background != STATE_NONE))
    {
      cairo_set_source_rgba (cr, 0.5, 0.0, 0.5, 0.3);
      cairo_set_font_size (cr, 64.0);
      cairo_move_to (cr, 0, 0);
      cairo_rotate (cr, M_PI / 4);
      cairo_move_to (cr, 200, 80);
      if (get_print_status()->typeset_type == TYPESET_MOVEMENT)
        cairo_show_text (cr, _("Current Movement"));
      else if (get_print_status()->typeset_type == TYPESET_EXCERPT)
        cairo_show_text (cr, _("Excerpt Only"));
    }

  cairo_restore (cr);

  if (get_wysiwig_info()->stage == SelectingFarEnd)
    {
      cairo_set_source_rgba (cr, 0.3, 0.3, 0.7, 0.9);
      //cairo_rectangle (cr, get_wysiwig_info()->near.x-PRINTMARKER/2, get_wysiwig_info()->near.y-PRINTMARKER/2, PRINTMARKER, PRINTMARKER );
      cairo_move_to (cr, get_wysiwig_info()->nearpoint.x, get_wysiwig_info()->nearpoint.y);
      cairo_arc (cr, get_wysiwig_info()->nearpoint.x, get_wysiwig_info()->nearpoint.y, 1.5, 0.0, 2 * M_PI);
      cairo_fill (cr);
    }
  if (get_wysiwig_info()->stage == WaitingForDrag)
    {
      cairo_set_source_rgba (cr, 0.3, 0.3, 0.7, 0.9);
      place_spot (cr, get_wysiwig_info()->farpoint.x, get_wysiwig_info()->farpoint.y);

      place_spot (cr, get_wysiwig_info()->nearpoint.x, get_wysiwig_info()->nearpoint.y);

    }
  if ((get_wysiwig_info()->stage == WaitingForDrag) || (get_wysiwig_info()->stage == DraggingNearEnd) || (get_wysiwig_info()->stage == DraggingFarEnd))
    {
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.7);
      cairo_move_to (cr, get_wysiwig_info()->nearpoint.x, get_wysiwig_info()->nearpoint.y);
      cairo_line_to (cr, get_wysiwig_info()->farpoint.x, get_wysiwig_info()->farpoint.y);
      cairo_stroke (cr);
      return TRUE;
    }

  if ((get_wysiwig_info()->stage == SelectingPoint) || (get_wysiwig_info()->stage == WaitingForCurveDrag) || (get_wysiwig_info()->stage == Dragging1) || (get_wysiwig_info()->stage == Dragging2) || (get_wysiwig_info()->stage == Dragging3) || (get_wysiwig_info()->stage == Dragging4))
    {
      //place_spot for all non-null points Curve.p1...
      if (get_wysiwig_info()->Curve.p1.x)
        {
          place_spot (cr, get_wysiwig_info()->Curve.p1.x, get_wysiwig_info()->Curve.p1.y);
        }
      if (get_wysiwig_info()->Curve.p2.x)
        {
          place_spot (cr, get_wysiwig_info()->Curve.p2.x, get_wysiwig_info()->Curve.p2.y);
        }
      if (get_wysiwig_info()->Curve.p1.x)
        {
          place_spot (cr, get_wysiwig_info()->Curve.p3.x, get_wysiwig_info()->Curve.p3.y);
        }

      if (get_wysiwig_info()->Curve.p4.x)
        {                       //all control points initialized
          place_spot (cr, get_wysiwig_info()->Curve.p4.x, get_wysiwig_info()->Curve.p4.y);

          cairo_set_source_rgba (cr, 0.5, 0.8, 0.0, 0.7);
          cairo_move_to (cr, get_wysiwig_info()->Curve.p1.x, get_wysiwig_info()->Curve.p1.y);
          cairo_curve_to (cr, get_wysiwig_info()->Curve.p2.x, get_wysiwig_info()->Curve.p2.y, get_wysiwig_info()->Curve.p3.x, get_wysiwig_info()->Curve.p3.y, get_wysiwig_info()->Curve.p4.x, get_wysiwig_info()->Curve.p4.y);
          cairo_stroke (cr);
        }
      return TRUE;
    }

  if (get_wysiwig_info()->stage == SelectingReference)
    {
      gint w, h;
      get_window_size (&w, &h);
      cairo_set_source_rgba (cr, 0.0, 0.0, 1.0, 0.7);
      cairo_move_to (cr, get_wysiwig_info()->curx, 0);
      cairo_line_to (cr, get_wysiwig_info()->curx, h);
      cairo_move_to (cr, 0, get_wysiwig_info()->cury);
      cairo_line_to (cr, w, get_wysiwig_info()->cury);
      cairo_stroke (cr);
    }
  if (get_wysiwig_info()->stage == Offsetting)
    {
      cairo_set_source_rgba (cr, 0.0, 0.0, 0.0, 0.7);
      cairo_move_to (cr, get_wysiwig_info()->Mark.x, get_wysiwig_info()->Mark.y);
      cairo_line_to (cr, get_wysiwig_info()->curx, get_wysiwig_info()->cury);
      cairo_stroke (cr);

      if (Denemo.pixbuf)
        {
          guint width = gdk_pixbuf_get_width (GDK_PIXBUF (Denemo.pixbuf));
          guint height = gdk_pixbuf_get_height (GDK_PIXBUF (Denemo.pixbuf));
          cairo_save (cr);
          gdk_cairo_set_source_pixbuf (cr, GDK_PIXBUF (Denemo.pixbuf), get_wysiwig_info()->curx - width / 2, get_wysiwig_info()->cury - height / 2);
          cairo_rectangle (cr, get_wysiwig_info()->curx - width / 2, get_wysiwig_info()->cury - height / 2, width, height);

          cairo_fill (cr);
          cairo_restore (cr);
        }
      else
        g_warning ("No pixbuf");
    }
  if (get_wysiwig_info()->stage == (unsigned int) Padding)
    {
      gint pad = ABS (get_wysiwig_info()->Mark.x - get_wysiwig_info()->curx);
      gint w = get_wysiwig_info()->nearpoint.x - get_wysiwig_info()->Mark.x;
      gint h = get_wysiwig_info()->nearpoint.y - get_wysiwig_info()->Mark.y;
      cairo_set_source_rgb (cr, 0.5, 0.5, 0.5);
      cairo_rectangle (cr, get_wysiwig_info()->Mark.x - pad / 2, get_wysiwig_info()->Mark.y - pad / 2, w + pad, h + pad);

      /*GdkWindow *window =*/ gtk_layout_get_bin_window (GTK_LAYOUT (Denemo.printarea));
      // gdk_draw_pixbuf(window, NULL, GDK_PIXBUF(Denemo.pixbuf),
      //    get_wysiwig_info()->Mark.x+x, get_wysiwig_info()->Mark.y+y, get_wysiwig_info()->Mark.x, get_wysiwig_info()->Mark.y,/* x, y in pixbuf, x,y in window */
      //    w,  h, GDK_RGB_DITHER_NONE,0,0);
    }
  return TRUE;
}

#if GTK_MAJOR_VERSION==3
static gint
printarea_draw_event (G_GNUC_UNUSED GtkWidget * w, cairo_t * cr)
{
  return overdraw_print (cr);
}
#else
static gint
printarea_draw_event (GtkWidget * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw_print (cr);
  cairo_destroy (cr);
  return TRUE;
}
#endif

static void
set_printarea (GError ** err)
{
  GFile *file;
  gchar *filename = get_print_status()->printname_pdf[get_print_status()->cycle];
  //g_print("using %s\n", filename);
  if (get_print_status()->invalid == 0)
    get_print_status()->invalid = (g_file_test (filename, G_FILE_TEST_EXISTS)) ? 0 : 3;
  file = g_file_new_for_commandline_arg (filename);
  //g_free(filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, err);
  //gint x = 0, y = 0, hupper, hlower, vupper, vlower;//store current position for reloading
  //get_window_position(&x, &y, &hupper, &hlower, &vupper, &vlower);
  if (*err)
    {
      g_warning ("Trying to read the pdf file %s gave an error: %s", uri, (*err)->message);
      get_print_status()->invalid = 3;
      gtk_widget_queue_draw (Denemo.printarea);
    }
  else
    set_printarea_doc (doc);
  static gboolean shown_once = FALSE;   //Make sure the user knows that the printarea is on screen
  if (!shown_once)
    {
      shown_once = TRUE;
      gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.printarea)));
    }
  return;
}

static void
printview_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, gboolean print)
{
  progressbar_stop ();
  g_spawn_close_pid (get_print_status()->printpid);
  //g_print("background %d\n", get_print_status()->background);
  if (get_print_status()->background == STATE_NONE)
    {
      call_out_to_guile ("(FinalizeTypesetting)");
      process_lilypond_errors ((gchar *) get_printfile_pathbasename ());
    }
  else
    {
      if (LilyPond_stderr != -1)
        close (LilyPond_stderr);
      LilyPond_stderr = -1;
    }
  get_print_status()->printpid = GPID_NONE;
  GError *err = NULL;
  set_printarea (&err);
  if (!err && print)
    libevince_print ();
  normal_cursor ();


}

static present_print_view_window(void) {
 GtkWidget *w = gtk_widget_get_toplevel (Denemo.printarea);
  if (gtk_widget_get_visible (w))
    gtk_window_present (GTK_WINDOW (w));
  else
    gtk_widget_show (w);
}
/* callback to print current part (staff) of score */
void
printpart_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  present_print_view_window();
  DenemoGUI *gui = Denemo.gui;
  if (gui->si->markstaffnum)
    if (confirm (_("A range of music is selected"), _("Print whole file?")))
      {
        gui->si->markstaffnum = 0;
      }
  if ((gui->movements && g_list_length (gui->movements) > 1) && (confirm (_("This piece has several movements"), _("Print this part from all of them?"))))
    create_pdf (TRUE, TRUE);
  else
    create_pdf (TRUE, FALSE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
                     
}


static gboolean
typeset (gboolean force)
{

  if ((force) || (changecount != Denemo.gui->changecount))
    {
      if (initialize_typesetting ())
        {
          g_warning ("InitializeTypesetting failed\n");
          return FALSE;
        }
      DenemoGUI *gui = Denemo.gui;
      gui->si->markstaffnum = 0;        //FIXME save and restore selection?    
      gui->lilycontrol.excerpt = FALSE;
      create_pdf (FALSE, TRUE);
      changecount = Denemo.gui->changecount;
      return TRUE;
    }
  return FALSE;
}

static gboolean
typeset_movement (gboolean force)
{

  if ((force) || (changecount != Denemo.gui->changecount))
    {
      if (initialize_typesetting ())
        {
          g_warning ("InitializeTypesetting failed\n");
          return FALSE;
        }
      DenemoGUI *gui = Denemo.gui;
      gui->si->markstaffnum = 0;        //FIXME save and restore selection?    
      gui->lilycontrol.excerpt = FALSE;
      create_pdf (FALSE, FALSE);
      return TRUE;
    }
  return FALSE;
}

void
printpreview_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  (void) typeset (TRUE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) print_finished, NULL);
}

void
refresh_print_view (G_GNUC_UNUSED gboolean interactive)
{
  busy_cursor ();
  if (typeset (FALSE))
    g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
  else
    normal_cursor ();
}

static void
print_from_print_view (gboolean all_movements)
{

  busy_cursor ();
  if (all_movements ? typeset (FALSE) : typeset_movement (FALSE))
    {
      g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
    }
  else
    {
      normal_cursor ();
      libevince_print ();       //printview_finished (get_print_status()->printpid, 0, TRUE);
    }
}

void
printselection_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  if (Denemo.gui->si->markstaffnum) {
    present_print_view_window();
    create_pdf (FALSE, FALSE);
    g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
  }
  else
    warningdialog (_("No selection to print"));
}

void
printexcerptpreview_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  if (!gui->si->markstaffnum)   //If no selection has been made 
    printrangedialog (gui);     //Launch a dialog to get selection
  if (gui->si->selection.firstmeasuremarked)
    {
      gui->lilycontrol.excerpt = TRUE;
      export_png ((gchar *) get_printfile_pathbasename (), (GChildWatchFunc) prepare_preview, gui);
      gui->lilycontrol.excerpt = FALSE;
    }
}



static gchar *
get_thumb_directory (void)
{
  return g_build_filename (g_get_home_dir (), ".thumbnails", "large", NULL);
}

static gchar *
get_thumb_printname (void)
{
  return g_build_filename (locateprintdir (), "denemothumb", NULL);
}

static gchar *
get_thumbname (gchar * uri)
{
  gchar *basethumbname = g_compute_checksum_for_string (G_CHECKSUM_MD5, uri, -1);
  gchar *thumbname = g_strconcat (basethumbname, ".png", NULL);
  g_free (basethumbname);
  return thumbname;
}

/*call back to finish thumbnail processing. */
static void
thumb_finished ()
{
  GError *err = NULL;
  g_spawn_close_pid (get_print_status()->printpid);
  get_print_status()->printpid = GPID_NONE;
  gchar *printname = get_thumb_printname ();
  gchar *printpng = g_strconcat (printname, ".png", NULL);
  GdkPixbuf *pbN = gdk_pixbuf_new_from_file_at_scale (printpng, 128, -1, TRUE, &err);
  if (err)
    {
      g_warning ("Thumbnail 128x128 file %s gave an error: %s", printpng, err->message);
      g_error_free (err);
      err = NULL;
    }
  GdkPixbuf *pbL = gdk_pixbuf_new_from_file_at_scale (printpng, 256, -1, TRUE, &err);
  if (err)
    {
      g_warning ("Thumbnail 256x256 file %s gave an error: %s", printpng, err->message);
      g_error_free (err);
      err = NULL;
    }
  //FIXME if pb->height>128 or 256 scale it down...
  if (pbN && pbL)
    {
      gchar *uri = g_strdup_printf ("file://%s", Denemo.gui->filename->str);
      gchar *thumbname = get_thumbname (uri);

      unsigned mtime = file_get_mtime (Denemo.gui->filename->str);
      //struct stat thebuf;
      //gint status =  g_stat(Denemo.gui->filename->str, &thebuf);
      // unsigned mtime = thebuf.st_mtime;
      //g_print("the mt is %u\n", mtime);



      gchar *thumbpathN = g_build_filename (thumbnailsdirN, thumbname, NULL);
      gchar *thumbpathL = g_build_filename (thumbnailsdirL, thumbname, NULL);

      gchar *mt = g_strdup_printf ("%u", mtime);
      if (!gdk_pixbuf_save (pbN, thumbpathN, "png" /*type */ , &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt, NULL))
        g_print ("%s\n", err->message);
      err = NULL;
      if (!gdk_pixbuf_save (pbL, thumbpathL, "png" /*type */ , &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt, NULL))
        g_print ("%s\n", err->message);

      //FIXME do the pbN L need freeing???
      g_free (uri);
      g_free (mt);
      g_free (thumbname);
      g_free (thumbpathN);
      g_free (thumbpathL);
    }
  g_free (printname);
  get_print_status()->printpid = GPID_NONE;
  progressbar_stop ();
  //g_print("Set get_print_status()->printpid = %d\n", get_print_status()->printpid);
}

// large_thumbnail_name takes a full path name to a .denemo file and returns the full path to the large thumbnail of that .denemo file. Caller must g_free the returned string
gchar *
large_thumbnail_name (gchar * filepath)
{
  gchar *temp = g_strdup_printf ("file://%s", filepath);
  gchar *ret = get_thumbname (temp);
  g_free (temp);
  return g_build_filename (get_thumb_directory (), ret, NULL);
}

/***
 *  Create a thumbnail for Denemo.gui if needed
 */
gboolean
create_thumbnail (gboolean async)
{
#ifdef G_OS_WIN32
  return FALSE;
#endif

  GError *err = NULL;
  if (get_print_status()->printpid != GPID_NONE)
    return FALSE;
  if (Denemo.gui->filename->len)
    {
      if (!thumbnailsdirN)
        {
          thumbnailsdirN = g_build_filename (g_get_home_dir (), ".thumbnails", "normal", NULL);
          g_mkdir_with_parents (thumbnailsdirN, 0700);
        }
      if (!thumbnailsdirL)
        {
          thumbnailsdirL = g_build_filename (g_get_home_dir (), ".thumbnails", "large", NULL);
          g_mkdir_with_parents (thumbnailsdirL, 0700);
        }
//check if thumbnail is newer than file
      struct stat thebuf;
      g_stat (Denemo.gui->filename->str, &thebuf);
      unsigned mtime = thebuf.st_mtime;
      gchar *uri = g_strdup_printf ("file://%s", Denemo.gui->filename->str);
      gchar *thumbname = get_thumbname (uri);
      gchar *thumbpathN = g_build_filename (thumbnailsdirN, thumbname, NULL);
      thebuf.st_mtime = 0;
      g_stat (thumbpathN, &thebuf);
      unsigned mtime_thumb = thebuf.st_mtime;
      if (mtime_thumb < mtime)
        {
          gint saved = g_list_index (Denemo.gui->movements, Denemo.gui->si);
          Denemo.gui->si = Denemo.gui->movements->data; //Thumbnail is from first movement
//set selection to thumbnailselection, if not set, to the selection, if not set to first three measures of staff 1
          if (Denemo.gui->thumbnail.firststaffmarked)
            memcpy (&Denemo.gui->si->selection, &Denemo.gui->thumbnail, sizeof (DenemoSelection));
          else if (Denemo.gui->si->selection.firststaffmarked)
            memcpy (&Denemo.gui->thumbnail, &Denemo.gui->si->selection, sizeof (DenemoSelection));
          else
            {
              Denemo.gui->thumbnail.firststaffmarked = 1;
              Denemo.gui->thumbnail.laststaffmarked = 3;
              Denemo.gui->thumbnail.firstmeasuremarked = 1;
              Denemo.gui->thumbnail.lastmeasuremarked = 3;
              Denemo.gui->thumbnail.firstobjmarked = 0;
              Denemo.gui->thumbnail.lastobjmarked = 100;        //or find out how many there are
              memcpy (&Denemo.gui->si->selection, &Denemo.gui->thumbnail, sizeof (DenemoSelection));
            }
          Denemo.gui->si->markstaffnum = Denemo.gui->si->selection.firststaffmarked;
          gchar *printname = get_thumb_printname ();
          Denemo.gui->lilycontrol.excerpt = TRUE;

          if (async)
            {
              gchar *arguments[] = {
                g_build_filename (get_bin_dir (), "denemo", NULL),
                "-n", "-a", "(d-CreateThumbnail #f)(d-Exit)",
                Denemo.gui->filename->str,
                NULL
              };

              g_spawn_async_with_pipes (NULL,   /* any dir */
                                        arguments, NULL,        /* env */
                                        G_SPAWN_SEARCH_PATH, NULL,      /* child setup func */
                                        NULL,   /* user data */
                                        NULL,   /* pid */
                                        NULL,   /* stdin */
                                        NULL,   /* stdout */
                                        NULL,   /* stderr */
                                        &err);
            }
          else
            {
              export_png (printname, NULL, Denemo.gui);
              thumb_finished ();
            }

          g_free (printname);
          Denemo.gui->si = g_list_nth_data (Denemo.gui->movements, saved);
          if (Denemo.gui->si == NULL)
            Denemo.gui->si = Denemo.gui->movements->data;
        }
    }
  return TRUE;
}

/* callback to print whole of score */
void
printall_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  print_from_print_view (TRUE);
}

/* callback to print movement of score */
void
printmovement_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  changecount = -1;
  print_from_print_view (FALSE);
  changecount = Denemo.gui->changecount;
}

//This gets an offset relative to get_wysiwig_info()->Mark which must be already setup on entry.
//A patch of the score around the target is dragged over the image with white showing as transparent and a line connects the original and new positions.
gboolean
get_offset (gdouble * offsetx, gdouble * offsety)
{
  get_wysiwig_info()->stage = Offsetting;
  gtk_main ();
  if (get_wysiwig_info()->stage == Offsetting)
    {
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      *offsetx = (get_wysiwig_info()->curx - get_wysiwig_info()->Mark.x) / scale; //Could/Should this better be get_wysiwig_info()->Reference????
      *offsety = -(get_wysiwig_info()->cury - get_wysiwig_info()->Mark.y) / scale;
      get_wysiwig_info()->stage = STAGE_NONE;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    return FALSE;
}


// start_seeking_end 
//if repeatable and grob is slur or beam and request matches gives prompt for slur or beam and goes to Waiting for drag
//else sets up near point to last button press and goes to selecting far end.
static void
start_seeking_end (gboolean slur)
{
  gchar *msg = (slur) ? _("Now select the notehead of the note where the slur ends") : _("Now select the notehead of the note where the beam ends");

  if (get_wysiwig_info()->repeatable && get_wysiwig_info()->grob == (slur ? Slur : Beam))
    {
      get_wysiwig_info()->stage = WaitingForDrag;
      msg = (get_wysiwig_info()->grob == Slur) ? _("Now drag the begin/end markers to suggest slur position/angle\nRight click when done.") : _("Now drag the begin/end markers to set position/angle of beam\nRight click when done."); //FIXME repeated text
    }
  else
    {
      get_wysiwig_info()->nearpoint = get_wysiwig_info()->near_i = get_wysiwig_info()->last_button_press;
      get_wysiwig_info()->stage = SelectingFarEnd;
    }
  if (get_wysiwig_info()->grob != (slur ? Slur : Beam))
    get_wysiwig_info()->repeatable = FALSE;
  get_wysiwig_info()->grob = slur ? Slur : Beam;
  gtk_widget_show (get_wysiwig_info()->dialog);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwig_info()->dialog), msg);
  gtk_widget_queue_draw (Denemo.printarea);
}

static gdouble
get_center_staff_offset (void)
{
  gdouble yadjust = 0.0;
  if (Denemo.gui->si->currentobject)
    {
      DenemoObject *obj = (DenemoObject *) Denemo.gui->si->currentobject->data;
      if (obj->type == CHORD)
        {
          chord *thechord = (chord *) obj->object;
          beamandstemdirhelper (Denemo.gui->si);
          if (thechord->notes)
            {
              note *thenote = (note *) (thechord->notes->data);
              gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
              if (staffsize < 1)
                staffsize = 20.0;
              yadjust = -(4 - thenote->y / 5) * staffsize / 8;
              EvDocumentModel *model;
              model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
              gdouble scale = ev_document_model_get_scale (model);
              yadjust *= scale;
            }
        }
    }
  return yadjust;
}

// get_postions gets two y-heights interactively, giving prompts either for slur or beam
// 

gboolean
get_positions (gdouble * neary, gdouble * fary, gboolean for_slur)
{
  get_wysiwig_info()->task = Positions;
  start_seeking_end (for_slur); //goes to WaitingForDrag
  gtk_main ();
  if (get_wysiwig_info()->stage == WaitingForDrag)
    {
      EvDocumentModel *model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");        //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwig_info()->pos.staff, get_wysiwig_info()->pos.measure, get_wysiwig_info()->pos.object);  //the cursor to the slur-begin note.
      gdouble nearadjust = get_center_staff_offset ();

      *neary = -(get_wysiwig_info()->nearpoint.y - get_wysiwig_info()->near_i.y + nearadjust) / scale;
      *fary = -(get_wysiwig_info()->farpoint.y - get_wysiwig_info()->near_i.y + nearadjust) / scale;   //sic! the value of far_i.y is irrelevant
      get_wysiwig_info()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwig_info()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}

gboolean
get_curve (gdouble * x1, gdouble * y1, gdouble * x2, gdouble * y2, gdouble * x3, gdouble * y3, gdouble * x4, gdouble * y4)
{
  //FIXME check for stage, to avoid re-entering
  get_wysiwig_info()->task = Shape;
  get_wysiwig_info()->stage = WaitingForCurveDrag;
  gtk_main ();
  if (get_wysiwig_info()->stage == WaitingForCurveDrag)
    {
      EvDocumentModel *model = g_object_get_data (G_OBJECT (Denemo.printarea), "model");        //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwig_info()->pos.staff, get_wysiwig_info()->pos.measure, get_wysiwig_info()->pos.object);  //the cursor to the slur-begin note.
      //!!! is pos set up?
      g_print ("Reference is %f %f %d %d\n", get_wysiwig_info()->Reference.x, get_wysiwig_info()->Reference.y, get_wysiwig_info()->Curve.p4.x, get_wysiwig_info()->Curve.p4.y);
      *x1 = (get_wysiwig_info()->Curve.p1.x - get_wysiwig_info()->Reference.x) / scale;
      *y1 = -(get_wysiwig_info()->Curve.p1.y - get_wysiwig_info()->Reference.y) / scale;

      *x2 = (get_wysiwig_info()->Curve.p2.x - get_wysiwig_info()->Reference.x) / scale;
      *y2 = -(get_wysiwig_info()->Curve.p2.y - get_wysiwig_info()->Reference.y) / scale;
      *x3 = (get_wysiwig_info()->Curve.p3.x - get_wysiwig_info()->Reference.x) / scale;
      *y3 = -(get_wysiwig_info()->Curve.p3.y - get_wysiwig_info()->Reference.y) / scale;
      *x4 = (get_wysiwig_info()->Curve.p4.x - get_wysiwig_info()->Reference.x) / scale;
      *y4 = -(get_wysiwig_info()->Curve.p4.y - get_wysiwig_info()->Reference.y) / scale;


      get_wysiwig_info()->repeatable = TRUE;

      get_wysiwig_info()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwig_info()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else
    {
      return FALSE;
    }
}


//Gets a new value into get_wysiwig_info()->Mark.x,y and changes to SelectingFarEnd
gboolean
get_new_target (void)
{
  get_wysiwig_info()->stage = SelectingNearEnd;
  g_print ("Starting main");
  gtk_main ();
  if (get_wysiwig_info()->stage == SelectingNearEnd)     //should have changed, but user cancelled
    return FALSE;
  else
    return TRUE;
}

//Gets a new value into get_wysiwig_info()->Mark.x,y and changes to STAGE_NONE
gboolean
get_new_point (void)
{
  get_wysiwig_info()->stage = SelectingPoint;
  g_print ("Starting main");
  gtk_main ();
  if (get_wysiwig_info()->stage == SelectingPoint)       //should have changed, but user cancelled
    return FALSE;
  else
    return TRUE;
}


gboolean
get_reference_point (void)
{
  get_wysiwig_info()->stage = SelectingReference;
  memset (&get_wysiwig_info()->Curve, 0, sizeof (Curve));
  gtk_main ();
  if (get_wysiwig_info()->stage == SelectingReference)
    {                           //should have changed, but the user cancelled
      return FALSE;
    }
  else
    {
      get_wysiwig_info()->Reference = get_wysiwig_info()->Mark;
      return TRUE;
    }
}

gboolean
get_control_point (gint which)
{
  gboolean ret = TRUE;
  if (get_new_point ())
    {                           //FIXME ... instead make purpose of get_new_target() the argument to it, and use that in the call
      switch (which)
        {
        case 1:
          get_wysiwig_info()->Curve.p1.x = get_wysiwig_info()->Mark.x;
          get_wysiwig_info()->Curve.p1.y = get_wysiwig_info()->Mark.y;
          break;
        case 2:
          get_wysiwig_info()->Curve.p2.x = get_wysiwig_info()->Mark.x;
          get_wysiwig_info()->Curve.p2.y = get_wysiwig_info()->Mark.y;
          break;
        case 3:
          get_wysiwig_info()->Curve.p3.x = get_wysiwig_info()->Mark.x;
          get_wysiwig_info()->Curve.p3.y = get_wysiwig_info()->Mark.y;
          break;
        case 4:
          get_wysiwig_info()->Curve.p4.x = get_wysiwig_info()->Mark.x;
          get_wysiwig_info()->Curve.p4.y = get_wysiwig_info()->Mark.y;
          break;
        default:
          g_warning ("Wrong call to get_control_point, no point %d possible", which);
          ret = FALSE;
          break;
        }

    }
  else
    ret = FALSE;
  gtk_widget_queue_draw (Denemo.printarea);
  get_wysiwig_info()->stage = (ret ? WaitingForCurveDrag : STAGE_NONE);
  return ret;
}

/*UNUSED
static gint
start_stage (GtkWidget * widget, WwStage stage)
{
  get_wysiwig_info()->stage = stage;
  return TRUE;
}*/

static void
create_all_pdf (void)
{
  busy_cursor ();
  create_pdf (FALSE, TRUE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
create_full_score_pdf (void)
{
  busy_cursor ();
  create_default_scoreblock ();
  create_pdf (FALSE, TRUE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
copy_pdf (void)
{
  //copy file get_print_status()->printname_pdf[get_print_status()->cycle] to user pdf name 
  //use get_output_uri_from_scoreblock() as default name.
  //use a gtk_file_chooser like this:
  gchar *filename;
  gchar *outuri = get_output_uri_from_scoreblock ();
  gchar *outpath;
  gchar *outname;
  outuri += strlen ("file://"); //skip the uri bit of it
  outpath = g_path_get_dirname (outuri);
  outname = g_path_get_basename (outuri);
  GtkWidget *chooser = gtk_file_chooser_dialog_new (_("PDF creation"),
                                                    GTK_WINDOW (Denemo.window),
                                                    GTK_FILE_CHOOSER_ACTION_SAVE,
                                                    GTK_STOCK_CANCEL,
                                                    GTK_RESPONSE_REJECT,
                                                    GTK_STOCK_SAVE,
                                                    GTK_RESPONSE_ACCEPT, NULL);
  gtk_file_chooser_set_current_folder (GTK_FILE_CHOOSER (chooser), outpath);
  gtk_file_chooser_set_current_name (GTK_FILE_CHOOSER (chooser), outname);
  gtk_widget_show_all (chooser);
  if (gtk_dialog_run (GTK_DIALOG (chooser)) == GTK_RESPONSE_ACCEPT)
    filename = gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (chooser));
  else
    filename = NULL;
  gtk_widget_destroy (chooser);

  if (filename)
    {
      gchar *contents;
      gsize length;
      if (g_file_get_contents (get_print_status()->printname_pdf[get_print_status()->cycle], &contents, &length, NULL))
        {
          if (!g_file_set_contents (filename, contents, length, NULL))
            {
              gchar *msg = g_strdup_printf (_("Errno %d:\nCould not copy %s to %s. Perhaps because some other process is using the destination file. Try again with a new location\n"),
                                            errno,
                                            get_print_status()->printname_pdf[get_print_status()->cycle],
                                            filename);
              warningdialog (msg);
              g_free (msg);
            }
          else
            {
              g_print ("I have copied %s to %s (default was %s)\n", get_print_status()->printname_pdf[get_print_status()->cycle], filename, outname);
            }
          g_free (contents);
        }
      g_free (outpath);
      g_free (outname);
      g_free (filename);
    }

}

static void
create_movement_pdf (void)
{

  busy_cursor ();
  create_pdf (FALSE, FALSE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static void
create_part_pdf (void)
{

  busy_cursor ();
  create_pdf (TRUE, TRUE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
}

static gint
popup_object_edit_menu (void)
{
  call_out_to_guile ("(EditTarget)");
  return TRUE;
}

/*UNUSED
static gboolean
same_position (DenemoPosition * pos1, DenemoPosition * pos2)
{
  return pos1->movement == pos2->movement && pos1->staff == pos2->staff && pos1->measure == pos2->measure && pos1->object == pos2->object;
}*/

static gboolean
same_target (DenemoTarget * pos1, DenemoTarget * pos2)
{
  return pos1->type == pos2->type && pos1->objnum == pos2->objnum && pos1->measurenum == pos2->measurenum && pos1->staffnum == pos2->staffnum && pos1->mid_c_offset == pos2->mid_c_offset && pos1->directivenum == pos2->directivenum;
}

static gint
action_for_link (G_GNUC_UNUSED EvView * view, EvLinkAction * obj)
{
#ifdef G_OS_WIN32
  g_print ("Signal from evince widget received %d %d\n", get_wysiwig_info()->grob, get_wysiwig_info()->stage);
#endif
  //g_print("Link action Mark at %f, %f\n", get_wysiwig_info()->Mark.x, get_wysiwig_info()->Mark.y);
  gchar *uri = (gchar *) ev_link_action_get_uri (obj);
  //g_print("Stage %d\n", get_wysiwig_info()->stage);
  if ((get_wysiwig_info()->stage == SelectingPoint) || (get_wysiwig_info()->stage == Dragging1) || (get_wysiwig_info()->stage == Dragging2) || (get_wysiwig_info()->stage == Dragging3) || (get_wysiwig_info()->stage == Dragging4))
    return TRUE;
  if ((get_wysiwig_info()->stage == WaitingForDrag) || (get_wysiwig_info()->grob == Slur && (get_wysiwig_info()->stage == SelectingFarEnd)))
    {
      return TRUE;
    }
  if (get_wysiwig_info()->stage == WaitingForCurveDrag || (get_wysiwig_info()->stage == SelectingReference))
    return TRUE;

  if (get_wysiwig_info()->stage == Offsetting)
    {
      return TRUE;              //?Better take over motion notify so as not to get this while working ...
    }
#ifdef G_OS_WIN32
  g_print ("action_for_link: uri %s\n", uri);
#endif
  //g_print("acting on external signal %s type=%d directivenum=%d\n", uri, Denemo.gui->si->target.type, Denemo.gui->si->target.directivenum);
  if (uri)
    {
      gchar **orig_vec = g_strsplit (uri, ":", 6);
      gchar **vec = orig_vec;
      if (vec[0] && vec[1] && vec[2] && vec[3] && vec[4] && vec[5] && *vec[5])
        vec++;
      if (g_str_has_prefix (uri, "textedit:") && vec[1] && vec[2] && vec[3])
        {
          DenemoTarget old_target = Denemo.gui->si->target;
          get_wysiwig_info()->ObjectLocated = goto_lilypond_position (atoi (vec[2]), atoi (vec[3]));     //sets si->target
#ifdef G_OS_WIN32
          g_print ("action_for_link: object located %d\n", get_wysiwig_info()->ObjectLocated);
#endif
          if (get_wysiwig_info()->ObjectLocated)
            {
              if (!(get_wysiwig_info()->grob == Beam && (get_wysiwig_info()->stage == SelectingFarEnd)))
                {
                  get_position (Denemo.gui->si, &get_wysiwig_info()->pos);
                  get_wysiwig_info()->repeatable = same_target (&old_target, &Denemo.gui->si->target);
                }
              else
                Denemo.gui->si->target = old_target;    //undo the change of target when getting the end of beam note
            }
          else
            get_wysiwig_info()->repeatable = FALSE;
          //g_print("Target type %d\n", Denemo.gui->si->target.type); 

          if ((get_wysiwig_info()->stage == SelectingNearEnd))
            return TRUE;

          if (get_wysiwig_info()->ObjectLocated && Denemo.gui->si->currentobject)
            {
              DenemoDirective *directive = NULL;
              DenemoObject *obj = (DenemoObject *) Denemo.gui->si->currentobject->data;
              if (obj->type == LILYDIRECTIVE)
                {
                  directive = ((lilydirective *) obj->object);
                }
              else
                switch (Denemo.gui->si->target.type)
                  {
                  case TARGET_NONE:
                    break;
                  case TARGET_NOTE:
                    if (Denemo.gui->si->target.directivenum)
                      {
                        if (Denemo.gui->si->target.type == TARGET_NOTE)
                          {
                            directive = get_note_directive_number (Denemo.gui->si->target.directivenum);
                          }
                      }
                    break;
                  case TARGET_CHORD:
                    g_print ("Chord directives may be not done");
                    if (Denemo.gui->si->target.directivenum)
                      {
                        //directive = get_chord_directive_number(Denemo.gui->si->target.directivenum);
                        if (obj->type == CHORD)
                          {
                            chord *thechord = (chord *) obj->object;
                            directive = (DenemoDirective *) g_list_nth_data (thechord->directives, Denemo.gui->si->target.directivenum - 1);
                            if (directive && directive->tag)
                              {
                                g_print ("Found %s\n", directive->tag->str);
                                //This is things like ToggleTrill ToggleCoda which require different offsets to their center
                                get_wysiwig_info()->grob = Articulation;
                              }

                          }
                      }

                    break;
                  case TARGET_SLUR:
                    //g_print("taking action on slur...");
                    if (get_wysiwig_info()->repeatable && get_wysiwig_info()->task == Positions)
                      {
                        if (confirm (_("Slur Angle/Position"), _("Repeat Slur Positioning Hint?")))
                          {
                            get_wysiwig_info()->stage = WaitingForDrag;
                            gtk_widget_queue_draw (Denemo.printarea);
                            call_out_to_guile ("(GetSlurPositions)");
                          }
                        else
                          get_wysiwig_info()->task = TASK_NONE;
                      }
                    else if (get_wysiwig_info()->stage == STAGE_NONE && get_wysiwig_info()->repeatable && get_wysiwig_info()->task == Shape)
                      {
                        if (confirm (_("Slur Shape"), _("Repeat Shaping Slur?")))
                          {
                            get_wysiwig_info()->stage = WaitingForCurveDrag;
                            gtk_widget_queue_draw (Denemo.printarea);
                            call_out_to_guile ("(ReshapeSlur)");
                          }
                        else
                          get_wysiwig_info()->task = TASK_NONE;
                      }
                    else
                      {
                        get_wysiwig_info()->stage = TargetEstablished;
                        get_wysiwig_info()->repeatable = FALSE;
                      }
                    break;
                  case TARGET_TIE:
                    g_warning ("Not yet done!!");
                    break;
                  default:
                    g_warning ("Target type %d not yet done!!", Denemo.gui->si->target.type);
                    break;
                  }
            }



        }
      else if (g_str_has_prefix (uri, "http:"))
        {
          gchar *text = g_strdup_printf ("(d-Help \"%s\")", uri);
          call_out_to_guile (text);
          g_free (text);
        }
      else if (g_str_has_prefix (uri, "scheme:"))
        {
          gchar *text = uri + strlen ("scheme:");
          if (*text)
            call_out_to_guile (text);
          else
            g_warning ("No script given after scheme:");
        }
      else
        {
          g_warning ("Cannot follow link type %s\n", orig_vec[0]);
        }
      g_strfreev (orig_vec);
    }
  //!!!! do we want to set_denemo_pixbuf() here if the object is located ???? that is what we are going to drag ....
  g_print ("Have get_wysiwig_info()->ObjectLocated (%.2f, %.2f) (%.2f, %.2f)\n", get_wysiwig_info()->Mark.x, get_wysiwig_info()->Mark.y, get_wysiwig_info()->curx, get_wysiwig_info()->cury);
  set_denemo_pixbuf ((gint) get_wysiwig_info()->curx, (gint) get_wysiwig_info()->cury);
  return TRUE;                  //we do not want the evince widget to handle this.
}

static gboolean
in_selected_object (gint x, gint y)
{
  gint xx, yy;
  //g_print("reading position of mark");
  get_window_position (&xx, &yy);
  x += (xx + PRINTMARKER / 2);
  y += (yy + PRINTMARKER / 2);
  return (x > get_wysiwig_info()->Mark.x && y > get_wysiwig_info()->Mark.y && x < (get_wysiwig_info()->Mark.x + get_wysiwig_info()->Mark.width) && y < (get_wysiwig_info()->Mark.y + get_wysiwig_info()->Mark.height));
}


static gboolean
is_near (gint x, gint y, WwPoint p)
{
  gint xx, yy;
  get_window_position (&xx, &yy);
  x += (xx + PRINTMARKER / 2);
  y += (yy + PRINTMARKER / 2);
  return (ABS (x - p.x) < PRINTMARKER) && (ABS (y - p.y) < PRINTMARKER);
}

static gboolean
printarea_motion_notify (G_GNUC_UNUSED GtkWidget * widget, GdkEventMotion * event)
{
  get_wysiwig_info()->ObjectLocated = FALSE;

  if (get_wysiwig_info()->stage == WaitingForDrag)
    {
      if ((is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->farpoint)) || (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->nearpoint)))
        {
          gtk_widget_queue_draw (Denemo.printarea);
        }
      return TRUE;
    }

  if (get_wysiwig_info()->stage == DraggingNearEnd)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      // get_wysiwig_info()->near.x = xx + (gint)event->x;
      get_wysiwig_info()->nearpoint.y = yy + (gint) event->y; //g_print("near y becomes %d\n", get_wysiwig_info()->near.y);
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == DraggingFarEnd)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      // get_wysiwig_info()->far.x = xx + (gint)event->x;
      get_wysiwig_info()->farpoint.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == Dragging1)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwig_info()->Curve.p1.x = xx + (gint) event->x;
      get_wysiwig_info()->Curve.p1.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == Dragging2)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwig_info()->Curve.p2.x = xx + (gint) event->x;
      get_wysiwig_info()->Curve.p2.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == Dragging3)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwig_info()->Curve.p3.x = xx + (gint) event->x;
      get_wysiwig_info()->Curve.p3.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == Dragging4)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwig_info()->Curve.p4.x = xx + (gint) event->x;
      get_wysiwig_info()->Curve.p4.y = yy + (gint) event->y;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }





  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwig_info()->curx = xx + (gint) event->x;
  get_wysiwig_info()->cury = yy + (gint) event->y;


  if ((get_wysiwig_info()->stage == Offsetting) || (get_wysiwig_info()->stage == SelectingReference))
    {
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (in_selected_object ((int) event->x, (int) event->x))
    {
      return TRUE;              //we have handled this.
    }
  return FALSE;                 //propagate further
}



/* UNUSED
static void
normalize (void)
{
  if (get_wysiwig_info()->near.x < get_wysiwig_info()->Mark.x)
    {
      gdouble temp = get_wysiwig_info()->near.x;
      get_wysiwig_info()->near.x = get_wysiwig_info()->Mark.x;
      get_wysiwig_info()->Mark.x = temp;
    }
  if (get_wysiwig_info()->near.y < get_wysiwig_info()->Mark.y)
    {
      gdouble temp = get_wysiwig_info()->near.y;
      get_wysiwig_info()->near.y = get_wysiwig_info()->Mark.y;
      get_wysiwig_info()->Mark.y = temp;
    }
  if (get_wysiwig_info()->Mark.x == get_wysiwig_info()->near.x)
    get_wysiwig_info()->near.x++;
  if (get_wysiwig_info()->Mark.y == get_wysiwig_info()->near.y)
    get_wysiwig_info()->near.y++;

}
*/

static void
apply_tweak (void)
{
  //g_print("Apply tweak Quitting with %d %d", get_wysiwig_info()->stage, get_wysiwig_info()->grob);
  gtk_main_quit ();
  return;
  if (get_wysiwig_info()->stage == Offsetting)
    {
      gtk_main_quit ();
    }
  else
    {
      normal_cursor ();
      EvDocumentModel *model;
      model = g_object_get_data (G_OBJECT (Denemo.printarea), "model"); //there is no ev_view_get_model(), when there is use it
      gdouble scale = ev_document_model_get_scale (model);
      gdouble staffsize = atof (Denemo.gui->lilycontrol.staffsize->str);
      if (staffsize < 1)
        staffsize = 20.0;
      scale *= (staffsize / 4); //Trial and error value scaling evinces pdf display to the LilyPond staff-line-spaces unit
      goto_movement_staff_obj (NULL, -1, get_wysiwig_info()->pos.staff, get_wysiwig_info()->pos.measure, get_wysiwig_info()->pos.object);  //the cursor to the slur-begin note.
      gdouble nearadjust = get_center_staff_offset ();

      gdouble neary = -(get_wysiwig_info()->nearpoint.y - get_wysiwig_info()->near_i.y + nearadjust) / scale;
      gdouble fary = -(get_wysiwig_info()->farpoint.y - get_wysiwig_info()->near_i.y + nearadjust) / scale;    //sic! the value of far_i.y is irrelevant
      //g_print("near %d %d far %d %d\n", get_wysiwig_info()->near.y, get_wysiwig_info()->near_i.y, get_wysiwig_info()->far.y, get_wysiwig_info()->far_i.y);
      gchar *script = (get_wysiwig_info()->grob == Slur) ? g_strdup_printf ("(SetSlurPositions \"%.1f\" \"%.1f\")", neary, fary) : g_strdup_printf ("(SetBeamPositions \"%.1f\" \"%.1f\")", neary, fary);
      //Move back to the correct place in the score
      goto_movement_staff_obj (NULL, -1, get_wysiwig_info()->pos.staff, get_wysiwig_info()->pos.measure, get_wysiwig_info()->pos.object);
      call_out_to_guile (script);
      g_free (script);
      get_wysiwig_info()->stage = STAGE_NONE;
      gtk_widget_hide (get_wysiwig_info()->dialog);
      gtk_widget_queue_draw (Denemo.printarea);
    }

}

static void
cancel_tweak (void)
{
  //gtk_widget_set_tooltip_markup(gtk_widget_get_parent(Denemo.printarea), standard_tooltip);
  gtk_widget_set_tooltip_markup (gtk_widget_get_parent (Denemo.printarea), NULL);
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwig_info()->dialog), _("Operation Cancelled"));
  gtk_widget_show (get_wysiwig_info()->dialog);
  get_wysiwig_info()->stage = STAGE_NONE;
  gtk_widget_queue_draw (Denemo.printarea);
  gtk_main_quit ();
}

static void
repeat_tweak (void)
{
  if (get_wysiwig_info()->grob == Slur)          //if(get_wysiwig_info()->repeatable && get_wysiwig_info()->grob==(slur?Slur:Beam))
    //call_out_to_guile("(GetSlurPositions)");
    call_out_to_guile ("(EditSlur)");
  else if (get_wysiwig_info()->grob == Beam)     //if(get_wysiwig_info()->repeatable && get_wysiwig_info()->grob==(slur?Slur:Beam))
    call_out_to_guile ("(GetBeamPositions)");
  else
    warningdialog (_("Do not know what to repeat"));
}

static void
set_score_size (void)
{
  call_out_to_guile ("(d-SetFontSize)");
}

static void
help_tweak (void)
{
  gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwig_info()->dialog), _("To tweak the positions of objects (and more) move the mouse until the hand pointer appears\nClick on the object and follow the prompts.\nFor beams, click on the notehead of the note where the beam starts."));
  gtk_widget_show (get_wysiwig_info()->dialog);
}

static void
toggle_lilypond_structure_markers (void)
{
  call_out_to_guile ("(d-ToggleWysiwygMarks)");
  call_out_to_guile ("(d-ToggleCurveControl)");
}

static gint
popup_tweak_menu (void)
{
  GtkWidget *menu = gtk_menu_new ();
  GtkWidget *item;
  if (get_wysiwig_info()->stage == WaitingForDrag || get_wysiwig_info()->stage == WaitingForCurveDrag || get_wysiwig_info()->stage == Offsetting)
    {
      item = gtk_menu_item_new_with_label (_("Apply"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect_swapped (G_OBJECT (item), "activate", G_CALLBACK (apply_tweak), NULL);
      item = gtk_menu_item_new_with_label (_("Cancel"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (cancel_tweak), NULL);
    }


  if (get_wysiwig_info()->stage == STAGE_NONE)
    {
      item = gtk_menu_item_new_with_label (_("Help for Tweaks"));
      gtk_widget_set_tooltip_markup (item, _("This window can be used to tweak the typesetting that LilyPond does in the case that it is not optimal"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (help_tweak), NULL);

      item = gtk_menu_item_new_with_label (_("Red dots and crosses (Off/On)"));
      gtk_widget_set_tooltip_markup (item, _("The exact positions of the graphical components of the score will be labelled with red dots\n" "and the control points for curves with red crosses for accurate tweaks\nTurn these off before printing!"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (toggle_lilypond_structure_markers), NULL);

      item = gtk_menu_item_new_with_label (_("Score Size"));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (set_score_size), NULL);

      if (get_wysiwig_info()->repeatable)
        {                       //never true 
          item = gtk_menu_item_new_with_label (_("Repeat"));
          gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
          g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (repeat_tweak), NULL);
        }
    }



  gtk_widget_show_all (menu);

  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
  return TRUE;
}



static gint
printarea_button_press (G_GNUC_UNUSED GtkWidget * widget, GdkEventButton * event)
{
  //DenemoTargetType type = Denemo.gui->si->target.type;
  gboolean left = (event->button == 1);
  gboolean right = !left;
  //g_print("Button press %d, %d %d\n",(int)event->x , (int)event->y, left);
  get_wysiwig_info()->button = event->button;
  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwig_info()->last_button_press.x = xx + event->x;
  get_wysiwig_info()->last_button_press.y = yy + event->y;
  gboolean hotspot = is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->nearpoint) || (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->farpoint));
  //g_print("stage %d hotspot %d", get_wysiwig_info()->stage, hotspot);
  if (left && (get_wysiwig_info()->stage == WaitingForDrag) && !hotspot)
    {
      popup_tweak_menu ();      //other stages STAGE_NONE for example. And make the offer of Repeat if appropriate...
      return TRUE;
    }

  if (get_wysiwig_info()->stage == WaitingForCurveDrag)
    {
      if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->Curve.p1))
        {
          get_wysiwig_info()->stage = Dragging1; //gtk_widget_queue_draw (Denemo.printarea);
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->Curve.p2))
        {
          get_wysiwig_info()->stage = Dragging2;
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->Curve.p3))
        {
          get_wysiwig_info()->stage = Dragging3;
          return TRUE;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->Curve.p4))
        {
          get_wysiwig_info()->stage = Dragging4;
          return TRUE;
        }
      popup_tweak_menu ();
      return TRUE;
    }
  if (right && get_wysiwig_info()->stage == WaitingForDrag && !hotspot)
    {
      apply_tweak ();
    }
  if ((get_wysiwig_info()->stage == SelectingNearEnd) || (get_wysiwig_info()->stage == SelectingReference))
    {
      get_wysiwig_info()->near_i = get_wysiwig_info()->nearpoint = get_wysiwig_info()->last_button_press;       //struct copy
      return TRUE;
    }
  if (get_wysiwig_info()->stage == SelectingPoint)
    {                           //handle on release as user may move before releasing
      return TRUE;
    }

  if (get_wysiwig_info()->stage == SelectingFarEnd)
    {                           //handle on release, after cursor has moved to note
      return TRUE;
    }

  if (get_wysiwig_info()->stage == WaitingForDrag)
    {
      if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->nearpoint))
        {
          get_wysiwig_info()->stage = DraggingNearEnd;
        }
      else if (is_near ((gint) event->x, (gint) event->y, get_wysiwig_info()->farpoint))
        {
          get_wysiwig_info()->stage = DraggingFarEnd;
        }
      //???text dialog
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }




  if (in_selected_object ((gint) event->x, (gint) event->y))
    {
      //g_print("Popping up menu");
      popup_object_edit_menu ();
      return TRUE;
    }

  if (get_wysiwig_info()->stage != Offsetting)
    {
      gint xx, yy;
      get_window_position (&xx, &yy);
      get_wysiwig_info()->curx = xx + event->x;
      get_wysiwig_info()->cury = yy + event->y;
    }
  return TRUE;
}

static gint
printarea_button_release (G_GNUC_UNUSED GtkWidget * widget, GdkEventButton * event)
{
//g_print("stage %d\n", get_wysiwig_info()->stage);
  gboolean left = (event->button == 1);
  gboolean right = !left;
  gboolean object_located_on_entry = get_wysiwig_info()->ObjectLocated;
  gint xx, yy;
  get_window_position (&xx, &yy);
  get_wysiwig_info()->last_button_release.x = xx + event->x;
  get_wysiwig_info()->last_button_release.y = yy + event->y;
  if (left && get_wysiwig_info()->ObjectLocated)
    gtk_window_present (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.scorearea)));
  //g_print("Button release %d, %d\n",(int)event->x , (int)event->y);

  if (get_wysiwig_info()->stage == Dragging1)
    {
      get_wysiwig_info()->Curve.p1.x = get_wysiwig_info()->last_button_release.x;
      get_wysiwig_info()->Curve.p1.y = get_wysiwig_info()->last_button_release.y;
      get_wysiwig_info()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwig_info()->stage == Dragging2)
    {
      get_wysiwig_info()->Curve.p2.x = get_wysiwig_info()->last_button_release.x;
      get_wysiwig_info()->Curve.p2.y = get_wysiwig_info()->last_button_release.y;
      get_wysiwig_info()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwig_info()->stage == Dragging3)
    {
      get_wysiwig_info()->Curve.p3.x = get_wysiwig_info()->last_button_release.x;
      get_wysiwig_info()->Curve.p3.y = get_wysiwig_info()->last_button_release.y;
      get_wysiwig_info()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }
  else if (get_wysiwig_info()->stage == Dragging4)
    {
      get_wysiwig_info()->Curve.p4.x = get_wysiwig_info()->last_button_release.x;
      get_wysiwig_info()->Curve.p4.y = get_wysiwig_info()->last_button_release.y;
      get_wysiwig_info()->stage = WaitingForCurveDrag;
      gtk_widget_queue_draw (Denemo.printarea);
      return TRUE;
    }

  if (get_wysiwig_info()->stage == WaitingForCurveDrag)
    {
      g_print ("End of curve drag - should give menu if right click\n");
      g_print ("Check level > 1  %d", gtk_main_level ());
      gtk_main_quit ();
      return TRUE;
    }


  if (get_wysiwig_info()->ObjectLocated || (get_wysiwig_info()->stage == SelectingNearEnd) || (get_wysiwig_info()->stage == SelectingReference))
    {
      get_wysiwig_info()->Mark.width = get_wysiwig_info()->Mark.height = PRINTMARKER;
      gtk_widget_queue_draw (Denemo.printarea);
      get_wysiwig_info()->Mark.x = event->x + xx;
      get_wysiwig_info()->Mark.y = event->y + yy;
      // switch_back_to_main_window();
      get_wysiwig_info()->ObjectLocated = FALSE;
    }

  if ( /* left && */ get_wysiwig_info()->stage == TargetEstablished)
    {
      if (Denemo.gui->si->target.type == TARGET_SLUR)
        {
          get_wysiwig_info()->grob = Slur;
          call_out_to_guile ("(EditSlur)");
          get_wysiwig_info()->stage = STAGE_NONE;
          return TRUE;
        }
    }
  if (get_wysiwig_info()->stage == SelectingNearEnd)
    {
      get_wysiwig_info()->stage = SelectingFarEnd;
      gtk_main_quit ();
      return TRUE;
    }

  if (get_wysiwig_info()->stage == SelectingReference)
    {
      get_wysiwig_info()->stage = STAGE_NONE;
      gtk_main_quit ();
      return TRUE;
    }
  if (get_wysiwig_info()->stage == SelectingPoint)
    {
      get_wysiwig_info()->stage = STAGE_NONE;
      get_wysiwig_info()->Mark.width = get_wysiwig_info()->Mark.height = PRINTMARKER;  //width=0 means no mark
      get_wysiwig_info()->Mark.x = event->x + xx;
      get_wysiwig_info()->Mark.y = event->y + yy;
      g_print ("Selected point, %f %f \n", get_wysiwig_info()->Mark.x, get_wysiwig_info()->Mark.y);
      gtk_main_quit ();
      return TRUE;
    }
  if (get_wysiwig_info()->stage == SelectingFarEnd)
    {
      get_wysiwig_info()->far_i = get_wysiwig_info()->farpoint = get_wysiwig_info()->last_button_release;
      get_wysiwig_info()->stage = WaitingForDrag;
      //first post-insert a \stemNeutral if beaming
      if (get_wysiwig_info()->grob == Beam)
        {
          call_out_to_guile ("(d-MoveCursorRight)(if (not (StemDirective?)) (begin   (d-InfoDialog (_ \"Note that a Directive to revert to automatic stems is now placed after the beamed notes. Edit this as needed for the voice you are using.\")) (d-InsertStem)))");
        }
      //g_print("yadjust %f %f\n", nearadjust, faradjust);
      //here we move the cursor back to the beam/slur start
      goto_movement_staff_obj (NULL, -1, get_wysiwig_info()->pos.staff, get_wysiwig_info()->pos.measure, get_wysiwig_info()->pos.object);
      gtk_widget_queue_draw (Denemo.printarea);
      gchar *msg = (get_wysiwig_info()->grob == Slur) ? _("Now drag the begin/end markers to suggest slur position/angle\nRight click when done.") : _("Now drag the begin/end markers to set position/angle of beam\nRight click when done.");

      gtk_message_dialog_set_markup (GTK_MESSAGE_DIALOG (get_wysiwig_info()->dialog), msg);
      gtk_widget_show (get_wysiwig_info()->dialog);
      return TRUE;
    }
  if ((get_wysiwig_info()->stage == DraggingNearEnd) || (get_wysiwig_info()->stage == DraggingFarEnd))
    {
      get_wysiwig_info()->stage = WaitingForDrag;
      return TRUE;
    }



  if (get_wysiwig_info()->stage == Offsetting)
    {
      if (right)
        popup_tweak_menu ();
      else
        {
          g_print ("Offsetting quitting with %d %d", get_wysiwig_info()->stage, get_wysiwig_info()->grob);
          //The offset depends on the object being dragged. ToogleTrill sign uses bottom right, ToggleCoda uses center left.
          //      get_wysiwig_info()->curx +=18;//for trill
          //      get_wysiwig_info()->cury +=18;//for coda, mordent ...
          //      ???


          gtk_main_quit ();
        }
      return TRUE;
    }

  // \once \override DynamicLineSpanner #'padding = #10 setting padding for cresc and dimin
  // \once \override DynamicLineSpanner #'Y-offset = #-10 to move a cresc or dimin vertically downwards.
  // \once \override DynamicLineSpanner #'direction = #1 to place above/below (-1)
  //g_print("Stage %d object loc %d left %d", get_wysiwig_info()->stage, object_located_on_entry, left);
  if (right && (get_wysiwig_info()->stage == STAGE_NONE))
    {
      if (object_located_on_entry)      //set by action_for_link
        popup_object_edit_menu ();
      else
        popup_tweak_menu ();
      return TRUE;
    }


  return TRUE;
#if 0
//This code for use later when dragging an object
  g_print ("get_wysiwig_info()->selecting %d\n", get_wysiwig_info()->selecting);

  if (get_wysiwig_info()->selecting)
    {
      get_wysiwig_info()->near.x = event->x;
      get_wysiwig_info()->near.y = event->y;
      gint width, height;
      normalize ();

      width = get_wysiwig_info()->near.x - get_wysiwig_info()->Mark.x;
      height = get_wysiwig_info()->near.y - get_wysiwig_info()->Mark.y;
      GtkIconFactory *icon_factory = gtk_icon_factory_new ();
      if (marky + adjust_y < 0 || (get_wysiwig_info()->Mark.y + adjust_y + height > gdk_pixbuf_get_height (Denemo.pixbuf)))
        return TRUE;
      GdkPixbuf *sub_pixbuf = gdk_pixbuf_new_subpixbuf (Denemo.pixbuf, get_wysiwig_info()->Mark.x + adjust_x, get_wysiwig_info()->Mark.y + adjust_y, width, height);

      GdkPixbuf *alphapixbuf = gdk_pixbuf_add_alpha (sub_pixbuf, TRUE, 255, 255, 255);
      GdkPixbuf *scaledpixbuf = gdk_pixbuf_scale_simple (alphapixbuf, width, height, GDK_INTERP_BILINEAR);
      if (scaledpixbuf)
        {
          gchar *data = create_xbm_data_from_pixbuf (scaledpixbuf, 0, 0, width, height);

          GtkIconSet *icon_set = gtk_icon_set_new_from_pixbuf (sub_pixbuf);
          g_object_unref (sub_pixbuf);
          gtk_icon_factory_add (icon_factory, "Save Graphic", icon_set);
          gtk_icon_factory_add_default (icon_factory);
          g_object_unref (alphapixbuf);
          if (data)
            {
              if (Denemo.gui->xbm)
                g_free (Denemo.gui->xbm);
              Denemo.gui->xbm = data;
              Denemo.gui->xbm_width = width;
              Denemo.gui->xbm_height = height;
            }
        }
    }
  get_wysiwig_info()->selecting = FALSE;
#endif
  return TRUE;
}

// get_print_status()->mtime = file_get_mtime(filename); use in get_printfile_pathbasename

static void
typeset_control (gpointer data)
{
  static gpointer last_data = NULL;
  static GString *last_script = NULL;
  gint markstaff = Denemo.gui->si->markstaffnum;
  Denemo.gui->si->markstaffnum = 0;

  //g_print("typeset control with %d : print view is %d\n",  Denemo.gui->textwindow && gtk_widget_get_visible(Denemo.gui->textwindow), get_print_status()->background==STATE_ON);
//  if(Denemo.gui->textwindow && gtk_widget_get_visible(Denemo.gui->textwindow) && (get_print_status()->background==STATE_ON) && get_print_status()->typeset_type!=TYPESET_ALL_MOVEMENTS)
//                      return;
  if (get_print_status()->background != STATE_ON)
    get_print_status()->background = 0; //STATE_NONE
  if (last_script == NULL)
    last_script = g_string_new ("(d-PrintView)");

  if (data == create_all_pdf)
    create_all_pdf ();
  else if (data == create_full_score_pdf)
    create_full_score_pdf ();
  else if (data == create_movement_pdf)
    create_movement_pdf ();
  else if (data == create_part_pdf)
    create_part_pdf ();
  else if (data != NULL)
    {
      if (get_print_status()->background == STATE_ON)
        {
          save_selection (Denemo.gui->si);
          if (get_print_status()->typeset_type == TYPESET_ALL_MOVEMENTS)
            {
              Denemo.gui->si->markstaffnum = 0;
              create_pdf (FALSE, TRUE);
            }
          else if (get_print_status()->typeset_type == TYPESET_MOVEMENT)
            {
              Denemo.gui->si->markstaffnum = 0;
              create_pdf (FALSE, FALSE);
            }
          else
            {
              gint value = Denemo.gui->si->currentstaffnum - get_print_status()->first_staff;
              if (value < 1)
                value = 1;
              Denemo.gui->si->markstaffnum = Denemo.gui->si->selection.firststaffmarked = value;

              value = Denemo.gui->si->currentstaffnum + get_print_status()->last_staff;
              if (value < 1)
                value = 1;
              Denemo.gui->si->selection.laststaffmarked = value;

              value = Denemo.gui->si->currentmeasurenum - get_print_status()->first_measure;
              if (value < 1)
                value = 1;
              Denemo.gui->si->selection.firstmeasuremarked = value;

              value = Denemo.gui->si->currentmeasurenum + get_print_status()->last_measure;
              if (value < 1)
                value = 1;
              Denemo.gui->si->selection.lastmeasuremarked = value;

              Denemo.gui->si->selection.firstobjmarked = 0;
              Denemo.gui->si->selection.lastobjmarked = G_MAXINT - 1;   //counts from 0, +1 must be valid
              create_pdf (FALSE, FALSE);        //this movement only cursor-relative selection of measures     
            }
        }
      else
        {
          busy_cursor ();
          create_pdf (FALSE, TRUE);
        }
      g_string_assign (last_script, data);
      last_data = NULL;
      g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));
      if (get_print_status()->background == STATE_ON)
        {
          restore_selection (Denemo.gui->si);
        }
      Denemo.gui->si->markstaffnum = markstaff;
      return;
    }
  else
    {                           //data is NULL, repeat last typeset
      if (last_data)
        {
          ((void (*)()) last_data) ();
          Denemo.gui->si->markstaffnum = markstaff;
          return;
        }
      else if (last_script->len)
        {

          busy_cursor ();
          call_out_to_guile (last_script->str);
          g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (FALSE));

          Denemo.gui->si->markstaffnum = markstaff;
          return;
        }
      Denemo.gui->si->markstaffnum = markstaff;

      return;
    }
  last_data = data;
  Denemo.gui->si->markstaffnum = markstaff;
}

//Callback for the command PrintView
//Ensures the print view window is visible.
//when called back as an action it calls create_all_pdf() provided the score has changed
void
show_print_view (GtkAction * action, G_GNUC_UNUSED gpointer param)
{
  GtkWidget *w = gtk_widget_get_toplevel (Denemo.printarea);
  present_print_view_window();
  if (action && (changecount != Denemo.gui->changecount || Denemo.gui->lilysync != Denemo.gui->changecount))
    {
      if (!initialize_typesetting ())
        typeset_control (create_all_pdf);
    }
}

void
typeset_current_layout (void)
{
  typeset_control (create_all_pdf);
}

/* typeset the score, and store the passed script for refresh purposes*/
gboolean
typeset_for_script (gchar * script)
{
  typeset_control (script);
  busy_cursor ();
  show_print_view (NULL, NULL);
  return TRUE;
}

static void
page_display (G_GNUC_UNUSED GtkWidget * button, gint page_increment)
{
  gint i;
  for (i = 0; i < page_increment; i++)
    ev_view_next_page ((EvView *) Denemo.printarea);
  for (i = 0; i > page_increment; i--)
    ev_view_previous_page ((EvView *) Denemo.printarea);
}

static void
dual_page (G_GNUC_UNUSED GtkWidget * button)
{
  GError *err = NULL;
  g_object_set_data (G_OBJECT (Denemo.printarea), "Duplex", GINT_TO_POINTER (!g_object_get_data (G_OBJECT (Denemo.printarea), "Duplex")));
//refresh...
//  EvDocumentModel  *model = ev_view_get_model((EvView*)Denemo.printarea);
//  ev_document_model_set_dual_page (model, (gboolean)g_object_get_data(G_OBJECT(Denemo.printarea), "Duplex"));
  set_printarea (&err);
}

#if 0
gint
printarea_scroll_event (GtkWidget * widget, GdkEventScroll * event)
{
  switch (event->direction)
    {
    case GDK_SCROLL_UP:
      //g_print("scroll up event\n");
      break;
    case GDK_SCROLL_DOWN:
      //g_print("scroll down event\n");
      break;
    }
  return FALSE;
}
#endif
static void
typeset_action (G_GNUC_UNUSED GtkWidget * button, gpointer data)
{
  if (initialize_typesetting ())
    {
      g_warning ("InitializeTypesetting failed\n");
    }
  else
    typeset_control (data);
}

void
typeset_part (void)
{
  typeset_control (create_part_pdf);
}

static gboolean
retypeset (void)
{
  static gint firstmeasure, lastmeasure, firststaff, laststaff, movementnum;
  DenemoScore *si = Denemo.gui->si;
  if ((get_print_status()->printpid == GPID_NONE) && (gtk_widget_get_visible (gtk_widget_get_toplevel (Denemo.printarea))))
    {
      if (get_print_status()->typeset_type == TYPESET_ALL_MOVEMENTS)
        {
          if (changecount != Denemo.gui->changecount)
            {
              get_print_status()->background = STATE_ON;
              typeset_control ("(disp \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
              get_print_status()->background = STATE_OFF;
              changecount = Denemo.gui->changecount;
            }
        }
      else if ((changecount != Denemo.gui->changecount) || (si->currentmovementnum != movementnum) || ((get_print_status()->typeset_type == TYPESET_EXCERPT) && (si->currentmeasurenum < firstmeasure || si->currentmeasurenum > lastmeasure || si->currentstaffnum < firststaff || si->currentstaffnum > laststaff)))
        {
          firstmeasure = si->currentmeasurenum - get_print_status()->first_measure;
          if (firstmeasure < 0)
            firstmeasure = 0;
          lastmeasure = si->currentmeasurenum + get_print_status()->last_measure;
          firststaff = si->currentstaffnum - get_print_status()->first_staff;
          if (firststaff < 0)
            firststaff = 0;
          laststaff = si->currentstaffnum + get_print_status()->last_staff;
          movementnum = si->currentmovementnum;
          get_print_status()->background = STATE_ON;
          typeset_control ("(disp \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
          get_print_status()->background = STATE_OFF;
          changecount = Denemo.gui->changecount;
        }
    }
  return TRUE;                  //continue
}

//turn the continuous update off and on
static void
toggle_updates (G_GNUC_UNUSED GtkWidget * menu_item, GtkWidget * button)
{
  if (get_print_status()->updating_id)
    {
      g_source_remove (get_print_status()->updating_id);
      get_print_status()->updating_id = 0;
      gtk_button_set_label (GTK_BUTTON (button), MANUAL);
      if (Denemo.prefs.persistence)
        Denemo.prefs.manualtypeset = TRUE;
      gtk_window_set_transient_for (GTK_WINDOW (gtk_widget_get_toplevel (Denemo.printarea)), NULL);
    }
  else
    {
      if (Denemo.prefs.typesetrefresh)
        get_print_status()->updating_id = g_timeout_add (Denemo.prefs.typesetrefresh, (GSourceFunc) retypeset, NULL);
      else
        get_print_status()->updating_id = g_idle_add ((GSourceFunc) retypeset, NULL);
      gtk_button_set_label (GTK_BUTTON (button), CONTINUOUS);
      if (Denemo.prefs.persistence)
        Denemo.prefs.manualtypeset = FALSE;
    }
}

static void
set_typeset_type (GtkWidget * radiobutton)
{
  if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (radiobutton)))
    {
      changecount = 0;          //reset so that a retype occurs
      gint index = g_slist_index (gtk_radio_button_get_group (GTK_RADIO_BUTTON (radiobutton)), radiobutton);
      //g_print("Get %s at %d\n", gtk_button_get_label(GTK_BUTTON(radiobutton)), index);
      switch (index)
        {
        case 0:
          get_print_status()->typeset_type = TYPESET_EXCERPT;
          break;
        case 1:
          get_print_status()->typeset_type = TYPESET_MOVEMENT;
          break;
        case 2:
          get_print_status()->typeset_type = TYPESET_ALL_MOVEMENTS;
        }
      if (Denemo.prefs.persistence)
        Denemo.prefs.typesettype = get_print_status()->typeset_type;
    }
}

static void
value_change (GtkWidget * spinner, gint * value)
{
  *value = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (spinner));
  if (Denemo.prefs.persistence)
    {
      Denemo.prefs.firstmeasure = get_print_status()->first_measure;
      Denemo.prefs.lastmeasure = get_print_status()->last_measure;
      Denemo.prefs.firststaff = get_print_status()->first_staff;
      Denemo.prefs.laststaff = get_print_status()->last_staff;
    }
}

static void
range_dialog (void)
{
  static GtkWidget *dialog;
  if (dialog == NULL)
    {


      dialog = gtk_dialog_new ();
      GtkWidget *area = gtk_dialog_get_action_area (GTK_DIALOG (dialog));
      GtkWidget *vbox = gtk_vbox_new (FALSE, 1);
      gtk_container_add (GTK_CONTAINER (area), vbox);
      GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);

      GtkWidget *label = gtk_label_new (_("Measures before cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      GtkWidget *spinner = gtk_spin_button_new_with_range (0, 1000, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &get_print_status()->first_measure);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), get_print_status()->first_measure);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);


      label = gtk_label_new (_("Measures after cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 1000, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &get_print_status()->last_measure);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), get_print_status()->last_measure);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);

      label = gtk_label_new (_("Staffs before cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 100, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &get_print_status()->first_staff);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), get_print_status()->first_staff);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      label = gtk_label_new (_("Staffs after cursor:"));
      gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, TRUE, 8);
      spinner = gtk_spin_button_new_with_range (0, 100, 1);
      g_signal_connect (spinner, "value-changed", (GCallback) value_change, &get_print_status()->last_staff);
      gtk_spin_button_set_value (GTK_SPIN_BUTTON (spinner), get_print_status()->last_staff);

      gtk_box_pack_start (GTK_BOX (hbox), spinner, TRUE, TRUE, 0);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);

      hbox = gtk_hbox_new (FALSE, 1);
      gtk_box_pack_start (GTK_BOX (vbox), hbox, TRUE, TRUE, 0);


      GtkWidget *button0 = gtk_radio_button_new_with_label_from_widget (NULL, _("All Movements"));
      g_signal_connect (G_OBJECT (button0), "toggled", G_CALLBACK (set_typeset_type), NULL);
      gtk_widget_set_tooltip_text (button0, _("If checked the current layout is re-typeset at every change"));
      gtk_box_pack_start (GTK_BOX (hbox), button0, TRUE, TRUE, 0);

      GtkWidget *button1 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (button0), _("Current Movement"));
      g_signal_connect (G_OBJECT (button1), "toggled", G_CALLBACK (set_typeset_type), NULL);
      gtk_widget_set_tooltip_text (button1, _("If checked the current movement is re-typeset at every change"));
      gtk_box_pack_start (GTK_BOX (hbox), button1, TRUE, TRUE, 0);


      GtkWidget *button2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON (button0), _("Cursor Context"));
      g_signal_connect (G_OBJECT (button2), "toggled", G_CALLBACK (set_typeset_type), NULL);
      gtk_widget_set_tooltip_text (button2, _("If checked the range around the current cursor position is re-typeset at every change or when the cursor moves out of range."));
      gtk_box_pack_start (GTK_BOX (hbox), button2, TRUE, TRUE, 0);
      if (Denemo.prefs.typesettype == TYPESET_MOVEMENT)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button1), TRUE);
      if (Denemo.prefs.typesettype == TYPESET_EXCERPT)
        gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (button2), TRUE);

      g_signal_connect (dialog, "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
      gtk_widget_show_all (dialog);
    }
  else
    gtk_widget_show (dialog);

}

static GtkWidget *
get_updates_menu (GtkWidget * button)
{
  static GtkWidget *menu;
  if (menu == NULL)
    {
      GtkWidget *item;
      menu = gtk_menu_new ();
      item = gtk_check_menu_item_new_with_label (CONTINUOUS);
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      gtk_widget_set_tooltip_text (item, _("Set background updates on/off."));
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (toggle_updates), button);

      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (item), !Denemo.prefs.manualtypeset);
      item = gtk_menu_item_new_with_label (_("Range"));
      gtk_widget_set_tooltip_text (item, _("Set how much of the score to re-draw."));
      gtk_menu_shell_append (GTK_MENU_SHELL (menu), item);
      g_signal_connect (G_OBJECT (item), "activate", G_CALLBACK (range_dialog), NULL);
      gtk_widget_show_all (menu);
    }
  return menu;
}

static void
updates_menu (GtkWidget * button)
{
  gtk_menu_popup (GTK_MENU (get_updates_menu (button)), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time ());
}

static GtkWidget *
get_updates_button (void)
{
  GtkWidget *button = gtk_button_new_with_label (MANUAL);
  gtk_widget_set_tooltip_text (button, _("Set background updater on/off. This controls if typesetting is re-done after each change to the music. The amount of the score to be re-typeset can be set via this button."));
  g_signal_connect (button, "clicked", G_CALLBACK (updates_menu), NULL);
  return button;
}

static void
popup_layouts_menu ()
{
  GtkWidget *menu = GetLayoutMenu ();
  gtk_menu_popup (GTK_MENU (menu), NULL, NULL, NULL, NULL, 0, GDK_CURRENT_TIME);
}

void
install_printpreview (GtkWidget * top_vbox)
{
  if (Denemo.printarea)
    return;
  get_print_status()->typeset_type = Denemo.prefs.typesettype;
  get_print_status()->first_measure = Denemo.prefs.firstmeasure;
  get_print_status()->last_measure = Denemo.prefs.lastmeasure;
  get_print_status()->first_staff = Denemo.prefs.firststaff;
  get_print_status()->last_staff = Denemo.prefs.laststaff;

  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox, FALSE, TRUE, 0);
  GtkWidget *hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_hbox), hbox, FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label (_("Print"));
  gtk_widget_set_tooltip_text (button, _("Pops up a Print dialog. From this you can send your typeset score to a printer or to a PDF file."));
  g_signal_connect (button, "clicked", G_CALLBACK (libevince_print), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("PDF"));
  gtk_widget_set_tooltip_text (button, _("Exports a pdf file for this layout"));
  g_signal_connect (button, "clicked", G_CALLBACK (copy_pdf), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Typeset"));

  gtk_widget_set_tooltip_text (button, _("Typesets the music using the one of the created layouts. See View->Score Layouts to see the layouts you have created."));
  g_signal_connect (button, "clicked", G_CALLBACK (popup_layouts_menu), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


  button = gtk_button_new_with_label (_("Movement"));
  gtk_widget_set_tooltip_text (button, _("Typesets the music from the current movement. This creates a score layout comprising one movement."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), create_movement_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Part"));
  gtk_widget_set_tooltip_text (button, _("Typesets the music from the current part for all movements. A part is all the music with the same staff-name. This creates a score layout with one part, all movements."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), create_part_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Refresh"));
  gtk_widget_set_tooltip_text (button, _("Re-issues the last print command. Use this after modifying the file to repeat the typesetting."));
  g_signal_connect (button, "clicked", G_CALLBACK (typeset_action), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = get_updates_button ();
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  (void) get_updates_menu (button);     //this is to initialize the continuous/manual state
  hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_end (GTK_BOX (main_hbox), hbox, FALSE, TRUE, 0);


  button = gtk_button_new_with_label (_("Duplex"));
  gtk_widget_set_tooltip_text (button, _("Shows pages side by side, so you can see page turns for back-to-back printing\n"));
  g_signal_connect (button, "clicked", G_CALLBACK (dual_page), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label (_("Next"));
  gtk_widget_set_tooltip_text (button, _("Move to the next page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect (button, "clicked", G_CALLBACK (page_display), (gpointer) 1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label (_("Previous"));
  gtk_widget_set_tooltip_text (button, _("Move to the previous page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect (button, "clicked", G_CALLBACK (page_display), (gpointer) - 1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);


  top_vbox = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  // if(!Denemo.prefs.manualtypeset)
  //      gtk_window_set_urgency_hint (GTK_WINDOW(Denemo.window), TRUE);//gtk_window_set_transient_for (GTK_WINDOW(top_vbox), GTK_WINDOW(Denemo.window));
  gtk_window_set_title (GTK_WINDOW (top_vbox), _("Denemo Print View"));
  gtk_window_set_default_size (GTK_WINDOW (top_vbox), 600, 750);
  g_signal_connect (G_OBJECT (top_vbox), "delete-event", G_CALLBACK (hide_printarea_on_delete), NULL);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);

  GtkAdjustment *printvadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printvscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (printvadjustment));

  GtkAdjustment *printhadjustment = GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printhscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (printhadjustment));

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (printhadjustment, printvadjustment);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE, 0);

  ev_init ();

  Denemo.printarea = (GtkWidget *) ev_view_new ();

  gtk_container_add (GTK_CONTAINER (score_and_scroll_hbox), Denemo.printarea);
  if (Denemo.prefs.newbie)
    gtk_widget_set_tooltip_markup (score_and_scroll_hbox,
                                   _
                                   ("This window shows the final typeset score from which you can print or (via print to file) create a PDF document.\nThis will be continuously updated while you edit the music in the main window.\nIn this Print View window you can click on a note to move to that place in the main Denemo display window. The right-click to get a menu of \"tweaks\" which you can apply to drag slurs, beams etc if they are not quite right.\n<b>Note</b>: It can take some time to generate a beautifully typeset score, especially for a large score on a slow machine so choose just a range to be continually updated in that case, or turn off continuous update."));

  g_signal_connect (G_OBJECT (Denemo.printarea), "external-link", G_CALLBACK (action_for_link), NULL);


#if GTK_MAJOR_VERSION==3
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "draw", G_CALLBACK (printarea_draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "expose_event", G_CALLBACK (printarea_draw_event), NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.printarea), "motion_notify_event", G_CALLBACK (printarea_motion_notify), NULL);


  //g_signal_connect (G_OBJECT (Denemo.printarea), "focus_in_event",
  //            G_CALLBACK (printarea_focus_in_event), NULL);


//g_print("Attaching signal...");
// !!!not available in early versions of libevince
//g_signal_connect (G_OBJECT (Denemo.printarea), "sync-source",
//                    G_CALLBACK (denemoprintf_sync), NULL);
//g_print("...Attached signal?\n");

//what would this one fire on???? g_signal_connect (G_OBJECT (Denemo.printarea), "binding-activated",
//                    G_CALLBACK (denemoprintf_sync), NULL);

// Re-connect this signal to work on the pop up menu for dragging Denemo objects...
  g_signal_connect (G_OBJECT (Denemo.printarea), "button_press_event", G_CALLBACK (printarea_button_press), NULL);

// We may not need this signal
//  g_signal_connect (G_OBJECT (score_and_scroll_hbox), "scroll_event", G_CALLBACK(printarea_scroll_event), NULL);

  g_signal_connect_after (G_OBJECT (Denemo.printarea), "button_release_event", G_CALLBACK (printarea_button_release), NULL);

  gtk_widget_show_all (main_vbox);
  gtk_widget_hide (top_vbox);

  get_wysiwig_info()->dialog = infodialog ("");
  g_signal_connect (get_wysiwig_info()->dialog, "delete-event", G_CALLBACK (gtk_widget_hide_on_delete), NULL);
  g_signal_handlers_block_by_func (get_wysiwig_info()->dialog, G_CALLBACK (gtk_widget_destroy), get_wysiwig_info()->dialog);
  gtk_widget_hide (get_wysiwig_info()->dialog);
}

gboolean
continuous_typesetting (void)
{
  return (get_print_status()->background == STATE_ON);
}
