/* Print.c
 * 
 * printing support for GNU Denemo
 * outputs to a pdf or png file
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

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#ifdef HAVE_WAIT_H
#include <wait.h>
#endif

#include "export/print.h"
#include "printview/printview.h"
#include "core/preferences.h"
#include "export/exportlilypond.h"
#include "core/utils.h"


  
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

gint LilyPond_stderr = -1;       //A file descriptor to pipe for LilyPond's stderr
GError *lily_err = NULL;

GPid previewerpid = GPID_NONE;

printstatus*
get_print_status(){
  static printstatus PrintStatus = { GPID_NONE, 0, 0, 4, 4, 4, 4, TYPESET_ALL_MOVEMENTS, 0, 0, {NULL, NULL} , {NULL, NULL}, {NULL, NULL} };
  return &PrintStatus;
}

WysiwygInfo*
get_wysiwyg_info(){
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
  //g_debug("Removed old pdf file %s %d\n",get_print_status()->printname_pdf[get_print_status()->cycle], success);
}


/*** 
 * make sure lilypond is in the path defined in the preferences
 */
/* UNUSED
gboolean
check_lilypond_path (DenemoProject * gui)
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

gchar *
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
                                                  
void
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
  //g_debug("\nLilyPond error messages\n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8>< %s \n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><\n", bytes);
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
          // FIXME this causes a lock-up     warningdialog(_("Typesetter detected errors. Cursor is position on the error point.\nIf in doubt delete and re-enter the measure."));
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
      warningdialog (_("Could not execute lilypond - check Edit->preferences → externals → lilypond setting\nand lilypond installation"));
      g_warning ("%s", lily_err->message);
      if (lily_err)
        g_error_free (lily_err);
      lily_err = NULL;
    }
  g_free (bytes);
}

static void
open_viewer (gint status, gchar * filename)
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

   
      printfile = g_strconcat (filename, ".png", NULL);
 


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

 
      arguments = png;
 
      if (Denemo.prefs.imageviewer->len == 0)
        {
          gboolean ok = run_file_association (printfile);
          if (!ok)
            {
              err = g_error_new (G_FILE_ERROR, -1, "Could not run file assoc for %s", ".png");
              g_warning ("Could not run the file association for a %s file", ".png");
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
          warningdialog (err->message);
          g_warning ("%s", err->message);
          g_error_free (err);
          err = NULL;
        }
      g_free (printfile);
    }
}

static void
open_pngviewer (G_GNUC_UNUSED GPid pid, gint status, gchar * filename)
{
  open_viewer (status, filename);
}
static gboolean
call_stop_lilypond (GtkWidget * w, GdkEvent * event, gboolean *progressing)
{
  *progressing = FALSE;
  stop_lilypond ();
  return TRUE;
}

static gint
run_lilypond (gchar ** arguments)
{
  gint error = 0;
  if (get_print_status()->background == STATE_NONE)
    progressbar (_("Denemo Typesetting"), call_stop_lilypond);
  if (lily_err)
    {
      g_warning ("Old error message from launching lilypond still present - message was %s\nDiscarding...", lily_err->message);
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
      g_warning ("Error launching lilypond! Message is %s", lily_err->message);
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
  DenemoProject *gui = Denemo.project;
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

/*  create pdf of current score, optionally restricted to voices/staffs whose name match the current one.
 *  generate the lilypond text (on disk)
 *  Fork and run lilypond
 */
void
create_pdf (gboolean part_only, gboolean all_movements)
{
      if (get_print_status()->printpid != GPID_NONE)
    {
      if (confirm (_("Already Typesetting"), _("Abandon this typeset?")))
        {
          if (get_print_status()->printpid != GPID_NONE)        //It could have died while the user was making up their mind...
            kill_process (get_print_status()->printpid);
          get_print_status()->printpid = GPID_NONE;
        }
      else
        {
          warningdialog (_("Cancelled"));
          
          return;
        }
    }
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
printrangedialog (DenemoProject * gui)
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

  gint max_measure = g_list_length (((DenemoStaff *) (gui->movement->thescore->data))->measures);

  label = gtk_label_new (_("Print from Measure"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  from_measure = gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure), (gdouble) gui->movement->selection.firstmeasuremarked);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_measure = gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  //  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure), (gdouble) gui->movement->selection.lastmeasuremarked);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->movement->selection.firstmeasuremarked = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
      gui->movement->selection.lastmeasuremarked = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
      //gtk_widget_destroy (dialog);
    }
  else
    {
      gui->movement->selection.firstmeasuremarked = gui->movement->selection.lastmeasuremarked = 0;
    }
  if (gui->movement->selection.firstmeasuremarked)
    {
      gui->movement->markstaffnum = gui->movement->selection.firststaffmarked = 1;
      gui->movement->selection.laststaffmarked = g_list_length (gui->movement->thescore);
    }

  gtk_widget_destroy (dialog);
}

static void
rm_temp_files (gchar * file, gpointer free_only)
{
  //g_debug("\n%s Deleting temp file %s\n",free_only?"Not":"", file);
  if (!free_only)
    g_remove (file);
  g_free (file);
}

void
printpng_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, GList * filelist)
{
  gchar *pngfile = g_strconcat((gchar *) get_printfile_pathbasename (), ".png", NULL);
  gchar *message = g_strdup_printf(_("A PNG file has been created at: %s\nIf you have an external viewer available (see Edit → Change Preferences → Externals → Image Viewer)\nthen it will be displayed with that program."), pngfile);
  g_debug ("printpng_finished\n");
  g_list_foreach (filelist, (GFunc) rm_temp_files, FALSE);
  g_list_free (filelist);
  g_spawn_close_pid (get_print_status()->printpid);
  get_print_status()->printpid = GPID_NONE;
  progressbar_stop ();
  infodialog (message);
  g_free(message);
  g_free(pngfile);
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
  infodialog (_("Your pdf file has now been created"));
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
 *  @param gui pointer to the DenemoProject structure
 */
void
export_png (gchar * filename, GChildWatchFunc finish, DenemoProject * gui)
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

  gchar* output = g_strconcat(filename, ".png", NULL);
  g_debug("Generating %s from Lilypond", output);

  /* generate the png file */
  if (finish)
    {
      gint error = run_lilypond (arguments);
      if (!error)
        g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) finish, (gchar *) filelist);

      if(!g_file_test(output, G_FILE_TEST_EXISTS))
        g_critical("Lilypond has not generated %s", output);
    }
  else
    {
      GError *err = NULL;
      g_spawn_sync (locateprintdir (),  /* dir */
                    arguments, 
                    NULL,    /* env */
                    G_SPAWN_SEARCH_PATH, 
                    NULL,  /* child setup func */
                    NULL,       /* user data */
                    NULL,       /* stdout */
                    NULL,       /* stderr */
                    NULL, &err);
      //These are in tmpdir and can be used for the .eps file, so don't delete them   
      //g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
      g_list_free (filelist);
    }
  g_free(output);
}

/**
 * Does all the export pdf work.
 * calls exportmudela and then  
 * runs lilypond to a create a filename.pdf
 *
 *  @param filename filename to save score to
 *  @param gui pointer to the DenemoProject structure
 */
void
export_pdf (gchar * filename, DenemoProject * gui)
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
  return_on_windows_if_printing;

  gint error = run_lilypond (arguments);
  if (error)
    {
      g_spawn_close_pid (get_print_status()->printpid);
      get_print_status()->printpid = GPID_NONE;
      return;
    }

  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printpdf_finished, filelist);
}

/* callback to print current part (staff) of score */
void
printpart_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  present_print_view_window();
  DenemoProject *gui = Denemo.project;
  if (gui->movement->markstaffnum)
    if (confirm (_("A range of music is selected"), _("Print whole file?")))
      {
        gui->movement->markstaffnum = 0;
      }
  if ((gui->movements && g_list_length (gui->movements) > 1) && (confirm (_("This piece has several movements"), _("Print this part from all of them?"))))
    create_pdf (TRUE, TRUE);
  else
    create_pdf (TRUE, FALSE);
  g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
#endif
}

void
printselection_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  if (Denemo.project->movement->markstaffnum) {
    present_print_view_window();
    create_pdf (FALSE, FALSE);
    g_child_watch_add (get_print_status()->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
  }
  else
    warningdialog (_("No selection to print"));
#endif
}

void
printexcerptpreview_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
  DenemoProject *gui = Denemo.project;
  if (!gui->movement->markstaffnum)   //If no selection has been made 
    printrangedialog (gui);     //Launch a dialog to get selection
  if (gui->movement->selection.firstmeasuremarked)
    {
      gui->lilycontrol.excerpt = TRUE;
      export_png ((gchar *) get_printfile_pathbasename (), (GChildWatchFunc) prepare_preview, gui);
      gui->lilycontrol.excerpt = FALSE;
    }
}

/* callback to print whole of score */
void
printall_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  print_from_print_view (TRUE);
#endif
}

/* callback to print movement of score */
void
printmovement_cb (G_GNUC_UNUSED GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  print_from_print_view (FALSE);
#endif
}

void
show_print_view (GtkAction * action, G_GNUC_UNUSED DenemoScriptParam * param){
#ifndef USE_EVINCE  
  g_debug("This feature requires denemo to be built with evince");
#else
  implement_show_print_view(action!=NULL);
#endif
}
