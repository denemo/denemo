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
#include "printview/markupview.h"
#include "core/prefops.h"
#include "export/exportlilypond.h"
#include "core/utils.h"

gint LilyPond_stderr;       //A file descriptor to pipe for LilyPond's stderr
GError *lily_err;
GPid previewerpid;


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




WysiwygInfo*
get_wysiwyg_info(){
  static WysiwygInfo Ww;                   //Wysywyg information
  return &Ww;
}
static gchar *include = "";
static gchar *local_include = "";
void initialize_lilypond_includes(void) 
	{
	local_include = g_strdup_printf ("-I%s", g_build_filename(get_user_data_dir(TRUE), get_local_dir (DENEMO_DIR_LILYPOND_INCLUDE), NULL));
	include = g_strdup_printf ("-I%s", get_system_dir (DENEMO_DIR_LILYPOND_INCLUDE));
	}
void initialize_print_status (void)
{
  Denemo.printstatus = (DenemoPrintInfo*)g_malloc0(sizeof (DenemoPrintInfo));
  Denemo.printstatus->printpid = GPID_NONE;
  Denemo.printstatus->typeset_type = TYPESET_ALL_MOVEMENTS;
  Denemo.printstatus->printbasename[0] = g_build_filename (locateprintdir (), "denemoprintA", NULL);
  Denemo.printstatus->printbasename[1] = g_build_filename (locateprintdir (), "denemoprintB", NULL);
  Denemo.printstatus->printname_pdf[0] = g_strconcat (Denemo.printstatus->printbasename[0], ".pdf", NULL);
  Denemo.printstatus->printname_svg[0] = g_strconcat (Denemo.printstatus->printbasename[0], ".svg", NULL);
#ifdef G_OS_WIN32
  Denemo.printstatus->printname_midi[0] = g_strconcat (Denemo.printstatus->printbasename[0], ".mid", NULL);//LilyPond outputs .mid files for midi
#else
  Denemo.printstatus->printname_midi[0] = g_strconcat (Denemo.printstatus->printbasename[0], ".midi", NULL);
#endif
  Denemo.printstatus->printname_ly[0] = g_strconcat (Denemo.printstatus->printbasename[0], ".ly", NULL);
  Denemo.printstatus->printname_pdf[1] = g_strconcat (Denemo.printstatus->printbasename[1], ".pdf", NULL);
  Denemo.printstatus->printname_svg[1] = g_strconcat (Denemo.printstatus->printbasename[1], ".svg", NULL);
#ifdef G_OS_WIN32
  Denemo.printstatus->printname_midi[1] = g_strconcat (Denemo.printstatus->printbasename[1], ".mid", NULL);//LilyPond outputs .mid files for midi
#else
  Denemo.printstatus->printname_midi[1] = g_strconcat (Denemo.printstatus->printbasename[1], ".midi", NULL);
#endif
  Denemo.printstatus->printname_ly[1] = g_strconcat (Denemo.printstatus->printbasename[1], ".ly", NULL);
  Denemo.printstatus->error_file = NULL;
}


static void
advance_printname ()
{


  Denemo.printstatus->cycle = !Denemo.printstatus->cycle;
  /*gint success =*/ g_unlink (Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle]);
  g_unlink (Denemo.printstatus->printname_svg[Denemo.printstatus->cycle]);

  //g_debug("Removed old pdf file %s %d\n",Denemo.printstatus->printname_pdf[Denemo.printstatus->cycle], success);
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

static int version_check (lilyversion base, lilyversion installed)
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

static lilyversion string_to_lilyversion (char *string)
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

gchar *get_lily_version_string (void)
 {
	gchar *arg[] = {Denemo.prefs.lilypath->str, "--version", NULL};
	gchar *out;
	GError *err = NULL;
	g_print ("Running %s", arg[0]);
	g_spawn_sync (NULL,
					arg,
					NULL,
					G_SPAWN_SEARCH_PATH,
					NULL,
					NULL,
					&out,
					NULL,
					NULL,
					&err);

	if (err)
		g_warning ("Returned %s with %s", out, err?err->message:"");
	else
	   return regex_parse_version_number ((const gchar *)out);
	return NULL; 
 }


gint check_lily_version (gchar * version)
{
  gchar *version_string = Denemo.lilypond_installed_version;
  lilyversion installed_version = string_to_lilyversion (version_string);
  lilyversion check_version = string_to_lilyversion (version);
  return version_check (check_version, installed_version);
}

gchar *get_lilypond_include_dir (void)
{
	if (Denemo.lilypond_installed_version)
	{
	  lilyversion installed_version = string_to_lilyversion (Denemo.lilypond_installed_version);
	  lilyversion check_version = string_to_lilyversion ("2.18.0");
	  if (version_check (check_version, installed_version) == GREATER)
		return LATER_VERSION_LILYPOND_INCLUDE_DIR;
	}
  return LILYPOND_INCLUDE_DIR;
}

/* returns the base name (/tmp/Denemo????/denemoprint usually) used as a base
   filepath for printing.
   The returned string should not be freed.
*/

gchar *
get_printfile_pathbasename (void)
{
  if (Denemo.printstatus->printbasename[0] == NULL)
    advance_printname ();
  return Denemo.printstatus->printbasename[Denemo.printstatus->cycle];
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

/* look in message for :line:col: where line and col are integers and return point where found or NULL if none*/
static gchar * get_error_point (gchar *bytes, gint *line, gint *col)
{
    gchar *epoint;
    gchar *message = bytes;
    while (*message)
        {
            while (*message && (*message!=':')) message++;
            if (*message==':')
                {
                 epoint = message;
                 *line = atoi (message+1);
                 if (*line == 0)
                    {
                        message++;
                        continue;
                     }
                 message++;
                 while (*message && g_ascii_isdigit (*message)) message++;

                 if (*message==':')
                    {
                     *col = atoi (message+1);
                     if (*col == 0)
                        {
                            message++;
                            continue;
                        }

                     message++;
                     while (*message && g_ascii_isdigit (*message)) message++;


                     g_print ("%c", *message);
                     if (*message==':')
                        {
                            gchar *colon = epoint;
                            *colon = 0;
                            while ((epoint != bytes) && (*epoint != '\n')) epoint--;//FIXME is epoint now referring to the main file or some include file, line col will not work for an include file
                            if(strcmp (Denemo.printstatus->printname_ly[Denemo.printstatus->cycle], epoint)) // error is in an include file
                                Denemo.printstatus->error_file = g_strdup (epoint);
                            *colon = ':';
                            return epoint;
                        }
                    }
                    else {
                        message++;g_print ("%c", *message);
                        continue;
                     }
                }
                else {
                        message++;g_print ("%c", *message);
                        continue;
                     }
        }
    return NULL;
}
void
process_lilypond_errors (gchar * filename)
{
  Denemo.printstatus->invalid = 0;
  gchar *logfile = g_strconcat (filename, ".log", NULL);
  gchar *epoint = NULL;

  gchar *bytes;
  gint numbytes = g_file_get_contents (logfile, &bytes, NULL, NULL);
  g_free (logfile);
  if (bytes)
   numbytes=strlen (bytes);
  else
    return;
  //g_print("\nLilyPond error messages\n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8>< %s \n8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><8><\n", bytes);
  gchar *page_system_info = g_strrstr (bytes, "DenemoInfo=");
  if (page_system_info)
	{
		sscanf (page_system_info, "DenemoInfo=%d,%d", &Denemo.printstatus->pages, &Denemo.printstatus->systems);
	}
  else
	Denemo.printstatus->pages = 0;
  
  
  gint line, column;
  epoint=get_error_point (bytes, &line, &column);
  if (epoint)
    {
      truncate_lines (epoint);  /* truncate epoint if it has too many lines */
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
      Denemo.printstatus->invalid = 2;      //print_is_valid = FALSE;
      if (Denemo.printarea)
        gtk_widget_queue_draw (Denemo.printarea);
      // FIXME this causes a lock-up     warningdialog(_("Typesetter detected errors. Cursor is position on the error point.\nIf in doubt delete and re-enter the measure."));
    }
  else
    {
       // console_output (_("Done"));
        set_lily_error (0, 0); /* line 0 meaning no line */
    }
  highlight_lily_error ();
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
  if (Denemo.printstatus->printpid == GPID_NONE)
    return;
  GError *err = NULL;
  gchar *printfile;
  gchar **arguments;
  progressbar_stop ();
  console_output (_("Done"));
  g_spawn_close_pid (Denemo.printstatus->printpid);
  Denemo.printstatus->printpid = GPID_NONE;
  //normal_cursor();
  process_lilypond_errors (filename);
#if GLIB_CHECK_VERSION(2,34,0)
  {
    GError* err = NULL;
     if (!g_spawn_check_exit_status (status, &err))
        g_warning ("Lilypond did not end successfully: %s", err->message);
  }
#endif

  if (status)
    {
      g_warning /* a warning dialog causes deadlock in threaded version of program */ ("LilyPond engraver failed - See highlighting in LilyPond window (open the LilyPond window and right click to print)");
    }
  else
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
call_stop_lilypond (void)
{
  progressbar_stop ();
  stop_lilypond ();
  return TRUE;
}

static gint
run_lilypond (gchar ** arguments)
{
  static gboolean old_error = FALSE;
  gint error = 0;
  if (old_error)
    {
      g_string_assign (Denemo.input_filters, "");
      gtk_widget_show (Denemo.input_label);
      write_input_status ();
      old_error = FALSE;  
    }
  Denemo.printstatus->pages = 0;
  if (Denemo.printstatus->background == STATE_NONE)
    progressbar (_("Denemo Typesetting"), call_stop_lilypond);
  if (lily_err)
    {
      g_warning ("Old error message from launching lilypond still present - message was %s\nDiscarding...", lily_err->message);
      g_error_free (lily_err);
      lily_err = NULL;
    }
  console_output (NULL);
  console_output (_("Typesetting ..."));
   gboolean lilypond_launch_success;
if (Denemo.non_interactive)
  g_spawn_sync (locateprintdir (),       /* dir */
                 arguments,
                 NULL,    /* env */
                 G_SPAWN_SEARCH_PATH,
                 NULL,    /* child setup func */
                 NULL,    /* user data */
                 NULL,    /* stdout */
                 NULL, /* stderr */
                 &lilypond_launch_success,
                 &lily_err);
else
  lilypond_launch_success = g_spawn_async_with_pipes (locateprintdir (),       /* dir */
                                                               arguments,
                                                               NULL,    /* env */
                                                               G_SPAWN_SEARCH_PATH | G_SPAWN_DO_NOT_REAP_CHILD,
                                                               NULL,    /* child setup func */
                                                               NULL,    /* user data */
                                                               &Denemo.printstatus->printpid,
                                                               NULL,
                                                               NULL,    /* stdout */
                                                               NULL, /* stderr */
                                                               &lily_err);


  if (lily_err)
    {
      g_critical ("Error launching lilypond %s in directory %s! Message is %s", arguments[0], locateprintdir (), lily_err->message);
      g_string_printf (Denemo.input_filters, "%s%s%s", "<span font_desc=\"24\" foreground=\"red\">", _("Error: see LilyPond window"), "</span>");
      gtk_widget_show (Denemo.input_label);
      write_input_status ();
      console_output (g_strdup_printf("Error launching lilypond! Message is %s", lily_err->message));    
      g_error_free (lily_err);
      lily_err = NULL;
      error = -1;
      old_error = TRUE;//clear the status bar on next attempt...
    }
  if (!lilypond_launch_success)
    {
      //g_critical ("Error executing lilypond. Perhaps Lilypond is not installed or its path is not correctly configured. %s", lily_err->message);
      error = -1;
    }
  if (error)
    progressbar_stop ();

  return error;
}

gboolean
stop_lilypond ()
{
  if (Denemo.printstatus->printpid != GPID_NONE)
    {
      kill_process (Denemo.printstatus->printpid);
      Denemo.printstatus->printpid = GPID_NONE;
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

// for populating the Print View, cf export_pdf ()
static void
run_lilypond_for_pdf (gchar * filename, gchar * lilyfile)
{
 // if(!include) initialize_lilypond_includes();
  /*arguments to pass to lilypond to create a pdf for printing */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "-dgui",
    "--loglevel=WARN",
    "--pdf",
    local_include,
    include,
    "-o",
    filename,
    lilyfile,
    NULL
  };
  run_lilypond (arguments);
}
//synchronous generation of outfile.pdf from input lilyfile
void generate_pdf_from_lily_file (gchar *lilyfile, gchar *outfile)
{
	gboolean old = Denemo.non_interactive;
	Denemo.non_interactive = 1;
	run_lilypond_for_pdf (outfile, lilyfile);
	Denemo.non_interactive = old;
}

static void
run_lilypond_for_svg (gchar * filename, gchar * lilyfile)
{
  //  if(!include) initialize_lilypond_includes();

  /*arguments to pass to lilypond to create a svg for printing */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "-dgui",
    "--loglevel=WARN",
     "-dno-point-and-click", "-ddelete-intermediate-files", "-dbackend=svg",
    local_include,
    include,
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
      if (Denemo.printstatus->printpid != GPID_NONE)
    {
      if (confirm (_("Already Typesetting"), _("Abandon this typeset?")))
        {
          if (Denemo.printstatus->printpid != GPID_NONE)        //It could have died while the user was making up their mind...
            kill_process (Denemo.printstatus->printpid);
          Denemo.printstatus->printpid = GPID_NONE;
        }
      else
        {
          warningdialog (_("Cancelled"));

          return;
        }
    }
  get_wysiwyg_info()->stage = STAGE_NONE;
  advance_printname ();
  gchar *filename = Denemo.printstatus->printbasename[Denemo.printstatus->cycle];
  gchar *lilyfile = Denemo.printstatus->printname_ly[Denemo.printstatus->cycle];
  g_remove (lilyfile);
  Denemo.printstatus->invalid = 0;
  g_free (Denemo.printstatus->error_file);Denemo.printstatus->error_file = NULL;
  generate_lilypond (lilyfile, part_only, all_movements);
  run_lilypond_for_pdf (filename, lilyfile);
}
/*  create pdf of current score, optionally restricted to voices/staffs whose name match the current one.
 *  generate the lilypond text (on disk)
 *  Fork and run lilypond
 */
void
create_svg (gboolean part_only, gboolean all_movements)
{
      if (Denemo.printstatus->printpid != GPID_NONE)
    {
      if (confirm (_("Already Typesetting"), _("Abandon this typeset?")))
        {
          if (Denemo.printstatus->printpid != GPID_NONE)        //It could have died while the user was making up their mind...
            kill_process (Denemo.printstatus->printpid);
          Denemo.printstatus->printpid = GPID_NONE;
        }
      else
        {
          warningdialog (_("Cancelled"));

          return;
        }
    }
  get_wysiwyg_info()->stage = TypesetForPlaybackView;
  advance_printname ();
  gchar *filename = Denemo.printstatus->printbasename[Denemo.printstatus->cycle];
  gchar *lilyfile = Denemo.printstatus->printname_ly[Denemo.printstatus->cycle];
  g_remove (lilyfile);
  Denemo.printstatus->invalid = 0;
  g_free (Denemo.printstatus->error_file);Denemo.printstatus->error_file = NULL;
  generate_lilypond (lilyfile, part_only, all_movements);
  run_lilypond_for_svg (filename, lilyfile);
}

void create_pdf_for_lilypond (gchar *lilypond)
{
#ifndef USE_EVINCE
          g_debug("This feature requires denemo to be built with evince");
#else
    if (Denemo.printstatus->printpid != GPID_NONE)
         return;
  get_wysiwyg_info()->stage = STAGE_NONE;
  advance_printname ();
  gchar *filename = Denemo.printstatus->printbasename[Denemo.printstatus->cycle];
  gchar *lilyfile = Denemo.printstatus->printname_ly[Denemo.printstatus->cycle];
  g_remove (lilyfile);
  g_file_set_contents (lilyfile, lilypond, -1, NULL);
  Denemo.printstatus->invalid = 0;
  g_free (Denemo.printstatus->error_file);Denemo.printstatus->error_file = NULL;
  run_lilypond_for_pdf (filename, lilyfile);
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) markupview_finished, (gpointer) (FALSE));
#endif
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

  dialog = gtk_dialog_new_with_buttons (_("Print Excerpt Range"), GTK_WINDOW (Denemo.window), (GtkDialogFlags) (GTK_DIALOG_MODAL | GTK_DIALOG_DESTROY_WITH_PARENT), _("_OK"), GTK_RESPONSE_ACCEPT, _("_Cancel"), GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);

  gint max_measure = g_list_length (((DenemoStaff *) (gui->movement->thescore->data))->themeasures);

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
  g_debug ("printpng_finished\n");
  g_list_foreach (filelist, (GFunc) rm_temp_files, FALSE);
  g_list_free (filelist);
  g_spawn_close_pid (Denemo.printstatus->printpid);
  Denemo.printstatus->printpid = GPID_NONE;
  progressbar_stop ();
  infodialog (_("Your PNG file has now been created"));
}

static void
printpdf_finished (G_GNUC_UNUSED GPid pid, G_GNUC_UNUSED gint status, GList * filelist)
{
  if (filelist)
    {
      g_list_foreach (filelist, (GFunc) rm_temp_files, FALSE);
      g_list_free (filelist);
    }
  g_spawn_close_pid (Denemo.printstatus->printpid);
  Denemo.printstatus->printpid = GPID_NONE;
  progressbar_stop ();
  infodialog (_("Your PDF file has now been created"));
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
    "-dgui",
    "--loglevel=WARN",
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
        g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) finish, (gchar *) filelist);

      if(!g_file_test(output, G_FILE_TEST_EXISTS))
        g_critical("Lilypond has not generated %s", output);
    }
  else
    {
      GError *err = NULL;
      gint ret = 0;
      gboolean success = g_spawn_sync (locateprintdir (),  /* dir */
                    arguments,
                    NULL,    /* env */
                    G_SPAWN_SEARCH_PATH,
                    NULL,  /* child setup func */
                    NULL,       /* user data */
                    NULL,       /* stdout */
                    NULL,       /* stderr */
                    &ret,
                    &err);
      if(!success)
        g_warning ("An error happened during lilypond launching: %s", err->message);

      if(ret != 0)
        g_debug ("Lilypond did not end successfully for file %s", filename);

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
 *  Called by (d-ExportPDF ...). The Print View is populated by run_lilypond_for_pdf ()
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

 // if(!include) initialize_lilypond_includes();
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
    "-dgui",
    "--loglevel=WARN", "-dno-point-and-click",
    "--pdf",
    local_include,
    include,
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
      g_spawn_close_pid (Denemo.printstatus->printpid);
      Denemo.printstatus->printpid = GPID_NONE;
      return;
    }
  if (Denemo.non_interactive)
    printpdf_finished (0, 0, filelist);
  else
    g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printpdf_finished, filelist);
}

/* callback to print current part (staff) of score */
void
printpart_cb (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
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
  g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
#endif
}

void
printselection_cb (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  if (Denemo.project->movement->markstaffnum) {
    present_print_view_window();
    create_pdf (FALSE, FALSE);
    g_child_watch_add (Denemo.printstatus->printpid, (GChildWatchFunc) printview_finished, (gpointer) (TRUE));
  }
  else
    warningdialog (_("No selection to print"));
#endif
}

void
printexcerptpreview_cb (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
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
printall_cb (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  print_from_print_view (TRUE);
#endif
}

/* callback to print movement of score */
void
printmovement_cb (G_GNUC_UNUSED DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param)
{ return_on_windows_if_printing;
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  print_from_print_view (FALSE);
#endif
}

void
show_print_view (DenemoAction * action, G_GNUC_UNUSED DenemoScriptParam * param){
#ifndef USE_EVINCE
  g_debug("This feature requires denemo to be built with evince");
#else
  implement_show_print_view(action!=NULL);
#endif
}
