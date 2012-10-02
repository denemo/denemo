/* Print.c
 * 
 * printing support for GNU Denemo
 * outputs to an evince widget, or to a pdf or png file
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Adam Tee, 2009, 2010, 2011 Richard Shann
 */
#ifndef PRINT_H

#define PRINT_H
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>

#include <glib/gstdio.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
#include <wait.h>
#endif
#include <errno.h>
#include <denemo/denemo.h>
#include <evince-view.h>
#include "print.h"
#include "prefops.h"
#include "exportlilypond.h"
#include "utils.h"
#include "view.h"
#include "external.h"
#include "scorelayout.h"

static gboolean retypeset(void);

#if GTK_MAJOR_VERSION==3
typedef enum {
  GDK_RGB_DITHER_NONE,
  GDK_RGB_DITHER_NORMAL,
  GDK_RGB_DITHER_MAX
} GdkRgbDither;
#endif

#define MARKER (24)
static GdkRectangle Mark;
static gboolean ObjectLocated;//TRUE when an external-link has just been followed back to a Denemo object
#define GREATER 2
#define SAME 1
#define LESSER 0

typedef struct lilyversion
{
  gint major;
  gint minor;
}lilyversion;


static gint changecount = -1;//changecount when the printfile was last created FIXME multiple tabs are muddled
#define GPID_NONE (-1)

static GPid previewerpid = GPID_NONE;
static GPid get_lily_version_pid = GPID_NONE;

typedef enum { STATE_NONE = 0, //not a background typeset
							 STATE_OFF = 1<<0, //background typeset complete
							 STATE_ON = 1<<1, //background typeset in progress
							 STATE_PAUSED = 1<<2 	//background typesetting turned off to allow printing
							 } background_state;
							 
		 
							 
typedef struct printstatus {
GPid printpid;
background_state background;
gint updating_id;//id of idle callback
gint first_measure;
gint last_measure;
gint first_staff;
gint last_staff;
typeset_type typeset_type;
gint invalid;//set 1 if  lilypond reported problems or 2 if generating new pdf failed 
gint cycle;//alternate 0 1 to switch print file names
gchar *printbasename[2];
gchar *printname_pdf[2];
gchar *printname_ly[2];
} printstatus;

static printstatus PrintStatus = {GPID_NONE, 0, 0, 4, 4, 4, 4, TYPESET_ALL_MOVEMENTS};



static gint errors=-1;
static   GError *lily_err = NULL;

static
void print_finished(GPid pid, gint status, GList *filelist);


static void advance_printname() {
	if(PrintStatus.printbasename[0]==NULL) {
		PrintStatus.printbasename[0] =  g_build_filename (locateprintdir (), "denemoprintA", NULL);
		PrintStatus.printbasename[1] =  g_build_filename (locateprintdir (), "denemoprintB", NULL);
		PrintStatus.printname_pdf[0] = g_strconcat(PrintStatus.printbasename[0], ".pdf", NULL);
		PrintStatus.printname_ly[0] = g_strconcat(PrintStatus.printbasename[0], ".ly", NULL);
		PrintStatus.printname_pdf[1] = g_strconcat(PrintStatus.printbasename[1], ".pdf", NULL);
		PrintStatus.printname_ly[1] = g_strconcat(PrintStatus.printbasename[1], ".ly", NULL);	
	}

	PrintStatus.cycle = !PrintStatus.cycle;
	gint success = g_unlink(PrintStatus.printname_pdf[PrintStatus.cycle]);
	g_print("Removed old pdf file %s %d\n",PrintStatus.printname_pdf[PrintStatus.cycle], success);
}


/*** 
 * make sure lilypond is in the path defined in the preferences
 */
gboolean 
check_lilypond_path (DenemoGUI * gui){
  
  gchar *lilypath = g_find_program_in_path (Denemo.prefs.lilypath->str);
  if (lilypath == NULL)
    {
      /* show a warning dialog */
      GtkWidget *dialog =
        gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
                                GTK_DIALOG_DESTROY_WITH_PARENT,
                                GTK_MESSAGE_WARNING,
                                GTK_BUTTONS_OK,
                                _("Could not find %s"),
                                Denemo.prefs.lilypath->str);
      gtk_message_dialog_format_secondary_text (GTK_MESSAGE_DIALOG (dialog),
                                                _("Please edit lilypond path "
                                                  "in the preferences."));
      gtk_dialog_run (GTK_DIALOG (dialog));

      /* free the memory and return */
      gtk_widget_destroy (dialog);
      return 0;
    }
  else
      return 1;
}

int
version_check(lilyversion base, lilyversion installed)
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

lilyversion
string_to_lilyversion(char *string)
{
  lilyversion version = { 2, 0};
  char **token;
  const char delimiters[] = ".";
  if(string==NULL)
    return version;
  /* split string */
  token = g_strsplit(string, delimiters, 2);

  /* get major version number */
  if(token[0])
    version.major = atoi(token[0]);
  /* get minor version number */
  if(token[1])
    version.minor = atoi(token[1]);
  g_strfreev(token);
  //intf("\nstring_to_lilyversion() major = %d minor = %d\n",version.major, version.minor);
  return version;
}

gchar * 
regex_parse_version_number (const gchar *string)
{
  GRegex *regex = NULL;
  GMatchInfo *match_info;
  GString *lilyversion = g_string_new ("");

  regex = g_regex_new("\\d.\\d\\d", 0, 0, NULL);
  g_regex_match(regex, string, 0, &match_info);

  if (g_match_info_matches (match_info))
  {
  g_string_append(lilyversion, g_match_info_fetch (match_info, 0));
  }

  g_match_info_free (match_info);
  g_regex_unref (regex);
  return g_string_free(lilyversion, FALSE); 	  
}
#define INSTALLED_LILYPOND_VERSION "2.13" /* FIXME set via gub */
gchar *
get_lily_version_string (void)
{
#ifndef G_OS_WIN32
  GError *error = NULL;
  int standard_output;
#define NUMBER_OF_PARSED_CHAR 30
  gchar buf[NUMBER_OF_PARSED_CHAR]; /* characters needed to parse */

  gchar *arguments[] = {
  "lilypond",
  "-v",
  NULL
  };
  g_spawn_async_with_pipes (NULL,            /* dir */
  arguments, NULL,       /* env */
  G_SPAWN_SEARCH_PATH | G_SPAWN_DO_NOT_REAP_CHILD, NULL, /* child setup func */
  NULL,          /* user data */
  &get_lily_version_pid,	/*pid*/
  NULL, 	/*standard_input*/
  &standard_output,	/*standard output*/
  NULL,	/*standard error*/
  &error);
  if(error==NULL) {
    read(standard_output, buf, sizeof(buf));
    return regex_parse_version_number(buf);
  } else {
    g_warning ("%s", error->message);
    g_error_free (error);
  }
#endif
return INSTALLED_LILYPOND_VERSION;
}
int
check_lily_version (gchar *version)
{
  gchar *  version_string = get_lily_version_string();
  lilyversion installed_version = string_to_lilyversion(version_string);
  lilyversion check_version = string_to_lilyversion(version);
  return version_check(check_version, installed_version);
}

 
/* returns the base name (/tmp/Denemo????/denemoprint usually) used as a base
   filepath for printing.
   The returned string should not be freed.
*/
   
static gchar *get_printfile_pathbasename(void) {
	if(PrintStatus.printbasename[0]==NULL)
		advance_printname();
  return PrintStatus.printbasename[PrintStatus.cycle];
}       
/* truncate epoint after 20 lines replacing the last three chars in that case with dots */
static void truncate_lines(gchar *epoint) {
  gint i;
  for(i=0;i<20 && *epoint;i++) {
    while (*epoint && *epoint!='\n')
      epoint++;
    if(*epoint)
      epoint++;
  }
  if(epoint)
    *epoint-- = '\0';
  /* replace last three chars with ... This is always possible if epoint is not NULL */
  if(*epoint)
    for(i=3;i>0;i--)
      *epoint-- = '.';
}
/***
 * Run the command line convert-ly to get the lilypond output 
 * current with the version running on the users computer
 *
 */

void convert_ly(gchar *lilyfile){
  GError *err = NULL;
#ifdef G_OS_WIN32
  gchar *conv_argv[] = {
    "python"
    "convert-ly.py",
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
  g_spawn_sync (locateprintdir (),		/* dir */
		conv_argv, NULL,	/* env */
		G_SPAWN_SEARCH_PATH, NULL,	/* child setup func */
		NULL,		/* user data */
		NULL,		/* stdout */
		NULL,		/* stderr */
		NULL, &err);

  if (err != NULL)
    {
      g_warning ("%s", err->message);
      if(err) g_error_free (err);
      err = NULL;
    }
}

static void
process_lilypond_errors(gchar *filename){
  DenemoGUI *gui = Denemo.gui;
  PrintStatus.invalid = 0;
  if (errors == -1)
    return;
  gchar *basename = g_path_get_basename(filename);
  gchar *filename_colon = g_strdup_printf("%s.ly%s", basename, ":");
  g_free(basename);
  gchar *epoint = NULL;
#define bufsize (1000)
  gchar *bytes = g_malloc0(bufsize);
  gint numbytes = read(errors, bytes, bufsize-1);
  close(errors);
  errors = -1;
#undef bufsize

  if(numbytes==-1) {
    g_free(bytes);
    return;
  }
  epoint = g_strstr_len (bytes, strlen(bytes), filename_colon);
  if(epoint) {
    gint line, column;
    gint cnv = sscanf(epoint+strlen(filename_colon), "%d:%d", &line, &column);
    truncate_lines(epoint);/* truncate epoint if it has too many lines */
    if(cnv==2) {
      line--;/* make this 0 based */
      if(line >= gtk_text_buffer_get_line_count(gui->textbuffer))
	warningdialog("Spurious line number"), line = 0;
      /* gchar *errmsg = g_strdup_printf("Error at line %d column %d %d", line,column, cnv); */
      /*     warningdialog(errmsg); */
      console_output(epoint);
      if(gui->textbuffer) {
        set_lily_error(line+1, column, gui);
      }
      goto_lilypond_position (line+1, column);
      PrintStatus.invalid = 2;//print_is_valid = FALSE;
      if(Denemo.printarea)
        gtk_widget_queue_draw(Denemo.printarea);
     // FIXME this causes a lock-up     warningdialog("Typesetter detected errors. Cursor is position on the error point.\nIf in doubt delete and re-enter the measure.");
    }
    else {
      set_lily_error(0, 0, gui);
      warningdialog(epoint);
    }
  } else
    set_lily_error(0, 0, gui);/* line 0 meaning no line */
  highlight_lily_error(gui);
  g_free(filename_colon);
  if (lily_err != NULL)
    {
      if(*bytes)
				console_output(bytes);
      warningdialog("Could not execute lilypond - check Edit->preferences->externals->lilypond setting\nand lilypond installation");
      g_warning ("%s", lily_err->message);
      if(lily_err) g_error_free (lily_err);
      lily_err = NULL;
    }
  g_free(bytes);
}

static void
open_viewer(GPid pid, gint status, gchar *filename, gboolean is_png){
  if(PrintStatus.printpid==GPID_NONE)
    return;
  GError *err = NULL;
  gchar *printfile;
  gchar **arguments;
  progressbar_stop();
  g_spawn_close_pid (PrintStatus.printpid);
  PrintStatus.printpid = GPID_NONE;
  //normal_cursor();
  process_lilypond_errors(filename); 
#ifndef G_OS_WIN32
  //status check seems to fail on windows, and errors are not highlighted for windows.
  if(status) {
    g_warning/* a warning dialog causes deadlock in threaded version of program */("LilyPond engraver failed - See highlighting in LilyPond window (open the LilyPond window and right click to print)");
  } else
#endif
 {

  if (is_png)
    printfile = g_strconcat (filename, ".png", NULL);
  else
  	printfile = g_strconcat (filename, ".pdf", NULL);
  
 
  if(!g_file_test (printfile, G_FILE_TEST_EXISTS)) {
    //FIXME use filename in message
    g_warning ("Failed to find %s, check permissions", (gchar *) printfile);
    g_free(printfile);
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
  if (is_png){

    arguments = png;
  }
  else {

    arguments = pdf;  
  }
  if((!is_png && (Denemo.prefs.pdfviewer->len==0))||
     (is_png && (Denemo.prefs.imageviewer->len==0))) {
    gboolean ok =  run_file_association(printfile);
    if(!ok) {
      err = g_error_new(G_FILE_ERROR, -1, "Could not run file assoc for %s", is_png?".png":".pdf");
      g_warning("Could not run the file association for a %s file\n", is_png?".png":".pdf");
    }
  }
  else {
    g_spawn_async_with_pipes (locateprintdir (),		/* dir */
		   arguments, 
		   NULL,	/* env */
		   G_SPAWN_SEARCH_PATH, /* search in path for executable */
		   NULL,	/* child setup func */
		   NULL,		/* user data */		
		   &previewerpid, /* FIXME &pid see g_spawn_close_pid(&pid) */
		   NULL,
		   NULL,
		   NULL,
		   &err);
  }
  if (err != NULL) {
    if(Denemo.prefs.pdfviewer->len) {
      g_warning ("Failed to find %s", Denemo.prefs.pdfviewer->str);
      warningdialog("Cannot display: Check Edit->Preferences->externals\nfor your PDF viewer");
    } else 
      warningdialog(err->message);
    g_warning ("%s", err->message);
    if(err) g_error_free (err);
    err = NULL;
  }
  g_free(printfile);
  }
}


static void
open_pngviewer(GPid pid, gint status, gchar *filename){
      open_viewer(pid, status, filename, TRUE);
}

static void
open_pdfviewer(GPid pid, gint status, gchar *filename){
     open_viewer(pid, status, filename, FALSE);
}

static gint 
run_lilypond(gchar **arguments) {
  gint error = 0;
  if(PrintStatus.background==STATE_NONE)
		progressbar("Denemo Typesetting");
  g_spawn_close_pid (get_lily_version_pid);
  get_lily_version_pid = GPID_NONE;

  if(PrintStatus.printpid!=GPID_NONE) {
    if(confirm("Already doing a print", "Kill that one off and re-start?")) {
      if(PrintStatus.printpid!=GPID_NONE) //It could have died while the user was making up their mind...
        kill_process(PrintStatus.printpid);
      PrintStatus.printpid = GPID_NONE;
    }
    else {
      warningdialog ("Cancelled");
      error = -1;
      return error;
    }
  }
  if(lily_err) {
    g_warning("Old error message from launching lilypond still present - message was %s\nDiscarding...\n", 
	lily_err->message);
    g_error_free(lily_err);
    lily_err = NULL;
  }
  
  gboolean lilypond_launch_success =
  g_spawn_async_with_pipes (locateprintdir (),		/* dir */
		arguments,
		NULL,		/* env */
		G_SPAWN_SEARCH_PATH  | G_SPAWN_DO_NOT_REAP_CHILD,
		NULL,		/* child setup func */
		NULL,		/* user data */
		&PrintStatus.printpid,
	        NULL,
		NULL,		/* stdout */
#ifdef G_OS_WIN32
		NULL,
#else
		&errors,	/* stderr */
#endif
		&lily_err);
  if(lily_err) {
    g_warning("Error launching lilypond! Message is %s\n", lily_err->message);
    g_error_free(lily_err);
    lily_err = NULL;
    error = -1;
  }
  if(!lilypond_launch_success) {
    warningdialog("Error executing lilypond. Perhaps Lilypond is not installed");
    error = -1;
  }
  if(error)
    progressbar_stop();
   
  return error;
}

gboolean
stop_lilypond()
{
  if(PrintStatus.printpid!=GPID_NONE){
    kill_process(PrintStatus.printpid);
    PrintStatus.printpid = GPID_NONE;
  }
 return FALSE;//do not call again
}

static void generate_lilypond(gchar *lilyfile, gboolean part_only, gboolean all_movements) {
  DenemoGUI *gui = Denemo.gui;
  if(part_only)
    export_lilypond_part (lilyfile, gui, all_movements);
  else
    exportlilypond (lilyfile, gui,  all_movements);
}

static void
run_lilypond_for_pdf(gchar *filename, gchar *lilyfile) {
  /*arguments to pass to lilypond to create a pdf for printing */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };
  run_lilypond(arguments);
}
static unsigned file_get_mtime(gchar *filename) {
	struct stat thebuf;
  g_stat(filename, &thebuf);
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
	advance_printname();
  gchar *filename = PrintStatus.printbasename[PrintStatus.cycle];
  gchar *lilyfile = PrintStatus.printname_ly[PrintStatus.cycle];
  g_remove (lilyfile);
  PrintStatus.invalid = 0;
  generate_lilypond(lilyfile, part_only, all_movements);
  run_lilypond_for_pdf(filename, lilyfile);
}


/** 
 * Dialog function used to select measure range 
 *
 */

void
printrangedialog(DenemoGUI * gui){
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  GtkWidget *from_measure;
  GtkWidget *to_measure;
  
  dialog = gtk_dialog_new_with_buttons (_("Print Excerpt Range"),
	 GTK_WINDOW (Denemo.window),
	 (GtkDialogFlags) (GTK_DIALOG_MODAL |
	      GTK_DIALOG_DESTROY_WITH_PARENT),
	 GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
	 GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);
  
  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), hbox);
  
  gint max_measure =
  g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  label = gtk_label_new (_("Print from Measure"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
 
  from_measure =
  gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure),
			     (gdouble) gui->si->selection.firstmeasuremarked);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_measure =
  gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  //  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure),
			     (gdouble) gui->si->selection.lastmeasuremarked);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->selection.firstmeasuremarked =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
      gui->si->selection.lastmeasuremarked =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
      //gtk_widget_destroy (dialog);
    }
  else 
    {
      gui->si->selection.firstmeasuremarked = gui->si->selection.lastmeasuremarked = 0;
    }
  if(gui->si->selection.firstmeasuremarked) {
    gui->si->markstaffnum = gui->si->selection.firststaffmarked = 1;
    gui->si->selection.laststaffmarked = g_list_length(gui->si->thescore);
  }
  
  gtk_widget_destroy (dialog);
}



static
void rm_temp_files(gchar *file, gpointer free_only) {
  //g_print("\n%s Deleting temp file %s\n",free_only?"Not":"", file);
  if(!free_only)
    g_remove(file);
  g_free(file);
}

static
void print_finished(GPid pid, gint status, GList *filelist) {
  if(PrintStatus.printpid==GPID_NONE)
    return;
  open_pdfviewer (pid,status, (gchar *) get_printfile_pathbasename());
  g_debug("print finished\n");
  changecount = Denemo.gui->changecount;
  progressbar_stop();
}


void printpng_finished(GPid pid, gint status, GList *filelist) {
  g_debug("printpng_finished\n");
  g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
  g_list_free(filelist);
  g_spawn_close_pid (PrintStatus.printpid);
  PrintStatus.printpid = GPID_NONE;
  progressbar_stop();
  infodialog("Your png file has now been created");
}

static
void printpdf_finished(GPid pid, gint status, GList *filelist) {
  if(filelist) {
    g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
    g_list_free(filelist);
  }
  g_spawn_close_pid (PrintStatus.printpid);
  PrintStatus.printpid = GPID_NONE;
  progressbar_stop();
  infodialog("Your pdf file has now been created");
}

static
void prepare_preview(GPid pid, gint status, GList *filelist) {
  open_pngviewer(pid, status, (gchar *) get_printfile_pathbasename());
  printpng_finished(pid, status, (GList *) filelist);
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

  GList *filelist=NULL;
  
  /* get the intended resolution of the png */
  gchar *resolution = g_strdup_printf("-dresolution=%d",(int) Denemo.prefs.resolution);
 
  /* create temp file names */
  basename =  get_printfile_pathbasename();
  lilyfile = g_strconcat (basename, ".ly", NULL);
  epsfile = g_strconcat (filename, ".eps", NULL);
  epsfile2 = g_strconcat (filename, "-1.eps", NULL);
  texfile = g_strconcat (filename, "-systems.tex", NULL);
  texifile = g_strconcat (filename, "-systems.texi", NULL);
  countfile = g_strconcat (filename, "-systems.count", NULL);
 
  /* create a list of files that need to be deleted */ 
  filelist = g_list_append(filelist, lilyfile);
  filelist = g_list_append(filelist, epsfile);
  filelist = g_list_append(filelist, epsfile2);
  filelist = g_list_append(filelist, texfile);
  filelist = g_list_append(filelist, texifile);
  filelist = g_list_append(filelist, countfile);

  /* generate the lilypond file */
  gui->lilysync = G_MAXUINT;
  exportlilypond (lilyfile, gui, finish == (GChildWatchFunc)printpng_finished?TRUE:FALSE);
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
  if(finish) {
    gint error = run_lilypond(arguments);
    if(!error)
      g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)finish, (gchar *) filelist);
  } else {
    GError *err = NULL; 
    g_spawn_sync (locateprintdir (),		/* dir */
		arguments, NULL,	/* env */
		G_SPAWN_SEARCH_PATH, NULL,	/* child setup func */
		NULL,		/* user data */
		NULL,		/* stdout */
		NULL,		/* stderr */
		NULL, &err);
 //These are in tmpdir and can be used for the .eps file, so don't delete them   g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
    g_list_free(filelist);
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
  GList *filelist=NULL;

  basename =  get_printfile_pathbasename();
  lilyfile = g_strconcat (basename, ".ly", NULL);
  psfile = g_strconcat (filename, ".ps", NULL);

  /* create list of files that will need to be deleted */
  filelist = g_list_append(filelist, lilyfile);
  filelist = g_list_append(filelist, psfile);
 

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

  gint error = run_lilypond(arguments);
  if(error){
    g_spawn_close_pid (PrintStatus.printpid);
    PrintStatus.printpid = GPID_NONE;
    return;
  }

  g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)printpdf_finished, filelist);
}

static void
print_and_view(gchar **arguments) {

  run_lilypond(arguments);
  if(PrintStatus.printpid!=GPID_NONE) {
    g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)open_pdfviewer, (gchar *) get_printfile_pathbasename());
    while(PrintStatus.printpid!=GPID_NONE) {
      gtk_main_iteration_do(FALSE);
    }
  }
}

static gboolean initialize_typesetting(void) {
  return call_out_to_guile("(InitializeTypesetting)");
}

//callback to print the LilyPond text in the LilyPond View window
void print_lily_cb (GtkWidget *item, DenemoGUI *gui){

  if(initialize_typesetting()) {
    g_warning("InitializeTypesetting failed\n");
    return;
  }
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);

  FILE *fp = fopen(lilyfile, "w");
  if(fp){
    GtkTextIter startiter, enditer;
    gtk_text_buffer_get_start_iter (gui->textbuffer, &startiter);
    gtk_text_buffer_get_end_iter (gui->textbuffer, &enditer);
    gchar *lily = gtk_text_buffer_get_text (gui->textbuffer, &startiter, &enditer, FALSE);
    fprintf(fp, "%s", lily);
    fclose(fp);
    /* create arguments to pass to lilypond to create a pdf for printing */
    gchar *arguments[] = {
      Denemo.prefs.lilypath->str,
      "--pdf",
      "-o",
      filename,
      lilyfile,
      NULL
    };
    print_and_view(arguments);
  }
}

// Displaying Print Preview

static gboolean selecting = FALSE;
static gboolean offsetting = FALSE;
static gboolean padding = FALSE;

static gint curx, cury;// position of mouse pointer while during motion
static gint pointx, pointy,  markx, marky;//coordinates defining a selected region in print preview pane. These are set by left button press/release, with pointx, pointy being set to top left

static GdkCursor *busycursor;
static GdkCursor *arrowcursor;
static void busy_cursor(void) {
  if(gtk_widget_get_window(Denemo.printarea))
    gdk_window_set_cursor(gtk_widget_get_window(Denemo.printarea), busycursor);
}
static void normal_cursor(void) {
  if(gtk_widget_get_window(Denemo.printarea))
    gdk_window_set_cursor(gtk_widget_get_window(Denemo.printarea), arrowcursor);
}

/*void                user_function                      (EvPrintOperation       *evprintoperation,
                                                        GtkPrintOperationResult arg1,
                                                        gpointer                user_data)             : Run Last */
void printop_done(EvPrintOperation *printop, GtkPrintOperationResult arg1, GtkPrintSettings **psettings) {
     if(*psettings)
      g_object_unref(*psettings);
    *psettings = ev_print_operation_get_print_settings (printop);
    g_object_ref(*psettings);
    //g_print("Came away with uri %s\n", gtk_print_settings_get(*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI));
    set_current_scoreblock_uri(g_strdup(gtk_print_settings_get(*psettings, GTK_PRINT_SETTINGS_OUTPUT_URI)));
    if(PrintStatus.background&STATE_PAUSED) {
			if(Denemo.prefs.typesetrefresh)
						PrintStatus.updating_id = g_timeout_add(Denemo.prefs.typesetrefresh, (GSourceFunc)retypeset, NULL);
			else
						PrintStatus.updating_id = g_idle_add( (GSourceFunc)retypeset, NULL);
			PrintStatus.background &= ~STATE_PAUSED;
		}
    call_out_to_guile("(FinalizePrint)");
}
static gboolean
libevince_print(void) {
  GError *err = NULL;
  gchar *filename = PrintStatus.printname_pdf[PrintStatus.cycle];
  gchar *uri = g_filename_to_uri(filename, NULL, &err);

  if(err) {
    g_warning ("Malformed filename %s\n", filename);
    return -1;
  }

  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if(err) {
    g_warning ("Trying to print the pdf file %s gave an error: %s", uri, err->message);
    if(err)
			g_error_free (err);
    err = NULL;
    return -1;
  } else {
    static GtkPrintSettings *settings;
    if(settings==NULL)
      settings = gtk_print_settings_new();
    EvPrintOperation *printop = ev_print_operation_new (doc);    
    g_signal_connect(printop, "done", G_CALLBACK(printop_done), &settings);
    gtk_print_settings_set(settings, GTK_PRINT_SETTINGS_OUTPUT_URI, get_output_uri_from_scoreblock());
    ev_print_operation_set_print_settings (printop, settings);
    		
		if(PrintStatus.updating_id) {
			PrintStatus.background|=STATE_PAUSED;
			g_source_remove(PrintStatus.updating_id);//if this is not turned off the print preview thread hangs until it is.
			PrintStatus.updating_id = 0;
		}
		
    ev_print_operation_run (printop, NULL);
  }
  return 0;
}

gboolean print_typeset_pdf(void){
return libevince_print();
}
static void
set_printarea_doc(EvDocument *doc) {
  EvDocumentModel  *model;

  model = g_object_get_data(G_OBJECT(Denemo.printarea), "model");//there is no ev_view_get_model(), when there is use it
  if(model==NULL) {
    model = ev_document_model_new_with_document(doc);
    ev_view_set_model((EvView*)Denemo.printarea, model);
    g_object_set_data(G_OBJECT(Denemo.printarea), "model", model);//there is no ev_view_get_model(), when there is use it
  } else
  ev_document_model_set_document (model, doc);
  ev_document_model_set_dual_page (model, GPOINTER_TO_INT(g_object_get_data(G_OBJECT(Denemo.printarea), "Duplex")));
  Mark.width=0;//indicate that there should no longer be any Mark placed on the score
}

static void get_window_position(gint*x, gint* y) {
  GtkAdjustment * adjust = gtk_range_get_adjustment(GTK_RANGE(Denemo.printhscrollbar));
  *x = (gint) gtk_adjustment_get_value(adjust);
  adjust = gtk_range_get_adjustment(GTK_RANGE(Denemo.printvscrollbar));
  *y = gtk_adjustment_get_value(adjust);
}

   
//setting up Denemo.pixbuf so that parts of the pdf can be dragged etc.
static void set_denemo_pixbuf(void)  {
      GdkWindow *window = gtk_layout_get_bin_window (GTK_LAYOUT(Denemo.printarea));
      if(window) {
      gint width, height;
#if GTK_MAJOR_VERSION==2
      gdk_drawable_get_size(window, &width, &height);
      
      Denemo.pixbuf = gdk_pixbuf_get_from_drawable(NULL, window, 
                                                   NULL/*gdk_colormap_get_system ()*/, 0, 0, 0, 0,
                                                  width, height);
#else
      width = gdk_window_get_width(window);
      height = gdk_window_get_height(window);
      Denemo.pixbuf = gdk_pixbuf_get_from_window(window, 0,0, width,height);
#endif
	}
}
 
//over-draw the evince widget with padding etc ...
static gboolean overdraw_print(cairo_t *cr) {
   if(Denemo.pixbuf==NULL)
    set_denemo_pixbuf();
   if(Denemo.pixbuf==NULL)
    return FALSE;
  gint x, y;

  get_window_position(&x, &y);

  gint width, height;
  width = gdk_pixbuf_get_width( GDK_PIXBUF(Denemo.pixbuf));
  height = gdk_pixbuf_get_height( GDK_PIXBUF(Denemo.pixbuf));

 // cairo_scale( cr, Denemo.gui->si->preview_zoom, Denemo.gui->si->preview_zoom );
  cairo_translate( cr, -x, -y );
  gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(Denemo.pixbuf), -x, -y);
  if(Mark.width) {
    cairo_set_source_rgba( cr, 0.5, 0.5, 1.0 , 0.5);
    cairo_rectangle (cr, Mark.x, Mark.y, MARKER, MARKER );
    cairo_fill(cr);
  }
  if(PrintStatus.invalid/*!print_is_valid*/) {
		gchar *headline, *explanation;
		switch(PrintStatus.invalid) {
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
  cairo_set_source_rgba( cr, 0.5, 0.0, 0.0 , 0.4);
  cairo_set_font_size( cr, 48.0 );
  cairo_move_to( cr, 50,50 );
  cairo_show_text( cr, headline);
  cairo_set_font_size( cr, 18.0 );
  cairo_move_to( cr, 50,80 );
	cairo_show_text( cr,explanation);

	
	
  }
  	if(PrintStatus.updating_id && (PrintStatus.background!=STATE_NONE)) {
		cairo_set_source_rgba( cr, 0.5, 0.0, 0.5 , 0.3);
		cairo_set_font_size( cr, 64.0 );
		cairo_move_to( cr, 0, 0);
		cairo_rotate(cr, M_PI/4);
		cairo_move_to( cr, 200,80);
		if(PrintStatus.typeset_type==TYPESET_MOVEMENT)
			cairo_show_text( cr, _("Current Movement"));
		else if(PrintStatus.typeset_type==TYPESET_EXCERPT)
			cairo_show_text( cr, _("Excerpt Only"));
	}
	
return TRUE;
//fragments of code for drawing actively dragging and already dragged objects - this will need a list of displaced rectangles and an actively dragged one
//the list of dragged rectangles would be emptied on refreshing the pdf
  if(selecting)
    {gint w = ABS(markx-curx);
    gint h = ABS(marky-cury);
    cairo_set_source_rgb(cr, 0, 0, 1);
    cairo_rectangle( cr, markx,marky, w, h );
    cairo_fill( cr );
    }
  if(offsetting)
    {
      gint w = pointx-markx;
      gint h = pointy-marky;
      cairo_set_source_rgb(cr, 0.5, 0.5, 0.5);
      cairo_rectangle( cr, markx,marky, w, h );
      cairo_fill( cr );
      GdkWindow *window = gtk_layout_get_bin_window (GTK_LAYOUT(Denemo.printarea));
      gdk_draw_pixbuf(window, NULL, GDK_PIXBUF(Denemo.pixbuf),
        markx+x, marky+y, curx, cury,/* x, y in pixbuf, x,y in window */
        w,  h, GDK_RGB_DITHER_NONE,0,0);
    }
  if(padding)
    {
      gint pad = ABS(markx-curx);
      gint w = pointx-markx;
      gint h = pointy-marky;
      cairo_set_source_rgb(cr, 0.5, 0.5, 0.5);
      cairo_rectangle( cr, markx-pad/2, marky-pad/2, w+pad, h+pad);

      GdkWindow *window = gtk_layout_get_bin_window (GTK_LAYOUT(Denemo.printarea));
      gdk_draw_pixbuf(window, NULL, GDK_PIXBUF(Denemo.pixbuf),
		      markx+x, marky+y, markx, marky,/* x, y in pixbuf, x,y in window */
          w,  h, GDK_RGB_DITHER_NONE,0,0);
    }
return FALSE;
}
#if GTK_MAJOR_VERSION==3
gint
printarea_draw_event(GtkWidget *w, cairo_t *cr) {
return overdraw_print(cr);
}
#else
gint
printarea_draw_event (GtkWidget * widget, GdkEventExpose * event)
{
  /* Setup a cairo context for rendering and clip to the exposed region. */
  cairo_t *cr = gdk_cairo_create (event->window);
  gdk_cairo_region (cr, event->region);
  cairo_clip (cr);
  overdraw_print(cr);
  cairo_destroy(cr);
  return TRUE;
}
#endif

static void
set_printarea(GError **err) {
  GFile       *file;
  gchar *filename = PrintStatus.printname_pdf[PrintStatus.cycle];
  g_print("using %s\n", filename);
  if(PrintStatus.invalid==0)
		PrintStatus.invalid = (g_file_test(filename, G_FILE_TEST_EXISTS))?0:3;
  file = g_file_new_for_commandline_arg (filename);
  //g_free(filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, err);
  //gint x = 0, y = 0, hupper, hlower, vupper, vlower;//store current position for reloading
  //get_window_position(&x, &y, &hupper, &hlower, &vupper, &vlower);
  if(*err) {
    g_warning ("Trying to read the pdf file %s gave an error: %s", uri, (*err)->message);
    PrintStatus.invalid = 3;
    gtk_widget_queue_draw(Denemo.printarea);
	}
  else
    set_printarea_doc(doc);
	//this will fail if the printarea is not visible, so it would need to be re-triggered on showing the printarea  
   set_denemo_pixbuf();
  return;
}

static void
printview_finished(GPid pid, gint status, gboolean print) {
  progressbar_stop();
  g_print("background %d\n", PrintStatus.background);
  if(PrintStatus.background==STATE_NONE) {
		call_out_to_guile("(FinalizeTypesetting)");
		process_lilypond_errors((gchar *) get_printfile_pathbasename());
  }
  PrintStatus.printpid = GPID_NONE;
  GError *err = NULL;
  set_printarea(&err);
  if(!err && print)
     libevince_print();
  normal_cursor();


}

/* callback to print current part (staff) of score */
void
printpart_cb (GtkAction *action, gpointer param) {


  DenemoGUI *gui = Denemo.gui;
  if(gui->si->markstaffnum)
    if(confirm("A range of music is selected","Print whole file?")){
      gui->si->markstaffnum=0;
    }
  if((gui->movements && g_list_length(gui->movements)>1) && 
     (confirm("This piece has several movements", "Print this part from all of them?")))
    create_pdf(TRUE, TRUE);
  else
   create_pdf(TRUE, FALSE); 
  g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)open_pdfviewer  /*  GChildWatchFunc function */, 
	(gchar *) get_printfile_pathbasename());
}


static gboolean typeset(gboolean force) {

if((force) || (changecount!=Denemo.gui->changecount)) {
  if(initialize_typesetting()) {
      g_warning("InitializeTypesetting failed\n");
      return FALSE;
  }
  DenemoGUI *gui = Denemo.gui;
  gui->si->markstaffnum=0;//FIXME save and restore selection?    
  gui->lilycontrol.excerpt = FALSE;
  create_pdf(FALSE, TRUE);
  changecount = Denemo.gui->changecount;
  return TRUE;
  }
return FALSE;
}

static gboolean typeset_movement(gboolean force) {

if((force) || (changecount!=Denemo.gui->changecount)) {
  if(initialize_typesetting()) {
      g_warning("InitializeTypesetting failed\n");
      return FALSE;
  }
  DenemoGUI *gui = Denemo.gui;
  gui->si->markstaffnum=0;//FIXME save and restore selection?    
  gui->lilycontrol.excerpt = FALSE;
  create_pdf(FALSE, FALSE);
  return TRUE;
  }
return FALSE;
}

void
printpreview_cb (GtkAction *action, DenemoScriptParam* param) {
  (void)typeset(TRUE);
  g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)print_finished, NULL);
}

void refresh_print_view (void) {
  busy_cursor();
  if(typeset(FALSE))
    g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
  else
    normal_cursor();
}

static
void print_from_print_view(gboolean all_movements) {

  busy_cursor();
  if(all_movements?typeset(FALSE):typeset_movement(FALSE)) {
    g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(TRUE));
  }
  else {
    normal_cursor();
    libevince_print();//printview_finished (PrintStatus.printpid, 0, TRUE);
  }
}

void
printselection_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  if(gui->si->markstaffnum)
    create_pdf(FALSE, FALSE);
  else
    warningdialog(_("No selection to print"));
  g_child_watch_add (PrintStatus.printpid, (GChildWatchFunc)open_pdfviewer  /*  GChildWatchFunc function */, 
	(gchar *) get_printfile_pathbasename());
}

void
printexcerptpreview_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  if(!gui->si->markstaffnum) //If no selection has been made 
    printrangedialog(gui);  //Launch a dialog to get selection
  if(gui->si->selection.firstmeasuremarked){
    gui->lilycontrol.excerpt = TRUE;
    export_png((gchar *) get_printfile_pathbasename(), (GChildWatchFunc)prepare_preview, gui);
    gui->lilycontrol.excerpt = FALSE;
  }
}



static gchar *get_thumb_directory(void) {
  return g_build_filename (g_get_home_dir(), ".thumbnails", "large", NULL);
}

static gchar *get_thumb_printname(void) {
return g_build_filename (locateprintdir (), "denemothumb", NULL);
}
static gchar *
  get_thumbname (gchar *uri) {
  gchar *basethumbname = g_compute_checksum_for_string (G_CHECKSUM_MD5, uri, -1);
  gchar *thumbname = g_strconcat(basethumbname, ".png", NULL);
  g_free(basethumbname);
  return thumbname;
}
static gchar *thumbnailsdirN = NULL;
static gchar *thumbnailsdirL = NULL;
  
/*call back to finish thumbnail processing. */
static
void thumb_finished(GPid pid, gint status) {
  GError *err = NULL;
  g_spawn_close_pid (PrintStatus.printpid); 
  PrintStatus.printpid = GPID_NONE;
  gchar *printname = get_thumb_printname();
    gchar *printpng = g_strconcat(printname, ".png", NULL);
    GdkPixbuf *pbN = gdk_pixbuf_new_from_file_at_scale   (printpng, 128, -1, TRUE, &err);
    GdkPixbuf *pbL = gdk_pixbuf_new_from_file_at_scale   (printpng, 256, -1, TRUE, &err);
    
    //FIXME if pb->height>128 or 256 scale it down...
    if(pbN && pbL) {
      gchar *uri = g_strdup_printf("file://%s", Denemo.gui->filename->str);
      gchar *thumbname = get_thumbname (uri);

						unsigned mtime = file_get_mtime(Denemo.gui->filename->str);
            //struct stat thebuf;
            //gint status =  g_stat(Denemo.gui->filename->str, &thebuf);
           // unsigned mtime = thebuf.st_mtime;
            //g_print("the mt is %u\n", mtime);
            
 

            gchar * thumbpathN = g_build_filename(thumbnailsdirN, thumbname, NULL);
            gchar * thumbpathL = g_build_filename(thumbnailsdirL, thumbname, NULL);
           
            gchar *mt = g_strdup_printf("%u", mtime);
            if(!gdk_pixbuf_save (pbN, thumbpathN, "png"/*type*/, &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt , NULL))
              g_print("%s\n", err->message);
            err = NULL;
            if(!gdk_pixbuf_save (pbL, thumbpathL, "png"/*type*/, &err, "tEXt::Thumb::URI", uri, "tEXt::Thumb::MTime", mt , NULL))
              g_print("%s\n", err->message);

              //FIXME do the pbN L need freeing???
            g_free(uri);
            g_free(mt);
            g_free(thumbname);
            g_free(thumbpathN);
            g_free(thumbpathL);
    }
    g_free(printname);
    PrintStatus.printpid=GPID_NONE;
    progressbar_stop();
    //g_print("Set PrintStatus.printpid = %d\n", PrintStatus.printpid);
  }

// large_thumbnail_name takes a full path name to a .denemo file and returns the full path to the large thumbnail of that .denemo file. Caller must g_free the returned string
gchar * large_thumbnail_name(gchar *filepath) {
  gchar *temp = g_strdup_printf("file://%s", filepath);
  gchar *ret = get_thumbname(temp);
  g_free(temp);
  return g_build_filename(get_thumb_directory(), ret, NULL);
}

/***
 *  Create a thumbnail for Denemo.gui if needed
 */
gboolean
create_thumbnail(gboolean async) {
  GError *err = NULL;
  if(PrintStatus.printpid!=GPID_NONE)
    return FALSE;
  if(Denemo.gui->filename->len) {
    if(!thumbnailsdirN) {
      thumbnailsdirN = g_build_filename (g_get_home_dir(), ".thumbnails", "normal", NULL);
      g_mkdir_with_parents(thumbnailsdirN, 0700);
    }
  if(!thumbnailsdirL) {
    thumbnailsdirL = g_build_filename (g_get_home_dir(), ".thumbnails", "large", NULL);
    g_mkdir_with_parents(thumbnailsdirL, 0700);
    }
//check if thumbnail is newer than file
  struct stat thebuf;
  gint status =  g_stat(Denemo.gui->filename->str, &thebuf);
  unsigned mtime = thebuf.st_mtime;
  gchar *uri = g_strdup_printf("file://%s", Denemo.gui->filename->str);
  gchar *thumbname = get_thumbname (uri);
  gchar * thumbpathN = g_build_filename(thumbnailsdirN, thumbname, NULL);
  thebuf.st_mtime = 0;
  status =  g_stat(thumbpathN, &thebuf);
  unsigned mtime_thumb = thebuf.st_mtime;
  if(mtime_thumb<mtime) {
    gint saved = g_list_index(Denemo.gui->movements, Denemo.gui->si);
    Denemo.gui->si = Denemo.gui->movements->data;//Thumbnail is from first movement
//set selection to thumbnailselection, if not set, to the selection, if not set to first three measures of staff 1
    if(Denemo.gui->thumbnail.firststaffmarked) 
      memcpy(&Denemo.gui->si->selection, &Denemo.gui->thumbnail, sizeof(DenemoSelection));
    else
      if(Denemo.gui->si->selection.firststaffmarked)
        memcpy(&Denemo.gui->thumbnail, &Denemo.gui->si->selection, sizeof(DenemoSelection));
        else {
          Denemo.gui->thumbnail.firststaffmarked = 1;
          Denemo.gui->thumbnail.laststaffmarked = 3;
          Denemo.gui->thumbnail.firstmeasuremarked = 1;
          Denemo.gui->thumbnail.lastmeasuremarked = 3;
          Denemo.gui->thumbnail.firstobjmarked = 0;
          Denemo.gui->thumbnail.lastobjmarked = 100;//or find out how many there are
          memcpy(&Denemo.gui->si->selection, &Denemo.gui->thumbnail, sizeof(DenemoSelection));
        }
    Denemo.gui->si->markstaffnum = Denemo.gui->si->selection.firststaffmarked;
    gchar * printname = get_thumb_printname();
    Denemo.gui->lilycontrol.excerpt = TRUE;

    if(async){
      gchar *arguments[] = {
      g_build_filename(get_bin_dir(), "denemo", NULL),
        "-n", "-a", "(d-CreateThumbnail #f)(d-Exit)",
      Denemo.gui->filename->str,
      NULL
    };

    g_spawn_async_with_pipes (NULL,		/* any dir */
		arguments, NULL,	/* env */
		G_SPAWN_SEARCH_PATH, NULL,	/* child setup func */
		NULL,		/* user data */
        NULL, /* pid */
		NULL,		/* stdin */
		NULL,		/* stdout */
		NULL,		/* stderr */
		 &err);
    } else {
      export_png(printname, NULL, Denemo.gui);
      thumb_finished (PrintStatus.printpid, 0);
    }
    
    g_free(printname);
    Denemo.gui->si = g_list_nth_data(Denemo.gui->movements, saved);
    if(Denemo.gui->si==NULL)
      Denemo.gui->si = Denemo.gui->movements->data;
  }
  }
  return TRUE;
}

/* callback to print whole of score */
void
printall_cb (GtkAction *action, gpointer param) {
    print_from_print_view(TRUE);
}

/* callback to print movement of score */
void
printmovement_cb (GtkAction *action, gpointer param) {
    changecount = -1;
    print_from_print_view(FALSE);
    changecount = Denemo.gui->changecount;
}

#if 0
static gint 
drag_selection(void) {
  offsetting = TRUE;
  return TRUE;
}
static gint 
start_drag(GtkWidget *widget, gboolean *flag) {
  *flag = TRUE;
  return TRUE;
}
#endif
static void create_all_pdf(void) {

busy_cursor();
create_pdf(FALSE, TRUE);
g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
}
static void create_movement_pdf(void) {

busy_cursor();
create_pdf(FALSE, FALSE);
g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
}
static void create_part_pdf(void) {

busy_cursor();
create_pdf(TRUE, TRUE);
g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
}
#if 0
static gint 
popup_print_preview_menu(void) {
  GtkWidget *menu = gtk_menu_new();
  GtkWidget *item = gtk_menu_item_new_with_label("Print");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect_swapped(G_OBJECT(item), "activate", G_CALLBACK(print_from_print_view), GINT_TO_POINTER(TRUE));
  item = gtk_menu_item_new_with_label("Refresh Typesetting");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(force_typeset),NULL);

  item = gtk_menu_item_new_with_label("Typeset Score");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(create_all_pdf),NULL);
  item = gtk_menu_item_new_with_label("Typeset Part");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(create_part_pdf),NULL);

#if 1
  item = gtk_menu_item_new_with_label("Drag to desired offset");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(start_drag), &offsetting);

  item = gtk_menu_item_new_with_label("Drag a space for padding");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(start_drag), &padding);
#endif

  gtk_widget_show_all(menu);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 
  return TRUE;
}
#endif

static gint
goto_position (EvView* view, EvLinkAction *obj)
{

  gchar *uri = (gchar*)ev_link_action_get_uri(obj);
  //g_print("external signal %s\n", uri);
  gchar **vec = g_strsplit (uri, ":",5);
  if(!strcmp(vec[0], "textedit") && vec[1] && vec[2] && vec[3]) {
      ObjectLocated = goto_lilypond_position(atoi(vec[2]), atoi(vec[3]));
  } else {
  g_warning ("Cannot follow external link type %s\n", vec[0]);
  }
  g_strfreev(vec);
return TRUE;//we do not want the evince widget to handle this.
}
static gint adjust_x=0;
static gint adjust_y=0;

#if 0
static gint
printarea_focus_in_event (GtkWidget * widget, GdkEventExpose * event)
{
  if((PrintStatus.printpid==GPID_NONE) && (changecount!=Denemo.gui->changecount)) {
    refresh_print_view();
    changecount = Denemo.gui->changecount;// keep track so we know if update is needed FIXME - different tabs etc
  }
  return TRUE;
}
#endif



//GdkBitmap *graphic = NULL; /* a selection from the print area */

static gint
printarea_motion_notify (GtkWidget * widget, GdkEventButton * event)
{
  ObjectLocated = FALSE;
   if(Denemo.pixbuf==NULL)
    set_denemo_pixbuf();
  if(Denemo.pixbuf==NULL)
    return TRUE;
  if(padding || offsetting || selecting) {
    curx = (int)event->x;
    cury = (int)event->y;
    gtk_widget_queue_draw (Denemo.printarea);
  }

  return FALSE;
}




static void normalize(void){
  if(pointx<markx) {
    gint temp=pointx;
    pointx=markx;
    markx=temp;
  }
  if(pointy<marky) {
    gint temp=pointy;
    pointy=marky;
    marky=temp;
  }
  if(markx==pointx)
    pointx++;
  if(marky==pointy)
    pointy++;

}

#if 0
static gint
printarea_button_release (GtkWidget * widget, GdkEventButton * event)
{
g_print("release\n");
 if(padding) {
    gint pad = ABS(curx - markx); 

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.printarea), "pad-dialog");
    g_object_set_data(G_OBJECT(Denemo.printarea), "padding", (gpointer)pad);
    if(thedialog){
      gtk_dialog_response(GTK_DIALOG(thedialog), 1/*DRAGGED*/);
    } else { 
      gchar *msg = g_strdup_printf("You have chosen a padding tweak of %d\nYour printed output will not change until you put this tweak into the corresponding Denemo directive\nUse Edit Directive to do this.", pad);
      warningdialog(msg);
      g_free(msg);
    
    padding = FALSE;
    return TRUE;
 }
 }

  return TRUE;
}

static gint
printarea_button_press (GtkWidget * widget, GdkEventButton * event)
{
//the old code - for dragging in the view window
 gboolean left = (event->button != 3);
  if((!left)) {
    if(PrintStatus.printpid==GPID_NONE)
      popup_print_preview_menu();
    return TRUE;
  }
  /* creating an offset? */
  if(offsetting) {
    offsetx = curx - markx;
    offsety = cury - marky;  

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.printarea), "offset-dialog");
    g_object_set_data(G_OBJECT(Denemo.printarea), "offsetx", GINT_TO_POINTER(offsetx));
    g_object_set_data(G_OBJECT(Denemo.printarea), "offsety", GINT_TO_POINTER(offsety));
    if(thedialog){
      gtk_dialog_response(GTK_DIALOG(thedialog), 1/*DRAGGED*/);
    } else { 
      gchar *msg = g_strdup_printf("You have chosen a offset tweak of %d, %d\nYour printed output will not change until you put this tweak into the corresponding Denemo directive\nUse Edit Directive to do this.", offsetx, -offsety);
      warningdialog(msg);
      g_free(msg);
    }
    offsetting = FALSE;
    return TRUE;
  }
  /*( creating a padding value? */
  if(padding) {
    gint pad = ABS(curx - markx); 

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.printarea), "pad-dialog");
    g_object_set_data(G_OBJECT(Denemo.printarea), "padding", GINT_TO_POINTER(pad));
    if(thedialog){
      gtk_dialog_response(GTK_DIALOG(thedialog), 1/*DRAGGED*/);
    } else { 
      gchar *msg = g_strdup_printf("You have chosen a padding tweak of %d\nYour printed output will not change until you put this tweak into the corresponding Denemo directive\nUse Edit Directive to do this.", pad);
      warningdialog(msg);
      g_free(msg);
    }
    padding = FALSE;
    return TRUE;
  }
  selecting = TRUE;
//  if(Denemo.pixbuf==NULL)
//    return TRUE;
  pointx = markx=event->x;
  pointy = marky=event->y;

  return TRUE;
}
#endif


static gint
printarea_button_release (GtkWidget * widget, GdkEventButton * event)
{
	if(Denemo.pixbuf==NULL)
    set_denemo_pixbuf();
  if(Denemo.pixbuf==NULL)
    return TRUE;
  gboolean left = (event->button != 3);
  if(left && ObjectLocated) {
    GdkWindow *window = gtk_layout_get_bin_window (GTK_LAYOUT(Denemo.printarea));
    gint x, y;
    //g_print("reading position of mark");
    get_window_position(&x, &y);
    if(Mark.width) {
      Mark.x -=x;
      Mark.y -=y;
      gdk_window_invalidate_rect(window,&Mark,TRUE); 
    }

    Mark.x = (gint)event->x-MARKER/2;
    Mark.y =  (gint)event->y-MARKER/2;
    Mark.width = Mark.height = MARKER;
    gdk_window_invalidate_rect(window,&Mark,TRUE);
    Mark.x +=x;
    Mark.y +=y;
    switch_back_to_main_window();
    }
return TRUE;

//This code for use later when dragging an object
  g_print("selecting %d\n", selecting);

  if(selecting) {
    pointx=event->x;
    pointy=event->y;
    gint width, height;
    normalize();

    width = pointx-markx;
    height = pointy-marky;
    GtkIconFactory *icon_factory = gtk_icon_factory_new ();
    if(marky+adjust_y<0 || (marky+adjust_y + height > gdk_pixbuf_get_height(Denemo.pixbuf)))
      return TRUE;
    GdkPixbuf *sub_pixbuf = gdk_pixbuf_new_subpixbuf (Denemo.pixbuf, markx+adjust_x, marky+adjust_y, width, height);

    GdkPixbuf *alphapixbuf = gdk_pixbuf_add_alpha (sub_pixbuf, TRUE, 255, 255, 255);
    GdkPixbuf *scaledpixbuf = gdk_pixbuf_scale_simple(alphapixbuf, width, height,GDK_INTERP_BILINEAR);
    if(scaledpixbuf) {
      gchar *data =  create_xbm_data_from_pixbuf(scaledpixbuf, 0, 0, width, height);

      GtkIconSet *icon_set = gtk_icon_set_new_from_pixbuf (sub_pixbuf);
      g_object_unref(sub_pixbuf);
      gtk_icon_factory_add (icon_factory, "Save Graphic", icon_set);
      gtk_icon_factory_add_default    (icon_factory);
      g_object_unref(alphapixbuf);
      if(data) {
	if(Denemo.gui->xbm)
	  g_free(Denemo.gui->xbm);
	Denemo.gui->xbm = data;
	Denemo.gui->xbm_width = width;
	Denemo.gui->xbm_height = height;
      }
    }
  }
  selecting = FALSE;
  return TRUE;
}

// PrintStatus.mtime = file_get_mtime(filename); use in get_printfile_pathbasename

static
void typeset_control(GtkWidget*dummy, gpointer data) {
  static gpointer last_data=NULL;
  static GString *last_script=NULL;
  gint markstaff = Denemo.gui->si->markstaffnum;
  Denemo.gui->si->markstaffnum = 0;
 	if(PrintStatus.background!=STATE_ON)
		PrintStatus.background=0; //STATE_NONE
  if(last_script==NULL)
    last_script=g_string_new("(d-PrintView)");
   
  if(data==create_all_pdf)
    create_all_pdf();
  else if(data==create_movement_pdf)
    create_movement_pdf();
  else if(data==create_part_pdf)
    create_part_pdf();
  else if(data!=NULL) {	
			if(PrintStatus.background==STATE_ON) {
				save_selection(Denemo.gui->si);
				if(PrintStatus.typeset_type==TYPESET_ALL_MOVEMENTS) {
					Denemo.gui->si->markstaffnum = 0;
					create_pdf(FALSE, TRUE);
				} else if(PrintStatus.typeset_type==TYPESET_MOVEMENT) 	{
					Denemo.gui->si->markstaffnum = 0;
					create_pdf(FALSE, FALSE);
				} else {
				gint value = Denemo.gui->si->currentstaffnum - PrintStatus.first_staff;
				if(value<1) value = 1;
				Denemo.gui->si->markstaffnum = Denemo.gui->si->selection.firststaffmarked = value;
				
				value = Denemo.gui->si->currentstaffnum + PrintStatus.last_staff;
				if(value<1) value = 1;		
				Denemo.gui->si->selection.laststaffmarked = value;
				
				value = Denemo.gui->si->currentmeasurenum - PrintStatus.first_measure;
				if(value<1) value = 1;
				Denemo.gui->si->selection.firstmeasuremarked = value;
				
				value = Denemo.gui->si->currentmeasurenum + PrintStatus.last_measure;
				if(value<1) value = 1;
				Denemo.gui->si->selection.lastmeasuremarked = value;
				
				Denemo.gui->si->selection.firstobjmarked = 0;Denemo.gui->si->selection.lastobjmarked = G_MAXINT-1;//counts from 0, +1 must be valid
				create_pdf(FALSE, FALSE);//this movement only cursor-relative selection of measures	
			} 			
		} else	{	
			busy_cursor();
			create_pdf(FALSE, TRUE);
		}
    g_string_assign(last_script, data);
    last_data = NULL;
    g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
    if(PrintStatus.background==STATE_ON) {
				restore_selection(Denemo.gui->si);
		}    
    Denemo.gui->si->markstaffnum = markstaff; 
    return;
  } else { //data is NULL, repeat last typeset
    if(last_data) {
          ((void (*)())last_data)();
          Denemo.gui->si->markstaffnum = markstaff; 
          return;
    } else if(last_script->len) {

				busy_cursor();
      call_out_to_guile(last_script->str);
      g_child_watch_add(PrintStatus.printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));

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
void show_print_view(GtkAction *action, gpointer param) {
  GtkWidget *w =  gtk_widget_get_toplevel(Denemo.printarea);
  if(gtk_widget_get_visible(w))
    gtk_window_present(GTK_WINDOW(w));
  else
    gtk_widget_show(w);
  if(action && (changecount!=Denemo.gui->changecount || Denemo.gui->lilysync != Denemo.gui->changecount) ) {

    if(!initialize_typesetting())
      typeset_control(NULL, create_all_pdf);	
  }
}

/* typeset the score, and store the passed script for refresh purposes*/
gboolean typeset_for_script(gchar *script) {
typeset_control(NULL, script);
busy_cursor();
show_print_view(NULL, NULL);
return TRUE;
}

static void page_display(GtkWidget *button, gint page_increment) {
  gint i;
  for(i=0;i<page_increment;i++)
    ev_view_next_page((EvView*)Denemo.printarea);
  for(i=0;i>page_increment;i--)
    ev_view_previous_page((EvView*)Denemo.printarea);
}
static void dual_page(GtkWidget *button) {
  GError *err = NULL;
g_object_set_data(G_OBJECT(Denemo.printarea), "Duplex", GINT_TO_POINTER(!g_object_get_data(G_OBJECT(Denemo.printarea), "Duplex")));
//refresh...
//  EvDocumentModel  *model = ev_view_get_model((EvView*)Denemo.printarea);
//  ev_document_model_set_dual_page (model, (gboolean)g_object_get_data(G_OBJECT(Denemo.printarea), "Duplex"));
  set_printarea(&err);
}

#if 0
gint
printarea_scroll_event (GtkWidget *widget, GdkEventScroll *event) {
  switch(event->direction) {
    case GDK_SCROLL_UP:
      g_print("scroll up event\n");
      break;
    case GDK_SCROLL_DOWN:
      g_print("scroll down event\n");
      break;
  }
return FALSE;
}
#endif
#define MANUAL _("Manual Updates")
#define CONTINUOUS _("Continuous")
static void typeset_action(GtkWidget *button, gpointer data) {
  initialize_typesetting();
  typeset_control(NULL, data);
}

static gboolean retypeset(void) {
static gint firstmeasure, lastmeasure, firststaff, laststaff, movementnum;
 DenemoScore *si = Denemo.gui->si;
 if((PrintStatus.printpid==GPID_NONE) &&
	 (gtk_widget_get_visible(gtk_widget_get_toplevel(Denemo.printarea)))) {
	 if(PrintStatus.typeset_type==TYPESET_ALL_MOVEMENTS) {
				if(changecount != Denemo.gui->changecount) {
					PrintStatus.background = STATE_ON;
					typeset_control(NULL, "(disp \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
					PrintStatus.background = STATE_OFF;
					changecount = Denemo.gui->changecount;
				}
  	} else if((changecount != Denemo.gui->changecount) ||
							(si->currentmovementnum!=movementnum) || 
							((PrintStatus.typeset_type==TYPESET_EXCERPT) && 
									(si->currentmeasurenum<firstmeasure || 
									si->currentmeasurenum>lastmeasure || 
									si->currentstaffnum<firststaff || 
									si->currentstaffnum>laststaff))) {
		firstmeasure = si->currentmeasurenum-PrintStatus.first_measure;
		if(firstmeasure<0) firstmeasure = 0;
		lastmeasure = si->currentmeasurenum+PrintStatus.last_measure;
		firststaff = si->currentstaffnum-PrintStatus.first_staff;
		if(firststaff<0) firststaff = 0;
		laststaff = si->currentstaffnum+PrintStatus.last_staff;
		movementnum = si->currentmovementnum;
		PrintStatus.background = STATE_ON;
		typeset_control(NULL, "(disp \"This is called when hitting the refresh button while in continuous re-typeset\")(d-PrintView)");
		PrintStatus.background = STATE_OFF;
		changecount = Denemo.gui->changecount;
	}
 }
 return TRUE;//continue
}
//turn the continuous update off and on
static void toggle_updates(GtkWidget *menu_item, GtkWidget *button) {
 if(PrintStatus.updating_id) {
	 g_source_remove(PrintStatus.updating_id);
	 PrintStatus.updating_id = 0;
	 gtk_button_set_label(GTK_BUTTON(button), MANUAL);
	 if(Denemo.prefs.persistence)
		Denemo.prefs.manualtypeset = TRUE;
 } else {
		if(Denemo.prefs.typesetrefresh)
			PrintStatus.updating_id = g_timeout_add(Denemo.prefs.typesetrefresh, (GSourceFunc)retypeset, NULL);
	 else
	 	 PrintStatus.updating_id = g_idle_add( (GSourceFunc)retypeset, NULL);
	 gtk_button_set_label(GTK_BUTTON(button), CONTINUOUS);
	 if(Denemo.prefs.persistence)
		Denemo.prefs.manualtypeset = FALSE;
 }
}

static void
set_typeset_type(GtkWidget *radiobutton) {
	if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(radiobutton))) {
		changecount = 0;//reset so that a retype occurs
		gint index = g_slist_index(gtk_radio_button_get_group(GTK_RADIO_BUTTON(radiobutton)), radiobutton);
		g_print("Get %s at %d\n", gtk_button_get_label(GTK_BUTTON(radiobutton)), index);
		switch(index) {
				case 0:
					PrintStatus.typeset_type = TYPESET_EXCERPT;
					break;
				case 1:
					PrintStatus.typeset_type = TYPESET_MOVEMENT;
					break;
				case 2:
					PrintStatus.typeset_type= TYPESET_ALL_MOVEMENTS;
		}
		if(Denemo.prefs.persistence)
			Denemo.prefs.typesettype = PrintStatus.typeset_type;
	}
}

static void value_change(GtkWidget *spinner, gint *value) {
	*value = gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON(spinner));
	if(Denemo.prefs.persistence) {
			Denemo.prefs.firstmeasure = PrintStatus.first_measure;
			Denemo.prefs.lastmeasure = PrintStatus.last_measure;
			Denemo.prefs.firststaff = PrintStatus.first_staff;
			Denemo.prefs.laststaff = PrintStatus.last_staff;
	}
}
static void range_dialog(void) {
	static GtkWidget *dialog;
	if(dialog==NULL) {

		
			dialog = gtk_dialog_new();
			GtkWidget *area = gtk_dialog_get_action_area (GTK_DIALOG(dialog));
			GtkWidget *vbox = gtk_vbox_new(FALSE, 1);
			gtk_container_add(GTK_CONTAINER(area), vbox);
			GtkWidget *hbox = gtk_hbox_new(FALSE, 1);
			gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
			
			GtkWidget *label = gtk_label_new(_("Measures before cursor:"));
			gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 8);
			GtkWidget *spinner = gtk_spin_button_new_with_range(0,1000,1);
			g_signal_connect(spinner, "value-changed", (GCallback)value_change, &PrintStatus.first_measure);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON(spinner), PrintStatus.first_measure);
			
			gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
			
			
			label = gtk_label_new(_("Measures after cursor:"));
			gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 8);
			spinner = gtk_spin_button_new_with_range(0,1000,1);
			g_signal_connect(spinner, "value-changed", (GCallback)value_change, &PrintStatus.last_measure);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON(spinner), PrintStatus.last_measure);
			
			gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
			
			hbox = gtk_hbox_new(FALSE, 1);
			gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
			
			label = gtk_label_new(_("Staffs before cursor:"));
			gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 8);
			spinner = gtk_spin_button_new_with_range(0,100,1);
			g_signal_connect(spinner, "value-changed", (GCallback)value_change, &PrintStatus.first_staff);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON(spinner), PrintStatus.first_staff);
			
			gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);
			
			label = gtk_label_new(_("Staffs after cursor:"));
			gtk_box_pack_start(GTK_BOX(hbox), label, FALSE, TRUE, 8);
			spinner = gtk_spin_button_new_with_range(0,100,1);
			g_signal_connect(spinner, "value-changed", (GCallback)value_change, &PrintStatus.last_staff);
			gtk_spin_button_set_value (GTK_SPIN_BUTTON(spinner), PrintStatus.last_staff);
			
			gtk_box_pack_start(GTK_BOX(hbox), spinner, TRUE, TRUE, 0);

			hbox = gtk_hbox_new(FALSE, 1);
			gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);

			hbox = gtk_hbox_new(FALSE, 1);
			gtk_box_pack_start(GTK_BOX(vbox), hbox, TRUE, TRUE, 0);
			
			
			GtkWidget *button0 = gtk_radio_button_new_with_label_from_widget (NULL,  _("All Movements"));
			g_signal_connect(G_OBJECT(button0), "toggled", G_CALLBACK(set_typeset_type), NULL);
			gtk_widget_set_tooltip_text(button0, _("If checked the current layout is re-typeset at every change"));
			gtk_box_pack_start(GTK_BOX(hbox), button0, TRUE, TRUE, 0);
			
			GtkWidget *button1 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON(button0), _("Current Movement"));
			g_signal_connect(G_OBJECT(button1), "toggled", G_CALLBACK(set_typeset_type), NULL);
			gtk_widget_set_tooltip_text(button1, _("If checked the current movement is re-typeset at every change"));
			gtk_box_pack_start(GTK_BOX(hbox), button1, TRUE, TRUE, 0);

			 
			GtkWidget *button2 = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON(button0),	_("Cursor Context"));
			g_signal_connect(G_OBJECT(button2), "toggled", G_CALLBACK(set_typeset_type), NULL);
			gtk_widget_set_tooltip_text(button2, _("If checked the range around the current cursor position is re-typeset at every change or when the cursor moves out of range."));
			gtk_box_pack_start(GTK_BOX(hbox), button2, TRUE, TRUE, 0);
			if(Denemo.prefs.typesettype==TYPESET_MOVEMENT)
			 gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button1), TRUE);
			if(Denemo.prefs.typesettype==TYPESET_EXCERPT)
			 gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(button2), TRUE);
			 
			g_signal_connect(dialog, "delete-event", G_CALLBACK(gtk_widget_hide_on_delete), NULL);
			gtk_widget_show_all(dialog);
		} else
		gtk_widget_show(dialog);
	
}

static GtkWidget *get_updates_menu(GtkWidget *button) {
		static GtkWidget *menu;
	if(menu==NULL) {
		GtkWidget *item;
		menu = gtk_menu_new();
		item = gtk_check_menu_item_new_with_label(CONTINUOUS);
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		gtk_widget_set_tooltip_text(item, _("Set background updats on/off."));
		g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(toggle_updates), button);
		
		gtk_check_menu_item_set_active(GTK_CHECK_MENU_ITEM(item), !Denemo.prefs.manualtypeset);
		item = gtk_menu_item_new_with_label(_("Range"));
		gtk_widget_set_tooltip_text(item, _("Set how much of the score to re-draw."));	
		gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
		g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(range_dialog), NULL);
		gtk_widget_show_all(menu);
	}
	return menu;
}

static void updates_menu(GtkWidget *button) {	
  gtk_menu_popup (GTK_MENU(get_updates_menu(button)), NULL, NULL, NULL, NULL, 0, gtk_get_current_event_time());
}

static GtkWidget *get_updates_button(void) {
  GtkWidget *button = gtk_button_new_with_label(MANUAL);
  gtk_widget_set_tooltip_text(button, _("Set background updater on/off. This controls if typesetting is re-done after each change to the music."));
  g_signal_connect(button, "clicked", G_CALLBACK(updates_menu), NULL);
  return button;	
}

void install_printpreview(DenemoGUI *gui, GtkWidget *top_vbox){ 
  if(Denemo.printarea)
    return;
  PrintStatus.typeset_type=Denemo.prefs.typesettype; 
	PrintStatus.first_measure=Denemo.prefs.firstmeasure; 
	PrintStatus.last_measure=Denemo.prefs.lastmeasure; 
	PrintStatus.first_staff=Denemo.prefs.firststaff;
	PrintStatus.last_staff=Denemo.prefs.laststaff;
  busycursor = gdk_cursor_new(GDK_WATCH);
  arrowcursor = gdk_cursor_new(GDK_RIGHT_PTR);

  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  GtkWidget *main_hbox =  gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), main_hbox,FALSE, TRUE, 0);
  GtkWidget *hbox =  gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_hbox), hbox,FALSE, TRUE, 0);
  GtkWidget *button = gtk_button_new_with_label(_("Typeset"));
  gtk_widget_set_tooltip_text(button, "Typesets the music using the current score layout. See View->Score Layouts to see which layouts you have created and which is currently selected.");
  g_signal_connect(button, "clicked", G_CALLBACK(typeset_action), create_all_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label(_("Print"));
  gtk_widget_set_tooltip_text(button, "Pops up a Print dialog. From this you can send your typeset score to a printer or to a PDF file.");
  g_signal_connect(button, "clicked", G_CALLBACK(libevince_print), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

    

  button = gtk_button_new_with_label(_("Movement"));
  gtk_widget_set_tooltip_text(button, _("Typesets the music from the current movement. This creates a score layout comprising one movement."));
  g_signal_connect(button, "clicked", G_CALLBACK(typeset_action), create_movement_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
 
  button = gtk_button_new_with_label(_("Part"));
  gtk_widget_set_tooltip_text(button, _("Typesets the music from the current part for all movements. A part is all the music with the same staff-name. This creates a score layout with one part, all movements."));
  g_signal_connect(button, "clicked", G_CALLBACK(typeset_action), create_part_pdf);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label(_("Refresh"));
  gtk_widget_set_tooltip_text(button, _("Re-issues the last print command. Use this after modifying the file to repeat the typesetting."));
  g_signal_connect(button, "clicked", G_CALLBACK(typeset_action), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  
  button = get_updates_button();
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
 (void)get_updates_menu(button);//this is to initialize the continuous/manual state
  hbox =  gtk_hbox_new (FALSE, 1);
  gtk_box_pack_end (GTK_BOX (main_hbox), hbox,FALSE, TRUE, 0);


  button = gtk_button_new_with_label("Duplex");
  gtk_widget_set_tooltip_text(button, _("Shows pages side by side, so you can see page turns for back-to-back printing\n"));
  g_signal_connect(button, "clicked", G_CALLBACK(dual_page), NULL);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  button = gtk_button_new_with_label(_("Next"));
  gtk_widget_set_tooltip_text(button, _("Move to the next page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect(button, "clicked", G_CALLBACK(page_display), (gpointer) 1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);
  button = gtk_button_new_with_label(_("Previous"));
  gtk_widget_set_tooltip_text(button, _("Move to the previous page - you can also scroll with the scroll-wheel, and zoom with control-wheel"));
  g_signal_connect(button, "clicked", G_CALLBACK(page_display), (gpointer) -1);
  gtk_box_pack_start (GTK_BOX (hbox), button, FALSE, TRUE, 0);

  
  top_vbox = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(top_vbox),_( "Denemo Print View"));
  gtk_widget_set_size_request(GTK_WIDGET(top_vbox), 600, 750);
  g_signal_connect (G_OBJECT (top_vbox), "delete-event",
		    G_CALLBACK (hide_printarea_on_delete), NULL);
  gtk_container_add (GTK_CONTAINER (top_vbox), main_vbox);

  GtkAdjustment *printvadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printvscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (printvadjustment));
		     
  GtkAdjustment *printhadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  Denemo.printhscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (printhadjustment));

  GtkWidget *score_and_scroll_hbox = gtk_scrolled_window_new (printhadjustment, printvadjustment);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE,
		      0);
 
  ev_init();
  
  Denemo.printarea = (GtkWidget*)ev_view_new();

  gtk_container_add (GTK_CONTAINER(score_and_scroll_hbox), Denemo.printarea);
  if(Denemo.prefs.newbie)
    gtk_widget_set_tooltip_markup(score_and_scroll_hbox, _("This window shows the final typeset score from which you can print or (via print to file) create a PDF document.\nWhen you have typeset the score you can click on a note to move to that place in the Denemo display.\n<b>Note</b>: It can take some time to generate a beautifully typeset score, so take a break!\nIf you just want to generate a score layout - e.g. for one part then you can kill off the actual typsetting by closing the Progress bar"));

 g_signal_connect (G_OBJECT (Denemo.printarea), "external-link",
		      G_CALLBACK (goto_position), NULL);


#if GTK_MAJOR_VERSION==3
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "draw",
		      G_CALLBACK (printarea_draw_event), NULL);
#else
  g_signal_connect_after (G_OBJECT (Denemo.printarea), "expose_event",
		      G_CALLBACK (printarea_draw_event), NULL);
#endif

  g_signal_connect (G_OBJECT (Denemo.printarea), "motion_notify_event",
		      G_CALLBACK (printarea_motion_notify), NULL);
          
          
 //g_signal_connect (G_OBJECT (Denemo.printarea), "focus_in_event",
	//	      G_CALLBACK (printarea_focus_in_event), NULL);

  
//g_print("Attaching signal...");
//!!!not available in early versions of libevince
//g_signal_connect (G_OBJECT (Denemo.printarea), "sync-source",
//		      G_CALLBACK (denemoprintf_sync), NULL);
//g_print("...Attached signal?\n");

//what would this one fire on???? g_signal_connect (G_OBJECT (Denemo.printarea), "binding-activated",
//		      G_CALLBACK (denemoprintf_sync), NULL);

// Re-connect this signal to work on the pop up menu for dragging Denemo objects...
// g_signal_connect (G_OBJECT (Denemo.printarea), "button_press_event", G_CALLBACK (printarea_button_press), NULL);

// We may not need this signal
//  g_signal_connect (G_OBJECT (score_and_scroll_hbox), "scroll_event", G_CALLBACK(printarea_scroll_event), NULL);

  g_signal_connect_after (G_OBJECT (Denemo.printarea), "button_release_event", G_CALLBACK (printarea_button_release), NULL);

  gtk_widget_show_all(main_vbox);
  gtk_widget_hide(top_vbox);

}



#endif /* PRINT_H */
