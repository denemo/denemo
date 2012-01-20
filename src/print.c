/* Print.c
 * 
 * printing support for GNU Denemo
 * outputs to a pdf or png file
 * and displays in a print-preview drawing area
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Adam Tee, 2009 Richard Shann
 */
#ifndef PRINT_H

#define PRINT_H
#include <stdio.h>
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

#define GREATER 2
#define SAME 1
#define LESSER 0

typedef struct lilyversion
{
  gint major;
  gint minor;
}lilyversion;


static changecount = -1;//changecount when the printfile was last created FIXME multiple tabs are muddled
#define GPID_NONE (-1)
static GPid printviewpid = GPID_NONE;
static GPid previewerpid = GPID_NONE;
static GPid get_lily_version_pid = GPID_NONE;
static GPid printpid = GPID_NONE;
static gint printpreview_errors=-1;
static gint output=-1;
static gint errors=-1;
static   GError *lily_err = NULL;

static
void print_finished(GPid pid, gint status, GList *filelist);

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
  gchar *version_string;
  double d;
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
    gint numbytes = read(standard_output, buf, sizeof(buf));
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
  gchar *filename = NULL;
  if(filename==NULL)
    filename = g_build_filename ( locateprintdir (), "denemoprint", NULL);
  return filename;
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
  if(printpid==GPID_NONE)
    return;
  DenemoGUI *gui = Denemo.gui;
  GError *err = NULL;
  gchar *printfile;
  gchar **arguments;
  progressbar_stop();
  g_spawn_close_pid (printpid);
  printpid = GPID_NONE;
  //normal_cursor();
  process_lilypond_errors(filename); 
#ifndef G_OS_WIN32
  //status check seems to fail on windows, and errors are not highlighted for windows.
  if(status) {
    warningdialog/* a warning dialog causes deadlock if progress bar is being destroyed, sometimes */("LilyPond engraver failed - See highlighting in LilyPond window (open the LilyPond window and right click to print)");
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
  DenemoGUI *gui = Denemo.gui;
  progressbar("Denemo Typesetting");
  g_spawn_close_pid (get_lily_version_pid);
  get_lily_version_pid = GPID_NONE;

  if(printpid!=GPID_NONE) {
    if(confirm("Already doing a print", "Kill that one off and re-start?")) {
      if(printpid!=GPID_NONE) //It could have died while the user was making up their mind...
        kill_process(printpid);
      printpid = GPID_NONE;
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
		&printpid,
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
  if(printpid!=GPID_NONE){
    kill_process(printpid);
    printpid = GPID_NONE;
  }
 return FALSE;//do not call again
}

/*  create pdf of current score, optionally restricted to voices/staffs whose name match the current one. 
 *  Save file in lilypond format
 *  Fork and run lilypond
 */
static void
create_pdf (DenemoGUI * gui, gboolean part_only, gboolean all_movements)
{
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);

  g_remove (lilyfile);
  if(part_only)
    export_lilypond_part (lilyfile, gui, all_movements);
  else
    exportlilypond (lilyfile, gui,  all_movements);
  /* create arguments to pass to lilypond to create a pdf for printing */
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };

  run_lilypond(arguments);
  gui->lilysync = G_MAXUINT;// in certain cases this may not be needed
  g_free(lilyfile);
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


static   cairo_surface_t *the_surface;
static gdouble repeat_length;
static void
begin_print (GtkPrintOperation *operation,
	     GtkPrintContext   *context)
{

  gchar *filename = get_printfile_pathbasename();
  gchar *path = g_strconcat (filename, "_.png", NULL);
  g_free(filename);
  the_surface = cairo_image_surface_create_from_png (path);
  g_free(path);
  int src_width = cairo_image_surface_get_width(the_surface);//PIXELS
  int src_height = cairo_image_surface_get_height(the_surface);

  double page_height =  1.414*((double)src_width);
  //g_print("Estimated page height from width %f giving %f pages\n", page_height, src_height/page_height);
  int num_pages = (int)(0.5+src_height/page_height);
  repeat_length = src_height/num_pages +1.0;
  //g_print("Estimated page height from width %f giving %f pages\nRepeat length %f", page_height, src_height/page_height, repeat_length);
  gtk_print_operation_set_n_pages (operation, num_pages);
  gtk_print_operation_set_use_full_page(operation, TRUE);
}


/***********************************
We divide the .png image into pages.
 we use the page_nr parameter to choose the n'th page-sized chunk of the .png surface.
 paper size in points:

Letter		 612x792
LetterSmall	 612x792
Tabloid		 792x1224
Ledger		1224x792
Legal		 612x1008
Statement	 396x612
Executive	 540x720
A0               2384x3371
A1              1685x2384
A2		1190x1684
A3		 842x1190
A4		 595x842
A4Small		 595x842
A5		 420x595
B4		 729x1032
B5		 516x729
Envelope	 ???x???
Folio		 612x936
Quarto		 610x780
10x14		 720x1008


*********************/
static void
draw_page (GtkPrintOperation *operation,
	   GtkPrintContext   *context,
	   gint               page_nr)
{
  cairo_t * cr = gtk_print_context_get_cairo_context (context);

  int src_width = cairo_image_surface_get_width(the_surface);//PIXELS
  int src_height = cairo_image_surface_get_height(the_surface);
  int page_origin_x = 0;
  int page_origin_y = 0;
  static double scale = 1.414;
  //g_print("width %d height %d\n", src_width, src_height);
  double page_height =  scale*src_width;
  int num_pages = (int)(0.5 + src_height/page_height);
  //g_print("Have %d pages of page height %f at page number %d\n", num_pages, page_height, page_nr);

  cairo_status_t  status = cairo_surface_status(the_surface);
  if(status != CAIRO_STATUS_SUCCESS)
    g_print("An error %d\n", status);
  if(page_nr>=num_pages) {
    g_warning("called for page_nr %d\n", page_nr+1);
    gtk_print_operation_cancel (operation);
    //cairo_surface_destroy (the_surface); the_surface = NULL;
  }
  else {

    double margin = 15.0;
    //cairo_rotate (cr, 45* 3.1418/180); works
    cairo_scale (cr, 70.0/180.0, 70.0/180.0);
    cairo_translate(cr, 4*margin,  4*margin   );
    cairo_set_source_surface (cr, the_surface, 0.0 /*2*margin*/,   - (double)(repeat_length*page_nr));
    cairo_rectangle (cr, 0.0, 0.0, (double)src_width+2*margin, (double)repeat_length);
    cairo_fill (cr);
  }
  if(page_nr==num_pages-1)
    g_print("Finished printing\n");
  else
    g_print("returning from drawing page %d\n", page_nr+1);
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
  if(printpid==GPID_NONE)
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
  g_spawn_close_pid (printpid);
  printpid = GPID_NONE;
  progressbar_stop();
  infodialog("Your png file has now been created");
}

static
void printpdf_finished(GPid pid, gint status, GList *filelist) {
  if(filelist) {
    g_list_foreach(filelist, (GFunc)rm_temp_files, FALSE);
    g_list_free(filelist);
  }
  g_spawn_close_pid (printpid);
  printpid = GPID_NONE;
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
  g_free (basename);
 
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
      g_child_watch_add (printpid, (GChildWatchFunc)finish, (gchar *) filelist);
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
  g_free (basename);

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
    g_spawn_close_pid (printpid);
    printpid = GPID_NONE;
    return;
  }

  g_child_watch_add (printpid, (GChildWatchFunc)printpdf_finished, filelist);
}

static void
print_and_view(gchar **arguments) {
  run_lilypond(arguments);
  if(printpid!=GPID_NONE) {
    g_child_watch_add (printpid, (GChildWatchFunc)open_pdfviewer, (gchar *) get_printfile_pathbasename());
    while(printpid!=GPID_NONE) {
      gtk_main_iteration_do(FALSE);
    }
  }
}

void print_lily_cb (GtkWidget *item, DenemoGUI *gui){
  if(call_out_to_guile("(InitializeTypesetting)")) {
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

static gint offsetx, offsety;

static gint curx, cury;// position of mouse pointer while during motion
static gint pointx, pointy,  markx, marky;//coordinates defining a selected region in print preview pane. These are set by left button press/release, with pointx, pointy being set to top left


static GtkPrintSettings *settings;
/* print the score as stored in .png image generated by LilyPond. */
static void
printall(void) {
  //g_print("print all");
  GtkPrintOperation *operation;
  gint res;
  GError *error = NULL;
  operation = gtk_print_operation_new ();
  if (settings != NULL)
    gtk_print_operation_set_print_settings (operation, settings);

  g_signal_connect (operation, "begin-print",
		    G_CALLBACK (begin_print), NULL);
  g_signal_connect (operation, "draw-page",
		    G_CALLBACK (draw_page), NULL);

  res = gtk_print_operation_run (operation, GTK_PRINT_OPERATION_ACTION_PRINT_DIALOG,
				 GTK_WINDOW (Denemo.window), &error);
  if (res == GTK_PRINT_OPERATION_RESULT_APPLY) {
    if (settings != NULL)
      g_object_unref (settings);
    settings = g_object_ref (gtk_print_operation_get_print_settings (operation));
  }
  else if (error) {
    GtkWidget *dialog;
    dialog = gtk_message_dialog_new (GTK_WINDOW (Denemo.window),
				     GTK_DIALOG_DESTROY_WITH_PARENT,
				     GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
				     "%s", error->message);
    g_error_free (error);
    error = NULL;
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
  }
  g_object_unref (operation);
}


static void draw_print(cairo_t *cr) {
  gint x, y;
  GtkAdjustment * adjust = gtk_range_get_adjustment(GTK_RANGE(Denemo.printhscrollbar));
  x = (gint)gtk_adjustment_get_value(adjust);
  adjust = gtk_range_get_adjustment(GTK_RANGE(Denemo.printvscrollbar));
  y = (gint)gtk_adjustment_get_value(adjust);

  gint width, height;
  width = gdk_pixbuf_get_width( GDK_PIXBUF(Denemo.pixbuf));
  height = gdk_pixbuf_get_height( GDK_PIXBUF(Denemo.pixbuf));

/*   gdk_draw_pixbuf(Denemo.printarea->window, NULL, GDK_PIXBUF(Denemo.pixbuf), */
/* 		  x,y,0,0,/\* x, y in pixbuf, x,y in window *\/ */
/* 		  width,  height, GDK_RGB_DITHER_NONE,0,0); */
  cairo_scale( cr, Denemo.gui->si->preview_zoom, Denemo.gui->si->preview_zoom );
  cairo_translate( cr, 0.5, 0.5 );
  gdk_cairo_set_source_pixbuf( cr, GDK_PIXBUF(Denemo.pixbuf), -x, -y);
  cairo_rectangle( cr,-x,-y, width, height );
  cairo_fill( cr );


  if(selecting)
    {gint w = ABS(markx-curx);
    gint h = ABS(marky-cury);
    cairo_set_source_rgba( cr, 0, 0, 1, 0.4 );
    cairo_rectangle( cr,markx,marky, w, h );
    cairo_fill( cr );
    }
  if(offsetting)
    {
      gint w = pointx-markx;
      gint h = pointy-marky;

      cairo_set_source_rgba( cr, 0.7, 0.7, 0.7, 0.4 );
      cairo_rectangle( cr, markx, marky, w, h);

	gdk_cairo_set_source_pixbuf (cr, GDK_PIXBUF(Denemo.pixbuf), curx, cury);
        //do i need this here? cairo_paint(cr);
	//gdk_draw_pixbuf(gtk_widget_get_window(Denemo.printarea), NULL, GDK_PIXBUF(Denemo.pixbuf),
	//	  markx+x, marky+y, curx, cury,/* x, y in pixbuf, x,y in window */
	//	w,  h, GDK_RGB_DITHER_NONE,0,0);

    }
  if(padding)
    {

      gint pad = ABS(markx-curx);
      gint w = pointx-markx;
      gint h = pointy-marky;
      cairo_set_source_rgba( cr, 0.7, 0.7, 0.7, 0.4 );
      cairo_rectangle( cr, markx-pad/2, marky-pad/2, w+pad, h+pad);
      gdk_cairo_set_source_pixbuf (cr, GDK_PIXBUF(Denemo.pixbuf), markx, marky);
      //do I need this here? cairo_paint(cr);
	 //gdk_draw_pixbuf(gtk_widget_get_window(Denemo.printarea), NULL, GDK_PIXBUF(Denemo.pixbuf),
	//	      markx+x, marky+y, markx, marky,/* x, y in pixbuf, x,y in window */
	//	w,  h, GDK_RGB_DITHER_NONE,0,0);

    }


}

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

static void
process_printpreview_errors(void){
  DenemoGUI *gui = Denemo.gui;
  if (printpreview_errors == -1)
    return;
#define bufsize (1000)
  gchar *bytes = g_malloc0(bufsize);
  gint numbytes = read(printpreview_errors, bytes, bufsize-1);
  close(printpreview_errors);
  printpreview_errors = -1;
#undef bufsize
  if(*bytes)
    console_output(bytes);
  g_free(bytes);
}

static void
libevince_print(void) {
  GError *err = NULL;
  GFile       *file;
  gchar *filename = g_strconcat((gchar *) get_printfile_pathbasename(), ".pdf", NULL);
  gchar *uri = g_filename_to_uri(filename, NULL, &err);

  if(err) {
    g_warning ("Malformed filename %s\n", filename);
    return;
  }

  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if(err) {
    g_warning ("Trying to read the pdf file %s gave an error: %s", uri, err->message);
    if(err)
			g_error_free (err);
    err = NULL;
  } else {
    EvPrintOperation *printop = ev_print_operation_new (doc);      
    ev_print_operation_run (printop, NULL);
  }
}


static void
printview_finished(GPid pid, gint status, gboolean print) {
  progressbar_stop();
  printpid = GPID_NONE;
  GError *err = NULL;
  GFile       *file;
  gchar *filename = g_strconcat((gchar *) get_printfile_pathbasename(), ".pdf", NULL);
  file = g_file_new_for_commandline_arg (filename);
  g_free(filename);
  gchar *uri = g_file_get_uri (file);
  g_object_unref (file);
  EvDocument *doc = ev_document_factory_get_document (uri, &err);
  if(err) {
    g_warning ("Trying to read the pdf file %s gave an error: %s", uri, err->message);
      if(err) g_error_free (err);
      err = NULL;
  } else {
    EvDocumentModel  *model = ev_document_model_new_with_document(doc);
    ev_view_set_model((EvView*)Denemo.printarea, model);
    if(print) {
     libevince_print();
    }
  }

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
    create_pdf(gui, TRUE, TRUE);
  else
   create_pdf(gui, TRUE, FALSE); 
  g_child_watch_add (printpid, (GChildWatchFunc)open_pdfviewer  /*  GChildWatchFunc function */, 
	(gchar *) get_printfile_pathbasename());
}


static gboolean typeset(gboolean force) {
if((force) || (changecount!=Denemo.gui->changecount)) {
  if(call_out_to_guile("(InitializeTypesetting)")) {
      g_warning("InitializeTypesetting failed\n");
      return FALSE;
  }
  DenemoGUI *gui = Denemo.gui;
  gui->si->markstaffnum=0;//FIXME save and restore selection?    
  gui->lilycontrol.excerpt = FALSE;
  if((gui->movements && g_list_length(gui->movements)>1) && 
     (confirm("This piece has several movements", "Print all of them?")))
    create_pdf(gui, FALSE, TRUE);
  else
    create_pdf(gui, FALSE, FALSE);
  return TRUE;
  }
return FALSE;
}
void
printpreview_cb (GtkAction *action, DenemoScriptParam* param) {
  (void)typeset(TRUE);
  g_child_watch_add (printpid, (GChildWatchFunc)print_finished, NULL);
}

void refresh_print_view (void) {
  busy_cursor();
  if(typeset(FALSE))
    g_child_watch_add (printpid, (GChildWatchFunc)printview_finished, (gpointer)(FALSE));
  else
    normal_cursor();
}

void print_from_print_view() {
  busy_cursor();
  if(typeset(FALSE)) {
    g_child_watch_add(printpid, (GChildWatchFunc)printview_finished, (gpointer)(TRUE));
  }
  else {
    normal_cursor();
    libevince_print();//printview_finished (printpid, 0, TRUE);
  }
}

void
printselection_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  if(gui->si->markstaffnum)
    create_pdf(gui, FALSE, FALSE);
  else
    warningdialog(_("No selection to print"));
  g_child_watch_add (printpid, (GChildWatchFunc)open_pdfviewer  /*  GChildWatchFunc function */, 
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
  g_spawn_close_pid (printpid); 
  printpid = GPID_NONE;
  gchar *printname = get_thumb_printname();
    gchar *printpng = g_strconcat(printname, ".png", NULL);
    GdkPixbuf *pbN = gdk_pixbuf_new_from_file_at_scale   (printpng, 128, -1, TRUE, &err);
    GdkPixbuf *pbL = gdk_pixbuf_new_from_file_at_scale   (printpng, 256, -1, TRUE, &err);
    
    //FIXME if pb->height>128 or 256 scale it down...
    if(pbN && pbL) {
      gchar *uri = g_strdup_printf("file://%s", Denemo.gui->filename->str);
      gchar *thumbname = get_thumbname (uri);

      
            struct stat thebuf;
            gint status =  g_stat(Denemo.gui->filename->str, &thebuf);
            unsigned mtime = thebuf.st_mtime;
            g_print("the mt is %u\n", mtime);
            
 

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
    printpid=GPID_NONE;
    progressbar_stop();
    //g_print("Set printpid = %d\n", printpid);
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
  if(printpid!=GPID_NONE)
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
      thumb_finished (printpid, 0);
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
    print_from_print_view();
}


//static gint 
//drag_selection(void) {
//  offsetting = TRUE;
//  return TRUE;
//}
static gint 
start_drag(GtkWidget *widget, gboolean *flag) {
  *flag = TRUE;
  return TRUE;
}

static gint 
popup_print_preview_menu(void) {
  GtkWidget *menu = gtk_menu_new();
  GtkWidget *item = gtk_menu_item_new_with_label("Print");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(print_from_print_view),NULL);

#if 0
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

static void
goto_position (EvView* view, EvLinkAction *obj)
{

  gchar *uri = (gchar*)ev_link_action_get_uri(obj);
  //g_print("external signal %s\n", uri);
  gchar **vec = g_strsplit (uri, ":",5);
  if(!strcmp(vec[0], "textedit") && vec[1] && vec[2] && vec[3]) {
    goto_lilypond_position(atoi(vec[2]), atoi(vec[3]));
  } else {
  g_warning ("Cannot follow external link type %s\n", vec[0]);
  }
  g_strfreev(vec);
}
static gint adjust_x=0;
static gint adjust_y=0;


static gint
printarea_focus_in_event (GtkWidget * widget, GdkEventExpose * event)
{
  if((printpid==GPID_NONE) && (changecount!=Denemo.gui->changecount)) {
    refresh_print_view();
    changecount = Denemo.gui->changecount;// keep track so we know if update is needed FIXME - different tabs etc
  }
  return TRUE;
}




//GdkBitmap *graphic = NULL; /* a selection from the print area */
//gint markx, marky, pointx, pointy;/* a selected area in the printarea */

gint
printarea_motion_notify (GtkWidget * widget, GdkEventButton * event)
{
  if(Denemo.pixbuf==NULL)
    return TRUE;
  if(padding || offsetting || selecting) {
    curx = (int)event->x;
    cury = (int)event->y;
    gtk_widget_queue_draw (Denemo.printarea);
  }

  return TRUE;
}


static gint
printarea_scroll_event (GtkWidget *widget, GdkEventScroll *event) {
  switch(event->direction) {
  case GDK_SCROLL_UP:
    if(event->state&GDK_CONTROL_MASK) {
      if(event->state&GDK_SHIFT_MASK)
	Denemo.gui->si->preview_zoom *= 1.01;
      else
	Denemo.gui->si->preview_zoom *= 1.1;
      gtk_widget_queue_draw(Denemo.printarea);
    } else {
      GtkAdjustment *vadj = gtk_range_get_adjustment(GTK_RANGE(Denemo.printvscrollbar));
      gtk_adjustment_set_value(vadj,
			       10.0 + gtk_adjustment_get_value(vadj));

    }
    break;
  case GDK_SCROLL_DOWN:
    if(event->state&GDK_CONTROL_MASK) {
      if(event->state&GDK_SHIFT_MASK)
	Denemo.gui->si->preview_zoom /= 1.01;
      else
	Denemo.gui->si->preview_zoom /= 1.1;
      if(Denemo.gui->si->preview_zoom <0.01)
	Denemo.gui->si->preview_zoom = 0.01;
      gtk_widget_queue_draw(Denemo.printarea);
    } else {
      GtkAdjustment *vadj = gtk_range_get_adjustment(GTK_RANGE(Denemo.printvscrollbar));
      gtk_adjustment_set_value(vadj,
			       -10.0 + gtk_adjustment_get_value(vadj));

    }
   break;
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
static	gboolean within_area(gint x, gint y) {
  return(x<=pointx &&
	 y<=pointy &&
	 x>=markx &&
	 y>=marky);
}
 
gint
printarea_button_press (GtkWidget * widget, GdkEventButton * event)
{
  gboolean left = (event->button != 3);
  if((!left)) {
    if(printpid==GPID_NONE)
      popup_print_preview_menu();
    return TRUE;
  }
  /* creating an offset? */
  if(offsetting) {
    offsetx = curx - markx;
    offsety = cury - marky;  

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.printarea), "offset-dialog");
    g_object_set_data(G_OBJECT(Denemo.printarea), "offsetx", (gpointer)offsetx);
    g_object_set_data(G_OBJECT(Denemo.printarea), "offsety", (gpointer)offsety);
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
    g_object_set_data(G_OBJECT(Denemo.printarea), "padding", (gpointer)pad);
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


  //  if(within_area((gint)event->x,(gint)event->y)) {
  //    offsetting = TRUE;
  //    return TRUE;
  //  } else 
     selecting = TRUE;
  if(Denemo.pixbuf==NULL)
    return TRUE;
  pointx = markx=event->x;
  pointy = marky=event->y;

  return TRUE;
}



gint
printarea_button_release (GtkWidget * widget, GdkEventButton * event)
{
  gboolean left = (event->button != 3);
  if(!left) {
        return TRUE;
  }
  if(Denemo.pixbuf==NULL)
    return TRUE;
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

void install_printpreview(DenemoGUI *gui, GtkWidget *top_vbox){ 
  if(Denemo.printarea)
    return;
  busycursor = gdk_cursor_new(GDK_WATCH);
  arrowcursor = gdk_cursor_new(GDK_RIGHT_PTR);//FIXME what is the system cursor called??

  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  top_vbox = gtk_window_new(GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title(GTK_WINDOW(top_vbox), "Denemo Print View");
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


 g_signal_connect (G_OBJECT (Denemo.printarea), "external-link",
		      G_CALLBACK (goto_position), NULL);
 g_signal_connect (G_OBJECT (Denemo.printarea), "focus_in_event",
		      G_CALLBACK (printarea_focus_in_event), NULL);
 g_signal_connect (G_OBJECT (Denemo.printarea), "button_press_event",
		      G_CALLBACK (printarea_button_press), NULL);

  gtk_widget_show_all(main_vbox);
  gtk_widget_hide(top_vbox);
}



#endif /* PRINT_H */
