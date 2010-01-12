/* print.c
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

#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
#include <wait.h>
#endif

#include <errno.h>
#include <denemo/denemo.h>
#include "print.h"
#include "prefops.h"
#include "exportlilypond.h"
#include "utils.h"
#include "gcs.h"
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

#define GPID_NONE (-1)
static GPid printviewpid = GPID_NONE;
static gint printpreview_errors=-1;
static GPid printpid = GPID_NONE;
static gint output=-1;
static gint errors=-1;
static   GError *lily_err = NULL;

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

#if 1 // lilypond -v command should be good for all versions. was GLIB_MINOR_VERSION >= 14
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
  lilyversion version;
  char **token;
  const char delimiters[] = ".";

  /* split string */
  token = g_strsplit(string, delimiters, 2);

  /* get major version number */
  version.major = atoi(token[0]);

  /* get minor version number */
  version.minor = atoi(token[1]);

  printf("\nstring_to_lilyversion() major = %d minor = %d\n",version.major, version.minor);
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

gchar *
get_lily_version_string (void)
{
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
  NULL,	/*pid*/
  NULL, 	/*standard_input*/
  &standard_output,	/*standard output*/
  NULL,	/*standard error*/
  &error);

  read(standard_output, buf, sizeof(buf));
  return regex_parse_version_number(buf);

}
int
check_lily_version (gchar *version)
{
  gchar *  version_string = get_lily_version_string();
  lilyversion installed_version = string_to_lilyversion(version_string);
  lilyversion check_version = string_to_lilyversion(version);
  return version_check(check_version, installed_version);
}
#endif
        
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
  g_spawn_sync (locatedotdenemo (),		/* dir */
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
  DenemoGUI *gui = Denemo.gui;
  GError *err = NULL;
  gchar *printfile;
  gchar **arguments;
  g_spawn_close_pid (printpid);
  printpid = GPID_NONE;
  //normal_cursor();

  process_lilypond_errors(filename); 

  if (is_png)
	printfile = g_strconcat (filename, "_.png", NULL);
  else
  	printfile = g_strconcat (filename, ".pdf", NULL);
  
  FILE *fp = fopen(printfile, "r");
  if(fp)
    fclose(fp);
  else {
    //FIXME use filename in message
    //warningdialog("Could not open ~/.denemo/denemoprint.pdf, check permissions");
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

  g_spawn_async (locatedotdenemo (),		/* dir */
		 arguments, NULL,	/* env */
		 G_SPAWN_SEARCH_PATH, /* search in path for executable */
		 NULL,	/* child setup func */
		 NULL,		/* user data */		
		 NULL, /* FIXME &pid see g_spawn_close_pid(&pid) */
		 &err);
  
  if (err != NULL)
    {
      g_warning ("Failed to find %s", Denemo.prefs.pdfviewer->str);
      warningdialog("Cannot display: Check Edit->Preferences->externals\nfor your PDF viewer");
      g_warning ("%s", err->message);
      err = NULL;
      if(err) g_error_free (err);
    }
  g_free(printfile);
}


static void
open_pngviewer(GPid pid, gint status, gchar *filename){
      open_viewer(pid, status, filename, TRUE);
}

static void
open_pdfviewer(GPid pid, gint status, gchar *filename){
     open_viewer(pid, status, filename, FALSE);
}

void
run_lilypond(gchar *filename, DenemoGUI *gui){

  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);
  gchar *resolution = g_strdup_printf("-dresolution=%d",(int) Denemo.prefs.resolution);
  gchar *printfile = g_strconcat (filename, "_", NULL);
  /* Check if Lilypond Version is set for this file, if so it may need conversion */
  if(gui->lilycontrol.lilyversion->len)
    convert_ly(lilyfile);

#if GLIB_MINOR_VERSION >= 14
  gchar **arguments; 
  gchar *png_arguments1[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-dbackend=eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
  gchar *png_arguments2[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-b",
    "eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
  
  if (check_lily_version("2.12") >= 1)
     arguments = png_arguments1;
  else
     arguments = png_arguments2;
#else
  gchar *png_arguments[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-b",
    "eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
#endif

  gchar *pdf[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };
  if (!gui->lilycontrol.excerpt)	  
	  arguments = pdf;
  
  g_spawn_async_with_pipes (locatedotdenemo (),		/* dir */
		arguments, NULL,	/* env */
		G_SPAWN_SEARCH_PATH  | G_SPAWN_DO_NOT_REAP_CHILD, NULL,	/* child setup func */
		NULL,		/* user data */
		&printpid,
	        NULL,
		NULL,		/* stdout */
		&errors,		/* stderr */
		&lily_err);
  if (gui->lilycontrol.excerpt == TRUE)
    g_child_watch_add (printpid, (GChildWatchFunc)open_pngviewer  /*  GChildWatchFunc function */, filename);
  else
    g_child_watch_add (printpid, (GChildWatchFunc)open_pdfviewer  /*  GChildWatchFunc function */, filename);
}

/* Run the LilyPond interpreter on the file (filename).ly
 * putting the PDF output in (filename).pdf
 * start an external PDF viewer on that file.
 * parse first LilyPond error and position the cursor in gui->textview on it
 */
void
run_lilypond_and_viewer(gchar *filename, DenemoGUI *gui) {
  if(printpid!=GPID_NONE) {
    if(confirm("Already doing a print", "Kill that one off and re-start?")) {
      if(printviewpid!=GPID_NONE) //It could have died while the user was making up their mind...
	 kill_process(printpid);
      printpid = GPID_NONE;
    }
    else {
      warningdialog ("Cancelled");
      return;
    }
  }
  /* remove old output files to avoid confusion */
  gchar *printfile;
  if (gui->lilycontrol.excerpt == TRUE)
    printfile = g_strconcat (filename, "_.png", NULL);
  else
    printfile = g_strconcat (filename, ".pdf", NULL);
  FILE *fp = fopen(printfile, "w");
  if(fp)
    fclose(fp);
  g_free(printfile);
  run_lilypond(filename, gui);
  //  g_print("print pid is %d\n", printpid);

}

/* returns the base name (~/.denemo/denemoprint usually) used as a base
   filepath for printing. On windows there is some filelocking trouble.
   The returned string should not be freed.
*/
   
gchar *get_printfile_pathbasename(void) {
  return g_build_filename ( locatedotdenemo (), "denemoprint", NULL);
}

/*  Print function 
 *  Save file in lilypond format
 *  Fork and run lilypond
 *  TODO Add in lpr command
 */
static void
print (DenemoGUI * gui, gboolean part_only, gboolean all_movements)
{
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);
  remove (lilyfile);
  if(part_only)
    export_lilypond_part (lilyfile, gui, all_movements);
  else
    exportlilypond (lilyfile, gui,  all_movements);
  run_lilypond_and_viewer(filename, gui);
  gui->lilycontrol.excerpt = FALSE;//The default value
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
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, TRUE, TRUE, 0);

  gint max_measure =
  g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  label = gtk_label_new (_("Print from Measure"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);
 
  from_measure =
  gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure),
			     (gdouble) gui->si->firstmeasuremarked);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, TRUE, TRUE, 0);

  to_measure =
  gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  //  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure),
			     (gdouble) gui->si->lastmeasuremarked);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      gui->si->firstmeasuremarked =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
      gui->si->lastmeasuremarked =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
      //gtk_widget_destroy (dialog);
    }
  else 
    {
      gui->si->firstmeasuremarked = gui->si->lastmeasuremarked = 0;
    }
  if(gui->si->firstmeasuremarked) {
    gui->si->markstaffnum = gui->si->firststaffmarked = 1;
    gui->si->laststaffmarked = g_list_length(gui->si->thescore);
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


/* callback to print current part (staff) of score */
void
printpart_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  gui->lilycontrol.excerpt = FALSE;
  if(gui->si->markstaffnum)
    if(confirm("A range of music is selected","Print whole file?")){
      gui->si->markstaffnum=0;
    }
  if((gui->movements && g_list_length(gui->movements)>1) && 
     (confirm("This piece has several movements", "Print this part from all of them?")))
    print(gui, TRUE, TRUE);
  else
   print(gui, TRUE, FALSE);
 
}
void
printpreview_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  gui->lilycontrol.excerpt = FALSE;
  gui->si->markstaffnum=0;//FIXME save and restore selection?    
  if((gui->movements && g_list_length(gui->movements)>1) && 
     (confirm("This piece has several movements", "Print all of them?")))
    print(gui, FALSE, TRUE);
  else
    print(gui, FALSE, FALSE);
}

void
printselection_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  gui->lilycontrol.excerpt = FALSE;
  if(gui->si->markstaffnum)
    print(gui, FALSE, FALSE);
  else
    warningdialog(_("No selection to print"));
}


void
printexcerptpreview_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  gui->lilycontrol.excerpt = TRUE;
  if(!gui->si->markstaffnum) //If no selection has been made 
    printrangedialog(gui);  //Launch a dialog to get selection
  if(gui->si->firstmeasuremarked)
    print(gui, FALSE, FALSE);
}


void printpdf_finished() {
  g_spawn_close_pid (printpid);
  printpid = GPID_NONE;
  infodialog("Your pdf file has now been created");
}
/**
 * Does all the export pdf work.
 * calls exportmudela and then  
 * runs lilypond to a create a temporary pdf file and 
 * renames to filename.pdf
 *
 *	@param filename filename to save score to
 *  @param gui pointer to the DenemoGUI structure
 */
static void
export_pdf (const gchar * filename, DenemoGUI * gui)
{
  const gchar *tmpdir = locatedotdenemo ();
  gchar *tmpfile;
  gchar *mudelafile;
  gchar *midifile;
  gchar *dvifile;
  gchar *psfile;
  gchar *pdffile;
  GError *err = NULL;
  gint exit_status;
  gboolean ok;

  /* look for lilypond */
  check_lilypond_path(gui);
  /* create a temp (and not existing) filepath in .denemo folder */
  do
    {
      tmpfile = get_temp_filename (NULL);
      mudelafile = g_strconcat (tmpfile, ".ly", NULL);
      midifile = g_strconcat (tmpfile, ".midi", NULL);
      dvifile = g_strconcat (tmpfile, ".dvi", NULL);
      psfile = g_strconcat (tmpfile, ".ps", NULL);
      pdffile = g_strconcat (tmpfile, ".pdf", NULL);

      if (g_file_test (mudelafile, G_FILE_TEST_EXISTS) ||
          g_file_test (midifile, G_FILE_TEST_EXISTS) ||
          g_file_test (dvifile, G_FILE_TEST_EXISTS) ||
          g_file_test (psfile, G_FILE_TEST_EXISTS) ||
          g_file_test (pdffile, G_FILE_TEST_EXISTS))
        ok = FALSE;
      else
        ok = TRUE;

      if (!ok)
        {
          g_free (tmpfile);
          g_free (mudelafile);
          g_free (midifile);
          g_free (dvifile);
          g_free (psfile);
          g_free (pdffile);
        }
    }
  while(!ok);

  /* generate the lilypond file */
  exportlilypond (mudelafile, gui, TRUE);

  /* generate the pdf file */
  run_lilypond(tmpfile, gui);

  gint status;
  if(printpid!=GPID_NONE) {
    //    g_print("print pid is %d\n", printpid);
    g_child_watch_add (printpid, (GChildWatchFunc)printpdf_finished, NULL);
    while(printpid!=GPID_NONE) {
      gtk_main_iteration_do(FALSE);
    }
    /* move the pdf file to its destination */
    if (rename (pdffile, filename) != 0)
      warningdialog (g_strdup_printf("Failed to rename %s to %s\n", pdffile, filename));
  }
  /* remove unnecessary files and free the memory */
  remove (mudelafile);
  remove (midifile);
  remove (dvifile);
  remove (psfile);

  g_free (tmpfile);
  g_free (mudelafile);
  g_free (midifile);
  g_free (dvifile);
  g_free (psfile);
  g_free (pdffile);
}


/**
 * Export pdf callback prompts for filename
 *
 */

void
export_pdf_action (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *file_selection;

  file_selection = gtk_file_chooser_dialog_new (_("Export PDF"),
						GTK_WINDOW (Denemo.window),
						GTK_FILE_CHOOSER_ACTION_SAVE,
						GTK_STOCK_CANCEL,
						GTK_RESPONSE_REJECT,
						GTK_STOCK_SAVE,
						GTK_RESPONSE_ACCEPT, NULL);

  gtk_widget_show_all (file_selection);
  gboolean close = FALSE;
  do
    {

      if (gtk_dialog_run (GTK_DIALOG (file_selection)) == GTK_RESPONSE_ACCEPT)
	{
	  gchar *filename =
	    gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (file_selection));

	  if (replace_existing_file_dialog
	      (filename, GTK_WINDOW (Denemo.window), -1))
	    {
	      gtk_widget_destroy (file_selection);
	      export_pdf (filename, gui);
	      close = TRUE;
	    }
	  g_free (filename);
	}
      else
	{
	  gtk_widget_destroy (file_selection);
	  close = TRUE;
	}

    }
  while (!close);
}




// Displaying Print Preview
static changecount = -1;//changecount when last refreshed
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
				     GTK_DIALOG_NO_SEPARATOR,
				     GTK_MESSAGE_ERROR, GTK_BUTTONS_CLOSE,
				     "%s", error->message);
    g_error_free (error);
    gtk_dialog_run (GTK_DIALOG (dialog));
    gtk_widget_destroy (dialog);
  }
  g_object_unref (operation);
}


static void draw_print(DenemoGUI *gui) {
  gint x, y;
  GtkAdjustment * adjust = gtk_range_get_adjustment(GTK_RANGE(gui->printhscrollbar));
  x = (gint)adjust->value;
  adjust = gtk_range_get_adjustment(GTK_RANGE(gui->printvscrollbar));
  y = (gint)adjust->value;

  gint width, height;
  width = gdk_pixbuf_get_width( GDK_PIXBUF(gui->pixbuf)) - x;
  height = gdk_pixbuf_get_height( GDK_PIXBUF(gui->pixbuf)) - y;

  gdk_draw_pixbuf(gui->printarea->window, NULL, GDK_PIXBUF(gui->pixbuf),
		  x,y,0,0,/* x, y in pixbuf, x,y in window */
		  width,  height, GDK_RGB_DITHER_NONE,0,0);
  if(selecting)
    {gint w = ABS(markx-curx);
    gint h = ABS(marky-cury);
    gdk_draw_rectangle (Denemo.gui->printarea->window,
			gcs_bluegc(), FALSE,markx, marky, w, h);
    }
  if(offsetting)
    {
      gint w = pointx-markx;
      gint h = pointy-marky;
      gdk_draw_rectangle (Denemo.gui->printarea->window,
			gcs_graygc(), TRUE, markx, marky, w, h);

      gdk_draw_pixbuf(gui->printarea->window, NULL, GDK_PIXBUF(gui->pixbuf),
		  markx+x, marky+y, curx, cury,/* x, y in pixbuf, x,y in window */
		w,  h, GDK_RGB_DITHER_NONE,0,0);

    }
  if(padding)
    {

      gint pad = ABS(markx-curx);
      gint w = pointx-markx;
      gint h = pointy-marky;

      gdk_draw_rectangle (Denemo.gui->printarea->window,
			gcs_graygc(), TRUE, markx-pad/2, marky-pad/2, w+pad, h+pad);
      gdk_draw_pixbuf(gui->printarea->window, NULL, GDK_PIXBUF(gui->pixbuf),
		      markx+x, marky+y, markx, marky,/* x, y in pixbuf, x,y in window */
		w,  h, GDK_RGB_DITHER_NONE,0,0);

    }


}

static GdkCursor *busycursor;
static GdkCursor *arrowcursor;
static void busy_cursor(void) {
 gdk_window_set_cursor(Denemo.gui->printarea->window, busycursor);
}
static void normal_cursor(void) {
 gdk_window_set_cursor(Denemo.gui->printarea->window, arrowcursor);
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
printview_finished(GPid pid, gint status, gboolean preview_only) {
  // g_print("printview finished with preview_only set %d", preview_only);
  DenemoGUI *gui = Denemo.gui;
  g_spawn_close_pid (printviewpid);
  printviewpid = GPID_NONE;
  GError *error = NULL;
  normal_cursor();
  process_printpreview_errors();
  gchar *filename = get_printfile_pathbasename();
  gchar *path = g_strconcat (filename, "_.png", NULL);
  if(gui->pixbuf)
    g_object_unref(gui->pixbuf);
  gui->pixbuf = gdk_pixbuf_new_from_file (path, &error);
 if(error != NULL)
   {
     g_warning (_("Could not load the print preview:\n%s\n"),
                 error->message);
     g_error_free (error);
     gui->pixbuf = NULL;
   } else {
     gboolean ret;
     //FIXME the parameters here are placed by trial and error - the docs indicate &ret should come at the end
     //but an error message results.
     g_signal_emit_by_name(gui->printarea, "configure_event", NULL, &ret, NULL);
   }
 g_object_set_data(G_OBJECT(Denemo.gui->printarea), "printviewupdate", (gpointer) changecount);
 gtk_widget_queue_draw (gui->printarea);
 
 if(!preview_only)
   printall();
}

void refresh_print_view (gboolean preview_only) {
  DenemoGUI *gui = Denemo.gui;
  GError *error = NULL;
  //g_print("preview only %d\n", preview_only);
  if((changecount == Denemo.gui->changecount) && ((gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "printviewupdate")==Denemo.gui->changecount)) {
    if(confirm ("No changes since last update", "Cancel refresh of print view?"))
      return;
  }

  if(printviewpid!=GPID_NONE) {
    if(confirm("Already doing a print", "Kill that one off and re-start?")) {
      if(printviewpid!=GPID_NONE) //It could have died while the user was making up their mind...
	 kill_process(printviewpid);
      printviewpid = GPID_NONE;
    }
    else {
      warningdialog ("Cancelled");
      return;
    }
  }
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, "_.ly", NULL);
  remove (lilyfile);
  gchar *path = g_strconcat (filename, "_.png", NULL);
  remove (path);
  gui->si->markstaffnum=0;//remove selection, as exportlilypond respects it - FIXME??
  exportlilypond (lilyfile, gui,  TRUE);
  convert_ly(lilyfile);

  // run_lilypond_and_viewer(filename, gui);
  gchar *printfile = g_strconcat (filename, "_", NULL);
  gchar *resolution = "-dresolution=180";


#if GLIB_MINOR_VERSION >= 14
  gchar **arguments;
  gchar *arguments1[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-dbackend=eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
  gchar *arguments2[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-b",
    "eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
  if (check_lily_version("2.12") >= 1)
   arguments = arguments1;
  else
    arguments = arguments2;
#else
  gchar *arguments[] = {
    Denemo.prefs.lilypath->str,
    "--png",
    "-b",
    "eps",
    resolution,
    "-o",
    printfile,
    lilyfile,
    NULL
  };
#endif


  busy_cursor();
  changecount = Denemo.gui->changecount;// keep track so we know it update is needed

  g_spawn_async_with_pipes (locatedotdenemo (),		/* dir */
		 arguments, NULL,	/* env */
		 G_SPAWN_SEARCH_PATH | G_SPAWN_DO_NOT_REAP_CHILD, NULL,	/* child setup func */
		 NULL,		/* user data */
		 &printviewpid,
		 NULL,
		 NULL,		/* stdout */
	         &printpreview_errors,		/* stderr */
		 &error);
  g_free(lilyfile);
  g_child_watch_add (printviewpid, (GChildWatchFunc)printview_finished  /*  GChildWatchFunc function */, (gpointer)preview_only);
}

/* callback to print whole of score */

void
printall_cb (GtkAction *action, gpointer param) {
  DenemoGUI *gui = Denemo.gui;
  gui->lilycontrol.excerpt = FALSE;
  gchar *str = g_strdup_printf("Direct printing is experimental - use print preview otherwise after setting pdf viewer in prefs (currently %s).", Denemo.prefs.pdfviewer->str);
  warningdialog(str);
  g_free(str);
  //g_print("changecount %d %d %d \n", changecount, Denemo.gui->changecount, (gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "printviewupdate"));
  if((changecount == Denemo.gui->changecount) && ((gint)g_object_get_data(G_OBJECT(Denemo.gui->printarea), "printviewupdate")==Denemo.gui->changecount)) 
    printall();
  else
    refresh_print_view(FALSE);//calls printall when lilypond has finished.
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
  GtkWidget *item = gtk_menu_item_new_with_label("Refresh Print Preview");

  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(refresh_print_view), (gpointer)TRUE);
  item = gtk_menu_item_new_with_label("Drag to desired offset");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(start_drag), &offsetting);

  item = gtk_menu_item_new_with_label("Drag a space for padding");
  gtk_menu_shell_append(GTK_MENU_SHELL(menu), item);
  g_signal_connect(G_OBJECT(item), "activate", G_CALLBACK(start_drag), &padding);


  gtk_widget_show_all(menu);
  gtk_menu_popup (GTK_MENU(menu), NULL, NULL, NULL, NULL,0, gtk_get_current_event_time()); 
  return TRUE;
}

gint
printarea_configure_event (GtkWidget * widget, GdkEventConfigure * event)
{
  DenemoGUI *gui = Denemo.gui;
  if(gui->pixbuf==NULL)
    return FALSE;
  gint width, height;
  gdk_drawable_get_size (gui->printarea->window, &width, &height);
  GtkAdjustment * vadjust = gtk_range_get_adjustment(GTK_RANGE(gui->printvscrollbar));
  vadjust->lower = vadjust->value = 0.0;
  vadjust->upper = (gdouble)gdk_pixbuf_get_height(gui->pixbuf);
  vadjust->page_size =  (gdouble)height;
  
  GtkAdjustment * hadjust = gtk_range_get_adjustment(GTK_RANGE(gui->printhscrollbar));
  hadjust->lower = hadjust->value = 0.0;
  hadjust->upper = (gdouble)gdk_pixbuf_get_width(gui->pixbuf);
  hadjust->page_size =  (gdouble)width;

  gtk_adjustment_changed(vadjust);
  gtk_adjustment_changed(hadjust);
  return TRUE;
}

static gint adjust_x=0;
static gint adjust_y=0;


static void
printvertical_scroll (GtkAdjustment * adjust)
{
  DenemoGUI *gui = Denemo.gui;
  // g_print("vertical %d to %d\n", (int)adjust->value, (int)(adjust->value+adjust->page_size));
  adjust_y=(int)adjust->value;
  gtk_widget_queue_draw (gui->printarea);
}

static void
printhorizontal_scroll (GtkAdjustment * adjust)
{
  DenemoGUI *gui = Denemo.gui;
  // g_print("horizontal %d to %d\n", (int)adjust->value, (int)(adjust->value+adjust->page_size));
  adjust_x=(int)adjust->value;
gtk_widget_queue_draw (gui->printarea);
}

static gint
printarea_expose_event (GtkWidget * widget, GdkEventExpose * event)
{
  DenemoGUI *gui = Denemo.gui;
  if(gui->pixbuf==NULL)
    return TRUE;
  draw_print(gui);
  return TRUE;
}




//GdkBitmap *graphic = NULL; /* a selection from the print area */
//gint markx, marky, pointx, pointy;/* a selected area in the printarea */

gint
printarea_motion_notify (GtkWidget * widget, GdkEventButton * event)
{
  if(Denemo.gui->pixbuf==NULL)
    return TRUE;
  if(padding || offsetting || selecting) {
    curx = (int)event->x;
    cury = (int)event->y;
    gtk_widget_queue_draw (Denemo.gui->printarea);
  }

  return TRUE;
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
  if((!left) || (Denemo.gui->pixbuf==NULL)) {
    popup_print_preview_menu();
    return TRUE;
  }
  /* creating an offset? */
  if(offsetting) {
    offsetx = curx - markx;
    offsety = cury - marky;  

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.gui->printarea), "offset-dialog");
    g_object_set_data(G_OBJECT(Denemo.gui->printarea), "offsetx", (gpointer)offsetx);
    g_object_set_data(G_OBJECT(Denemo.gui->printarea), "offsety", (gpointer)offsety);
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

    GtkWidget *thedialog = g_object_get_data(G_OBJECT(Denemo.gui->printarea), "pad-dialog");
    g_object_set_data(G_OBJECT(Denemo.gui->printarea), "padding", (gpointer)pad);
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
  if(Denemo.gui->pixbuf==NULL)
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
  if(Denemo.gui->pixbuf==NULL)
    return TRUE;
  if(selecting) {
    pointx=event->x;
    pointy=event->y;
    gint width, height;
    normalize();

    width = pointx-markx;
    height = pointy-marky;
    GtkIconFactory *icon_factory = gtk_icon_factory_new ();
    GdkPixbuf *sub_pixbuf = gdk_pixbuf_new_subpixbuf (Denemo.gui->pixbuf, markx+adjust_x, marky+adjust_y, width, height);

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
  busycursor = gdk_cursor_new(GDK_WATCH);
  arrowcursor = gdk_cursor_new(GDK_RIGHT_PTR);//FIXME what is the system cursor called??

  GtkWidget *main_vbox = gtk_vbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (top_vbox), main_vbox, TRUE, TRUE,
		      0);
  GtkWidget *score_and_scroll_hbox = gtk_hbox_new (FALSE, 1);
  gtk_box_pack_start (GTK_BOX (main_vbox), score_and_scroll_hbox, TRUE, TRUE,
		      0);
  gui->printarea = gtk_drawing_area_new ();
  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->printarea, TRUE,
		      TRUE, 0);
  GtkAdjustment *printvadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  g_signal_connect (G_OBJECT (printvadjustment), "value_changed",
		      G_CALLBACK (printvertical_scroll), gui);
  gui->printvscrollbar = gtk_vscrollbar_new (GTK_ADJUSTMENT (printvadjustment));
  gtk_box_pack_start (GTK_BOX (score_and_scroll_hbox), gui->printvscrollbar, FALSE,
		      TRUE, 0);
  GtkAdjustment *printhadjustment =  GTK_ADJUSTMENT (gtk_adjustment_new (1.0, 1.0, 2.0, 1.0, 4.0, 1.0));
  g_signal_connect (G_OBJECT (printhadjustment), "value_changed",
		      G_CALLBACK (printhorizontal_scroll), gui);
  gui->printhscrollbar = gtk_hscrollbar_new (GTK_ADJUSTMENT (printhadjustment));
  gtk_box_pack_start (GTK_BOX (main_vbox), gui->printhscrollbar, FALSE, TRUE, 0);

  g_signal_connect (G_OBJECT (gui->printarea), "configure_event",
		      G_CALLBACK (printarea_configure_event), gui);
  g_signal_connect (G_OBJECT (gui->printarea), "expose_event",
		      G_CALLBACK (printarea_expose_event), gui);
  g_signal_connect (G_OBJECT (gui->printarea), "button_release_event",
		      G_CALLBACK (printarea_button_release), gui);
  g_signal_connect (G_OBJECT (gui->printarea), "motion_notify_event",
		      G_CALLBACK (printarea_motion_notify), gui);
  g_signal_connect (G_OBJECT (gui->printarea), "button_press_event",
		      G_CALLBACK (printarea_button_press), gui);
  gtk_widget_add_events (gui->printarea, (GDK_EXPOSURE_MASK
					  | GDK_POINTER_MOTION_MASK
					  /* | GDK_LEAVE_NOTIFY_MASK */
					  | GDK_BUTTON_PRESS_MASK
					  | GDK_BUTTON_RELEASE_MASK));


  gtk_widget_show_all(main_vbox);
  gtk_widget_hide(main_vbox);
}



#endif /* PRINT_H */
