/* print.cpp
 * 
 * basic printing support for GNU Denemo
 * outputs to a dvi file
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2001-2005 Adam Tee
 */
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

#include "print.h"
#include "prefops.h"
#include "exportlilypond.h"
#include "utils.h"


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
/* Run the LilyPond interpreter on the file (filename).ly
 * putting the PDF output in (filename).pdf
 * start an external PDF viewer on that file.
 * parse first LilyPond error and position the cursor in gui->textview on it
 */
void
run_lilypond_and_viewer(gchar *filename, DenemoGUI *gui) {
  GError *err = NULL;
  gchar *printfile = g_strconcat (filename, ".pdf", NULL);
  FILE *fp = fopen(printfile, "w");
  if(fp)
    fclose(fp);
  else {
    //FIXME use filename in message
    warningdialog("Could not open ~/.denemo/denemoprint.pdf, check permissions");
    return;
  }
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);
#ifdef G_OS_WIN32
/*   gchar *dirname = g_path_get_dirname (Denemo.prefs.lilypath->str ); */
/*   gchar *convert = g_build_filename(dirname, "convert-ly.py");// FIXME memory leaks */

#else
  gchar *convert = "convert-ly";

  gchar *conv_argv[] = {
    convert,
    "-e",
    lilyfile,
    NULL
  };

/* (denemo:29198): GLib-WARNING **: In call to g_spawn_sync(), exit status of a child process was requested but SIGCHLD action was set to SIG_IGN and ECHILD was received by waitpid(), so exit status can't be returned. This is a bug in the program calling g_spawn_sync(); either don't request the exit status, or don't set the SIGCHLD action. 
 FIXED*/

  g_spawn_sync (locatedotdenemo (),		/* dir */
		conv_argv, NULL,	/* env */
		G_SPAWN_SEARCH_PATH, NULL,	/* child setup func */
		NULL,		/* user data */
		NULL,		/* stdout */
		NULL,		/* stderr */
		NULL, &err);

  if (err != NULL)
    {
      warningdialog("Could not execute lilypond's convert-ly program - check lilypond installation or just ignore");
      g_warning ("%s", err->message);
      g_error_free (err);
      err = NULL;
    }
#endif




  gchar *argv[] = {
    Denemo.prefs.lilypath->str,
    "--pdf",
    "-o",
    filename,
    lilyfile,
    NULL
  };

  gchar *output=NULL, *errors=NULL;


  g_spawn_sync (locatedotdenemo (),		/* dir */
		argv, NULL,	/* env */
		G_SPAWN_SEARCH_PATH, NULL,	/* child setup func */
		NULL,		/* user data */
		&output,		/* stdout */
		&errors,		/* stderr */
		NULL, &err);
  gchar *filename_colon = g_strdup_printf("%s%s",lilyfile,":");
  //g_print("filename_colon = %s\n", filename_colon);
  gchar *epoint = NULL;
  if(errors) 
    epoint = g_strstr_len (errors,strlen(errors), filename_colon);

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
      infodialog(epoint);
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
  if (err != NULL)
    {
      if(errors)
	infodialog(errors);
      warningdialog("Could not execute lilypond - check Edit->preferences->externals->lilypond setting\nand lilypond installation");
      g_warning ("%s", err->message);
      g_error_free (err);
      err = NULL;
    }


  if((fp=fopen(printfile, "r"))) {
    if(getc(fp)==EOF) {
      g_warning ("Failed to read %s", (gchar *) printfile);
      warningdialog("Cannot make score, probably errors in lilypond output");
      fclose(fp);
      return;
    }
  } else
  {
    g_warning ("Failed to find %s", (gchar *) printfile);
    warningdialog("Could not create a pdf - check permissions");
    return;
  }
    

  //g_print("using %s\n", printfile);
  gchar *args[] = {
    Denemo.prefs.pdfviewer->str,
    printfile,
    NULL
  };

  GPid printpid;//ignored
  g_spawn_async (locatedotdenemo (),		/* dir */
		 args, NULL,	/* env */
		 G_SPAWN_SEARCH_PATH, /* search in path for executable */
		 NULL,	/* child setup func */
		 NULL,		/* user data */		
		 NULL, /* FIXME &printpid see g_spawn_close_pid(&printpid) */
		 &err);
  

  if (err != NULL)
    {
      g_warning ("Failed to find %s", Denemo.prefs.pdfviewer->str);
      warningdialog("Cannot display: Check Edit->Preferences->externals\nfor your PDF viewer");
      g_warning ("%s", err->message);
      g_error_free (err);
      err = NULL;
    }
  g_free(printfile);
}

/* returns the base name (~/.denemo/denemoprint usually) used as a base
   filepath for printing. On windows there is some filelocking trouble.
   The returned string should not be freed.
*/
   
gchar *get_printfile_pathbasename(void) {
  static gchar *filename = NULL;
#ifdef G_OS_WIN32
 {
   static int count=1;
   gchar *denemoprint = g_strdup_printf("denemoprint%d", count);
   count++;
   if(filename)
     g_free(filename);
   filename = g_build_filename ( locatedotdenemo (), denemoprint, NULL);
   g_free(denemoprint);
 }
#else
  if (!filename)
      filename = g_build_filename ( locatedotdenemo (), "denemoprint", NULL);
#endif
  return filename;
}

/*  Print function 
 *  Save file in lilypond format
 *  Fork and run lilypond
 *  TODO Add in lpr command
 */
static void
print (DenemoGUI * gui, gboolean part_only, gboolean all_movements)
{
  DenemoScore *si = gui->si;
  gchar *filename = get_printfile_pathbasename();
  gchar *lilyfile = g_strconcat (filename, ".ly", NULL);
  remove (lilyfile);
  if(part_only)
    export_lilypond_part (lilyfile, gui, si->start, si->end, all_movements);
  else
    exportlilypond (lilyfile, gui, 0, 0, all_movements);
  run_lilypond_and_viewer(filename, gui);
  g_free(lilyfile);
}



/* callback to print whole of score */
void
printall_cb (GtkAction * action, DenemoGUI * gui) {
  gchar *str = g_strdup_printf("No direct printing yet\nWe will run the PDF viewer program %s so you can use its print command.\nYou can change the PDF viewer using \nEdit->Preferences->Externals->Pdf viewer.", Denemo.prefs.pdfviewer->str);
  warningdialog(str);
  g_free(str);
  print(gui, FALSE, TRUE);
}
/* callback to print current part (staff) of score */
void
printpart_cb (GtkAction * action, DenemoGUI * gui) {
  if((gui->movements && g_list_length(gui->movements)>1) && 
     (confirm("This piece has several movements", "Print this part from all of them?")))
    print(gui, TRUE, TRUE);
  else
   print(gui, TRUE, FALSE);
  
}
void
printpreview_cb (GtkAction * action, DenemoGUI * gui) {
  print(gui, FALSE, TRUE);
}
