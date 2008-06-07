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
  gchar **printfile;
  if (gui->lilycontrol.excerpt == TRUE)
	printfile = g_strconcat (filename, ".png", NULL);
  else
  	printfile = g_strconcat (filename, ".pdf", NULL);
  
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
  //pointer to pointer that changes according to *argv[]

  gchar **arguments;
  if (gui->lilycontrol.excerpt == TRUE){
	  gchar *argv[] = {
		    Denemo.prefs.lilypath->str,
		    "--png",
		    "-b",
		    "eps", 
		    "-o",
		    filename,
		    lilyfile,
		    NULL
	  };
	  arguments = argv;
  }
  else {
	  gchar *argv[] = {
		   
		    Denemo.prefs.lilypath->str,
		    "--pdf",
		    "-o",
		    filename,
		    lilyfile,
		    NULL
	  };
	  arguments = argv;
  }

  gchar *output=NULL, *errors=NULL;


  g_spawn_sync (locatedotdenemo (),		/* dir */
		arguments, NULL,	/* env */
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
    
  //gchar **arguments;
  //g_print("using %s\n", printfile);
  if (gui->lilycontrol.excerpt == TRUE){
  	  gchar *args[] = {
	    Denemo.prefs.imageviewer->str,
	    printfile,
	    NULL
	  };
	  arguments = args;
  }
  else {
	  gchar *args[] = {
	    Denemo.prefs.pdfviewer->str,
	    printfile,
	    NULL
	  };
	  arguments = args;  
  }


  GPid printpid;//ignored
  g_spawn_async (locatedotdenemo (),		/* dir */
		 arguments, NULL,	/* env */
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
  gui->lilycontrol.excerpt = FALSE;
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

/** 
 * Dialog function used to select measure range 
 *
 */

void
PrintRageDialog(DenemoGUI * gui){
	GtkWidget *dialog;
	GtkWidget *table;
	GtkWidget *label;
	GtkWidget *hbox;
	GtkWidget *from_measure;
	GtkWidget *to_measure;
	GtkWidget *print_measure;
	GtkWidget *print_entire_piece;
	GtkWidget *print_all_staves;
	GtkWidget *print_only;
	GtkWidget *staves;
	staffnode *n;

	dialog = gtk_dialog_new_with_buttons (_("Print Excerpt Range"),
		 GTK_WINDOW (gui->window),
		 (GtkDialogFlags) (GTK_DIALOG_MODAL |
		      GTK_DIALOG_DESTROY_WITH_PARENT),
		 GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
		 GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

	hbox = gtk_hbox_new (FALSE, 8);
	//table = gtk_table_new (8, 2, FALSE);
/*
  	print_entire_piece = gtk_radio_button_new_with_label (NULL, _("Print from cursor to end"));
  	gtk_table_attach (GTK_TABLE (table), print_entire_piece, 0, 1, 0, 1,
		                        (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
					                    0);
*/

	print_measure = gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON
		       (print_entire_piece),
		       _("Print measure"));
	gtk_box_pack_start (GTK_BOX (hbox), print_measure, FALSE, FALSE, 0);
/*
  gint max_measure =
    g_list_length (((DenemoStaff *) (gui->si->thescore->data))->measures);

  from_measure =
    gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), from_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (from_measure),
			     (gdouble) gui->si->start);

  label = gtk_label_new (_("to"));
  gtk_box_pack_start (GTK_BOX (hbox), label, FALSE, FALSE, 0);

  to_measure =
    gtk_spin_button_new_with_range (1.0, (gdouble) max_measure, 1.0);
  gtk_box_pack_start (GTK_BOX (hbox), to_measure, TRUE, TRUE, 0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (to_measure),
			     (gdouble) gui->si->end);

  if (gui->si->start == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (print_entire_piece),
				    TRUE);
      gtk_widget_set_sensitive (from_measure, FALSE);
      gtk_widget_set_sensitive (to_measure, FALSE);
    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (print_measure), TRUE);
    }


  label = gtk_label_new (_("<b>Print Staves</b>"));
  gtk_table_attach (GTK_TABLE (table), label, 0, 2, 5, 6,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 6, 7,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  print_all_staves =
    gtk_radio_button_new_with_label (NULL, _("Print all staves"));
  gtk_table_attach (GTK_TABLE (table), print_all_staves, 1, 2, 6, 7,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);
  // spacer
  label = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 7, 8,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (label, 12, -1);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_table_attach (GTK_TABLE (table), hbox, 1, 2, 7, 8,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (GTK_FILL), 0, 0);

  print_only =
    gtk_radio_button_new_with_label_from_widget (GTK_RADIO_BUTTON
						 (print_all_staves),
						 _("Print only"));
  gtk_box_pack_start (GTK_BOX (hbox), print_only, FALSE, FALSE, 0);

  staves = gtk_combo_box_new_text ();
  gtk_box_pack_start (GTK_BOX (hbox), staves, TRUE, TRUE, 0);
  for (n = gui->si->thescore; n != NULL; n = g_list_next (n))
    {
      gchar *staff_label = NULL;
      DenemoStaff *s = (DenemoStaff *) n->data;
      if (s->staff_name != NULL)
	{
	  staff_label = s->staff_name->str;
	}
      else if (s->denemo_name != NULL)
	{
	  staff_label = s->denemo_name->str;
	}

      if (staff_label == NULL)
	{
	  staff_label = _("Unnamed staff");
	}
      gtk_combo_box_append_text (GTK_COMBO_BOX (staves), staff_label);
    }
  if (gui->si->stafftoplay == 0)
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (print_all_staves),
				    TRUE);
      gtk_widget_set_sensitive (staves, FALSE);
      gtk_combo_box_set_active (GTK_COMBO_BOX (staves), 0);
    }
  else
    {
      gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON (print_only), TRUE);
      gtk_combo_box_set_active (GTK_COMBO_BOX (staves),
				gui->si->stafftoplay - 1);
    }
  */
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {
      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (print_measure)))
	{
	  //gui->si->start =
	   // gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (from_measure));
	  //gui->si->end =
	   // gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (to_measure));
	}
      else
	{

	  //gui->si->start = 0;
	  //gui->si->end = 0;
	}

      if (gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON (print_only)))
	{
	  //gui->si->stafftoplay =
	    //1 + gtk_combo_box_get_active (GTK_COMBO_BOX (staves));
	}
      else
	{
	  //gui->si->stafftoplay = 0;
	}
    }
  gtk_widget_destroy (dialog);

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
void
PrintExcerptPreview_cb (GtkAction * action, DenemoGUI * gui) {
  gui->lilycontrol.excerpt = TRUE;
  PrintRageDialog(gui);
  print(gui, FALSE, FALSE);
}

