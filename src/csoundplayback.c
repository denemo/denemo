/**
 * csoundplayback.c
 *
 * CSound playback functions, 
 * runs csound with -g -W flags (creates wav file)
 * for Denemo, a GTK Frontend to GNU Lilypond
 *
 * (c) 2002-2005 Adam Tee 
 */
#include "denemo/denemo.h"
//#include "csoundplayback.h"
//#include "exportcsound.h"
//#include "prefops.h"
//#include "utils.h"
#include "external.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <signal.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
#include <wait.h>
#endif
#include <errno.h>

/**
 * Local function prototypes
 */
void selorcfile (GtkWidget * widget, gpointer data);
//void chooseorcfile (GtkWidget * widget, struct cs_callback *data);

/**
 * CSound playback to set the csound argument and 
 * orchestra file to use
 */

void
csoundplayback (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
#ifdef G_OS_WIN32
  g_print ("csound playback is currently not working on windows...\n");
#else /* not G_OS_WIN32 */
  GString *filename;
  GString *pidpath;
  GString *outputfile;
  GString *csoundline;
  gboolean extplayer;
  GPid playerpid = -1;
  FILE *fp;
  int got = 0;

  pidpath = g_string_new ((const gchar *)get_temp_filename (ext_pidfiles[EXT_CSOUND]));
  fp = fopen (pidpath->str, "rb+");
#ifdef HAVE_FILE_LOCKS
  if (ftrylockfile (fp))	/* FIXME: is it multiprocs? */
    {
      g_debug ("ext_csound: another instance is working\n");
      /* better chance next time */
      fclose (fp);
      g_string_free (pidpath, TRUE);
      return;
    }
#endif /* HAVE_FILE_LOCKS */
  /* ok, it's our turn: */
  got = fread (&playerpid, sizeof(GPid), 1, fp);
  rewind (fp);

  g_debug ("ext_csound: got player %d\n", playerpid);

  /* ensure we got something */
  if (got && (playerpid > 0))
    {
      g_debug ("ext_csound: killing player: %d\n", playerpid);
      kill (playerpid, SIGTERM);
      g_spawn_close_pid (playerpid);	/* fix leaks on win32 */
      /* sleep(1); wait /dev/dsp do be *really* closed */
    }

  filename = g_string_new ((const gchar *)get_temp_filename ("denemocsoundplayback.sco"));
  exportcsound (filename->str, gui->si, gui->si->start, gui->si->end);
  extplayer = Denemo.prefs.rtcs;
  if (!extplayer)
    outputfile = g_string_new ("devaudio ");
  else
    outputfile = g_string_new ((const gchar *)get_temp_filename ("denemocsoundplayback.wav"));
  csoundline = g_string_new (Denemo.prefs.csoundcommand->str);
  g_string_append (csoundline, " -o ");
  g_string_append (csoundline, outputfile->str);
  if (extplayer)
    g_string_append (csoundline, " -W ");
  g_string_append (csoundline, Denemo.prefs.csoundorcfile->str);
  g_string_append (csoundline, "\t");
  g_string_append (csoundline, filename->str);
  /* the *real* moment: */
  playerpid = spawn_external (csoundline);

  g_debug ("ext_csound: spawned %d\n", playerpid);

  if ((playerpid > 0) && extplayer)	/* play _after_ render */
    {
      wait (playerpid);

      g_debug ("csound pid %d finished\n", playerpid);

      g_string_free (csoundline, TRUE);
      csoundline = g_string_new (Denemo.prefs.audioplayer->str);
      g_string_append (csoundline, "\t");
      g_string_append (csoundline, outputfile->str);
      playerpid = spawn_external (csoundline);
      if (playerpid > 0)
        fwrite (&playerpid, sizeof(GPid), 1, fp);
#ifdef HAVE_FILE_LOCKS
      funlockfile (fp);		/* FIXME: is it multiprocs? */
#endif
      fclose (fp);
      g_string_free (csoundline, TRUE);
      g_string_free (outputfile, TRUE);
    }
  else if (playerpid > 0)
    {				/* we were on devaudio */
      fwrite (&playerpid, sizeof(GPid), 1, fp);
#ifdef HAVE_FILE_LOCKS
      funlockfile (fp);		/* FIXME: is it multiprocs ? */
#endif
      fclose (fp);
      g_string_free (csoundline, TRUE);
    }
#endif /* not G_OS_WIN32 */
}

void
dnm_csoundplayback (GtkAction *action, gpointer param)
{
	csoundplayback (action, param);	
}
/**
 * Select the csound orchestra to use during playback
 */
void
chooseorcfile (GtkWidget * widget, struct cs_callback *data)
{
  GtkWidget *fs;
  GtkFileFilter *filter;
  DenemoGUI *gui = data->gui;
  GtkWidget *entry = data->entry;


  fs = gtk_file_chooser_dialog_new (_("Choose CSound Orchestra File"),
				    GTK_WINDOW (data->dialog),
				    GTK_FILE_CHOOSER_ACTION_OPEN,
				    GTK_STOCK_CANCEL,
				    GTK_RESPONSE_REJECT,
				    GTK_STOCK_OPEN,
				    GTK_RESPONSE_ACCEPT, NULL);

  filter = gtk_file_filter_new ();
  gtk_file_filter_set_name (filter, "CSound Orchestra File");
  gtk_file_filter_add_pattern (filter, "*.orc");
  gtk_file_chooser_add_filter (GTK_FILE_CHOOSER (fs), filter);

  gtk_widget_show_all (fs);
  if (gtk_dialog_run (GTK_DIALOG (fs)) == GTK_RESPONSE_ACCEPT)
    {
      g_string_assign (Denemo.prefs.csoundorcfile,
		       gtk_file_chooser_get_filename (GTK_FILE_CHOOSER (fs)));
      gtk_entry_set_text (GTK_ENTRY (entry), Denemo.prefs.csoundorcfile->str);

    }
  gtk_widget_destroy (fs);
}

void
dnm_chooseorcfile (GtkWidget * widget, struct cs_callback *data)
{
	chooseorcfile (widget, data);

}

