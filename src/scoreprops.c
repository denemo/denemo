/* scoreprops.c
 * Callback that creates a "Score Properties" dialog box.
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 1999-2005 Adam Tee, Matthew Hiller */

#include <stdlib.h>
#include <string.h>
#include <denemo/denemo.h>
#include "utils.h"
#include "commandfuncs.h"
#include "dialogs.h"

#define COL_NAME 0
#define COL_VALUE 1
#define COL_PTR 2

/**
 * Lilyponds supported font sizes
 */
static gchar *fontsizes[8] =
  {
    "11", "13", "14", "16", "18", "20", "23","26"
  };

/**
 * Lilyponds supported paper sizes
 */
static gchar *papersizes[6] =
  {
    "a4", "a6", "a5", "legal", "letter", "tabloid"
  };



typedef struct papersetupcb
{
	GtkWidget *papersize;
	GtkWidget *lilyversion;
	GtkWidget *portrait;
	GtkWidget *fontsize;
	GtkWidget *lilypond;
}papersetupcb;

static void
cell_edited (GtkCellRendererText * cellrenderertext,
	     gchar * path_string, gchar * new_text, GtkTreeModel * model)
{
  GtkTreePath *path = gtk_tree_path_new_from_string (path_string);
  GtkTreeIter iter;

  gtk_tree_model_get_iter (model, &iter, path);
  gtk_list_store_set (GTK_LIST_STORE (model), &iter, COL_VALUE, new_text, -1);
  gtk_tree_path_free (path);
}
static  
gboolean abandon_editprops_custom_scoreblock(DenemoGUI *gui) {
  if(gui->custom_scoreblocks)
    return confirm("Custom LilyPond Score Block", "You will need to edit the LilyPond text to copy these edits from the standard scoreblock.\nIt might be easier to edit your custom scoreblock directly. Abandon?");
  return FALSE;
}

static papersetupcb *
papersetup(GtkWidget *notebook, DenemoGUI *gui, gboolean isnotebook)
{
  gint i;

  papersetupcb *setup = (papersetupcb *) g_malloc0(sizeof(papersetupcb));
  GtkWidget *table = gtk_table_new(3, 4 , FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  GtkWidget *label = gtk_label_new(_("Paper Size"));
  gtk_table_attach(GTK_TABLE(table), label, 0, 1,0 ,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  GtkWidget *papersize = gtk_combo_box_text_new();
  for(i=0; i < 6; i++)
    {
      gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(papersize), papersizes[i]);
    }

  //gtk_entry_set_text(GTK_ENTRY(GTK_BIN(papersize)->child), 
//		    gui->lilycontrol.papersize->len? gui->lilycontrol.papersize->str:"");
  gtk_table_attach(GTK_TABLE(table), papersize,1,2,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new(_("Font Size"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,1,2,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  GtkWidget *fontsize = gtk_combo_box_text_new();
  for(i=0; i < 8; i++)
    {
      gtk_combo_box_text_append_text(GTK_COMBO_BOX_TEXT(fontsize), fontsizes[i]);
    }
  gchar *tmp;
  //tmp = g_strdup_printf( "%d", gui->lilycontrol.fontsize);
  //gtk_entry_set_text (GTK_ENTRY (GTK_BIN(fontsize)->child),  gui->lilycontrol.staffsize->len?gui->lilycontrol.staffsize->str:"");
  //g_free(tmp);
  gtk_table_attach(GTK_TABLE(table), fontsize, 1,2,1,2,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);


  label = gtk_label_new(_("Lilypond Version"));
  gtk_table_attach(GTK_TABLE(table), label, 0,1,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  GtkWidget *lilyversion = gtk_entry_new();
  gtk_table_attach(GTK_TABLE(table), lilyversion, 1,2,2,3,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);
  gtk_entry_set_text(GTK_ENTRY(lilyversion), gui->lilycontrol.lilyversion->len?
		     gui->lilycontrol.lilyversion->str:"");






  GtkWidget *vbox = gtk_vbox_new(FALSE,0);
  GtkWidget *portraitradio = 
    gtk_radio_button_new_with_label(NULL, _("Portrait"));
  gtk_box_pack_start(GTK_BOX(vbox), portraitradio, TRUE, TRUE,0);
  
  GtkWidget *landscaperadio = 
    gtk_radio_button_new_with_label
    (gtk_radio_button_get_group (GTK_RADIO_BUTTON (portraitradio)),_("Landscape"));
  gtk_box_pack_start(GTK_BOX(vbox), landscaperadio, TRUE, TRUE,0);

  if(gui->lilycontrol.orientation)
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(portraitradio), TRUE);
  else
    gtk_toggle_button_set_active (GTK_TOGGLE_BUTTON(landscaperadio), TRUE);
  gtk_table_attach(GTK_TABLE(table), vbox, 2,3,0,1,
                   (GtkAttachOptions) (GTK_FILL),
                   (GtkAttachOptions) (0), 0, 0);

  if(isnotebook)
    {
      gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, NULL);
      gtk_notebook_set_tab_label_text (GTK_NOTEBOOK (notebook), table,
                                       _("Paper Setup"));
    }
  else
    {
      gtk_box_pack_start(GTK_NOTEBOOK(notebook), table, TRUE, TRUE,0);
    }

  setup->papersize = papersize;
  setup->fontsize = fontsize;
  setup->portrait = portraitradio;
  setup->lilyversion = lilyversion;

  score_status(gui, TRUE);
  return setup;
}

/**
 * Function to set the printed score parameters
 *
 */
static void 
setpaperconfig(papersetupcb *cbdata, DenemoGUI *gui)
{
  g_string_assign(gui->lilycontrol.papersize, 
		  (gchar *) 
		    gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (cbdata->papersize)));
  
  g_string_assign(gui->lilycontrol.lilyversion, 
		  (gchar *)gtk_entry_get_text 
		  (GTK_ENTRY (cbdata->lilyversion)));

  g_string_assign(gui->lilycontrol.staffsize,
    (gchar *) 
      gtk_combo_box_text_get_active_text (GTK_COMBO_BOX_TEXT (cbdata->fontsize)));

  if(gtk_toggle_button_get_active (GTK_TOGGLE_BUTTON(cbdata->portrait)))
    {
      gui->lilycontrol.orientation = TRUE;
    }
  else
    gui->lilycontrol.orientation = FALSE;

  //g_print(" %s %s %d %d \n", gui->lilycontrol.papersize->str, gui->lilycontrol.lilyversion->str, gui->lilycontrol.fontsize, gui->lilycontrol.orientation);
  score_status(gui, TRUE);
}


/**
 * Create and run a modal score properties dialog.
 */
void
score_properties_dialog (GtkAction *action, DenemoScriptParam *param)
{
  DenemoGUI *gui = Denemo.gui;

  GET_1PARAM(action, param, fontsize);
 if(query) {
   if(*query) if(!strcmp("fontsize", query)) {
     g_string_assign(param->string, Denemo.gui->lilycontrol.staffsize->str);
     param->status = TRUE;
   }
   return;
 }
 
  if(fontsize) {
    g_string_assign(Denemo.gui->lilycontrol.staffsize, fontsize);
    param->status = TRUE;
    return;
  }

  if(abandon_editprops_custom_scoreblock(gui))
    return;
  GtkWidget *dialog;
  GtkWidget *notebook;
  GtkWidget *scrolled_window;
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *list;
  GtkListStore *list_store = NULL;
  GtkTreeIter iter;
  GtkCellRenderer *renderer;
  GtkWidget *measure_width;
  GtkWidget *staff_spacing;
  papersetupcb *cbdata = NULL;

  DenemoScore *si = gui->si;
  g_assert (si != NULL);

  dialog = gtk_dialog_new_with_buttons (_("Score properties"),
					GTK_WINDOW (Denemo.window),
					(GtkDialogFlags) (GTK_DIALOG_MODAL |
							  GTK_DIALOG_DESTROY_WITH_PARENT),
					GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
					GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT,
					NULL);

  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  //gtk_dialog_set_has_separator (GTK_DIALOG (dialog), FALSE);

  notebook = gtk_notebook_new ();

  GtkWidget *content_area = gtk_dialog_get_content_area (GTK_DIALOG (dialog));
  gtk_container_add (GTK_CONTAINER (content_area), notebook);

  // Layout
  label = gtk_label_new_with_mnemonic (_("Display Layout"));
  table = gtk_table_new (2, 2, FALSE);
  gtk_container_set_border_width (GTK_CONTAINER (table), 12);
  gtk_table_set_row_spacings (GTK_TABLE (table), 8);
  gtk_table_set_col_spacings (GTK_TABLE (table), 8);

  gtk_notebook_append_page (GTK_NOTEBOOK (notebook), table, label);

  label = gtk_label_new (_("Measure width (pixels):"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  measure_width = gtk_spin_button_new_with_range (10.0, 1000, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (measure_width),
			     (gdouble) si->measurewidth);
  gtk_table_attach (GTK_TABLE (table), measure_width, 1, 2, 0, 1,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);


  label = gtk_label_new (_("Staff spacing (pixels):"));
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);
  gtk_table_attach (GTK_TABLE (table), label, 0, 1, 1, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  staff_spacing =
    gtk_spin_button_new_with_range (2 * STAFF_HEIGHT, 1000, 1.0);
  gtk_spin_button_set_value (GTK_SPIN_BUTTON (staff_spacing),
			     (gdouble) si->staffspace);
  gtk_table_attach (GTK_TABLE (table), staff_spacing, 1, 2, 1, 2,
		    (GtkAttachOptions) (GTK_FILL), (GtkAttachOptions) (0), 0,
		    0);

  cbdata = papersetup (notebook, gui, TRUE);
  gtk_dialog_set_default_response (GTK_DIALOG (dialog), GTK_RESPONSE_ACCEPT);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
    {

	/*
       if (!si->lily_file)
	{
	  gchar *value;
	  GString *dest;
	  gtk_tree_model_get_iter_first (GTK_TREE_MODEL (list_store), &iter);
	  do
	    {
	      gtk_tree_model_get (GTK_TREE_MODEL (list_store), &iter,
				  COL_VALUE, &value, COL_PTR, &dest, -1);
	      g_string_assign (dest, value);
	    }
	  while (gtk_tree_model_iter_next
		 (GTK_TREE_MODEL (list_store), &iter));
	}
	*/

      // Set layout options
      gint width =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (measure_width));
      if (width != si->measurewidth)
	{
	  si->measurewidth = width;
	  adjustmeasurewidth (si, 0);
	  gtk_widget_queue_draw (Denemo.scorearea);
	}

      gint spacing =
	gtk_spin_button_get_value_as_int (GTK_SPIN_BUTTON (staff_spacing));
      if (spacing != si->staffspace)
	{
	  si->staffspace = spacing;
	  gtk_widget_queue_draw (Denemo.scorearea);
	}
      setpaperconfig (cbdata, gui);

    }
  g_free (cbdata);
  gtk_widget_destroy (dialog);
}





/***************** movement properties *********************/
struct callbackdata
{
  DenemoGUI *gui;

  GtkWidget *title;	  
  GtkWidget *subtitle;	  
  GtkWidget *poet;	  
  GtkWidget *composer;	  
  GtkWidget *meter;	  
  GtkWidget *opus; 	  
  GtkWidget *arranger;	  
  GtkWidget *instrument;	  
  GtkWidget *dedication;	  
  GtkWidget *piece;	  
  GtkWidget *head;	  
  GtkWidget *copyright;	  
  GtkWidget *footer;	  
  GtkWidget *tagline;	  

  GtkWidget *lilypond_before; 
  GtkWidget *lilypond_after;  
  // GtkWidget *layout;
  // GtkWidget *extra;  
};

/**
 * Function to set the properties
 *
 */
static void
set_movement_props (gpointer data)
{

}



/**
 * Creates a dialog for setting properties of current movement gui->si
 *
 */
void
movement_props_dialog (GtkAction *action, DenemoScriptParam * param)
{

}
