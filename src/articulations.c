/* articulations.c
 * Implements articulation markings which are not notes 
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * A Tee  (c) 2000-2005
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "chordops.h"
#include "calculatepositions.h"
#include "commandfuncs.h"
#include "contexts.h"
#include "dialogs.h"
#include "draw.h"
#include "objops.h"
#include "staffops.h"
#include "utils.h"
#include "articulations.h"


/**
 * Insert ornament into list if not present 
 * If the ornament is present then remove as action has been
 * performed twice.
 *
 * @param orn  ornament to insert into the ornament list
 * @param list the ornament list
 * @return the new ornament list
 */
GList *
insert_ornament_list (enum ornament orn, GList * list)
{
  /*
   * Find the ornament in the list and remove
   */
  GList *tmp;
  for (tmp = list; tmp; tmp = tmp->next)
    {
      if (*(enum ornament *) tmp->data == (enum ornament) orn)
	return (g_list_remove (list, tmp->data));
    }
  /*
   * Insert new ornament into the list.
   */
  enum ornament *tmporn =
    (enum ornament *) g_malloc0 (sizeof (enum ornament));
  *tmporn = orn;
  list = g_list_append (list, tmporn);
#ifdef DEBUG

  g_print ("Inserted ORNAMENT %d into list \n", *tmporn);
#endif

  return list;
}

/**
 * Set the relevant articulation flag on the 
 * current DenemoObject.
 *
 * @param string textual description of the articulation
 * @param obj		 the object to add the ornament to
 * 
 */
void
set_articulation (gchar * string, DenemoObject * obj)
{



  if (!strcmp (string, "staccato"))
    {
      addornament (obj, STACCATO);
    }
  else if (!strcmp (string, "tenuto"))
    {
      addornament (obj, TENUTO);
    }
  else if (!strcmp (string, "staccatissimo"))
    {
      addornament (obj, STACCATISSIMO);
    }
  else if (!strcmp (string, "accent"))
    {
      addornament (obj, D_ACCENT);
    }
  else if (!strcmp (string, "marcato"))
    {
      addornament (obj, MARCATO);
    }
  else if (!strcmp (string, "fermata"))
    {
      addornament (obj, FERMATA);
    }
  else if (!strcmp (string, "trill"))
    {
      addornament (obj, TRILL);
    }
  else if (!strcmp (string, "turn"))
    {
      addornament (obj, TURN);
    }
  else if (!strcmp (string, "mordent"))
    {
      addornament (obj, MORDENT);
    }
  else if (!strcmp (string, "reverse turn"))
    {
      addornament (obj, REVERSETURN);
    }
  /* String specific articulations */
  else if (!strcmp (string, "up bow"))
    {
      addornament (obj, UBOW);
    }
  else if (!strcmp (string, "down bow"))
    {
      addornament (obj, DBOW);
    }
  /*organ articulations */
  else if (!strcmp (string, "rheel"))
    {
      addornament (obj, RHEEL);
    }
  else if (!strcmp (string, "lheel"))
    {
      addornament (obj, LHEEL);
    }
  else if (!strcmp (string, "rtoe"))
    {
      addornament (obj, RTOE);
    }
  else if (!strcmp (string, "ltoe"))
    {
      addornament (obj, LTOE);
    }
  else if (!strcmp (string, "coda"))
    {
      addornament (obj, CODA);
    }
  else if (!strcmp (string, "arpeggio"))
    {
      addornament (obj, D_ARPEGGIO);
    }
  else if (!strcmp (string, "flagoelet"))
    {
      addornament (obj, FLAGEOLET);
    }
  else if (!strcmp (string, "open"))
    {
      addornament (obj, OPEN);
    }
  else if (!strcmp (string, "prall"))
    {
      addornament (obj, PRALL);
    }
  else if (!strcmp (string, "prallmordent"))
    {
      addornament (obj, PRALLMORDENT);
    }
  else if (!strcmp (string, "prallprall"))
    {
      addornament (obj, PRALLPRALL);
    }
  else if (!strcmp (string, "segno"))
    {
      addornament (obj, SEGNO);
    }
  else if (!strcmp (string, "stopped"))
    {
      addornament (obj, STOPPED);
    }
  else if (!strcmp (string, "thumb"))
    {
      addornament (obj, THUMB);
    }
  else if (!strcmp (string, "upprall"))
    {
      addornament (obj, UPPRALL);
    }
}

/**
 * Insert articulation callback
 * @param widget widget which calls the callback
 * @param si the scoreinfo structure
 * @return none
 */
static void
insert_artic_cb (GtkWidget * widget, DenemoGUI * gui)
{
  DenemoObject *mudelaobj;
  gchar *tmp;
  gchar *articulation;

  g_assert (gui != NULL);

  tmp =
    g_strdup ((gchar *)
	      g_object_get_data (G_OBJECT (widget), "articulation"));

  // Convert stock name to articulation name
  articulation = tmp;
  if (g_str_has_prefix (tmp, "denemo-"))
    {
      char *c;

      articulation += 7;
      for (c = articulation; *c != '\0'; c++)
	{
	  if (*c == '-')
	    {
	      *c = ' ';
	    }
	}
    }

  mudelaobj = (DenemoObject *)
    (gui->si->currentobject ? gui->si->currentobject->data : NULL);

  if (mudelaobj)
    {
      set_articulation (articulation, mudelaobj);
    }

  gtk_widget_queue_draw (Denemo.scorearea);
  gtk_widget_grab_focus (Denemo.window);
  g_free (tmp);
}

/**
 * Creates button for the articulation palette 
 * 
 * @param stock_id	textual description of articulation 
 * @param tips			tooltip for articulation
 * @param table			table to insert button into
 * @param col				column in table to insert button into
 * @param row				row in table to insert button into
 * @param si				the score info structure
 *
 */
static GtkWidget *
create_articulation_button (const gchar * stock_id, 
  GtkTooltip * tips, GtkWidget * table, gint col, gint row,
			    DenemoGUI * si)
{
  GtkWidget *button;
  GtkStockItem stock;
  GtkWidget *image;

  button = gtk_button_new ();
  //gtk_button_set_focus_on_click(GTK_BUTTON(button), FALSE);
  g_object_set_data (G_OBJECT (button), "articulation", (void *) stock_id);

  if (gtk_stock_lookup (stock_id, &stock))
    {
      gtk_tooltip_set_text (tips, stock.label);
      //gtk_tooltips_set_tip (tips, button, stock.label, NULL); //FIXME obsolete function
      image = gtk_image_new_from_stock (stock_id, GTK_ICON_SIZE_BUTTON);
      gtk_container_add (GTK_CONTAINER (button), image);
    }

  gtk_table_attach (GTK_TABLE (table), button, col, col + 1, row, row + 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);

  g_signal_connect (button, "clicked", G_CALLBACK (insert_artic_cb), si);

  return button;
}

/**
 * Create the articulation selection widget.
 * This widget can be put into a dialog or popup.
 * @param si the scoreinfo structure
 * @return the vbox
 */
static GtkWidget *
create_articulation_widget (DenemoGUI * si)
{
  GtkWidget *vbox;
  GtkWidget *label;
  GtkWidget *table;
  GtkWidget *spacer;
  GtkTooltip *tips;
  
  vbox = gtk_vbox_new (FALSE, 8);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 12);

  label = gtk_label_new (_("<b>General</b>"));
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  table = gtk_table_new (4, 7, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

  spacer = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), spacer, 0, 1, 0, 2,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (spacer, 8, -1);

  create_articulation_button ("denemo-staccato", tips, table, 1, 0, si);
  create_articulation_button ("denemo-staccatissimo", tips, table, 2, 0, si);
  create_articulation_button ("denemo-marcato", tips, table, 3, 0, si);
  create_articulation_button ("denemo-accent", tips, table, 4, 0, si);
  create_articulation_button ("denemo-fermata", tips, table, 5, 0, si);
  create_articulation_button ("denemo-coda", tips, table, 6, 0, si);

  create_articulation_button ("denemo-tenuto", tips, table, 1, 1, si);
  create_articulation_button ("denemo-turn", tips, table, 2, 1, si);
  create_articulation_button ("denemo-reverse-turn", tips, table, 3, 1, si);
  create_articulation_button ("denemo-trill", tips, table, 4, 1, si);
  create_articulation_button ("denemo-mordent", tips, table, 5, 1, si);
  create_articulation_button ("denemo-prall", tips, table, 6, 1, si);

  create_articulation_button ("denemo-flagoelet", tips, table, 1, 2, si);
  create_articulation_button ("denemo-open", tips, table, 2, 2, si);
  create_articulation_button ("denemo-prallmordent", tips, table, 3, 2, si);
  create_articulation_button ("denemo-prallprall", tips, table, 4, 2, si);
  create_articulation_button ("denemo-segno", tips, table, 5, 2, si);
  create_articulation_button ("denemo-stopped", tips, table, 6, 2, si);

  create_articulation_button ("denemo-thumb", tips, table, 1, 3, si);
  create_articulation_button ("denemo-upprall", tips, table, 2, 3, si);
  create_articulation_button ("denemo-arpeggio", tips, table, 3, 3, si);


  label = gtk_label_new (_("<b>String</b>"));
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  table = gtk_table_new (1, 3, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

  spacer = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), spacer, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (spacer, 8, -1);

  create_articulation_button ("denemo-up-bow", tips, table, 1, 0, si);
  create_articulation_button ("denemo-down-bow", tips, table, 2, 0, si);

  label = gtk_label_new (_("<b>Organ</b>"));
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  table = gtk_table_new (1, 6, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

  spacer = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), spacer, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (spacer, 12, -1);

  create_articulation_button ("denemo-rheel", tips, table, 1, 0, si);
  create_articulation_button ("denemo-lheel", tips, table, 2, 0, si);
  create_articulation_button ("denemo-rtoe", tips, table, 3, 0, si);
  create_articulation_button ("denemo-ltoe", tips, table, 4, 0, si);

#if 0

  label = gtk_label_new (_("<b>Brass</b>"));
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  table = gtk_table_new (1, 6, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

  spacer = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), spacer, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (spacer, 8, -1);

  label = gtk_label_new (_("<b>Woodwind</b>"));
  gtk_box_pack_start (GTK_BOX (vbox), label, FALSE, FALSE, 0);
  gtk_label_set_use_markup (GTK_LABEL (label), TRUE);
  gtk_misc_set_alignment (GTK_MISC (label), 0, 0.5);

  table = gtk_table_new (1, 6, FALSE);
  gtk_box_pack_start (GTK_BOX (vbox), table, FALSE, TRUE, 0);

  spacer = gtk_label_new ("");
  gtk_table_attach (GTK_TABLE (table), spacer, 0, 1, 0, 1,
		    (GtkAttachOptions) (GTK_FILL),
		    (GtkAttachOptions) (0), 0, 0);
  gtk_widget_set_size_request (spacer, 8, -1);

#endif

  return vbox;
}


/**
 * Callback to hide articulation palette
 */
static gboolean
hide_palette (GtkWidget * widget, GdkEvent * event, DenemoGUI * gui)
{
  GtkWidget *toggle_palette;

  if (gui->articulation_palette)
    {
      toggle_palette = gtk_ui_manager_get_widget (Denemo.ui_manager,
						  "/MainMenu/ViewMenu/ToggleArticulationPalette");
      gtk_check_menu_item_set_active (GTK_CHECK_MENU_ITEM (toggle_palette),
				      FALSE);
    }
  return TRUE;
}

/**
 * Toggle articulation palette
 * @param action the gtk action emitted by menu item
 * @param si the scoreinfo structure
 * @return none
 */
void
toggle_articulation_palette (GtkAction *action, gpointer param)
{
  DenemoGUI *gui = Denemo.gui;
  GtkWidget *window;
  GtkWidget *vbox;

  g_assert (gui != NULL);

  if (gui->articulation_palette)
    {
      if (gtk_widget_get_visible (gui->articulation_palette))
	{
	  gtk_widget_hide (gui->articulation_palette);
	}
      else if (Denemo.prefs.articulation_palette == FALSE
	       && gtk_widget_get_visible (gui->articulation_palette))
	{
	  gtk_widget_hide (gui->articulation_palette);
	}
      else
	{
	  gtk_widget_show (gui->articulation_palette);
	}
      return;
    }

  window = gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gui->articulation_palette = window;
  gtk_window_set_title (GTK_WINDOW (window), _("Articulation"));
  gtk_window_set_focus_on_map (GTK_WINDOW (window), FALSE);
  //GTK_WIDGET_UNSET_FLAGS(window, GTK_CAN_FOCUS);
  gtk_window_set_transient_for (GTK_WINDOW (window), GTK_WINDOW (Denemo.window));
  gtk_window_set_type_hint (GTK_WINDOW (window),
			    GDK_WINDOW_TYPE_HINT_UTILITY);
  gtk_window_set_resizable (GTK_WINDOW (window), FALSE);
  gtk_window_set_role (GTK_WINDOW (window), "articulation-toolbox");

  vbox = create_articulation_widget (gui);
  gtk_container_set_border_width (GTK_CONTAINER (vbox), 8);
  gtk_container_add (GTK_CONTAINER (window), vbox);

  g_signal_connect (window, "delete-event", G_CALLBACK (hide_palette), gui);

  gtk_widget_show_all (window);
}
