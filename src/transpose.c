#include "exportcsound.h"
#include "utils.h"

void change_note(GList *notes, int amount){
	GList *tnode = notes;
	note *cur_tone = (note *) tnode;
	cur_tone->mid_c_offset += amount;
	printf("\n note = %i\n",(int) cur_tone->mid_c_offset);
}

void transpose_entire_piece(DenemoGUI *gui){
  staffnode *curstaff = NULL;
  curstaff = gui->si->thescore;
  DenemoStaff *curstaffstruct;
  gint i;
  
  for (curstaff = gui->si->thescore, i = 1; curstaff;
  		curstaff = curstaff->next, i++){
    curstaffstruct = (DenemoStaff *) curstaff->data;
    //transpose_staff (gui, i);
  }
}
	
void transpose_staff (DenemoGUI *gui, gint amount){
  staffnode *curstaff = NULL;
  curstaff = gui->si->thescore;
  DenemoStaff *curstaffstruct = (DenemoStaff *) curstaff->data;

  measurenode *curmeasure;
  objnode *curobj;
  DenemoObject *mudelaitem;
  GList *node = NULL;

  for (curmeasure = (measurenode *) curstaffstruct->measures;
       curmeasure; curmeasure = curmeasure->next)
    {
      for (curobj = (objnode *) curmeasure->data; curobj;
	   curobj = curobj->next)
	{
	  mudelaitem = (DenemoObject *) curobj->data;
	  note *newnote = NULL;
	  if (mudelaitem->type == CHORD)
	    {
	      node = ((chord *) mudelaitem->object)->notes;
		g_list_foreach(node, (GFunc )change_note, (int *) amount);

	    }
	}
    }
}

void 
new_transpose_staff_dialog(DenemoGUI *gui){
  GtkWidget *dialog;
  GtkWidget *label;
  GtkWidget *hbox;
  
  dialog = gtk_dialog_new_with_buttons (_("Transpose Staff"),
           GTK_WINDOW (Denemo.window),
           (GtkDialogFlags) (GTK_DIALOG_MODAL |
           GTK_DIALOG_DESTROY_WITH_PARENT),
           GTK_STOCK_OK, GTK_RESPONSE_ACCEPT,
           GTK_STOCK_CANCEL, GTK_RESPONSE_REJECT, NULL);

  hbox = gtk_hbox_new (FALSE, 8);
  gtk_box_pack_start (GTK_BOX (GTK_DIALOG (dialog)->vbox), hbox, TRUE, TRUE, 0);

  gtk_widget_show (hbox);
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_window_set_modal (GTK_WINDOW (dialog), TRUE);
  gtk_widget_show_all (dialog);

  if (gtk_dialog_run (GTK_DIALOG (dialog)) == GTK_RESPONSE_ACCEPT)
  {
  }
  // gtk_widget_destroy (dialog);
}

gboolean staff_transposition (GtkAction * action){
  DenemoGUI *gui = Denemo.gui;
  gchar *transpose_amount_char = string_dialog_entry(gui, "Transpose Staff", "place and integer amount to transpose the staff notes", NULL);
  if (transpose_amount_char){
    gint transpose_amount = atoi(transpose_amount_char);
    transpose_staff (gui, transpose_amount);
    g_free(transpose_amount_char);
  }
  //transpose_staff_dialog(gui);
  score_status(gui, TRUE);
  displayhelper(gui);
  return TRUE;
}


