/*
 * analysis_highlighting.c
 * Implements note highlighting for 
 * analysis results
 *
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * A Tee (c) 2001-2005
 */

#include "importresults.h"
//#include "../src/frogio.h"
//#include "../src/prefops.h"
//#include "../src/moveviewport.h"

#include <unistd.h>
#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
# include <sys/wait.h>
#endif
#ifdef HAVE_WAIT_H
# include <wait.h>
#endif
#include <errno.h>

#define GTK_ENABLE_BROKEN 
struct callbackdata {
  struct scoreinfo *si;
  GtkWidget *algocombo;
  GtkWidget *contourchoice;
  GtkWidget *outputentry;
  GtkWidget *threshold;
  GtkWidget *harmonicentry;
  GtkWidget *query;
};

void unhighlight(GtkWidget *widget, gpointer callback_data); 

void highlight(GtkWidget *widget, gpointer callback_data);

//void perform_analysis(GtkWidget *entry,struct scoreinfo *si, int algorithm, 
//		      int contour, int threshold, int features,
//		      const gchar *sequence);
int beatcmp(int bar1, float beat1, int bar2, float beat2);
void read_resultsfile(GtkWidget *widget, gpointer callback_data);
Results *createnewres(void);
void note_highlight(DenemoGUI *si,
		    int staff, int sbar, float sbeat, 
		    int ebar, float ebeat);
void analysis_gui(GtkMenuItem *menuitem, gpointer data);
void run_analysis(GtkWidget *widget, gpointer data);
int get_algorithm(GtkWidget *combobox);
int get_contour(GtkWidget *combobox);
void add_harmony(GtkWidget *widget, gpointer data);
int getfeatures(GtkWidget *combo);

#ifdef __cplusplus
extern "C" {
#endif
  void denemo_plugin_init(DenemoGUI *si, PluginData *pd);
  void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd);
#ifdef __cplusplus
}
#endif

AnalysisResPtr results = NULL;

void cb_itemselect(GtkWidget *item, 
		   gpointer data)
{
  gchar *name;
  GtkLabel *label;
  int staff;
  int sbar,ebar;
  float sbeat, ebeat;
  /*struct callbackdata *cbdata = (struct callbackdata *)data;
  g_print("Item select cbdata si %p\n", cbdata->si);*/
  DenemoGUI *si = (DenemoGUI *)data;

  label = GTK_LABEL(GTK_BIN(item)->child);
  gtk_label_get(label, &name);
  sscanf(name, "%d,%d,%f,%d,%f", &staff,&sbar,&sbeat,&ebar,&ebeat);
#ifdef DEBUG
  printf("Staff %d, SBar %d, SBeat %f, EBar %d, EBeat %f\n",
	 staff,sbar,sbeat,ebar,ebeat);
#endif
  if(si)
    note_highlight(si, staff,sbar,sbeat,ebar,ebeat);
}

void cb_itemdeselect(GtkWidget *item, 
		     gpointer data)
{
  gchar *name;
  GtkLabel *label;
  int staff;
  int sbar,ebar;
  float sbeat, ebeat, similarity;
  DenemoGUI *si = (DenemoGUI *)data;
  
  label = GTK_LABEL(GTK_BIN(item)->child);
  gtk_label_get(label, &name);
  sscanf(name, "%d,%d,%f,%d,%f,%f", &staff,&sbar,&sbeat,&ebar,&ebeat, 
         &similarity);
#ifdef DEBUG
  printf("Staff %d, SBar %d, SBeat %f, EBar %d, EBeat %f\n",
	 staff,sbar,sbeat,ebar,ebeat);
#endif
  if(si)
    note_highlight(si, staff,sbar,sbeat,ebar,ebeat);
  
}



/* Initialize the plugin 
 *
 * Add extra menu items
 */
#ifdef __cplusplus
extern "C" {
#endif
  //Nasty Global to test removing menu items 
  static GtkWidget *root_menu = NULL;
  void denemo_plugin_init(DenemoGUI *si, PluginData *pd)
  {
    GtkWidget *menu_item1;
    GtkWidget *menu_item2;
    GtkWidget *menu_item3;
    GtkWidget *menu_item4;
    GtkWidget *menu_item5;
    GtkWidget *new_menu;
    static gint run = 1;
    g_print("run %d\n", run);
#ifdef DEBUG 
    g_print("In init in analysis plugin\n");
    g_print("Score is %p\n", si);
#endif/*DEBUG*/
    
    /*Set plugin Name in list and increment counter*/
    pd->title = g_strdup("analyse");
    pd->clean_up = denemo_plugin_clean_up;
    si->plugincounter++;
    
    new_menu = gtk_menu_new();
    
    menu_item1 = gtk_menu_item_new_with_label("Read Results");
    gtk_menu_append(GTK_MENU(new_menu), menu_item1);
    gtk_signal_connect(GTK_OBJECT(menu_item1), "activate",
		       GTK_SIGNAL_FUNC( read_resultsfile),
		       (struct scoreinfo *)si);
    gtk_widget_show(menu_item1);
    
   

    menu_item2 = gtk_menu_item_new_with_label("Highlight Results");
    gtk_menu_append(GTK_MENU(new_menu), menu_item2);
    gtk_widget_set_sensitive(menu_item2, TRUE);

    gtk_signal_connect(GTK_OBJECT(menu_item2), "activate",
		       GTK_SIGNAL_FUNC(highlight),
		       (struct scoreinfo *) si);

    gtk_widget_show(menu_item2);
    
    menu_item3 = gtk_menu_item_new_with_label("Unhighlight Results");
    gtk_menu_append(GTK_MENU(new_menu), menu_item3);
    gtk_signal_connect(GTK_OBJECT(menu_item3), "activate",
		       GTK_SIGNAL_FUNC(unhighlight),
		       (struct scoreinfo *)si);
    gtk_widget_show(menu_item3);
    
    menu_item5 = gtk_menu_item_new_with_label("Add Harmony");
    gtk_menu_append(GTK_MENU(new_menu), menu_item5);
    gtk_signal_connect(GTK_OBJECT(menu_item5), "activate",
		       GTK_SIGNAL_FUNC(add_harmony),
		       (struct scoreinfo *)si);
    gtk_widget_show(menu_item5);
    menu_item4 = gtk_menu_item_new_with_label("Perform Analysis");
    gtk_menu_append(GTK_MENU(new_menu), menu_item4);
    gtk_signal_connect(GTK_OBJECT(menu_item4), "activate",
		       GTK_SIGNAL_FUNC(analysis_gui),
		       (struct scoreinfo *)si);
    gtk_widget_show(menu_item4);
    
    root_menu = gtk_menu_item_new_with_label("Analysis");
    gtk_widget_show(root_menu);
    
    gtk_menu_item_set_submenu(GTK_MENU_ITEM(root_menu), new_menu);
    
    if(run == 1)
      gtk_menu_bar_append(GTK_MENU_BAR(si->menubar), root_menu);
    
    run++;
    return;
  }

  /*cleanup function */
  void denemo_plugin_clean_up(DenemoGUI *si, PluginData *pd)
  {
    g_free(pd->title);
    //Remove analysis submenu 
    gtk_menu_item_remove_submenu(GTK_MENU_ITEM(root_menu));
    //TODO Remove Analysis root menuitem from menubar
    gtk_container_remove(GTK_CONTAINER(si->menubar), root_menu);
  }
#ifdef __cplusplus
}
#endif

void add_harmony(GtkWidget *widget, gpointer callback_data) {

}


void
unhighlight (GtkWidget *widget,gpointer callback_data)
{
  staffnode *curstaff;
  DenemoStaff *curstaffstruct;
  measurenode *curmeasure;
  objnode *curobj;
  DenemoObject *mudelaitem;
  DenemoScore *si = (DenemoScore *)callback_data;

  for (curstaff = si->thescore; curstaff; curstaff = curstaff->next)
    {
      curstaffstruct = (DenemoStaff *)curstaff->data;
      for (curmeasure = (measurenode *) curstaffstruct->measures;
	   curmeasure; curmeasure = curmeasure->next)
	{

	  for (curobj = (objnode *)curmeasure->data; curobj; 
	       curobj = curobj->next)
	    {
	      mudelaitem = (DenemoObject *)curobj->data;
	      ((chord *)mudelaitem->object)->is_highlighted = FALSE;
	    }
	}
    }
}




#if GTK_MAJOR_VERSION > 1
enum {
  STAFF=0,
  SBAR,
  SBEAT,
  EBAR,
  EBEAT,
  SIM
};

static void
add_columns (GtkTreeView *treeview)
{
  gint col_offset;
  GtkCellRenderer *renderer;
  GtkTreeViewColumn *column;
//  GtkTreeModel *model = gtk_tree_view_get_model (treeview);
  
  /* column for staff number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "Staff",
						renderer, "text",
						STAFF,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
  
  /* column for start bar number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "Start Bar",
						renderer, "text",
						SBAR,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
  
  /* column for start beat number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "Start Beat",
						renderer, "text",
						SBEAT,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  /* column for end bar number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "End Bar",
						renderer, "text",
						EBAR,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  /* column for end beat number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "End Beat",
						renderer, "text",
						EBEAT,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);

  /* column for similarity number */
  renderer = gtk_cell_renderer_text_new ();
  g_object_set (G_OBJECT (renderer), "xalign", 0.0, NULL);
  
  col_offset = 
    gtk_tree_view_insert_column_with_attributes(GTK_TREE_VIEW (treeview),
						-1, "Similarity",
						renderer, "text",
						SIM,
						NULL);
  column = gtk_tree_view_get_column (GTK_TREE_VIEW (treeview), col_offset - 1);
  gtk_tree_view_column_set_clickable (GTK_TREE_VIEW_COLUMN (column), TRUE);
  
}

static void highlightnotes(GtkTreeView     *treeview,
    			   GtkTreePath     *arg1,
			   GtkTreeViewColumn *arg2,
     		   	   gpointer user_data)
{
  GtkTreeIter iter;
  GtkTreeModel *list_store;
  GtkTreeViewColumn *column;
  GtkTreePath *path;
  int staff;
  int sbar,ebar;
  float sbeat, ebeat, similarity;
  DenemoGUI *si = (DenemoGUI *)user_data;
	 
  
  list_store = gtk_tree_view_get_model(GTK_TREE_VIEW(treeview));
  g_print("list_store %p\n", list_store);
  gtk_tree_view_get_cursor(GTK_TREE_VIEW(treeview), &path, &column);

  gtk_tree_model_get_iter(list_store, &iter, path);

  gtk_tree_model_get(list_store, &iter, STAFF, &staff,
      		     SBAR, &sbar,
		     SBEAT,&sbeat,
		     EBAR, &ebar,
		     EBEAT, &ebeat,
		     SIM, &similarity, -1);

  g_print("The text is: %d\n", staff);
  gtk_tree_path_free(path);

  if(si)
    note_highlight(si, staff,sbar,sbeat,ebar,ebeat);
  
}



void highlight (GtkWidget *widget ,gpointer callback_data)
{
  GtkWidget *dialog;
  GtkWidget *scrolled;
  GtkWidget *close_button;
  GtkWidget *tree = NULL;

  
  struct scoreinfo *si = (struct scoreinfo *)callback_data;
  
  printf("In Highlight, Score is %p\n", si);
  
  dialog = gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW (dialog), "Test Tree Dialog");
  gtk_widget_set_usize(dialog,400,100);
   
  scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
				 GTK_POLICY_AUTOMATIC,
				 GTK_POLICY_AUTOMATIC);

  //gtk_widget_set_usize(scrolled, 300, 80);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox),scrolled,
		     TRUE,TRUE,0);
  gtk_widget_show(scrolled);

  

  GtkTreeStore *model = gtk_tree_store_new(6, G_TYPE_UINT, G_TYPE_UINT,
					   G_TYPE_FLOAT, G_TYPE_UINT, 
					   G_TYPE_FLOAT, G_TYPE_FLOAT);
  GtkTreeIter iter;
  GList *tmp;
  for(tmp = results->pm; tmp; tmp=tmp->next) {
    Results *res = (Results *)tmp->data;
    gtk_tree_store_append(model, &iter, NULL);
    gtk_tree_store_set(model, &iter,
		       STAFF, res->staff,
		       SBAR, res->start_bar,
		       SBEAT, res->start_beat,
		       EBAR, res->end_bar,
		       EBEAT, res->end_beat,
		       SIM, res->similarity,
		       -1);
  }
					   
  tree = gtk_tree_view_new_with_model(GTK_TREE_MODEL(model));
  gtk_tree_selection_set_mode
    (gtk_tree_view_get_selection(GTK_TREE_VIEW(tree)),
				 GTK_SELECTION_SINGLE);

  add_columns(GTK_TREE_VIEW(tree));
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW(scrolled),
					 tree);
  gtk_signal_connect(GTK_OBJECT(tree), "row-activated",
		     G_CALLBACK(highlightnotes),
		     (struct scoreinfo *)si);

  gtk_widget_show(tree);
  close_button = gtk_button_new_with_label("Close");
  gtk_signal_connect_object(GTK_OBJECT (close_button), "clicked",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));
  gtk_box_pack_start(GTK_BOX (GTK_DIALOG(dialog)->action_area), close_button,
		     TRUE, TRUE, 0);
  gtk_widget_show(close_button);

  gtk_signal_connect_object(GTK_OBJECT (dialog), "destroy",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));

  
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show(dialog);
  
}
#else
void highlight (GtkWidget *widget ,gpointer callback_data)
{
  GtkWidget *dialog;
  GtkWidget *scrolled;
  GtkWidget *close_button;
  GtkWidget *tree = NULL;
  GtkWidget *subtree = NULL;
  GtkWidget *item = NULL;
  GList *tmp;
  
  int i=1;

  DenemoScore *si = (DenemoScore *)callback_data;
  
  printf("In Highlight, Score is %p\n", si);
  
  dialog = gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW (dialog), "Test Tree Dialog");
  
  scrolled = gtk_scrolled_window_new(NULL, NULL);
  gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scrolled),
				 GTK_POLICY_AUTOMATIC,
				 GTK_POLICY_AUTOMATIC);

  gtk_widget_set_usize(scrolled, 300, 80);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox),scrolled,
		     TRUE,TRUE,0);
  gtk_widget_show(scrolled);

  

  tree = gtk_tree_new();
  gtk_scrolled_window_add_with_viewport (GTK_SCROLLED_WINDOW(scrolled),
					 tree);
  gtk_tree_set_selection_mode(GTK_TREE(tree), GTK_SELECTION_MULTIPLE);
  gtk_widget_show(tree);
  
#ifdef DEBUG
  if(results) {
    g_print("results not equal to NULL\n");
    g_print("si %p\n", si);
  }
#endif 
 
  for(tmp = results->pm; tmp; tmp=tmp->next) {
    Results *res = (Results *)tmp->data;
    
#ifdef DEBUG
    g_print("Loop %d\n", i);
#endif
    if(i != res->staff) {
	gchar label[8];
	snprintf(label,8 , "Staff %d", res->staff);
#ifdef DEBUG	
	g_print("%s\n", label);
#endif
	item = gtk_tree_item_new_with_label(label);
	gtk_tree_append(GTK_TREE(tree), item);
	gtk_widget_show(item);
	
	subtree = gtk_tree_new();
	gtk_tree_item_set_subtree(GTK_TREE_ITEM(item), subtree);
	gtk_tree_set_selection_mode(GTK_TREE(subtree), GTK_SELECTION_SINGLE);
	
	gtk_widget_show(subtree);
     }
    if(item && subtree) {
      gchar subitemlabel[50]; 
      snprintf(subitemlabel,50 ,"%d,%d, %.3f, %d, %.3f,  %.6f", 
	       res->staff, res->start_bar, 
	       res->start_beat, res->end_bar, res->end_beat,
	       res->similarity);
      
      GtkWidget *subitem = gtk_tree_item_new_with_label(subitemlabel);
      
      gtk_tree_append(GTK_TREE(subtree), subitem);
      gtk_signal_connect (GTK_OBJECT(subitem), "select",
			  GTK_SIGNAL_FUNC(cb_itemselect), 
			  (gpointer)si);
      gtk_signal_connect (GTK_OBJECT(subitem), "deselect",
			  GTK_SIGNAL_FUNC(cb_itemdeselect), 
			  (gpointer)si);
      
      gtk_widget_show(subitem);
    }
    i = res->staff;
  }

  close_button = gtk_button_new_with_label("Close");
  gtk_signal_connect_object(GTK_OBJECT (close_button), "clicked",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));
  gtk_box_pack_start(GTK_BOX (GTK_DIALOG(dialog)->action_area), close_button,
		     TRUE, TRUE, 0);
  gtk_widget_show(close_button);

  gtk_signal_connect_object(GTK_OBJECT (dialog), "destroy",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));

  
  gtk_window_set_position (GTK_WINDOW (dialog), GTK_WIN_POS_MOUSE);
  gtk_widget_show(dialog);


}

#endif

void
note_highlight (DenemoGUI *si,
		int mstaff, int sbar, float sbeat, 
		int ebar, float ebeat)
{
  staffnode *curstaff = NULL;
  DenemoStaff *curstaffstruct = NULL;
  measurenode *curmeasure = NULL;
  objnode *curobj = NULL;
  DenemoObject *mudelaitem = NULL;
  gint bar;
  gfloat beat;
  
 
  curstaff = (staffnode *) g_list_nth(si->si->thescore, mstaff-1);
  curstaffstruct = (DenemoStaff *) curstaff->data;
  for (curmeasure = (measurenode *) curstaffstruct->measures,
	 bar = 1; curmeasure; curmeasure = curmeasure->next, bar++)
    {
      if (bar >= sbar && bar <= ebar)
	{
	  beat = 1.00;
	  for (curobj = (objnode *)curmeasure->data; curobj;
	       curobj = curobj->next)
	    {
#ifdef DEBUG
	      g_print ("Bar %d, Beat %f\n", bar, beat);
#endif /*DEBUG*/
	      mudelaitem = (DenemoObject *)curobj->data;
	      if (beatcmp(bar, beat, sbar, sbeat) >= 0
		  || beatcmp (bar, beat, ebar, ebeat) >= 0)
		{
		  ((chord *)mudelaitem->object)->is_highlighted = 
		    !((chord *)mudelaitem->object)->is_highlighted;
#ifdef DEBUG
		  g_print ("Set Highlight %d, %f \n", bar, beat);
#endif /*DEBUG*/
		}
	      //beat += durationtofloat
		//(((chord *)mudelaitem->object)->baseduration,
		 //((chord *)mudelaitem->object)->numdots);
	    }
	  
	}
    }
  
  //set_currentmeasurenum(si, sbar);
  gtk_widget_draw ((GtkWidget *)si->scorearea,NULL);
}


/*void
perform_analysis (GtkWidget *entry, struct scoreinfo *si, int algorithm, 
		  int contour, int threshold, int features, 
		  const gchar *sequence)
{
  static GString *filename = NULL;
  static GString *patternname = NULL;
  static GString *passtosystem = NULL;
  static GString *resfilename = NULL;
  FILE *pipe=NULL;
  
#ifdef DEBUG  
  g_print("Score (Perform Analysis) %p\n", si);
#endif DEBUG
  if (!filename)
    {
      filename = g_string_new (locatedotdenemo ());
      g_string_append (filename, "/denemoanalysis.jtf");
      patternname = g_string_new (locatedotdenemo ());
      g_string_append (patternname, "/denemoanalysispattern.jtf");
      passtosystem = g_string_new (NULL);
      resfilename = g_string_new (locatedotdenemo ());
      g_string_append (resfilename, "/denemoanalysisresults");
    }

  g_print ("%s \n %s", filename->str, patternname->str);


  
  filesave (filename->str, si, 0, 0, 0);
  filesaveselection (patternname->str,si);
  g_string_sprintf(passtosystem, "simulation -m -a %d -s %s"
      		   " -p %s -r %s -t %d -n -q %d" ,
		   algorithm, filename->str,patternname->str,
		   resfilename->str, threshold, features);
  
  if((pipe= popen(passtosystem->str, "r")) != NULL)	
    {
      g_print("%s, Opening pipe(%s) for read.\n",
	      strerror(errno),passtosystem->str);
     
    }
  static GdkFont *font=NULL;
  if(!font)
    font= gdk_font_load("-misc-fixed-medium-r-*-*-*-140-*-*-*-*-*-*");

  gchar buf[75];
  while(fgets(buf, sizeof(buf),pipe) != 0) {
#if GTK_MAJOR_VERSION > 1
    GtkTextBuffer *buffer = gtk_text_view_get_buffer(GTK_TEXT_VIEW(entry));
  
    gtk_text_buffer_insert_at_cursor(buffer,buf, -1);
    gtk_text_view_set_buffer(GTK_TEXT_VIEW(entry),buffer);
#endif
  }
  if(!pclose(pipe))
    g_print("%s: pclose()\n",
	    strerror(errno));

}
*/
int
beatcmp (int bar1, float beat1, int bar2, float beat2)
{

  if (bar1 <= bar2 && beat1 <= beat2 || 
      bar1 >= bar2 && beat1 >= beat2)
    return -1;
  else if (bar1 == bar2 && beat1 == beat2)
    return 0;
  else
    return 1;
}

void
read_resultsfile (GtkWidget *widget,gpointer callback_data)
{
  gchar *resfile = NULL;
  GList *temp;
//  Results *res = NULL;
// FILE *fp = NULL;
//  gint i = 0;
  
  GList *tmp;
  chordsPtr chord;
  
  results = ( AnalysisResPtr) malloc(sizeof( AnalysisRes));
 
 // resfile = g_strconcat (locatedotdenemo(),"/denemoanalysisresults", NULL);
 
  results = parseAnalysisResFile(resfile);

  
  for(tmp=results->harmony; tmp; tmp=tmp->next) {
    chord = (chordsPtr)tmp->data;
    g_print("Bar %d Length %f\n", chord->bar, chord->length);
  }
  /*if ((fp = fopen (resfile, "r")) == NULL)
    return;
  else
    {
      while (!feof (fp))
	{
	  res = (Results *)g_malloc (sizeof (Results));	
	  if (res)
	    {
	      fscanf (fp, "%d %d %f %d %f\n", &res->staff,
		      &res->start_bar, &res->start_beat,
		      &res->end_bar, &res->end_beat);
#ifdef DEBUG	     
	      g_print ("Read record %d\n", i);
	      
	      g_print ("%d %d %f %d %f\n", res->staff,
		       res->start_bar, res->start_beat,
		       res->end_bar, res->end_beat);
#endif
	      results = g_list_insert (results, res, i);

	      i++;

	    }
	  
	}
      fclose (fp);
    }
  
#ifdef DEBUG
  for (temp = results; temp; temp = temp->next)
    {
      Results *tempres = (Results *) temp->data;
      g_print ("%d %d %f %d %f\n", tempres->staff,
	       tempres->start_bar, tempres->start_beat,
	       tempres->end_bar, tempres->end_beat);
    }
#endif DEBUG
*/
}

Results *
createnewres (void)
{
  Results *newres = (Results *)g_malloc (sizeof (Results));

  return newres;
}

static GList *comboitems = 0;
static GList *contouritems = 0;
static gchar *algorithms[] = {"DP Algorithm", "XCorrelation Algorithm",
			       "MSE", "DP Pattern Duration",
			       "XCorrelation PD", "Combined DP",
			       "Tonality", "Harmony",
			       "Harmonic Sequence Comparison" 
			     };

static gchar *contours[] = {"pitch contour", "intervallic contour",
			     "durational contour" };

static gchar *queries[] = { "Exact", "Exact Transposed", 
  			    "Exact Aug/Dim", "Transposed Aug/Dim",
			    "Inverted Exact", "Inverted Transposed Exact",
			    "Inverted Aug/Dim", "Inverted Transposed Aug/Dim",
			    "Exact no MP" };
/**
 * Gui for choosing the algorithm to use.
 *
 *
 */
void analysis_gui(GtkMenuItem *menuitem, gpointer data)
{
  GtkWidget *dialog = 0;
  GtkWidget *label=0;
  GtkWidget *algocombo=0;
  GtkWidget *table=0;
  GtkWidget *contourcombo=0;
  GtkWidget *featurescombo=0;
  GtkWidget *threshold=0;
  GtkWidget *runbutton=0;
  GtkWidget *cancelbutton=0;
  GtkWidget *outputentry=0;
  GtkWidget *harmonicentry=0;
  static struct callbackdata cbdata;
  struct scoreinfo *si = (struct scoreinfo *)data;
  
  if(!comboitems) {
	  int i;
    for(i=0; i < 13; i++)
      comboitems = g_list_append(comboitems, algorithms[i]);
  }
  
  if(!contouritems) {
	  int i;
    for(i=0; i < 3; i++)
      contouritems = g_list_append(contouritems, contours[i]);
  }
     
  dialog = gtk_dialog_new();
  gtk_window_set_title(GTK_WINDOW (dialog), "Select Analysis Algorithm");
  
  table = gtk_table_new(4,2,FALSE);
  gtk_box_pack_start (GTK_BOX(GTK_DIALOG(dialog)->vbox), table,TRUE,TRUE,0);
  gtk_widget_show(table);
  
  label = gtk_label_new("Select Algorithm");
  gtk_table_attach_defaults(GTK_TABLE(table),label,0,1,0,1);
  gtk_widget_show(label);

  algocombo = gtk_combo_new();
  gtk_combo_set_popdown_strings(GTK_COMBO(algocombo),comboitems);
  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(algocombo)->entry), algorithms[0]);
  gtk_table_attach_defaults(GTK_TABLE(table),algocombo, 1,2,0,1);
  gtk_widget_show(algocombo);
  
  label = gtk_label_new("If using EDP Single\n Select Contour"); 
  gtk_table_attach_defaults(GTK_TABLE(table),label,0,1,1,2);
  gtk_widget_show(label);

  contourcombo = gtk_combo_new();
  gtk_combo_set_popdown_strings(GTK_COMBO(contourcombo),contouritems);
  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(contourcombo)->entry), contours[0]);
  gtk_table_attach_defaults(GTK_TABLE(table),contourcombo, 1,2,1,2);
  gtk_widget_show(contourcombo);
  
  label = gtk_label_new("Threshold");
  gtk_table_attach_defaults(GTK_TABLE(table),label, 0,1,2,3);
  gtk_widget_show(label);
  
  threshold = gtk_entry_new_with_max_length(10);
  gtk_table_attach_defaults(GTK_TABLE(table),threshold, 1,2,2,3);
  gtk_widget_show(threshold);
  
  label = gtk_label_new("Harmonic Sequence");
  gtk_table_attach_defaults(GTK_TABLE(table), label, 0,1,3,4);
  gtk_widget_show(label);

  harmonicentry = gtk_entry_new();
  gtk_table_attach_defaults(GTK_TABLE(table), harmonicentry, 1,2,3,4);
  gtk_widget_show(harmonicentry);
    

  GtkWidget *hbox = gtk_hbox_new(FALSE,1);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox), hbox, TRUE, TRUE,0);
  gtk_widget_show(hbox);
  
  label = gtk_label_new("Query");
  gtk_box_pack_start(GTK_BOX(hbox), label, TRUE, TRUE, 0);
  gtk_widget_show(label);
  static GList *features=0;
  if(!features) {
	  int i;
    for(i=0; i < 13; i++)
      features = g_list_append(features, queries[i]);
  }
  featurescombo = gtk_combo_new();
  gtk_combo_set_popdown_strings(GTK_COMBO(featurescombo),features);
  gtk_entry_set_text(GTK_ENTRY(GTK_COMBO(featurescombo)->entry), queries[0]);
  gtk_box_pack_start(GTK_BOX(hbox), featurescombo, TRUE,TRUE,0);
  gtk_widget_show(featurescombo);
#if GTK_MAJOR_VERSION > 1
  outputentry = gtk_text_view_new();
  GtkWidget *sw = gtk_scrolled_window_new (NULL, NULL);
  gtk_scrolled_window_set_policy (GTK_SCROLLED_WINDOW (sw),
				  GTK_POLICY_AUTOMATIC,
				  GTK_POLICY_AUTOMATIC);
  gtk_container_add (GTK_CONTAINER (sw), outputentry);
  gtk_widget_show(sw);
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->vbox),sw,
		     TRUE,TRUE,0);
  gtk_widget_show(outputentry);
  cbdata.outputentry = outputentry;
#endif
  cbdata.si = si;
  cbdata.algocombo = algocombo;
  cbdata.contourchoice = contourcombo;
  cbdata.threshold = threshold;
  
  cbdata.harmonicentry = harmonicentry;
  cbdata.query = featurescombo;
  runbutton = gtk_button_new_with_label("Run");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area),
		     runbutton,TRUE,TRUE,0);
  gtk_signal_connect(GTK_OBJECT(runbutton), "clicked",
		     GTK_SIGNAL_FUNC(run_analysis),
		     &cbdata);
  gtk_widget_show(runbutton);

  cancelbutton = gtk_button_new_with_label("Cancel");
  gtk_box_pack_start(GTK_BOX(GTK_DIALOG(dialog)->action_area),
		     cancelbutton,TRUE,TRUE,0);
  gtk_signal_connect_object(GTK_OBJECT (cancelbutton), "clicked",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));
  gtk_widget_show(cancelbutton);
  gtk_signal_connect_object(GTK_OBJECT (dialog), "destroy",
			    GTK_SIGNAL_FUNC(gtk_widget_destroy), 
			    GTK_OBJECT(dialog));
  gtk_widget_show(dialog);
}


void run_analysis(GtkWidget *widget, gpointer data)
{
  int algorithm, contour;
  struct callbackdata *cbdata = (struct callbackdata *)data;
 // struct scoreinfo *si = (struct scoreinfo *)cbdata->si;
  //int thresh = atoi(gtk_entry_get_text(GTK_ENTRY(cbdata->threshold)));
  algorithm = get_algorithm(cbdata->algocombo);
  contour = get_contour(cbdata->contourchoice);
 // const gchar *sequence = gtk_entry_get_text(GTK_ENTRY(cbdata->harmonicentry));
  //int features = getfeatures(cbdata->query);
  //perform_analysis(cbdata->outputentry,si, algorithm, contour, thresh, 
  //    features,sequence);

}

int getfeatures(GtkWidget *combo)
{
  int f=0; 
  const gchar *tmp = gtk_entry_get_text(GTK_ENTRY(GTK_COMBO(combo)->entry));
  if(!strcmp(tmp,"Exact")) 
    f= 1;	  
  else if(!strcmp(tmp,"Exact Transposed")) 
    f = 2;
  else if(!strcmp(tmp,"Exact Aug/Dim")) {
    f = 3;
  }
  else if(!strcmp(tmp,"Trans Aug/Dim")) {
    f = 4;
  }
  else if(!strcmp(tmp,"Inverted Exact")) {
    f = 5;
  }
  else if(!strcmp(tmp,"Inverted Transposed Exact")) {
    f = 6;
  }
  else if(!strcmp(tmp,"Inverted Aug/Dim")) {
    f = 7;
  }else if(!strcmp(tmp,"Inverted Trans Aug/Dim")) {
    f= 8;
  }else if(!strcmp(tmp,"Exact no MP")) {
    f = 17;
  }
  return f;
}

int get_algorithm(GtkWidget *combobox)
{
  const gchar *text = 
    gtk_entry_get_text(GTK_ENTRY (GTK_COMBO (combobox)->entry));
  if(!strcmp("DP Algorithm" ,text))
    return 1;
  else if(!strcmp("XCorrelation Algorithm", text))
    return 2;
  else if(!strcmp("MSE", text))
    return 3;
  else if(!strcmp("DP Pattern Duration",text))
    return 4;
  else if(!strcmp("XCorrelation PD", text))
    return 5;
  else if(!strcmp("Combined DP",text))
    return 6;
  else if(!strcmp("Tonality", text))
    return 0;
  else if(!strcmp("Harmony",text))
    return 9;
  else if(!strcmp("Harmonic Sequence Comparison",text))
    return 13;

  return 1;
}

int get_contour(GtkWidget *combobox)
{
	int i;
  const gchar *text = 
    gtk_entry_get_text(GTK_ENTRY (GTK_COMBO (combobox)->entry));
  for(i=0; i < 3; i++) {
    if(!strcmp(contours[i],text))
      return i;
  }

  return 1;

}
