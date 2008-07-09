/* dynamic.h
 * function prototypes for inserting 
 * dynamics
 *
 * for Denemo, a gtk+frontend to GNU Lilypond 
 * (c) 2001 Adam Tee
 */


void
insert_dynamic (GtkAction *action);

void
insertdynamic (GtkWidget *widget, gpointer data);

void add_dynamic(DenemoObject *mudelaobj, GString *dynamic);
  
