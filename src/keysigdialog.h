typedef struct keysig_data
{
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
  GtkWidget *modenamecombo;
  GtkWidget *radiobutton1;
  GtkWidget *radiobutton2;
  GtkWidget *radiobutton3;
}keysig_data;

void set_keysig (GtkWidget * widget, struct keysig_data *data);
GtkWidget *keysig_widget_new(keysig_data *keysig_widgets);
