typedef struct keysig_data
{
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
  GList     *majorlist;
  GList     *minorlist;
  GtkWidget *radiobutton1;
  GtkWidget *radiobutton2;
}keysig_data;

void set_keysig (struct keysig_data *data);
GtkWidget *keysig_widget_new(keysig_data *keysig_widgets);
