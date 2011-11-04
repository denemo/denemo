typedef struct modedata
{
  GtkWidget *dialog;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
  GtkWidget *modenamecombo;
  GList *majorlist;
  GList *minorlist;
  GList *modelist;
}modedata;

typedef struct keysig_callbackdata
{
  DenemoGUI *gui;
  DenemoStaff *curstaffstruct;
  GtkWidget *checkbutton;
  GtkWidget *majorkeycombo;
  GtkWidget *minorkeycombo;
  GtkWidget *radiobutton2;
  GtkWidget *radiobutton3;
  GtkWidget *mode;
  modedata *mdata;
}keysig_callbackdata;


void majorcallback (GtkWidget * widget, struct modedata *data);
void minorcallback (GtkWidget * widget, struct modedata *data);
void modedialog (GtkWidget * widget, struct modedata *mdata);
void set_keysig (GtkWidget * widget, gpointer data);
gint findkey (GtkWidget * combobox, modedata *mdata, gint type);
gint findmode (GtkWidget * modebox, modedata *mdata);

