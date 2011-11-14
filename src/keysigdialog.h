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

void majorcallback (GtkWidget * widget, struct modedata *data);
void minorcallback (GtkWidget * widget, struct modedata *data);
void modedialog (GtkWidget * widget, struct modedata *mdata);
void set_keysig (GtkWidget * widget, struct keysig_data *data);

