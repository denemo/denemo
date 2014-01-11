#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h>

#define PRINTMARKER (22)
#define GREATER 2
#define SAME 1
#define LESSER 0
#define GPID_NONE (-1)
#define INSTALLED_LILYPOND_VERSION "2.18"       /* FIXME set via gub */
#define MANUAL _("Manual Updates")
#define CONTINUOUS _("Continuous")

typedef enum
{
  STATE_NONE = 0,               //not a background typeset
  STATE_OFF = 1 << 0,           //background typeset complete
  STATE_ON = 1 << 1,            //background typeset in progress
  STATE_PAUSED = 1 << 2         //background typesetting turned off to allow printing
} background_state;

typedef struct printstatus
{
  GPid printpid;
  background_state background;
  gint updating_id;             //id of idle callback
  gint first_measure;
  gint last_measure;
  gint first_staff;
  gint last_staff;
  typeset_type typeset_type;
  gint invalid;                 //set 1 if  lilypond reported problems or 2 if generating new pdf failed 
  gint cycle;                   //alternate 0 1 to switch print file names
  gchar *printbasename[2];
  gchar *printname_pdf[2];
  gchar *printname_ly[2];
} printstatus;

typedef struct WwRectangle
{
  gdouble x, y, width, height;
} WwRectangle;                    //Width=0 means no rectangle set

typedef struct WwPoint
{
  gint x, y;
} WwPoint;                    //Width=0 means no rectangle set

typedef struct Curve
{
  WwPoint p1, p2, p3, p4;
} Curve;

typedef enum
{
  STAGE_NONE,
  Offsetting,
  Selecting,
  TargetEstablished,            //the get_wysiwyg_info()->grob has been set
  SelectingNearEnd,
  SelectingFarEnd,
  DraggingNearEnd,
  DraggingFarEnd,
  WaitingForDrag,
  SelectingReference,
  WaitingForCurveDrag,
  SelectingPoint,
  Dragging1,
  Dragging2,
  Dragging3,
  Dragging4,
} WwStage;

typedef enum
{
  TASK_NONE,
  Positions,
  Padding,
  Offset,
  Shape
} WwTask;

typedef enum
{
  OBJ_NONE,
  Beam,
  Slur,
  Tie,
  Articulation,
  BassFigure,
} WwGrob;

typedef struct WysiwygInfo
{
  WwRectangle Mark;
  WwRectangle Reference;          //reference is origin for LilyPond offsets, set by the user with blue cross wires.
  Curve Curve;
  gdouble curx, cury;           // position of mouse pointer during motion
  //gdouble pointx, pointy; becomes near.x,y
  gboolean ObjectLocated;       //TRUE when an external-link has just been followed back to a Denemo object
  gint button;                  //which mouse button was last pressed
  WwPoint nearpoint;                //left hand end of slur, beam etc
  WwPoint farpoint;                 //right hand end of slur, beam etc
  WwPoint near_i;              //initial left hand end of slur, beam etc
  WwPoint far_i;               //initial right hand end of slur, beam etc
  WwPoint last_button_press;
  WwPoint last_button_release;
  WwStage stage;
  WwGrob grob;
  WwTask task;
  DenemoPosition pos;
  gboolean repeatable;          //if pos is still the same, and the same edit parameters, just continue editing.
  GtkWidget *dialog;            //an info dialog to tell the user what to do next...
} WysiwygInfo;

gint LilyPond_stderr;       //A file descriptor to pipe for LilyPond's stderr
GError *lily_err;
gint changecount;   //changecount when the printfile was last created FIXME multiple tabs are muddled
GPid previewerpid;

WysiwygInfo* get_wysiwyg_info();
printstatus* get_print_status();
void printall_cb (GtkAction * action, DenemoScriptParam * param);
void printmovement_cb (GtkAction * action, DenemoScriptParam * param);
void printpart_cb (GtkAction * action, DenemoScriptParam * param);
void printselection_cb (GtkAction * action, DenemoScriptParam * param);

void printexcerptpreview_cb (GtkAction * action, DenemoScriptParam * param);
gchar *get_lily_version_string (void);
void export_pdf (gchar * filename, DenemoProject * gui);
void export_png (gchar * filename, GChildWatchFunc finish, DenemoProject * gui);
void printpng_finished (GPid pid, gint status, GList * filelist);
gboolean create_thumbnail (gboolean async);
gchar *large_thumbnail_name (gchar * filepath);
gboolean stop_lilypond ();
void process_lilypond_errors (gchar * filename);
gchar *get_printfile_pathbasename (void);
void create_pdf (gboolean part_only, gboolean all_movements);
void show_print_view (GtkAction * action, DenemoScriptParam * param);

#endif /*PRINT_H */
