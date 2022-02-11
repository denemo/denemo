#ifndef PRINT_H
#define PRINT_H

#include <denemo/denemo.h>

#define PRINTMARKER (22)
#define GREATER 2
#define SAME 1
#define LESSER 0
#define GPID_NONE (-1)
#define INSTALLED_LILYPOND_VERSION "2.18.0"       /* FIXME set via gub */
#define MANUAL _("Manual Updates")
#define CONTINUOUS _("Continuous")
#ifdef G_OS_WIN32
#define  return_on_windows_if_printing \
  if (Denemo.printstatus->printpid != GPID_NONE)\
    {\
    warningdialog (_("Already doing a print"));\
    return;\
    }
#define  return1_on_windows_if_printing \
  if (Denemo.printstatus->printpid != GPID_NONE)\
    {\
    warningdialog (_("Already doing a print"));\
    return 1;\
    }
#else
#define  return_on_windows_if_printing
#define  return1_on_windows_if_printing
#endif
void initialize_lilypond_includes(void);

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
  TypesetForPlaybackView
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

extern gint LilyPond_stderr;       //A file descriptor to pipe for LilyPond's stderr
extern GError *lily_err;
extern GPid previewerpid;

WysiwygInfo* get_wysiwyg_info();
void initialize_print_status (void);
void printall_cb (DenemoAction * action, DenemoScriptParam * param);
void printmovement_cb (DenemoAction * action, DenemoScriptParam * param);
void printpart_cb (DenemoAction * action, DenemoScriptParam * param);
void printselection_cb (DenemoAction * action, DenemoScriptParam * param);

void printexcerptpreview_cb (DenemoAction * action, DenemoScriptParam * param);
gchar *get_lily_version_string (void);
gchar *get_lilypond_include_dir (void);
int check_lily_version (gchar * version);
void export_pdf (gchar * filename, DenemoProject * gui);
void generate_pdf_from_lily_file (gchar *lilyfile, gchar *outfile);
void export_png (gchar * filename, GChildWatchFunc finish, DenemoProject * gui);
void printpng_finished (GPid pid, gint status, GList * filelist);
gboolean create_thumbnail (gboolean async, gchar* thumbnail_path);
gchar *large_thumbnail_name (gchar * filepath);
gboolean stop_lilypond ();
void process_lilypond_errors (gchar * filename);
gchar *get_printfile_pathbasename (void);
void create_pdf (gboolean part_only, gboolean all_movements);
void show_print_view (DenemoAction * action, DenemoScriptParam * param);
void create_svg (gboolean part_only, gboolean all_movements);
void create_pdf_for_lilypond (gchar *lilypond);
#endif /*PRINT_H */
