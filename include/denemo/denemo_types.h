/*
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Library General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor Boston, MA 02110-1301,  USA
 */

#ifndef DENEMOTYPES_H
#define DENEMOTYPES_H

#include "denemo_objects.h"
#define EXT_MIDI 0
#define EXT_CSOUND 1

#define DENEMO_DEFAULT_ANON_FTP "ftp://www.denemo.org/download/"

#define DENEMO_TEXTEDITOR_TAG "texteditor"

#define DENEMO_CLICK_TRACK_NAME _("Click")

/* values for is_grace attribute of CHORD type */
#define GRACED_NOTE (1<<1) /*Note is an (appogiatura) grace note */
#define ENDGRACE (1<<2) /*Note is last grace note in a group */
#define ACCIACCATURA (1<<3) /*Note is an acciacatura grace note */
#define DURATION_SET (1<<4) /*Note has been assigned a duration (stored in durinticks) */


typedef void (*GActionCallback) (GtkAction *action, gpointer data);
#define G_ACTIONCALLBACK(f) ((GActionCallback)(f))
 /* and the following typedefs are basically here for so that it's
 * possible to understand what my code is doing -- just as much for
 * my sake as yours!
 *
 * What can I say; I've done a lot of programming in Java and
 * SML/NJ;
 * I like my type names to convey information. */

/* The ->data part of each objnode presently points to a DenemoObject */

typedef GList objnode;


typedef enum
  {
    NOT_UNDO_REDO = 0,
    UNDO,
    REDO
  }unre_mode;

typedef enum
  {
    SAVE_NORMAL=0,
    SAVE_TEMPLATE,
    SAVE_COPY
  } DenemoSaveType;



typedef enum DenemoGraphicType
  {
    DENEMO_BITMAP,
    DENEMO_PATTERN,
    DENEMO_FONT
  } DenemoGraphicType;
typedef enum DenemoActionType
  {
    DENEMO_SCHEME = 0,
    DENEMO_MENU_ITEM
  } DenemoActionType;
    
typedef struct DenemoAction {
    gchar *name;
    gchar *label;
    gchar *tooltip;
    DenemoActionType type;
    gpointer callback;
} DenemoAction; 

typedef struct DenemoGlyph
{
  gchar *fontname;/**< font to be used */
  gchar *utf; /**< utf8 char(s) to be placed as graphic */
  gdouble size; /**< font size to be used */
  gint slant, weight; /**< CAIRO_FONT_SLANT_xxx and WEIGHT_xxx values */
}
DenemoGlyph;

typedef struct DenemoGraphic
{
  DenemoGraphicType type;
  gpointer graphic; /**< either a GdkBitmap, a cairo_pattern_t or a DenemoGlyph*/
  gint width, height;
}
DenemoGraphic;


typedef struct DenemoSelection
{
    gint firststaffmarked;
    gint laststaffmarked;
    gint firstmeasuremarked;
    gint lastmeasuremarked;
    gint firstobjmarked;
    gint lastobjmarked;
}
DenemoSelection;

typedef struct DenemoPalette
{
    gchar *name;
    GtkWidget *box;
    GtkWidget *window;
    gboolean menu;/* if this widget is derived from a menu */
    gboolean rows; /* widgets should be packed row-wise or not */
    gint limit; /* how many widgets to pack in the chosen direction */
    gboolean docked; /**< whether this palette is displayed in the main display */
    GList *buttons; /**< the data are GtkWidget which have a script attached to be run when activated */
}
DenemoPalette;



/* The idea here is to make everything recursive.  The dominant
   paradigm is a linked list.  Handy that there's such a nice
   precooked implementation of them in glib, eh?  Note, however, that
   the whole score isn't treated as a linked list of notes and other
   objects as it is in, say Rosegarden; instead, the program has a
   linked list of musical objects for each measure, and only then are
   measures linked into staffs.  That's my main beef with Rosegarden
   -- I don't tend to compose my stuff in the order in which it will
   eventually be played. As such, I like being able to start entering
   music in the middle of my score, then the beginning, then the end,
   or whatever, as appropriate.  */

typedef enum DenemoObjType
{
  CHORD=0,
  TUPOPEN,
  TUPCLOSE,
  CLEF,
  TIMESIG,
  KEYSIG,
  BARLINE,
  STEMDIRECTIVE,
  MEASUREBREAK,
  STAFFBREAK,
  DYNAMIC,
  GRACE_START,
  GRACE_END,
  LYRIC,
  FIGURE,
  LILYDIRECTIVE,
  FAKECHORD,
  PARTIAL
  /* WARNING when adding to this list, add also to the type names
     in denemo_types.c */
}DenemoObjType;

extern gchar *DenemoObjTypeNames[18];

#define DENEMO_OBJECT_TYPE_NAME(obj) ((obj)?(((obj)->type<G_N_ELEMENTS(DenemoObjTypeNames))?DenemoObjTypeNames[(obj)->type]:NULL):NULL)


/**
 * Enumeration for Tuplets type
 *
 */
typedef enum tuplet_type{
    DUPLET,
    TRIPLET,
    QUADTUPLET,
    QUINTUPLET,
    SEXTUPLET,
    SEPTUPLET
}tuplet_type;

/**
 * Enumeration for Denemo's input mode
 */
typedef enum input_mode {
#define MODE_MASK (~(INPUTEDIT))
        /*INPUTCLASSIC = 1<<0,  classic mode */
  INPUTEDIT = 1<<1,/*edit mode, the only mode, as we are now non-modal  */
        /* INPUTINSERT = 1<<2, insert mode*/
#define ENTRY_TYPE_MASK (~(INPUTNORMAL|INPUTREST|INPUTBLANK))
  INPUTNORMAL = 1<<3, /* entry type notes */
  INPUTREST = 1<<4, /* entry type rests */
  INPUTBLANK = 1<<5,/* entry type non-printing rests */
  TRAVERSE = 1<<6, /* read-only */
#define ENTRY_FEEDBACK_MASK (~(INPUTRHYTHM))
  INPUTRHYTHM = 1<<7, /*Input rhythms gives feedback*/
}input_mode;

/**
 * Denemo Action type currently used for undo/redo
 *
 */
typedef enum  action_type {
  ACTION_INSERT,//0
  ACTION_DELETE,//1
  ACTION_CHANGE,//2
  ACTION_SNAPSHOT,//3
  ACTION_STAGE_START,//4
  ACTION_STAGE_END,//5
  ACTION_SCRIPT_ERROR,//6
  ACTION_MEASURE_REMOVE,//7
  ACTION_MEASURE_CREATE,//8
  ACTION_MEASURE_INSERT,//9
  ACTION_MEASURE_DELETE,//10
  ACTION_NOOP = -1//
}action_type;

/**
 * Contains all the top-level information of an musical object
 * the object pointer contains the actual object
 */
typedef struct
{
  DenemoObjType type; /**< The type of object pointed to by the gpointer object field below */
  gchar *lilypond;/**< Holds lilypond generated to represent this object, empty until typesetting is called and out of date if the object has been modified since typesetting */
  gint basic_durinticks; /**< Duration of object including dotting but not tuplet/grace note effects. */
  gint durinticks; /**< Duration of object where 384 (PPQN) is a quarter note, includes any tuplet/grace note effects */
  gint starttick; /**< When the object occurs */
  gint starttickofnextnote; /**< When the next object occurs */
  gdouble earliest_time;/**< time in seconds from start of movement for the start of the note/rest   */
  gdouble latest_time;/**< time in seconds from start of movement for the end of the note/rest */
  /**< Allots extra space for accidentals or reverse-aligned notes if
   * the stem is down */
  gint space_before; /**< Used to specify how much space is needed before the object in display */
  gint minpixelsalloted;  /**< horizontal space allowed for this object in display */
  gint x; /**< Holds x co-ordinate relative to the beginning of the measure. used in mousing.c */
  gboolean isstart_beamgroup; /**< TRUE if object is the start of a beam group */
  gboolean isend_beamgroup; /**< TRUE if this is the end of a beam group */
  gpointer object; /**< the structures pointed to are given in denemo_objects.h */
  gboolean isinvisible; /**< If  TRUE it will be drawn in a distinctive color and will be printed transparent. */
  GList *directives; /* Directives attached to the object. These are transient */
   /* cached values of clef, keysignature and stemdir prevailing at the object */
    clef *clef;
    keysig *keysig;
    stemdirective *stemdir;
} DenemoObject;


/**
 * Control of LilyPond context
 * Allows for e.g.  Piano context within staff group context by using bit fields
 */
typedef enum
{
  DENEMO_NONE = 0,
  DENEMO_PIANO_START =  1<<0,
  DENEMO_PIANO_END = 1<<1,
  DENEMO_GROUP_START = 1<<2,
  DENEMO_GROUP_END = 1<<3,
  DENEMO_CHOIR_START = 1<<4,
  DENEMO_CHOIR_END = 1<<5
} DenemoContext;

/**
 * Control of Voices
 *
 */
typedef enum
{
  DENEMO_PRIMARY = 1<<0, /**< If set this voice should have its own staff for display*/
  DENEMO_SECONDARY = 1<<1 /**< If set this is a secondary voice, but may still have PRIMARY set for display */
} DenemoVoice;

/**
 * The ->data part of each measurenode points to an objlist, which is
 * a list of the musical objects in that measure.
 */
typedef GList measurenode;


/**
 * DenemoStaff contains all the information relating to a musical staff
 *
 */
typedef struct
{
  gchar *type; /**< type of staff, e.g. TabStaff or RhythmicStaff. Null for normal staffs */
  GtkMenu *staffmenu; /**< a menu to popup up with the staff directives attached */
  GtkMenu *voicemenu; /**< a menu to popup up with the voice directives attached */
  GList *sources;/**< List of source pixbufs, one for each measure staff-view */
  measurenode *themeasures; /**< This is a GList of DenemoMeasure objects */
  clef clef; /**< The initial clef see denemo_objects.h clefs */
  keysig keysig;
  timesig timesig;
  keysig *leftmost_keysig;
  timesig *leftmost_timesig;

  /* we make leftmost_clefcontext a reference to a clef (a pointer) & re-validate leftmost clefcontext in the delete of CLEF object. */
  clef* leftmost_clefcontext; /**< The clef for the leftmost measure visible in the window*/

  gint leftmost_stem_directive; /**< Stem directive at start of leftmost visible measure */
  DenemoContext context;   /**< The Lilypond context in which this staff appears */
  /*
   * Staff Parameters
   * Added Adam Tee 27/1/2000, 2001
   */
  gint no_of_lines; /**< Number of lines on the staff */
  gint transposition; /**< Determines if the notes are to be played back at pitch or not */
  gint color; /**< color to display the staff */

  gint volume;  /**< Master Volume used to scale midi playback */
  gboolean override_volume; /**< when true staff plays full Volume always */
  gboolean mute;/**< when true mutes regardless of override_volume */
  /* Back to Hiller stuff */
  //  GString *staff_name;

  GString *denemo_name; /**< partname gets transformed into legitimate lily_name */
  GString *subpart; /**< distinguishes staffs belonging to one part */
  GString *lily_name; /**< this is the name of the staff that is exported to lilypond */
  GString *midi_instrument; /**< midi instrument name used for the staff when exported via midi */
  GString *device_port; /**< Device name and Port name concatenated into one string */
  gint midi_prognum; /**< midi prognum assigned to the staff voice */
  gint midi_channel; /**< midi channel assigned to the staff voice */
  gint midi_port; /**< midi port assigned to the staff voice */
  gint space_above; /**< space above the staff used in the denemo gui */
  gint space_shorten; /**< space by the staff is shorter in height because of few staff lines */
  gint space_below; /**< space below the staff used in the denemo gui */
  gboolean range;/**<TRUE if range_hi,lo should be observed. */
  gint range_hi;/**< highest note playable by instrument, mid_c_offset */
  gint range_lo;/**< lowest note playable by instrument, mid_c_offset */
  GList *verse_views;/**< a list of text editor widgets each containing a verse */
  GList *current_verse_view;/**< verse to be displayed */
  GList *verses;/**< gchar of the verses, synchronized with GtkTextView buffers */
  gboolean hide_lyrics; /**< true if lyrics should not be typeset */
  gboolean hasfigures; /**<TRUE if the staff has had figures attached. Only one staff should have this set */
  gboolean hasfakechords; /**<TRUE if the staff has had chord symbols attached. Only one staff should have this set */
  DenemoVoice voicecontrol; /**< either controls whether this staff is displayed and printed separately or as a voice */
  gboolean hidden; /**< should this staff appear in the display? */
  measurenode ** is_parasite; /**< points to address of host staff's measures
                 field if measures are owned by another
                 staff */

  gint nummeasures; /**< Number of measures in the staff*/
  GList *tone_store; /**< list of pitches and durations used a source for
            the notes in this staff
            the data are tone* */

  GList *staff_directives;/**< List of DenemoDirective for the staff context, (only relevant for primary staff)*/
  GList *voice_directives;/**< List of DenemoDirective for the voice context */



} DenemoStaff;

typedef struct DenemoMeasure {
    GList *objects;/* list of DenemoObject */
    /* cached values of clef, timesignature, keysignature and stemdir prevailing at start of measure */
    clef *clef;
    keysig *keysig;
    timesig *timesig;
    stemdirective *stemdir;
    gint measure_number; //measure number to display
    gint measure_numbering_offset;//measures from this one on should display numbers offset by this value from actual measure count.
    gdouble earliest_time;//start time of measure, set by exportmidi if measure is empty
    //gdouble latest_time;//end time of measure, set by exportmidi
}  DenemoMeasure;

/* The ->data part of each staffnode points to a staff structure */

typedef GList staffnode;/**< The ->data part of each staffnode points to a DenemoStaff structure */
//typedef staffnode *score;

/* a pair of staffs, used to relate two staffs together */
typedef struct staff_info
{
  DenemoStaff *main_staff; /**< eg the bass line or the vocal part */
  DenemoStaff *related_staff; /**< eg the figures for the bass or the lyrics*/
}
staff_info;

typedef enum
{
    KeymapEntry,
    KeymapToggleEntry,
    KeymapRadioEntry
}KeymapCommandType;

typedef struct DenemoKeymap
{
  GHashTable *commands; // hashtable for the commands

  //reference for easy access
  GHashTable *idx_from_name; //hashtable linking the name of a command to
                             //its index in the ListStore (values are guint *)

  GHashTable *idx_from_keystring; //hashtable linking the string representing
                                  //a keypress to the index of its command
                                  //The keystring is the output of
                  //dnm_accelerator_name()
  GHashTable *cursors;//hashtable linking GdkEvent state to a cursor that should be used in that state

  GHashTable *continuations_table;//hashtable giving possible continuations for two-keybindings

}keymap;

/** @struct command_row
 *  @brief This structure represents commands in the keymap command list.
 */
typedef struct command_row
{
  KeymapCommandType type;
  gchar *name;
  gchar *label;
  gchar *tooltip;
  gpointer callback;
  GList *bindings;
  gboolean hidden;
  gboolean deleted;
  gint script_type;
  GList* locations;
  gchar* after;
  gchar* fallback;
  gchar* menupath;
  gchar* scheme;
} command_row;

typedef enum{
  COMMAND_BUILTIN,
  COMMAND_SCHEME
} SCRIPT_TYPE;

//index of columns in the keymap command list store FIXME if you add columns you must add them in allocate_keymap !!!!
enum
{
  COL_TYPE = 0,
  COL_NAME,
  COL_LABEL,
  COL_TOOLTIP,
  COL_CALLBACK,
  COL_BINDINGS,
  COL_HIDDEN,
  COL_DELETED,
  COL_SCRIPTTYPE,
  COL_LOCATIONS,
  COL_ROW,
  COL_SCHEME,
  N_COLUMNS
};

typedef enum { TYPESET_EXCERPT, TYPESET_MOVEMENT, TYPESET_ALL_MOVEMENTS} typeset_type;

/**
 * DenemoPrefs holds information on user preferences.
 */
typedef struct DenemoPrefs
{
  // FIXME: the GStrings in here are never freed

  GString *profile; /**< Which set of commands and shortcuts to load, and which initialization of scheme to run */
  GString *lilypath; /**< This is the executable or full path to the lilypond executable */
  GString *graphicseditor; /**< editing eps graphics files */
  GString *fontname; /**< Font name - usually Denemo as this has the required characters */
  gint fontsize; /**< Font size in points used for menus, help text etc */
  gboolean manualtypeset;/**< typeset on request, not continuously */
  gint typesetrefresh;/**< ms between re-trying typeset */
  gint typesettype;/**< what to re-typeset when not manual typeset */
  gint firstmeasure;/**< first measure to typeset relative to the cursor when not manual typset */
  gint firststaff;/**< first staff to typeset relative to the cursor when not manual typset */
  gint lastmeasure;/**< last measure to typeset relative to the cursor when not manual typset */
  gint laststaff;/**< last staff to typeset relative to the cursor when not manual typset */
  gboolean immediateplayback; /**< This options sends audio directly to synth as notes are being entered */
  gint measureswitchsound; /**< MIDI key to play on cursor move to new measure */
  gint pitchspellingchannel; /**< channel to use for feedback when entering extreme intervals via MIDI in, that may indicate an enharmonic error, 0 means no pitch spelling*/
  gint pitchspellingprogram; /**< program to set pitchspellingchannel to on startup */
  gboolean startmidiin; /**< try to start midi in on startup */
  gboolean notesonlymidiin; /**< whether to ignore MIDI signals other than note on/off from MIDI in source. */
  gboolean applytoselection; /**< apply commands to selection */

  gboolean quickshortcuts;/**< TRUE if pressing a key while hovering over a menu item sets a shortcut */
  gboolean overlays; /*< whether overlays or insert should be used with pitch entry */
  gboolean continuous; /*< whether pitch entry overlays should cross barlines */
  gboolean spillover;  /*< whether appending a note that does not fit the measure should cause a spillover to the next measure */
  gboolean ignore_ties; /*< when entering pitches tied notes are filled in unless this is true */
  gboolean createclones;
  gint mode; /**< mode to start in */
  gboolean persistence;/**< whether Denemo should re-use settings over sessions */
  gint zoom; /**< percent zoom of score area display  1-100 */
  gint system_height; /**< percent of display height per system  1-100 */
  gboolean cursor_highlight; /** Should the cursor be highlighted */
  gboolean return_key_is_special; /** Should the Return key be treated as movable shortcut */
  gboolean newbie; /** Give maximum help */
  gboolean learning; /** Show which keypresses have been used */
  gboolean progressbardecorations; /** TRUE if you want window decorations on progressbar */
  gboolean toolbar; /**< makes the toolbar visible */
  gboolean playback_controls; /**< makes the playback controls visible */
  gboolean midi_in_controls; /**< makes the midi in controls visible */
  gboolean rhythm_palette; /**< makes the rhythm toolbar visable */
  gboolean object_palette;  /**< makes the object menu toolbar visible */
  gboolean console_pane;  /**< makes the console pane visible */
  gboolean lyrics_pane;  /**< makes the lyrics pane visible */

  gboolean visible_directive_buttons; /**< This option makes the hbox containing score/movement directives visible */
  gboolean hide_windows; /**< whether to hide windows when a modal dialog is active */
  gboolean enable_thumbnails;
  gboolean opensources; /**< whether to search and open source files in the first measure of newly opened scores */
  gboolean ignorescripts; /**< whether to execute Scheme embedded in files and initializations on file load*/
  gboolean disable_undo; /**< Do not collect undo information */
  gboolean saveparts; /**< Automatically save parts*/
  gboolean autosave; /**< whether to Auto save data */
  gint autosave_timeout;
  gint recording_timeout;
  gboolean autoupdate;/**< update command set from denemo.org */
  gint maxhistory;/**< how long a history of used files to retain */
  gint compression;/**< compression to be applied to .denemo files, suffix is unchanged */
  GString *browser; /**< Default browser string */


  // audio and MIDI driver
  GString *audio_driver;  /* the name of the audio driver to be used */
  GString *midi_driver;   /* the name of the MIDI driver to be used */

  // JACK options
  GString *jack_connect_ports_l;
  GString *jack_connect_ports_r;
  GString *jack_connect_midi_in_port;
  GString *jack_connect_midi_out_port;
  gboolean jacktransport; /**< toggle on and off jack transport */
  gboolean jacktransport_start_stopped; /**< toggle if you don't want transport to play immediately but rely on the transport controls */

  // PortAudio options
  GString *portaudio_device;
  unsigned int portaudio_sample_rate;/**< sample rate in Hz > */
  unsigned int portaudio_period_size;/**< The size of the audio buffers (in frames).> */
  unsigned int maxrecordingtime;/**< The maximum time a recording can be in seconds.> */

  // PortMidi options
  GString *portmidi_input_device;
  GString *portmidi_output_device;

  // fluidsynth options
  GString *fluidsynth_soundfont; /**< Default soundfont for fluidsynth */
  gboolean fluidsynth_reverb; /**< Toggle if reverb is applied to fluidsynth */
  gboolean fluidsynth_chorus; /**< Toggle if chorus is applied to fluidsynth */

  gint dynamic_compression;/**< percent compression of dynamic range desired when listening to MIDI-in */
  gboolean damping;/**< when true notes are re-sounded when left off at a lower velocity depending on their duration */
  gdouble display_refresh;/**< time in ms between refresh of display during playback */
  gint max_menu_size;/** < maximum number of menu entries allowed */
  gint animation_steps;/** < number of steps to use animating the page turns during playback */
  gint tooltip_timeout;/** < timeout before a tooltip appears */
  gint tooltip_browse_timeout;/** < timeout before a tooltip appears in tooltip browse mode */
  gint tooltip_browse_mode_timeout;/** < timeout before a tooltip browse mode is dropped*/

  GString *imageviewer; /**< Image Viewer */
  GString *username; /**< Username for use on denemo.org website */
  GString *password; /**< password  for use on denemo.org website (blank means prompt for username) */
  GString *texteditor; /**< texteditor for editing scripts and lilypond files */
  GString *denemopath; /**< path were denemo files are saved */
  GQueue *history; /**< Queue to contain recently opened files */

  GString *temperament; /**< Preferred temperament for tuning to */
  gboolean strictshortcuts; /**< Classic shortcut scheme, now deprecated */
  gboolean menunavigation; /**< arrows and Escape work for menu navigation*/
  gboolean verboseshortcuts; /**< whether shortcuts are known by their gdk name e.g. period for . */

  gint resolution; /**< Resolution of exported selection in dpi */
}DenemoPrefs;

/* DenemoDirectives are attached to chords and to the individual notes of a chord. They attach LilyPond and MIDI directivees that add to the note information & describe how to display themselves in the Denemo display */
typedef struct DenemoDirective
{
  GString *tag; /**< tag identifying the owner of this directive, usually the denemo command that created it */
  GString *prefix; /**< LilyPond text to be inserted before the chord */
  GString *postfix;/**< LilyPond text to be inserted after the chord */
  GString *display; /**< some text to display to describe the LilyPond attached to the chord */
  gint tx,ty; /**< x and y offsets in pixels for the display text */
  gint minpixels;/**< horizontal space needed by the display, for directives that are attached to non-display objects (e.g. Score, Paper ...) this field is free for scripts to use */
 // guint x /*only_for*/, y /*ignored_by*/; /**< ids of score layouts that are to enable/ignore this directive */
#define DENEMO_ALLOW_FOR_LAYOUTS (0)
#define DENEMO_IGNORE_FOR_LAYOUTS (1)

  gint flag;/**<interpretation of layouts field DENEMO_ALLOW_FOR_LAYOUTS DENEMO_IGNORE_FOR_LAYOUTS */
  GList *layouts;/**< list of layouts that ignore/respect this directive */


  DenemoGraphic *graphic; /**< what to draw for this directive */
  GtkWidget *widget;  /**<  a button or menu item for accessing the directive for editing or actioning */
  gint gx, gy; /**< x and y offsets in pixels for the graphic */
  GString *graphic_name; /**< name of the graphic to be drawn the suffixes .svg or .png will be searched for */
  GString *grob; /**<name of LilyPond grob which this directive creates. Use for tweaking positions, padding etc */

  /* warnings
     1) these values cannot be changed without bumping the denemo file format version
     2) if they are to be available to scheme scripts (normal) you must insert the code in view.c*/
#define DENEMO_OVERRIDE_LILYPOND (1<<0)
#define DENEMO_ALT_OVERRIDE (1<<1)
#define DENEMO_OVERRIDE_GRAPHIC (1<<2)
#define DENEMO_OVERRIDE_EDITOR (1<<3)
#define DENEMO_OVERRIDE_AFFIX (1<<4)
#define DENEMO_OVERRIDE_TAGEDIT (1<<5)

//for with{} blocks in staff directives
#define DENEMO_OVERRIDE_WITH (DENEMO_ALT_OVERRIDE | DENEMO_OVERRIDE_AFFIX) 

#define DENEMO_OVERRIDE_VOLUME (1<<8)
#define DENEMO_OVERRIDE_DURATION (1<<9)
#define DENEMO_OVERRIDE_REPEAT (1<<10)
#define DENEMO_OVERRIDE_CHANNEL (1<<11)
#define DENEMO_OVERRIDE_TEMPO (1<<12)
#define DENEMO_OVERRIDE_TRANSPOSITION (1<<13)

#define DENEMO_MIDI_MASK (DENEMO_OVERRIDE_VOLUME | DENEMO_OVERRIDE_DURATION | DENEMO_OVERRIDE_REPEAT | DENEMO_OVERRIDE_CHANNEL | DENEMO_OVERRIDE_TEMPO | DENEMO_OVERRIDE_TRANSPOSITION)

#define DENEMO_OVERRIDE_ONCE (1<<16)
#define DENEMO_OVERRIDE_STEP (1<<17)
#define DENEMO_OVERRIDE_RAMP (1<<18)

#define DENEMO_MIDI_ACTION_MASK (DENEMO_OVERRIDE_ONCE | DENEMO_OVERRIDE_STEP | DENEMO_OVERRIDE_RAMP)


#define DENEMO_OVERRIDE_RELATIVE (1<<24)
#define DENEMO_OVERRIDE_PERCENT (1<<25)

#define DENEMO_MIDI_INTERPRETATION_MASK (DENEMO_OVERRIDE_RELATIVE | DENEMO_OVERRIDE_PERCENT)

#define DENEMO_OVERRIDE_DYNAMIC (1<<28)
#define DENEMO_OVERRIDE_HIDDEN (1<<29)
#define DENEMO_OVERRIDE_MARKUP (1<<30) //For score and movement widgets (which can display as buttons) this controls whether the button text uses markup
#define DENEMO_OVERRIDE_ABOVE DENEMO_OVERRIDE_MARKUP //For chord directives, makes the graphic display above rather than below the chord


  guint32 override; /**< specifies what if anything of the built-in behaviour of the object the directive is attached to is to be overriden by this directive and values to use when overriding MIDI */
  GString *midibytes;/**< values to be used for MIDI generation; the meaning depends fields in override */
  gboolean locked;/**< If true the directive cannot be deleted easily */
  GString *data;/**< data used by scripts that manipulate the directive, typically scheme data */
} DenemoDirective;

/**
 * Contains the lilypond header information for the movements, plus markup between movements.
 *
 */

typedef struct LilypondHeaderFields
{
/* LilyPond movement header and markup information */
  GString *title;
  GString *subtitle;
  GString *poet;
  GString *composer;
  GString *meter;
  GString *opus;
  GString *arranger;
  GString *instrument;
  GString *dedication;
  GString *piece;
  GString *head;
  GString *copyright;
  GString *footer;
  GString *tagline;
  //GString *extra;
  /* lilypond before and after each \score block     */
  GString *lilypond_before;
  GString *lilypond_after;
  /* preferences to go into \layout block */
  //GString *layout;
}LilypondHeaderFields;



typedef enum
{
  REPLACE_SCORE,
  ADD_STAFFS,
  ADD_MOVEMENTS,
  GUIDED_IMPORT,
  SOURCE_PDF,
  PROOFREAD
} ImportType;

typedef enum
{
  WOODWIND,
  BRASS,
  STRINGS,
  VOCALS,
  PITCHEDPERCUSSION,
  PLUCKEDSTRINGS,
  KEYBOARDS,
  NONE
}InstrumentType;


typedef enum InputSource {
  INPUTKEYBOARD = 0,
  INPUTAUDIO = 2,
  INPUTMIDI = 1
} InputSource;

typedef enum MidiDestination {
  MIDITOSCORE = 0,
  MIDITHRU = (1<<1),
  MIDIRECORD = (1<<2),
  MIDIPLAYALONG = (1<<5),
  MIDICONDUCT = (1<<6),

} MidiDestination;

typedef enum DenemoViewType {
  DENEMO_MENU_VIEW,//menus are visible
  DENEMO_LINE_VIEW,//menus not visible,
  DENEMO_PAGE_VIEW //menus not visible, defaults to full screen and several systems
} DenemoViewType;


/**
 * Structure to contain the list of Instruments for the score
 * configuration wizard
 *
 */
typedef struct
{
    GString *name;
    GString *midiinstrument;
    gint sclef;
    gint transposition;
    gint numstaffs;

}InstrumentConfig;

/**
 * Stores global instrument type and a list of InstrumentConfig structures
 */
typedef struct
{
    InstrumentType type;
    GList *instruments;  // List to contain a list of Instruments of given type
}InstrumentList;

typedef struct DenemoPosition { /**<Represents a position in a Score */
  gint movement;
  gint staff;
  gint measure;
  gint object;/**< 0 means no object */
  gboolean appending;/**< if cursor is in appending position */
  gboolean offend;/**< cursor is shown red in this case, obscure connection with appending */
  gint leftmeasurenum; /**< start at 1 */
} DenemoPosition;


typedef enum
{
  STATE_NONE = 0,               //not a background typeset
  STATE_OFF = 1 << 0,           //background typeset complete
  STATE_ON = 1 << 1,            //background typeset in progress
  STATE_PAUSED = 1 << 2         //background typesetting turned off to allow printing
} background_state;



typedef struct DenemoPrintInfo
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
  gchar *printname_svg[2];
  gchar *printname_midi[2];
  gchar *printname_ly[2];
  gchar *error_file;
} DenemoPrintInfo;
/**
 * Contains data required for undo/redo operation
 * Borrowed idea from GScore
 */
typedef struct DenemoUndoData
{
  enum action_type action; /*action type must come first*/

  gpointer object;    /* pointer to object to be undone/redone */
  DenemoPosition position; /* position where delete/insert took place */

} DenemoUndoData;


/**
 * Control of the LilyPond output for the whole musical score DenemoProject
 *
 */
typedef struct DenemoLilyControl
{
  GString *papersize;
  GString *staffsize;
  GString *lilyversion;
  gboolean orientation;
  gboolean excerpt;
  GList *directives; /**< list of DenemoDirective for all music in the movements */

} DenemoLilyControl;


typedef struct DenemoScriptParam { /**< commands called by scripts use one of these to pass in a string and return a boolean */
  GString *string;/**< input string to command */
  gboolean status;/**< return value - TRUE = normal case execution of command/FALSE = exceptional case*/
} DenemoScriptParam;


typedef struct DenemoScoreblock {
  GString *lilypond;/**< text of the scoreblock */
  gboolean visible;/**< Whether the scoreblock should be used by default */
  gboolean layout_sync;/**< Value of project->layout_sync when the scoreblock was created */
  GtkWidget *widget;/**< Widget to be placed in the Score Layout window for this scoreblock */
  GList *staff_list;/**< List of staff frames contained in widget */
  gchar *name;/**< name for this scoreblock */
  gchar *uri;/**< uri for the output from printing this scoreblock */
  guint32 id;/**< an id for this scoreblock generated from name, as a quick identifier */
  gint movement;/**< Which movement the scoreblock outputs, 0 = all movements. Only used for standard scoreblocks */
  gchar *partname; /**< Which part the scoreblock outputs, NULL = all parts. Only used for standard scoreblocks */
  gchar *instrumentation;/**< value for the instrumentation value for book titling, taken from the part name */
  gboolean text_only;/**< TRUE if only the lilypond text exists for this widget - no widget structure to be refreshed */
} DenemoScoreblock;

//these typdefs are needed to make the macros that install all the directive functions work
typedef struct header
{
  GList *directives;
}
header;
typedef struct scoreheader
{
  GList *directives;
}
scoreheader;
typedef struct paper
{
  GList *directives;
}
paper;
typedef struct layout
{
  GList *directives;
}
layout;

typedef struct movementcontrol
{
  GList *directives;
}
movementcontrol;

typedef enum DenemoRecordingType {
  DENEMO_RECORDING_AUDIO,/**< Recording is an audio file represented by libsndfile handle */
  DENEMO_RECORDING_MIDI/**< Recording is an audio file stored in recorded_midi_track */
} DenemoRecordingType;

typedef struct DenemoRecordedNote {
  gint timing;/**< time in frames, (divide by samplerate to get to seconds) */
  //gboolean noteoff;
  gint mid_c_offset;
  gint enshift;
  gint octave;
  gint duration;/**< guessed baseduration, 0 if not known */
  gint dots; /**<guessed number of dots */
  gchar *midi_event;/* three byte midi message 0x90 or 0x80, midi_note*/
  gint event_number;/**< GUIDED MIDI IMPORT ONLY number of midi event giving rise to the note */
  gint track_number;/**< GUIDED MIDI IMPORT ONLY  track holding the event midi event giving rise to the note */
} DenemoRecordedNote;

typedef struct DenemoRecording {
  DenemoRecordingType type;
  gchar *filename; /**< audio file. Could be extended to take MIDI file too */
  gint samplerate; /**< frames per second */
  gint channels; /**< audio only */
  gint leadin;/**< AUDIO: number of frames to skip at start, silence to be emitted before play if negative */
  gdouble offset;/**<MIDI: time in seconds by which the MIDI track has been offset */
  gdouble volume;
  gint nframes;/**< number of frames in the audio */
  GList *notes;  /**< data is DenemoRecordedNote* */
  gpointer sndfile; /**< sndfile handle */
} DenemoRecording;

typedef enum DenemoTargetType {
    TARGET_NONE = 0,
    TARGET_OBJECT,
    TARGET_CHORD,
    TARGET_NOTE,
    TARGET_SLUR,
    TARGET_TIE,
    TARGET_CRESC,
    TARGET_DIM,

    //TARGET_,
} DenemoTargetType;

typedef struct DenemoTarget {
  DenemoTargetType type;
  gint mid_c_offset;
  gint directivenum;
  gint staffnum;
  gint measurenum;
  gint objnum;
} DenemoTarget;

typedef struct DenemoBrace {
    gint startstaff, endstaff;//count from 1
    int starty, endy;
    gboolean curly;
} DenemoBrace;

typedef struct DenemoScrollPoint {
     gdouble time, adj;
     gdouble x, y;
} DenemoScrollPoint;
typedef struct DenemoOmissionCriterion {
     gchar *name;
     guint32 id;
} DenemoOmissionCriterion;

/*
 *  DenemoScore structure representing a single movement of a piece of music.
 *  A movement corresponds with a single \score{} block in the LilyPond language
 *  that is,  uninterrupted music on a set of staffs, preceded by a title.
 */

typedef struct DenemoMovement
{
  gboolean readonly; /**< Indicates if the file is readonly or not (NOT USED)*/
  gboolean sketch; /** Movement is not intended for inclusion in final typeset */
  gint leftmeasurenum; /**< start at 1 */
  gint rightmeasurenum;/**< start at 1 */
  gint top_staff;
  gint bottom_staff;
  gint measurewidth; /**< List of all minimum measure widths */
  GList *measurewidths;
  gint widthtoworkwith;
  gint staffspace;

  DenemoRecording *recording;/**< Audio or MIDI recording attached to movement */
  gint marked_onset_position;/**< horizontal position in display of note onset in audio marked by user */
  GList *marked_onset;/**< Note onset in audio selected by user */
  gdouble start_time; /**< time in seconds to start playing at */
  gdouble end_time; /**< time to end playing at */
  DenemoObject *playingnow; /**< the last object played via MIDI; it must not be dereferenced as it may no longer exist */
  gdouble playhead; /**< MIDI time in seconds of playhead, ie point when last MIDI event was processed for output */
  gdouble start_player;/**< system time when MIDI player started */
  gpointer recorded_midi_track;//an smf_track_t recorded
  gpointer loaded_midi_track;//a (part of) the recorded_midi_track synchronized to the score
  
  gdouble master_volume;/**< the volume (velocity) used is this times the nominal vol */
  gdouble master_tempo;/**< the tempo used is this times the nominal tempo */
  gdouble tempo_change_time;/**< system time from which the master_tempo is to be used */
  gdouble rightmost_time; /**< MIDI time of rightmost visible object */
  GList *scroll_points; /**< playback view scroll points for this movement */
  DenemoDirective* directive_on_clipboard;/**< when a (non-standalone) directive can be "pasted" using the script CreateScriptForDirective::clipboard this records its origin. Do not de-reference as it may no longer exist */
  gboolean selection_is_copied;
  gdouble zoom;/**< zoom setting for this movement */
  gdouble preview_zoom; /**< zoom  of print preview pane */
  gdouble system_height;/**< fraction of scorearea height allotted to one system (line) in this movement of the score, the same for all lines */
  //Settings for the views on this movement

  gdouble page_zoom;/**< zoom for page view */
  gdouble page_system_height;/**< system height for page view */
  gint page_width;/**< width to use for window in page view. 0 means use full screen */
  gint page_height;/**< height to use for window in page view */
  gint stored_width;/**< width to use for window returning from page view */
  gint stored_height;/**< height to use for window returning from page view */

  /* Fields that have more to do with the data model and its manipulation,
   * though they may be modified by side-effects of the drawing routines */
  // score thescore;
  staffnode *thescore;
  gint currentmovementnum;/**< position of this DenemoMovement in the project->movements list starting at 1 */
  staffnode *currentprimarystaff;
  staffnode *currentstaff;
  gint currentstaffnum;/**< start at 1 */
  measurenode *currentmeasure;
  DenemoTarget target;/**< The target of a wysiwyg operation. This is currently only set by clicking on the print view window. */
  gint currentmeasurenum;/**< start at 1 */
  objnode *currentobject; /**< currentobject points to the note preceding the cursor when the
   * cursor is appending. == NULL only when currentmeasure is empty. */
  gint highesty; /**< max value of highesty of chord in the staff */
  gint cursor_x;/** this is currentobjnum, that is, the position of the current object in the currentobject GList */
  gint cursor_y;
  gint staffletter_y;
  gint maxkeywidth;
  gboolean cursor_appending;

  gboolean cursoroffend;
  //gint cursorclef;
  //gint cursoraccs[7];
  //gint cursortime1;
  //gint cursortime2;
  //gint curmeasureclef;
  //gint curmeasurekey;
  //gint curmeasureaccs[7];
  //gint nextmeasureaccs[7];
  /* These are used for determining what accidentals should be there
   * if the cursor advances to the next measure from the next "insert chord"
   * operation */
  gint curmeasure_stem_directive;
  gint pending_enshift;/**< accidental to be used for next inserted note, cleared on note insert */


  /* Is there a figured bass present, is so this relates the bass
   * with its figures staff, if one is present */
  staff_info * has_figures;
  staff_info *has_fakechords;

  /* Now stuff that's used for marking areas */
  gint markstaffnum;
  gint markmeasurenum;
  gint markcursor_x;
  DenemoSelection selection;


  movementcontrol movementcontrol;/*< Directives for control of the whole movement */
  layout layout;/*< Directives for the layout block of the movement */
  header header;/*< Directives for the header block of the movement */


  guint changecount;
  /* Fields used for MIDI playback */
  gpointer smf;/*< an smf_t structure for libsmf to work with */
  gint tempo;
  gint start;
  gint end;
  gint stafftoplay;
  guint smfsync;/**< value of changecount when the smf MIDI data was last refreshed */

  /*list of undo data */
  GQueue *undodata;
  GQueue *redodata;
  gint undo_guard;
  gboolean redo_invalid;/*< the re-do queue is awaiting freeing and should not be used */



  GList *Instruments;
  GtkWidget *buttonbox;/*< box for buttons accessing DenemoDirectives attached to the this movement*/
  GtkWidget *lyricsbox;/*< box for notebooks containing verses of lyrics for the movement */
} DenemoMovement;

/**
 * DenemoProject representing a musical score, with associated top level
 * GUI and a list of movements (DenemoMovement) and a pointer to the current
 * movement.
 */
#define DENEMO_MAX_SYSTEMS (100) /**< Number of lines of music that can be displayed */
typedef struct DenemoProject
{
  gint id; /* A unique id, not repeated for this run of the Denemo program */

  DenemoViewType view;/**< The current view */
  gint lefts[DENEMO_MAX_SYSTEMS];/**< an array to hold the leftmeasurenum of each system in the last-drawn score, used for determining the mouse position on the music */
  gint rights[DENEMO_MAX_SYSTEMS];/**< an array to hold the rightmeasurenum of each system in the last-drawn score, used for determining the mouse position on the music */
  gint scales[DENEMO_MAX_SYSTEMS];/**< an array to hold the percent horizontal scaling of each system in the last-drawn score, used for determining the mouse position on the music */
  gint leftmargin;
  #define BASIC_LEFT_MARGIN (20) /**< margin in display for staff/voice tools */
  GList *braces;
  #define BRACEWIDTH (20) /**< width of each brace in display */
  GtkWidget *buttonboxes;/**< box for boxes showing directives */
  GtkWidget *buttonbox;/**< box for buttons accessing DenemoDirectives attached to the whole score */
  GtkWidget *movements_selector; /**< box for buttons to select movements */

  gchar *xbm; /**< xbm representation of graphic bitmap from selected rectangle in print preview area*/
  gint xbm_width, xbm_height;/**< width and height of the xbm data */


  gchar *namespec;/**< A spec of which parts/movements to print */
  GString *printhistory;


  InputSource input_source;/**< Where pitches are coming into Denemo (keyboard, audio, midi) */
  InputSource last_source;/**< Where the last input came from */
  MidiDestination midi_destination;/**< Where MIDI in should go */
  GQueue *pending_midi;/**< a MIDI effect to be output with the next note */
  gboolean audio_recording; // currently recording audio output
  gboolean midi_recording; // currently recording MIDI input
  input_mode mode; /**< Input mode for Score */


  GList *movements;   /**< a list of DenemoMovement, NULL if just one movement */
  DenemoMovement *movement;  /**< the (current)  movement in the musical score controlled by this project */
  DenemoLilyControl lilycontrol; /**< Directives for the start of the score and before every movement */

  scoreheader scoreheader;/*< Directives for the header block at the start of the score */
  paper paper;/*< Directives for the paper block of the score */

  gboolean has_script;/*< true if there is a script to be run on loading the DenemoProject from disk */
  GList *standard_scoreblocks; /**< List of automatically generated \score blocks for LilyPond output elements are DenemoScoreblock * */
  GList *custom_scoreblocks; /**< List of customized  \score blocks for LilyPond output, elements are DenemoScoreblock * */
  GtkWidget *score_layout; /**< The window in which custom_scoreblock widgets are placed */
  guint layout_id; /**< cached value of the currently selected layout id */
  DenemoOmissionCriterion *criterion; /**< an omission criterion chosen by the user to avoid directives marked to ignore this name - a lightweight score layout in effect. */
  GList *criteria; /**< a list of DenemoOmissionCriterion which have been defined for this score */
  GList *callbacks;/**< scheme callbacks on deletion */
  gpointer lilystart, lilyend; /**<range of lilytext  */
  GString **target; /**< pointer to target string for modification in lilytext  */
  GList *anchors;/**< anchors in the LilyPond text at points where that can be edited */

  GString *filename;/**< the filename to save to */
  GString *tabname;/**< the name of windows tab */
  GString *autosavename;/**< the filename to autosave to, full path */
  DenemoSelection thumbnail; /**< the selection from which to create a thumbnail on exit */

  gint undo_level;/**< level of script nesting 0 = staging point for undo to return to */
  gboolean notsaved;/**< edited since last save */
  guint changecount;/**< number of edits since score loaded */
  guint lilysync;/**< value of changecount when the Lily text was last refreshed */

  guint layout_sync;/**< value of changecount when the layout structure of the score was last changed, used to detect if a DenemoScoreblock is out of date */
  /* support for rhythm patterns */
  GList *rhythms;/**< list of RhythmPattern s */
  GList *currhythm; /**< a pointer into rhythms, whose data is the RhythmPattern being followed */
  GList *cstep; /**< step within RhythmPattern->clipboard, the a GList* holding the current DenemoObject of the current rhythm pattern in its clipboard
                        the data field of cstep holds a DenemoObject*. Several of these csteps correspond to a single rstep if the objects are not pitched (ie not of type CHORD with at least one note) */
  GList *rstep; /**< step within the circular list RhythmPattern->rsteps, the step that will be used for the next entered pitch
                     when applying pitches to the pattern non pitched DenemoObjects on the clipboard are emitted at each step, moving cstep on for each object.*/

  struct RhythmPattern *prevailing_rhythm; /**< one of singleton_rhythms used for entering notes */

  /* support for positioning source views */
  gint source_x;
  gint source_y;
  gint source_width;
  gint source_height;
  gint source_scale;/* scale is x1000 */
  /* support for time spent editing the score */
  gint total_edit_time;/* in seconds */
  gchar* script; /**< The script, synchronized with the view buffer */
}DenemoProject;


/**
 * RhythmPattern: a list of RhythmElements with a button to invoke it;
 */

typedef struct RhythmPattern
{
  GList *rsteps; /**< the data are RhythmElements */
  GtkToolButton *button; /**< the button on the rhythm toolbar which invokes this rhythm. Its label is set to the highlightlabel field of the currently in use rstep */
  GList *clipboard;/**< a Denemo clipboard, used to create this patttern. FIXME as with the DenemoClipboard there is a redundant layer: the data element of this field is the GList* of DenemoObjects */
  gchar *name;/**< a user-facing name for this pattern */
  GString *nickname;/**< a custom user name for this pattern */
  GString *lilypond; /**< LilyPond syntax for this pattern, used to paste snippets into markup */
} RhythmPattern;


/**
 * RhythmElement: information about one element of a RhythmPattern,
 * e.g. one RhythmElement could contain the actions "quarter-note,dotted,begin slur";
*/

typedef struct RhythmElement
{
  GList* functions; /**< data in list are functions to be called including modifiers
              eg insert_chord_3key, add dot, slur ...  */
  gpointer highlightlabel; /**< a string, but displayed in music font, which labels the button when this RhythmElement is
          the current one*/
  RhythmPattern *rhythm_pattern;/**< the rhythm pattern which this element belongs to */
} RhythmElement;


struct cs_callback
{
    GtkWidget *entry;
    GtkWidget *dialog;
    DenemoProject *project;
};


/**
 * The (singleton) root object for the program
 *
 */
struct DenemoRoot
{
  gboolean non_interactive; /* if TRUE denemo should not display project, receive or send sounds etc*/
  gchar *scheme_file;/* filename for scheme code to run on startup */
  gchar *scheme_commands;/* scheme code to run on startup after scheme_file */
  /* Fields used fairly directly for drawing */
  GtkWidget *page;
  GtkWidget *scorearea;
  GtkWidget *command_manager;
  GtkAdjustment *vadjustment;
  GtkWidget *vscrollbar;
  GtkAdjustment *hadjustment;
  GtkWidget *hscrollbar;
  DenemoPalette *currentpalette;
  GtkWidget *hpalettes;/**< hbox holding horizontal docked palettes */
  GtkWidget *vpalettes;/**< vbox holding vertical docked palettes */
  GtkWidget *printarea;/**< area holding a print preview */
  GtkWidget *printvscrollbar;/**< scrollbar widget for printarea */
  GtkWidget *printhscrollbar;/**< scrollbar widget for printarea */
  GdkPixbuf *pixbuf;/**< print preview pixbuf */
  GtkWidget *playbackview;/**< area holding svg typeset for animating playback */
  DenemoPrintInfo *printstatus;/**< Information about the currenty typesetting activity */
  gchar *lilypond_installed_version;/**< lilypond version that will be executed to typeset the score */
  gchar *lilypond_include_dir;/**< Directory holding lilypond include files - varies with lilypond version */
  GtkWidget *textwindow; /**< LilyPond output window */
  GtkTextView *textview; /**< LilyPond output text view */
  GtkTextBuffer *textbuffer;   /**< buffer for LilyPond text */
  /* window state */
  gint width;
  gint height;
  gboolean maximized;
  gint color;/**< the color of the background */
  gboolean hovering_over_margin_up;
  gboolean hovering_over_margin_down;
  gboolean hovering_over_brace;
  gboolean hovering_over_partname;
  gboolean hovering_over_clef;
  gboolean hovering_over_timesig;
  gboolean hovering_over_keysharpen;
  gboolean hovering_over_keyflatten;
  gboolean hovering_over_movement;
  gboolean hovering_over_left_arrow;
  gboolean hovering_over_right_arrow;
  GList *object_hovering_over;
  gboolean dragging_start_playback_marker;
  gboolean dragging_end_playback_marker;
  
  GdkCursor *GDK_LEFT_PTR;
  GdkCursor *GDK_SB_V_DOUBLE_ARROW;
  GdkCursor *GDK_SB_H_DOUBLE_ARROW;
  GdkCursor *GDK_BLANK_CURSOR;
  GdkCursor *GDK_X_CURSOR;
  GdkCursor *GDK_TARGET;
  
  
  
  GList *hidden_staff_heights; /**< list of y positions of markers in display for hidden staff positions */
  keymap *map; /**< pointer to data describing each of the Denemo commands and their keyboard shortcuts */
  gchar *last_merged_command;/**<filename of last command merged into the menu system */
  gint last_keyval, last_keystate;/**< most recent keypress which successfully invoked a command */
  GList *projects; /**< the list of DenemoProject objects, representing pieces of music simultaneously open */
  DenemoProject *project; /**< The current project */
  DenemoPrefs prefs;  /**< Preferences stored on exit and re-loaded on startup */
  gint autosaveid;/**< autosave timer id current tab is saved, so this is only used as a flag to decide whether timer needs starting */
  gint accelerator_status; /**< if the accelerators have been saved, or extra ones for special keys defined  */
  GtkUIManager *ui_manager;  /**< UI manager */
  GtkWidget *window;
  GtkWidget *console;/**< GtkTextView for console output */
  GtkActionGroup *action_group;/*< The action group for the actions that are Denemo commands */
  GtkWidget *notebook;/**< contains the project.page widgets */
  GtkWidget *statuslabel;/**< label that appears at bottom left of main window to describe cursor position in score */
  GtkWidget *playback_control;/**< frame containing controls for playback */
  GtkWidget *midi_in_control;/**< frame containing controls for midi in */
  GtkWidget *audio_vol_control;/**< hbox containing vol control for source audio */
  GtkWidget *input_label; /**< A label widget advising of source of external input */
  GString *input_filters; /**< Description of any filters operating on external input */
  GtkWidget *menubar;/**< Main menubar to giving load/save play etc functionality */

  GList *continuations;

#define NB_SINGLETON_RHYTHMS 256
  struct RhythmPattern *singleton_rhythms[NB_SINGLETON_RHYTHMS]; /**< rhythm patterns for the EntryToolbar */
  gboolean ScriptRecording;/**< TRUE when menuitems presses are being recorded as scheme script*/
  gint LastCommandId; /**< id of last command used */
  gint keyboard_state;/**< state of last keypress/release controlling MIDI in*/
  gboolean keyboard_state_locked;/**< lock against changes by shift and control keypresses */

  GtkWidget *script_view; /**< a GtkTextView containing a scheme script */
  GList *palettes; /**< list of palettes of buttons for activating scripts */
  gchar *old_user_data_dir; /**< set in case of upgrading denemo version */
  gboolean *fatal_scheme_errors; /** Abort on scheme errors */
  gboolean *silent; /** Don't log any message */
  gboolean *verbose; /** Display every messages */
  guint pending_layout_id;//Non zero when the current layout being created will be renamed to have this id 
};

extern struct DenemoRoot Denemo; /**< The root object. */
#endif
