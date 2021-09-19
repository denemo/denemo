/**
 * denemo_objects.h
 *
 * Description: Contains definitions for denemo data structures
 *
 *
 * Author: Adam Tee <adam@ajtee.plus.com>, (C) 2005
 *
 * Copyright: See COPYING file that comes with this distribution
 *
 */

#include <gtk/gtk.h>

#ifndef DENEMOOBJECTS_H
#define DENEMOOBJECTS_H



/**
 * enum containing notehead
 * definitions
 */
typedef enum headtype
{
  DENEMO_NORMAL_NOTEHEAD, /*!< Enum value DENEMO_NORMAL_NOTEHEAD */
  DENEMO_CROSS_NOTEHEAD,
  DENEMO_HARMONIC_NOTEHEAD,
  DENEMO_DIAMOND_NOTEHEAD,
  DENEMO_BLACK_NOTEHEAD
}headtype;

/**
 * structure encapsulating a
 * grace note
 */
typedef struct grace
{
  gboolean on_beat;
  gint duration;
}grace;


/**
 * structure encapsulating a
 * musical note
 */
typedef struct note
{
  gint mid_c_offset;/**< This is used to define (the pitch of) a note.
               A positive number is the number of half
               steps above middle c. A negative number is below middle c.*/
  gint enshift;/**< Enharmonic shift. Should the note be notated as sharp (1) double-sharp (2) or flat (-1) double-flat (-2) or natural (0). No other values are legal. */
  gboolean reversealign;
  gboolean showaccidental;/**< This tells denemo to show all possible accidentals?? cautionary accidental?? */
  gint position_of_accidental; /**< Holds number of pixels to the left of the notehead that the
                   accidental should be drawn at.  */
  gint y; /**< Holds y co-ordinate for drawing rather than recalculating it each
           run through the drawing loop. see calculateheight. The coordinate is based on the top line of the staff = 0, above the staff is negative
           *    below is positive. The staff space is 10  */
  enum headtype noteheadtype; /**< Holds note head type.  */
  GList *directives;/**< list of DenemoDirective to apply to the note */
}
note;

/**
 * Enum defining ornament types
 *
 */
typedef enum ornament {
  STACCATO=1,
  D_ACCENT,
  CODA,
  FERMATA,
  TENUTO,
  TRILL,
  TURN,
  REVERSETURN,
  MORDENT,
  STACCATISSIMO,
  MARCATO,
  UBOW,
  DBOW,
  RHEEL,
  LHEEL,
  RTOE,
  LTOE,
  FLAGEOLET,
  OPEN,
  PRALLMORDENT,
  PRALL,
  PRALLPRALL,
  SEGNO,
  SFORZATO,/*unused is a dynamic */
  STOPPED,
  THUMB,
  TRILL_ELEMENT,
  TRILLELEMENT,
  UPPRALL,
  D_ARPEGGIO
}Ornament;




/**
 * Structure describing a chord
 * 1;7B
 *
 */
typedef struct chord
{
  GList *notes; /**< NULL if the chord is a rest
           else Glist of the notes of the chord
           (in order of mid_c_offset value)
           notes->data fields are of type note*
        */
  GList *dynamics;  /**< test for new dynamic handling */
  gboolean has_dynamic;
  gint highestpitch;
  gint lowestpitch;
  gint highesty;
  gint lowesty;
  gint baseduration; /**< Value to denote the basic chord length, 0 = whole note, 1 = 1/2 note etc. Values -1...-7 are swung notes other negative values specifies the basic chord length in terms of ticks, used for breve etc. */
  gint numdots; /**< Number of dots that are applied to the note */
  gint sum_mid_c_offset;
  gint numnotes;
  gboolean chordize;/**< TRUE if this chord is to be treated as a multi-note chord even if it only has one note */

  gboolean is_tied;
  gboolean is_stemup;
  gboolean is_reversealigned;
  gboolean slur_begin_p; /**< Is this note a beginning of a slur? */
  gboolean slur_end_p; /**< Is this note an end of a slur? */
  gboolean crescendo_begin_p;
  gboolean crescendo_end_p;
  gboolean diminuendo_begin_p;
  gboolean diminuendo_end_p;
  gboolean hasanacc;
  gboolean is_grace;  /**< Flag to show whether note is a grace note */
  gboolean struck_through; /**< Flag for a struck through stem*/
  gint stemy;       /**< Stores the y position of the end of the stem */
  GString *lyric; /**< Pointer to the lyrics applied to that chord */
  gboolean is_syllable; /**< Is the lyric a syllable? */
  gboolean center_lyric; /**< Should the lyrics be centered or
                should it be directly under the note?
                that it is attatched to? */

  gboolean is_figure; /**< the reason for using this boolean is to exploit
             the fact that all the spacing and printing of
             figures can use the code for the CHORD type */
  gpointer figure; /**< when this chord is a bass note
              (figure !=NULL && is_figure==FALSE) this
              pointer points to an objnode in a FiguredBassStaff.
              That objnode's data is a DenemoObject of type CHORD.
              It is linked into the corresponding FiguredBassStaff if
              one exists.
              When this chord is_figure then figure is a
              GString* containing the
              figures in lilypond format. */
  GList *tone_node; /**< which tone this note was extracted from */
  gboolean is_fakechord; /**< This is the actual value of the fake chord if is_fakechord */
  gpointer fakechord; /**< This is the actual fake chord string if is_fakechord */

  GList *directives;/**< list of DenemoDirective to apply to the chord */

}
chord;

/**
 * Structure defining an indicator that a tuplet is starting
 */
typedef struct tupopen
{
  gint numerator;
  gint denominator;
  GList *directives;/**< list of DenemoDirective to apply to the tuplet */
}
tupopen;

typedef tupopen tuplet; //used for tupclose or tupopen

/**
 * Enum defining clef values
 *
 */
typedef enum clefs
{
  DENEMO_TREBLE_CLEF=0,
  DENEMO_BASS_CLEF,
  DENEMO_ALTO_CLEF,
  DENEMO_G_8_CLEF,
  DENEMO_TENOR_CLEF,
  DENEMO_SOPRANO_CLEF,
  DENEMO_F_8_CLEF,
  DENEMO_FRENCH_CLEF,
  DENEMO_BARITONE_CLEF,
  DENEMO_INVALID_CLEF
}clefs;

/**
 * Indicator for a clef change
 */
typedef struct clef
{
  enum clefs type;
  GList *directives;
}
clef;

/**
 * Indicator for a time-signature change. Only appears at the
 * beginning of a measure
 */
typedef struct timesig
{
  gint time1; /**< This is the numerator for a time signature */
  gint time2; /**< This is the denominator for a time signature */
  GList *directives;
}
timesig;

/**
 * Indicator for a key-signature change.
 */
typedef struct keysig
{
  gint number; /**< key number -7 to 7 for major/minor 0 to 40 for mode */
  gint isminor; /**< Type of key 0-major 1-minor 2-mode */
  gint mode;  /**< Mode indicator */
  gint accs[7];
  GList *directives;
}
keysig;

/* Dynamic */

typedef struct dynamic
{
  GString *type;
}
dynamic;

/**
 * Enum defining barline types
 *
 */
typedef enum barline_type {
  ORDINARY_BARLINE,
  DOUBLE_BARLINE, /**< Double Bar */
  END_BARLINE,
  OPENREPEAT_BARLINE,
  CLOSE_REPEAT,
  OPEN_CLOSE_REPEAT
} barline_type;

/**
 * Structure encapsulating a barline
 *
 */
typedef struct barline
{
  barline_type type;
}
barline;



/**
 * Lyric datastructure
 */
typedef struct lyric
{
  GString *lyrics;  /**< This is the text string containing the lyrics */
  gint position;
  gboolean is_syllable; /**< Is the lyric a syllable? */
  gboolean center_lyric; /**< Should the lyrics be centered? */
}lyric;


/* A standalone DenemoDirective. lilydirective is an obsolete name */

#define lilydirective DenemoDirective
#if 0
typedef struct lilydirective DenemoDirective;

{
  GString *directive;/**< the LilyPond text */
  gboolean locked;/**< If true the directive cannot be deleted easily */
  GString *display;/**< Something for Denemo to display (to indicate what the directive is doing*/
  gint x;/**< horizontal offset of display text */
  gint y;/**< vertical offset of display text */
  GdkBitmap *graphic; /**< bitmap to draw for this directive */
  gint width, height; /**< width and height of the bitmap */
}
lilydirective;
#endif

/**
 * Enum defining stem direction values
 *
 */
typedef enum stemdirections
{
  DENEMO_STEMDOWN=1,
  DENEMO_STEMBOTH,
  DENEMO_STEMUP
}stemdirections;

/**
 * Indicator that the following music should be all stemup, all
 * stemdown, or stemmed normally
 */
typedef struct stemdirective
{
  enum stemdirections type;
  GList *directives;/**< list of DenemoDirective to apply to the stemdirective */
}
stemdirective;


/**
 * a note and duration (e.g. obtained by pitch recognition)
 * plus field to indicate if the tone is spurious
 */
typedef struct tone
{
  gint duration;
  gint step;
  gint octave;
  gint enshift;
  gboolean valid;
}
tone;

#define NOTE0 "\xF0\x9D\x85\x9D"
#define NOTE1 "\xF0\x9D\x85\x9E"
#define NOTE2 "\xF0\x9D\x85\x9F"
#define NOTE3 "\xF0\x9D\x85\xA0"
#define NOTE4 "\xF0\x9D\x85\xA1"
#define NOTE5 "\xF0\x9D\x85\xA2"
#define NOTE6 "\xF0\x9D\x85\xA3"
#define NOTE7 "\xF0\x9D\x85\xA4"
#define NOTE8 "\xF0\x9D\x85\xA5"

#define REST0 "\xF0\x9D\x84\xBB"
#define REST1 "\xF0\x9D\x84\xBC"
#define REST2 "\xF0\x9D\x84\xBD"
#define REST3 "\xF0\x9D\x84\xBE"
#define REST4 "\xF0\x9D\x84\xBF"
#define REST5 "\xF0\x9D\x85\x80"
#define REST6 "\xF0\x9D\x85\x81"
#define REST7 "\xF0\x9D\x85\x82"
#define REST8 "\xF0\x9D\x85\x83"

#endif
