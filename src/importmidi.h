/* importmidi.h

AJAnderson

*/

#include <gtk/gtk.h>
#include <stdio.h>

typedef struct notetype
{
	gint notetype;
	gint numofdots;
	gint tied;
}notetype;

typedef struct nstack 
{
	gint *pitch;
   	gint *measure; /*can this be calculated on noteoff?*/
     	gint *timeon;
	gint *on_delta_time; /* this is time between last event and this notes start */
     	gint *duration; 
	gint *staffnum;	
}nstack;

typedef struct midicallback
{
	GList *notestack;
	GList *chordnotes;
	GList *currentnote;
	DenemoGUI *gui;
	gint leftover; /* note/rest value that is leftover across the measure */
	gint PPQN;
	gint bartime; /* time relative to barlength 0-barlength */
	gint delta_time; /* distance between notes */
	gint barlength; /* amount of time in measure */
	gint lastoff; /* starttime + duration. The time when the note is finished */
	gint trackplus;
	gint key;
	gint track;
}midicallback;

typedef struct harmonic
{
	gint pitch;
	gint enshift;
}harmonic;

gint importMidi(gchar * filename, DenemoGUI *gui);

struct harmonic enharmonic(gint input, gint key);

gint readBytes(FILE* fp, gint numb);

gint readheader(FILE* fp, midicallback *mididata);

void readtrack(FILE* fp, midicallback *mididata);

gint readVariable(FILE* fp);

void dotimesig(FILE* fp, midicallback *mididata);

void dokeysig(FILE* fp, midicallback *mididata);

void dotempo(FILE* fp,  midicallback *mididata);

void dotrackname(FILE* fp, midicallback *mididata, gint x);

void doinstrname(FILE* fp, midicallback *mididata, gint x);

void donoteon(midicallback *mididata, gint *pitchon, gint *attack, gint *timeon);

void donoteoff(midicallback *mididata, gint *pitchoff, gint *timeoff);

void restcheck(GList *tmp, midicallback *mididata);

struct notetype ConvertLength(gint endnote, midicallback *mididata);


