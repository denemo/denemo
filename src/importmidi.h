/* importmidi.h

AJAnderson

*/

#include <gtk/gtk.h>
#include <stdio.h>

typedef struct notetype
{
	gint notetype;
	gint tied;
}notetype;

typedef struct nstack 
{
	gint pitch;
   	gint measure;
     	gint timeon;
     	gint duration;  
}nstack;

typedef struct midicallback
{
	GList *notestack;
	GList *final_list;
	GList *chordnotes;
	GList *currentnote;
	DenemoGUI *gui;
	gint leftover;
	gint PPQN;
	gint bartime;
	gint barlength;
	gint lastoff;
	gint trackplus;
	gint key;
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

void donoteon(midicallback *mididata, gint pitchon, gint attack, gint timeon);

void donoteoff(midicallback *mididata, gint pitchoff, gint timeoff);

void restcheck(GList *tmp, midicallback *mididata);

struct notetype ConvertLength(gint endnote, midicallback *mididata);
