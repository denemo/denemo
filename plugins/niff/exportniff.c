/* exportniff.c
 * Functions for actually exporting what Denemo's working on to a mudela file
 *
 * AJT 14/3/2000 Parametised for quick midi playback
 * for Denemo, a gtk+ frontend to GNU Lilypond
 * (c) 2000, 2001, 2002 Matthew Hiller, Adam Tee
 */

/* Yes -- if you're curious, this is a very straightforward adaptation
 * of what I used to export mudela in my extensions to kooBase. */

#include "config.h"
#include <denemo/denemo.h>
#include "exportniff.h"
#include "utils.h"
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>






extern void doerror(const char *strMessage);
void MyWriteStringTable(void);

/* Actually export the niff */
extern "C" {
void
exportniff (gchar * thefilename, struct scoreinfo *si, gint start, gint end)
{
	 staff *curstaffstruct;
	measurenode *curmeasure;
	objnode *curobjnode;
  	mudelaobject *curobj;
    chord chordval;
	GList *curtone;
	int duration, numdots, noteheadtype;
	int beatposn;
	int beatvalue;
	int measureposn;
	int measurevalue;   //might need this is if time signature changes
	int clefoffset;
	int step;
	int shape;
	int length;
	unsigned char version[8] = "6b";

  GString *filename = g_string_new (thefilename);

  /* Append .nif onto the filename if necessary */
  if (strcmp (filename->str + filename->len - 4, ".nif"))
    g_string_append (filename, ".nif");

  /* Now open the file */
  FILE *fp = fopen (filename->str, "wb");
	if (!fp)
        doerror("Can't open that file");
        
	 NIFFIOStorage  *pstore =  new NIFFIOStorage(fp);
    if (!pstore)
        doerror("Can't create new NIFFIOStorage");
     NIFFIOStorageSetCurrent(pstore);

 /*   Begin writing to file.  Start with NIFF form. */
    if (! NIFFIOStartNiff())
        doerror ("NIFFIOStoreFormNiff failed");


    /*   Setup Section */
    if (! NIFFIOStartSetupSection())
        doerror ("NIFFIOStartSetupSection failed");


    /*  Special Chunk Length Table routine */
    if (! NIFFIOStoreDefaultCLT())
        doerror ("NIFFIOStoreDefaultCLT failed");

    /*  Other Setup Section chunks and lists */

  if (! NIFFIOchunkNiffInfo(version, progtypeNone, 0, -1 ,-1))
        doerror ("NIFFIOStoreListSetupSection failed");

    if (! NIFFIOStartParts())
        doerror ("NIFFIOStartParts failed");

//start cheating here - one part, one staff
    if (! NIFFIOchunkPart(0,0,0,1,-1,-1,-1))
        doerror ("NIFFIOchunkPart failed");
   
    NIFFIOEndParts();

  /*  if (! NIFFIOchunkStringTable())
        doerror ("NIFFIOStringTable failed");*/

    //MyWriteStringTable();

    NIFFIOEndSetupSection(); /* SetupSection */


	if(! NIFFIOStartDataSection())
		doerror("data list failed");

//new page
	if(! NIFFIOStartPage())
		doerror("page list failed");
     if(! NIFFIOchunkPageHeader())
		doerror("page header failed");
	dowidth(10944);
	doheight(14544);

//new system
     NIFFIOStartSystem();
	NIFFIOchunkSystemHeader();
	doabsolute(400,1800);
	dowidth(10400);
	doheight(400);

//new staff
     NIFFIOStartStaff();
	NIFFIOchunkStaffHeader();
//lime top staff is anchored to the page header
    doabsolute(400,1800);
    dowidth(10400);
    doheight(400);
    dostafflines(5);

      curstaffstruct = (staff *)si->thescore->data;

//start with clefs etc.
	measureposn = 0;
	measurevalue = curstaffstruct->stime2;
	beatposn = 0;
	beatvalue = curstaffstruct->stime2;
	NIFFIOchunkTimeSlice((BYTE)1,makerational(measureposn,measurevalue));
	 clefoffset = determineclef (curstaffstruct->sclef);
     NIFFIOchunkTimeSignature((SIGNEDBYTE)curstaffstruct->stime1,(BYTE)curstaffstruct->stime2);

     curmeasure = curstaffstruct->measures;
 
      for (; curmeasure ; curmeasure = curmeasure->next)
	{
		NIFFIOchunkTimeSlice((BYTE)1,makerational(measureposn,measurevalue));
		for (curobjnode = (objnode *)curmeasure->data; curobjnode;
	       	curobjnode = curobjnode->next)
	    {
	      	curobj = (mudelaobject *)curobjnode->data;
	      	switch (curobj->type)
			{
					case CHORD:
		  		chordval = *(chord *)curobj->object;
				//some function returning a RATIONAL from durationa and numdots
		  		/*duration = internaltomuduration (chordval.baseduration);
		  		numdots = chordval.numdots;  */
				duration =4;
				NIFFIOchunkTimeSlice((BYTE)2,makerational(beatposn,beatvalue));    //onset
				//crotchets oonly to start with
				/*if(duration>basevalue)
				{
					beatposn = beatposn*duration/beatvalue;
					beatvalue = duration;
                       length = 1;
				};
				if(numdots>0)
				{
					beatvalue = beatvalue*numdots;
					beatposn = beatposn*numdots;
					length = powf(2, (numdots+1)) -1;
				}; */
				  
		  		if (!chordval.tones)
		    		{		/* A rest */
		        	   // NIFFIOchunkRest(.........)
		    		}
		  		else
		    		{
					NIFFIOchunkStem();
		       		for (curtone = chordval.tones; curtone;
			   			curtone = curtone->next)
					{
			  			noteheadtype = ((note *) curtone->data)->noteheadtype;
                            switch (noteheadtype)
			    			{
			    				case DENEMO_NORMAL_NOTEHEAD:
                                 shape =4;
							 if(duration<= 4)
								shape = duration;
			      			break;
			    				case DENEMO_CROSS_NOTEHEAD:
							shape = 7;
							if(duration<4)
								shape = 8;			      
			      			break;
			    				case DENEMO_HARMONIC_NOTEHEAD:
			      
			      			break;
			    				case DENEMO_DIAMOND_NOTEHEAD:
			     
			      			break;
			    				default:
			     
			      			break;
			    			}
						step =  ((note *) curtone->data)->mid_c_offset + clefoffset;
						NIFFIOchunkNotehead((BYTE)shape,(SIGNEDBYTE)step,makerational(1,beatvalue));
			  			
			  		}	/* End chord loop */
					beatposn = beatposn + 1;     //still assuming crotchets
				}
				break;
					case CLEF:
		          //problem with clef change in denemo
				NIFFIOchunkTimeSlice((BYTE)2,makerational(beatposn,beatvalue));       
		 		clefoffset = determineclef (((clef *)curobj->object)->type);
				dosmall();
		  		break;
					case KEYSIG:
		 
		  		break;
					case TIMESIG:
				//pretend this won't happen
					case TUPOPEN:
		 
		  		break;
					case TUPCLOSE:
		  
		  		break;
					case GRACE_START:
		  
		  		break;
					case GRACE_END:

				break;
					case STEMDIRECTIVE:
		  		
		  		break;
					case DYNAMIC:
		 
		  		break;
					case LYRIC:

		  		break;
					default:

		  		break;
			};
	    	};			/* Done with this measure */
	  	if (curmeasure->next)
	   		{
				NIFFIOchunkTimeSlice((BYTE)2,makerational(beatposn,beatvalue));
			 	NIFFIOchunkBarline((BYTE)1, (BYTE)1,(SHORT)1);
				measureposn = measureposn + beatposn;
				beatposn = 0;
			}
		
	  	else
	   		NIFFIOEndStaff();
	};			/* Done with this staff */

    NIFFIOEndSystem();

    NIFFIOEndPage();

    NIFFIOEndDataSection();

    NIFFIOEndNiff();

    NIFFIOStorageDelete(pstore);

    fclose(fp);

}
}
void
doerror(const char *strMessage)
{
        fprintf(stderr, "%s\n", strMessage);
        exit(1);
}

void doheight(int height)
{
 	NIFFIOtagHeight((niffHeight)height);
}

void dowidth(int width)
{
	NIFFIOtagWidth((niffWidth)width);
}

void doabsolute(int horz, int vert)
{
	NIFFIOtagAbsPlacement((SHORT)horz,(SHORT)vert);
}

void dostafflines(int lines)
{
	NIFFIOtagNumStaffLines((niffNumStaffLines)lines);
}

RATIONAL makerational(int numint, int denint)
{
	RATIONAL compound;
     compound.numerator = (SHORT)numint;
     compound.denominator = (SHORT)denint;
	return compound;
}

int determineclef (gint type)
{
  switch (type)
    {
    case DENEMO_TREBLE_CLEF:
     NIFFIOchunkClef((BYTE)1,(SIGNEDBYTE)2,(BYTE)0);
	return -2;
      break;
    case DENEMO_BASS_CLEF:
      NIFFIOchunkClef((BYTE)2,(SIGNEDBYTE)6,(BYTE)0);
     return 10;
	 break;
    case DENEMO_ALTO_CLEF:
      NIFFIOchunkClef((BYTE)3,(SIGNEDBYTE)4,(BYTE)0);
	  return 4;
      break;
    case DENEMO_G_8_CLEF:
      NIFFIOchunkClef((BYTE)1,(SIGNEDBYTE)2,(BYTE)1);
	  return -9;
      break;
    case DENEMO_TENOR_CLEF:
      NIFFIOchunkClef((BYTE)3,(SIGNEDBYTE)6,(BYTE)0);
	  return 6;
      break;
    case DENEMO_SOPRANO_CLEF:
     NIFFIOchunkClef((BYTE)1,(SIGNEDBYTE)2,(BYTE)1);
	  return 0;
      break;
    default:
      NIFFIOchunkClef((BYTE)1,(SIGNEDBYTE)2,(BYTE)0);
	return -2;
      break;
    }
}

void dosmall(void)
{
	NIFFIOtagSmallSize();
}




