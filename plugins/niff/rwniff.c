#ifndef lint
/*static char rcsid[] =
"$Id: rwniff.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/*
 * Public Domain 1995,1996 Timothy Butler
 *
 * THIS DOCUMENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 */
/**************************************************************************/
/*
 * NAME
 * ====
 * rwniff - Read and Write structures defined in "niff.h"
 *
 * SYNOPSIS
 * ========
 * 
 * - NIFFIOWriteniffXXX()
 * - NIFFIOReadniffXXX()
 *
 * Where XXX is the name of a structure defined in "niff.h"
 *
 * EXAMPLES
 * ========
 *
 *| NIFFIOFile *pnf1, *pnf1;
 *| niffPart   part;
 *|
 *| NIFFIOReadniffPart(pnf1, &part);
 *| NIFFIOWriteniffPart(pnf2, &part);
 *|
 */
/**************************************************************************/

#include <assert.h>

#include <niff/niffio.h>

RIFFIOSuccess
NIFFIOWriteniffChklentabEntry(NIFFIOFile *pnf, 
                              niffChklentabEntry *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteFOURCC(pnf, p->chunkName);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteLONG(pnf, p->offsetOfFirstTag);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffChklentabEntry(NIFFIOFile *pnf,
                             niffChklentabEntry *p)
{

    RIFFIOSuccess success;
        
    success = NIFFIOReadFOURCC(pnf, &p->chunkName);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadLONG(pnf, &p->offsetOfFirstTag);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
                                                         
}

/*********************************************
 *                Setup Chunks
 *********************************************/

RIFFIOSuccess
NIFFIOWriteniffDefaultValues(NIFFIOFile *pnf, 
                             niffDefaultValues *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteFONTIDX(pnf, p->musicFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteFONTIDX(pnf, p->partNameFont);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteFONTIDX(pnf, p->lyricFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteFONTIDX(pnf, p->chordSymbolFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteFONTIDX(pnf, p->measureNumberFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteFONTIDX(pnf, p->rehearsalMarkFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->tupletGroupingSymbol);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->tupletNumberStyle);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffDefaultValues(NIFFIOFile *pnf, 
                            niffDefaultValues *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadFONTIDX(pnf, &p->musicFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadFONTIDX(pnf, &p->partNameFont);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadFONTIDX(pnf, &p->lyricFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadFONTIDX(pnf, &p->chordSymbolFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadFONTIDX(pnf, &p->measureNumberFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadFONTIDX(pnf, &p->rehearsalMarkFont);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->tupletGroupingSymbol);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->tupletNumberStyle);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOWriteniffFontDescription(NIFFIOFile *pnf, 
                               niffFontDescription *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->fontNamePtr);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->size);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->spaceHeight);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->where);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->style);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffFontDescription(NIFFIOFile *pnf, 
                              niffFontDescription *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->fontNamePtr);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->size);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->spaceHeight);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->where);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->style);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOWriteniffNiffInfo(NIFFIOFile *pnf, 
                        niffNiffInfo *p)
{
    RIFFIOSuccess success;
    long nBytesWritten;

    nBytesWritten = NIFFIOWrite(pnf, p->NIFFVersion, 8);
    if (nBytesWritten != 8)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->programType);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->standardUnits);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->absoluteUnits);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->midiClocksPerQuarter);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffNiffInfo(NIFFIOFile *pnf, 
                       niffNiffInfo *p)
{
    RIFFIOSuccess success;
    long nBytesRead;

    nBytesRead = NIFFIORead(pnf, &p->NIFFVersion, 8);
    if (nBytesRead != 8)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->programType);
    if (! success)
        return RIFFIO_FAIL;
         
    success = NIFFIOReadBYTE(pnf, &p->standardUnits);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->absoluteUnits);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->midiClocksPerQuarter);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOWriteniffPart(NIFFIOFile *pnf, 
                    niffPart *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->partID);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSTROFFSET(pnf, p->name);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSTROFFSET(pnf, p->abbreviation);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->numberOfStaves);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->midiChannel);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->midiCable);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->transpose);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffPart(NIFFIOFile *pnf, 
                   niffPart *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->partID);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSTROFFSET(pnf, &p->name);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSTROFFSET(pnf, &p->abbreviation);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->numberOfStaves);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->midiChannel);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->midiCable);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->transpose);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOWriteniffStaffGrouping(NIFFIOFile *pnf, 
                             niffStaffGrouping *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->groupingType);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->firstStaff);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->lastStaff);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOReadniffStaffGrouping(NIFFIOFile *pnf, 
                            niffStaffGrouping *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->groupingType);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->firstStaff);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->lastStaff);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/********************************************
 *              Data Section
 *              Header Chunks
 ********************************************/

RIFFIOSuccess
NIFFIOWriteniffTimeSlice(NIFFIOFile *pnf, 
                         niffTimeSlice *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->type);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteRATIONAL(pnf, p->startTime);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffTimeSlice(NIFFIOFile *pnf, 
                        niffTimeSlice *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->type);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadRATIONAL(pnf, &p->startTime);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/********************************************
 *              Data Section
 *              Symbol Chunks
 ********************************************/
/********************************************
 *              Data Section
 *              Symbol Chunks
 ********************************************/

RIFFIOSuccess
NIFFIOWriteniffAccidental(NIFFIOFile *pnf, 
                          niffAccidental *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;
}

RIFFIOSuccess
NIFFIOReadniffAccidental(NIFFIOFile *pnf, 
                         niffAccidental *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;
}


/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffAltEndingGraphic(NIFFIOFile *pnf, 
                                niffAltEndingGraphic *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->bracketShape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSTROFFSET(pnf, p->textString);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffAltEndingGraphic(NIFFIOFile *pnf, 
                               niffAltEndingGraphic *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->bracketShape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSTROFFSET(pnf, &p->textString);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffArpeggio(NIFFIOFile *pnf, 
                        niffArpeggio *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffArpeggio(NIFFIOFile *pnf, 
                       niffArpeggio *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffArticulation(NIFFIOFile *pnf, 
                            niffArticulation *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffArticulation(NIFFIOFile *pnf, 
                           niffArticulation *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffBarline(NIFFIOFile *pnf, 
                       niffBarline *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->type);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->extendsTo);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->numberOfStaves);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffBarline(NIFFIOFile *pnf, 
                      niffBarline *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->type);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->extendsTo);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->numberOfStaves);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffBeam(NIFFIOFile *pnf, 
                    niffBeam *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->beamPartsToLeft);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->beamPartsToRight);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOReadniffBeam(NIFFIOFile *pnf, 
                   niffBeam *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->beamPartsToLeft);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->beamPartsToRight);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffChordSymbol(NIFFIOFile *pnf, 
                           niffChordSymbol *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffChordSymbol(NIFFIOFile *pnf, 
                          niffChordSymbol *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}


/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffClef(NIFFIOFile *pnf, 
                    niffClef *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->octaveNumber);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffClef(NIFFIOFile *pnf, 
                   niffClef *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->octaveNumber);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffCustomGraphicChk(NIFFIOFile *pnf, 
                                niffCustomGraphicChk *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->value);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffCustomGraphicChk(NIFFIOFile *pnf, 
                               niffCustomGraphicChk *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->value);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffDynamic(NIFFIOFile *pnf, 
                       niffDynamic *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->code);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffDynamic(NIFFIOFile *pnf, 
                      niffDynamic *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->code);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFiguredBass(NIFFIOFile *pnf, 
                           niffFiguredBass *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffFiguredBass(NIFFIOFile *pnf, 
                          niffFiguredBass *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFingering(NIFFIOFile *pnf, 
                         niffFingering *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffFingering(NIFFIOFile *pnf, 
                        niffFingering *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffGuitarGrid(NIFFIOFile *pnf, 
                          niffGuitarGrid *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberOfFrets);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->numberOfStrings);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSTROFFSET(pnf, p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffGuitarGrid(NIFFIOFile *pnf, 
                         niffGuitarGrid *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberOfFrets);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->numberOfStrings);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSTROFFSET(pnf, &p->textDescription);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffGuitarTabNum(NIFFIOFile *pnf, 
                            niffGuitarTabNum *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->number);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffGuitarTabNum(NIFFIOFile *pnf, 
                           niffGuitarTabNum *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->number);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffHairpin(NIFFIOFile *pnf, 
                       niffHairpin *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->direction);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffHairpin(NIFFIOFile *pnf, 
                      niffHairpin *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->direction);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffHarpPedal(NIFFIOFile *pnf, 
                         niffHarpPedal *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->pedalPositions);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffHarpPedal(NIFFIOFile *pnf, 
                        niffHarpPedal *p)
{ 
    RIFFIOSuccess success;
 
    success = NIFFIOReadSTROFFSET(pnf, &p->pedalPositions);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffKeySignature(NIFFIOFile *pnf, 
                            niffKeySignature *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->standardCode);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffKeySignature(NIFFIOFile *pnf, 
                           niffKeySignature *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->standardCode);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffKeySignNonstandard(NIFFIOFile *pnf, 
                                  niffKeySignNonstandard *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffKeySignNonstandard(NIFFIOFile *pnf, 
                                 niffKeySignNonstandard *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffLine(NIFFIOFile *pnf, 
                    niffLine *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffLine(NIFFIOFile *pnf, 
                   niffLine *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffLyric(NIFFIOFile *pnf, 
                     niffLyric *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->text);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->lyricVerseID);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffLyric(NIFFIOFile *pnf, 
                    niffLyric *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->text);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->lyricVerseID);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffMeasureNumbering(NIFFIOFile *pnf, 
                                niffMeasureNumbering *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberWhichMeasures);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->numberFrequency);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->startingNumber);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteFONTIDX(pnf, p->fontID);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->aboveOrBelow);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->horizontalCentering);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->enclosure);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffMeasureNumbering(NIFFIOFile *pnf, 
                               niffMeasureNumbering *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberWhichMeasures);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->numberFrequency);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->startingNumber);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadFONTIDX(pnf, &p->fontID);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->aboveOrBelow);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->horizontalCentering);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->enclosure);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/*
 * !!! Watch out
 * MidiDataStream will change in NIFF 6b
 */ 
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffMidiDataStream(NIFFIOFile *pnf, 
                              niffMidiDataStream *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->startTime);
    if (! success)
        return RIFFIO_FAIL;

#if 0
    success = NIFFIOWriteVARLEN(pnf, p->value);
    if (! success)
        return RIFFIO_FAIL;
#endif
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffMidiDataStream(NIFFIOFile *pnf, 
                             niffMidiDataStream *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->startTime);
    if (! success)
        return RIFFIO_FAIL;

#if 0
    success = NIFFIOReadVARLEN(pnf, &p->value);
    if (! success)
        return RIFFIO_FAIL;
#endif
    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFontSymbol(NIFFIOFile *pnf, 
                          niffFontSymbol *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOWriteFOURCC(pnf, p->chunkType);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSHORT(pnf, p->spaceHeight);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffFontSymbol(NIFFIOFile *pnf, 
                         niffFontSymbol *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadFOURCC(pnf, &p->chunkType);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSHORT(pnf, &p->spaceHeight);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffNotehead(NIFFIOFile *pnf,
                        niffNotehead *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteRATIONAL(pnf, p->duration);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffNotehead(NIFFIOFile *pnf, 
                       niffNotehead *p)
{ 
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->staffStep);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadRATIONAL(pnf, &p->duration);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffOctaveSign(NIFFIOFile *pnf,
                          niffOctaveSign *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberOfOctaves);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->aboveOrBelow);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteBYTE(pnf, p->type);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOWriteSTROFFSET(pnf, p->textString);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffOctaveSign(NIFFIOFile *pnf,
                         niffOctaveSign *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberOfOctaves);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->aboveOrBelow);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadBYTE(pnf, &p->type);
    if (! success)
        return RIFFIO_FAIL;

    success = NIFFIOReadSTROFFSET(pnf, &p->textString);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffOrnament(NIFFIOFile *pnf,
                        niffOrnament *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffOrnament(NIFFIOFile *pnf,
                       niffOrnament *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffParenthesis(NIFFIOFile *pnf,
                           niffParenthesis *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffParenthesis(NIFFIOFile *pnf,
                          niffParenthesis *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffPedalPiano(NIFFIOFile *pnf,
                          niffPedalPiano *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffPedalPiano(NIFFIOFile *pnf,
                         niffPedalPiano *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffPedalOrgan(NIFFIOFile *pnf,
                          niffPedalOrgan *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffPedalOrgan(NIFFIOFile *pnf,
                         niffPedalOrgan *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRehearsalMark(NIFFIOFile *pnf,
                             niffRehearsalMark *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->textString);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->enclosure);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRehearsalMark(NIFFIOFile *pnf,
                            niffRehearsalMark *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->textString);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->enclosure);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRepeatSign(NIFFIOFile *pnf,
                          niffRepeatSign *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->graphicalCode);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->logicalCode);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRepeatSign(NIFFIOFile *pnf,
                         niffRepeatSign *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->graphicalCode);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->logicalCode);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRest(NIFFIOFile *pnf,
                    niffRest *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->staffStep);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteRATIONAL(pnf, p->duration);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRest(NIFFIOFile *pnf,
                   niffRest *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->shape);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->staffStep);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadRATIONAL(pnf, &p->duration);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffSystemSeparation(NIFFIOFile *pnf,
                                niffSystemSeparation *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->where);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffSystemSeparation(NIFFIOFile *pnf,
                               niffSystemSeparation *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->where);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTempoMarking(NIFFIOFile *pnf,
                            niffTempoMarking *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->textString);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteRATIONAL(pnf, p->noteValue);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->beatsPerMinute);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTempoMarking(NIFFIOFile *pnf,
                           niffTempoMarking *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->textString);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadRATIONAL(pnf, &p->noteValue);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->beatsPerMinute);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTempoMarkNonstandard(NIFFIOFile *pnf,
                                    niffTempoMarkNonstandard *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTempoMarkNonstandard(NIFFIOFile *pnf,
                                   niffTempoMarkNonstandard *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffText(NIFFIOFile *pnf,
                    niffText *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSTROFFSET(pnf, p->value);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffText(NIFFIOFile *pnf,
                   niffText *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSTROFFSET(pnf, &p->value);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTimeSignature(NIFFIOFile *pnf,
                             niffTimeSignature *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSIGNEDBYTE(pnf, p->topNumber);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->bottomNumber);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTimeSignature(NIFFIOFile *pnf,
                            niffTimeSignature *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSIGNEDBYTE(pnf, &p->topNumber);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->bottomNumber);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTimeSigNonstandard(NIFFIOFile *pnf,
                                  niffTimeSigNonstandard *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTimeSigNonstandard(NIFFIOFile *pnf,
                                 niffTimeSigNonstandard *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->numberOfChunks);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTremolo(NIFFIOFile *pnf,
                       niffTremolo *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteBYTE(pnf, p->attachedBeamParts);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->unattachedBeamParts);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTremolo(NIFFIOFile *pnf,
                      niffTremolo *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadBYTE(pnf, &p->attachedBeamParts);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->unattachedBeamParts);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}

/***********************************************
 *                   Tags
 ***********************************************/

RIFFIOSuccess
NIFFIOWriteniffAbsPlacement(NIFFIOFile *pnf,
                            niffAbsPlacement *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOReadniffAbsPlacement(NIFFIOFile *pnf,
                           niffAbsPlacement *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffAltEnding(NIFFIOFile *pnf,
                         niffAltEnding *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffAltEnding(NIFFIOFile *pnf,
                        niffAltEnding *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffAnchorOverride(NIFFIOFile *pnf,
                              niffAnchorOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteFOURCC(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffAnchorOverride(NIFFIOFile *pnf,
                             niffAnchorOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadFOURCC(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffArticDirection(NIFFIOFile *pnf,
                              niffArticDirection *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffArticDirection(NIFFIOFile *pnf,
                             niffArticDirection *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffBezierIncoming(NIFFIOFile *pnf,
                              niffBezierIncoming *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
        
}
RIFFIOSuccess
NIFFIOReadniffBezierIncoming(NIFFIOFile *pnf,
                             niffBezierIncoming *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffBezierOutgoing(NIFFIOFile *pnf,
                              niffBezierOutgoing *p)
{
    RIFFIOSuccess success;

    success = NIFFIOWriteSHORT(pnf, p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSHORT(pnf, p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
        
}
RIFFIOSuccess
NIFFIOReadniffBezierOutgoing(NIFFIOFile *pnf,
                             niffBezierOutgoing *p)
{
    RIFFIOSuccess success;

    success = NIFFIOReadSHORT(pnf, &p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSHORT(pnf, &p->vertical);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffChordSymbolsOffset(NIFFIOFile *pnf,
                                  niffChordSymbolsOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffChordSymbolsOffset(NIFFIOFile *pnf,
                                 niffChordSymbolsOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffCustomFontChar(NIFFIOFile *pnf,
                              niffCustomFontChar *p)
{
    RIFFIOSuccess success;
    long nBytesWritten;

    success = NIFFIOWriteFONTIDX(pnf, p->fontID);
    if (! success)
        return RIFFIO_FAIL;
        
    nBytesWritten = NIFFIOWrite(pnf, &p->characterCode, 2);
    if (nBytesWritten != 2)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
        
}
RIFFIOSuccess
NIFFIOReadniffCustomFontChar(NIFFIOFile *pnf,
                             niffCustomFontChar *p)
{
    RIFFIOSuccess success;
    long nBytesRead;

    success = NIFFIOReadFONTIDX(pnf, &p->fontID);
    if (! success)
        return RIFFIO_FAIL;
        
    nBytesRead = NIFFIORead(pnf, &p->characterCode, 2);
    if (nBytesRead != 2)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
        
}

/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffCustomGraphicTag(NIFFIOFile *pnf,
                                niffCustomGraphicTag *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffCustomGraphicTag(NIFFIOFile *pnf,
                               niffCustomGraphicTag *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFannedBeam(NIFFIOFile *pnf,
                          niffFannedBeam *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffFannedBeam(NIFFIOFile *pnf,
                         niffFannedBeam *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFigBassOffset(NIFFIOFile *pnf,
                             niffFigBassOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffFigBassOffset(NIFFIOFile *pnf,
                            niffFigBassOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffFontID(NIFFIOFile *pnf,
                      niffFontID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteFONTIDX(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffFontID(NIFFIOFile *pnf,
                     niffFontID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadFONTIDX(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffGraceNote(NIFFIOFile *pnf,
                         niffGraceNote *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteRATIONAL(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffGraceNote(NIFFIOFile *pnf,
                        niffGraceNote *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadRATIONAL(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffGuitarGridOffset(NIFFIOFile *pnf,
                                niffGuitarGridOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffGuitarGridOffset(NIFFIOFile *pnf,
                               niffGuitarGridOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffHeight(NIFFIOFile *pnf,
                      niffHeight *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffHeight(NIFFIOFile *pnf,
                     niffHeight *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffID(NIFFIOFile *pnf,
                  niffID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffID(NIFFIOFile *pnf,
                 niffID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffLineQuality(NIFFIOFile *pnf,
                           niffLineQuality *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffLineQuality(NIFFIOFile *pnf,
                          niffLineQuality *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffLogicalPlacement(NIFFIOFile *pnf,
                                niffLogicalPlacement *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->vertical);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->proximity);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffLogicalPlacement(NIFFIOFile *pnf,
                               niffLogicalPlacement *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, &p->horizontal);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->vertical);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->proximity);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffLyricVerseOffset(NIFFIOFile *pnf,
                                niffLyricVerseOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, p->lyricLineOffset);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->lyricVerseID);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffLyricVerseOffset(NIFFIOFile *pnf,
                               niffLyricVerseOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, &p->lyricLineOffset);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->lyricVerseID);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffMidiPerformance(NIFFIOFile *pnf,
                               niffMidiPerformance *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteLONG(pnf, p->startTime);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteLONG(pnf, p->duration);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->pitch);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->velocity);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffMidiPerformance(NIFFIOFile *pnf,
                              niffMidiPerformance *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadLONG(pnf, &p->startTime);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadLONG(pnf, &p->duration);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->pitch);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->velocity);
    if (! success)
        return RIFFIO_FAIL;
        

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffNumberOfFlags(NIFFIOFile *pnf,
                             niffNumberOfFlags *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffNumberOfFlags(NIFFIOFile *pnf,
                            niffNumberOfFlags *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffNumberOfNodes(NIFFIOFile *pnf,
                             niffNumberOfNodes *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffNumberOfNodes(NIFFIOFile *pnf,
                            niffNumberOfNodes *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffNumStaffLines(NIFFIOFile *pnf,
                             niffNumStaffLines *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffNumStaffLines(NIFFIOFile *pnf,
                            niffNumStaffLines *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffOssia(NIFFIOFile *pnf,
                     niffOssia *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffOssia(NIFFIOFile *pnf,
                    niffOssia *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffPartDescOverride(NIFFIOFile *pnf,
                                niffPartDescOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->MIDIchannel);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->MIDIcable);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, p->transpose);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffPartDescOverride(NIFFIOFile *pnf,
                               niffPartDescOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->MIDIchannel);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->MIDIcable);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, &p->transpose);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffPartID(NIFFIOFile *pnf,
                      niffPartID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffPartID(NIFFIOFile *pnf,
                     niffPartID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRefPtOverride(NIFFIOFile *pnf,
                             niffRefPtOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, p->anchor_h);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->dependent_h);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->anchor_v);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->dependent_v);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRefPtOverride(NIFFIOFile *pnf,
                            niffRefPtOverride *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, &p->anchor_h);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->dependent_h);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->anchor_v);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->dependent_v);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRehearsalOffset(NIFFIOFile *pnf,
                               niffRehearsalOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRehearsalOffset(NIFFIOFile *pnf,
                              niffRehearsalOffset *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffRestNumeral(NIFFIOFile *pnf,
                           niffRestNumeral *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffRestNumeral(NIFFIOFile *pnf,
                          niffRestNumeral *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffStaffStep(NIFFIOFile *pnf,
                         niffStaffStep *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSIGNEDBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffStaffStep(NIFFIOFile *pnf,
                        niffStaffStep *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSIGNEDBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffThickness(NIFFIOFile *pnf,
                         niffThickness *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffThickness(NIFFIOFile *pnf,
                        niffThickness *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTieDirection(NIFFIOFile *pnf,
                            niffTieDirection *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTieDirection(NIFFIOFile *pnf,
                           niffTieDirection *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffTupletDesc(NIFFIOFile *pnf,
                          niffTupletDesc *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteRATIONAL(pnf, p->transformRatioAB);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteRATIONAL(pnf, p->transformRatioCD);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOWriteBYTE(pnf, p->groupingSymbol);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffTupletDesc(NIFFIOFile *pnf,
                         niffTupletDesc *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadRATIONAL(pnf, &p->transformRatioAB);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadRATIONAL(pnf, &p->transformRatioCD);
    if (! success)
        return RIFFIO_FAIL;
        
    success = NIFFIOReadBYTE(pnf, &p->groupingSymbol);
    if (! success)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffNumberStyle(NIFFIOFile *pnf,
                           niffNumberStyle *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteBYTE(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffNumberStyle(NIFFIOFile *pnf,
                          niffNumberStyle *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadBYTE(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffVoiceID(NIFFIOFile *pnf,
                       niffVoiceID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffVoiceID(NIFFIOFile *pnf,
                      niffVoiceID *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
/***********************************************************/
RIFFIOSuccess
NIFFIOWriteniffWidth(NIFFIOFile *pnf,
                     niffWidth *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOWriteSHORT(pnf, *p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
RIFFIOSuccess
NIFFIOReadniffWidth(NIFFIOFile *pnf,
                    niffWidth *p)
{
    RIFFIOSuccess success;
        
    success = NIFFIOReadSHORT(pnf, p);
    if (! success)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


