#ifndef lint
/*static char rcsid[] = "$Header: /sources/denemo/denemo/plugins/niff/storenif.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/***************************************************************************/
/*
 * NAME
 * ====
 * storenif - Handy routines for creating NIFF lists, chunks, and tags.
 *
 * SYNOPSIS
 * ========
 * 
 * - NIFFIOStartXXX()
 * - NIFFIOEndXXX()
 *
 * - NIFFIOchunkXXX()
 * - NIFFIOtagXXX()
 * 
 * DESCRIPTION
 * ===========
 * Need a description here. 
 */
/***************************************************************************/

#include <assert.h>
#include <string.h>
#include <stdlib.h>

#include <niff.h>
#include <niffio.h>


/***************************
 * Form
 ****************************/

RIFFIOSuccess
NIFFIOStartNiff(void)
{
        RIFFIOChunk *formChunk = new RIFFIOChunk(RIFFIO_FOURCC_RIFX,niffformNiff,0);

        return NIFFIOStorageListStart(formChunk);
}

RIFFIOSuccess
NIFFIOEndNiff(void)
{
       assert (NIFFIOStoragePendingList()->fccType ==    niffformNiff);
        
                        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
    assert(! NIFFIOStorageIsListPending()); /* no more lists pending */
        return RIFFIO_OK;
}

/***************************
 * Lists
 ****************************/

RIFFIOSuccess
NIFFIOStartCustomGraphics(void)
{
    
        RIFFIOChunk chunkList;

        chunkList.fccId = RIFFIO_FOURCC_LIST;
        chunkList.fccType = nifflistCustomGraphics;

        if (! NIFFIOStorageListStart(&chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndCustomGraphics(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistCustomGraphics);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartDataSection(void)
{
    
        RIFFIOChunk *chunkList =new RIFFIOChunk(RIFFIO_FOURCC_LIST,nifflistDataSection);
   
        if (! NIFFIOStorageListStart(chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndDataSection(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistDataSection);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartFontDescs(void)
{
    
        RIFFIOChunk chunkList;

        chunkList.fccId = RIFFIO_FOURCC_LIST;
        chunkList.fccType = nifflistFontDescs;

        if (! NIFFIOStorageListStart(&chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndFontDescs(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistFontDescs);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartGroupings(void)
{
    
        RIFFIOChunk chunkList;

        chunkList.fccId = RIFFIO_FOURCC_LIST;
        chunkList.fccType = nifflistGroupings;

        if (! NIFFIOStorageListStart(&chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndGroupings(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistGroupings);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartPage(void)
{
    
        RIFFIOChunk *chunkList = new RIFFIOChunk(RIFFIO_FOURCC_LIST,nifflistPage);
        
        if (! NIFFIOStorageListStart(chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndPage(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistPage);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartParts(void)
{
    
        RIFFIOChunk *chunkList = new RIFFIOChunk(RIFFIO_FOURCC_LIST,nifflistParts);

        if (! NIFFIOStorageListStart(chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndParts(void)
{
       assert (NIFFIOStoragePendingList()->fccType == nifflistParts);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartRiffInfo(void)
{
    
        RIFFIOChunk chunkList;

        chunkList.fccId = RIFFIO_FOURCC_LIST;
        chunkList.fccType = nifflistRiffInfo;

        if (! NIFFIOStorageListStart(&chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndRiffInfo(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistRiffInfo);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartSetupSection(void)
{
    
        RIFFIOChunk *setupChunk = new RIFFIOChunk(RIFFIO_FOURCC_LIST,nifflistSetupSection);

        if (! NIFFIOStorageListStart(setupChunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndSetupSection(void)
{
	  assert (NIFFIOStoragePendingList()->fccType ==  nifflistSetupSection);
         
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartStaff(void)
{
    
        RIFFIOChunk *chunkList =new RIFFIOChunk(RIFFIO_FOURCC_LIST, nifflistStaff);

        if (! NIFFIOStorageListStart(chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndStaff(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistStaff);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}


RIFFIOSuccess
NIFFIOStartSystem(void)
{
    
        RIFFIOChunk *chunkList = new RIFFIOChunk(RIFFIO_FOURCC_LIST, nifflistSystem);

        if (! NIFFIOStorageListStart(chunkList))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOEndSystem(void)
{
        assert (NIFFIOStoragePendingList()->fccType ==
                        nifflistSystem);
        
        if (!NIFFIOStorageListEnd())
                return RIFFIO_FAIL;
        
        return RIFFIO_OK;
}

/***************************
 * Raw chunks
 ****************************/

RIFFIOSuccess
NIFFIOchunkAugDot(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidAugDot;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkChnkLenTable(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidChnkLenTable);


         if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkEpsGraphic(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidEpsGraphic;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkGlissando(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidGlissando;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkPageHeader(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidPageHeader);

        if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkPortamento(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidPortamento;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkPsType1Font(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidPsType1Font;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkPsType3Font(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidPsType3Font;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkSlur(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidSlur;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkStaffHeader(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidStaffHeader);

        if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkStem(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidStem);

        if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkStringTable(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidStringTable);

        if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkSystemHeader(void)
{
    
        RIFFIOChunk *chunk = new RIFFIOChunk(niffckidSystemHeader);

        if (! NIFFIOStorageChunkStart(chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkTagActivate(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidTagActivate;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkTagInactivate(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidTagInactivate;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkTie(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidTie;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOchunkTuplet(void)
{
    
        RIFFIOChunk chunk;


        chunk.fccId = niffckidTuplet;

        if (! NIFFIOStorageChunkStart(&chunk))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

/***************************
 * Cooked chunks
 ****************************/
RIFFIOSuccess
NIFFIOchunkAccidental(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffAccidental Accidental;

    /* Initialize the chunk's structure */
    Accidental.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidAccidental;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffAccidental(NIFFIOStorageGetFile(), 
                                     &Accidental))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkAltEndingGraphic(
    BYTE bracketShape  ,
    STROFFSET textString)
{
    
    RIFFIOChunk chunk;
    niffAltEndingGraphic AltEndingGraphic;

    /* Initialize the chunk's structure */
    AltEndingGraphic.bracketShape = bracketShape;
    AltEndingGraphic.textString = textString;

    /* Write the chunk header */
    chunk.fccId = niffckidAltEndingGraphic;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffAltEndingGraphic(NIFFIOStorageGetFile(), 
                                     &AltEndingGraphic))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkArpeggio(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffArpeggio Arpeggio;

    /* Initialize the chunk's structure */
    Arpeggio.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidArpeggio;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffArpeggio(NIFFIOStorageGetFile(), 
                                     &Arpeggio))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkArticulation(
    SHORT shape)
{
    
    RIFFIOChunk chunk;
    niffArticulation Articulation;

    /* Initialize the chunk's structure */
    Articulation.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidArticulation;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffArticulation(NIFFIOStorageGetFile(), 
                                     &Articulation))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkBarline(
    BYTE type  ,
    BYTE extendsTo  ,
    SHORT numberOfStaves)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidBarline);
    niffBarline *Barline = new niffBarline(type, extendsTo, numberOfStaves);

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffBarline(NIFFIOStorageGetFile(), 
                                     Barline))
		delete (Barline);
        return RIFFIO_FAIL;

	delete (Barline);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkBeam(
    BYTE beamPartsToLeft  ,
    BYTE beamPartsToRight)
{
    
    RIFFIOChunk chunk;
    niffBeam Beam;

    /* Initialize the chunk's structure */
    Beam.beamPartsToLeft = beamPartsToLeft;
    Beam.beamPartsToRight = beamPartsToRight;

    /* Write the chunk header */
    chunk.fccId = niffckidBeam;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffBeam(NIFFIOStorageGetFile(), 
                                     &Beam))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkChordSymbol(
    STROFFSET textDescription)
{
    
    RIFFIOChunk chunk;
    niffChordSymbol ChordSymbol;

    /* Initialize the chunk's structure */
    ChordSymbol.textDescription = textDescription;

    /* Write the chunk header */
    chunk.fccId = niffckidChordSymbol;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffChordSymbol(NIFFIOStorageGetFile(), 
                                     &ChordSymbol))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkClef(
    BYTE shape  ,
    SIGNEDBYTE staffStep  ,
    BYTE octaveNumber)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidClef);
    niffClef *Clef = new niffClef(shape,staffStep,octaveNumber);

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffClef(NIFFIOStorageGetFile(), 
                                     Clef))
		delete (Clef);
        return RIFFIO_FAIL;

    delete (Clef);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkCustomGraphicChk(
    SHORT value)
{
    
    RIFFIOChunk chunk;
    niffCustomGraphicChk CustomGraphicChk;

    /* Initialize the chunk's structure */
    CustomGraphicChk.value = value;

    /* Write the chunk header */
    chunk.fccId = niffckidCustomGraphicChk;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffCustomGraphicChk(NIFFIOStorageGetFile(), 
                                     &CustomGraphicChk))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkDefaultValues(
    FONTIDX musicFont  ,
    FONTIDX partNameFont  ,
    FONTIDX lyricFont  ,
    FONTIDX chordSymbolFont  ,
    FONTIDX measureNumberFont  ,
    FONTIDX rehearsalMarkFont  ,
    BYTE tupletGroupingSymbol  ,
    BYTE tupletNumberStyle)
{
    
    RIFFIOChunk chunk;
    niffDefaultValues DefaultValues;

    /* Initialize the chunk's structure */
    DefaultValues.musicFont = musicFont;
    DefaultValues.partNameFont = partNameFont;
    DefaultValues.lyricFont = lyricFont;
    DefaultValues.chordSymbolFont = chordSymbolFont;
    DefaultValues.measureNumberFont = measureNumberFont;
    DefaultValues.rehearsalMarkFont = rehearsalMarkFont;
    DefaultValues.tupletGroupingSymbol = tupletGroupingSymbol;
    DefaultValues.tupletNumberStyle = tupletNumberStyle;

    /* Write the chunk header */
    chunk.fccId = niffckidDefaultValues;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffDefaultValues(NIFFIOStorageGetFile(), 
                                     &DefaultValues))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkDynamic(
    BYTE code)
{
    
    RIFFIOChunk chunk;
    niffDynamic Dynamic;

    /* Initialize the chunk's structure */
    Dynamic.code = code;

    /* Write the chunk header */
    chunk.fccId = niffckidDynamic;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffDynamic(NIFFIOStorageGetFile(), 
                                     &Dynamic))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkFiguredBass(
    STROFFSET textDescription)
{
    
    RIFFIOChunk chunk;
    niffFiguredBass FiguredBass;

    /* Initialize the chunk's structure */
    FiguredBass.textDescription = textDescription;

    /* Write the chunk header */
    chunk.fccId = niffckidFiguredBass;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffFiguredBass(NIFFIOStorageGetFile(), 
                                     &FiguredBass))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkFingering(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffFingering Fingering;

    /* Initialize the chunk's structure */
    Fingering.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidFingering;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffFingering(NIFFIOStorageGetFile(), 
                                     &Fingering))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkFontDescription(
    STROFFSET fontNamePtr  ,
    SHORT size  ,
    SHORT spaceHeight  ,
    SHORT where  ,
    BYTE style)
{
    
    RIFFIOChunk chunk;
    niffFontDescription FontDescription;

    /* Initialize the chunk's structure */
    FontDescription.fontNamePtr = fontNamePtr;
    FontDescription.size = size;
    FontDescription.spaceHeight = spaceHeight;
    FontDescription.where = where;
    FontDescription.style = style;

    /* Write the chunk header */
    chunk.fccId = niffckidFontDescription;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffFontDescription(NIFFIOStorageGetFile(), 
                                     &FontDescription))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkFontSymbol(
    FOURCC chunkType  ,
    SHORT spaceHeight  ,
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffFontSymbol FontSymbol;

    /* Initialize the chunk's structure */
    FontSymbol.chunkType = chunkType;
    FontSymbol.spaceHeight = spaceHeight;
    FontSymbol.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidFontSymbol;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffFontSymbol(NIFFIOStorageGetFile(), 
                                     &FontSymbol))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkGuitarGrid(
    BYTE numberOfFrets  ,
    BYTE numberOfStrings  ,
    STROFFSET textDescription)
{
    
    RIFFIOChunk chunk;
    niffGuitarGrid GuitarGrid;

    /* Initialize the chunk's structure */
    GuitarGrid.numberOfFrets = numberOfFrets;
    GuitarGrid.numberOfStrings = numberOfStrings;
    GuitarGrid.textDescription = textDescription;

    /* Write the chunk header */
    chunk.fccId = niffckidGuitarGrid;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffGuitarGrid(NIFFIOStorageGetFile(), 
                                     &GuitarGrid))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkGuitarTabNum(
    BYTE number  ,
    SIGNEDBYTE staffStep)
{
    
    RIFFIOChunk chunk;
    niffGuitarTabNum GuitarTabNum;

    /* Initialize the chunk's structure */
    GuitarTabNum.number = number;
    GuitarTabNum.staffStep = staffStep;

    /* Write the chunk header */
    chunk.fccId = niffckidGuitarTabNum;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffGuitarTabNum(NIFFIOStorageGetFile(), 
                                     &GuitarTabNum))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkHairpin(
    BYTE direction)
{
    
    RIFFIOChunk chunk;
    niffHairpin Hairpin;

    /* Initialize the chunk's structure */
    Hairpin.direction = direction;

    /* Write the chunk header */
    chunk.fccId = niffckidHairpin;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffHairpin(NIFFIOStorageGetFile(), 
                                     &Hairpin))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkHarpPedal(
    STROFFSET pedalPositions)
{
    
    RIFFIOChunk chunk;
    niffHarpPedal HarpPedal;

    /* Initialize the chunk's structure */
    HarpPedal.pedalPositions = pedalPositions;

    /* Write the chunk header */
    chunk.fccId = niffckidHarpPedal;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffHarpPedal(NIFFIOStorageGetFile(), 
                                     &HarpPedal))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkKeySignNonstandard(
    BYTE numberOfChunks)
{
    
    RIFFIOChunk chunk;
    niffKeySignNonstandard KeySignNonstandard;

    /* Initialize the chunk's structure */
    KeySignNonstandard.numberOfChunks = numberOfChunks;

    /* Write the chunk header */
    chunk.fccId = niffckidKeySignNonstandard;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffKeySignNonstandard(NIFFIOStorageGetFile(), 
                                     &KeySignNonstandard))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkKeySignature(
    SIGNEDBYTE standardCode)
{
    
    RIFFIOChunk chunk;
    niffKeySignature KeySignature;

    /* Initialize the chunk's structure */
    KeySignature.standardCode = standardCode;

    /* Write the chunk header */
    chunk.fccId = niffckidKeySignature;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffKeySignature(NIFFIOStorageGetFile(), 
                                     &KeySignature))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkLine(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffLine Line;

    /* Initialize the chunk's structure */
    Line.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidLine;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffLine(NIFFIOStorageGetFile(), 
                                     &Line))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkLyric(
    STROFFSET text  ,
    BYTE lyricVerseID)
{
    
    RIFFIOChunk chunk;
    niffLyric Lyric;

    /* Initialize the chunk's structure */
    Lyric.text = text;
    Lyric.lyricVerseID = lyricVerseID;

    /* Write the chunk header */
    chunk.fccId = niffckidLyric;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffLyric(NIFFIOStorageGetFile(), 
                                     &Lyric))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkMeasureNumbering(
    BYTE numberWhichMeasures  ,
    BYTE numberFrequency  ,
    SHORT startingNumber  ,
    FONTIDX fontID  ,
    BYTE aboveOrBelow  ,
    BYTE horizontalCentering  ,
    BYTE enclosure)
{
    
    RIFFIOChunk chunk;
    niffMeasureNumbering MeasureNumbering;

    /* Initialize the chunk's structure */
    MeasureNumbering.numberWhichMeasures = numberWhichMeasures;
    MeasureNumbering.numberFrequency = numberFrequency;
    MeasureNumbering.startingNumber = startingNumber;
    MeasureNumbering.fontID = fontID;
    MeasureNumbering.aboveOrBelow = aboveOrBelow;
    MeasureNumbering.horizontalCentering = horizontalCentering;
    MeasureNumbering.enclosure = enclosure;

    /* Write the chunk header */
    chunk.fccId = niffckidMeasureNumbering;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffMeasureNumbering(NIFFIOStorageGetFile(), 
                                     &MeasureNumbering))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkMidiDataStream(
    BYTE startTime)
{
    
    RIFFIOChunk chunk;
    niffMidiDataStream MidiDataStream;

    /* Initialize the chunk's structure */
    MidiDataStream.startTime = startTime;

    /* Write the chunk header */
    chunk.fccId = niffckidMidiDataStream;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffMidiDataStream(NIFFIOStorageGetFile(), 
                                     &MidiDataStream))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkNiffInfo(
    CHAR NIFFVersion [8] ,
    BYTE programType  ,
    BYTE standardUnits  ,
    SHORT absoluteUnits  ,
    SHORT midiClocksPerQuarter)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidNiffInfo);
    niffNiffInfo NiffInfo;

    /* Initialize the chunk's structure */
    strncpy((char *) NiffInfo.NIFFVersion, 
            (char *) NIFFVersion, 
            8);
    NiffInfo.programType = programType;
    NiffInfo.standardUnits = standardUnits;
    NiffInfo.absoluteUnits = absoluteUnits;
    NiffInfo.midiClocksPerQuarter = midiClocksPerQuarter;

   

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffNiffInfo(NIFFIOStorageGetFile(), 
                                     &NiffInfo))
        return RIFFIO_FAIL;

  if(! NIFFIOStorageChunkEnd())
	return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkNotehead(
    BYTE shape  ,
    SIGNEDBYTE staffStep  ,
    RATIONAL duration)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidNotehead);
    niffNotehead *Notehead = new niffNotehead(shape,staffStep,duration);

      if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffNotehead(NIFFIOStorageGetFile(), 
                                     Notehead))
		delete (Notehead);
        return RIFFIO_FAIL;

	delete (Notehead);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkOctaveSign(
    BYTE numberOfOctaves  ,
    BYTE aboveOrBelow  ,
    BYTE type  ,
    STROFFSET textString)
{
    
    RIFFIOChunk chunk;
    niffOctaveSign OctaveSign;

    /* Initialize the chunk's structure */
    OctaveSign.numberOfOctaves = numberOfOctaves;
    OctaveSign.aboveOrBelow = aboveOrBelow;
    OctaveSign.type = type;
    OctaveSign.textString = textString;

    /* Write the chunk header */
    chunk.fccId = niffckidOctaveSign;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffOctaveSign(NIFFIOStorageGetFile(), 
                                     &OctaveSign))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkOrnament(
    SHORT shape)
{
    
    RIFFIOChunk chunk;
    niffOrnament Ornament;

    /* Initialize the chunk's structure */
    Ornament.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidOrnament;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffOrnament(NIFFIOStorageGetFile(), 
                                     &Ornament))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkParenthesis(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffParenthesis Parenthesis;

    /* Initialize the chunk's structure */
    Parenthesis.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidParenthesis;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffParenthesis(NIFFIOStorageGetFile(), 
                                     &Parenthesis))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkPart(
    SHORT partID  ,
    STROFFSET name  ,
    STROFFSET abbreviation  ,
    BYTE numberOfStaves  ,
    SIGNEDBYTE midiChannel  ,
    SIGNEDBYTE midiCable  ,
    SIGNEDBYTE transpose)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidPart);;
    niffPart Part;

    /* Initialize the chunk's structure */
    Part.partID = partID;
    Part.name = name;
    Part.abbreviation = abbreviation;
    Part.numberOfStaves = numberOfStaves;
    Part.midiChannel = midiChannel;
    Part.midiCable = midiCable;
    Part.transpose = transpose;

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffPart(NIFFIOStorageGetFile(), 
                                     &Part))
        return RIFFIO_FAIL;

	if(! NIFFIOStorageChunkEnd())
		return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkPedalOrgan(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffPedalOrgan PedalOrgan;

    /* Initialize the chunk's structure */
    PedalOrgan.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidPedalOrgan;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffPedalOrgan(NIFFIOStorageGetFile(), 
                                     &PedalOrgan))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkPedalPiano(
    BYTE shape)
{
    
    RIFFIOChunk chunk;
    niffPedalPiano PedalPiano;

    /* Initialize the chunk's structure */
    PedalPiano.shape = shape;

    /* Write the chunk header */
    chunk.fccId = niffckidPedalPiano;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffPedalPiano(NIFFIOStorageGetFile(), 
                                     &PedalPiano))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkRehearsalMark(
    STROFFSET textString  ,
    BYTE enclosure)
{
    
    RIFFIOChunk chunk;
    niffRehearsalMark RehearsalMark;

    /* Initialize the chunk's structure */
    RehearsalMark.textString = textString;
    RehearsalMark.enclosure = enclosure;

    /* Write the chunk header */
    chunk.fccId = niffckidRehearsalMark;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffRehearsalMark(NIFFIOStorageGetFile(), 
                                     &RehearsalMark))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkRepeatSign(
    SHORT graphicalCode  ,
    BYTE logicalCode)
{
    
    RIFFIOChunk chunk;
    niffRepeatSign RepeatSign;

    /* Initialize the chunk's structure */
    RepeatSign.graphicalCode = graphicalCode;
    RepeatSign.logicalCode = logicalCode;

    /* Write the chunk header */
    chunk.fccId = niffckidRepeatSign;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffRepeatSign(NIFFIOStorageGetFile(), 
                                     &RepeatSign))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkRest(
    BYTE shape  ,
    SIGNEDBYTE staffStep  ,
    RATIONAL duration)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidRest);
    niffRest *Rest = new niffRest(shape,staffStep,duration);

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffRest(NIFFIOStorageGetFile(), 
                                     Rest))
		delete (Rest);
        return RIFFIO_FAIL;

	delete (Rest);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkStaffGrouping(
    BYTE groupingType  ,
    SHORT firstStaff  ,
    SHORT lastStaff)
{
    
    RIFFIOChunk chunk;
    niffStaffGrouping StaffGrouping;

    /* Initialize the chunk's structure */
    StaffGrouping.groupingType = groupingType;
    StaffGrouping.firstStaff = firstStaff;
    StaffGrouping.lastStaff = lastStaff;

    /* Write the chunk header */
    chunk.fccId = niffckidStaffGrouping;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffStaffGrouping(NIFFIOStorageGetFile(), 
                                     &StaffGrouping))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkSystemSeparation(
    BYTE where)
{
    
    RIFFIOChunk chunk;
    niffSystemSeparation SystemSeparation;

    /* Initialize the chunk's structure */
    SystemSeparation.where = where;

    /* Write the chunk header */
    chunk.fccId = niffckidSystemSeparation;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffSystemSeparation(NIFFIOStorageGetFile(), 
                                     &SystemSeparation))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTempoMarkNonstandard(
    BYTE numberOfChunks)
{
    
    RIFFIOChunk chunk;
    niffTempoMarkNonstandard TempoMarkNonstandard;

    /* Initialize the chunk's structure */
    TempoMarkNonstandard.numberOfChunks = numberOfChunks;

    /* Write the chunk header */
    chunk.fccId = niffckidTempoMarkNonstandard;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTempoMarkNonstandard(NIFFIOStorageGetFile(), 
                                     &TempoMarkNonstandard))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTempoMarking(
    STROFFSET textString  ,
    RATIONAL noteValue  ,
    SHORT beatsPerMinute)
{
    
    RIFFIOChunk chunk;
    niffTempoMarking TempoMarking;

    /* Initialize the chunk's structure */
    TempoMarking.textString = textString;
    TempoMarking.noteValue = noteValue;
    TempoMarking.beatsPerMinute = beatsPerMinute;

    /* Write the chunk header */
    chunk.fccId = niffckidTempoMarking;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTempoMarking(NIFFIOStorageGetFile(), 
                                     &TempoMarking))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkText(
    STROFFSET value)
{
    
    RIFFIOChunk chunk;
    niffText Text;

    /* Initialize the chunk's structure */
    Text.value = value;

    /* Write the chunk header */
    chunk.fccId = niffckidText;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffText(NIFFIOStorageGetFile(), 
                                     &Text))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTimeSigNonstandard(
    BYTE numberOfChunks)
{
    
    RIFFIOChunk chunk;
    niffTimeSigNonstandard TimeSigNonstandard;

    /* Initialize the chunk's structure */
    TimeSigNonstandard.numberOfChunks = numberOfChunks;

    /* Write the chunk header */
    chunk.fccId = niffckidTimeSigNonstandard;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTimeSigNonstandard(NIFFIOStorageGetFile(), 
                                     &TimeSigNonstandard))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTimeSignature(
    SIGNEDBYTE topNumber  ,
    BYTE bottomNumber)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidTimeSignature);
    niffTimeSignature *TimeSignature = new niffTimeSignature(topNumber,bottomNumber);

    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTimeSignature(NIFFIOStorageGetFile(), 
                                     TimeSignature))
		delete (TimeSignature);
        return RIFFIO_FAIL;

	delete (TimeSignature);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTimeSlice(
    BYTE type  ,
    RATIONAL startTime)
{
    
    RIFFIOChunk *chunk = new RIFFIOChunk(niffckidTimeSlice);
    niffTimeSlice *TimeSlice = new niffTimeSlice(type, startTime);

   
    if (! NIFFIOStorageChunkStart(chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTimeSlice(NIFFIOStorageGetFile(), 
                                     TimeSlice))
		delete (TimeSlice);
        return RIFFIO_FAIL;

	delete (TimeSlice);
    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOchunkTremolo(
    BYTE attachedBeamParts  ,
    BYTE unattachedBeamParts)
{
    
    RIFFIOChunk chunk;
    niffTremolo Tremolo;

    /* Initialize the chunk's structure */
    Tremolo.attachedBeamParts = attachedBeamParts;
    Tremolo.unattachedBeamParts = unattachedBeamParts;

    /* Write the chunk header */
    chunk.fccId = niffckidTremolo;

    if (! NIFFIOStorageChunkStart(&chunk))
        return RIFFIO_FAIL;

    /* Write the chunk structure */
    if (! NIFFIOWriteniffTremolo(NIFFIOStorageGetFile(), 
                                     &Tremolo))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***************************
 * Raw tags
 ****************************/

RIFFIOSuccess
NIFFIOtagEndOfSystem(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagEndOfSystem;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagGuitarTabTag(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagGuitarTabTag;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagInvisible(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagInvisible;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagLargeSize(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagLargeSize;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagMultiNodeEndOfSyst(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagMultiNodeEndOfSyst;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagMultiNodeStartOfSyst(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagMultiNodeStartOfSyst;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagSilent(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagSilent;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagSlashedStem(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagSlashedStem;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagSmallSize(void)
{
    
        NIFFIOTag tag;

		tag.tagid = nifftagSmallSize;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagSpacingByPart(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagSpacingByPart;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagSplitStem(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagSplitStem;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}


RIFFIOSuccess
NIFFIOtagStaffName(void)
{
    
        NIFFIOTag tag;


        tag.tagid = nifftagStaffName;

        if (! NIFFIOStorageTagStart(&tag))
                return RIFFIO_FAIL;

        return RIFFIO_OK;

}

/***************************
 * Cooked composite tags
 ****************************/
RIFFIOSuccess
NIFFIOtagAbsPlacement(
    SHORT horizontal  ,
    SHORT vertical)
{
    
    NIFFIOTag tag;
    niffAbsPlacement AbsPlacement;

    /* Initialize the tag's structure */
    AbsPlacement.horizontal = horizontal;
    AbsPlacement.vertical = vertical;

    /* Write the tag header */
    tag.tagid = nifftagAbsPlacement;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffAbsPlacement(NIFFIOStorageGetFile(), 
                                     &AbsPlacement))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagBezierIncoming(
    SHORT horizontal  ,
    SHORT vertical)
{
    
    NIFFIOTag tag;
    niffBezierIncoming BezierIncoming;

    /* Initialize the tag's structure */
    BezierIncoming.horizontal = horizontal;
    BezierIncoming.vertical = vertical;

    /* Write the tag header */
    tag.tagid = nifftagBezierIncoming;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffBezierIncoming(NIFFIOStorageGetFile(), 
                                     &BezierIncoming))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagBezierOutgoing(
    SHORT horizontal  ,
    SHORT vertical)
{
    
    NIFFIOTag tag;
    niffBezierOutgoing BezierOutgoing;

    /* Initialize the tag's structure */
    BezierOutgoing.horizontal = horizontal;
    BezierOutgoing.vertical = vertical;

    /* Write the tag header */
    tag.tagid = nifftagBezierOutgoing;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffBezierOutgoing(NIFFIOStorageGetFile(), 
                                     &BezierOutgoing))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagCustomFontChar(
    FONTIDX fontID  ,
    CHAR characterCode [2])
{
    
    NIFFIOTag tag;
    niffCustomFontChar CustomFontChar;

    /* Initialize the tag's structure */
    CustomFontChar.fontID = fontID;
    strncpy((char *) CustomFontChar.characterCode, 
            (char *) characterCode, 
            2);

    /* Write the tag header */
    tag.tagid = nifftagCustomFontChar;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffCustomFontChar(NIFFIOStorageGetFile(), 
                                     &CustomFontChar))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagLogicalPlacement(
    BYTE horizontal  ,
    BYTE vertical  ,
    BYTE proximity)
{
    
    NIFFIOTag tag;
    niffLogicalPlacement LogicalPlacement;

    /* Initialize the tag's structure */
    LogicalPlacement.horizontal = horizontal;
    LogicalPlacement.vertical = vertical;
    LogicalPlacement.proximity = proximity;

    /* Write the tag header */
    tag.tagid = nifftagLogicalPlacement;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffLogicalPlacement(NIFFIOStorageGetFile(), 
                                     &LogicalPlacement))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagLyricVerseOffset(
    SHORT lyricLineOffset  ,
    BYTE lyricVerseID)
{
    
    NIFFIOTag tag;
    niffLyricVerseOffset LyricVerseOffset;

    /* Initialize the tag's structure */
    LyricVerseOffset.lyricLineOffset = lyricLineOffset;
    LyricVerseOffset.lyricVerseID = lyricVerseID;

    /* Write the tag header */
    tag.tagid = nifftagLyricVerseOffset;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffLyricVerseOffset(NIFFIOStorageGetFile(), 
                                     &LyricVerseOffset))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagMidiPerformance(
    LONG startTime  ,
    LONG duration  ,
    BYTE pitch  ,
    BYTE velocity)
{
    
    NIFFIOTag tag;
    niffMidiPerformance MidiPerformance;

    /* Initialize the tag's structure */
    MidiPerformance.startTime = startTime;
    MidiPerformance.duration = duration;
    MidiPerformance.pitch = pitch;
    MidiPerformance.velocity = velocity;

    /* Write the tag header */
    tag.tagid = nifftagMidiPerformance;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffMidiPerformance(NIFFIOStorageGetFile(), 
                                     &MidiPerformance))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagPartDescOverride(
    SIGNEDBYTE MIDIchannel  ,
    SIGNEDBYTE MIDIcable  ,
    SIGNEDBYTE transpose)
{
    
    NIFFIOTag tag;
    niffPartDescOverride PartDescOverride;

    /* Initialize the tag's structure */
    PartDescOverride.MIDIchannel = MIDIchannel;
    PartDescOverride.MIDIcable = MIDIcable;
    PartDescOverride.transpose = transpose;

    /* Write the tag header */
    tag.tagid = nifftagPartDescOverride;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffPartDescOverride(NIFFIOStorageGetFile(), 
                                     &PartDescOverride))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagRefPtOverride(
    BYTE anchor_h  ,
    BYTE dependent_h  ,
    BYTE anchor_v  ,
    BYTE dependent_v)
{
    
    NIFFIOTag tag;
    niffRefPtOverride RefPtOverride;

    /* Initialize the tag's structure */
    RefPtOverride.anchor_h = anchor_h;
    RefPtOverride.dependent_h = dependent_h;
    RefPtOverride.anchor_v = anchor_v;
    RefPtOverride.dependent_v = dependent_v;

    /* Write the tag header */
    tag.tagid = nifftagRefPtOverride;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffRefPtOverride(NIFFIOStorageGetFile(), 
                                     &RefPtOverride))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagTupletDesc(
    RATIONAL transformRatioAB  ,
    RATIONAL transformRatioCD  ,
    BYTE groupingSymbol)
{
    
    NIFFIOTag tag;
    niffTupletDesc TupletDesc;

    /* Initialize the tag's structure */
    TupletDesc.transformRatioAB = transformRatioAB;
    TupletDesc.transformRatioCD = transformRatioCD;
    TupletDesc.groupingSymbol = groupingSymbol;

    /* Write the tag header */
    tag.tagid = nifftagTupletDesc;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffTupletDesc(NIFFIOStorageGetFile(), 
                                     &TupletDesc))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***************************
 * Cooked primitive tags
 ****************************/
RIFFIOSuccess
NIFFIOtagAltEnding(niffAltEnding AltEnding)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagAltEnding;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffAltEnding(NIFFIOStorageGetFile(), 
                                     &AltEnding))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagAnchorOverride(niffAnchorOverride AnchorOverride)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagAnchorOverride;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffAnchorOverride(NIFFIOStorageGetFile(), 
                                     &AnchorOverride))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagArticDirection(niffArticDirection ArticDirection)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagArticDirection;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffArticDirection(NIFFIOStorageGetFile(), 
                                     &ArticDirection))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagChordSymbolsOffset(niffChordSymbolsOffset ChordSymbolsOffset)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagChordSymbolsOffset;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffChordSymbolsOffset(NIFFIOStorageGetFile(), 
                                     &ChordSymbolsOffset))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagCustomGraphicTag(niffCustomGraphicTag CustomGraphicTag)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagCustomGraphicTag;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffCustomGraphicTag(NIFFIOStorageGetFile(), 
                                     &CustomGraphicTag))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagFannedBeam(niffFannedBeam FannedBeam)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagFannedBeam;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffFannedBeam(NIFFIOStorageGetFile(), 
                                     &FannedBeam))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagFigBassOffset(niffFigBassOffset FigBassOffset)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagFigBassOffset;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffFigBassOffset(NIFFIOStorageGetFile(), 
                                     &FigBassOffset))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagFontID(niffFontID FontID)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagFontID;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffFontID(NIFFIOStorageGetFile(), 
                                     &FontID))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagGraceNote(niffGraceNote GraceNote)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagGraceNote;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffGraceNote(NIFFIOStorageGetFile(), 
                                     &GraceNote))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagGuitarGridOffset(niffGuitarGridOffset GuitarGridOffset)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagGuitarGridOffset;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffGuitarGridOffset(NIFFIOStorageGetFile(), 
                                     &GuitarGridOffset))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagHeight(niffHeight Height)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagHeight;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffHeight(NIFFIOStorageGetFile(), 
                                     &Height))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagID(niffID ID)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagID;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffID(NIFFIOStorageGetFile(), 
                                     &ID))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagLineQuality(niffLineQuality LineQuality)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagLineQuality;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffLineQuality(NIFFIOStorageGetFile(), 
                                     &LineQuality))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagNumStaffLines(niffNumStaffLines NumStaffLines)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagNumStaffLines;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffNumStaffLines(NIFFIOStorageGetFile(), 
                                     &NumStaffLines))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagNumberOfFlags(niffNumberOfFlags NumberOfFlags)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagNumberOfFlags;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffNumberOfFlags(NIFFIOStorageGetFile(), 
                                     &NumberOfFlags))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagNumberOfNodes(niffNumberOfNodes NumberOfNodes)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagNumberOfNodes;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffNumberOfNodes(NIFFIOStorageGetFile(), 
                                     &NumberOfNodes))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagNumberStyle(niffNumberStyle NumberStyle)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagNumberStyle;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffNumberStyle(NIFFIOStorageGetFile(), 
                                     &NumberStyle))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagOssia(niffOssia Ossia)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagOssia;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffOssia(NIFFIOStorageGetFile(), 
                                     &Ossia))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagPartID(niffPartID PartID)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagPartID;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffPartID(NIFFIOStorageGetFile(), 
                                     &PartID))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagRehearsalOffset(niffRehearsalOffset RehearsalOffset)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagRehearsalOffset;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffRehearsalOffset(NIFFIOStorageGetFile(), 
                                     &RehearsalOffset))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagRestNumeral(niffRestNumeral RestNumeral)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagRestNumeral;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffRestNumeral(NIFFIOStorageGetFile(), 
                                     &RestNumeral))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagStaffStep(niffStaffStep StaffStep)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagStaffStep;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffStaffStep(NIFFIOStorageGetFile(), 
                                     &StaffStep))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagThickness(niffThickness Thickness)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagThickness;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffThickness(NIFFIOStorageGetFile(), 
                                     &Thickness))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagTieDirection(niffTieDirection TieDirection)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagTieDirection;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffTieDirection(NIFFIOStorageGetFile(), 
                                     &TieDirection))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagVoiceID(niffVoiceID VoiceID)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagVoiceID;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffVoiceID(NIFFIOStorageGetFile(), 
                                     &VoiceID))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

RIFFIOSuccess
NIFFIOtagWidth(niffWidth Width)
{
    
    NIFFIOTag tag;

    /* Write the tag header */
    tag.tagid = nifftagWidth;

    if (! NIFFIOStorageTagStart(&tag))
        return RIFFIO_FAIL;

    /* Write the tag structure */
    if (! NIFFIOWriteniffWidth(NIFFIOStorageGetFile(), 
                                     &Width))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}
