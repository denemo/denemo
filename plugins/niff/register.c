#ifndef lint
/*static char rcsid[] =
"$Id: register.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/*
 * Public Domain 1995,1996 Timothy Butler
 *
 * THIS DOCUMENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 */
/***************************************************************************/
/*
 * NAME
 * ====
 * register - parser callback registration routines
 *
 * SYNOPSIS
 * ======== 
 *
 * - NIFFIORegisterDefaultList()
 * - NIFFIORegisterDefaultChunk()
 * - NIFFIORegisterDefaultAtomicChunk()
 * - NIFFIORegisterDefaultTag()
 *
 * - NIFFIORegisterForm()
 * - NIFFIORegisterList()
 * - NIFFIORegisterAtomicChunk()
 * 
 * - NIFFIORegisterListXXX()
 * - NIFFIORegisterChunkXXX()
 * - NIFFIORegisterTagXXX()
 */
/***************************************************************************/

#include <assert.h>
#include <stdlib.h>

#include <niffio.h>
#include <niffiop.h>

/***************************************************************************/
/*
 * NIFFIORegisterDefaultList
 * =========================
 * Register default list callbacks
 */
RIFFIOSuccess
NIFFIORegisterDefaultList(NIFFIOParser *pparser,
                          NIFFIOChunkCallback cbStart,
                          NIFFIOChunkCallback cbEnd)
/***************************************************************************/
{
    assert (pparser != 0);

    pparser->defaultList.cbStart = cbStart;
    pparser->defaultList.cbEnd = cbEnd;

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIORegisterDefaultTaggedChunk
 * ================================
 * Register default chunk callbacks
 */
RIFFIOSuccess
NIFFIORegisterDefaultTaggedChunk(NIFFIOParser *pparser,
                                 NIFFIOChunkCallback cbStart,
                                 NIFFIOChunkCallback cbEnd)
/***************************************************************************/
{
    assert (pparser != 0);

    pparser->defaultTaggedChunk.cbStart = cbStart;
    pparser->defaultTaggedChunk.cbEnd = cbEnd;

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIORegisterDefaultAtomicChunk
 * ================================
 * Register a default callback for Atomic chunks
 */
RIFFIOSuccess
NIFFIORegisterDefaultAtomicChunk(NIFFIOParser *pparser,
                                 NIFFIOChunkCallback cb)
/***************************************************************************/
{
    assert (pparser != 0);
        
    pparser->defaultAtomicChunk.cb = cb;

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIORegisterDefaultTag
 * ========================
 * Register a default callback for tags
 */
RIFFIOSuccess
NIFFIORegisterDefaultTag(NIFFIOParser *pparser,
                         NIFFIOTagCallback cbTag)
/***************************************************************************/
{
    assert (pparser != 0);

    pparser->defaultTag.cbTag = cbTag;

    return RIFFIO_OK;
        
}

/***************************************************************************/
/*
 * NIFFIORegisterForm
 * ==================
 * Register callbacks for a Niff form
 */
RIFFIOSuccess
NIFFIORegisterForm(NIFFIOParser *pparser,
                   NIFFIOChunkCallback cbStart,
                   NIFFIOChunkCallback cbEnd)
/***************************************************************************/
{
    assert (pparser != 0);
        
    pparser->formentry.cbStart = cbStart;
    pparser->formentry.cbEnd = cbEnd;

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * NIFFIORegisterList
 * ==================
 * Register callbacks for specific lists
 */
RIFFIOSuccess
NIFFIORegisterList(NIFFIOParser *pparser,
                   FOURCC fccType,
                   NIFFIOChunkCallback cbStart,
                   NIFFIOChunkCallback cbEnd)
/***************************************************************************/
{
        
    char strModule[] = "NIFFIORegisterList";
    char strType[RIFFIO_FOURCC_LIM];

    NIFFIOPListEntry *pListEntry; /* newly allocated entry */
    RIFFIOFCCTableEntry tblEntry; /* interface to RIFFIOFCCTableMakeEntry */

    assert (pparser != 0);

    RIFFIOFOURCCToString(fccType, strType);
        
    if (!RIFFIOFOURCCIsValid(fccType))
    {
        RIFFIOError(strModule, "Invalid FOURCC type <%s>", strType);
        return RIFFIO_FAIL;
    }

    /*
     * Allocate a new list entry structure to hold our callbacks
     */
    pListEntry = (NIFFIOPListEntry *) malloc(sizeof(NIFFIOPListEntry));
    if (pListEntry == 0)
    {
        RIFFIOError(strModule, 
                    "Failed to allocate list callback entry, type <%s>",
                    strType);

        return RIFFIO_FAIL;
    }

    /*
     * Initialize the new entry
     */
    pListEntry->cbStart = cbStart;
    pListEntry->cbEnd = cbEnd;

    /*
     * Make the entry in the list callback table
     */
    tblEntry.fcc = fccType;
    tblEntry.value.l = (long) pListEntry;

    if (! RIFFIOFCCTableMakeEntry(pparser->pfcctblLists, tblEntry))
    {
        /*
         * Clean up on failure
         */
        free(pListEntry);

        RIFFIOError(strModule,
                    "Failed to install new list callback entry, type <%s>",
                    strType);
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * NIFFIORegisterAtomicChunk
 * =========================
 * Register a callback for specific Atomic chunks
 */
RIFFIOSuccess
NIFFIORegisterAtomicChunk(NIFFIOParser *pparser,
                          FOURCC fccId,
                          NIFFIOChunkCallback cb)
/***************************************************************************/
{

    char strModule[] = "NIFFIORegisterAtomicChunk";
    char strId[RIFFIO_FOURCC_LIM];

    NIFFIOPAtomicChunkEntry *pAtomicChunkEntry;   /* newly allocated entry */
    RIFFIOFCCTableEntry tblEntry;  /* Interface to RIFFIOFCCTableMakeEntry */

    assert (pparser != 0);

    RIFFIOFOURCCToString(fccId, strId);

    if (! RIFFIOFOURCCIsValid(fccId))
    {
        RIFFIOError(strModule, "Invalid FOURCC id <%s>", strId);
        return RIFFIO_FAIL;
    }

    /*
     * Allocate a new list entry structure to hold our callbacks
     */
    pAtomicChunkEntry = 
        (NIFFIOPAtomicChunkEntry *) malloc(sizeof(NIFFIOPAtomicChunkEntry));
    if (pAtomicChunkEntry == 0)
    {
        RIFFIOError(strModule, 
                    "Failed to allocate chunk callback entry, id <%s>",
                    strId);
        return RIFFIO_FAIL;
    }

    /*
     * Initialize the new entry
     */
    pAtomicChunkEntry->cb = cb;

    /*
     * Make the entry in the list callback table
     */
    tblEntry.fcc = fccId;
    tblEntry.value.l = (long) pAtomicChunkEntry;

    if (! RIFFIOFCCTableMakeEntry(pparser->pfcctblAtomicChunks, tblEntry))
    {
        /*
         * Clean up on failure
         */
        free(pAtomicChunkEntry);

        RIFFIOError(strModule,
                    "Failed to install new chunk callback entry, id <%s>",
                    strId);
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;

}

/*
 * NIFFIOPRegisterTaggedCookedChunk
 * ================================
 * (Private)
 * Register callbacks, including a dispatching routine
 * for specific composite chunks
 */
RIFFIOSuccess
NIFFIOPRegisterTaggedCookedChunk(NIFFIOParser *pparser,
                                 FOURCC fccId,
                                 NIFFIOPReader cbRead,
                                 NIFFIOCookedChunkCallback cbStart,
                                 NIFFIOCookedChunkCallback cbEnd)
{
    
    char strModule[] = "NIFFIOPRegisterTaggedCookedChunk";
    char strId[RIFFIO_FOURCC_LIM];
    
    NIFFIOPTaggedChunkEntry *pTaggedChunkEntry; /* a new value for our table */
    
    RIFFIOFCCTableEntry tblEntry;    /* interface to NIFFIOFCCTableMakeEntry */
    
    assert (pparser != 0);
    
    RIFFIOFOURCCToString(fccId, strId);
    
    if (! RIFFIOFOURCCIsValid(fccId))
    {
        RIFFIOError(strModule, "Invalid FOURCC id <%s>", strId);
        return RIFFIO_FAIL;
    }

    /*
     * Allocate a new list entry structure to hold our callbacks
     */
    pTaggedChunkEntry = (NIFFIOPTaggedChunkEntry *) 
        malloc(sizeof(NIFFIOPTaggedChunkEntry));
        
    if (! pTaggedChunkEntry)
    {
        RIFFIOError(strModule, 
                    "Failed to allocate chunk callback entry, id <%s>",
                    strId);

        return RIFFIO_FAIL;
    }

    /*
     * Initialize the new entry 
     */
    pTaggedChunkEntry->cbRead = cbRead;
    pTaggedChunkEntry->cbStart.cooked = cbStart;
    pTaggedChunkEntry->cbEnd.cooked = cbEnd;

    /*
     * Make the entry in the callback table
     */
    tblEntry.fcc = fccId;
    tblEntry.value.l = (long) pTaggedChunkEntry;

    if (! RIFFIOFCCTableMakeEntry(pparser->pfcctblTaggedChunks, tblEntry))
    {
        /*
         * Clean up on failure 
         */
        free(pTaggedChunkEntry);

        RIFFIOError(strModule,
                    "Failed to install new chunk callback entry, id <%s>",
                    strId);
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;

}


/*
 * NIFFIOPRegisterTaggedRawChunk
 * =============================
 * (Private)
 * Register callbacks, for specific tagged, raw chunks
 * These are taggable chunks with an empty fixed portion.
 * (CLT entry == 0)
 */
RIFFIOSuccess
NIFFIOPRegisterTaggedRawChunk(NIFFIOParser *pparser,
                              FOURCC fccId,
                              NIFFIOChunkCallback cbStart,
                              NIFFIOChunkCallback cbEnd)
{

    char strModule[] = "NIFFIOPRegisterTaggedRawChunk";
    char strId[RIFFIO_FOURCC_LIM];

    NIFFIOPTaggedChunkEntry *pTaggedChunkEntry; /* a new value for our table */

    RIFFIOFCCTableEntry tblEntry;    /* interface to NIFFIOFCCTableMakeEntry */

    assert (pparser != 0);

    RIFFIOFOURCCToString(fccId, strId);

    if (! RIFFIOFOURCCIsValid(fccId))
    {
        RIFFIOError(strModule, "Invalid FOURCC id <%s>", strId);
        return RIFFIO_FAIL;
    }

    /*
     * Allocate a new list entry structure to hold our callbacks
     */
    pTaggedChunkEntry = (NIFFIOPTaggedChunkEntry *) 
        malloc(sizeof(NIFFIOPTaggedChunkEntry));
        
    if (! pTaggedChunkEntry)
    {
        RIFFIOError(strModule, 
                    "Failed to allocate chunk callback entry, id <%s>",
                    strId);

        return RIFFIO_FAIL;
    }

    /*
     * Initialize the new entry 
     */
    pTaggedChunkEntry->cbRead = 0;
    pTaggedChunkEntry->cbStart.raw = cbStart;
    pTaggedChunkEntry->cbEnd.raw = cbEnd;

    /*
     * Make the entry in the callback table
     */
    tblEntry.fcc = fccId;
    tblEntry.value.l = (long) pTaggedChunkEntry;

    if (! RIFFIOFCCTableMakeEntry(pparser->pfcctblTaggedChunks, tblEntry))
    {
        /*
         * Clean up on failure 
         */
        free(pTaggedChunkEntry);

        RIFFIOError(strModule,
                    "Failed to install new chunk callback entry, id <%s>",
                    strId);
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;

}

/*
 * NIFFIOPRegisterCookedTag
 * ========================
 * (private)
 * Register a callback and a reader for a specific tag/FOURCC combination
 */ 
RIFFIOSuccess
NIFFIOPRegisterCookedTag(NIFFIOParser *pparser,
                         BYTE tagid,
                         FOURCC fcc,
                         NIFFIOPReader cbRead,
                         NIFFIOCookedTagCallback cbTag)
{
    NIFFIOPTagEntry tagentry;

    assert (pparser != 0);
    assert (pparser->ptcbtblTags != 0);
    assert (cbTag != 0);

    tagentry.fcc = fcc;
    tagentry.cbRead = cbRead;
    tagentry.cbTag.cooked = cbTag;
        
    return NIFFIOPTCBTableMakeEntry(pparser->ptcbtblTags, tagid, &tagentry);
        
}

/*
 * NIFFIOPRegisterRawTag
 * =====================
 * (private)
 * Register a callback for a specific tag/FOURCC combination
 */ 
RIFFIOSuccess
NIFFIOPRegisterRawTag(NIFFIOParser *pparser,
                      BYTE tagid,
                      FOURCC fcc,
                      NIFFIOTagCallback cbTag)
{
    NIFFIOPTagEntry tagentry;

    assert (pparser != 0);
    assert (pparser->ptcbtblTags != 0);
    assert (cbTag != 0);

    tagentry.fcc = fcc;
    tagentry.cbRead = 0;
    tagentry.cbTag.raw = cbTag;
        
    return NIFFIOPTCBTableMakeEntry(pparser->ptcbtblTags, tagid, &tagentry);
        

}

/***************************************************************************/
/*
 * Registration functions for Lists
 */
#define REG_LIST(nifftype)                                                   \
RIFFIOSuccess                                                                \
NIFFIORegisterList##nifftype(NIFFIOParser *pparser,                          \
                             NIFFIOChunkCallback cbStart,                    \
                             NIFFIOChunkCallback cbEnd)                      \
{                                                                            \
     return NIFFIORegisterList(pparser, nifflist##nifftype, cbStart, cbEnd); \
}
/***************************************************************************/

REG_LIST(SetupSection)
REG_LIST(Parts)
REG_LIST(RiffInfo)
REG_LIST(Groupings)
REG_LIST(FontDescs)
REG_LIST(CustomGraphics)

REG_LIST(DataSection)
REG_LIST(Page)
REG_LIST(System)
REG_LIST(Staff)
                                                         
#undef REG_LIST

/***************************************************************************/
/*
 * Registration functions for atomic, raw chunks
 */
#define REG_ATOMIC(nifftype)                                            \
RIFFIOSuccess                                                           \
NIFFIORegisterChunk##nifftype(                                          \
                              NIFFIOParser *pparser,                    \
                              NIFFIOChunkCallback cb)                   \
{                                                                       \
     return NIFFIORegisterAtomicChunk(pparser, niffckid##nifftype, cb); \
}

/***************************************************************************/

REG_ATOMIC(ChnkLenTable)
REG_ATOMIC(EpsGraphic)
REG_ATOMIC(PsType1Font)
REG_ATOMIC(PsType3Font)
REG_ATOMIC(StringTable)

#undef REG_ATOMIC

/***************************************************************************/
/*
 * Registration functions for tagable, raw Chunks
 */

#define REG_TAGRAW(nifftype)                                    \
RIFFIOSuccess                                                   \
NIFFIORegisterChunk##nifftype(                                  \
                              NIFFIOParser *pparser,            \
                              NIFFIOChunkCallback cbStart,      \
                              NIFFIOChunkCallback cbEnd)        \
{                                                               \
     return NIFFIOPRegisterTaggedRawChunk(pparser,              \
                                          niffckid##nifftype,   \
                                          cbStart,              \
                                          cbEnd);               \
}
/***************************************************************************/

REG_TAGRAW(PageHeader)
REG_TAGRAW(StaffHeader)
REG_TAGRAW(SystemHeader)

REG_TAGRAW(AugDot)
REG_TAGRAW(Glissando)
REG_TAGRAW(Portamento)
REG_TAGRAW(Slur)
REG_TAGRAW(Stem)
REG_TAGRAW(TagActivate)
REG_TAGRAW(TagInactivate)
REG_TAGRAW(Tie)
REG_TAGRAW(Tuplet)

#undef REG_TAGRAW


/***************************************************************************/
/*
 * Registration functions for tagable, cooked Chunks
 */
#define REG_TAGCOOK(nifftype)                                           \
RIFFIOSuccess                                                           \
NIFFIORegisterChunk##nifftype(                                          \
   NIFFIOParser *pparser,                                               \
   RIFFIOSuccess (*cbStart)(NIFFIOChunkContext *, niff##nifftype *),    \
   RIFFIOSuccess (*cbEnd)(NIFFIOChunkContext *, niff##nifftype *))      \
{                                                                       \
   return NIFFIOPRegisterTaggedCookedChunk(                             \
        pparser,                                                        \
        niffckid##nifftype,                                             \
        (NIFFIOPReader) NIFFIOReadniff##nifftype,                       \
        (NIFFIOCookedChunkCallback) cbStart,                            \
        (NIFFIOCookedChunkCallback) cbEnd);                             \
}
/***************************************************************************/

REG_TAGCOOK(DefaultValues)
REG_TAGCOOK(FontDescription)
REG_TAGCOOK(NiffInfo)
REG_TAGCOOK(Part)
REG_TAGCOOK(StaffGrouping)
REG_TAGCOOK(TimeSlice)

REG_TAGCOOK(Accidental)
REG_TAGCOOK(AltEndingGraphic)
REG_TAGCOOK(Arpeggio)
REG_TAGCOOK(Articulation)
REG_TAGCOOK(Barline)
REG_TAGCOOK(Beam)
REG_TAGCOOK(ChordSymbol)
REG_TAGCOOK(Clef)
REG_TAGCOOK(CustomGraphicChk)
REG_TAGCOOK(Dynamic)
REG_TAGCOOK(FiguredBass)
REG_TAGCOOK(Fingering)
REG_TAGCOOK(GuitarGrid)
REG_TAGCOOK(GuitarTabNum)
REG_TAGCOOK(Hairpin)
REG_TAGCOOK(HarpPedal)
REG_TAGCOOK(KeySignature)
REG_TAGCOOK(KeySignNonstandard)
REG_TAGCOOK(Line)
REG_TAGCOOK(Lyric)
REG_TAGCOOK(MeasureNumbering)
REG_TAGCOOK(MidiDataStream)
REG_TAGCOOK(FontSymbol)
REG_TAGCOOK(Notehead)
REG_TAGCOOK(OctaveSign)
REG_TAGCOOK(Ornament)
REG_TAGCOOK(Parenthesis)
REG_TAGCOOK(PedalPiano)
REG_TAGCOOK(PedalOrgan)
REG_TAGCOOK(RehearsalMark)
REG_TAGCOOK(RepeatSign)
REG_TAGCOOK(Rest)
REG_TAGCOOK(SystemSeparation)
REG_TAGCOOK(TempoMarking)
REG_TAGCOOK(TempoMarkNonstandard)
REG_TAGCOOK(Text)
REG_TAGCOOK(TimeSignature)
REG_TAGCOOK(TimeSigNonstandard)
REG_TAGCOOK(Tremolo)

#undef REG_TAGCOOK



/***************************************************************************/
/*
 * Registration functions for raw Tags
 */
#define REG_TAGRAW(nifftype)                                            \
RIFFIOSuccess                                                           \
NIFFIORegisterTag##nifftype(                                            \
                            NIFFIOParser *pparser,                      \
                            FOURCC fcc,                                 \
                            NIFFIOTagCallback cb)                       \
{                                                                       \
     return NIFFIOPRegisterRawTag(pparser, nifftag##nifftype, fcc, cb); \
}

/***************************************************************************/

REG_TAGRAW(EndOfSystem)
REG_TAGRAW(GuitarTabTag)
REG_TAGRAW(Invisible)
REG_TAGRAW(LargeSize)
REG_TAGRAW(MultiNodeEndOfSyst)
REG_TAGRAW(MultiNodeStartOfSyst)
REG_TAGRAW(Silent)
REG_TAGRAW(SlashedStem)
REG_TAGRAW(SmallSize)
REG_TAGRAW(SpacingByPart)
REG_TAGRAW(SplitStem)
REG_TAGRAW(StaffName)

#undef REG_TAGRAW


/***************************************************************************/
/*
 * Registration functions for cooked Tags
 */
#define REG_TAGCOOK(nifftype)                                           \
RIFFIOSuccess                                                           \
NIFFIORegisterTag##nifftype(                                            \
      NIFFIOParser *pparser,                                            \
      FOURCC fcc,                                                       \
      RIFFIOSuccess (*cb)(NIFFIOTagContext *, niff##nifftype *))        \
{                                                                       \
     return NIFFIOPRegisterCookedTag(                                   \
                pparser,                                                \
                nifftag##nifftype,                                      \
                fcc,                                                    \
                (NIFFIOPReader) NIFFIOReadniff##nifftype,               \
                (NIFFIOCookedTagCallback) cb);                          \
}

/***************************************************************************/
   

REG_TAGCOOK(AbsPlacement)
REG_TAGCOOK(AltEnding)
REG_TAGCOOK(AnchorOverride)
REG_TAGCOOK(ArticDirection)
REG_TAGCOOK(BezierIncoming)
REG_TAGCOOK(BezierOutgoing)
REG_TAGCOOK(ChordSymbolsOffset)
REG_TAGCOOK(CustomFontChar)
REG_TAGCOOK(CustomGraphicTag)
REG_TAGCOOK(FannedBeam)
REG_TAGCOOK(FigBassOffset)
REG_TAGCOOK(FontID)
REG_TAGCOOK(GraceNote)
REG_TAGCOOK(GuitarGridOffset)
REG_TAGCOOK(Height)
REG_TAGCOOK(ID)
REG_TAGCOOK(LineQuality)
REG_TAGCOOK(LogicalPlacement)
REG_TAGCOOK(LyricVerseOffset)
REG_TAGCOOK(MidiPerformance)
REG_TAGCOOK(NumberOfFlags)
REG_TAGCOOK(NumberOfNodes)
REG_TAGCOOK(NumStaffLines)
REG_TAGCOOK(Ossia)
REG_TAGCOOK(PartDescOverride)
REG_TAGCOOK(PartID)
REG_TAGCOOK(RefPtOverride)
REG_TAGCOOK(RehearsalOffset)
REG_TAGCOOK(RestNumeral)
REG_TAGCOOK(StaffStep)
REG_TAGCOOK(Thickness)
REG_TAGCOOK(TieDirection)
REG_TAGCOOK(TupletDesc)
REG_TAGCOOK(NumberStyle)
REG_TAGCOOK(VoiceID)
REG_TAGCOOK(Width)


#undef REG_TAGCOOK


