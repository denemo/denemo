#ifndef lint
static char rcsid[] = 
"$Id: names.c,v 1.1 2007/01/28 11:48:19 atee Exp $";
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
 * names - Convert from niff values to strings.
 *
 * SYNOPSIS
 * ========
 *
 * - NIFFIONameListType()
 * - NIFFIONameChunkId()
 * - NIFFIONameTagId()
 *
 * - NIFFIOSymbolTS()
 * - NIFFIOSymbolBAREXT()
 * - NIFFIOSymbolBARTYPE()
 * - NIFFIOSymbolCLEFSHAPE()
 * - NIFFIOSymbolCLEFOCT()
 * - NIFFIOSymbolNOTESHAPE()
 * - NIFFIOSymbolREST()
 * - NIFFIOSymbolLOGPLACEV()
 * - NIFFIOSymbolLOGPLACEPROX()
 *
 * DESCRIPTION
 * ===========
 * Treat chunks and lists (including forms) separately because
 * they are separate namespaces
 */
/***************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <string.h>

#include <niffio.h>

/***************************************************************************/
/*
 * NIFFIONameListType
 * ==================
 * Return a string description of a NIFF list type */
const char *
NIFFIONameListType(RIFFIOFOURCC fccType)
/***************************************************************************/
{
    switch (fccType)
    {
      case niffformNiff:   return "NIFF Form";
          
      case nifflistSetupSection: return "Setup Section";
      case nifflistParts:  return "Parts";
      case nifflistRiffInfo: return "RIFF INFO";
      case nifflistGroupings: return "Staff Groupings";
      case nifflistFontDescs: return "Font Descriptions";
      case nifflistCustomGraphics: return "Custom Graphics";
          
      case nifflistDataSection: return "Data Section";
      case nifflistPage: return "Page";
      case nifflistSystem: return "System";
      case nifflistStaff: return "Staff";
          
    }

    return "UNKNOWN LIST";
}

/***************************************************************************/
/*
 * NIFFIONameChunkId
 * =================
 * Return a string description of a NIFF chunk id
 */
const char *
NIFFIONameChunkId(RIFFIOFOURCC fccId)
/***************************************************************************/
{

    switch (fccId)
    {
      case niffckidChnkLenTable: return "Chunk Length Table";
      case niffckidDefaultValues: return "Default Values";
      case niffckidEpsGraphic: return "EPS Graphic";
      case niffckidFontDescription: return "Font Description";
      case niffckidNiffInfo: return "NIFF Information";
      case niffckidPart: return "Part Description";
      case niffckidPsType1Font: return "Postscript Type 1 Font";
      case niffckidPsType3Font: return "Postscript Type 3 Font";
      case niffckidStaffGrouping: return "Staff Grouping";
      case niffckidStringTable: return "String Table";

         
      case niffckidPageHeader: return "Page Header";
      case niffckidStaffHeader: return "Staff Header";
      case niffckidSystemHeader: return "System Header";
      case niffckidTimeSlice: return "Time-Slice";

      case niffckidAccidental: return "Accidental";
      case niffckidAltEndingGraphic: return "Alternate Ending Graphic";
      case niffckidArpeggio: return "Arpeggio";
      case niffckidArticulation: return "Articulation";
      case niffckidAugDot: return "Augmentation Dot";
      case niffckidBarline: return "Barline";
      case niffckidBeam: return "Beam";
      case niffckidChordSymbol: return "Chord Symbol";
      case niffckidClef: return "Clef";
      case niffckidCustomGraphicChk: return "Custom Graphic Symbol";
      case niffckidDynamic: return "Dynamic";
      case niffckidFiguredBass: return "Figured Bass";
      case niffckidFingering: return "Fingering";
      case niffckidGlissando: return "Glissando";
      case niffckidGuitarGrid: return "Guitar Grid";
      case niffckidGuitarTabNum: return "Guitar TAB Number";
      case niffckidHairpin: return "Hairpin";
      case niffckidHarpPedal: return "Harp Pedal Symbol";
      case niffckidKeySignature: return "Key Signature";
      case niffckidKeySignNonstandard: return "Key Signature - Nonstandard";
      case niffckidLine: return "Line";
      case niffckidLyric: return "Lyric";
      case niffckidMeasureNumbering: return "Measure Numbering";
      case niffckidMidiDataStream: return "MIDI Data Stream";
      case niffckidFontSymbol: return "NIFF Font Symbol";
      case niffckidNotehead: return "Notehead";
      case niffckidOctaveSign: return "Octave Sign";
      case niffckidOrnament: return "Ornament";
      case niffckidParenthesis: return "Parenthesis";
      case niffckidPedalPiano: return "Pedal (Piano)";
      case niffckidPedalOrgan: return "Pedal (Organ)";
      case niffckidPortamento: return "Portamento";
      case niffckidRehearsalMark: return "Rehearsal Mark";
      case niffckidRepeatSign: return "Repeat Sign";
      case niffckidRest: return "Rest";
      case niffckidSlur: return "Slur";
      case niffckidStem: return "Stem";
      case niffckidSystemSeparation: return "System Separation Mark";
      case niffckidTagActivate: return "Tag Activate";
      case niffckidTagInactivate: return "Tag Inactivate";
      case niffckidTempoMarking: return "Tempo Marking";
      case niffckidTempoMarkNonstandard: return "Tempo Marking NonStandard";
      case niffckidText: return "Text";
      case niffckidTie: return "Tie";
      case niffckidTimeSignature: return "Time Signature";
      case niffckidTimeSigNonstandard: return "Time Signature - NonStandard";
      case niffckidTremolo: return "Tremolo";
      case niffckidTuplet: return "Tuplet";
      case niffckidUserDefined: return "User Defined";

    }
    
    return "UNKNOWN CHUNK";
    
}



/***************************************************************************/
/*
 * NIFFIONameTagId
 * ===============
 * Return a string description of a kind of NIFF tag
 */
const char *
NIFFIONameTagId(BYTE tagid)
/***************************************************************************/
{
    switch (tagid)
    {
      case nifftagAbsPlacement: return "Absolute Placement";
      case nifftagAltEnding: return "Alternate Ending";
      case nifftagAnchorOverride: return "Anchor Override";
      case nifftagArticDirection: return "Articulation Direction";
      case nifftagBezierIncoming: return "Bezier Incoming";
      case nifftagBezierOutgoing: return "Bezier Outgoing";
      case nifftagChordSymbolsOffset: return "Chord Symbols Offset";
      case nifftagCustomFontChar: return "Custom Font Character";
      case nifftagCustomGraphicTag: return "Custom Graphic";
      case nifftagEndOfSystem: return "End Of System";
      case nifftagFannedBeam: return "Fanned Beam";
      case nifftagFigBassOffset: return "Figured Bass Offset";
      case nifftagFontID: return "Font ID";
      case nifftagGraceNote: return "Grace Note";
      case nifftagGuitarGridOffset: return "Guitar Grid Offset";
      case nifftagGuitarTabTag: return "Guitar Tablature";
      case nifftagHeight: return "Height";
      case nifftagID: return "ID";
      case nifftagInvisible: return "Invisible";
      case nifftagLargeSize: return "Large Size";
      case nifftagLineQuality: return "Line Quality";
      case nifftagLogicalPlacement: return "Logical Placement";
      case nifftagLyricVerseOffset: return "Lyric Verse Offset";
      case nifftagMidiPerformance: return "MIDI Performace";
      case nifftagMultiNodeEndOfSyst: return "Multi-node End Of System";
      case nifftagMultiNodeStartOfSyst: return "Multi-node Start Of System";
      case nifftagNumberOfFlags: return "Number of Flags";
      case nifftagNumberOfNodes: return "Number of Nodes";
      case nifftagNumStaffLines: return "Number of Staff Lines";
      case nifftagOssia: return "Ossia";
      case nifftagPartDescOverride: return "Part Description Override";
      case nifftagPartID: return "Part ID";
      case nifftagRefPtOverride: return "Reference Point Override";
      case nifftagRehearsalOffset: return "Rehearsal Mark Offset";
      case nifftagRestNumeral: return "Rest Numeral";
      case nifftagSilent: return "Silent";
      case nifftagSlashedStem: return "Slashed Stem";
      case nifftagSmallSize: return "Small Size";
      case nifftagSpacingByPart: return "Spacing By Part";
      case nifftagSplitStem: return "Split Stem";
      case nifftagStaffName: return "Staff Name";
      case nifftagStaffStep: return "Staff Step";
      case nifftagThickness: return "Thickness";
      case nifftagTieDirection: return "Tie Direction";
      case nifftagTupletDesc: return "Tuplet Description";
      case nifftagNumberStyle: return "Number Style";
      case nifftagVoiceID: return "Voice ID";
      case nifftagWidth: return "Width";
      case nifftagUserDefined: return "User Defined";


    }

    return "UNKNOWN TAG";

}

/***************************************************************************/
/*
 * NIFFIOSymbolTS
 * ==============
 * Return a symbolic name for a time-slice type, NULL on failure
 */
const char *
NIFFIOSymbolTS(BYTE ts)
/***************************************************************************/
{
    switch(ts)
    {
      case tsMeasureStart: return "tsMeasureStart";
      case tsEvent: return "tsEvent";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolBAREXT
 * ==================
 * Return a symbolic name for a Barline extent, NULL on failure
 */
const char *
NIFFIOSymbolBAREXT(BYTE be)
/***************************************************************************/
{
    switch(be)
    {
      case barextThruLastStaff: return "barextThruLastStaff";
      case barextThruSpace: return "barextThruSpace";
      case barextBetweenStaves: return "barextBetweenStaves";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolBARTYPE
 * ===================
 * Return a symbolic name for a Barline type, NULL on failure
 */
const char *
NIFFIOSymbolBARTYPE(BYTE bt)
/***************************************************************************/
{
    switch(bt)
    {
      case bartypeThin: return "bartypeThin";
      case bartypeThick: return "bartypeThick";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolCLEFSHAPE
 * =====================
 * Return a symbolic name for a clef shape, NULL on failure
 */
const char *
NIFFIOSymbolCLEFSHAPE(BYTE cs)
/***************************************************************************/
{
    switch(cs)
    {
      case clefshapeGclef: return "clefshapeGclef";
      case clefshapeFclef: return "clefshapeFclef";
      case clefshapeCclef: return "clefshapeCclef";
      case clefshapePercussion: return "clefshapePercussion";
      case clefshapeDoubleGclef: return "clefshapDoubleGclef";
      case clefshapeGuitarTab: return "clefshapeGuitarTab";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolCLEFOCT
 * ===================
 * Return a symbolic name for a clef octave number, NULL on failure
 */
const char *
NIFFIOSymbolCLEFOCT(BYTE co)
/***************************************************************************/
{
    switch(co)
    {
      case clefoctNoNumber: return "clefoctNoNumber";
      case clefoct8Above: return   "clefoct8Above";
      case clefoct8Below: return   "clefoct8Below";
      case clefoct15Above: return  "clefoct15Above";
      case clefoct15Below: return  "clefoct15Below";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolNOTESHAPE
 * =====================
 * Return a symbolic name for a note shape, NULL on failure
 */
const char *
NIFFIOSymbolNOTESHAPE(BYTE ns)
/***************************************************************************/
{
    switch(ns)
    {
      
      case noteshapeBreve: return "noteshapeBreve";
      case noteshapeWhole: return "noteshapeWhole";
      case noteshapeHalf: return "noteshapeHalf";
      case noteshapeFilled: return "noteshapeFilled";
      case noteshapeOpenDiamond: return "noteshapeOpenDiamond";
      case noteshapeSolidDiamond: return "noteshapeSolidDiamond";
      case noteshapeX: return "noteshapeX";
      case noteshapeOpenX: return "noteshapeOpenX";
      case noteshapeGuitarSlash: return "noteshapeGuitarSlash";
      case noteshapeOpenGuitarSlash: return "noteshapeOpenGuitarSlash";
      case noteshapeFilledSquare: return "noteshapeFilledSquare";
      case noteshapeOpenSquare: return "noteshapeOpenSquare";
      case noteshapeFilledTriangle: return "noteshapeFilledTriangle";
      case noteshapeOpenTriangle: return "noteshapeOpenTriangle";
    }

    return 0;
}
/***************************************************************************/
/*
 * NIFFIOSymbolREST
 * ================
 * Return a symbolic name for a rest shape, NULL on failure
 */
const char *
NIFFIOSymbolREST(BYTE rs)
/***************************************************************************/
{
    switch(rs)
    {
      case restBreve: return "restBreve";
      case restWhole: return "restWhole";
      case restHalf: return "restHalf";
      case restQuarter: return "restQuarter";
      case restEighth: return "restEighth";
      case rest16th: return "rest16th";
      case rest32nd: return "rest32th";
      case rest64th: return "rest64th";
      case rest128th: return "rest128th";
      case rest256th: return "rest256th";
      case restMult4measures: return "restMult4Measures";
      case restMultHzBar: return "restMultHzBar";
      case restMultSlantedBar: return "restSlantedBar";
      case restVocalComma: return "restVocalComma";
      case restVocal2Slashes: return "restVocal2Slashes";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolLOGPLACEH
 * =====================
 * Return a symbolic name for a logical placement horizonal, NULL on failure
 */
const char *
NIFFIOSymbolLOGPLACEH(BYTE lh)
/***************************************************************************/
{
    switch(lh)
    {
      case logplaceHDefault: return  "logplaceHDefault";
      case logplaceHLeft: return     "logplaceHLeft";
      case logplaceHRight: return    "logplaceHRight";
      case logplaceHStemside: return "logplaceHStemside";
      case logplaceHNoteside: return "logplaceHNoteside";
      case logplaceHCentered: return "logplaceHCentered";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolLOGPLACEV
 * =====================
 * Return a symbolic name for a logical placement vertical, NULL on failure
 */
const char *
NIFFIOSymbolLOGPLACEV(BYTE lv)
/***************************************************************************/
{
    switch(lv)
    {
      case logplaceVDefault: return  "logplaceVDefault";
      case logplaceVAbove: return    "logplaceVAbove";
      case logplaceVBelow: return    "logplaceVBelow";
      case logplaceVStemside: return "logplaceVStemside";
      case logplaceVNoteside: return "logplaceVNoteside";
      case logplaceVCentered: return "logplaceVCentered";
    }

    return 0;
}

/***************************************************************************/
/*
 * NIFFIOSymbolLOGPLACEPROX
 * ========================
 * Return a symbolic name for a logical placement proximity, NULL on failure
 */
const char *
NIFFIOSymbolLOGPLACEPROX(BYTE lp)
/***************************************************************************/
{
    switch(lp)
    {
      case logplaceProxDefault: return  "logplaceProxDefault";
      case logplaceProxAligned: return  "logplaceProxAligned";
      case logplaceProxOffset:  return  "logplaceProxOffset";
    }

    return 0;
}
