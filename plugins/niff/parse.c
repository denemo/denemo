#ifndef lint
/*static char rcsid[] =
"$Header: /sources/denemo/denemo/plugins/niff/parse.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * parse - Functions to create a new parser and parse a NIFF file.
 * 
 * SYNOPSIS
 * ========
 * 
 * - NIFFIOParserNew()
 * - NIFFIOParserDelete()
 *
 * - NIFFIOParserSetTracing()
 * - NIFFIOParserGetTracing()
 *
 * - NIFFIOParseFile()
 *
 */
/***************************************************************************/

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <niff/niffio.h>
#include <niff/niffiop.h>


static RIFFIOSuccess
NIFFIOParseForm(NIFFIOParser *pparser, NIFFIOChunkContext *pctxChunk);

static RIFFIOSuccess
NIFFIOParseList(NIFFIOParser *pparser, NIFFIOChunkContext *pctxChunk);

static RIFFIOSuccess
NIFFIOParseTaggedChunk(NIFFIOParser *pparser, NIFFIOChunkContext *pctxChunk);
                                                        
static RIFFIOSuccess
NIFFIOParseAtomicChunk(NIFFIOParser *pparser, NIFFIOChunkContext *pctxChunk);

static RIFFIOSuccess
NIFFIOParseUserChunk(NIFFIOParser *pparser, NIFFIOChunkContext *pctxChunk);

static RIFFIOSuccess
NIFFIOParseTag(NIFFIOParser *pparser, NIFFIOTagContext *pctxTag);

static RIFFIOSuccess
NIFFIOParseUserTag(NIFFIOParser *pparser, NIFFIOTagContext *pctxTag);


/*
 * _DefaultParserTracer
 * ====================
 * We provide a default tracing routine when parser tracing is 
 * enabled.
 *
 * Write a message to standard error
 *
 */
static void
_DefaultParserTracer(const char *strParser,
                     const unsigned nLevel,
                     const char *strFormat,/*vprintf compatible format string*/
                     ...)                  /* args to vprintf */
{
    va_list ap;
    
    va_start(ap, strFormat);
    
    (void) printf("%s:", strParser);
    (void) vprintf(strFormat, ap);
    (void) printf("\n");
    
    va_end(ap);
    
}

/***************************************************************************/
/*
 * NIFFIOParserNew
 * ===============
 * Return a pointer to a new parser, or null on failure
 */
NIFFIOParser *
NIFFIOParserNew(void)
/***************************************************************************/
{
    struct NIFFIOPParser *pparserNew;

    /* Allocate the parser structure */
    pparserNew = (struct NIFFIOPParser *) malloc(sizeof(struct NIFFIOPParser));
    if (pparserNew == 0)
        return pparserNew;

    /*
     * Initialize the parser entries
     */

    /*
     * Install the default parser tracer
     */
    (void) strcpy(pparserNew->strName, "NIFFIOParser");
    pparserNew->isTracing = 1;
    pparserNew->tracer = _DefaultParserTracer;

    /*
     * Initialize callbacks 
     */
    pparserNew->formentry.cbStart = 0;
    pparserNew->formentry.cbEnd = 0;

    pparserNew->defaultList.cbStart = 0;
    pparserNew->defaultList.cbEnd = 0;

    pparserNew->defaultTaggedChunk.cbStart = 0;
    pparserNew->defaultTaggedChunk.cbEnd = 0;

    pparserNew->defaultAtomicChunk.cb = 0;
    pparserNew->defaultTag.cbTag = 0;

    /* 
     * Prepare for failure to allocate tables
     */
    pparserNew->pfcctblLists = 0;
    pparserNew->pfcctblTaggedChunks = 0;
    pparserNew->pfcctblAtomicChunks = 0;
    pparserNew->ptcbtblTags = 0;

    pparserNew->pvChunkBuffer = 0;
    pparserNew->pvTagBuffer = 0;

    /* Allocate a FOURCC Table for list chunks */
    pparserNew->pfcctblLists = RIFFIOFCCTableNew();
    if (pparserNew->pfcctblLists == 0)
        goto ParserNewCleanup;
        
    /* Allocate a FOURCC Table for chunks */
    pparserNew->pfcctblTaggedChunks = RIFFIOFCCTableNew();
    if (pparserNew->pfcctblTaggedChunks == 0)
        goto ParserNewCleanup;
        
    /* Allocate a FOURCC Table for Atomic chunks */
    pparserNew->pfcctblAtomicChunks = RIFFIOFCCTableNew();
    if (pparserNew->pfcctblAtomicChunks == 0)
        goto ParserNewCleanup;

    /* Allocate Tag Callback Table for tags */
    pparserNew->ptcbtblTags = NIFFIOPTCBTableNew();
    if (pparserNew->ptcbtblTags == 0)
        goto ParserNewCleanup;

    /* Allocate buffers to hold chunks and tags that we read */
    /* We will only ever store one of each at a time         */
    pparserNew->pvChunkBuffer = malloc(NIFFIOP_DEFAULT_CHUNKBUFFER);
    if (pparserNew->pvChunkBuffer == 0)
        goto ParserNewCleanup;

    pparserNew->pvTagBuffer = malloc(NIFFIOP_DEFAULT_TAGBUFFER);
    if (pparserNew->pvTagBuffer == 0)
        goto ParserNewCleanup;

    /* 
     * Success 
     */
    return pparserNew;

ParserNewCleanup:

    if (pparserNew->pfcctblLists)
        RIFFIOFCCTableDelete(pparserNew->pfcctblLists);

    if (pparserNew->pfcctblLists)
        RIFFIOFCCTableDelete(pparserNew->pfcctblTaggedChunks);

    if (pparserNew->pfcctblLists)
        RIFFIOFCCTableDelete(pparserNew->pfcctblAtomicChunks);

    if (pparserNew->ptcbtblTags)
        NIFFIOPTCBTableDelete(pparserNew->ptcbtblTags);

    if (pparserNew->pvChunkBuffer)
        free(pparserNew->pvChunkBuffer);

    if (pparserNew->pvTagBuffer)
        free(pparserNew->pvTagBuffer);
 
    free(pparserNew);

    return 0;

}

/***************************************************************************/
/*
 * NIFFIOParserDelete
 * ==================
 * Free the memory allocated to a parser.
 */
void
NIFFIOParserDelete(NIFFIOParser *pparser)
/***************************************************************************/
{
    assert(pparser != 0);

    RIFFIOFCCTableFreeEntries(pparser->pfcctblLists);
    RIFFIOFCCTableDelete(pparser->pfcctblLists);

    RIFFIOFCCTableFreeEntries(pparser->pfcctblTaggedChunks);
    RIFFIOFCCTableDelete(pparser->pfcctblTaggedChunks);

    RIFFIOFCCTableFreeEntries(pparser->pfcctblAtomicChunks);
    RIFFIOFCCTableDelete(pparser->pfcctblAtomicChunks);

    NIFFIOPTCBTableDelete(pparser->ptcbtblTags);

    free(pparser->pvChunkBuffer);
    free(pparser->pvTagBuffer);

    free(pparser);
}

/***************************************************************************/
/*
 * NIFFIOParserSetTracing
 * ======================
 * Enable (or disable) built-in parser tracing
 */
void
NIFFIOParserSetTracing(NIFFIOParser *pparser, int isTracing)
/*
 * Parser tracing will be enabled according to the boolean value
 * of <isTracing>.
 */
/***************************************************************************/
{
    assert(pparser);

    pparser->isTracing = (isTracing ? 1 : 0);
}

/***************************************************************************/
/*
 * NIFFIOParserGetTracing
 * ======================
 * Return true if a parser has tracing enabled.
 */
int
NIFFIOParserGetTracing(NIFFIOParser *pparser)
/***************************************************************************/
{
    assert(pparser);

    return pparser->isTracing;

}

/***************************************************************************/
/*
 * NIFFIOParseFile
 * ===============
 * Parse a NIFF file.
 */
RIFFIOSuccess
NIFFIOParseFile(NIFFIOParser *pparser, 
                NIFFIOFile *pnf, 
                NIFFIOUserContext userctxIn,
                NIFFIOUserContext *puserctxOut)
/*
 * Performs a recursive descent scan of all the chunks and tags in a 
 * NIFF file. Each chunk and tag that matches a registered parser callback
 * generates a callback as it is scanned.  Chunks and tags are processed
 * in the order they appear in the NIFF file.
 * 
 * ENTRY
 * -----
 *
 * - T <*pnf> must be positioned at the start of the NIFF Form.
 *
 * - T <usercntxIn> describes the top level "context".  It will be the
 *     parent context of the Form chunk. It may be null.
 * 
 * EXIT
 * ----
 * T <*puserctxOut> will be filled in with the context returned by
 * the form callbacks. 
 * If <puserctxOut> is null then it is ignored and won't be dereferenced.
 * 
 * RETURN
 * ------ 
 *   RIFFIO_OK :
 *       if the file parses without error,
 *
 *   RIFFIO_FAIL :
 *       otherwise
 * 
 * ERRORS
 * ------
 * If any callback generates an error, then the NIFF object that corresponds
 * to that callback is skipped.  That means if a list's start callback fails,
 * none of the subchunks in the list will be scanned.  If a list's end callback
 * fails, NIFFIOParseFile will eventually return an error, but it is a
 * little late to do much else. 
 */
 
/***************************************************************************/
{
    char strModule[] = "NIFFIOParseFile";
    RIFFIOSuccess success;

    RIFFIOChunk chunkForm;              /* the niff form */

    char strId[5];                      /* form id, eg. RIFF or RIFX */
    char strType[RIFFIO_FOURCC_LIM];    /* form type, eg. NIFF */

    NIFFIOChunkContext ctxForm;         /* context of the form */

    assert(pparser != 0);
    assert(pnf != 0);
        

    /*
     * Read the form header
     */
    if (! NIFFIOChunkDescend(pnf, &chunkForm))
    {
        RIFFIOError(strModule, "Failed to descend into form");
        return RIFFIO_FAIL;
    }

   
    /*
     * Make sure this is a valid NIFF form 
     */
    RIFFIOFOURCCToString(chunkForm.fccId, strId);
    if (chunkForm.fccId != RIFFIO_FOURCC_RIFX)
    {
        RIFFIOError(strModule, "Expected form id <RIFX>, got <%s>", strId);
        return RIFFIO_FAIL;
    }

    RIFFIOFOURCCToString(chunkForm.fccType, strType);
    if (chunkForm.fccType != niffformNiff)
    {
        RIFFIOError(strModule, "Expected form type <NIFF>, got <%s>", strType);
        return RIFFIO_FAIL;
    }

    /*
     * Initialize the form context 
     */
    ctxForm.nLevel = 0;
    ctxForm.pnf = pnf;
    ctxForm.pchunk = &chunkForm;
    ctxForm.ctxParent = userctxIn;

    success = NIFFIOParseForm(pparser, &ctxForm); 

    /* 
     * Return the form callback's user context 
     * if it was requested 
     */
    if (puserctxOut)
    {
        *puserctxOut = ctxForm.ctxMe;
    }

    /*
     * Position the file after the NIFF form
     */
    if (! NIFFIOChunkAscend(pnf, &chunkForm))
    {
        RIFFIOError(strModule, "Failed to advance past NIFF form");
        return RIFFIO_FAIL;
    }

    return success;

}

/*
 * NIFFIOParseForm
 * ===============
 * Parse a Niff form according to a specific parser
 * 
 * ENTRY
 * -----
 *   - T <*pparser>'s file is positioned at the start of the form
 *
 *   - T <pctxForm> is a pointer to a user-defined "context"
 */ 
static
RIFFIOSuccess
NIFFIOParseForm(NIFFIOParser *pparser,
                NIFFIOChunkContext *pctxForm)
{
    char strModule[] = "NIFFIOParseForm";

    RIFFIOSuccess success; /* return value */


    NIFFIOChunkContext ctxFormItem; /* contexts of the form's subchunks */
    RIFFIOChunk chunkFormItem;      /* each subchunk in the form        */

    char strId[RIFFIO_FOURCC_LIM];
    char strType[RIFFIO_FOURCC_LIM];

    assert(pparser != 0);
    assert(pctxForm != 0);
    assert(pctxForm->pnf != 0);
    assert(pctxForm->pchunk != 0);
    assert(pctxForm->pchunk->fccId == RIFFIO_FOURCC_RIFX);
    assert(pctxForm->pchunk->fccType == niffformNiff);

    /*
     * Allow subchunk parsers to override return status 
     */
    success = RIFFIO_OK;

    /*
     * Initialize the FOURCC strings for messages
     */
    RIFFIOFOURCCToString(pctxForm->pchunk->fccId, strId);
    RIFFIOFOURCCToString(pctxForm->pchunk->fccType, strType);

    /*
     * Initialize the context for each form item 
     */
    ctxFormItem.nLevel = pctxForm->nLevel + 1;
    ctxFormItem.pnf = pctxForm->pnf;
    ctxFormItem.pchunk = &chunkFormItem;
    ctxFormItem.ctxParent = pctxForm->ctxMe;

    /* 
     * Log the Form
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxForm->nLevel, 
                        "FORM:id <%s>, type <%s>, size <%lu>\n", 
                        strId, strType, pctxForm->pchunk->sizeData);

    /*
     * Make the start callback, if it is non-null
     */
    if (pparser->formentry.cbStart)
        if (! pparser->formentry.cbStart(pctxForm))
        {
            RIFFIOError(strModule, "Form start callback failed");
            return RIFFIO_FAIL;
        }

    /*
     * Parse each list item in the form
     */
    while (! NIFFIOChunkEnd(pctxForm->pnf, pctxForm->pchunk))/* Not form end */
    {
        /* 
         * Invariant: We are poised to read a chunk
         */

        /*
         * Get the next form item chunk
         */
        if (! NIFFIOChunkDescend(pctxForm->pnf, &chunkFormItem))
        {
            RIFFIOError(strModule, "Failed to descend into form item chunk");
            success = RIFFIO_FAIL;
            break;
        }
                
        /*
         * Parse the form item chunk
         * I only know of lists that are subchunks of Niff forms
         */
        if (RIFFIOChunkIsList(&chunkFormItem))
        {
            /*
             * Is the list type a valid FOURCC ?
             */
            if (! RIFFIOFOURCCIsValid(chunkFormItem.fccType))
            {
                /* yes, get out of the list item loop */

                char strFormItemType[RIFFIO_FOURCC_LIM]; 
                RIFFIOFOURCCToString(chunkFormItem.fccType, strFormItemType);

                RIFFIOError(strModule, 
                            "Found invalid list type FOURCC, type <%s>", 
                            strFormItemType);
                success = RIFFIO_FAIL;
                break;
            }

            /* 
             * Parse it!
             */
            if (! NIFFIOParseList(pparser, &ctxFormItem))
                success = RIFFIO_FAIL;
        }
        else
        {
            RIFFIOError(strModule, "Form sub-chunk not a list");
            success = RIFFIO_FAIL;
        }
        /*
         * Position the file after the form item chunk
         */             
        if (! NIFFIOChunkAscend(pctxForm->pnf, &chunkFormItem))
        {
            RIFFIOError(strModule, "Failed to advance past form item chunk");
            success = RIFFIO_FAIL;
            break;
        }
    }
        
    /*
     * Make the form end callback, if it is non-null
     */
    if (pparser->formentry.cbEnd)
        if (! pparser->formentry.cbEnd(pctxForm))
        {
            RIFFIOError(strModule, "Form end callback failed");
            return RIFFIO_FAIL;
        }

    return success;

}


/*
 * NIFFIOParseList
 * ===============
 * Parse a list according to a specific parser
 * 
 * ENTRY
 * -----
 * - T <*pparser>'s file is positioned at the start of the list's 
 *   first subchunk
 * 
 * - T <pctxList> is a pointer to a user-defined "context"
 */ 
static
RIFFIOSuccess
NIFFIOParseList(NIFFIOParser *pparser,
                NIFFIOChunkContext *pctxList)
{
    char                strModule[] = "NIFFIOParseList";
    char                strType[RIFFIO_FOURCC_LIM];  /* String version of our 
                                                        list type FOURCC */
    
    RIFFIOSuccess       success;      /* return success value */

        
    RIFFIOFCCTableEntry entry;        /* interface to RIFFIOFCCTableLookup */
    NIFFIOPListEntry    *pListEntry;  /* callbacks found in table */

    int  isListRegistered;            /* True if we find a match in our 
                                       * list callback table */

    NIFFIOChunkContext ctxListItem;   /* The contexts of our subchunks */
    RIFFIOChunk        chunkListItem; /* our subchunks */

    /* Check the arguments */
    assert (pparser != 0);
    assert (pparser->pfcctblLists != 0);
    assert (pctxList !=0);
    assert (pctxList->pnf != 0);
    assert (pctxList->pchunk != 0);
    assert (pctxList->pchunk->fccId == RIFFIO_FOURCC_LIST);

    /*
     * Assume the best
     */
    success = RIFFIO_OK;

    RIFFIOFOURCCToString(pctxList->pchunk->fccType, strType);

    /*
     * Initialize the context for our subchunks
     */
    ctxListItem.nLevel = pctxList->nLevel + 1;
    ctxListItem.pnf = pctxList->pnf;
    ctxListItem.pchunk = &chunkListItem;
    ctxListItem.ctxParent = pctxList->ctxMe;

    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxList->nLevel, 
                        "LIST: type <%s>, size <%lu>\n", 
                        strType, pctxList->pchunk->sizeData);


    /*
     * Is the list type registered?
     */
    entry.fcc = pctxList->pchunk->fccType;
    isListRegistered = RIFFIOFCCTableLookup(pparser->pfcctblLists, &entry);

    /*
     * If the list is registered, then make a non-null start callback
     * else make a non-null default start callback
     */
    if (isListRegistered)
    {
        pListEntry = (NIFFIOPListEntry *)entry.l;
                
        if (pListEntry->cbStart != 0)
            if (! pListEntry->cbStart(pctxList))
            {
                RIFFIOError(strModule, "List start callback failed");
                return RIFFIO_FAIL;
            }
    }
    else
    {
        if (pparser->defaultList.cbStart != 0)
            if (! ( * pparser->defaultList.cbStart) (pctxList))
            {
                RIFFIOError(strModule, "Default list start callback failed");
                return RIFFIO_FAIL;
            }
    }

    /*
     * Parse each chunk in the list 
     */

    /* While not at list end */
    while (! NIFFIOChunkEnd(pctxList->pnf, pctxList->pchunk)) 
    {
        /*
         * Invariant: We are at the beginning of 
         *            a chunk (list or chunk)
         */

        if (! NIFFIOChunkDescend(pctxList->pnf, &chunkListItem))
        {
            RIFFIOError(strModule, 
                        "Failed to descend into list item chunk, type <%s>",
                        strType);
            success = RIFFIO_FAIL;
            break;
        }
                
        if (RIFFIOChunkIsList(&chunkListItem))
        {
            /*
             * Parse a List
             */

            /*
             * Is the list type a valid FOURCC ?
             */
            if (! RIFFIOFOURCCIsValid(chunkListItem.fccType))
            {
                /* yes, get out of the list item loop */

                char strListItemType[RIFFIO_FOURCC_LIM];

                RIFFIOFOURCCToString(chunkListItem.fccType, strListItemType);
                RIFFIOError(strModule, 
                            "Found invalid list type FOURCC, type <%s>", 
                            strListItemType);
                success = RIFFIO_FAIL;
                break;
            }

            if (! NIFFIOParseList(pparser, &ctxListItem))
                success = RIFFIO_FAIL;
        }
        else
        {
            /* 
             * Parse a chunk
             *
             * bogus id?     break out of while
             * User chunk?   call NIFFIOParseUserChunk
             * Atomic chunk? call NIFFIOParseAtomicChunk
             * Otherwise     call NIFFIOParseTaggedChunk
             */

            /*
             * Is the chunk id a valid FOURCC ?
             */
            if (! RIFFIOFOURCCIsValid(chunkListItem.fccId))
            {
                char strListItemId[RIFFIO_FOURCC_LIM];
                                
                RIFFIOFOURCCToString(chunkListItem.fccId, strListItemId);
                RIFFIOError(strModule, "Found invalid FOURCC, id <%s>", 
                            strListItemId);
                success = RIFFIO_FAIL;
                break;
            }

            if (chunkListItem.fccId == niffckidUserDefined )
            {
                if (! NIFFIOParseUserChunk(pparser, &ctxListItem)) 
                    success = RIFFIO_FAIL;
            }
            else
            {
                /*
                 * Assume all chunks are Atomic until we have a CLT 
                 */

                if ((NIFFIOFileGetCLT(pctxList->pnf) == 0)
                    || NIFFIOPChunkIsAtomic(pctxList->pnf->pclt, 
                                            chunkListItem.fccId))
                {
                    if (! NIFFIOParseAtomicChunk(pparser, &ctxListItem))
                    { success = RIFFIO_FAIL; }
                }
                else
                {
                    if (! NIFFIOParseTaggedChunk(pparser, &ctxListItem))
                    { success = RIFFIO_FAIL;} 
                } /* else tagged chunk */
            } /* if user chunk */
        } /* if list chunk */
                
        /* 
         * Position the file to read the next list item chunk
         */
        if (! NIFFIOChunkAscend(pctxList->pnf, &chunkListItem))
        {
            RIFFIOError(strModule, 
                        "Failed to move past list item chunk, type <%s>",
                        strType);
            success = RIFFIO_FAIL;
            break;
        }
    }

    /*
     * If the list is registered then make a non-null list end callback, 
     * else make a non-null default list end callback
     */
    if (isListRegistered)
    { 
        if (pListEntry->cbEnd != 0)
            if (! pListEntry->cbEnd(pctxList))
            {
                RIFFIOError(strModule, "List end callback failed");
                return RIFFIO_FAIL;
            }
    }
    else
    {
        if (pparser->defaultList.cbEnd != 0)
            if ( !(* pparser->defaultList.cbEnd)(pctxList))
            {
                RIFFIOError(strModule, "Default list end callback failed");
                return RIFFIO_FAIL;
            }
    }

    /* 
     * Leave the file positioned after the list
     */
    if (! NIFFIOChunkAscend(pctxList->pnf, pctxList->pchunk))
    {
        RIFFIOError(strModule, "Failed to move past list type <%s>", strType);
        return RIFFIO_FAIL;
    }

    return success;

}

/*
 * NIFFIOParseTaggedChunk
 * ======================
 * Parse a compound chunk (might have tags)
 */
static
RIFFIOSuccess
NIFFIOParseTaggedChunk(NIFFIOParser *pparser,
                       NIFFIOChunkContext *pctxChunk)
{
    char          strModule[] = "NIFFIOParseTaggedChunk";
    char          strId[RIFFIO_FOURCC_LIM];
    RIFFIOSuccess success;           /* Success return value */

    RIFFIOFCCTableEntry      entry;  /* interface to RIFFIOFCCTableLookup() */

    NIFFIOPTaggedChunkEntry *pTaggedChunkEntry; /* our looked-up callbacks  */


    int isRegistered;          /* True if we find a match in our
                                  callback table */

    NIFFIOTagContext ctxTag;   /* context of each of our child tags */
    NIFFIOTag tag;             /* each of our child tags */
                        

    /* Check the arguments */
    assert (pparser != 0);
    assert (pparser->pfcctblTaggedChunks != 0);
    assert (pctxChunk != 0);
    assert (pctxChunk->pnf != 0);
    assert (pctxChunk->pchunk != 0);
    assert (pctxChunk->pchunk->fccId != RIFFIO_FOURCC_LIST);
   
    /*
     * We will return OK as long as each child user-defined tag
     * parses OK.  All normal tags parse ok as long as we can 
     * descend and ascend them.
     * Lets be optimistic
     */
    success = RIFFIO_OK;

    RIFFIOFOURCCToString(pctxChunk->pchunk->fccId, strId);

    /*
     * Initialize the context for each child tag
     */
    ctxTag.nLevel = pctxChunk->nLevel + 1;
    ctxTag.pnf = pctxChunk->pnf;
    ctxTag.ptag = &tag;
    ctxTag.pchunkParent = pctxChunk->pchunk;
    ctxTag.ctxParent = pctxChunk->ctxMe;

    /*
     * Log the chunk
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxChunk->nLevel, 
                        "CHUNK: id <%s>, size <%lu>\n", 
                        strId, pctxChunk->pchunk->sizeData);

    /*
     * Is the chunk id a valid FOURCC ?
     */
    if (! RIFFIOFOURCCIsValid(pctxChunk->pchunk->fccId))
    {
        RIFFIOError(strModule, "Found invalid FOURCC, id <%s>", strId);
        return RIFFIO_FAIL;
    }

    /*
     * Is the chunk id registered?
     */
    entry.fcc = pctxChunk->pchunk->fccId;

    isRegistered = RIFFIOFCCTableLookup(pparser->pfcctblTaggedChunks, &entry);

    /*
     * If the chunk is registered then make a non-null chunk start callback
     * else make a non-null default chunk start callback
     */
    if (isRegistered)
    {
        pTaggedChunkEntry = (NIFFIOPTaggedChunkEntry *)entry.l;

        /* 
         * Read the chunk structure if a reader is provided
         */
        if (pTaggedChunkEntry->cbRead)
        {
            if (! (*pTaggedChunkEntry->cbRead)(pctxChunk->pnf, 
                                               pparser->pvChunkBuffer))
            {
                RIFFIOError(strModule, "Failed to read chunk structure");
                return RIFFIO_FAIL;
            }
                
            if (pTaggedChunkEntry->cbStart.cooked != 0)
            {
                if (! (*pTaggedChunkEntry->cbStart.cooked)
                    (pctxChunk, pparser->pvChunkBuffer))
                {
                    RIFFIOError(strModule, 
                                "Cooked chunk start callback failed");
                    return RIFFIO_FAIL;
                }
            }
        }
        else
        {
            if (pTaggedChunkEntry->cbStart.raw != 0)
                if (!  (*pTaggedChunkEntry->cbStart.raw)(pctxChunk))
                {
                    RIFFIOError(strModule, "Raw chunk start callback failed");
                    return RIFFIO_FAIL;
                }

        }
    }
    else
    {
        if (pparser->defaultTaggedChunk.cbStart != 0)
            if (! (* pparser->defaultTaggedChunk.cbStart)(pctxChunk))
            {
                RIFFIOError(strModule, "Default chunk start callback failed");
                return RIFFIO_FAIL;
            }
    }

    /* 
     * Seek to the first tag
     */
    if (! NIFFIOSeekChunkTags(pctxChunk->pnf, pctxChunk->pchunk))
    {
        RIFFIOError(strModule, 
                    "Failed to seek to first tag of chunk, id <%s>",
                    strId);
        success = RIFFIO_FAIL;
    }
    else
    {
        /*
         * Parse each tag in the chunk 
         */
        while (! NIFFIOChunkDataEnd(pctxChunk->pnf, pctxChunk->pchunk))
        {
            /*
             * Invariant: We are at the beginning of a tag
             */
                        
            /*
             * Read the tag
             */
            if (! NIFFIOTagDescend(pctxChunk->pnf, &tag))
            {
                RIFFIOError(strModule, 
                            "Failed to descend into chunk tag, id <%s>",
                            strId);
                success = RIFFIO_FAIL;
                break;
            }
                        
            /*
             * Parse the tag
             *
             * User tag? call NIFFIOParseUserTag
             * Normal tag? call NIFFIOParseTag
             */
            if (tag.tagid == nifftagUserDefined)
            {
                if (! NIFFIOParseUserTag(pparser, &ctxTag))
                { success = RIFFIO_FAIL;}
            }
            else
            {
                if (! NIFFIOParseTag(pparser, &ctxTag))
                { success = RIFFIO_FAIL; }
            }

            /* 
             * Position the file to read the next tag
             */
            if (! NIFFIOTagAscend(pctxChunk->pnf, &tag))
            {
                RIFFIOError(strModule, 
                            "Failed to move past chunk tag, id <%s>",
                            strId);
                success = RIFFIO_FAIL;
                break;
            } /* if TagAscend */
        } /* while */
    } /* if SeekChunkTags */

    /*
     * If the chunk is registered then make a non-null chunk end callback
     * else make a non-null default chunk end callback
     */
    if (isRegistered)
    { 
        /* Raw or cooked? */
        if (pTaggedChunkEntry->cbRead)
        {
            /* Cooked */
            if (pTaggedChunkEntry->cbEnd.cooked != 0)
                if (! pTaggedChunkEntry->cbEnd.cooked(pctxChunk,
                                                      pparser->pvChunkBuffer))
                {
                    RIFFIOError(strModule, "Cooked chunk end callback failed");
                    return RIFFIO_FAIL;
                }
        }  /* cooked */
        else
        {
            /* Raw */
            if (pTaggedChunkEntry->cbEnd.raw != 0)
                if (! pTaggedChunkEntry->cbEnd.raw(pctxChunk))
                {
                    RIFFIOError(strModule, "Raw chunk end callback failed");
                    return RIFFIO_FAIL;
                }
        } /* raw */
    }
    else
    {
        /* Not registered */

        if (pparser->defaultTaggedChunk.cbEnd != 0)
            if (! (* pparser->defaultTaggedChunk.cbEnd)(pctxChunk))
            {
                RIFFIOError(strModule, "Default chunk end callback failed");
                return RIFFIO_FAIL;
            }
    }

    /* 
     * Leave the file positioned after the chunk
     */
    if (! NIFFIOChunkAscend(pctxChunk->pnf, pctxChunk->pchunk))
    {
        RIFFIOError(strModule, "Failed to move past chunk id <%s>", strId);
        return RIFFIO_FAIL;
    }

    return success;

}

/*
 * NIFFIOParseAtomicChunk
 * ======================
 * Parse an Atomic chunk (no tags allowed)
 */
static
RIFFIOSuccess
NIFFIOParseAtomicChunk(NIFFIOParser *pparser,
                       NIFFIOChunkContext *pctxChunk)
{
    char  strModule[] = "NIFFIOParseAtomicChunk";
    char  strId[RIFFIO_FOURCC_LIM];
        
    RIFFIOFCCTableEntry entry; /* Interface to RIFFIOFCCTableLookup */
    NIFFIOPAtomicChunkEntry *pAtomicChunkEntry; /* Value of entry looked up */

    int isRegistered;  /* True if we matched an entry in the callback table */

    /* Check the arguments */
    assert (pparser != 0);
    assert (pparser->pfcctblAtomicChunks != 0);
    assert (pctxChunk != 0);
    assert (pctxChunk->pnf != 0);
    assert (pctxChunk->pchunk != 0);
    assert (pctxChunk->pchunk->fccId != RIFFIO_FOURCC_LIST);
   
    RIFFIOFOURCCToString(pctxChunk->pchunk->fccId, strId);

    /*
     * Log the chunk
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxChunk->nLevel, 
                        "ATOMIC CHUNK: id <%s>, size <%lu>\n", 
                        strId, pctxChunk->pchunk->sizeData);

    /*
     * Is the chunk id a valid FOURCC ?
     */
    if (!RIFFIOFOURCCIsValid(pctxChunk->pchunk->fccId))
    {
        RIFFIOError(strModule, "Found invalid FOURCC, id <%s>", strId);
        return RIFFIO_FAIL;
    }

    /* 
     * Is the chunk id registered ?
     */
    entry.fcc = pctxChunk->pchunk->fccId;
    isRegistered = RIFFIOFCCTableLookup(pparser->pfcctblAtomicChunks, &entry);

    /*
     * If the chunk is registered then make a non-null callback
     * else make a non-null default callback
     */
    if (isRegistered)
    {
        pAtomicChunkEntry = (NIFFIOPAtomicChunkEntry *)entry.l;
                
        if (pAtomicChunkEntry->cb)
            if (! pAtomicChunkEntry->cb(pctxChunk))
            {
                RIFFIOError(strModule, "Chunk callback failed");
                return RIFFIO_FAIL;
            }
    }
    else
    {
        if (pparser->defaultAtomicChunk.cb != 0)
            if (! (* pparser->defaultAtomicChunk.cb) (pctxChunk))
            {
                RIFFIOError(strModule, "Default chunk callback failed");
                return RIFFIO_FAIL;
            }

    }

    /* 
     * For chunk length tables
     * Make sure the NIFFIOFile structure knows about the CLT
     */
    if ( /* We are parsing a CLT and the NIFFIOFile lacks a CLT */
        (pctxChunk->pchunk->fccId == niffckidChnkLenTable)
        && 
        (NIFFIOFileGetCLT(pctxChunk->pnf) == 0)) 
    {
        if (! NIFFIOFileReadCLT(pctxChunk->pnf, pctxChunk->pchunk))
        {
            RIFFIOError(strModule, "Failed to read chunk length table");
            return RIFFIO_FAIL;
        }
        assert(NIFFIOFileGetCLT(pctxChunk->pnf));

    } 
 
    return RIFFIO_OK;

}

/*
 * NIFFIOPChunkIsAtomic
 * ====================
 * Use a chunk length table to decide if a chunk type
 * refers to an Atomic (tags disallowed) or compound (tagful) chunk.
 */
int
NIFFIOPChunkIsAtomic(RIFFIOFCCTable *pclt, FOURCC fccId)
{
    RIFFIOSuccess successLookup; /* was the fccId found in the clt ? */
    niffChklentabEntry cltEntry; /* CLT entry corresponding to fccId */

    assert (pclt);

    /*
     * Lookup the FOURCC in the chunk length table 
     */
    cltEntry.chunkName = fccId;

    successLookup = NIFFIOCLTLookup(pclt, &cltEntry);

        
    /* 
     * Did we find the chunk id in the CLT ?
     */
    if (! successLookup)
    {
        /* nope */
        RIFFIOError("NIFFIOPChunkIsAtomic", 
                    "Failed to find entry in chunk length table");
    }
    
    return (cltEntry.offsetOfFirstTag == -1);
        
}

/*
 * NIFFIOParseUserChunk
 * ====================
 * Parse a user-defined chunk
 * TWB - callbacks not implemented 
 */
static
RIFFIOSuccess
NIFFIOParseUserChunk(NIFFIOParser *pparser, 
                     NIFFIOChunkContext *pctxChunk)
{
    char strModule[] = "NIFFIOParseUserChunk";
    unsigned short userid;  /* The user-defined identifier */

    assert (pparser != 0);
    assert (pctxChunk != 0);
    assert (pctxChunk->pnf != 0);
    assert (pctxChunk->pchunk != 0);
    assert (pctxChunk->pchunk->fccId == niffckidUserDefined);

    /*
     * Read the user id
     */
    if (! NIFFIORead16(pctxChunk->pnf, &userid))
    {
        RIFFIOError(strModule, "Failed to read user id");
        return RIFFIO_FAIL;
    }

    /*
     * Log the user chunk
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxChunk->nLevel, 
                        "USER CHUNK: user id <%hd>, size <%lu>\n", 
                        userid, pctxChunk->pchunk->sizeData);
        
    /*
     * Make the user chunk callback
     * NOT IMPLEMENTED
     */

    return RIFFIO_OK;
}

/*
 * NIFFIOParseTag
 * ==============
 * Parse a Tag
 */
static
RIFFIOSuccess
NIFFIOParseTag(NIFFIOParser *pparser,
               NIFFIOTagContext *pctxTag)
{
    char strModule[] = "NIFFIOParseTag";
    NIFFIOPTagEntry tagentry;  /* our tag callbacks */
    RIFFIOSuccess successLookup;

    assert (pparser != 0);
    assert (pparser->ptcbtblTags != 0);
    assert (pctxTag != 0);
    assert (pctxTag->pnf != 0);
    assert (pctxTag->ptag != 0);
    assert (pctxTag->pchunkParent != 0);

    /*
     * Log the tag
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxTag->nLevel, 
                        "TAG: id <%u>, size <%u>\n", 
                        pctxTag->ptag->tagid, 
                        pctxTag->ptag->tagsizeData);
        
    /*
     * Lookup the tag callback using the supplied parent FOURCC
     */
    tagentry.fcc = pctxTag->pchunkParent->fccId;
    successLookup = NIFFIOPTCBTableLookup(pparser->ptcbtblTags, 
                                          pctxTag->ptag->tagid, 
                                          &tagentry);

    /*
     * If we didn't find and entry then 
     * Lookup the tag callback using a wildcard FOURCC
     */
    if (! successLookup)
    {
        tagentry.fcc = NIFFIO_FOURCC_WILDCARD;
        successLookup = NIFFIOPTCBTableLookup(pparser->ptcbtblTags, 
                                              pctxTag->ptag->tagid, 
                                              &tagentry);
    }

    if (successLookup)
    {
        /* Found the callback */
        if (tagentry.cbRead)
        {
            if (! (* tagentry.cbRead)(pctxTag->pnf, pparser->pvTagBuffer))
            {
                RIFFIOError(strModule, "Failed to read tag structure");
                return RIFFIO_FAIL;
            }
                        
            if (tagentry.cbTag.cooked)
                if ((*tagentry.cbTag.cooked) (pctxTag, pparser->pvTagBuffer))
                    return RIFFIO_OK;
                else
                {
                    RIFFIOError(strModule, "Cooked tag callback failed");
                    return RIFFIO_FAIL;
                }

        }
        else
        {
            if  (tagentry.cbTag.raw)
                if ((*tagentry.cbTag.raw)(pctxTag))
                    return RIFFIO_OK;
                else
                {
                    RIFFIOError(strModule, "Raw tag callback failed");
                    return RIFFIO_FAIL;
                }

        }
        
    }

    /*
     * If we matched above we should have already returned.
     * As a last resort, try a default callback
     */
    if (pparser->defaultTag.cbTag != 0)
    {
        if ((* pparser->defaultTag.cbTag)(pctxTag))
            return RIFFIO_OK;
        else
        {
            RIFFIOError(strModule, "Default tag callback failed");
            return RIFFIO_FAIL;
        }

    }

    /* 
     * Found no matching callback for this tag, 
     * but that is OK.
     */
    return RIFFIO_OK;
}

/*
 * NIFFIOParseUserTag
 * ==================
 * Parse a user-defined Tag
 * TWB - Callbacks not implemented
 */
static
RIFFIOSuccess
NIFFIOParseUserTag(NIFFIOParser *pparser,
                   NIFFIOTagContext *pctxTag)
{
    char strModule[] = "NIFFIOParseTag";
    unsigned short userid;

    assert (pparser != 0);
    assert (pctxTag != 0);
    assert (pctxTag->pnf != 0);
    assert (pctxTag->ptag != 0);
    assert (pctxTag->ptag->tagid == nifftagUserDefined);
    assert (pctxTag->pchunkParent != 0);

    /*
     * Read the user id
     */
    if (! NIFFIORead16(pctxTag->pnf, &userid))
    {
        RIFFIOError(strModule, "Failed to read user id");
        return RIFFIO_FAIL;
    }

    /*
     * Log the user tag
     */
    if(pparser->isTracing && pparser->tracer)
        pparser->tracer(pparser->strName, pctxTag->nLevel, 
                        "USER TAG: user id <%hd>, size <%u>\n", 
                        userid, 
                        pctxTag->ptag->tagsizeData);
        
    /*
     * Make the user tag callback
     * NOT IMPLEMENTED
     */

    return RIFFIO_OK;


}


