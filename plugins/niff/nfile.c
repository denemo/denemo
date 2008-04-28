#ifndef lint
/*static char rcsid[] =
"$Id: nfile.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * nfile - NIFFIOFile routines
 *
 * SYNOPSIS
 * ========
 *
 * - NIFFIOFileDelete()
 *
 * - NIFFIOFileGetCLT()
 * - NIFFIOFileReadCLT()
 * - NIFFIOFileAdoptCLT()
 *
 * - NIFFIOSeekChunkTags()
 */
/***************************************************************************/

#include <assert.h>
#include <stdlib.h>
#include <stdio.h>

#include <niff/niffio.h>

NIFFIOFile::NIFFIOFile(FILE *fp)
{
	pclt = new RIFFIOFCCTable;
	rf =  new RIFFIOFile(fp);
}




/***************************************************************************/
/*
 * NIFFIOFileDelete
 * ================
 * Free the memory associated with a NIFFIOFile.
 */
void
NIFFIOFileDelete(NIFFIOFile *pnf)
/*
 * WARNING
 * ------- 
 * This also frees the memory of any RIFFIOFCCTable
 * that may have been associated with the NIFFIOFile 
 * using NIFFIOFileAdoptCLT().
 */
/***************************************************************************/
{
    assert (pnf != 0);

    /*
     * Delete the chunk length table
     * if it exists
     */
    if (pnf->pclt != 0)
          RIFFIOFCCTableDelete(pnf->pclt);
	delete (pnf->rf);

    delete(pnf);
}



/***************************************************************************/
/*
 * NIFFIOFileGetCLT
 * ================
 * Return a pointer to a NIFFIOFile's chunk length table.
 */
RIFFIOFCCTable *
NIFFIOFileGetCLT(NIFFIOFile *pnf)
/* 
 * RETURN
 * ------
 * null if the chunk length table has not be set or read yet. 
 *
 * OBLIGATIONS
 * -----------
 * This pointer may become invalid after calls to
 * other NIFFIO routines.  Such as what ?????
 *
 */
/***************************************************************************/
{
        assert(pnf != 0);
        return pnf->pclt;
} 

/***************************************************************************/
/*
 * NIFFIOFileReadCLT
 * =================
 * Read and set a NIFFIOFile's chunk length table.
 */
RIFFIOSuccess
NIFFIOFileReadCLT(NIFFIOFile *pnf, RIFFIOChunk *pchunkCLT)
/*
 * ENTRY
 * -----
 * <*pchunkCLT> :
 *    is a chunk length table chunk that has been
 *    returned by NIFFIOChunkDescend().
 * 
 * <*pnf> :
 *    may be positioned anywhere
 * 
 * EXIT
 * ----
 * Leaves the NIFFIOFile positioned after the chunk length table chunk.
 */
/***************************************************************************/
{
    char strModule[] = "NIFFIOFileReadCLT";

    RIFFIOFCCTable *pcltNew;
    niffChklentabEntry     niffcltentry;

    assert (pnf != 0);
    assert (pchunkCLT != 0);
        
    /*
     * Do we already have the CLT?
     */
    if (pnf->pclt != 0)
    {
       RIFFIOFCCTableDelete(pnf->pclt);
        pnf->pclt = 0;
    }

    /*
     * Seek to the first CLT entry
     */
    if (! NIFFIOChunkSeekData(pnf, pchunkCLT))
    {
        RIFFIOError(strModule, 
                    "Failed to seek to first chunk length table entry");
        return RIFFIO_FAIL;
    }
        
    pcltNew = NIFFIOCLTNew();
    if (pcltNew == 0)
        return RIFFIO_FAIL;

    /*
     * Read each entry
     */
    while (! NIFFIOChunkDataEnd(pnf, pchunkCLT))
    {
        if (!NIFFIOReadniffChklentabEntry(pnf, &niffcltentry))
        {
            RIFFIOError(strModule, "Failed to read chunk length table entry");
            RIFFIOFCCTableDelete(pcltNew);
            return RIFFIO_FAIL;
        }

        if (! NIFFIOCLTMakeEntry(pcltNew, niffcltentry))
        {
            RIFFIOError(strModule, 
                        "Failed to record chunk length table entry");
              RIFFIOFCCTableDelete(pcltNew);
            return RIFFIO_FAIL;
        }
    }

    /*
     * Leave the file positioned at the end of the chunk length table
     */

    if (! NIFFIOChunkAscend(pnf, pchunkCLT))
    {
       RIFFIOFCCTableDelete(pcltNew);
        return RIFFIO_FAIL;
    }

    pnf->pclt = pcltNew;


    return RIFFIO_OK;

}


/*
 * _CLTWriteHelper
 * ===============
 * Used by sort() to compare FOURCCs 
 *
 * Compare two chunk length table entries 
 * Return <0 if a comes before b
 *         0 if a is the same as b
 *        >0 if a comes after b
 *
 * The results of this function are 
 * compatible with bsearch() and qsort() 
 * in the Standard C Library
 */
static int
_CLTWriteHelper(const void *aEntryp, 
                                const void *bEntryp)
{
    assert(aEntryp != 0);
    assert(bEntryp != 0);
    
    return NIFFIOCompareFOURCC(
                               ((RIFFIOFCCTableEntry *) aEntryp)->fcc,
                               ((RIFFIOFCCTableEntry *) bEntryp)->fcc);
    
}

/***************************************************************************/
/*
 * NIFFIOFileAdoptCLT
 * ==================
 * Assign and write a chunk length table to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOFileAdoptCLT(NIFFIOFile *pnf, RIFFIOFCCTable *pclt)
/* 
 * ENTRY
 * -----
 * The NIFFIOFile must be positioned at the start of data
 * of its CLT chunk.
 *
 * EXIT
 * ----
 * Leaves the file positioned at the end of the CLT chunk data
 * that it just wrote.
 *
 * OBLIGATIONS
 * ----------- 
 * Upon return, the NIFFIOFile takes responsibility for
 * deleting the CLT you just gave it.  
 * Don't free or delete <*pclt>; that will be done by either another
 * call to NIFFIOFileAdoptCLT() or NIFFIOFileDelete().
 */
/***************************************************************************/
{

    char strModule[] = "NIFFIOFileAdoptCLT";
    niffChklentabEntry *pentry;
    int count;
  
    count = RIFFIOFCCTableCount(pclt);
     
        int i;
        char strId[5];

        for(i = 0; i < count; i++)
        {

			pentry = (niffChklentabEntry *) g_list_nth_data(pclt->items, i);
            RIFFIOFOURCCToString((RIFFIOFOURCC)pentry->chunkName, strId);
                  
            if (! NIFFIOWriteniffChklentabEntry(pnf, pentry))
            {
                RIFFIOError(strModule, 
                            "Failed to write chunk length table entry %s, size %lu",
                            strId, pentry->offsetOfFirstTag);
                return RIFFIO_FAIL;
            }

        }
        
    /*
     * Replace the clt associated with this niff file
     */

    if (pnf->pclt != 0)
         RIFFIOFCCTableFreeEntries(pnf->pclt);
        
    pnf->pclt = pclt;

    return RIFFIO_OK;

}


/***************************************************************************/
/*
 * NIFFIOSeekChunkTags
 * ===================
 * Position a NIFFIOFile at the start of a given chunk's tags.
 */
RIFFIOSuccess
NIFFIOSeekChunkTags(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/*
 * This is done according to the chunk length table associated
 * with the NIFFIOFile.
 * 
 * It is an error if there is no associated CLT. Use NIFFIOFileGetCLT()
 * to find out.
 * 
 * RETURNS
 * -------
 *  RIFFIO_OK :
 *    on success
 *  
 *  RIFFIO_FAIL :
 *    if the lookup failed to find the chunks id in the CLT
 * 
 *  RIFFIO_FAIL :
 *    on a file seek error 
 */
/***************************************************************************/
{
    char strModule[] = "NIFFIOSeekChunkTags";

    char strID[RIFFIO_FOURCC_LIM];
    niffChklentabEntry cltentry;

    assert (pnf != 0);
    assert (pnf->pclt != 0); /* Make sure file has an assigned CLT */

    assert (pchunk != 0);

    RIFFIOFOURCCToString(pchunk->fccId, strID);

    /*
     * Lookup chunk fcc in CLT
     */
    cltentry.chunkName = pchunk->fccId;
        
    if (! NIFFIOCLTLookup(pnf->pclt, &cltentry))
    {
        RIFFIOError(strModule, "CLT lookup failed, id <%s>", strID);
        return RIFFIO_FAIL;
    }

    if (cltentry.offsetOfFirstTag == -1)
    {
        RIFFIOError(strModule, "Chunk tags not permitted, id <%s>", strID);
        return RIFFIO_FAIL;
    }

    /*
     * Seek to the first tag
     */
    if (! NIFFIOSeek(pnf, 
                     pchunk->offsetData + cltentry.offsetOfFirstTag,
                     RIFFIO_SEEK_SET))
    {
        RIFFIOError(strModule, "Seek failed, id <%s>", strID);
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;
}




