#ifndef lint
/*static char rcsid[] =
"$Id: chunks.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * chunks - create, navigate, and operate on chunks 
 *
 * SYNOPSIS
 * ========
 * 
 * Chunk Creation
 * --------------
 * - RIFFIOChunkCreate()
 * - RIFFIOChunkFinalize()
 *
 * Chunk Navigation
 * ----------------
 * - RIFFIOChunkDescend()
 * - RIFFIOChunkAscend()
 * - RIFFIOChunkDataEnd()
 * - RIFFIOChunkEnd()
 * - RIFFIOChunkDataOffset()
 * - RIFFIOChunkSeekData()
 * 
 * Chunk Operations
 * ----------------
 * - RIFFIOChunkIsList()
 * 
 * DESCRIPTION
 * ===========
 * 
 * 
 */
/***************************************************************************/

#include <assert.h>
#include <riffio.h>
#include <niff.h>
#include <string.h>

#ifndef NDEBUG
/*
 * The following variables and routines support a debugging feature 
 * to catch unbalanced calls to RIFFIOChunkCreate() and RIFFIOChunkFinalize()
 */
#include <stdlib.h>

static int _isCreateCalled = 0;            /* true if RIFFIOChunkCreate has
                                              been called at least once */
static int _CreateFinalizeMatch = 0;       /* counts outstanding calls of
                                              RIFFIOChunkCreate vs Finalize*/
static void _CheckCreateFinalize(void);

/*
 * _CheckCreateFinalize
 * ====================
 * Make sure RIFFIOChunkCreate() and RIFFIOChunkFinalize() are called
 * and equal number of times.
 *
 * This function will be registered with atexit the first time
 * RIFFIOChunkCreate is called.
 */
static void
_CheckCreateFinalize(void)
{
    assert(_CreateFinalizeMatch == 0);
}
#endif

/***************************************************************************/
/*
 * RIFFIOChunkCreate
 * =================
 * Start a new chunk in a RIFF file.
 */
RIFFIOSuccess
RIFFIOChunkCreate(RIFFIOFile *prf, RIFFIOChunk *pchunk) 
/*   
 * ENTRY
 * -----
 * - pchunk->fccId provides the new chunk id.
 *
 * - pchunk->fccType must contain a type id for Lists and Forms and
 *   is ignored otherwise.
 *
 * - pchunk->sizeData does not need to be correct.
 *
 * EXIT
 * ----
 * - Writes a chunk header to *prf according to *pchunk.
 *   RIFFIOChunkCreate() will write pchunk->sizeData to the file.
 * 
 * - Leaves *prf positioned at the start of the new chunk's data,
 * 
 * - If *pchunk is a form, then *prf is marked with the proper
 *   byte order. 
 * 
 * OBLIGATIONS
 *  ----------- 
 * - After writing the chunk contents, *pchunk
 *   must be finished with RIFFIOChunkFinalize().
 *  
 * - Don't change the <*pchunk> data members before calling
 *   RIFFIOChunkFinalize().  
 *
 * - If the pchunk->sizeData is correct when the chunk is finalized,
 *   then RIFFIOChunkFinalize() will not update the chunk's size in
 *   the file.
 *
 * ERRORS
 * ------
 *  On failure, the file position is undefined.  
 */
/***************************************************************************/
{
    char strModule[] = "RIFFIOChunkCreate";
	char fourcc[] = "AAAA";
    /* Check the arguments */
    assert(prf != 0);
    assert(pchunk != 0);
    
#ifndef NDEBUG
    /*
     * Is this the first time RIFFIOChunkCreate was called?
     */
    if (! _isCreateCalled)
    {
        /* yes, initialize the create/finalize balance debugging */
        assert(atexit(_CheckCreateFinalize) == 0);
        
        _isCreateCalled = 1;
    }
    
    /* 
     * Increment the number of times RIFFIOChunkCreate was
     * called relative to RIFFIOChunkFinalize
     */
    _CreateFinalizeMatch++;

#endif

    /*
     * Write the chunk id 
     */
   if (! RIFFIOWriteFOURCC(prf, pchunk->fccId))
    {
        RIFFIOError(strModule, "Failed to write chunk id");
        return RIFFIO_FAIL;
    }
   
    /*
     * If this is a form chunk, then mark the file with the specific
     * form type (byte order).
     * This has to be done before we write the size.
     *
     */
 	if(pchunk->fccType ==  niffformNiff)
        {
 		RIFFIOFOURCCToString(pchunk->fccId, fourcc);

  			if(strcmp(fourcc, "RIFF")==0)
   				prf->formType = RIFFIO_FORM_RIFF;
  			if(strcmp(fourcc, "RIFX")==0)
         	 		prf->formType = RIFFIO_FORM_RIFX;
 	};


    /*
     * Write the chunk size
     */
    if (!RIFFIOWrite32(prf, pchunk->sizeData))
    {
        RIFFIOError(strModule, "Failed to write chunk size");
        return RIFFIO_FAIL;
    }

    /*
     * Remember the offset to the chunk's contents
     * This includes a form or list's FOURCC type.
     */
    pchunk->offsetData = RIFFIOTell(prf);


    /*
     * Write a list chunk's type (or form type)
     */
    if (RIFFIOChunkIsList(pchunk))
    {
        if (!RIFFIOWriteFOURCC(prf, pchunk->fccType))
        {
            RIFFIOError(strModule, "Failed to write chunk type");
            return RIFFIO_FAIL;
        }
    }

    /*
     * Mark the chunk as dirty
     */
    pchunk->isDirty = 1;

    /*
     * Leave the file positioned at the start of either
     * the chunk's data or a list's first subchunk
     */
    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * RIFFIOChunkFinalize
 * ===================
 * Finish writing a new chunk in a RIFF file.
 */ 
RIFFIOSuccess
RIFFIOChunkFinalize(RIFFIOFile *prf, RIFFIOChunk *pchunk)
/*
 * ENTRY
 * -----
 * <*pchunk> was returned by RIFFIOChunkCreate
 *
 * EXIT
 * ----
 * - Updates the chunk size in the file and in <*pchunk> (if necessary).
 *
 * - Writes a NUL pad byte (if necessary).
 *
 * - Leaves the file positioned after the chunk.
 *
 * ERRORS
 * ------
 * On failure, the file position is undefined.
 */
/***************************************************************************/
{

    char strModule[] = "RIFFIOChunkFinalize";

    RIFFIOSize   sizeNew;     /* Size of a "dirty" chunk */
    RIFFIOOffset offChunkEnd; /* Offset of the end of a "dirty" chunk
                               * not including any nul pad byte 
                               */


    /* Check the arguments */
    assert (prf != 0);
    assert (pchunk !=0);

    /* 
     * We should only be finalizing newly created chunks
     */
//    assert (pchunk->isDirty);

#ifndef NDEBUG
        
    /* 
     * Decrement the number of times RIFFIOChunkCreate was called
     * relative to RIFFIOChunkFinalize
     */
    _CreateFinalizeMatch--;

#endif

    /*
     * We are at the end of chunk data
     * by definition.
     */
    offChunkEnd = RIFFIOTell(prf);
        
    /* 
     * Make sure we aren't trying to update a chunk with
     * its end before its beginnning.
     */
    assert( 
           (RIFFIOChunkIsList(pchunk) 
            && (offChunkEnd >= pchunk->offsetData+4))
           || 
           (!RIFFIOChunkIsList(pchunk) 
            && (offChunkEnd >= pchunk->offsetData)));

    /*
     * Calculate the size of the chunk's contents
     * not including any pad byte
     */
    sizeNew = offChunkEnd - (pchunk->offsetData);
        
    /*
     * Add a nul pad byte if the chunks contents is 
     * an odd number of bytes.
     * We don't count this in the chunk's size.
     */
    if (sizeNew % 2) 
    {
        if(! RIFFIOWrite8(prf, '\0'))
        {
            RIFFIOError(strModule, 
                        "Failed to write pad byte at end of chunk");
            return RIFFIO_FAIL;
        }
    }
        
    /* 
     * If the newly calculated chunk size differs
     * from its created value, then update the size
     */
    if (sizeNew != pchunk->sizeData)
    {
        /* Seek to the size field */
        if (! RIFFIOSeek(prf, pchunk->offsetData - 4, RIFFIO_SEEK_SET))
        {
            RIFFIOError(strModule, "Failed seek to update chunk size");
            return RIFFIO_FAIL;
        }
                        
        /* Re-write the size field */
        if (! RIFFIOWrite32(prf, sizeNew))
        {
            RIFFIOError(strModule, "Failed to write updated chunk size");
            return RIFFIO_FAIL;
        }
                        
        pchunk->sizeData = sizeNew;
    }

    /* 
     * Mark the chunk as having the correct size 
     */
    pchunk->isDirty = 0;
        
    /*
     * Leave the file positioned after the newly created chunk
     */
    if (! RIFFIOChunkAscend(prf, pchunk))
    {
        RIFFIOError(strModule, "Failed to ascend past new chunk");
        return RIFFIO_FAIL;
    }

     
	delete (pchunk);

    return RIFFIO_OK;

}


/***************************************************************************/
/*
 * RIFFIOChunkDescend
 * ==================
 * Read a chunk header from a RIFF file.
 */
RIFFIOSuccess
RIFFIOChunkDescend(RIFFIOFile *prf, RIFFIOChunk *pchunk)
/*
 * ENTRY
 * -----
 * <*prf> must be positioned at the start of a chunk.
 *
 * EXIT
 * ----
 * - Leaves the file positioned at the start of the chunk's contents 
 *   (either the first subchunk of a LIST or a normal chunk's data).
 *
 * - If the chunk is a RIFF or RIFX form, then
 *   marks the RIFFIOFile byte order accordingly.
 * 
 * - <*pchunk> holds the id, size, and possibly the
 *   list type of the newly read chunk.
 * 
 * ERRORS 
 * ------ 
 * On failure the file position is undefined.
 */ 
/***************************************************************************/
{

    char strModule[] = "RIFFIOChunkDescend";
        
    /* Check the arguments */
    assert(prf != 0);
    assert(pchunk != 0);

    /*
     * We are positioned at the start of a chunk.
     */

    /*
     * Read the chunk's id
     */
    if (!RIFFIOReadFOURCC(prf, &(pchunk->fccId)))
    {
        RIFFIOError(strModule, "Failed to read chunk id");
        return RIFFIO_FAIL;
    }

    /*
     * If the chunk id is RIFF or RIFX then mark the
     * file form type accordingly.
     * This must be done before we read the chunk size.
     */
    switch (pchunk->fccId)
    {
      case RIFFIO_FOURCC_RIFF:
        prf->formType = RIFFIO_FORM_RIFF;
        break;

      case RIFFIO_FOURCC_RIFX:
        prf->formType = RIFFIO_FORM_RIFX;
        break;
    }

    /*
     * Read the chunk size
     */
    if (! RIFFIORead32(prf, &(pchunk->sizeData)))
    {
        RIFFIOError(strModule, "Failed to read chunk size");
        return RIFFIO_FAIL;
    }

    /* 
     * Remember the offset of the chunk's contents
     * (includes a form or list's type)
     */ 
    pchunk->offsetData = RIFFIOTell(prf);

    /*
     * If the chunk is a form or list, 
     * then read its type
     */
    if (RIFFIOChunkIsList(pchunk))
    {
        if(pchunk->sizeData < 4)
        {
            RIFFIOError(strModule, 
                        "Found list data size less than four bytes");
            return RIFFIO_FAIL;
        }

        if(! RIFFIOReadFOURCC(prf, &(pchunk->fccType)))
        {
            RIFFIOError(strModule, "Failed to read chunk type");
            return RIFFIO_FAIL;
        }
    }

    /* 
     * Mark the chunk as "clean"
     */
    pchunk->isDirty = 0;

    /* 
     * Leave the user positioned at the beginning of the chunk's
     * data (not including list or form's type).
     */

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * RIFFIOChunkAscend 
 * =================
 * Advance a RIFF file past a chunk.
 */
RIFFIOSuccess
RIFFIOChunkAscend(RIFFIOFile *prf, const RIFFIOChunk *pchunk)
/*
 * ENTRY
 * -----
 * <*pchunk> was returned by either RIFFIOChunkDescend or
 * RIFFIOChunkFinalize.
 *
 * EXIT
 * ----
 * <*prf> is positioned at the end of <*pchunk>
 *
 * ERRORS
 * ------ 
 * On failure, the file position is undefined.
 * 
 */ 
/***************************************************************************/
{

    char strModule[] = "RIFFIOChunkAscend";


    /* Check the arguments */
    assert(prf != 0);
    assert(pchunk != 0);

        
    /* 
     * The chunk must have the correct size
     */
    assert (pchunk->isDirty == 0);

    /*
     * Leave the user positioned after the chunk and any
     * nul pad byte
     */
    if (! RIFFIOSeek(prf, 
                     pchunk->offsetData 
                     + pchunk->sizeData 
                     + (pchunk->sizeData % 2), 
                     RIFFIO_SEEK_SET))
    {
        RIFFIOError(strModule, "Failed to seek past chunk");
        return RIFFIO_FAIL; 
    }

    return RIFFIO_OK;

}



/***************************************************************************/
/*
 * RIFFIOChunkDataOffset
 * =====================
 * Return the file position of a chunk's data (or list's first subchunk)
 */
RIFFIOOffset
RIFFIOChunkDataOffset(const RIFFIOChunk *pchunk)
/***************************************************************************/
{
    assert (pchunk != 0);

    /*
     * Figure out where the chunk's contents start.
     * If it's a list then we have to account for the
     * type field
     */
    if (RIFFIOChunkIsList(pchunk))
        return pchunk->offsetData + 4;
    else
        return pchunk->offsetData;

}


/***************************************************************************/
/*
 * RIFFIOChunkSeekData
 * ===================
 * Seek to the start of a chunk's data or a list's contents
 */
RIFFIOSuccess
RIFFIOChunkSeekData(RIFFIOFile *prf, const RIFFIOChunk *pchunk)
/*
 * Note that this is different than RIFFIOChunkDescend() because
 * RIFFIOChunkSeekData() can be called regardless of the current 
 * file postion.
 * 
 * ENTRY 
 * ----- 
 * <*prf> may be positioned anywhere.
 *
 * EXIT 
 * ----
 * <*prf> is postioned at the start of a chunk's data or the first
 * subchunk of a list.
 * 
 * ERRORS 
 * ------
 * On error, the file postion is undefined.
 */
/***************************************************************************/
{
    const char   strModule[] = "RIFFIOChunkSeekData";

    assert (prf != 0);
    assert (pchunk != 0);

        
    if (! RIFFIOSeek(prf, 
                     RIFFIOChunkDataOffset(pchunk),
                     RIFFIO_SEEK_SET))
    {
        RIFFIOError(strModule, "Failed to seek to chunk contents");
        return RIFFIO_FAIL;
    }
        
    return RIFFIO_OK;
   
}

/***************************************************************************/
/*
 * RIFFIOChunkIsList
 * =================
 * Return true if a chunk is a form or a list.
 */
int
RIFFIOChunkIsList(const RIFFIOChunk *pchunk)
/***************************************************************************/
{
	    char fourcc []="AAAA";
    assert(pchunk != 0);

	RIFFIOFOURCCToString(pchunk->fccId, fourcc);

   	if(strcmp(fourcc, "LIST")==0)
		return 1;
	if(strcmp(fourcc, "RIFX")==0)
         return 1;

	return 0;


}

/***************************************************************************/
/*
 * RIFFIOChunkDataEnd
 * ==================
 * Return true if a file is positioned at the end of a chunk's data.
 */
int
RIFFIOChunkDataEnd(RIFFIOFile *prf, const RIFFIOChunk *pchunk)
/*
 * The chunk data's end may be before any NUL pad byte.
 * 
 * ENTRY
 * -----
 * <*pchunk> must be a "clean" chunk, returned by either 
 * RIFFIOChunkDescend() or RIFFIOChunkFinalize().
 */
/***************************************************************************/
{

    RIFFIOOffset offFile; /* current file position */
        
    assert(prf != 0);
    assert(pchunk != 0);

    /*
     * We can't test "dirty" chunks
     */
    assert(pchunk->isDirty == 0);

    offFile = RIFFIOTell(prf);
        
    if (offFile == pchunk->offsetData + pchunk->sizeData) 
        return 1;
    else
        return 0;
        
}


/***************************************************************************/
/*
 * RIFFIOChunkEnd
 * ==============
 * Return true if a RIFF file is positioned at the end of a chunk.
 */
int
RIFFIOChunkEnd(RIFFIOFile *prf, const RIFFIOChunk *pchunk)
/* 
 * The chunk end is after any NUL pad byte.
 *
 * ENTRY
 * -----
 * <*pchunk> must be a "clean" chunk, returned by either 
 * RIFFIOChunkDescend() or RIFFIOChunkFinalize().
 */
/***************************************************************************/
{

    RIFFIOOffset offFile; /* File position */
        
    assert(prf != 0);
    assert(pchunk != 0);

    /*
     * We can't test "dirty" chunks
     */
    assert(pchunk->isDirty == 0);

    offFile = RIFFIOTell(prf);
        
    if (offFile == 
        pchunk->offsetData 
        + pchunk->sizeData 
        + (pchunk->sizeData % 2))
        return 1;
    else
        return 0;
        
}

