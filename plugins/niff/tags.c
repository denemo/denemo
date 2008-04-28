#ifndef lint
/*static char rcsid[] =
"$Id: tags.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/***************************************************************************/
/*
 * NAME
 * ====
 * tags - tag creation and navigation routines
 * 
 * SYNOPSIS
 * ========
 *
 * Tag Creation
 * ------------
 * - NIFFIOTagCreate()
 * - NIFFIOTagFinalize()
 * 
 * Tag Navigation
 * --------------
 * - NIFFIOTagDescend()
 * - NIFFIOTagAscend()
 */
/***************************************************************************/

#include <assert.h>

#include <niff/niffio.h>

#ifndef NDEBUG

/* 
 * The following variables and routines support a debugging feature
 * designed to catch unbalanced calls to NIFFIOTagCreate() and
 * NIFFIOTagFinalize(). 
 */
#include <stdlib.h>
static int _isCreateCalled = 0;         /* true if NIFFIOCreateTag() called
                                           at least once */
static int _CreateFinalizeMatch = 0;    /* counts outstanding calls to
                                           NIFFIOTagFinalize() */
static void _CheckCreateFinalize(void); 

/*
 * Make sure NIFFIOTagCreate() and NIFFIOTagFinalize() are called
 * and equal number of times.
 * This function will be registered with atexit() the first time
 * NIFFIOTagCreate is called.
 */
static void
_CheckCreateFinalize(void)
{
        assert(_CreateFinalizeMatch == 0);
}
#endif

/***************************************************************************/
/*
 * NIFFIOTagCreate
 * ===============
 * Start a new tag in a NIFF file.
 */
RIFFIOSuccess
NIFFIOTagCreate(NIFFIOFile *pnf, NIFFIOTag *ptag)
/*
 * ENTRY
 * -----
 * - T <ptag->tagid> must contain the tag ID of the new tag.
 * - T <ptag->tagsizeData> will be written to the file but 
 *   does not need to have a correct value.
 * 
 * EXIT
 * ----
 * - Writes a new tag header to <*pnf>.
 * - Marks the tag as "dirty" (means the data size may need updating).
 *
 * OBLIGATIONS
 * -----------
 * NIFFIOTagFinalize() must eventually be called with <*ptag> 
 * to finish writing the tag.
 * If NIFFIOTagFinalize() determines that <ptag->tagsizeData> is correct 
 * then it will not seek back in the file to correct it.
 */
/***************************************************************************/
{
    char strModule[] = "NIFFIOTagCreate";
    
    /* Check the arguments */
    assert (pnf != 0);
    assert (ptag != 0);
    
#ifndef NDEBUG
    /*
     * Is this the first time NIFFIOTagCreate was called?
     */
    if (! _isCreateCalled)
    {
        /* yes, initialize the create/finalize balance debugging */
        assert(atexit(_CheckCreateFinalize) == 0);
        
        _isCreateCalled = 1;
    }
    
    /* 
     * Increment the number of times NIFFIOTagCreate was
     * called relative to NIFFIOTagFinalize
     */
    _CreateFinalizeMatch++;
    
#endif
    
    /*
     * Write the tag id
     */
    if (! NIFFIOWrite8(pnf, ptag->tagid))
    {
        RIFFIOError(strModule, "Failed to write tag id %u", ptag->tagid);
        return RIFFIO_FAIL;
    }
    
    /*
     * Write the tag size
     */
    if (! NIFFIOWrite8(pnf, ptag->tagsizeData))
    {
        RIFFIOError(strModule, 
                    "Failed to write tag size %u", ptag->tagsizeData);
        return RIFFIO_FAIL;
    }
    
    /* 
     * Remember the offset of the tag's data
     */
    ptag->offsetData = NIFFIOTell(pnf);
    
    /*
     * Mark the tag as dirty
     */
    ptag->isDirty = 1;
    
    return RIFFIO_OK;
}


/***************************************************************************/
/*
 * NIFFIOTagFinalize
 * =================
 * Finish writing a tag to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOTagFinalize(NIFFIOFile *pnf, NIFFIOTag *ptag)
/*
 * ENTRY
 * -----
 * T <*ptag> must be the result of a call to NIFFIOTagCreate.
 * 
 * EXIT
 * ----
 *    - Updates the tag data size on the file and in <*ptag> (if necessary).
 *    - Writes a NUL pad byte (if necessary).
 *    - Leaves the file positioned at the end of the new tag.
 *
 * ERRORS
 * ------
 * On failure, the file position is undefined.
 */
/***************************************************************************/
{
    char strModule[] = "NIFFIOTagFinalize";
    
    RIFFIOOffset offsetTagEnd; /* Offset of end of tag, not including 
                                  possible pad byte */
    BYTE tagsizeNew;          /* Calculated size of a dirty tag */
    
    /* Check the arguments */
    assert (pnf != 0);
    assert (ptag != 0);
    
    /*
     * Only finalize newly created tags
     */
    assert (ptag->isDirty);
    
    
#ifndef NDEBUG
    
    /* 
     * Decrement the number of times RIFFIOChunkCreate was called
     * relative to RIFFIOChunkFinalize
     */
    _CreateFinalizeMatch--;
    
#endif

    offsetTagEnd = NIFFIOTell(pnf);
    
    /*
     * Make sure we aren't trying to update a tag
     * with its end positioned before its beginning
     */
    assert (offsetTagEnd >= ptag->offsetData);
        
    /* 
     * Calculate the tag's size 
     */
    tagsizeNew = offsetTagEnd - ptag->offsetData;
        
    /*
     * Write a nul pad byte if the tag size is an
     * odd number of bytes
     */
    if (tagsizeNew % 2)
    {
        if (! NIFFIOWrite8(pnf, '\0'))
        {
            RIFFIOError(strModule, "Failed to write pad byte at end of tag");
            return RIFFIO_FAIL;
        }
    }
        
    /* 
     * If the newly calculated tag size differs 
     * from its created value, then update the size stored in the file
     */
    if (tagsizeNew != ptag->tagsizeData)
    {
        /* Seek to the size byte */
        if(! NIFFIOSeek(pnf, ptag->offsetData - 1, RIFFIO_SEEK_SET))
        {
            RIFFIOError(strModule, "Failed to seek to tag size");
            return RIFFIO_FAIL;
        }
                
        /* Write the size byte */
        if (! NIFFIOWrite8(pnf, tagsizeNew))
        {
            RIFFIOError(strModule, "Failed to write tag size");
            return RIFFIO_FAIL;
        }

        ptag->tagsizeData = tagsizeNew;         
    }

    /* The tag structure now reflects to correct tag size */
    assert(ptag->tagsizeData == tagsizeNew);

    /* Mark the tag as "clean" */
    ptag->isDirty = 0;

    /*
     * Leave the file positioned after the tag
     */
    if (! NIFFIOTagAscend(pnf, ptag))
    {
        RIFFIOError(strModule, "Failed to ascend past newly created tag");
        return RIFFIO_FAIL;
    }

    return RIFFIO_OK;
        
}

/***************************************************************************/
/*
 * NIFFIOTagDescend
 * ================
 * Read a tag header from a RIFF file.
 */
RIFFIOSuccess
NIFFIOTagDescend(NIFFIOFile *pnf, NIFFIOTag *ptag)
/*
 * ENTRY
 * -----
 * The file must be positioned at the start of a tag.
 * 
 * EXIT
 * ----
 *  - Leaves the file positioned after the tag's size field.
 *  - <ptag->tagid> and 
 *    <ptag->tagsizeData> are updated from the
 *    tag's header.
 */ 
/***************************************************************************/
{
    char strModule[] = "NIFFIOTagDescend";

    /* Check the arguments */
    assert (pnf != 0);
    assert (ptag != 0);

    /* Read the tag's id */
    if (! NIFFIORead8(pnf, &(ptag->tagid)))
    {
        RIFFIOError(strModule, "Failed to read the tag's id");
        return RIFFIO_FAIL;
    }

    /* Read the tag's data size */
    if (! NIFFIORead8(pnf, &(ptag->tagsizeData)))
    {
        RIFFIOError(strModule, "Failed to read the tag's size");
        return RIFFIO_FAIL;
    }
  
    /* Remember the offset of the tag data */
    ptag->offsetData = NIFFIOTell(pnf);

    /* Mark the chunk as clean */
    ptag->isDirty = 0;

    /* 
     * Leave the file positioned at the 
     * beginning of the tag data.
     */
    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIOTagAscend
 * ===============
 * Position a NIFFIOFile after a specified tag.
 */
RIFFIOSuccess
NIFFIOTagAscend(NIFFIOFile *pnf, NIFFIOTag *ptag)
/*
 * ENTRY
 * -----
 * T <*ptag> was returned by NIFFIOTagDescend.
 * 
 * EXIT
 * ----
 * T <*pnf> is postioned at the end of <*ptag>
 * 
 * ERRORS
 * ------
 * On failure, the file position is undefined. 
 */
/***************************************************************************/
{
    char strModule[] = "NIFFIOTagAscend";


    /* Check the arguments */
    assert (pnf != 0);
    assert (ptag != 0);

    /*
     * We can't handle tags that haven't been finalized
     */
    assert (ptag->isDirty == 0);

    /*
     * Leave the user positioned after the tag and any
     * nul padding byte
     */
    if (! NIFFIOSeek(pnf,
                     ptag->offsetData 
                     + ptag->tagsizeData 
                     + (ptag->tagsizeData % 2),
                     RIFFIO_SEEK_SET))
    {
        RIFFIOError(strModule, "Failed to seek past of tag");
        return RIFFIO_FAIL; 
    }
                   
    return RIFFIO_OK;

}












