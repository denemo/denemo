#ifndef lint
/*static char rcsid[] =
"$Id: stbl.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * stbl - string table functions
 *
 * SYNOPSIS
 * ========
 *
 * - NIFFIOStblWrite()
 *
 * SEE ALSO
 * ========
 *
 * - NIFFIOStoreStbl()
 *
 */
/***************************************************************************/

#include <assert.h>
#include <string.h>

#include <niffio.h>

/***************************************************************************/
/*
 * NIFFIOStblWrite
 * =====================
 * Write a single string table entry to a RIFFIOFile.
 */
RIFFIOSuccess
NIFFIOStblWrite(NIFFIOFile *pnf, 
                RIFFIOChunk *pchunkSTBL, 
                NIFFIOStbl *pstbl)
/*
 * ENTRY
 * -----
 * T <*pchunkSTBL> must be newly created by RIFFIOChunkCreate() and not
 * yet finalized by RIFFIOChunkFinalize().
 *
 * EXIT
 * ----
 * Writes < pstbl->str > (including its terminating NUL character) to
 * a string table chunk <*pchunkSTBL> in the NIFFIOFile <*pnf>.
 *
 * Writing takes place at the current file position.
 * 
 * Calculates the string offset from pchunkSTBL's data offset and
 * returns it in <pstbl->offset>.
 *
 * RETURN
 * ------ 
 * RIFFIO_RESULT_OK on success.
 */
/***************************************************************************/
{
        
    char strModule[] = "NIFFIOStblWrite";

    long nStringLength;        /* The length of str (not including nul) */
    RIFFIOOffset offsetString; /* Offset of string relative to _file_ start */
    long nBytesWritten;        /* Bytes written to pnf */

    assert (pnf != 0);
    assert (pchunkSTBL != 0);
    assert (pchunkSTBL->fccId == niffckidStringTable);
    assert (pchunkSTBL->isDirty);
    assert (pstbl->str != 0);

    nStringLength = strlen(pstbl->str);
        
    /* 
     * Remember the offset of the string relative to the file start
     */
    offsetString = NIFFIOTell(pnf);

    /*
     * We had better be writing somewhere _after_ the start of the
     * chunk data
     */
    assert (offsetString >= RIFFIOChunkDataOffset(pchunkSTBL));

    /*
     * Write the string (including nul)
     */
    nBytesWritten = NIFFIOWrite(pnf, (void *) pstbl->str, nStringLength + 1);
    if (nBytesWritten != nStringLength+1)
    {
        RIFFIOError(strModule, "Failed to write string");
        return RIFFIO_FAIL;
    }
        
    /*
     * Fill in the STROFFSET 
     */
    pstbl->offset = offsetString - RIFFIOChunkDataOffset(pchunkSTBL);

    return RIFFIO_OK;
}




