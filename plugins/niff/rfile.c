#ifndef lint
/*static char rcsid[] =
 "$Id: rfile.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * rfile - RIFFIOFile routines
 *
 * SYNOPSIS
 * ======== 
 *
 * - RIFFIOFileDelete()
 *
 * - RIFFIOFileGetFormType()
 *
 * - RIFFIORead()
 * - RIFFIOWrite()
 * - RIFFIOSeek()
 * - RIFFIOTell()
 *
 */
/***************************************************************************/

#include <assert.h>
#include <stdlib.h>

#include <riffio.h>

/*
 * Create some bogus callbacks to catch anyone who
 * forgets to initialize a RIFFIOFile
 */




/***************************************************************************/
/*
 * RIFFIOFileDelete
 * ================
 * Free the memory allocated to a RIFFIOFile.
 */
void
RIFFIOFileDelete(RIFFIOFile *prf)
/***************************************************************************/
{
    assert (prf != 0);
    
    free(prf);
    
}


/***************************************************************************/
/*
 * RIFFIOFileGetFormType
 * =====================
 * Return the form type (UNKNOWN, RIFF, or RIFX) 
 * of a RIFFIOFile
 */
RIFFIOFormType 
RIFFIOFileGetFormType(RIFFIOFile *prf)
/*
 * The form type is unknown until the first form chunk of a RIFFIOFile
 * is descended into or created.
 */
/***************************************************************************/
{
        assert(prf != 0);
        return prf->formType;
}

/***************************************************************************/
/*
 * RIFFIORead
 * ==========
 * Read bytes from a RIFF file.
 */
long 
RIFFIORead(RIFFIOFile *prf, void *pvBuffer, long nBytes)
/*
 * ENTRY
 * -----
 * T <*pvBuffer> must be allocated to hold <nBytes>.
 *
 * EXIT
 * ----
 * - Up to <nBytes> have been read from <*prf> into <*pvBuffer>. 
 *
 * RETURN
 * ------
 *     *   The number of bytes actually read,
 *     *   0 on end of file,
 *     *  -1 on error.
 */
/***************************************************************************/
{
    long result;

    /*
     * Check input arguments, make sure nothing is Null
     */
    assert(prf != 0);
    assert(prf->read != 0);
    assert(pvBuffer != 0);

    /*
     * Perform the read
     */
    result = prf->read(prf->pvUserFile, pvBuffer, nBytes);

    return result;
}

/***************************************************************************/
/*
 * RIFFIOWrite
 * ===========
 * Write bytes to a RIFF file.
 */
long 
RIFFIOWrite(RIFFIOFile *prf, void *pvBuffer, long nBytes)
/*
 * Write <nBytes> from <*pvBuffer> to <*prf>.
 *
 * RETURN
 * ------
 * The number of bytes actually written.
 */
/***************************************************************************/
{
    long bytesWritten;

    /*
     * Check input arguments, make sure nothing is Null
     */
    assert(prf->pvUserFile);


    /*
     * Perform the write.
     * It's not up to us to check the result.
     */
    bytesWritten = prf->write(prf->pvUserFile, pvBuffer, nBytes);

    return bytesWritten;

}

/***************************************************************************/
/*
 * RIFFIOSeek
 * ==========
 * Seek to a location in a RIFF file.
 */
RIFFIOSuccess
RIFFIOSeek(RIFFIOFile *prf, RIFFIOOffset offset, RIFFIOSeekOrigin origin)
/* 
 * Seek <offset> bytes relative to <origin>.
 *
 * <origin> may be one of
 *
 *     RIFFIO_SEEK_SET:
 *       beginning of file
 *
 *     RIFFIO_SEEK_CUR:
 *       current file position
 *    
 *     RIFFIO_SEEK_END:
 *       end of file
 * 
 * ERRORS
 * ------ 
 * On failure, the file position is undefined.
 */
/***************************************************************************/
{

    /*
     * Check input arguments, make sure nothing is Null
     */
    assert(prf != 0);
    assert(prf->seek != 0);
    assert(origin >= RIFFIO_SEEK_SET);
    assert(origin <= RIFFIO_SEEK_END);

    /*
     * Perform the seek 
     */
    return  prf->seek(prf->pvUserFile, offset, origin);

}

/***************************************************************************/
/*
 * RIFFIOTell
 * ==========
 * Return the current RIFF file position.
 */
long
RIFFIOTell(RIFFIOFile *prf)
/*
 * A file's position is measured in bytes from the
 * beginning of the file.
 */
/***************************************************************************/
{
    long result;

    /*
     * Check input arguments, make sure nothing is Null
     */
    assert(prf != 0);

    /*
     * Perform the tell
     */
    result = prf->tell(prf->pvUserFile);

    return result;

}
