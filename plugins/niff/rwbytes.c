#ifndef lint
/*static char rcsid[] =
"$Id: rwbytes.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * rwbytes - Read and write FOURCCs and unsigned 8, 16, and 32-bit integers
 *
 * SYNOPSIS
 * ========
 *
 * - RIFFIORead8()
 * - RIFFIORead16()
 * - RIFFIORead32()
 * - RIFFIOReadFOURCC()
 * 
 * - RIFFIOWrite8()
 * - RIFFIOWrite16()
 * - RIFFIOWrite32()
 * - RIFFIOWriteFOURCC()
 * 
 * RETURN
 * ======
 * All of these routines return a zero RIFFIOSuccess on failure.
 *
 * ERRORS
 * ======
 * On failure, the RIFFIOFile position is undefined.
 * None of these functions call RIFFIOError.
 *
 */
/***************************************************************************/

#include <assert.h>
#include <riffio.h>

/***************************************************************************/
/*
 * RIFFIORead8
 * ===========
 * Read an unsigned 8-bit integer from a RIFF file.
 */
RIFFIOSuccess
RIFFIORead8(RIFFIOFile *prf, unsigned char *ucp)
/***************************************************************************/
{
    long bytesRead; 

    assert(prf != 0);
    assert(ucp != 0);
    
    bytesRead = RIFFIORead(prf, ucp, 1);

    if (bytesRead != 1)
        return RIFFIO_FAIL;

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * RIFFIORead16
 * ============
 * Read an unsigned 16-bit integer from a RIFF file.
 */
RIFFIOSuccess
RIFFIORead16(RIFFIOFile *prf, unsigned short *usp)
/***************************************************************************/
{
    long bytesRead; 
    unsigned char bytes[2];

    assert(prf != 0);
    assert(prf->formType != RIFFIO_FORM_UNKNOWN);
    assert(usp != 0);
        
    bytesRead = RIFFIORead(prf, bytes, 2);

    if (bytesRead != 2)
        return RIFFIO_FAIL;
    
    if (prf->formType == RIFFIO_FORM_RIFF)
    {
        *usp =   (unsigned short) bytes[0] 
               | (unsigned short) bytes[1] << 8;
    }
    else
    {
        *usp =   (unsigned short) bytes[1] 
               | (unsigned short) bytes[0] << 8;
    }

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * RIFFIORead32
 * ============
 * Read an unsigned 32-bit integer from a RIFF file.
 */
RIFFIOSuccess
RIFFIORead32(RIFFIOFile *prf, unsigned long *ulp)
/***************************************************************************/
{
    long bytesRead; 
    unsigned char bytes[4];

    assert(prf != 0);
    assert(prf->formType != RIFFIO_FORM_UNKNOWN);
    assert(ulp != 0);

    bytesRead = RIFFIORead(prf, bytes, 4);

    if (bytesRead != 4)
        return RIFFIO_FAIL;

    if (prf->formType == RIFFIO_FORM_RIFF)
    {
        *ulp =   (unsigned long) bytes[0] 
            | (unsigned long) bytes[1] << 8 
            | (unsigned long) bytes[2] << 16
            | (unsigned long) bytes[3] << 24;
    }
    else
    {
        *ulp =   (unsigned long) bytes[3] 
            | (unsigned long) bytes[2] << 8 
            | (unsigned long) bytes[1] << 16
            | (unsigned long) bytes[0] << 24;
    }

    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * RIFFIOReadFOURCC
 * ================
 * Read a four-character code from a RIFF file.
 */
RIFFIOSuccess
RIFFIOReadFOURCC(RIFFIOFile *prf, RIFFIOFOURCC *fccp)
/***************************************************************************/
{
    long bytesRead; 
    unsigned char bytes[4];

    assert(prf != 0);
    assert(fccp != 0);

    bytesRead = RIFFIORead(prf, bytes, 4);

    if (bytesRead != 4)
        return RIFFIO_FAIL;
    
    *fccp =  RIFFIOMAKEFOURCC(bytes[0],bytes[1],bytes[2],bytes[3]); 
    
    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * RIFFIOWrite8
 * ============
 * Write an unsigned 8-bit integer to a RIFF file.
 */
RIFFIOSuccess
RIFFIOWrite8(RIFFIOFile *prf, unsigned char uc)
/***************************************************************************/
{
    long bytesWritten; 

    assert(prf != 0);

     
    bytesWritten =  RIFFIOWrite(prf, &uc, 1);
    if (bytesWritten != 1)
        return RIFFIO_FAIL;
    
    return RIFFIO_OK;
 
}


/***************************************************************************/
/* 
 * RIFFIOWrite16
 * =============
 * Write an unsigned 16-bit integer to a RIFF file.
 */
RIFFIOSuccess
RIFFIOWrite16(RIFFIOFile *prf, unsigned short us)
/***************************************************************************/
{
    long bytesWritten; 
    unsigned char bytes[2];

    assert(prf != 0);
    assert(prf->formType != RIFFIO_FORM_UNKNOWN);

    if (prf->formType == RIFFIO_FORM_RIFF)
    {
        bytes[0] = us &      0x00ffU;
        bytes[1] = (us>>8) & 0x00ffU;
    }
    else
    {
        bytes[1] = us &      0x00ffU;
        bytes[0] = (us>>8) & 0x00ffU;
    }

    bytesWritten = RIFFIOWrite(prf, &bytes, 2);
    if (bytesWritten != 2)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * RIFFIOWrite32
 * =============
 * Write an unsigned 32-bit integer to a RIFF file.
 */ 
RIFFIOSuccess
RIFFIOWrite32(RIFFIOFile *prf, unsigned long ul)
/***************************************************************************/
{
    long bytesWritten; 
    unsigned char bytes[4];

    assert(prf != 0);

    if (prf->formType == RIFFIO_FORM_RIFF)
    {
        bytes[0] = (ul) & 0x000000ffUL;
        bytes[1] = (ul >> 8) & 0x000000ffUL;
        bytes[2] = (ul >> 16)& 0x000000ffUL;
        bytes[3] = (ul >> 24)& 0x000000ffUL;
    }
    else
    {
        bytes[3] = (ul) & 0x000000ffUL;
        bytes[2] = (ul >> 8) & 0x000000ffUL;
        bytes[1] = (ul >> 16)& 0x000000ffUL;
        bytes[0] = (ul >> 24)& 0x000000ffUL;
    }

    bytesWritten = RIFFIOWrite(prf, &bytes, 4);

    if (bytesWritten != 4)
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}


/***************************************************************************/
/*
 * RIFFIOWriteFOURCC
 * =================
 * Write a four-character code to a RIFF file.
 */
RIFFIOSuccess
RIFFIOWriteFOURCC(RIFFIOFile *prf, RIFFIOFOURCC fcc)
/***************************************************************************/
{
    long bytesWritten; 
    unsigned char bytes[4];

    assert(prf != 0);


    bytes[0] =  fcc        & 0x000000ffUL;
    bytes[1] = (fcc >> 8)  & 0x000000ffUL;
    bytes[2] = (fcc >> 16) & 0x000000ffUL;
    bytes[3] = (fcc >> 24) & 0x000000ffUL;

    bytesWritten =  RIFFIOWrite(prf, &bytes, 4);

    if (bytesWritten != 4)
        return RIFFIO_FAIL;
        
    return RIFFIO_OK;

}
