#ifndef lint
/*static char rcsid[] =
"$Id: inherit.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif
/*
 * Public Domain 1995,1996 Timothy Butler
 *
 * THIS DOCUMENT IS PROVIDED "AS IS" AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 */

/*
 * Alasdair wonders if casting NIFFIOFile pointers to RIFFIOFile pointers
 * [(RIFFIOFile *)pnf] is neccessary when we can say [pnf->rf], in fact,
 * is this not more correct??
*/

/***************************************************************************/
/*
 * NAME
 * ====
 * inherit - RIFFIO-style file operations on NIFFIOFiles
 *
 * SYNOPSIS
 * ========
 * All of these routines behave just like their RIFFIO counterparts.
 *
 * - NIFFIOFileGetFormType()
 *
 * - NIFFIORead()
 * - NIFFIOWrite()
 * - NIFFIOSeek()
 * - NIFFIOTell()
 *
 * - NIFFIOChunkCreate()
 * - NIFFIOChunkFinalize()
 * - NIFFIOChunkDescend()
 * - NIFFIOChunkAscend()
 * 
 * - NIFFIOChunkDataSeek()
 * 
 * - NIFFIOChunkDataEnd()
 * - NIFFIOChunkEnd()
 *
 * - NIFFIOWrite8()
 * - NIFFIOWrite16()
 * - NIFFIOWrite32()
 * - NIFFIOWriteFOURCC()
 * - NIFFIORead8()
 * - NIFFIORead16()
 * - NIFFIORead32()
 * - NIFFIOReadFOURCC() 
 */ 
/***************************************************************************/
/* We simply cast the (NIFFIOFile *) to a (RIFFIOFile *)
 * This could have been implemented with macros, but then
 * we would have lost the type checking abilities of the compiler.
 */
/* Alasdair says "Nooooooo" (NIFFIOFile *)->rf */

#include <niffio.h>

/***************************************************************************/
/*
 * NIFFIOFileGetFormType
 * =====================
 * See RIFFIOFileGetFormType()
 */
RIFFIOFormType
NIFFIOFileGetFormType(NIFFIOFile *pnf)
/***************************************************************************/
{
    return(RIFFIOFileGetFormType(&pnf->rf));
}

/***************************************************************************/
/*
 * NIFFIORead
 * ==========
 * See RIFFIORead()
 */
long
NIFFIORead(NIFFIOFile *pnf, void *bufferp, long n)
/***************************************************************************/
{
    return RIFFIORead(&pnf->rf, bufferp, n);
}

/***************************************************************************/
/*
 * NIFFIOWrite
 * ===========
 * See RIFFIOWrite()
 */
long
NIFFIOWrite(NIFFIOFile *pnf, void *bufferp, long n)
/***************************************************************************/
{
    return RIFFIOWrite(&pnf->rf, bufferp, n);
}

/***************************************************************************/
/*
 * NIFFIOSeek
 * ==========
 * See RIFFIOSeek()
 */
RIFFIOSuccess
NIFFIOSeek(NIFFIOFile *pnf, RIFFIOOffset offset, RIFFIOSeekOrigin origin)
/***************************************************************************/
{
    return RIFFIOSeek(&pnf->rf, offset, origin);
}

/***************************************************************************/
/*
 * NIFFIOTell
 * ==========
 * See RIFFIOTell()
 */
long
NIFFIOTell(NIFFIOFile *pnf)
/***************************************************************************/
{
    return RIFFIOTell(&pnf->rf);
}


/***************************************************************************/
/*
 * NIFFIOChunkCreate
 * =================
 * See RIFFIOChunkCreate()
 */
RIFFIOSuccess
NIFFIOChunkCreate(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkCreate(&pnf->rf, pchunk);
}

/***************************************************************************/
/*
 * NIFFIOChunkFinalize
 * ===================
 * See RIFFIOChunkFinalize()
 */
RIFFIOSuccess
NIFFIOChunkFinalize(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkFinalize(&pnf->rf, pchunk);
}

/***************************************************************************/
/*
 * NIFFIOChunkAscend
 * =================
 * See RIFFIOChunkAscend()
 */
RIFFIOSuccess
NIFFIOChunkAscend(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkAscend(&pnf->rf, pchunk);
}

/***************************************************************************/
/*
 * NIFFIOChunkDescend
 * ==================
 * See RIFFIOChunkDescend()
 */
RIFFIOSuccess
NIFFIOChunkDescend(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkDescend(&pnf->rf, pchunk);
}
/***************************************************************************/
/*
 * NIFFIOChunkDataSeek
 * ===================
 * See RIFFIOChunkDataSeek()
 */
RIFFIOSuccess
NIFFIOChunkSeekData(NIFFIOFile *pnf, const RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkSeekData(&pnf->rf, pchunk);
}

/***************************************************************************/
/*
 * NIFFIOChunkDataEnd
 * ==================
 * See RIFFIOChunkDataEnd()
 */
int
NIFFIOChunkDataEnd(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkDataEnd(&pnf->rf, pchunk);
}

/***************************************************************************/
/*
 * NIFFIOChunkEnd
 * ==============
 * See RIFFIOChunkEnd()
 */
int
NIFFIOChunkEnd(NIFFIOFile *pnf, RIFFIOChunk *pchunk)
/***************************************************************************/
{
    return RIFFIOChunkEnd(&pnf->rf, pchunk);
}


/***************************************************************************/
/*
 * NIFFIOWrite8
 * ============
 * See RIFFIOWrite8()
 */
RIFFIOSuccess NIFFIOWrite8(NIFFIOFile *pnf, unsigned char uc)
/***************************************************************************/
{ return RIFFIOWrite8(&pnf->rf, uc); }

/***************************************************************************/
/*
 * NIFFIOWrite16
 * =============
 * See RIFFIOWrite16()
 */
RIFFIOSuccess NIFFIOWrite16(NIFFIOFile *pnf, unsigned short us)
/***************************************************************************/
{ return RIFFIOWrite16(&pnf->rf, us); }

/***************************************************************************/
/*
 * NIFFIOWrite32
 * =============
 * See RIFFIOWrite32()
 */
RIFFIOSuccess NIFFIOWrite32(NIFFIOFile *pnf, unsigned long ul)
/***************************************************************************/
{ return RIFFIOWrite32(&pnf->rf, ul); }

/***************************************************************************/
/*
 * NIFFIOWriteFOURCC
 * =================
 * See RIFFIOWriteFOURCC()
 */
RIFFIOSuccess NIFFIOWriteFOURCC(NIFFIOFile *pnf, FOURCC fcc)
/***************************************************************************/
{ return RIFFIOWriteFOURCC(&pnf->rf, fcc); }

/***************************************************************************/
/*
 * NIFFIORead8
 * ===========
 * See RIFFIORead8()
 */
RIFFIOSuccess NIFFIORead8(NIFFIOFile *pnf, unsigned char *puc)
/***************************************************************************/
{ return RIFFIORead8(&pnf->rf, puc); }

/***************************************************************************/
/*
 * NIFFIORead16
 * ============
 * See RIFFIORead16()
 */
RIFFIOSuccess NIFFIORead16(NIFFIOFile *pnf, unsigned short *pus)
/***************************************************************************/
{ return RIFFIORead16(&pnf->rf, pus); }

/***************************************************************************/
/*
 * NIFFIORead32
 * ============
 * See RIFFIORead32()
 */
RIFFIOSuccess NIFFIORead32(NIFFIOFile *pnf, unsigned long *pul)
/***************************************************************************/
{ return RIFFIORead32(&pnf->rf, pul); }

/***************************************************************************/
/*
 * NIFFIOReadFOURCC
 * ================
 * See RIFFIOReadFOURCC()
 */
RIFFIOSuccess NIFFIOReadFOURCC(NIFFIOFile *pnf, FOURCC *pfcc)
/***************************************************************************/
{ return RIFFIOReadFOURCC(&pnf->rf, pfcc); }
