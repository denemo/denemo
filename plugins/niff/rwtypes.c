#ifndef lint
/*static char rcsid[] = "$Header: /sources/denemo/denemo/plugins/niff/rwtypes.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
#endif

/***************************************************************************/
/*
 * NAME
 * ====
 * rwtypes - Read and Write basic types defined in "niff.h"
 *
 * - NIFFIOReadBYTE()
 * - NIFFIOReadCHAR()
 * - NIFFIOReadSIGNEDBYTE()
 * - NIFFIOReadSHORT()
 * - NIFFIOReadLONG()
 * - NIFFIOReadRATIONAL()
 * - NIFFIOReadSTROFFSET()
 * - NIFFIOReadFONTIDX()
 *
 * - NIFFIOWriteBYTE()
 * - NIFFIOWriteCHAR()
 * - NIFFIOWriteSIGNEDBYTE()
 * - NIFFIOWriteSHORT()
 * - NIFFIOWriteLONG()
 * - NIFFIOWriteRATIONAL()
 * - NIFFIOWriteSTROFFSET()
 * - NIFFIOWriteFONTIDX()
 */
/***************************************************************************/

#include <assert.h>

#include <niff/niffio.h>

/***************************************************************************/
/*
 * NIFFIOReadBYTE
 * ==============
 * Read an NIFF BYTE from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadBYTE(NIFFIOFile *pnf, BYTE *pbyte)
/***************************************************************************/
{
    return NIFFIORead8(pnf, pbyte);
}

/***************************************************************************/
/*
 * NIFFIOReadCHAR
 * ==============
 * Read a NIFF CHAR from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadCHAR(NIFFIOFile *pnf, CHAR *pchar)
/***************************************************************************/
{
    return NIFFIORead8(pnf, pchar);
}

/***************************************************************************/
/*
 * NIFFIOReadSIGNEDBYTE
 * ====================
 * Read a NIFF SIGNEDBYTE from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadSIGNEDBYTE(NIFFIOFile *pnf, SIGNEDBYTE *sbp)
/***************************************************************************/
{
    unsigned char ub;    /* byte read from file */

    assert(pnf != 0);
    assert(sbp !=0);

    if (! NIFFIORead8(pnf, &ub))
        return RIFFIO_FAIL;
 
    /* Convert two's complement to native representation */
    if (ub & 0x80U)
    {
        *sbp = (~ub) + 1;
        *sbp &= 0x00ff;
        *sbp = - (*sbp);
    } 
    else
    {
        *sbp = ub;
    }
#if 0
    assert(ub == *sbp); /* only true for two's complement machines */
#endif 

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * NIFFIOReadSHORT
 * ===============
 * Read a NIFF SHORT from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadSHORT(NIFFIOFile *pnf, SHORT *ssp)
/***************************************************************************/
{
        
    unsigned short us;

    assert(pnf != 0);
    assert(ssp != 0);

    if (! NIFFIORead16(pnf, &us))
        return RIFFIO_FAIL;

    /* Convert from two's complement to native representation */
    if (us & 0x8000U)
    {
        *ssp = (~us) + 1;
        *ssp &= 0x00ffffU;
        *ssp = - (*ssp);
    }
    else
    {
        *ssp = us;
    }
    
#if 0
    assert(*ssp == us); /* true only on two's complement machines */
#endif
    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIOReadLONG
 * ==============
 * Read a NIFF LONG from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadLONG(NIFFIOFile *pnf, LONG *slp)
/***************************************************************************/
{

    unsigned long ul;

    assert(pnf != 0);
    assert(slp != 0);

    if (! NIFFIORead32(pnf, &ul))
        return RIFFIO_FAIL;

    /* Convert from two's complement to native representation */
    if (ul &0x80000000UL)
    {
        *slp = (~ul) + 1;
        *slp &= 0x00ffffffffUL;
        *slp = - (*slp);
    }
    else
    {
        *slp = ul;
    }
    assert(*slp == (long)ul); /* true only on two's complement machines */
    return RIFFIO_OK;
}

/***************************************************************************/
/*
 * NIFFIOReadRATIONAL
 * ==================
 * Read a NIFF RATIONAL from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadRATIONAL(NIFFIOFile *pnf, RATIONAL *prat)
/***************************************************************************/
{


    assert(pnf != 0);
    assert(prat != 0);

    if (! NIFFIOReadSHORT(pnf, &(prat->numerator)))
        return RIFFIO_FAIL;

    if (! NIFFIOReadSHORT(pnf, &(prat->denominator)))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * NIFFIOReadSTROFFSET
 * ===================
 * Read a NIFF STROFFSET from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadSTROFFSET(NIFFIOFile *pnf, STROFFSET *pstroff)
/***************************************************************************/
{
    return NIFFIOReadLONG(pnf, pstroff);
}

/***************************************************************************/
/*
 * NIFFIOReadFONTIDX
 * =================
 * Read a NIFF FONTIDX from a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOReadFONTIDX(NIFFIOFile *pnf, FONTIDX *pfidx)
/***************************************************************************/
{
    return NIFFIORead16(pnf, pfidx);
}

/***************************************************************************/
/*
 * NIFFIOWriteBYTE
 * ===============
 * Write a NIFF BYTE to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteBYTE(NIFFIOFile *pnf, BYTE b)
/***************************************************************************/
{
    return NIFFIOWrite8(pnf, b);
}

/***************************************************************************/
/*
 * NIFFIOWriteSIGNEDBYTE
 * =====================
 * Write a NIFF SIGNEDBYTE to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteSIGNEDBYTE(NIFFIOFile *pnf, SIGNEDBYTE sb)
/***************************************************************************/
{
    unsigned char uc; /* the byte to write, in two's complement */

    assert(pnf != 0);

    /* 
     * Convert sb to a two's complement representation
     * (of course it probably already _is_ but we don't know 
     * that for sure)
     */
    if(sb < 0)
    {
        uc = -sb;
        uc = ~uc;
        uc = uc+1;
    }
    else
    {
        uc = sb;
    }

    return NIFFIOWrite8(pnf, uc);
}

/***************************************************************************/
/*
 * NIFFIOWriteSHORT
 * ================
 * Write a NIFF SHORT to a NIFFIOfile.
 */
RIFFIOSuccess
NIFFIOWriteSHORT(NIFFIOFile *pnf, SHORT ss)
/***************************************************************************/
{

    unsigned short us; /* two's complement representation of ss */
    assert(pnf != 0);

    if (ss < 0)
    {
        us = -ss;
        us = ~us;
        us = us+1;
    }
    else
    {
        us = ss;
    }

    return NIFFIOWrite16(pnf, us);
}

/***************************************************************************/
/*
 * NIFFIOWriteLONG
 * ===============
 * Write a NIFF LONG to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteLONG(NIFFIOFile *pnf, LONG sl)
/***************************************************************************/
{
    unsigned long ul; /* two's complement representation of sl */

    assert(pnf != 0);

    if (sl <0)
    {
        ul = -sl;
        ul = ~ul;
        ul = ul+1;
    }
    else
    {
        ul = sl;
    }
    return NIFFIOWrite32(pnf, ul);
}

/***************************************************************************/
/*
 * NIFFIOWriteRATIONAL
 * ===================
 * Write a NIFF RATIONAL to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteRATIONAL(NIFFIOFile *pnf, RATIONAL rat)
/***************************************************************************/
{

    if (! NIFFIOWriteSHORT(pnf, rat.numerator))
        return RIFFIO_FAIL;

    if (! NIFFIOWriteSHORT(pnf, rat.denominator))
        return RIFFIO_FAIL;

    return RIFFIO_OK;

}

/***************************************************************************/
/*
 * NIFFIOWriteSTROFFSET
 * ====================
 * Write a NIFF STROFFSET to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteSTROFFSET(NIFFIOFile *pnf, STROFFSET stroff)
/***************************************************************************/
{
    return NIFFIOWriteLONG(pnf, stroff);

}

/***************************************************************************/
/*
 * NIFFIOWriteFONTIDX
 * ==================
 * Write a NIFF FONTIDX to a NIFFIOFile.
 */
RIFFIOSuccess
NIFFIOWriteFONTIDX(NIFFIOFile *pnf, FONTIDX f)
/***************************************************************************/
{
    return NIFFIOWrite16(pnf, f);
}












