#ifndef lint
/*static char rcsid[] =
 "$Id: fcc.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * fcc - Four-character code operations
 * 
 * SYNOPSIS
 * ========
 *
 * - RIFFIOFOURCCIsValid()
 * - RIFFIOFOURCCToString()
 *
 */
/***************************************************************************/

#include <assert.h>
#include <ctype.h>
#include <stdio.h>

#include <riffio.h>

/***************************************************************************/
/*
 * RIFFIOFOURCCIsValid
 * ===================
 * Check the validity of a four-character code.
 */
int
RIFFIOFOURCCIsValid(RIFFIOFOURCC fcc)
/*
 * Check <fcc> according to the rules
 *
 * - alphanumeric ASCII characters only [A-Z],[0-9],[a-z]
 *
 * - padded on the right with spaces
 *
 * - no embedded spaces
 *
 * RETURN
 * ------
 * 1 - if fcc is valid,
 *
 * 0 - otherwise
 * 
 * BUGS
 * ----
 * This does not work on FOURCC's that require escape codes to 
 * represent them in string form.
 */
/***************************************************************************/
{
    char str[RIFFIO_FOURCC_LIM]; /* nul-terminated string version of fcc */
    int i;                       /* index into str */

    /*
     * Break up the FOURCC 
     */
    RIFFIOFOURCCToString(fcc, str);
        
    /*
     * Locate the first non-alphanumeric character
     * The condition depends on C short-circuit AND evaluation
     */
    for (i = 0; (i < 4) && isalnum(str[i]); i++)
    {
        /* do nothing */
    } 
        
    /* 
     * Are all the characters alphanumeric?
     */
    if (i == 4)
    {
        /* yes */
        return 1;
    }

    /*
     * We should have stopped on a space
     * Scan the spaces to the end of the FOURCC
     * This while statement depends on C short-ciruit AND evaluation
     */
    while((i < 4) && (str[i] == ' ') )
    {
        i++;
    }

    /* 
     * Did we make it to the end of the FOURCC ?
     */
    if (i == 4)
        return 1; /* Yes, valid FOURCC  */
    else
        return 0; /* No, invalid FOURCC */
}

/***************************************************************************/
/*
 * RIFFIOFOURCCToString
 * ====================
 * Write a four-character code into a string.
 */
void
RIFFIOFOURCCToString(const RIFFIOFOURCC fcc, char *str)
/*
 * ENTRY
 * -----
 * T <*str> must already be allocated to hold at 
 * least RIFFIO_FOURCC_MIN characters.
 *
 * EXIT
 * ----
 * Writes the four characters (possibly using escape sequences) 
 * of <fcc> and a terminating NUL into <*str>.  
 *
 * BUGS
 * ----
 * This does not work for FOURCC's that require escape codes to 
 * represent them as a string.
 */
/***************************************************************************/
{

    assert (str != 0);

    str[0] = (unsigned char) (fcc       & 0x000000ff);
    str[1] = (unsigned char) (fcc >> 8  & 0x000000ff);
    str[2] = (unsigned char) (fcc >> 16 & 0x000000ff);
    str[3] = (unsigned char) (fcc >> 24 & 0x000000ff);

    str[4] = '\0';
 
}



