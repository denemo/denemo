#ifndef lint
/*static char rcsid[] =
"$Id: error.c,v 1.1 2007/01/28 11:48:19 atee Exp $";*/
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
 * error - handle RIFFIO errors
 * 
 * SYNOPSIS
 * ========
 * 
 * - RIFFIOError()
 * - RIFFIOInstallErrorHandler()
 *
 * If RIFFIO_NO_ERROR is defined then disable the default error processing
 * Otherwise use DefaultErrorHandler.
 */
/***************************************************************************/

#include <assert.h>
#include <stdarg.h>

#include <riffio.h>

#ifndef RIFFIO_NO_ERROR
#include <stdio.h>
static void ehDefault(const char *module, const char *message, va_list args);
#endif

/* The current RIFFIO error handler */
#ifndef RIFFIO_NO_ERROR
static RIFFIOErrorHandler ehCurrent = &ehDefault;  
#else
static RIFFIOErrorHandler ehCurrent = 0;
#endif

/***************************************************************************/
/*
 * RIFFIOInstallErrorHandler
 * =========================
 * Override RIFFIO error handling.
 */
RIFFIOErrorHandler 
RIFFIOInstallErrorHandler(RIFFIOErrorHandler ehNew)
/* 
 * <ehNew> is a function to handle errors.  Null to disable error
 * handling.
 * 
 * RETURNS
 * -------
 *   the current error handling function.
 */
/***************************************************************************/
{
    RIFFIOErrorHandler ehOld; /* remember the old Handler */

    ehOld = ehCurrent;
    ehCurrent = ehNew;
    return ehOld;

}

/***************************************************************************/
/*
 * RIFFIOError
 * ===========
 * Post an error message 
 */
void 
RIFFIOError(const char *strModule, const char *strFormat, ...)
/* PARAMETERS
 * ----------
 * 
 *   strModule :
 *     string nameing the module reporting the error
 *
 *   strFormat :
 *    printf compatible string corresponding to the remaining arguments
 */
/***************************************************************************/
{

    va_list args; /* variable argument list */

    /* Check the input arguments */
    assert(strModule != 0);
    assert(strFormat != 0);

    va_start(args, strFormat);

    /* If there is a designated error handler, then call it 
     * Otherwise do nothing 
     */
    if (ehCurrent != 0)
       ehCurrent(strModule, strFormat, args);

    va_end(args);
    
}

#ifndef RIFFIO_NO_ERROR
/*
 * Provide a default error handler that prints a
 * message to stderr.
 */
static void
ehDefault(const char *strModule, const char *strFormat, va_list args)
{

    assert(strModule != 0);
    assert(strFormat != 0);

    (void) fprintf(stderr,"RIFFIO:ERROR:%s:", strModule);
    (void) vfprintf(stderr, strFormat, args); 
    (void) fprintf(stderr,"\n");
    
}
#endif
