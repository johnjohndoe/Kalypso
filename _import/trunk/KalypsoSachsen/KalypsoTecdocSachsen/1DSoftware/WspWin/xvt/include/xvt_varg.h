/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvt_varg.h,v $ 
 *  $Revision: 1.3 $
 *
 *  Purpose: Define macros dealing with variable argument lists
 *
 ****************************************************************************/

#ifndef XVT_INCL_VARG
#define XVT_INCL_VARG


/* 
   We only define macro for va_start, as the rest of variable arg list
   support ( va_alist, va_dcl, va_end, va_list, va_arg ) are the same
   for both <stdargs.h> and <varargs.h>
*/
#undef STD_ARGS
#undef XVT_CC_STDARGS
#undef XVT_CC_VA_START

/* include variable args header, and define STD_ARGS if appropriate. */
#if XVT_CC_ISANSI
#include <stdarg.h>

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#define STD_ARGS 1    /* for any old code */
#define XVT_CC_STDARGS    TRUE
#define XVT_CC_VA_START(a,b) va_start(a,b)

#if defined(_cplusplus) || defined(__cplusplus)
}     /* extern "C" */
#endif

#else /* Not ANSI */
#if ((XVT_OS == XVT_OS_VMS || XVT_OS == XVT_OS_ISC) || !defined( va_start ))
#include <varargs.h>    /* Sun unbundled K&R cc, VMS CC need sthis */

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

#define XVT_CC_STDARGS    FALSE
#define XVT_CC_VA_START(a,b) va_start(a)

#if defined(_cplusplus) || defined(__cplusplus)
}     /* extern "C" */
#endif

#endif
#endif


#endif /* XVT_INCL_VARG */
