/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtdebug.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the debug object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTDEBUG
#define XVT_INCL_XVTDEBUG

/* Define xvt_debug only if app compiled with DEBUG defined.  
 * Params to this macro are surrounded by "((" and "))" */
#ifdef DEBUG
#define xvt_debug(args) xvt_debug_printf args
#else
#define xvt_debug(args)
#endif

extern   void                                   XVT_CALLCONV1
         xvt_debug_printf                       XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );

#endif /* XVT_INCL_XVTDEBUG */
