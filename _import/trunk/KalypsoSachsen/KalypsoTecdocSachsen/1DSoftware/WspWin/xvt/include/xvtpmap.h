/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtpmap.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the pixmap object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTPMAP
#define XVT_INCL_XVTPMAP

extern   XVT_PIXMAP                             XVT_CALLCONV1
         xvt_pmap_create                        XVT_CALLCONV2
         (
            WINDOW            parent,
            XVT_PIXMAP_FORMAT format,
            short             width,
            short             height,
            XVT_PIXMAP_ATTR   reserved
         );

extern   void                                   XVT_CALLCONV1
         xvt_pmap_destroy                       XVT_CALLCONV2
         (
            WINDOW w
         );
 
#endif /* XVT_INCL_XVTPMAP */
