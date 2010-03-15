/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtpict.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Public picture object interface.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTPICT
#define XVT_INCL_XVTPICT

extern   PICTURE                                XVT_CALLCONV1
         xvt_pict_create                        XVT_CALLCONV2
         (
            DATA_PTR data,
            long     size,
            RCT *    rctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_pict_destroy                       XVT_CALLCONV2
         (
            PICTURE pict
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_pict_lock                          XVT_CALLCONV2
         (
            PICTURE pict,
            long *  sizep
         );


extern   void                                   XVT_CALLCONV1
         xvt_pict_unlock                        XVT_CALLCONV2
         (
            PICTURE pict
         );

#endif /* XVT_INCL_XVTPICT */
