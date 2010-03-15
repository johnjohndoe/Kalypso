/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtcb.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the clipboard object.
 * 
 ****************************************************************************/

#ifndef XVT_INCL_XVTCB
#define XVT_INCL_XVTCB

extern   DATA_PTR                               XVT_CALLCONV1
         xvt_cb_alloc_data                      XVT_CALLCONV2
         (
            long size
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_cb_close                           XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_cb_free_data                       XVT_CALLCONV2
         (
            void
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_cb_get_data                        XVT_CALLCONV2
         (
            CB_FORMAT cbfmt,
            char *    name,
            long *    sizep
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_cb_has_format                      XVT_CALLCONV2
         (
            CB_FORMAT cbfmt,
            char *    name
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_cb_open                            XVT_CALLCONV2
         (
            BOOLEAN writing
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_cb_put_data                        XVT_CALLCONV2
         (
            CB_FORMAT cbfmt,
            char *    name,
            long      size,
            PICTURE   pic
         );


#endif /* XVT_INCL_XVTCB */
