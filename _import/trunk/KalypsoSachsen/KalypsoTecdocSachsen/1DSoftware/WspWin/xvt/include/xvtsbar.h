/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtsbar.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the scrollbar object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTSBAR
#define XVT_INCL_XVTSBAR

extern   int                                    XVT_CALLCONV1
         xvt_sbar_get_pos                       XVT_CALLCONV2
         (
            WINDOW      win,
            SCROLL_TYPE t
         );


extern   int                                    XVT_CALLCONV1
         xvt_sbar_get_proportion                XVT_CALLCONV2
         (
            WINDOW      win,
            SCROLL_TYPE t
         );


extern   void                                   XVT_CALLCONV1
         xvt_sbar_get_range                     XVT_CALLCONV2
         (
            WINDOW      win,
            SCROLL_TYPE t,
            int *       mnp,
            int *       mxp
         );


extern   void                                   XVT_CALLCONV1
         xvt_sbar_set_pos                       XVT_CALLCONV2
         (
            WINDOW      w,
            SCROLL_TYPE t,
            int         pos
         );


extern   void                                   XVT_CALLCONV1
         xvt_sbar_set_proportion                XVT_CALLCONV2
         (
            WINDOW      win,
            SCROLL_TYPE t,
            int         proportion
         );


extern   void                                   XVT_CALLCONV1
         xvt_sbar_set_range                     XVT_CALLCONV2
         (
            WINDOW      w,
            SCROLL_TYPE t,
            int         mn,
            int         mx
         );


#endif /* XVT_INCL_XVTSBAR */
