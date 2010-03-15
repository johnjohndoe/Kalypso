/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtrect.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the rectangle object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTRECT
#define XVT_INCL_XVTRECT

extern   short                                  XVT_CALLCONV1
         xvt_rect_get_height                    XVT_CALLCONV2
         (
            RCT *rctp
         );


extern   PNT *                                  XVT_CALLCONV1
         xvt_rect_get_pos                       XVT_CALLCONV2
         (
            RCT *rctp,
            PNT *pos
         );


extern   short                                  XVT_CALLCONV1
         xvt_rect_get_width                     XVT_CALLCONV2
         (
            RCT *rctp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_has_point                     XVT_CALLCONV2
         (
            RCT *rctp,
            PNT  pnt
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_intersect                     XVT_CALLCONV2
         (
            RCT *rctp,
            RCT *rctp1,
            RCT *rctp2
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_is_empty                      XVT_CALLCONV2
         (
            RCT *rctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_rect_offset                        XVT_CALLCONV2
         (
            RCT *rctp,
            short dh,
            short dv
         );


extern   void                                   XVT_CALLCONV1
         xvt_rect_set                           XVT_CALLCONV2
         (
            RCT *rctp,
            short left,
            short top,
            short right,
            short bottom
         );


extern   void                                   XVT_CALLCONV1
         xvt_rect_set_empty                     XVT_CALLCONV2
         (
            RCT *rctp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_set_height                    XVT_CALLCONV2
         (
            RCT *rctp,
            short height
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_set_pos                       XVT_CALLCONV2
         (
            RCT *rctp,
            PNT  pos
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_rect_set_width                     XVT_CALLCONV2
         (
            RCT *rctp,
            short width
         );


#endif /* XVT_INCL_XVTRECT */
