/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtvobj.h,v $ 
 *  $Revision: 1.7 $
 *
 *  Purpose: Interface to the visible object object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTVOBJ
#define XVT_INCL_XVTVOBJ

extern   void                                   XVT_CALLCONV1
         xvt_vobj_destroy                       XVT_CALLCONV2
         (
            WINDOW w
         );

extern   long                                   XVT_CALLCONV1
         xvt_vobj_get_attr                      XVT_CALLCONV2
         (
            WINDOW win,
            int    attr
         );

extern   RCT *                                  XVT_CALLCONV1
         xvt_vobj_get_client_rect               XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  lrctp
         );

extern   long                                   XVT_CALLCONV1
         xvt_vobj_get_data                      XVT_CALLCONV2
         (
            WINDOW w
         );

extern   long                                   XVT_CALLCONV1
         xvt_vobj_get_flags                     XVT_CALLCONV2
         (
            WINDOW w
         );

extern   XVT_FORMAT_HANDLER                     XVT_CALLCONV1
         xvt_vobj_get_formatter                 XVT_CALLCONV2
         (
            WINDOW win
         );

extern   void *                                 XVT_CALLCONV1
         xvt_vobj_get_formatter_data            XVT_CALLCONV2
         (
            WINDOW win
         );

extern   RCT *                                  XVT_CALLCONV1
         xvt_vobj_get_outer_rect                XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  rctp
         );

extern   XVT_PALETTE                            XVT_CALLCONV1
         xvt_vobj_get_palet                     XVT_CALLCONV2
         (
            WINDOW vwWin
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_vobj_get_parent                    XVT_CALLCONV2
         (
            WINDOW w
         );

extern   char *                                 XVT_CALLCONV1
         xvt_vobj_get_title                     XVT_CALLCONV2
         (
            WINDOW w,
            char * title,
            int    sz_title
         );

extern   WIN_TYPE                               XVT_CALLCONV1
         xvt_vobj_get_type                      XVT_CALLCONV2
         (
            WINDOW w
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_vobj_is_focusable                  XVT_CALLCONV2
         (
            WINDOW w
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_vobj_is_valid                      XVT_CALLCONV2
         (
            WINDOW w
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_move                          XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  grctp
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_raise                         XVT_CALLCONV2
         (
            WINDOW w
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_attr                      XVT_CALLCONV2
         (
            WINDOW win,
            int    attr,
            long   value
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_data                      XVT_CALLCONV2
         (
            WINDOW w,
            long   data
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_enabled                   XVT_CALLCONV2
         (
            WINDOW  w,
            BOOLEAN enable
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_formatter                 XVT_CALLCONV2
         (
            WINDOW             win,
            XVT_FORMAT_HANDLER handler,
			void *             data
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_formatter_data            XVT_CALLCONV2
         (
            WINDOW             win,
			void *             data
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_palet                     XVT_CALLCONV2
         (
            WINDOW      vwWin,
            XVT_PALETTE vpPalette
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_title                     XVT_CALLCONV2
         (
            WINDOW w,
            char * title
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_set_visible                   XVT_CALLCONV2
         (
            WINDOW  w,
            BOOLEAN show
         );

extern   void                                   XVT_CALLCONV1
         xvt_vobj_translate_points              XVT_CALLCONV2
         (
            WINDOW from,
            WINDOW to,
            PNT *  pntp,
            int    npnts
         );

#endif /* XVT_INCL_XVTVOBJ */

