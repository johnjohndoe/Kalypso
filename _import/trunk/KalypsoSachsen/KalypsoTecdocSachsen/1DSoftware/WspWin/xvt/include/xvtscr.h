/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtscr.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the screen object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTSCR
#define XVT_INCL_XVTSCR

extern   void                                   XVT_CALLCONV1
         xvt_scr_beep                           XVT_CALLCONV2
         (
            void
         );


extern   WINDOW                                 XVT_CALLCONV1
         xvt_scr_get_focus_topwin               XVT_CALLCONV2
         (
            void
         );


extern   WINDOW                                 XVT_CALLCONV1
         xvt_scr_get_focus_vobj                 XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_scr_hide_cursor                    XVT_CALLCONV2
         (
            void
         );


extern   SLIST                                  XVT_CALLCONV1
         xvt_scr_list_wins                      XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_scr_set_busy_cursor                XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_scr_set_focus_vobj                 XVT_CALLCONV2
         (
            WINDOW w
         );


#endif /* XVT_INCL_XVTSCR */
