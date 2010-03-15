/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtnav.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the nav object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTNAV
#define XVT_INCL_XVTNAV

extern   XVT_NAV                                XVT_CALLCONV1
         xvt_nav_create                         XVT_CALLCONV2
         (
            WINDOW win,
            SLIST  win_list
         );

extern   void                                   XVT_CALLCONV1
         xvt_nav_destroy                        XVT_CALLCONV2
         (
            XVT_NAV nav
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_nav_add_win                        XVT_CALLCONV2
         (
            XVT_NAV           nav,
            WINDOW            win,
            WINDOW            refwin,
            XVT_NAV_INSERTION where
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_nav_rem_win                        XVT_CALLCONV2
         (
            XVT_NAV nav,
            WINDOW win
         );

extern   SLIST                                  XVT_CALLCONV1
         xvt_nav_list_wins                      XVT_CALLCONV2
         (
            XVT_NAV nav
         );

#endif /* XVT_INCL_XVTNAV */

