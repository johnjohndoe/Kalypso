/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtctl.h,v $ 
 *  $Revision: 1.5 $
 *
 *  Purpose: Interface to the control objects. 
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTCTL
#define XVT_INCL_XVTCTL

extern  void                                    XVT_CALLCONV1 
        xvt_ctl_check_radio_button              XVT_CALLCONV2
        (
            WINDOW  win, 
            WINDOW *ctls, 
            int     nctls
        ) ;

extern   WINDOW                                 XVT_CALLCONV1
         xvt_ctl_create                         XVT_CALLCONV2
         (
            WIN_TYPE wtype,
            RCT *    rct_p,
            char *   title,
            WINDOW   parent_win,
            long     ctl_flags,
            long     app_data,
            int      ctrl_id
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_ctl_create_def                     XVT_CALLCONV2
         (
            WIN_DEF * win_def_p,
            WINDOW    parent_win,
            long      app_data
         );

extern   XVT_COLOR_COMPONENT *                  XVT_CALLCONV1
         xvt_ctl_get_colors                     XVT_CALLCONV2
         (
            WINDOW ctl_win
         );

extern   COLOR                                  XVT_CALLCONV1
         xvt_ctl_get_color_component            XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype
         );

extern   int			                XVT_CALLCONV1
         xvt_ctl_get_id                         XVT_CALLCONV2
         (
            WINDOW ctl_win
         );

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_ctl_get_font                       XVT_CALLCONV2
         (
            WINDOW ctl_win
         );

extern   XVT_COLOR_COMPONENT *                  XVT_CALLCONV1
         xvt_ctl_get_native_colors              XVT_CALLCONV2
         (
            WIN_TYPE type
         );

extern   COLOR                                  XVT_CALLCONV1
         xvt_ctl_get_native_color_component     XVT_CALLCONV2
         (
            WIN_TYPE type,
            XVT_COLOR_TYPE ctype
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_get_text_sel                   XVT_CALLCONV2
         (
            WINDOW win,
            int *  first,
            int *  last
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_ctl_is_checked                     XVT_CALLCONV2
         (
            WINDOW win
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_set_checked                    XVT_CALLCONV2
         (
            WINDOW  win,
            BOOLEAN check
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_set_colors                     XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_COMPONENT * colors,
            XVT_COLOR_ACTION      action
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_set_color_component            XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype,
            COLOR                 color
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_set_font                       XVT_CALLCONV2
         (
            WINDOW ctl_win,
            XVT_FNTID font_id
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_set_text_sel                   XVT_CALLCONV2
         (
            WINDOW win,
            int    first,
            int    last
         );

extern   void                                   XVT_CALLCONV1
         xvt_ctl_unset_color_component          XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype
         );


#endif /* XVT_INCL_XVTCTL */
