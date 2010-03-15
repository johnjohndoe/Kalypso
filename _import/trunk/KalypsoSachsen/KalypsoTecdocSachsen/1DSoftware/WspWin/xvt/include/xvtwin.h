/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtwin.h,v $ 
 *  $Revision: 1.7 $
 *
 *  Purpose: Interface to the window object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTWIN
#define XVT_INCL_XVTWIN

extern   WINDOW                                 XVT_CALLCONV1
         xvt_win_create                         XVT_CALLCONV2
         (
            WIN_TYPE      wtype,
            RCT *         rct_p,
            char *        title,
            int           menu_rid,
            WINDOW        parent_win,
            long          win_flags,
            EVENT_MASK    mask,
            EVENT_HANDLER eh,
            long          app_data
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_win_create_def                     XVT_CALLCONV2
         (
            WIN_DEF *     win_def_p,
            WINDOW        parent_win,
            EVENT_MASK    mask,
            EVENT_HANDLER eh,
            long          app_data
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_win_create_res                     XVT_CALLCONV2
         (
            int           rid,
            WINDOW        parent_win,
            EVENT_MASK    mask,
            EVENT_HANDLER eh,
            long          app_data
         );

extern   long                                   XVT_CALLCONV1
         xvt_win_dispatch_event                 XVT_CALLCONV2
         (
            WINDOW  win,
            EVENT * ep
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_win_enum_wins                      XVT_CALLCONV2
         (
            WINDOW            parent_win,
            XVT_ENUM_CHILDREN func,
            long              data,
            unsigned long     reserved
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_win_get_ctl                        XVT_CALLCONV2
         (
            WINDOW win,
            int    cid
         );

extern   XVT_COLOR_COMPONENT *                  XVT_CALLCONV1
         xvt_win_get_ctl_colors                 XVT_CALLCONV2
         (
            WINDOW win
         );

extern   COLOR                                  XVT_CALLCONV1
         xvt_win_get_ctl_color_component        XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype
         );

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_win_get_ctl_font                   XVT_CALLCONV2
         (
            WINDOW win
         );

extern   CURSOR                                 XVT_CALLCONV1
         xvt_win_get_cursor                     XVT_CALLCONV2
         (
            WINDOW w
         );

extern   XVT_CXO                                XVT_CALLCONV1
         xvt_win_get_cxo                        XVT_CALLCONV2
         (
            WINDOW w,
            char * class_name,
            long   cxo_id
         );

extern   EVENT_MASK                             XVT_CALLCONV1
         xvt_win_get_event_mask                 XVT_CALLCONV2
         (
            WINDOW win
         );

extern   EVENT_HANDLER                          XVT_CALLCONV1
         xvt_win_get_handler                    XVT_CALLCONV2
         (
            WINDOW win
         );

extern   XVT_NAV                                XVT_CALLCONV1
         xvt_win_get_nav                        XVT_CALLCONV2
         (
            WINDOW win
         );

extern   TXEDIT                                 XVT_CALLCONV1
         xvt_win_get_tx                         XVT_CALLCONV2
         (
            WINDOW win,
            int    cid
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_win_has_menu                       XVT_CALLCONV2
         (
            WINDOW w
         );

extern   SLIST                                  XVT_CALLCONV1
         xvt_win_list_wins                      XVT_CALLCONV2
         (
            WINDOW        parent_win,
            unsigned long reserved
         );

extern   SLIST                                  XVT_CALLCONV1
         xvt_win_list_cxos                      XVT_CALLCONV2
         (
            WINDOW win    
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_process_modal                  XVT_CALLCONV2
         (
            WINDOW win
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_release_pointer                XVT_CALLCONV2
         (
            void
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_caret_pos                  XVT_CALLCONV2
         (
            WINDOW w,
            PNT    pos
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_caret_size                 XVT_CALLCONV2
         (
            WINDOW win,
            int    width,
            int    height
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_caret_visible              XVT_CALLCONV2
         (
            WINDOW  w,
            BOOLEAN visible
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_ctl_colors                 XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_COMPONENT * colors,
            XVT_COLOR_ACTION      action
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_ctl_color_component        XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype,
            COLOR                 color
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_ctl_font                   XVT_CALLCONV2
         (
            WINDOW win,
            XVT_FNTID font_id
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_cursor                     XVT_CALLCONV2
         (
            WINDOW w,
            CURSOR c
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_doc_title                  XVT_CALLCONV2
         (
            WINDOW w,
            char * title
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_event_mask                 XVT_CALLCONV2
         (
            WINDOW     win,
            EVENT_MASK mask
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_set_handler                    XVT_CALLCONV2
         (
            WINDOW        win,
            EVENT_HANDLER eh
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_trap_pointer                   XVT_CALLCONV2
         (
            WINDOW w
         );

extern   void                                   XVT_CALLCONV1
         xvt_win_unset_ctl_color_component      XVT_CALLCONV2
         (
            WINDOW                win,
            XVT_COLOR_TYPE        ctype
         );

#endif /* XVT_INCL_XVTWIN */

