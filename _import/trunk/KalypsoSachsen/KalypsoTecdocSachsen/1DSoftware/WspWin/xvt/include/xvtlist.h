/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtlist.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the list object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTLIST
#define XVT_INCL_XVTLIST

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_add                           XVT_CALLCONV2
         (
            WINDOW win,
            int    index,
            char * sx
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_clear                         XVT_CALLCONV2
         (
            WINDOW win
         );


extern   int                                    XVT_CALLCONV1
         xvt_list_count_all                     XVT_CALLCONV2
         (
            WINDOW win
         );


extern   int                                    XVT_CALLCONV1
         xvt_list_count_sel                     XVT_CALLCONV2
         (
            WINDOW win
         );


extern   SLIST                                  XVT_CALLCONV1
         xvt_list_get_all                       XVT_CALLCONV2
         (
            WINDOW win
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_get_elt                       XVT_CALLCONV2
         (
            WINDOW win,
            int    index,
            char * s,
            int    sz_s
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_get_first_sel                 XVT_CALLCONV2
         (
            WINDOW win,
            char * s,
            int    sz_s
         );


extern   SLIST                                  XVT_CALLCONV1
         xvt_list_get_sel                       XVT_CALLCONV2
         (
            WINDOW win
         );


extern   int                                    XVT_CALLCONV1
         xvt_list_get_sel_index                 XVT_CALLCONV2
         (
            WINDOW win
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_is_sel                        XVT_CALLCONV2
         (
            WINDOW win,
            int    index
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_rem                           XVT_CALLCONV2
         (
            WINDOW win,
            int    index
         );


extern   void                                   XVT_CALLCONV1
         xvt_list_resume                        XVT_CALLCONV2
         (
            WINDOW win
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_list_set_sel                       XVT_CALLCONV2
         (
            WINDOW  win,
            int     index,
            BOOLEAN select
         );


extern   void                                   XVT_CALLCONV1
         xvt_list_suspend                       XVT_CALLCONV2
         (
            WINDOW win
         );


#endif /* XVT_INCL_XVTLIST */
