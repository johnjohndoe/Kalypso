/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtres.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the resource object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTRES
#define XVT_INCL_XVTRES

extern   void                                   XVT_CALLCONV1
         xvt_res_free_menu_tree                 XVT_CALLCONV2
         (
            MENU_ITEM * mip
         );


extern   void                                   XVT_CALLCONV1
         xvt_res_free_win_def                   XVT_CALLCONV2
         (
            WIN_DEF * windef
         );

extern   char *                                 XVT_CALLCONV1
         xvt_res_get_dlg_data                   XVT_CALLCONV2
         (
            int rid,
            int cid,
            int tag
         );


extern   WIN_DEF *                              XVT_CALLCONV1
         xvt_res_get_dlg_def                    XVT_CALLCONV2
         (
            int rid
         );


extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_res_get_font                       XVT_CALLCONV2
         (
            int rid
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_res_get_image                      XVT_CALLCONV2
         (
            int rid
         );


extern   char *                                 XVT_CALLCONV1
         xvt_res_get_image_data                 XVT_CALLCONV2
         (
            int rid,
            int tag
         );


extern   MENU_ITEM *                            XVT_CALLCONV1
         xvt_res_get_menu                       XVT_CALLCONV2
         (
            int rid
         );


extern   char *                                 XVT_CALLCONV1
         xvt_res_get_menu_data                  XVT_CALLCONV2
         (
            int rid,
            int cid,
            int tag
         );


extern   char *                                 XVT_CALLCONV1
         xvt_res_get_str                        XVT_CALLCONV2
         (
            int    rid,
            char * s,
            int    sz_s
         );


extern   SLIST                                  XVT_CALLCONV1
         xvt_res_get_str_list                   XVT_CALLCONV2
         (
            int rid_first,
            int rid_last
         );


extern   char *                                 XVT_CALLCONV1
         xvt_res_get_win_data                   XVT_CALLCONV2
         (
            int rid,
            int cid,
            int tag
         );


extern   WIN_DEF *                              XVT_CALLCONV1
         xvt_res_get_win_def                    XVT_CALLCONV2
         (
            int rid
         );


#endif /* XVT_INCL_XVTRES */
