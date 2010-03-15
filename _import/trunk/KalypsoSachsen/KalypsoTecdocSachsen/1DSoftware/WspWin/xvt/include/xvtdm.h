/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtdm.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the dialog manager object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTDM
#define XVT_INCL_XVTDM

extern   void                                   XVT_CALLCONV1
         xvt_dm_post_about_box                  XVT_CALLCONV2
         (
            void
         );


extern   ASK_RESPONSE                           XVT_CALLCONV1
         xvt_dm_post_ask                        XVT_CALLCONV2
         (
            char * lbl_dflt,
            char * lbl2,
            char * lbl3,
            char * fmt,
                   ...
         );


extern   void                                   XVT_CALLCONV1
         xvt_dm_post_error                      XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );


extern   void                                   XVT_CALLCONV1
         xvt_dm_post_fatal_exit                 XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );


extern   FL_STATUS                              XVT_CALLCONV1
         xvt_dm_post_file_open                  XVT_CALLCONV2
         (
            FILE_SPEC * fsp,
            char *      msg
         );


extern   FL_STATUS                              XVT_CALLCONV1
         xvt_dm_post_file_save                  XVT_CALLCONV2
         (
            FILE_SPEC * fsp,
            char *      msg
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dm_post_font_sel                   XVT_CALLCONV2
         (
            WINDOW        win,
            XVT_FNTID     font_id,
            PRINT_RCD *   precp,
            unsigned long reserved
         );

extern   void                                   XVT_CALLCONV1
         xvt_dm_post_message                    XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );


extern   void                                   XVT_CALLCONV1
         xvt_dm_post_note                       XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dm_post_page_setup                 XVT_CALLCONV2
         (
            PRINT_RCD * precp
         );


extern   char *                                 XVT_CALLCONV1
         xvt_dm_post_string_prompt              XVT_CALLCONV2
         (
            char * msg,
            char * resp,
            int    sz_resp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dm_post_warning                    XVT_CALLCONV2
         (
            char * fmt,
                   ...
         );


#endif /* XVT_INCL_XVTDM */
