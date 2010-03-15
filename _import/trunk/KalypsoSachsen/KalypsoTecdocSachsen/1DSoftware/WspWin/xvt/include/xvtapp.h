/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtapp.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the application object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTAPP
#define XVT_INCL_XVTAPP

extern   void                                   XVT_CALLCONV1
         xvt_app_allow_quit                     XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_app_create                         XVT_CALLCONV2
         (
            int           argc,
            char *        argv[],
            unsigned long flags,
            EVENT_HANDLER eh,
            XVT_CONFIG *  config
         );


extern   void                                   XVT_CALLCONV1
         xvt_app_destroy                        XVT_CALLCONV2
         (
            void
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_app_escape                         XVT_CALLCONV2
         (
            int esc_code,
                ...
         );


extern   void                                   XVT_CALLCONV1
         xvt_app_get_files_count                     XVT_CALLCONV2
         (
            BOOLEAN * printp,
            int *     countp
         );


extern   void                                   XVT_CALLCONV1
         xvt_app_set_file_processed                  XVT_CALLCONV2
         (
            void
         );


extern   FILE_SPEC *                            XVT_CALLCONV1
         xvt_app_get_file                      XVT_CALLCONV2
         (
            void
         );


extern   DRAW_CTOOLS *                          XVT_CALLCONV1
         xvt_app_get_default_ctools             XVT_CALLCONV2
         (
            DRAW_CTOOLS * ctoolsp
         );


extern   void                                   XVT_CALLCONV1
         xvt_app_process_pending_events         XVT_CALLCONV2
         (
            void
         );


#endif /* XVT_INCL_XVTAPP */
