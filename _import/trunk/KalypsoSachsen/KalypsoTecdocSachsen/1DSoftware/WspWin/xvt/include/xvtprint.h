/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtprint.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the print object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTPRINT
#define XVT_INCL_XVTPRINT

extern   void                                   XVT_CALLCONV1
         xvt_print_close                        XVT_CALLCONV2
         (
            void
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_print_close_page                   XVT_CALLCONV2
         (
            PRINT_RCD * precp
         );


extern   PRINT_RCD *                            XVT_CALLCONV1
         xvt_print_create                       XVT_CALLCONV2
         (
            int * sizep
         );


extern   WINDOW                                 XVT_CALLCONV1
         xvt_print_create_win                   XVT_CALLCONV2
         (
            PRINT_RCD * precp,
            char *      title
         );


extern   void                                   XVT_CALLCONV1
         xvt_print_destroy                      XVT_CALLCONV2
         (
            PRINT_RCD * precp
         );


extern   RCT *                                  XVT_CALLCONV1
         xvt_print_get_next_band                XVT_CALLCONV2
         (
            void
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_print_is_valid                     XVT_CALLCONV2
         (
            PRINT_RCD * precp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_print_open                         XVT_CALLCONV2
         (
            void
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_print_open_page                    XVT_CALLCONV2
         (
            PRINT_RCD * precp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_print_start_thread                 XVT_CALLCONV2
         (
             XVT_PRINT_FUNCTION func,
             long               data
         );

#endif /* XVT_INCL_XVTPRINT */
