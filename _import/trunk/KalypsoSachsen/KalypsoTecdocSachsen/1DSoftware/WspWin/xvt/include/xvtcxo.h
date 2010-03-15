/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtcxo.h,v $ 
 *  $Revision: 1.5 $
 *
 *  Purpose: Interface to the cxo object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTCXO
#define XVT_INCL_XVTCXO

extern   long                                   XVT_CALLCONV1
         xvt_cxo_call_next                      XVT_CALLCONV2
         (
            XVT_CXO cxo,
            EVENT * event_p
         );

extern   XVT_CXO                                XVT_CALLCONV1
         xvt_cxo_create                         XVT_CALLCONV2
         (
            WINDOW                win,
            const long            state_data,
            XVT_CXO_INSERTION     where,
            XVT_CXO_EVENT_HANDLER cxo_eh,
            EVENT_MASK            mask,
            char *                class_name,
            long                  cxo_id
         );

extern   void                                   XVT_CALLCONV1
         xvt_cxo_destroy                        XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   long                                   XVT_CALLCONV1
         xvt_cxo_dispatch_msg                   XVT_CALLCONV2
         (
            XVT_CXO cxo,
            long    msg_id,
            void *  data
         );

extern   char *                                 XVT_CALLCONV1
         xvt_cxo_get_class_name                 XVT_CALLCONV2
         (
            XVT_CXO cxo,
            char *  class_name,
            int     sz_class_name
         );

extern   WINDOW                                 XVT_CALLCONV1
         xvt_cxo_get_win                        XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   long                                   XVT_CALLCONV1
         xvt_cxo_get_data                       XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   XVT_CXO_EVENT_HANDLER                  XVT_CALLCONV1
         xvt_cxo_get_event_handler              XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   EVENT_MASK                             XVT_CALLCONV1
         xvt_cxo_get_event_mask                 XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_cxo_is_valid                       XVT_CALLCONV2
         (
            XVT_CXO cxo
         );

extern   void                                   XVT_CALLCONV1
         xvt_cxo_set_data                       XVT_CALLCONV2
         (
            XVT_CXO cxo,
            long    state_data
         );

extern   void                                   XVT_CALLCONV1
         xvt_cxo_set_event_handler              XVT_CALLCONV2
         (
            XVT_CXO               cxo,
            XVT_CXO_EVENT_HANDLER cxo_eh
         );

extern   void                                   XVT_CALLCONV1
         xvt_cxo_set_event_mask                 XVT_CALLCONV2
         (
            XVT_CXO    cxo,
            EVENT_MASK mask
         );

#endif /* XVT_INCL_XVTCXO */

