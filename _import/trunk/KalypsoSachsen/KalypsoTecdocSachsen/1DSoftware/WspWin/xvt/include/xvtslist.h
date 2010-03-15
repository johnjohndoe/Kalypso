/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtslist.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the slist object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTSLIST
#define XVT_INCL_XVTSLIST

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_slist_add_at_elt                   XVT_CALLCONV2
         (
            SLIST     x,
            SLIST_ELT e,
            char *    sx,
            long      data
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_slist_add_at_pos                   XVT_CALLCONV2
         (
            SLIST  x,
            int    index,
            char * sx,
            long   data
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_slist_add_sorted                   XVT_CALLCONV2
         (
            SLIST   x,
            char *  s,
            long    data,
            BOOLEAN unique,
            BOOLEAN case_sensitive
         );


extern   int                                    XVT_CALLCONV1
         xvt_slist_count                        XVT_CALLCONV2
         (
            SLIST x
         );


extern   SLIST                                  XVT_CALLCONV1
         xvt_slist_create                       XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_slist_debug                        XVT_CALLCONV2
         (
            SLIST x
         );


extern   void                                   XVT_CALLCONV1
         xvt_slist_destroy                      XVT_CALLCONV2
         (
            SLIST x
         );


extern   char *                                 XVT_CALLCONV1
         xvt_slist_get                          XVT_CALLCONV2
         (
            SLIST     x,
            SLIST_ELT e,
            long *    datap
         );


extern   long *                                 XVT_CALLCONV1
         xvt_slist_get_data                     XVT_CALLCONV2
         (
            SLIST_ELT e
         );


extern   char *                                 XVT_CALLCONV1
         xvt_slist_get_elt                      XVT_CALLCONV2
         (
            SLIST  x,
            int    index,
            long * datap
         );


extern   SLIST_ELT                              XVT_CALLCONV1
         xvt_slist_get_first                    XVT_CALLCONV2
         (
            SLIST x
         );


extern   SLIST_ELT                              XVT_CALLCONV1
         xvt_slist_get_next                     XVT_CALLCONV2
         (
            SLIST     x,
            SLIST_ELT e
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_slist_is_valid                     XVT_CALLCONV2
         (
            SLIST x
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_slist_rem                          XVT_CALLCONV2
         (
            SLIST     x,
            SLIST_ELT e
         );


#endif /* XVT_INCL_XVTSLIST */
