/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtgmem.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the global memory object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTGMEM
#define XVT_INCL_XVTGMEM

extern   GHANDLE                                XVT_CALLCONV1
         xvt_gmem_alloc                         XVT_CALLCONV2
         (
            long size
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_gmem_free                          XVT_CALLCONV2
         (
            GHANDLE h
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_gmem_lock                          XVT_CALLCONV2
         (
            GHANDLE h
         );


extern   GHANDLE                                XVT_CALLCONV1
         xvt_gmem_realloc                       XVT_CALLCONV2
         (
            GHANDLE h,
            long    size
         );


extern   long                                   XVT_CALLCONV1
         xvt_gmem_get_size                      XVT_CALLCONV2
         (
            GHANDLE h
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_gmem_unlock                        XVT_CALLCONV2
         (
            GHANDLE h
         );


#endif /* XVT_INCL_XVTGMEM */
