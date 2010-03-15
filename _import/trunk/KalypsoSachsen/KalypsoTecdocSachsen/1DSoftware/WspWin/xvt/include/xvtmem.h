/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtmem.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the memory object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTMEM
#define XVT_INCL_XVTMEM

extern   DATA_PTR                               XVT_CALLCONV1
         xvt_mem_alloc                          XVT_CALLCONV2
         (
            size_t size
         );


extern   void                                   XVT_CALLCONV1
         xvt_mem_free                           XVT_CALLCONV2
         (
            DATA_PTR p
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_mem_realloc                        XVT_CALLCONV2
         (
            DATA_PTR p,
            size_t   size
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_mem_rep                            XVT_CALLCONV2
         (
            DATA_PTR dst,
            DATA_PTR src,
            UINT     srclen,
            long     reps
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_mem_zalloc                         XVT_CALLCONV2
         (
            size_t size
         );


#endif /* XVT_INCL_XVTMEM */
