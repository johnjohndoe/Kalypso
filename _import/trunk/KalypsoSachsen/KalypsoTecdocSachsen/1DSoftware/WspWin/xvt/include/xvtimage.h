/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtimage.h,v $ 
 *  $Revision: 1.5 $
 *
 *  Purpose: Interface to the image object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTIMAGE
#define XVT_INCL_XVTIMAGE

#include "xvtiostr.h"

extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_create                       XVT_CALLCONV2
         (
            XVT_IMAGE_FORMAT format,
            short            width,
            short            height,
            XVT_IMAGE_ATTR   reserved
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_destroy                      XVT_CALLCONV2
         (
            XVT_IMAGE image
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_duplicate                    XVT_CALLCONV2
         (
            XVT_IMAGE image
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_fill_rect                    XVT_CALLCONV2
         (
            XVT_IMAGE image,
            COLOR     color,
            RCT *     lrectp
         );


extern   COLOR                                  XVT_CALLCONV1
         xvt_image_get_clut                     XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     index
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_get_dimensions               XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short *   width,
            short *   height
         );


extern   XVT_IMAGE_FORMAT                       XVT_CALLCONV1
         xvt_image_get_format                   XVT_CALLCONV2
         (
            XVT_IMAGE image
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_get_from_pmap                XVT_CALLCONV2
         (
            XVT_IMAGE  dst_image,
            XVT_PIXMAP src_pixmap,
            RCT *      dst_rect,
            RCT *      src_rect
         );


extern   short                                  XVT_CALLCONV1
         xvt_image_get_ncolors                  XVT_CALLCONV2
         (
            XVT_IMAGE image
         );


extern   COLOR                                  XVT_CALLCONV1
         xvt_image_get_pixel                    XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     x,
            short     y
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_get_resolution               XVT_CALLCONV2
         (
            XVT_IMAGE image,
            long *    hres,
            long *    vres
         );


extern   DATA_PTR                               XVT_CALLCONV1
         xvt_image_get_scanline                 XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     linenum
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read                         XVT_CALLCONV2
         (
            char * filename
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_bmp                     XVT_CALLCONV2
         (
            char * filename
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_bmp_from_iostr          XVT_CALLCONV2
         (
            XVT_IOSTREAM iostr
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_macpict                 XVT_CALLCONV2
         (
            char * filename
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_macpict_from_iostr      XVT_CALLCONV2
         (
            XVT_IOSTREAM iostr
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_xbm                     XVT_CALLCONV2
         (
            char * filename
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_xbm_from_iostr          XVT_CALLCONV2
         (
            XVT_IOSTREAM iostr
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_xpm                     XVT_CALLCONV2
         (
            char * filename
         );


extern   XVT_IMAGE                              XVT_CALLCONV1
         xvt_image_read_xpm_from_iostr          XVT_CALLCONV2
         (
            XVT_IOSTREAM iostr
         );

extern   void                                   XVT_CALLCONV1
         xvt_image_set_clut                     XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     index,
            COLOR     color
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_set_ncolors                  XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     ncolors
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_set_pixel                    XVT_CALLCONV2
         (
            XVT_IMAGE image,
            short     x,
            short     y,
            COLOR     color
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_set_resolution               XVT_CALLCONV2
         (
            XVT_IMAGE image,
            long      hres,
            long      vres
         );


extern   void                                   XVT_CALLCONV1
         xvt_image_transfer                     XVT_CALLCONV2
         (
            XVT_IMAGE dst_image,
            XVT_IMAGE src_image,
            RCT *     dst_rect,
            RCT *     src_rect
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_image_write_macpict_to_iostr       XVT_CALLCONV2
         (
            XVT_IMAGE    image,
            XVT_IOSTREAM iostr
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_image_write_bmp_to_iostr           XVT_CALLCONV2
         (
            XVT_IMAGE    image,
            XVT_IOSTREAM iostr
         );


#endif /* XVT_INCL_XVTIMAGE */
