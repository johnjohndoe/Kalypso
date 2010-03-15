/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtpalet.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the palette object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTPALET
#define XVT_INCL_XVTPALET

extern   XVT_PALETTE                            XVT_CALLCONV1
         xvt_palet_create                       XVT_CALLCONV2
         (
            XVT_PALETTE_TYPE   type,
            XVT_PALETTE_ATTR   reserved
         );


extern   XVT_PALETTE                            XVT_CALLCONV1
         xvt_palet_default                      XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_palet_destroy                      XVT_CALLCONV2
         (
            XVT_PALETTE palet
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_get_size                     XVT_CALLCONV2
         (
            XVT_PALETTE palet
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_get_ncolors                  XVT_CALLCONV2
         (
            XVT_PALETTE palet
         );


extern   XVT_PALETTE_TYPE                       XVT_CALLCONV1
         xvt_palet_get_type                     XVT_CALLCONV2
         (
            XVT_PALETTE palet
         );


extern   void                                   XVT_CALLCONV1
         xvt_palet_set_tolerance                XVT_CALLCONV2
         (
            XVT_PALETTE palet,
			short       tolerance
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_get_tolerance                XVT_CALLCONV2
         (
            XVT_PALETTE palet
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_get_colors                   XVT_CALLCONV2
         (
            XVT_PALETTE palet,
            COLOR *     colorsp,
            short       maxcolors
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_add_colors                   XVT_CALLCONV2
         (
            XVT_PALETTE palet,
            COLOR *     colorsp,
            short       numcolors
         );


extern   short                                  XVT_CALLCONV1
         xvt_palet_add_colors_from_image        XVT_CALLCONV2
         (
            XVT_PALETTE palet,
            XVT_IMAGE   image
         );


#endif /* XVT_INCL_XVTPALET */
