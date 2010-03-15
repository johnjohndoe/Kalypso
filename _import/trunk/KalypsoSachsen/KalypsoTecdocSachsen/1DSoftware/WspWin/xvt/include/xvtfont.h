/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvtfont.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the font object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTFONT
#define XVT_INCL_XVTFONT

extern   long                    				XVT_CALLCONV1
         xvt_fmap_get_families                  XVT_CALLCONV2
         (
			PRINT_RCD 			*precp,
			char** 				family_array,
			long                max_families
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_fmap_get_family_styles             XVT_CALLCONV2
         (
            PRINT_RCD            *precp,      
            char                 *family,      
            XVT_FONT_STYLE_MASK  *style_array, 
            long                 max_styles   
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_fmap_get_familysize_styles         XVT_CALLCONV2
         (
            PRINT_RCD            *precp, 
            char                 *family, 
            long                 size,   
            XVT_FONT_STYLE_MASK  *style_array,
            long                 max_styles  
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_fmap_get_family_sizes              XVT_CALLCONV2
         (
			PRINT_RCD 			*precp,
            char                *family,
			long                *size_array,
			BOOLEAN				*scalable,
			long                max_sizes
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_fmap_get_familystyle_sizes         XVT_CALLCONV2
         (
			PRINT_RCD 			*precp,
            char                *family,
			XVT_FONT_STYLE_MASK style,
			long                *size_array,
			BOOLEAN				*scalable,
			long                max_sizes
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_copy                          XVT_CALLCONV2
         (
            XVT_FNTID          dest_font_id,
            XVT_FNTID          src_font_id,
            XVT_FONT_ATTR_MASK mask
         ) ;

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_font_create                        XVT_CALLCONV2
         (
            void
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_map_using_default                   XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_deserialize                   XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char* buf
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_destroy                       XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_font_get_app_data                  XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_get_family                    XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char* buf,
            long max_buf
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_get_family_mapped             XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char* buf,
            long max_buf
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_get_metrics                   XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            int *   leadingp,
            int *   ascentp,
            int *   descentp
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_get_native_desc               XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char* buf,
            long max_buf
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_font_get_size                      XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_font_get_size_mapped               XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   XVT_FONT_STYLE_MASK                    XVT_CALLCONV1
         xvt_font_get_style                     XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   XVT_FONT_STYLE_MASK                    XVT_CALLCONV1
         xvt_font_get_style_mapped              XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   WINDOW                                 XVT_CALLCONV1
         xvt_font_get_win                       XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_is_mapped                     XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_is_print                      XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_is_scalable                   XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_is_valid                      XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_map                           XVT_CALLCONV2
         (
            XVT_FNTID font_id, 
			WINDOW win
         ) ;

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_font_has_valid_native_desc          XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

extern   long                                   XVT_CALLCONV1
         xvt_font_serialize                     XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char* buf,
            long max_buf
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_set_app_data                  XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            long    app_data
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_set_family                    XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char*   family
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_set_native_desc               XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            char*   native_desc
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_set_size                      XVT_CALLCONV2
         (
            XVT_FNTID font_id,
            long    size
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_set_style                     XVT_CALLCONV2
         (
            XVT_FNTID           font_id,
            XVT_FONT_STYLE_MASK mask
         ) ;

extern   void                                   XVT_CALLCONV1
         xvt_font_unmap                         XVT_CALLCONV2
         (
            XVT_FNTID font_id
         ) ;

#endif /* XVT_INCL_XVTFONT */
