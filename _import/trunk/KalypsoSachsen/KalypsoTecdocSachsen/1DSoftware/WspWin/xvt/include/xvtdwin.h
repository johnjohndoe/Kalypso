/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvtdwin.h,v $ 
 *  $Revision: 1.4 $
 *
 *  Purpose: Interface to the drawable window object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTDWIN
#define XVT_INCL_XVTDWIN

extern   void                                   XVT_CALLCONV1
         xvt_dwin_clear                         XVT_CALLCONV2
         (
            WINDOW win,
            COLOR  color
         );


extern   PICTURE                                XVT_CALLCONV1
         xvt_dwin_close_pict                    XVT_CALLCONV2
         (
            WINDOW win
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_aline                    XVT_CALLCONV2
         (
            WINDOW  win,
            PNT     pnt,
            BOOLEAN start_arrow,
            BOOLEAN end_arrow
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_arc                      XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp,
            int    start_x,
            int    start_y,
            int    stop_x,
            int    stop_y
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_icon                     XVT_CALLCONV2
         (
            WINDOW win,
            int    x,
            int    y,
            int    rid
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_image                    XVT_CALLCONV2
         (
            WINDOW    vwWin,
            XVT_IMAGE vimImage,
            RCT *     p_rctDst,
            RCT *     p_rctSrc
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_line                     XVT_CALLCONV2
         (
            WINDOW win,
            PNT    pnt
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_oval                     XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_pict                     XVT_CALLCONV2
         (
            WINDOW  win,
            PICTURE pic,
            RCT *   lrctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_pie                      XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp,
            int    start_x,
            int    start_y,
            int    stop_x,
            int    stop_y
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_pmap                     XVT_CALLCONV2
         (
            WINDOW     vwWin,
            XVT_PIXMAP vpmPixmap,
            RCT *      p_rctDst,
            RCT *      p_rctSrc
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_polygon                  XVT_CALLCONV2
         (
            WINDOW win,
            PNT *  lpntp,
            int    npnts
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_polyline                 XVT_CALLCONV2
         (
            WINDOW win,
            PNT *  lpntp,
            int    npnts
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_rect                     XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_roundrect                XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp,
            int    oval_width,
            int    oval_height
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_set_pos                  XVT_CALLCONV2
         (
            WINDOW win,
            PNT    lpnt
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_draw_text                     XVT_CALLCONV2
         (
            WINDOW win,
            int    x,
            int    y,
            char * s,
            int    len
         );


extern   RCT *                                  XVT_CALLCONV1
         xvt_dwin_get_clip                      XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  lrctp
         );


extern   DRAW_CTOOLS *                          XVT_CALLCONV1
         xvt_dwin_get_draw_ctools               XVT_CALLCONV2
         (
            WINDOW        win,
            DRAW_CTOOLS * ctoolsp
         );

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_dwin_get_font                      XVT_CALLCONV2
         (
            WINDOW win
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font                      XVT_CALLCONV2
         (
            WINDOW  win,
            XVT_FNTID font_id
         );

extern   long                                   XVT_CALLCONV1
         xvt_dwin_get_font_app_data             XVT_CALLCONV2
         (
            WINDOW  win
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dwin_get_font_family               XVT_CALLCONV2
         (
            WINDOW  win,
            char *buf,
            long max_buf
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dwin_get_font_family_mapped        XVT_CALLCONV2
         (
            WINDOW  win,
            char *buf,
            long max_buf
         );

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dwin_get_font_native_desc          XVT_CALLCONV2
         (
            WINDOW  win,
            char *buf,
            long max_buf
         );

extern   long                                   XVT_CALLCONV1
         xvt_dwin_get_font_size                 XVT_CALLCONV2
         (
            WINDOW  win
         );

extern   long                                   XVT_CALLCONV1
         xvt_dwin_get_font_size_mapped          XVT_CALLCONV2
         (
            WINDOW  win
         );

extern   XVT_FONT_STYLE_MASK                    XVT_CALLCONV1
         xvt_dwin_get_font_style                XVT_CALLCONV2
         (
            WINDOW  win
         );

extern   XVT_FONT_STYLE_MASK                    XVT_CALLCONV1
         xvt_dwin_get_font_style_mapped         XVT_CALLCONV2
         (
            WINDOW  win
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font_app_data             XVT_CALLCONV2
         (
            WINDOW win,
            long   app_data 
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font_family               XVT_CALLCONV2
         (
            WINDOW win,
            char*  family
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font_native_desc          XVT_CALLCONV2
         (
            WINDOW win,
            char*  native_desc
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font_size                 XVT_CALLCONV2
         (
            WINDOW win,
            long   size 
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_font_style                XVT_CALLCONV2
         (
            WINDOW win,
            XVT_FONT_STYLE_MASK mask
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_get_font_metrics              XVT_CALLCONV2
         (
            WINDOW win,
            int *   leadingp,
            int *   ascentp,
            int *   descentp
         );

extern   int                                    XVT_CALLCONV1
         xvt_dwin_get_text_width                XVT_CALLCONV2
         (
            WINDOW win,
            char * s,
            int    len
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_invalidate_rect               XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  lrctp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dwin_is_update_needed              XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  lrctp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_dwin_open_pict                     XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_scroll_rect                   XVT_CALLCONV2
         (
            WINDOW win,
            RCT *  lrctp,
            int    dh,
            int    dv
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_back_color                XVT_CALLCONV2
         (
            WINDOW win,
            COLOR  color
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_cbrush                    XVT_CALLCONV2
         (
            WINDOW   win,
            CBRUSH * cbrush
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_clip                      XVT_CALLCONV2
         (
            WINDOW w,
            RCT *  rctp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_cpen                      XVT_CALLCONV2
         (
            WINDOW win,
            CPEN * cpen
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_draw_ctools               XVT_CALLCONV2
         (
            WINDOW        win,
            DRAW_CTOOLS * ctoolsp
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_draw_mode                 XVT_CALLCONV2
         (
            WINDOW    win,
            DRAW_MODE mode
         );

extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_fore_color                XVT_CALLCONV2
         (
            WINDOW win,
            COLOR  color
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_std_cbrush                XVT_CALLCONV2
         (
            WINDOW win,
            long   flag
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_set_std_cpen                  XVT_CALLCONV2
         (
            WINDOW win,
            long   flag
         );


extern   void                                   XVT_CALLCONV1
         xvt_dwin_update                        XVT_CALLCONV2
         (
            WINDOW w
         );


#endif /* XVT_INCL_XVTDWIN */
