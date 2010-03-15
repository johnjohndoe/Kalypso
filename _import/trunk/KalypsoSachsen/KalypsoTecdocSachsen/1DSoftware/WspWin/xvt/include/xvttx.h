/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvttx.h,v $ 
 *  $Revision: 1.6 $
 *
 *  Purpose: Interface to the text edit object.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVTTX
#define XVT_INCL_XVTTX

/*
*	Textedit Versions
*/
#define XVT_TX_VERSION_MAJOR  4
#define XVT_TX_VERSION_MINOR  57
#define XVT_TX_VERSION_PATCH  0
#define XVT_TX_VERSION XVT_MAKE_VERSION(XVT_TX_VERSION_MAJOR, XVT_TX_VERSION_MINOR, XVT_TX_VERSION_PATCH)

extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_add_par                         XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum,
            char * s
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_append                          XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum,
            char * s
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_clear                           XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   TXEDIT                                 XVT_CALLCONV1
         xvt_tx_create                          XVT_CALLCONV2
         (
            WINDOW   win,
            RCT *    lrctp,
            unsigned attr,
            XVT_FNTID  font_id,
            int      margin,
            int      limit
         );

extern   XVT_FNTID                              XVT_CALLCONV1
         xvt_tx_get_font                        XVT_CALLCONV2
         (
            TXEDIT tx
         );

extern   void                                   XVT_CALLCONV1
         xvt_tx_set_font                        XVT_CALLCONV2
         (
            TXEDIT  tx,
            XVT_FNTID font_id
         );

extern   TXEDIT                                 XVT_CALLCONV1
         xvt_tx_create_def                      XVT_CALLCONV2
         (
            WIN_DEF * win_def_p,
            WINDOW    parent_win,
            long      app_data
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_destroy                         XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   TXEDIT                                 XVT_CALLCONV1
         xvt_tx_get_active                      XVT_CALLCONV2
         (
            void
         );


extern   unsigned                               XVT_CALLCONV1
         xvt_tx_get_attr                        XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   unsigned                               XVT_CALLCONV1
         xvt_tx_get_cid                         XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   long                                   XVT_CALLCONV1
         xvt_tx_get_data                        XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   int                                    XVT_CALLCONV1
         xvt_tx_get_limit                       XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   char *                                 XVT_CALLCONV1
         xvt_tx_get_line                        XVT_CALLCONV2
         (
            TXEDIT     tx,
            T_PNUM     pnum,
            ACCESS_CMD cmd,
            T_LNUM     lnum,
            unsigned * lenp
         );


extern   TXEDIT                                 XVT_CALLCONV1
         xvt_tx_get_next_tx                     XVT_CALLCONV2
         (
            TXEDIT   tx,
            WINDOW   win
         );


extern   int                                    XVT_CALLCONV1
         xvt_tx_get_margin                      XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   T_CNUM                                 XVT_CALLCONV1
         xvt_tx_get_num_chars                   XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum,
            T_LNUM lnum
         );


extern   T_LNUM                                 XVT_CALLCONV1
         xvt_tx_get_num_lines                   XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   T_LNUM                                 XVT_CALLCONV1
         xvt_tx_get_num_par_lines               XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum
         );


extern   T_PNUM                                 XVT_CALLCONV1
         xvt_tx_get_num_pars                    XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_get_origin                      XVT_CALLCONV2
         (
            TXEDIT   tx,
            T_PNUM * pnump,
            T_LNUM * lnump,
            T_LNUM * org_linep,
            T_CPOS * org_offsetp
         );


extern   RCT *                                  XVT_CALLCONV1
         xvt_tx_get_rect                        XVT_CALLCONV2
         (
            TXEDIT tx,
            RCT *  rct
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_get_sel                         XVT_CALLCONV2
         (
            TXEDIT   tx,
            T_PNUM * p1,
            T_LNUM * l1,
            T_CNUM * c1,
            T_PNUM * p2,
            T_LNUM * l2,
            T_CNUM * c2
         );


extern   T_CNUM                                 XVT_CALLCONV1
         xvt_tx_get_tabstop                     XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   RCT *                                  XVT_CALLCONV1
         xvt_tx_get_view                        XVT_CALLCONV2
         (
            TXEDIT tx,
            RCT *  rct
         );


extern   WINDOW                                 XVT_CALLCONV1
         xvt_tx_get_win                         XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_is_scroll_update                XVT_CALLCONV2
         (
            void
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_move                            XVT_CALLCONV2
         (
            TXEDIT tx,
            RCT *  lrctp
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_process_event                   XVT_CALLCONV2
         (
            WINDOW  win,
            EVENT * ep
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_rem_par                         XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_reset                           XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_resume                          XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_scroll_hor                      XVT_CALLCONV2
         (
            TXEDIT tx,
            int    pixel_amt
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_scroll_vert                     XVT_CALLCONV2
         (
            TXEDIT tx,
            int    line_amt
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_active                      XVT_CALLCONV2
         (
            TXEDIT tx
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_attr                        XVT_CALLCONV2
         (
            TXEDIT   tx,
            unsigned attr
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_colors                      XVT_CALLCONV2
         (
            TXEDIT tx,
            COLOR  text,
            COLOR  border,
            COLOR  back
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_data                        XVT_CALLCONV2
         (
            TXEDIT tx,
            long   app_data
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_limit                       XVT_CALLCONV2
         (
            TXEDIT tx,
            int    limit
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_margin                      XVT_CALLCONV2
         (
            TXEDIT tx,
            int    margin
         );


extern   BOOLEAN                                XVT_CALLCONV1
         xvt_tx_set_par                         XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM pnum,
            char * s
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_scroll_callback             XVT_CALLCONV2
         (
            TXEDIT          tx,
            SCROLL_CALLBACK scroll_callback
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_sel                         XVT_CALLCONV2
         (
            TXEDIT tx,
            T_PNUM p1,
            T_LNUM l1,
            T_CNUM c1,
            T_PNUM p2,
            T_LNUM l2,
            T_CNUM c2
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_set_tabstop                     XVT_CALLCONV2
         (
            TXEDIT   tx,
            T_CNUM   tabstop
         );


extern   void                                   XVT_CALLCONV1
         xvt_tx_suspend                         XVT_CALLCONV2
         (
            TXEDIT tx
         );


#endif /* XVT_INCL_XVTTX */
