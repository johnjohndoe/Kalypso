/*  TABLE.H header file for table custom control for Design 2.x and Design++

   This code was written by Marc Rochkind and Christopher Williamson,
   May be distributed in object form only when embedded in a
   registered XVT user's application.
   Copyright 1993-1994, XVT Software Inc., All Rights Reserved.

   $Revision: 1.4 $ $Author: sheldon $ $Date: 1996/02/12 16:39:25 $
 */
#ifndef TABLE_INCL
#define TABLE_INCL

/*
   class "table"
 */

#include "xvtcm.h"

#if defined(_cplusplus) || defined(__cplusplus)
extern "C"
{
#endif

#define MAX_WIDTHS 31

/* forward ref properties since we have a circular reference */
  typedef struct s_TABLE_PROPERTIES TABLE_PROPERTIES;

#ifdef DSA_TABLE

  /* This version of DATA_REQ_FCN is for DSA only. */
  typedef XVT_CALLCONV_TYPEDEF(char *, DATA_REQ_FCN,
              (WINDOW win, TABLE_PROPERTIES * props, long lnum));

#else

  /* This version of DATA_REQ_FCN is for DSC only */
  typedef XVT_CALLCONV_TYPEDEF(char *, DATA_REQ_FCN,
              (WINDOW win, TABLE_PROPERTIES * props, long lnum, char *sel));
#endif

  typedef XVT_CALLCONV_TYPEDEF(char, SEL_REQ_FCN,
                         (WINDOW win, TABLE_PROPERTIES * props, long lnum));
  typedef XVT_CALLCONV_TYPEDEF(BOOLEAN, DRAW_FCN,
                           (WINDOW win, TABLE_PROPERTIES * props, long lnum,
                            int x, int y, int line_height, char *s));

#ifdef XVT_R3_FONT              /* this enables the old R3 font migration layer funcs */
  extern void XVT_CALLCONV1 table_get_props XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, TABLE_PROPERTIES * props));
  extern void XVT_CALLCONV1 table_set_props XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, TABLE_PROPERTIES * props));
#else
  typedef long FONT;
#define table_get_props table_get_properties    /* in case they get confused... */
#define table_set_props table_set_properties
#endif

  typedef struct s_TABLE_PROPERTIES
    {
      long flags;
      long app_data;
      int buf_lines;
      int line_height;
      int width[MAX_WIDTHS];
      DATA_REQ_FCN data_req;
      SEL_REQ_FCN sel_req;
      DRAW_FCN draw_fcn;
      COLOR rule_color;
      COLOR hdg_color;
      COLOR text_color;
      COLOR back_color;
      int top_margin;
      int left_margin;
      int col_gap;
      int horz_step;
      char *hdg;
      RCT anchor_gap;
      int ncolors;
      COLOR *colors;
      int nfonts;
      XVT_FNTID *fontids;
      XVT_FNTID hdg_fontid;
      XVT_FNTID fontid;
      BOOLEAN r3fonts;
      FONT font;                /* safest at the bottom of the struct */
      FONT hdg_font;
    }
  _TABLE_PROPERTIES;

/* Marc's TF codes */
#define TF_NONE                 0x00000000L
#define TF_BP_VIRTUAL           0x00000001L
#define TF_BP_REAL              0x00000002L
#define TF_BP_SCROLL            0x00000004L
#define TF_VERT_RULES           0x00000008L
#define TF_HORZ_RULES           0x00000010L
#define TF_HDG_RULE             0x00000020L
#define TF_RULES                0x00000038L     /* VERT, HORZ, and HDG */
#define TF_ALLOW_SELECT         0x00000040L
#define TF_MULT_SELECT          0x000000C0L     /* includes TF_ALLOW_SELECT */
#define TF_TABBED               0x00000100L
#define TF_INFINITE_HORZ        0x00000200L
#define TF_LEFT_ANCHOR          0x00000400L
#define TF_TOP_ANCHOR           0x00000800L
#define TF_RIGHT_ANCHOR         0x00001000L
#define TF_BOTTOM_ANCHOR        0x00002000L
#define TF_ANCHORED             0x00003C00L     /* anchored on all 4 sides */
#define TF_FIXED_HEIGHT         0x00004000L
#define TF_CODES_OFF            0x00008000L

/* Symbols for notices */
#define N_TABLE_SEL         101
#define N_TABLE_UNSELALL    102
#define N_TABLE_DBL_CLICK   103
#define N_TABLE_FOCUS_IN    104
#define N_TABLE_FOCUS_OUT   105
#define N_TABLE_MOUSE_UP    106

  typedef struct
    {
      XVTCM_CONTROL_INFO ci;    /* required as first member */
      long lnum;                /* line number */
      char state;               /* new selection state (prev and cur) */
    }
  TABLE_DATA;

/*
   Function to create the control, called automatically by XVT-Design. May
   also be called explicitly.
 */
  extern WINDOW XVT_CALLCONV1 table_create XVT_CALLCONV2
    XVT_CC_ARGS((int cid, int left, int top, int right, int bottom,
                 int prop_count, char **prop_list, WINDOW parent_win,
                 int parent_rid, long parent_flags, char *parent_class));
  extern void XVT_CALLCONV1 table_clear XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win));
  extern void XVT_CALLCONV1 table_printf XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, char *fmt,...));
  extern void XVT_CALLCONV1 table_display XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long nlines, DATA_REQ_FCN data_req,
                 SEL_REQ_FCN sel_req));
  extern void XVT_CALLCONV1 table_headings XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, char *hdg));
  extern void XVT_CALLCONV1 table_widths XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, int nwidths, int *width, BOOLEAN pixels));
  extern void XVT_CALLCONV1 table_get_properties XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, TABLE_PROPERTIES * props));
  extern void XVT_CALLCONV1 table_set_properties XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, TABLE_PROPERTIES * props));
  extern void XVT_CALLCONV1 table_suspend XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win));
  extern void XVT_CALLCONV1 table_resume XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win));
  extern WINDOW XVT_CALLCONV1 table_create_window XVT_CALLCONV2
    XVT_CC_ARGS((RCT * rct_p, char *title, int menu_rid));
  extern void XVT_CALLCONV1 table_copy XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win));
  extern char *XVT_CALLCONV1 table_first_sel XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long *lnum));
  extern char *XVT_CALLCONV1 table_next_sel XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long *lnum));
  extern char *XVT_CALLCONV1 table_get_line XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long lnum));
  extern void XVT_CALLCONV1 table_jump XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long lnum, BOOLEAN center));
  extern void XVT_CALLCONV1 table_set_sel XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win, long lnum, BOOLEAN sel));
  extern void XVT_CALLCONV1 table_resize XVT_CALLCONV2
    XVT_CC_ARGS((WINDOW win));

#if defined(_cplusplus) || defined(__cplusplus)
}                               /* extern "C" */
#endif

#endif                          /* TABLE_INCL */
