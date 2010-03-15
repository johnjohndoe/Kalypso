/*
    Copyright 1993-1996 XVT Software. All rights reserved. May be used only
    by licensed XVT programmers for the construction of XVT programs or for
    educational purposes. XVT is a trademark of XVT Software.
    
    XVT Software., Box 18750, Boulder CO 80308 USA
    (303) 443-4223  fax: (303) 443-0969


    XVTCM.H header file for custom control manager for Design 2.x and Design++

    $Revision: 1.3 $

    This file is the header for the Type 1 Control Manager, for use with
    XVT-Design 2.01 (or later) and XVT-Design++ 1.0 (or later). It works
    for both C and C++.

    See the document "Using and Implementing Type 1 Custom Controls."
*/

#ifndef XVTCM_INCL
#define XVTCM_INCL

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

/* some PTKs don't have these defined in xvt.h */
#ifndef NULL_PICTURE
#define NULL_PICTURE (PICTURE)0
#endif

/* older PTKs don't have this CALLCONV defined, if not, provide one */
#ifndef XVT_CALLCONV_TYPEDEF
#define XVT_CALLCONV_TYPEDEF (ret, func, args) \
  ret (*XVT_CALLCONV1 func XVT_CALLCONV2) XVT_CC_ARGS(args)
#endif

/*
    Structure used by control implementations to set the arguments to be used
    to create the window for the control.
*/
typedef struct s_WIN_CREATION_ARGS {
    WIN_TYPE wtype;                         /* window type */
    RCT rct;                                /* rectangle */
    WINDOW parent_win;                      /* parent window */
    long win_flags;                         /* flags */
    EVENT_MASK mask;                        /* event mask */
    EVENT_HANDLER eh;                       /* event handler */
    int cid;                                /* control ID */
    int prop_count;                         /* count of properties */
    char **prop_list;                       /* property list */
} WIN_CREATION_ARGS;

#define WC_CUSTOM               W_NO_BORDER

/*
    Following macros are for use by application when receiving notices.
    XVT-Design and XVT-Design++ generate code using these automatically--
    there is no need to code expressions using them manually.
*/
#define XVTCM_GET_INFO(ep)      (XVTCM_CONTROL_INFO *)(ep->v.user.ptr)
#define XVTCM_GET_INFOPP(d)     (XVTCM_CONTROL_INFO *)(d)
#define XVTCM_GET_ID(ep)        (short)ep->v.user.id

/* 
        User required to keep property names under this value, and they
        are assured that any property values they get back will also be
        less than this size.
*/
#define MAX_PROPERTY_LEN 256

/*
    Following needed for XVTCM_CONTROL_INFO.
*/
#define SZ_CLASS 10                     /* size of class name, incl. NUL */
typedef short XVTCM_NOTICE;             /* notice type */

/*
    Structure for notification data, pointer to which is returned by
    XVTCM_GET_INFO. Always the first member of the control class-specific
    structure. For example, the control "prog_ind" uses this structure:

        typedef struct s_SAMPLE_DATA {
            XVTCM_CONTROL_INFO ci;
            int btn_number;
        } SAMPLE_DATA;
*/

typedef struct s_XVTCM_CONTROL_INFO {
    WIN_TYPE type;                  /* WC_CUSTOM */
    WINDOW win;                     /* W_CONTROL (child) WINDOW */
    XVTCM_NOTICE notice;            /* notice type */
    char class_name[SZ_CLASS];      /* class name as used in XVT-Design */
} XVTCM_CONTROL_INFO;

/*
    Functions for use by C or C++ applications.
*/
extern WINDOW   XVT_CALLCONV1       xvtcm_get_ctl_window        XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW parent_win, int cid));
extern void     XVT_CALLCONV1       xvtcm_parent_event          XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW parent_win, EVENT *ep));

/* Function prototypes for the SET_CREATION_ARGS callback function and
   also for the custom control's create function */
typedef XVT_CALLCONV_TYPEDEF(void, XVT_CUSTOM_ARG_HANDLER, 
                            (WIN_CREATION_ARGS *a));
typedef XVT_CALLCONV_TYPEDEF(WINDOW, XVT_CUSTOM_CREATE_HANDLER,
                (int cid, int left, int top, int right, int bottom,
                int prop_count, char **prop_list, WINDOW parent_win,
                int parent_rid, long parent_flags, char *parent_class));

/*
    Functions for use by control implementations only.
*/
extern WINDOW   XVT_CALLCONV1       xvtcm_create                XVT_CALLCONV2
                XVT_CC_ARGS((int cid, int left, int top, int right, int bottom,
                int prop_count, char **prop_list, WINDOW parent_win,
                int parent_rid, long parent_flags, char *parent_class,
                EVENT_HANDLER eh, XVT_CUSTOM_ARG_HANDLER a));

extern BOOLEAN  XVT_CALLCONV1       xvtcm_eh_start              XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win, EVENT *ep));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_eh_end                XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win, EVENT *ep));
extern WINDOW   XVT_CALLCONV1       xvtcm_get_previous_focus    XVT_CALLCONV2
                XVT_CC_ARGS((void));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_notify                XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win, XVTCM_CONTROL_INFO *info));
extern void *   XVT_CALLCONV1       xvtcm_get_model             XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win, size_t size));
extern char **  XVT_CALLCONV1       xvtcm_get_properties        XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win, int *argc));

/* Additional functions added to support scaling */
extern PNT      XVT_CALLCONV1       xvtcm_get_src_char_size     XVT_CALLCONV2
                XVT_CC_ARGS((void));
extern void     XVT_CALLCONV1       xvtcm_set_src_char_size     XVT_CALLCONV2
                XVT_CC_ARGS((PNT size));
extern PNT      XVT_CALLCONV1       xvtcm_get_curr_char_size    XVT_CALLCONV2
                XVT_CC_ARGS((void));
extern int      XVT_CALLCONV1       xvtcm_hscale                XVT_CALLCONV2
                XVT_CC_ARGS((int h));
extern int      XVT_CALLCONV1       xvtcm_vscale                XVT_CALLCONV2
                XVT_CC_ARGS((int v));

/* Additional functions added to support property parsing */
extern char *   XVT_CALLCONV1       xvtcm_get_property          XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_property_exists       XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_long     XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property,
                             long *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_int      XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property,
                             int *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_short    XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property, 
                             short *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_char     XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property, 
                             char *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_pnt      XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property, 
                             PNT *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_float    XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property, 
                             float *result));
extern BOOLEAN  XVT_CALLCONV1       xvtcm_get_property_color    XVT_CALLCONV2
                XVT_CC_ARGS((int prop_cnt, char *prop[], char *property, 
                             COLOR *result));
extern char *   XVT_CALLCONV1       xvtcm_get_title             XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win));

/* additional functions added for C++ and parent (TASK_WIN) abstraction */
extern long     XVT_CALLCONV1       xvtcm_get_cid               XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW cwin));
extern WINDOW   XVT_CALLCONV1       xvtcm_get_container         XVT_CALLCONV2
                XVT_CC_ARGS((WINDOW win));

#if defined(_cplusplus) || defined(__cplusplus)
}
#endif

#endif /* XVTCM_INCL */
