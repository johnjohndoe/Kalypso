/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvt_type.h,v $ 
 *  $Revision: 1.17 $
 *
 *  Purpose: XVT global type definitions.
 *
 ****************************************************************************/

/*===========================================================================
                             DISCALIMER
  This file contains definitions for data types and union members suffixed
  with _R3 or _R4. Such definitions are solely for XVT's internal use and
  will be removed without warning.  NO compatibility support will be
  provided for the removed definitions.

  This file also defines several "opaque" data types. Using such data types
  in any way other than as documented in the XVT manuals (such as obtaining
  knowledge of opaque data internals) is NOT supported and the actual data
  representation will change without any notice.
  ==========================================================================*/

#ifndef XVT_INCL_XVTTYPE
#define XVT_INCL_XVTTYPE

typedef long           WINDOW;         /* window descriptor */
typedef long           PICTURE;        /* encapsulated picture descriptor */
typedef long           XVT_CXO;
typedef unsigned long  COLOR;          /* color encapsulation */
typedef unsigned long  EVENT_MASK;     /* event delivery mask */
typedef unsigned long  GHANDLE;        /* handle to global memory block */
typedef short          MENU_TAG;       /* menu item tag */
typedef short          CURSOR;         /* cursor shape */
typedef WINDOW         TXEDIT;         /* Identifies a text-edit object */
typedef unsigned short T_CNUM;
typedef unsigned short T_CPOS;
typedef unsigned short T_LNUM;
typedef unsigned short T_PNUM;
typedef unsigned long  XVT_COLOR_TYPE; /* Color Component Type (XVT_COLOR_* */

/* The following legacy "data types" are being phased out, as they cause    */
/* problems with constness: "const int *x" is NOT same as "const INT_PTR x" */
//typedef int           *INT_PTR;
typedef BOOLEAN       *BOOLEAN_PTR;
typedef char           XVT_BYTE;    /* raw data */
typedef unsigned char  XVT_UBYTE;   /* raw data */
typedef XVT_BYTE      *DATA_PTR;    /* ptr to arbitrary data - backwards compat. */
typedef XVT_UBYTE     *UDATA_PTR;   /* unsigned ptr to arbitrary data */
typedef XVT_UBYTE      DATA_BYTE;   /* for raw data */
typedef long          *LONG_PTR;
typedef unsigned long *ULONG_PTR;

/* define a point to function which will be used in xvt_win_enum_wins etc. */
typedef XVT_CALLCONV_TYPEDEF( BOOLEAN, XVT_ENUM_CHILDREN, (WINDOW child, long data));

#ifndef PWIMPH
typedef unsigned int UINT;
typedef unsigned long ULONG;
#endif

/* Wide character string type */
typedef XVTK_WCHAR     XVT_WCHAR;

/* Old fatal dialog exit hook, being phased out  */
typedef XVT_CALLCONV_TYPEDEF (void, FATAL_ERR_FUNC, (void));

/* Popup menu alignment options */

typedef enum e_xvt_alignment {
     XVT_POPUP_CENTER,
     XVT_POPUP_LEFT_ALIGN,
     XVT_POPUP_RIGHT_ALIGN,
     XVT_POPUP_OVER_ITEM,
     XVT_ENUM_DUMMY19 = XVT_CC_ENUM_END
} XVT_POPUP_ALIGNMENT;

/* Error Messaging Facility Public Data Types */

typedef enum e_xvt_errsev {                     /* Error Severity */
        SEV_NONE = 0,                           /* undefined */
        SEV_WARNING,                            /* no functionality loss */
        SEV_ERROR,                              /* failure, but may continue */
        SEV_FATAL,                              /* must terminate execution */
        XVT_ENUM_DUMMY20 = XVT_CC_ENUM_END
        }                        XVT_ERRSEV;
 
typedef unsigned long            XVT_ERRID;     /* Error Message Identifier */
 
/* Error Message Object handle*/
typedef struct s_errmsg {long* unsupp;}  *XVT_ERRMSG;
 
                                                /* Error Message Handler */
typedef XVT_CALLCONV_TYPEDEF( BOOLEAN, XVT_ERRMSG_HANDLER, (
        XVT_ERRMSG   err_msg,                   /* Message object handle */
        DATA_PTR     context                    /* Context from push */
        )                                       );
 
typedef enum e_mode {           /* drawing (transfer) mode */
    M_COPY,                     /*   patCopy (Mac) */
    M_OR,                       /*   patOr         */
    M_XOR,                      /*   patXor        */
    M_CLEAR,                    /*   patBic        */
    M_NOT_COPY,                 /*   notPatCopy    */
    M_NOT_OR,                   /*   notPatOr      */
    M_NOT_XOR,                  /*   notPatXor     */
    M_NOT_CLEAR,                /*   notPatBic     */
    XVT_ENUM_DUMMY1 = XVT_CC_ENUM_END
} DRAW_MODE;

typedef enum e_eol {            /* terminator found by find_eol fcn */
    EOL_NORMAL,                 /* normal (or first) line terminator */
    EOL_DIFF,                   /* terminator different from previous */
    EOL_NONE,                   /* end of buf before any terminator */
    XVT_ENUM_DUMMY2 = XVT_CC_ENUM_END
} EOL_FORMAT, *EOL_FORMAT_PTR;

typedef enum e_access {
    A_LOCK, 
    A_GET, 
    A_UNLOCK, 
    XVT_ENUM_DUMMY3 = XVT_CC_ENUM_END 
} ACCESS_CMD;

typedef enum e_ask_resp {       /* response from ask fcn */
    RESP_DEFAULT,               /* default button */
    RESP_2,                     /* second button */
    RESP_3,                     /* third button */
    XVT_ENUM_DUMMY4 = XVT_CC_ENUM_END 
} ASK_RESPONSE;

typedef enum e_cb {             /* standard clipboard format */
    CB_TEXT,                    /* ASCII text */
    CB_PICT,                    /* encapsulated picture */
    CB_APPL,                    /* app's own type (must have name) */
    CB_PIXMAP,
    XVT_ENUM_DUMMY5 = XVT_CC_ENUM_END 
} CB_FORMAT;

typedef struct s_dir {         /* DIRECTORY - directory descriptor */
#ifdef XVT_DIRECTORY_FIELD
    XVT_DIRECTORY_FIELD;
#else
    char path[SZ_FNAME + 5];
#endif
} DIRECTORY, *DIRECTORY_PTR;

typedef enum e_file {           /* result from file open & save dialogs */
    FL_BAD,                     /* error occurred */
    FL_CANCEL,                  /* cancel button clicked */
    FL_OK,                      /* OK button clicked */
    XVT_ENUM_DUMMY6 = XVT_CC_ENUM_END 
} FL_STATUS;

typedef enum e_pat {
    PAT_NONE,
    PAT_HOLLOW,
    PAT_SOLID,
    PAT_HORZ,
    PAT_VERT,
    PAT_FDIAG,
    PAT_BDIAG,
    PAT_CROSS,
    PAT_DIAGCROSS,
    PAT_RUBBER,
    PAT_SPECIAL,
    XVT_ENUM_DUMMY8 = XVT_CC_ENUM_END 
} PAT_STYLE;

typedef enum e_pen_style {      /* pen style (must be P_SOLID) */
    P_SOLID,                    /* solid */
    P_DOT,                      /* dotted line */
    P_DASH,                     /* dashed line */
    XVT_ENUM_DUMMY9 = XVT_CC_ENUM_END 
} PEN_STYLE;

typedef struct s_pnt {          /* mathematical point */
    short v;                    /* vertical (y) coordinate */
    short h;                    /* horizontal (x) coordinate */
} PNT, *PNT_PTR;

typedef struct s_printrcd {     /* print record (holds setup) */
    char data;
} PRINT_RCD, *PRINT_RCD_PTR;    /* structure to make type unique */

typedef struct s_rct {          /* mathematical rectangle */
    short top;                  /* top coordinate */
    short left;                 /* left coordinate */
    short bottom;               /* bottom coordinate */
    short right;                /* right coordinate */
} RCT, *RCT_PTR;

typedef enum e_scroll_ctl {     /* site of scroll bar activity */
    SC_NONE,                    /* nowhere (event should be ignored) */
    SC_LINE_UP,                 /* one line up */
    SC_LINE_DOWN,               /* one line down */
    SC_PAGE_UP,                 /* previous page */
    SC_PAGE_DOWN,               /* next page */
    SC_THUMB,                   /* thumb repositioning */
    SC_THUMBTRACK,              /* dynamic thumb tracking */
    XVT_ENUM_DUMMY10 = XVT_CC_ENUM_END 
} SCROLL_CONTROL;

typedef enum e_scroll_type {    /* type of scroll bar */
    HSCROLL,                    /* horizontal */
    VSCROLL,                    /* vertical */
    HVSCROLL,                   /* either (used for dlg box ctls) */
    XVT_ENUM_DUMMY11 = XVT_CC_ENUM_END 
} SCROLL_TYPE;

typedef struct s_pat {        /* formatting pattern */
    long dummy;
} *XVT_PATTERN;

typedef struct s_codeset_map {        /* Loaded codeset map file */
      long dummy;
} *XVT_CODESET_MAP;

typedef struct s_slist {        /* string list */
    short dummy;
} *SLIST;

typedef struct s_slistelt {     /* element of string list */
    short dummy;
} *SLIST_ELT;

 
#if XVT_OS == XVT_OS_WIN16 || XVT_OS == XVT_OS_WIN32
#define XVT_FONT_FAMILY_TYPE \
struct s_fontfam {         \
    unsigned int fixed: 1; \
    unsigned int proof: 1; \
    unsigned int : 14;     \
    unsigned char chset;   \
    int family;            \
}
#else
#define XVT_FONT_FAMILY_TYPE short
#endif  /* XVT_OS == XVT_OS_WIN16/WIN32 */

/* These are defined for XVT applications running in R4 font mode */
typedef struct s_fntid {long dummy;} *XVT_FNTID;
typedef unsigned long XVT_FONT_ATTR_MASK;   /* font attribute mask */
typedef unsigned long XVT_FONT_STYLE_MASK;  /* font style mask */
 
/* Prototype for the app supplied font mapper */
typedef XVT_CALLCONV_TYPEDEF( BOOLEAN, XVT_FONT_MAPPER,
    (
    XVT_FNTID     font_id       /* font being mapped */
    )                   );

/* Prototype for the app supplied font selection dialog */
typedef XVT_CALLCONV_TYPEDEF( BOOLEAN , XVT_FONT_DIALOG,
    (
    WINDOW         win,         /* window to send E_FONT event to */
    XVT_FNTID      font_id,     /* default font id */
    PRINT_RCD     *precp,       /* print record or NULL */
    unsigned long  reserved     /* reserved */
    )                   );

typedef enum e_win_type {       /* type of window */
    W_NONE,                     /* marker for end of WIN_DEF array */
    W_DOC,                      /* document window */
    W_PLAIN,                    /* window with plain border */
    W_DBL,                      /* window with double border */
    W_PRINT,                    /* XVT internal use only */
    W_TASK,                     /* task window */        
    W_SCREEN,                   /* screen window */        
    W_NO_BORDER,                /* no border */
    W_PIXMAP,                   /* pixmap */
    W_MODAL,                    /* modal window */
    WD_MODAL,                   /* modal dialog */
    WD_MODELESS,                /* modeless dialog */
    WC_PUSHBUTTON,              /* button control */
    WC_RADIOBUTTON,             /* radio button control */
    WC_CHECKBOX,                /* check box control */
    WC_HSCROLL,                 /* horizontal scroll bar control */
    WC_VSCROLL,                 /* vertical scroll bar control */
    WC_EDIT,                    /* edit control */
    WC_TEXT,                    /* static text control */
    WC_LBOX,                    /* list box control */
    WC_LISTBUTTON,              /* button with list */
    WC_LISTEDIT,                /* edit field with list */
    WC_GROUPBOX,                /* group box */
    WC_TEXTEDIT,                /* text-edit object */
    WC_ICON,                    /* icon control */
    WC_NUM_WIN_TYPES,           /* number of WIN_TYPE's */
    XVT_ENUM_DUMMY12 = XVT_CC_ENUM_END 
} WIN_TYPE;

#define WO_TE WC_TEXTEDIT

typedef struct s_cbrush {
    PAT_STYLE pat;
    COLOR color;
} CBRUSH;

typedef struct s_mitem {
    MENU_TAG tag;               /* bad alignment, but this structure */
    char *text;                 /* may be statically intialized by app */
    short mkey;
#if (XVT_OS == XVT_OS_SCOUNIX) || ((XVT_CC == XVT_CC_BCPLUS32) && defined(__cplusplus))
    /* inert to C, but causes C++ to align to next allocation unit boundary */
    unsigned :0;
#endif
    unsigned enabled:1;
    unsigned checked:1;
    unsigned checkable:1;
    unsigned separator:1;
#if XVT_OS == XVT_OS_WIN16
    unsigned:12;
#elif XVT_CC != XVT_CC_GNU
    unsigned:0;
#endif
    struct s_mitem *child;

    /* non-portable special fields defined by platform-specific section */
    XVT_NP_MENU_FIELDS
} MENU_ITEM;


typedef struct s_cpen {
    short width;
    PAT_STYLE pat;
    PEN_STYLE style;
    COLOR color;
} CPEN;

typedef struct s_filespec {     /* file specification */
    DIRECTORY dir;              /* directory */
    char type[6];               /* file type or extension */
    char name[SZ_FNAME + 1];    /* file name (may be partial path) */
    char creator[6];            /* file creator */
} FILE_SPEC, *FILE_SPEC_PTR;


/* Structures pertaining to color in controls */

typedef enum e_xvt_color_action {
   XVT_COLOR_ACTION_SET,
   XVT_COLOR_ACTION_UNSET,
   XVT_ENUM_DUMMY17 = XVT_CC_ENUM_END
} XVT_COLOR_ACTION;
 
typedef struct s_xvt_color_component {
   XVT_COLOR_TYPE type;
   COLOR          color;
} XVT_COLOR_COMPONENT;


typedef struct s_ctlinfo {      /* info passed with E_CONTROL event*/
    WIN_TYPE type;              /* WC_* control type */
    WINDOW win;                 /* WINDOW id of the control being operated */
    union {
        struct s_pushbutton {
            int reserved;            /* Reserved...no usage yet.*/
        } pushbutton;
        struct s_radiobutton {
            int reserved;            /* Reserved...no usage yet.*/
        } radiobutton;
        struct s_checkbox {
            int reserved;            /* Reserved...no usage yet.*/
        } checkbox;
        struct s_scroll {            /* scroll bar action */
            SCROLL_CONTROL what;     /* site of activity */
            short pos;               /* thumb position */
        } scroll;
        struct s_edit {
            BOOLEAN focus_change;    /* is event a focus change? */
            BOOLEAN active;          /* if so: gaining focus? (vs. losing) */
        } edit;
        struct s_statictext {
            int reserved;            /* Reserved...no usage yet.*/
        } statictext;
        struct s_lbox {              /* list box action */
            BOOLEAN dbl_click;       /* double click (vs. single)? */
        } lbox;
        struct s_listbutton {
            int reserved;            /* Reserved...no usage yet.*/
        } listbutton;
        struct s_listedit {
            BOOLEAN focus_change;    /* Did the edit field part change focus? */
            BOOLEAN active;          /* If so, focus gained (vs lost)? */
        } listedit;
        struct s_groupbox {
            int reserved;            /* Reserved...no usage yet.*/
        } groupbox;
        struct s_textedit {
            BOOLEAN focus_change;
            BOOLEAN active;
        } textedit;
        struct s_icon {
            int reserved;            /* Reserved...no usage yet.*/
        } icon;
    } v;
} CONTROL_INFO, *CONTROL_INFO_PTR;

typedef struct s_drawct {       /* set of drawing tools */
    CPEN pen;                   /* color pen */
    CBRUSH brush;               /* color brush */
    DRAW_MODE mode;             /* drawing mode */
    COLOR fore_color;           /* foreground color */
    COLOR back_color;           /* background color */
    BOOLEAN opaque_text;        /* is text drawn opaquely? */
} DRAW_CTOOLS;

typedef enum _event_type {
    E_CREATE,               /* creation */
    E_DESTROY,              /* destruction */
    E_FOCUS,                /* window focus gain/loss */
    E_SIZE,                 /* resize */
    E_UPDATE,               /* update */
    E_CLOSE,                /* close window request */
    E_MOUSE_DOWN,           /* mouse down */
    E_MOUSE_UP,             /* mouse up */    
    E_MOUSE_MOVE,           /* mouse move */
    E_MOUSE_DBL,            /* mouse double click */
    E_CHAR,                 /* character typed */
    E_VSCROLL,              /* horz. window scrollbar activity */
    E_HSCROLL,              /* vert. window scrollbar activity */
    E_COMMAND,              /* menu command */
    E_FONT,                 /* font menu selection */
    E_CONTROL,              /* control activity */
    E_TIMER,                /* timer */
    E_QUIT,                 /* application shutdown request */
    E_HELP,                 /* help invoked */
    E_USER,                 /* user defined */
    E_CXO,                  /* cxo event */
    XVT_ENUM_DUMMY14 = XVT_CC_ENUM_END 
} EVENT_TYPE;

typedef long  XVT_HELP_TID;

typedef struct s_event {
    EVENT_TYPE type;
    union _v {
        struct s_mouse {          /* E_MOUSE_DOWN, E_MOUSE_UP, E_MOUSE_MOVE,
                                     E_MOUSE_DBL */
            PNT where;            /* location of event (window relative) */
            BOOLEAN shift;        /* shift key down? */
            BOOLEAN control;      /* control or option key down? */
            short button;         /* button number */
        } mouse;
        struct s_char {           /* E_CHAR */
            XVT_WCHAR ch;         /* wide character */
            BOOLEAN shift;        /* shift key down? */
            BOOLEAN control;      /* control or option key down? */
            BOOLEAN virtual_key;  /* ch contains virtual key or not? */
            unsigned long modifiers; /* bit field of key modifiers */
        } chr;
        BOOLEAN active;           /* E_FOCUS: activation? (vs. deactivation) */
        BOOLEAN query;            /* E_QUIT: query only? (app calls quit_OK) */
        struct s_scroll_info {    /* E_VSCROLL, E_HSCROLL */
            SCROLL_CONTROL what;  /* site of activity */
            short pos;            /* thumb position, if SC_THUMB */
        } scroll;
        struct s_cmd {            /* E_COMMAND */
            MENU_TAG tag;         /* menu item tag */
            BOOLEAN shift;        /* shift key? */
            BOOLEAN control;      /* control or option key? */
        } cmd;
        struct s_size {           /* E_SIZE */
            short height;         /* new height */
            short width;          /* new width */
        } size;
        struct s_efont {          /* E_FONT */
            XVT_FNTID font_id;    /* R4 font id of selected font */
       } font;
        struct s_ctl {            /* E_CONTROL */
            short id;             /* control's ID */
            CONTROL_INFO ci;      /* control info */
        } ctl;
        struct s_update {         /* E_UPDATE */
            RCT rct;              /* update rectangle */
        } update;
        struct s_timer {          /* E_TIMER */
            long id;              /* timer ID */
        } timer;
        struct s_user {           /* E_USER */
            long id;              /* application ID */
            void *ptr;            /* application pointer */
        } user;
        struct s_help {           /* E_HELP */
            WINDOW obj;           /* help for control, window, dialog */
            MENU_TAG tag;         /* help for menu item */
            XVT_HELP_TID  tid;    /* predefined help topic */
        } help;
        struct s_cxo {            /* E_CXO */
            long msg_id;          /* CXO message id - Unique to each CXO */
            void * ptr;           /* message data pointer */
        } cxo;

    } v;
} EVENT, *EVENT_PTR;

typedef XVT_CALLCONV_TYPEDEF( void, SCROLL_CALLBACK, 
         (TXEDIT tx, T_LNUM org_line, T_LNUM nlines, T_CPOS org_offset));

typedef XVT_CALLCONV_TYPEDEF( long, EVENT_HANDLER,
         (WINDOW win, EVENT *ep));

typedef XVT_CALLCONV_TYPEDEF( long, XVT_CXO_EVENT_HANDLER,
         (XVT_CXO cxo, EVENT *ep));

typedef XVT_CALLCONV_TYPEDEF( BOOLEAN, XVT_PRINT_FUNCTION,
         (long data));

typedef XVT_CALLCONV_TYPEDEF( long, XVT_COLLATE_FUNCTION,
         (const char *mbs1, const char *mbs2));

typedef XVT_CALLCONV_TYPEDEF( BOOLEAN, XVT_HELP_HOOK, 
         (void *, WINDOW, EVENT *) );

typedef XVT_CALLCONV_TYPEDEF( char *, XVT_FORMAT_HANDLER,
         ( WINDOW win, char * instr, int * start, int * end, void * data ) );

typedef enum e_unit_type { 
    U_PIXELS, 
    U_CHARS, 
    U_SEMICHARS,
    XVT_ENUM_DUMMY15 = XVT_CC_ENUM_END 
} UNIT_TYPE;

typedef struct s_xvt_config {
    short menu_bar_ID;      /* menu bar resource ID */
    short about_box_ID;     /* about box resource ID */
    char *base_appl_name;   /* application's "file" name */
    char *appl_name;        /* application's name */
    char *taskwin_title;    /* title for task window */
} XVT_CONFIG;

typedef struct s_win_def {
    WIN_TYPE  wtype;
    RCT       rct;
    char     *text;
    UNIT_TYPE units;
    XVT_COLOR_COMPONENT * ctlcolors;/* PTK 4.1 addition */
    union {
        struct s_win_def_win {
            short int menu_rid;
            MENU_ITEM *menu_p;
            long flags;
            XVT_FNTID ctl_font_id;  /* PTK 4.1 addition */
        } win;
        struct s_win_def_dlg {
            long flags;
            XVT_FNTID ctl_font_id;  /* PTK 4.1 addition */
        } dlg;
        struct s_win_def_ctl {
            short int ctrl_id;
            short int icon_id;
            long flags;
            XVT_FNTID font_id;      /* PTK 4.1 addition */
        } ctl;
        struct s_win_def_tx {
            unsigned short attrib;
            XVT_FNTID font_id;  
            short margin;
            short limit;
            short int tx_id;
        } tx;
    } v;
} WIN_DEF;

/* Image Types */
 
typedef enum e_display_type {
    XVT_DISPLAY_MONO,           /* monochromatic display */
    XVT_DISPLAY_GRAY_16,        /* 16-entry grayscale */
    XVT_DISPLAY_GRAY_256,       /* 256-entry grayscale */
    XVT_DISPLAY_COLOR_16,       /* 16-entry color */ 
    XVT_DISPLAY_COLOR_256,      /* 256-entry color */
    XVT_DISPLAY_DIRECT_COLOR,   /* full color capabilities */
    XVT_ENUM_DUMMY21 = XVT_CC_ENUM_END
} XVT_DISPLAY_TYPE;

typedef enum e_image_format {
    XVT_IMAGE_NONE,
    XVT_IMAGE_CL8,
    XVT_IMAGE_RGB,
    XVT_IMAGE_MONO,
    XVT_ENUM_DUMMY22 = XVT_CC_ENUM_END
} XVT_IMAGE_FORMAT;

typedef struct _xvt_image_attr { int dummy; } *XVT_IMAGE_ATTR;
 
typedef struct _xvt_image { int dummy; } *XVT_IMAGE;
 
/* Palette Types */
 
typedef enum e_palette_type {
    XVT_PALETTE_NONE,
    XVT_PALETTE_STOCK,
    XVT_PALETTE_CURRENT,
    XVT_PALETTE_CUBE16,
    XVT_PALETTE_CUBE256,
    XVT_PALETTE_USER,
    XVT_ENUM_DUMMY23 = XVT_CC_ENUM_END
} XVT_PALETTE_TYPE;
 
typedef struct _xvt_palette_attr { int dummy; } *XVT_PALETTE_ATTR;
 
typedef struct _xvt_palette { int dummy; } *XVT_PALETTE;
 
/* Pixmap Types */
 
typedef enum e_pixmap_format {
    XVT_PIXMAP_NONE,
    XVT_PIXMAP_DEFAULT,
    XVT_ENUM_DUMMY18 = XVT_CC_ENUM_END
} XVT_PIXMAP_FORMAT;

typedef struct _xvt_pixmap_attr { int dummy; } *XVT_PIXMAP_ATTR;

typedef WINDOW XVT_PIXMAP;

/* Structure to contain memory management functions for use with
 * ATTR_MEMORY_MANAGER attribute */ 
typedef struct s_mem {
    XVT_CALLCONV_TYPEDEF( DATA_PTR, malloc,  (size_t size));
    XVT_CALLCONV_TYPEDEF( VOID,     free,    (DATA_PTR data));
    XVT_CALLCONV_TYPEDEF( DATA_PTR, realloc, (DATA_PTR data, size_t size));
    XVT_CALLCONV_TYPEDEF( DATA_PTR, zalloc,  (size_t size));
} XVT_MEM;

/* Types for IOSTREAM interface. */
typedef DATA_PTR XVT_IOSTR_CONTEXT;

typedef struct s_xvt_iostream { int opaque; } *XVT_IOSTREAM;

typedef XVT_CALLCONV_TYPEDEF( short, XVT_IOSTR_RWFUNC,
            (XVT_IOSTREAM    iostr,
             unsigned short  nbytes,
             XVT_BYTE        *buf));

typedef XVT_CALLCONV_TYPEDEF( long, XVT_IOSTR_SZFUNC,
            (XVT_IOSTREAM    iostr));


/* Types for the navigation object */

typedef long XVT_NAV;

typedef enum e_nav_insertion
{
	XVT_NAV_POS_FIRST,
	XVT_NAV_POS_LAST,
	XVT_NAV_POS_BEFORE,
	XVT_NAV_POS_AFTER,
	XVT_ENUM_DUMMY24 = XVT_CC_ENUM_END
} XVT_NAV_INSERTION;


/* CXO specific information */

#define SZ_CLASS_NAME 256

/* Predefined CXO messages, we get the negative range */

#define XVT_CXO_CREATE_MSG  -1
#define XVT_CXO_DESTROY_MSG -2

/* Insertion enumerated type */

typedef enum e_cxo_insertion
{
    XVT_CXO_POS_FIRST,
    XVT_CXO_POS_LAST,
    XVT_ENUM_DUMMY25 = XVT_CC_ENUM_END
} XVT_CXO_INSERTION;

#endif /* XVT_INCL_TYPE */
