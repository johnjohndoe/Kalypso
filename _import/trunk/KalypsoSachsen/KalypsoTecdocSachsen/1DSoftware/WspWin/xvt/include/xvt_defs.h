/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *	$RCSfile: xvt_defs.h,v $ 
 *  $Revision: 1.20 $
 *
 *  Purpose: Global XVT macro definitions.
 *
 ****************************************************************************/

#ifndef XVT_INCL_DEFS
#define XVT_INCL_DEFS

/*---------------------------------------------------------------------------
	Machine-related constants noramlly found in limits.h
---------------------------------------------------------------------------*/
#ifndef UINT_MAX
#define UINT_MAX  (unsigned)(~0)
#endif
/* I18N OK - Must leave in because already in the API */
#ifndef UCHAR_MAX
#define UCHAR_MAX (unsigned char)(~0)
#endif
#ifndef CHAR_MAX
#define CHAR_MAX  ((char)(UCHAR_MAX >> 1))
#endif
#ifndef USHRT_MAX
#define USHRT_MAX (unsigned short)(~0)
#endif
#ifndef ULONG_MAX
#define ULONG_MAX (unsigned long)(~0L)
#endif
#ifndef INT_MAX
#define INT_MAX   ((int)(UINT_MAX >> 1))
#endif
#ifndef SHRT_MAX
#define SHRT_MAX  ((short)(USHRT_MAX >> 1))
#endif
#ifndef LONG_MAX
#define LONG_MAX  ((long)(ULONG_MAX >> 1))
#endif

/*---------------------------------------------------------------------------
	Resource ID constants
---------------------------------------------------------------------------*/
/* DECLINING usage: Do not rely on MENU_BAR_RID, as support for it may
 * be discontinued in a future release */
#define MENU_BAR_RID  9001    /* ID for default menubar resource */
#define ICON_RSRC     9012
#define DB_ABOUT      9050
#define DB_ASK        9051
#define DB_ERROR      9052
#define DB_NOTE       9053
#define DB_OPEN       9054
#define DB_ABORT      9055
#define DB_SAVE       9056
#define DB_HELPTOPICS 9057
#define DB_HELPTEXT   9058
#define DB_RESPONSE   9059
#define DB_WARNING    9060
#define DB_FONTSEL    9061
/* Number 9062 reserved for XVT/Mac FontSize dialog */
#define STR_HELPTYPE  40000 /* string resource for help file-type */

/* Define the beginning of the common code and K layer string resources */
/* The maximum reserved string res ID is 32767 */
#define XVT_STRING_RES_BASE 30000
#define XVTV_STRING_RES_BASE XVT_STRING_RES_BASE + 1300

/*---------------------------------------------------------------------------
	Standard dialog pushbutton control IDs
---------------------------------------------------------------------------*/
#define DLG_OK       1          /* default button was clicked */
#define DLG_YES      DLG_OK     /* synonym */
#define DLG_CANCEL   2          /* cancel button was clicked */
#define DLG_OUTLINE  3          /* ID of userItem on Mac (internal use) */
#define DLG_NO       4          /* other button was clicked */

/*---------------------------------------------------------------------------
	Colors
---------------------------------------------------------------------------*/
#ifndef COLOR_RED
#define COLOR_RED       0x01FF0000L
#endif
#ifndef COLOR_GREEN
#define COLOR_GREEN     0x0200FF00L
#endif
#ifndef COLOR_BLUE
#define COLOR_BLUE      0x030000FFL
#endif
#ifndef COLOR_CYAN
#define COLOR_CYAN      0x0400FFFFL
#endif
#ifndef COLOR_MAGENTA
#define COLOR_MAGENTA   0x05FF00FFL
#endif
#ifndef COLOR_YELLOW
#define COLOR_YELLOW    0x06FFFF00L
#endif
#ifndef COLOR_BLACK
#define COLOR_BLACK     0x07000000L
#endif
#ifndef COLOR_DKGRAY
#define COLOR_DKGRAY    0x08404040L
#endif
#ifndef COLOR_GRAY
#define COLOR_GRAY      0x09808080L
#endif
#ifndef COLOR_LTGRAY
#define COLOR_LTGRAY    0x0AC0C0C0L
#endif
#ifndef COLOR_WHITE
#define COLOR_WHITE     0x0BFFFFFFL
#endif
 
#define COLOR_INVALID   ((COLOR)~0)
 
/*---------------------------------------------------------------------------
	String and Character Constants
---------------------------------------------------------------------------*/
#define XVT_MAX_MB_SIZE  XVTK_MAX_MB_SIZE


/*---------------------------------------------------------------------------
	Key codes
---------------------------------------------------------------------------*/
#define K_DEL    127 /* delete (same as ASCII) */
#define K_UP     301 /* up arrow */
#define K_DOWN   302 /* down arrow */
#define K_RIGHT  303 /* right arrow */
#define K_LEFT   304 /* left arrow */
#define K_PREV   305 /* previous screen */
#define K_NEXT   306 /* next screen */
#define K_LHOME  307 /* line home */
#define K_LEND   308 /* line end */
#define K_HOME   309 /* home */
#define K_END    310 /* end */
#define K_INS    312 /* insert */
#define K_WLEFT  313 /* word left */
#define K_WRIGHT 314 /* word right */
#define K_BTAB   315 /* back tab */
#define K_HELP   316 /* help */
#define K_CLEAR  317 /* clear */
#define K_KP0    318 /* keypad '0' */
#define K_KP1    319
#define K_KP2    320
#define K_KP3    321
#define K_KP4    322
#define K_KP5    323
#define K_KP6    324
#define K_KP7    325
#define K_KP8    326
#define K_KP9    327 /* keypad '9' */
#define K_COPY   328 /* copy */
#define K_CUT    329 /* cut */
#define K_PASTE  330 /* paste */
#define K_F1     331 /* function key 1 */
#define K_F2     332
#define K_F3     333
#define K_F4     334
#define K_F5     335
#define K_F6     336
#define K_F7     337
#define K_F8     338
#define K_F9     339
#define K_F10    340
#define K_F11    341
#define K_F12    342
#define K_F13    343
#define K_F14    344
#define K_F15    345 /* function key 15 */
#define K_F16    346
#define K_F17    347
#define K_F18    348
#define K_F19    349
#define K_F20    350
#define K_F21    351
#define K_F22    352
#define K_F23    353
#define K_F24    354

#define K_KPMULT 372 /* keypad '*' */
#define K_KPSUB  373 /* keypad '-' */
#define K_KPADD  374 /* keypad '+' */
#define K_KPDIV  375 /* keypad '/' */
#define K_KPDOT  376 /* keypad '.' */
#define K_KPEQ   377 /* keypad '=' */

/*---------------------------------------------------------------------------
	Text edit module
---------------------------------------------------------------------------*/
#define TX_READONLY     0x0001 /* text is not editable */
#define TX_WRAP         0x0002 /* wrap text to margin */
#define TX_AUTOVSCROLL  0x0004 /* autoscroll vertically */
#define TX_AUTOHSCROLL  0x0008 /* autoscroll horizontally */
#define TX_BORDER       0x0010 /* rectangular border */
#define TX_VSCROLLBAR   0x0020 /* vertical scroll bar */
#define TX_HSCROLLBAR   0x0040 /* horizontal scroll bar */
#define TX_ONEPAR       0x0080 /* one paragraph only (no \r) */
#define TX_NOCOPY       0x0100 /* no copy allowed */
#define TX_NOCUT        0x0200 /* no cut allowed */
#define TX_NOPASTE      0x0400 /* no paste allowed */
#define TX_NOMENU       0x0800 /* no edit menu changes */
#define TX_ENABLECLEAR  0x1000 /* leave CLEAR enabled always */
#define TX_OVERTYPE     0x2000 /* overtype mode */
#define TX_DISABLED     0x4000
#define TX_INVISIBLE    0x8000

/*---------------------------------------------------------------------------
	Cursors
---------------------------------------------------------------------------*/
#define CURSOR_ARROW    0       /* arrow */
#define CURSOR_IBEAM    1       /* I-beam */
#define CURSOR_CROSS    2       /* cross hair */
#define CURSOR_PLUS     3       /* plus sign (fatter than cross hair) */
#define CURSOR_WAIT     4       /* waiting symbol (e.g., hourglass) */
#define CURSOR_HELP     5       /* help system */
#define CURSOR_USER     11      /* user defined shape (>= 11) */

/*---------------------------------------------------------------------------
	Event masks
---------------------------------------------------------------------------*/

#define EM_NONE       ((EVENT_MASK)0L)
#define EM_ALL        ((EVENT_MASK)~0L)
#define EM_CREATE     ((EVENT_MASK)(1L << E_CREATE))
#define EM_DESTROY    ((EVENT_MASK)(1L << E_DESTROY))
#define EM_FOCUS      ((EVENT_MASK)(1L << E_FOCUS))
#define EM_SIZE       ((EVENT_MASK)(1L << E_SIZE))
#define EM_UPDATE     ((EVENT_MASK)(1L << E_UPDATE))
#define EM_CLOSE      ((EVENT_MASK)(1L << E_CLOSE))
#define EM_MOUSE_DOWN ((EVENT_MASK)(1L << E_MOUSE_DOWN))
#define EM_MOUSE_UP   ((EVENT_MASK)(1L << E_MOUSE_UP))
#define EM_MOUSE_MOVE ((EVENT_MASK)(1L << E_MOUSE_MOVE))
#define EM_MOUSE_DBL  ((EVENT_MASK)(1L << E_MOUSE_DBL))
#define EM_CHAR       ((EVENT_MASK)(1L << E_CHAR))
#define EM_VSCROLL    ((EVENT_MASK)(1L << E_VSCROLL))
#define EM_HSCROLL    ((EVENT_MASK)(1L << E_HSCROLL))
#define EM_COMMAND    ((EVENT_MASK)(1L << E_COMMAND))
#define EM_FONT       ((EVENT_MASK)(1L << E_FONT))
#define EM_CONTROL    ((EVENT_MASK)(1L << E_CONTROL))
#define EM_TIMER      ((EVENT_MASK)(1L << E_TIMER))
#define EM_QUIT       ((EVENT_MASK)(1L << E_QUIT))
#define EM_HELP       ((EVENT_MASK)(1L << E_HELP))
#define EM_USER       ((EVENT_MASK)(1L << E_USER))
#define EM_CXO        ((EVENT_MASK)(1L << E_CXO))

/*---------------------------------------------------------------------------
	XVT escape code value ranges
---------------------------------------------------------------------------*/
#define XVT_ESC_COMMON_BASE   8000
#define XVT_ESC_INTERNAL_BASE 30000

/*---------------------------------------------------------------------------
	Control, window, and dialog creation flags
---------------------------------------------------------------------------*/
#define CTL_FLAG_DISABLED      0x00000001L
#define CTL_FLAG_CHECKED       0x00000004L
#define CTL_FLAG_DEFAULT       0x00000008L
#define CTL_FLAG_INVISIBLE     0x00000010L
#define CTL_FLAG_GROUP         0x00000020L
#define CTL_FLAG_MAC_MULTILINE 0x00000080L /* opt3 */
#define CTL_FLAG_MAC_WORDWRAP  0x00000100L /* opt4 */
#define CTL_FLAG_READONLY      0x00000200L
#define CTL_FLAG_MULTIPLE      0x00000400L
#define CTL_FLAG_MAC_GENEVA9   0x00000800L /* was opt1 */
#define CTL_FLAG_PM_SYSICON    0x00000800L
#define CTL_FLAG_MAC_MONACO9   0x00001000L /* was opt2 */
#define CTL_FLAG_NATIVE_JUST   0x0L        /* default */
#define CTL_FLAG_LEFT_JUST     0x00002000L /* left text */
#define CTL_FLAG_CENTER_JUST   0x00004000L /* centered text */
#define CTL_FLAG_RIGHT_JUST    0x00008000L /* right justified text */
#define CTL_FLAG_PASSWORD      0x00010000L

#define WSF_NONE        0x00000000L
#define WSF_SIZE        0x00000001L /* is user sizeable */
#define WSF_CLOSE       0x00000002L /* is user closeable */
#define WSF_HSCROLL     0x00000004L /* has horz. scrolbar outside client area */
#define WSF_VSCROLL     0x00000008L /* has vert. scrolbar outside client area */
#define WSF_DECORATED   0x0000000FL /* all of above four flags */
#define WSF_INVISIBLE   0x00000010L /* is initially invisible */
#define WSF_DISABLED    0x00000020L /* is initially disabled */
#define WSF_FLOATING    0x00000040L /* is floating */
#define WSF_ICONIZABLE  0x00000080L
#define WSF_ICONIZED    0x00000100L /* is initially iconized */
#define WSF_SIZEONLY    0x00000200L /* lacks border rectangles (Mac only) */
#define WSF_NO_MENUBAR  0x00000800L /* has no menu bar of its own */
#define WSF_MAXIMIZED	0x00001000L /* initially maximized */
#define WSF_PLACE_EXACT 0x00002000L /* do not auto-place */
#define WSF_DEFER_MODAL 0x00008000L /* defer modal state for W_MODAL windows */

#define DLG_FLAG_DISABLED  0x00000001L
#define DLG_FLAG_INVISIBLE 0x00000002L

/*---------------------------------------------------------------------------
	Standard tool constants
---------------------------------------------------------------------------*/
#define TL_PEN_BLACK  1L
#define TL_PEN_HOLLOW 2L
#define TL_PEN_RUBBER 3L
#define TL_PEN_WHITE  4L

#define TL_BRUSH_BLACK 0L
#define TL_BRUSH_WHITE 1L

/*---------------------------------------------------------------------------
	Font support 
---------------------------------------------------------------------------*/
/* Font style */
#define XVT_FS_NONE         0L
#define XVT_FS_BOLD         (1L<<0)
#define XVT_FS_ITALIC       (1L<<1)
#define XVT_FS_UNDERLINE    (1L<<4)
#define XVT_FS_OUTLINE      (1L<<5)
#define XVT_FS_SHADOW       (1L<<6)
#define XVT_FS_INVERSE      (1L<<7)
#define XVT_FS_BLINK        (1L<<8)
#define XVT_FS_STRIKEOUT    (1L<<9)
#define XVT_FS_USER1        (1L<<15)
#define XVT_FS_USER2        (1L<<16)
#define XVT_FS_USER3        (1L<<17)
#define XVT_FS_USER4        (1L<<18)
#define XVT_FS_USER5        (1L<<19)
#define XVT_FS_WILDCARD     (1L<<25)
 
/* Font attribute type */
#define XVT_FA_FAMILY   (XVT_FONT_ATTR_MASK)(1L<<0)
#define XVT_FA_SIZE     (XVT_FONT_ATTR_MASK)(1L<<1)
#define XVT_FA_STYLE    (XVT_FONT_ATTR_MASK)(1L<<2)
#define XVT_FA_NATIVE   (XVT_FONT_ATTR_MASK)(1L<<3)
#define XVT_FA_APP_DATA (XVT_FONT_ATTR_MASK)(1L<<4)
#define XVT_FA_WIN      (XVT_FONT_ATTR_MASK)(1L<<5)
#define XVT_FA_ALL      (XVT_FONT_ATTR_MASK)~(0L)
 
/* Guaranteed support for these font families */
#define XVT_FFN_TIMES       "times"
#define XVT_FFN_HELVETICA   "helvetica"
#define XVT_FFN_COURIER     "Courier"
#define XVT_FFN_FIXED       "fixed"
#define XVT_FFN_SYSTEM      "system"

/* Convenience macro for identifying a NULL font id */
#define NULL_FNTID ((XVT_FNTID)NULL)


/*---------------------------------------------------------------------------
	COLOR macros
---------------------------------------------------------------------------*/

/* allocated nbr of entries in image->v.cl8.clut */

#define XVT_CLUT_SIZE 256
#define XVT_PALETTE_SIZE  256

/* macros for COLOR values */
#define XVT_MAKE_COLOR(r,g,b) ((COLOR)((((ULONG)(r)&0xFF) << 16) | \
								       (((ULONG)(g)&0xFF) << 8)  | \
								       (((ULONG)(b)&0xFF))))
#define XVT_COLOR_GET_RED(color)   ((unsigned char)(((color) >> 16) & 0xFF))
#define XVT_COLOR_GET_GREEN(color) ((unsigned char)(((color) >> 8) & 0xFF))
#define XVT_COLOR_GET_BLUE(color)  ((unsigned char)((color) & 0xFF))

/* old MAKE_COLOR macro */
#define MAKE_COLOR(r,g,b) XVT_MAKE_COLOR(r,g,b)

/* Do not use the following, obsoleted macros */
#define XVT_RED_OF_COLOR(color)   ((unsigned char)(((color) >> 16) & 0xFF))
#define XVT_GREEN_OF_COLOR(color) ((unsigned char)(((color) >> 8) & 0xFF))
#define XVT_BLUE_OF_COLOR(color)  ((unsigned char)((color) & 0xFF))

/*---------------------------------------------------------------------------
	File attributes
---------------------------------------------------------------------------*/
/* definitions for attributes used in the xvt_fsys_*_file_attr calls */

#define XVT_FILE_ATTR_MINIMUM           1L

#define XVT_FILE_ATTR_EXIST             1L
#define XVT_FILE_ATTR_READ              2L
#define XVT_FILE_ATTR_WRITE             3L
#define XVT_FILE_ATTR_EXECUTE           4L
#define XVT_FILE_ATTR_DIRECTORY         5L
#define XVT_FILE_ATTR_NUMLINKS          6L
#define XVT_FILE_ATTR_SIZE              7L
#define XVT_FILE_ATTR_ATIME             8L
#define XVT_FILE_ATTR_MTIME             9L
#define XVT_FILE_ATTR_CTIME             10L
#define XVT_FILE_ATTR_CREATORSTR        11L
#define XVT_FILE_ATTR_DIRSTR            12L
#define XVT_FILE_ATTR_FILESTR           13L
#define XVT_FILE_ATTR_TYPESTR           14L

#define XVT_FILE_ATTR_MAXIMUM           14L

/*---------------------------------------------------------------------------
	Miscellaneous
---------------------------------------------------------------------------*/
#define XVT_TIMER_ERROR (-1L)
#define XVT_MAX_WINDOW_RECT ((RCT *)NULL)
#define DIR_TYPE "/\1\2\3"      /* used with list_files */
#ifndef NULL
#define NULL 0L
#endif
#define NULL_WIN     ((WINDOW)NULL)
#define NULL_PICTURE ((PICTURE)NULL)
#define NULL_PIXMAP  ((XVT_PIXMAP)NULL)
#define NULL_PALETTE ((XVT_PALETTE)NULL)
#define NULL_IMAGE   ((XVT_IMAGE)NULL)
#define NULL_TID     ((XVT_HELP_TID)NULL)
#define NULL_TXEDIT  NULL_WIN
#define BAD_TXEDIT   NULL_TXEDIT

#define TASK_WIN   ((WINDOW)xvt_vobj_get_attr(NULL_WIN, ATTR_TASK_WINDOW))
#define SCREEN_WIN ((WINDOW)xvt_vobj_get_attr(NULL_WIN, ATTR_SCREEN_WINDOW))

#define PTR_LONG(p) ((long)(char *)(p))

#ifndef max
#define max(x, y) ((x) > (y) ? (x) : (y))
#endif
#ifndef min
#define min(x, y) ((x) < (y) ? (x) : (y))
#endif

#define XVT_MAKE_VERSION(major, minor, patch) ((major)*10000L + (minor)*100L + (patch))

/*---------------------------------------------------------------------------
	Attribute definitions for get/set_value()
	Note that non-portable constants are defined by the platform header.
---------------------------------------------------------------------------*/
#define ATTR_BASE 0
/* system config attributes */
#define ATTR_BACK_COLOR                 (ATTR_BASE + 100)
#define ATTR_HAVE_COLOR                 (ATTR_BASE + 101)
#define ATTR_HAVE_MOUSE                 (ATTR_BASE + 102)
#define ATTR_NUM_TIMERS                 (ATTR_BASE + 103)
#define ATTR_XVT_CONFIG                 (ATTR_BASE + 104)
#define ATTR_DISPLAY_TYPE               (ATTR_BASE + 105)

/* Object size attributes */
#define ATTR_CTL_BUTTON_HEIGHT          (ATTR_BASE + 200)
#define ATTR_CTL_CHECKBOX_HEIGHT        (ATTR_BASE + 201)
#define ATTR_CTL_EDIT_TEXT_HEIGHT       (ATTR_BASE + 202)
#define ATTR_CTL_HORZ_SBAR_HEIGHT       (ATTR_BASE + 203)
#define ATTR_CTL_VERT_SBAR_WIDTH        (ATTR_BASE + 204)
#define ATTR_CTL_RADIOBUTTON_HEIGHT     (ATTR_BASE + 205)
#define ATTR_CTL_STATIC_TEXT_HEIGHT     (ATTR_BASE + 206)
#define ATTR_ICON_WIDTH                 (ATTR_BASE + 207)
#define ATTR_ICON_HEIGHT                (ATTR_BASE + 208)

/* Predefined windows */
#define ATTR_SCREEN_WINDOW              (ATTR_BASE + 300)
#define ATTR_TASK_WINDOW                (ATTR_BASE + 301)

/* System metric attributes */
#define ATTR_SCREEN_HEIGHT              (ATTR_BASE + 400)
#define ATTR_SCREEN_WIDTH               (ATTR_BASE + 401)
#define ATTR_SCREEN_HRES                (ATTR_BASE + 402)
#define ATTR_SCREEN_VRES                (ATTR_BASE + 403)
#define ATTR_PRINTER_HEIGHT             (ATTR_BASE + 404)
#define ATTR_PRINTER_WIDTH              (ATTR_BASE + 405)
#define ATTR_PRINTER_HRES               (ATTR_BASE + 406)
#define ATTR_PRINTER_VRES               (ATTR_BASE + 407)
#define ATTR_DOC_STAGGER_HORZ           (ATTR_BASE + 408)
#define ATTR_DOC_STAGGER_VERT           (ATTR_BASE + 409)
                                        
/* Window metric attributes */          
#define ATTR_DOCFRAME_WIDTH             (ATTR_BASE + 500)
#define ATTR_DOCFRAME_HEIGHT            (ATTR_BASE + 501)
#define ATTR_FRAME_WIDTH                (ATTR_BASE + 502)
#define ATTR_FRAME_HEIGHT               (ATTR_BASE + 503)
#define ATTR_DBLFRAME_WIDTH             (ATTR_BASE + 504)
#define ATTR_DBLFRAME_HEIGHT            (ATTR_BASE + 505)
#define ATTR_MENU_HEIGHT                (ATTR_BASE + 506)
#define ATTR_TITLE_HEIGHT               (ATTR_BASE + 507)
 
/* Window attributes */
#define ATTR_NATIVE_GRAPHIC_CONTEXT     (ATTR_BASE + 601)
#define ATTR_NATIVE_WINDOW              (ATTR_BASE + 602)
#define ATTR_PROPAGATE_NAV_CHARS        (ATTR_BASE + 603)

/* Misc attributes */
#define ATTR_DEBUG_FILENAME             (ATTR_BASE + 700)
#define ATTR_MALLOC_ERR_HANDLER         (ATTR_BASE + 701)  /* DECLINING */
#define ATTR_KEY_HOOK                   (ATTR_BASE + 702)
#define ATTR_EVENT_HOOK                 (ATTR_BASE + 703)
#define ATTR_SUPPRESS_UPDATE_CHECK      (ATTR_BASE + 704)
#define ATTR_FATAL_ERR_HANDLER          (ATTR_BASE + 705)  /* DECLINING */
#define ATTR_ERRMSG_HANDLER             (ATTR_BASE + 706)
#define ATTR_MEMORY_MANAGER             (ATTR_BASE + 707)
#define ATTR_DEFAULT_PALETTE_TYPE       (ATTR_BASE + 708)
#define ATTR_ERRMSG_FILENAME            (ATTR_BASE + 709)
#define ATTR_HELP_HOOK                  (ATTR_BASE + 710)
#define ATTR_HELP_CONTEXT               (ATTR_BASE + 711)
#define ATTR_COLLATE_HOOK               (ATTR_BASE + 712)
#define ATTR_MULTIBYTE_AWARE            (ATTR_BASE + 713)
#define ATTR_RESOURCE_FILENAME          (ATTR_BASE + 714)
#define ATTR_APP_CTL_COLORS             (ATTR_BASE + 715)
#define ATTR_APPL_NAME_RID              (ATTR_BASE + 716)
#define ATTR_TASKWIN_TITLE_RID          (ATTR_BASE + 717)
#define ATTR_R40_TXEDIT_BEHAVIOR        (ATTR_BASE + 718)
#define ATTR_APP_CTL_FONT_RID           (ATTR_BASE + 719)

/* Font attributes */
#define ATTR_FONT_MAPPER                (ATTR_BASE + 800)
#define ATTR_FONT_DIALOG                (ATTR_BASE + 801)
#define ATTR_FONT_CACHE_SIZE            (ATTR_BASE + 802)

/*---------------------------------------------------------------------------
	Values for the "modifier" field of the E_CHAR event. 
---------------------------------------------------------------------------*/
#define XVT_MOD_KEY_NONE        0L
#define XVT_MOD_KEY_SHIFT       (1L<<1)
#define XVT_MOD_KEY_CTL         (1L<<2)
#define XVT_MOD_KEY_ALT         (1L<<3)
#define XVT_MOD_KEY_LSHIFT      (1L<<4)
#define XVT_MOD_KEY_RSHIFT      (1L<<5)
#define XVT_MOD_KEY_CMD         (1L<<6)
#define XVT_MOD_KEY_OPTION      (1L<<7)
#define XVT_MOD_KEY_COMPOSE     (1L<<8)
#define XVT_MOD_KEY_LALT        (1L<<9)
#define XVT_MOD_KEY_RALT        (1L<<10)
#define XVT_MOD_KEY_ALTGRAF     (1L<<11)


/*---------------------------------------------------------------------------
	Values for the type XVT_COLOR_TYPE. 
---------------------------------------------------------------------------*/
#define XVT_COLOR_NULL          (XVT_COLOR_TYPE)(0L)
#define XVT_COLOR_FOREGROUND    (XVT_COLOR_TYPE)(1L<<1)
#define XVT_COLOR_BACKGROUND    (XVT_COLOR_TYPE)(1L<<2)
#define XVT_COLOR_BLEND         (XVT_COLOR_TYPE)(1L<<3)
#define XVT_COLOR_HIGHLIGHT     (XVT_COLOR_TYPE)(1L<<4)
#define XVT_COLOR_BORDER        (XVT_COLOR_TYPE)(1L<<5)
#define XVT_COLOR_TROUGH        (XVT_COLOR_TYPE)(1L<<6)
#define XVT_COLOR_SELECT        (XVT_COLOR_TYPE)(1L<<7)


#endif /* XVT_INCL_DEFS */
