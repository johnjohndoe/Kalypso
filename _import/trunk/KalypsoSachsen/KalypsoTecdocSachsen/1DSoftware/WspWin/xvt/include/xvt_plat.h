/****************************************************************************
 *
 *  Copyright 1987-1995 XVT Software Inc.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software Inc.
 *
 *  Purpose:  Fingers XVT/WIN and XVT/NT supported compilers, operating systems,
 *            window systems and file systems and performs other
 *            platform-specific initializations
 * 
 ****************************************************************************/

#ifndef XVT_INCL_XVTPLAT
#define XVT_INCL_XVTPLAT


/****************************************************************************
 *    MISCELLANEOUS ENVIRONMENT-SPECIFIC MACROS
 ****************************************************************************/


/* #if defined(BOOLEAN)
#undef BOOLEAN
#endif
#define BOOLEAN  short int */

/* #ifdef VOID
#undef VOID
#endif
#define VOID void */

/* #undef NOREF
#define NOREF(a) a = a */

/* #undef XVT_CC_ENUM_END
#define XVT_CC_ENUM_END 127 */


/****************************************************************************
 *     OPERATING SYSTEM-SPECIFIC MACROS
 ****************************************************************************/


#if defined(_WIN32) || defined(__NT__) || defined(__WIN32__) || defined(XVT_CURL_FMT_RCNT)
#undef XVT_OS
#define XVT_OS XVT_OS_WIN32
#else
#undef XVT_OS
#define XVT_OS XVT_OS_WIN16
#endif

#undef XVT_OS_IS_WINOS
#define XVT_OS_IS_WINOS       TRUE     /* reset to TRUE */

#if defined (__MSDOS__) || defined (MSDOS)
#undef XVT_OS_IS_DOSBASED
#define XVT_OS_IS_DOSBASED    TRUE     /* reset to TRUE */
#endif


/****************************************************************************
 *    WINDOW SYSTEM-SPECIFIC MACROS
 ****************************************************************************/

#ifndef WSWIN
#define WSWIN
#endif

#undef XVTWS
#if XVT_OS == XVT_OS_WIN16
#define XVTWS                 WIN16WS
#endif

#if XVT_OS == XVT_OS_WIN32
#define XVTWS                 WIN32WS
#endif


/****************************************************************************
 *    FILE SYSTEM-SPECIFIC MACROS
 ****************************************************************************/

#if XVT_OS == XVT_OS_WIN32
#undef XVT_FILESYS_HPFS
#define XVT_FILESYS_HPFS      TRUE
#undef XVT_FILESYS_NTFS
#define XVT_FILESYS_NTFS      TRUE
#undef XVT_FILESYS_DOS
#define XVT_FILESYS_DOS       TRUE
#endif


#if XVT_OS == XVT_OS_WIN16
#undef XVT_FILESYS_DOS
#define XVT_FILESYS_DOS       TRUE
#undef SZ_FNAME
#define SZ_FNAME              63       /* reset to short filenames */
#endif




/****************************************************************************
 *    COMPILER-SPECIFIC MACROS
 ****************************************************************************/

#if (XVT_CC != XVT_CC_CURL)    && \
    (XVT_CC == XVT_CC_UNKNOWN) && \
    defined(XVT_OS_IS_DOSBASED)

/*Define Microsoft Visual C++ 1.52 (16-bit) compiler */
#if (XVT_OS_IS_DOSBASED == TRUE) && defined(M_I86) && defined(_MSC_VER) && (_MSC_VER == 800)
#if XVT_CC != XVT_CC_UNKNOWN
#error "Compiler redifined"
#endif
#undef XVT_CC
#define XVT_CC XVT_CC_MSV80
#undef XVT_CC_ISANSI
#define XVT_CC_ISANSI         TRUE
#endif

/*Define Microsoft Visual C++ 4.0 (32-bit) compiler */
#if (XVT_OS_IS_DOSBASED == FALSE) && defined(_M_IX86) && defined(_MSC_VER) && (_MSC_VER >= 800)
#if XVT_CC != XVT_CC_UNKNOWN
#error "Compiler redifined"
#endif
#undef XVT_CC
#define XVT_CC XVT_CC_MSV80NT
#undef XVT_CC_ISANSI
#define XVT_CC_ISANSI         TRUE
#endif

/*Define Borland 5.0 C++ (16-bit) compiler */
#if (XVT_OS_IS_DOSBASED == TRUE) && defined(__BORLANDC__) && !defined(__WIN32__)
#if XVT_CC != XVT_CC_UNKNOWN
#error "Compiler redefined"
#endif
#undef XVT_CC
#define XVT_CC XVT_CC_BCPLUS
#undef XVT_CC_ISANSI
#define XVT_CC_ISANSI         TRUE
#endif

/*Define Borland 5.0 C++ (32-bit) compiler */
#if defined(__BORLANDC__) && defined(__WIN32__)
#if XVT_CC != XVT_CC_UNKNOWN
#error "Compiler redefined"
#endif
#undef XVT_CC
#define XVT_CC XVT_CC_BCPLUS32
#undef XVT_CC_ISANSI
#define XVT_CC_ISANSI         TRUE
#endif

/*Define Microsoft Compiler for DEC Alpha workstation */
#if defined(_ALPHA_) && defined(_MSC_VER) && (_MSC_VER >= 800)
#if XVT_CC != XVT_CC_UNKNOWN
#error "Compiler redifined"
#endif
#undef XVT_CC
#define XVT_CC XVT_CC_MSV80NT
#undef XVT_CC_ISANSI
#define XVT_CC_ISANSI         TRUE
#endif


#endif /* XVT_CC */



/****************************************************************************
 *    THREADING AND DLL SPECIFIC MACROS
 ****************************************************************************/

#if (XVT_OS == XVT_OS_WIN16) || ((XVT_OS == XVT_OS_WIN32) && !defined(_ALPHA_))
# undef  XVT_MULTINSTANCE
# define XVT_MULTINSTANCE      TRUE
#endif


/****************************************************************************
 *    XVT/WIN and XVT/NT SPECIFIC ENVIRONMENT INFORMATION
 ****************************************************************************/

#if XVT_OS == XVT_OS_WIN16

# undef pascal
# undef _pascal
# undef __pascal
# undef _export
# undef __export
# undef _asm
# undef __asm
# undef _cdecl
# undef __cdecl
# undef near
# undef _near
# undef __near
# undef far
# undef _far
# undef __far
# undef huge
# undef _huge
# undef __huge
# undef _loadds
# undef __loadds

# undef XVT_CALLCONV1
# if defined(_WINDLL) || defined(__DLL__)
#  define XVT_CALLCONV1 __cdecl __loadds
# else
#  define XVT_CALLCONV1 __cdecl
# endif
/* Note that the compiler complains about __loadds in function pointer
   typedefs, so we exclude it from XVT_CALLCONV_TYPEDEF */ 
# undef  XVT_CALLCONV_TYPEDEF
# define XVT_CALLCONV_TYPEDEF(ret, func, args) \
        ret ( __cdecl * func) args


# if XVT_CC == XVT_CC_BCPLUS && defined(__DLL__)
#  undef XVT_EXPORT
#  define XVT_EXPORT __export
# endif

#endif

#if XVT_OS == XVT_OS_WIN32
# undef _asm
# undef __asm
# undef pascal
# undef __stdcall
# undef _cdecl
# undef __cdecl

# define pascal __stdcall

# undef XVT_CALLCONV1
# define XVT_CALLCONV1 __cdecl

# undef  XVT_CALLCONV_TYPEDEF
# define XVT_CALLCONV_TYPEDEF(ret, func, args) \
         ret ( XVT_CALLCONV1 * func) args

# undef XVT_EXPORT
# ifdef XVT_BUILD_PWR_DLL
#  define XVT_EXPORT __declspec(dllexport)
# else
#  define XVT_EXPORT __declspec(dllimport)
# endif

#endif
  /* Define XVT_CALLCONV_TYPEDEF same for both */

#define EOL_SEQ  "\015\012"

#define XVT_NP_MENU_FIELDS unsigned state;

#define ATTR_WIN_BASE 10000
#define ATTR_WIN_CMD_LINE               (ATTR_WIN_BASE + 0)
#define ATTR_WIN_INSTANCE               (ATTR_WIN_BASE + 1)
#define ATTR_WIN_PREV_INSTANCE          (ATTR_WIN_BASE + 2)
#define ATTR_WIN_MDI                    (ATTR_WIN_BASE + 3)
#define ATTR_WIN_FCN_PRINT_INIT         (ATTR_WIN_BASE + 4)
#define ATTR_WIN_PM_CLASS_ICON          (ATTR_WIN_BASE + 5)
#define ATTR_WIN_PM_DRAWABLE_TWIN       (ATTR_WIN_BASE + 6)
#define ATTR_WIN_PM_SPECIAL_1ST_DOC     (ATTR_WIN_BASE + 7)
#define ATTR_WIN_PM_NO_TWIN             (ATTR_WIN_BASE + 8)
#define ATTR_WIN_PM_TWIN_STARTUP_DATA   (ATTR_WIN_BASE + 9)
#define ATTR_WIN_PM_TWIN_STARTUP_MASK   (ATTR_WIN_BASE + 10)
#define ATTR_WIN_PM_TWIN_STARTUP_RCT    (ATTR_WIN_BASE + 11)
#define ATTR_WIN_PM_TWIN_STARTUP_STYLE  (ATTR_WIN_BASE + 12)
#define ATTR_WIN_PM_USERFONT            (ATTR_WIN_BASE + 13)
#define ATTR_WIN_PM_LOGFONT             (ATTR_WIN_BASE + 14)
#define ATTR_WIN_TIMER_HI_GRANULARITY   (ATTR_WIN_BASE + 15)
#define ATTR_WIN_WCLASSREG_HOOK         (ATTR_WIN_BASE + 16)
#define ATTR_WIN_RAW_EVENT_HOOK         (ATTR_WIN_BASE + 17)
#define ATTR_WIN_POPUP_DETACHED         (ATTR_WIN_BASE + 18)
#define ATTR_WIN_CREATEWINDOW_HOOK      (ATTR_WIN_BASE + 19)
#define ATTR_WIN_OPENFILENAME_HOOK      (ATTR_WIN_BASE + 20)
#define ATTR_WIN_MDI_CLIENT_HWND        (ATTR_WIN_BASE + 21)
#define ATTR_WIN_USE_PCL_RECTS          (ATTR_WIN_BASE + 22)
#define ATTR_WIN_MENU_CACHE_COUNT_MAX   (ATTR_WIN_BASE + 23)
#define ATTR_WIN_MENU_CACHE_COUNT       (ATTR_WIN_BASE + 24)
#define ATTR_WIN_DRAWABLE_TWIN_BACKGRND (ATTR_WIN_BASE + 25)
#define ATTR_WIN_R3_DIALOG_PLACEMENT    (ATTR_WIN_BASE + 26)
#define ATTR_WIN_DELAY_FOCUS_EVENTS     (ATTR_WIN_BASE + 27)
#define ATTR_WIN_USE_PRINT_BANDING      (ATTR_WIN_BASE + 28)
#define ATTR_WIN_USE_CTL3D              (ATTR_WIN_BASE + 29)
#define ATTR_WIN_PM_TWIN_MARGIN_TOP     (ATTR_WIN_BASE + 30)
#define ATTR_WIN_PM_TWIN_MARGIN_BOTTOM  (ATTR_WIN_BASE + 31)
#define ATTR_WIN_PM_TWIN_MARGIN_LEFT    (ATTR_WIN_BASE + 32)
#define ATTR_WIN_PM_TWIN_MARGIN_RIGHT   (ATTR_WIN_BASE + 33)
#define ATTR_WIN_PM_TWIN_FRAME_WINDOW   (ATTR_WIN_BASE + 34)

#define XVT_ESC_WIN_BASE                4000
#define XVT_ESC_GET_PRINTER_INFO        (XVT_ESC_WIN_BASE + 0)
#define XVT_ESC_WIN_TERMINATE           (XVT_ESC_WIN_BASE + 1)


/*---------------------------------------------------------------------------
   Constants for Windows resource scripts
---------------------------------------------------------------------------*/
#define IDM_WINDOWMENU      (0x8000)

#define STR_APPNAME         30399       /* string table constant */
#define ICON_RSRC           9012        /* icon resource */
#define ID_PATH             9081        /* control IDs */
#define ID_EDIT             9082
#define ID_LISTBOX          9083
#define ID_TITLE            9084

#define IDM_WINDOWTILE      (IDM_WINDOWMENU + 1)
#define IDM_WINDOWCASCADE   (IDM_WINDOWMENU + 2)
#define IDM_WINDOWICONS     (IDM_WINDOWMENU + 3)
#define IDM_WINDOWCHILD     (IDM_WINDOWMENU + 20)

#define M_FONT_SELECT       (M_FONT + 1 )  

#if !defined(NOT_XVT_APP)
#define main   xvt_main
#ifdef __cplusplus
extern "C" {
   extern int XVT_CALLCONV1 xvt_main(int argc, char **argv);
}
#endif
#endif

#define XVT_FAST_WIDTH 1

#define DLLFAR

/*---------------------------------------------------------------------------
   Unix-like File Declarations
---------------------------------------------------------------------------*/
 
#ifndef NO_INCLUDES
#include <sys/types.h>
#include <sys/stat.h>
typedef struct stat STAT;
#include <io.h>
#endif

/****************************************************************************
 *    Overide to new PTK level
 ****************************************************************************/

#undef XVT_PTK_VERSION_PATCH
#define XVT_PTK_VERSION_PATCH 0

/****************************************************************************
 *    INCLUDE NATIVE HEADER FILES
 ****************************************************************************/

#ifdef XVT_INCL_NATIVE
# if XVT_OS == XVT_OS_WIN32
#  undef  VOID
#  undef  BOOLEAN
#  define BOOLEAN BOOLEAN_NT
# endif

#if !defined(NOT_XVT_APP)
# include "windows.h"
# include "windowsx.h"
#endif

# if XVT_OS == XVT_OS_WIN32
#  define VOID void
#  undef  BOOLEAN
#  define BOOLEAN  short int
# endif
#endif

#if !defined(NO_INCLUDES) && (XVT_CC != XVT_CC_CURL)

#include <stdlib.h>
#include <limits.h>

typedef wchar_t XVTK_WCHAR;
#define XVTK_MAX_MB_SIZE MB_LEN_MAX

#endif

#endif /* XVT_INCL_XVTPLAT */

