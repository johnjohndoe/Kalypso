
/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvt_env.h,v $
 *  $Revision: 1.14 $
 *
 *  Purpose:  Fingers XVT-supported compilers, operating systems,
 *            window systems and file systems and performs other
 *            platform-specific initializations
 * 
 ****************************************************************************/

#ifndef XVT_INCL_XVTENV
#define XVT_INCL_XVTENV

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

/****************************************************************************
 *    MISCELLANEOUS ENVIRONMENT-SPECIFIC MACROS
 ****************************************************************************
 *
 *    BOOLEAN
 *
 *       Is defined to be "short int" if it was not previously defined.
 *
 *       TRUE - Is defined to be 1 if it was not previously defined.
 *
 *       FALSE - Is defined to be 0 if it was not previously defined.
 *
 *    VOID
 *
 *       Is defined to be void if the latter is supported for
 *       pointer types, char otherwise.  This macro is intended
 *       to be used to represent pointers to generic data as in:
 *
 *          VOID  *foo;
 *
 *    NOREF
 *
 *       NOREF is a macro that is defined such that putting NOREF(x); in
 *       function will guarantee that no warnings are generated for
 *       formal parameter x.
 *
 *
 *    XVT_CC_ENUM_END
 *
 *       Will be defined to a number so that we can force some C++
 *       compilers to make our enums the size of an int, which is
 *       necessary for C/C++ compatibility.
 *
 ****************************************************************************/

#ifndef _BOOLEAN_DEFINED   /* silly, but seems to be common */
#define _BOOLEAN_DEFINED
#ifndef BOOLEAN
#define BOOLEAN               short int
#endif
#endif

#ifdef TRUE
#if (TRUE == 0)
#undef TRUE
#define TRUE 				  1
#endif
#else
#define TRUE                  1
#endif

#ifdef FALSE
#if (FALSE != 0)
#undef FALSE
#define FALSE 0
#endif
#else
#define FALSE                 0
#endif

#ifdef VOID
#undef VOID
#endif
#define VOID                  void

#define NOREF(a)              a = a

#define XVT_CC_ENUM_END       127


/****************************************************************************
 *     OPERATING SYSTEM-SPECIFIC MACROS
 ****************************************************************************
 *
 *     XVT_OS
 *
 *        Will be defined to be one of the list of known operating
 *        systems.
 *
 *     XVT_OS_ISUNIX
 *
 *        Will be defined to 1 (TRUE) if the operating system is a
 *        Unix (V7 or later) operating system, 0 (FALSE) otherwise.
 *        In particular, you can count on select() being available
 *        if this is defined.
 *
 *     XVT_OS_BSD_SIGNALS
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        BSD-style signal handling.
 *
 *     XVT_OS_BSD_TIMERS
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        BSD-style timers.
 *
 *     XVT_OS_BSD_DIR
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        BSD-style directory headers.
 *
 *     XVT_OS_BSD_GETWD
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        a BSD-style getwd() call.
 *
 *     XVT_OS_SYSV_SIGNALS
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        SYSV-style signal handling.
 *
 *     XVT_OS_SYSV_TIMERS
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        SYSV-style timers.
 *
 *     XVT_OS_SYSV_DIR
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        SYSV-style directory headers.
 *
 *     XVT_OS_SYSV_GETCWD
 *
 *        Will be defined to 1 (TRUE) if the operating system has
 *        a SYSV-style getcwd() call.
 *
 *     XVT_OS_IS_SUNOS
 *
 *        Will be defined to 1 (TRUE) if the operating system is
 *        any version of SUNOS
 *
 *     XVT_OS_IS_WINOS
 *
 *        Will be defined to 1 (TRUE) if the operating system is
 *        any version of MS-Windows, including NT
 *
 *     XVT_OS_IS_DOSBASED
 *        Will be defined to 1 (TRUE) if the operating system is
 *        based on DOS -- one of DOS, Windows 2.x or Windows 3.x
 *
 *     XVT_OS_IS_MACOS
 *        Will be defined to 1 (TRUE) if the operating system is
 *        based on MACOS -- either 680x0 or PowerPC
 *
 ****************************************************************************/

/*--------------------------------------------------------------------------
     Unsupported Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_UNKNOWN        0        /* Undefined operating system */

/*-------------------------------------------------------------------------- 
     Support for XVT/MAC Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_MAC            100		/* Apple Mac-OS */
#define XVT_OS_MACPPC         101		/* Apple PowerPC Mac-OS */

/*--------------------------------------------------------------------------
     Support for XVT/PM Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_OS23           200      /* IBM OS/2 3.x */
#define XVT_OS_OS22           XVT_OS_OS23

/*--------------------------------------------------------------------------
     Support for XVT/NT Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_WIN32          300           /* MS Windows NT 3.1 */
#define XVT_OS_NT             XVT_OS_WIN32  /* for compatibility with docs */

/*--------------------------------------------------------------------------
     Support for XVT/Win, XVT/CH Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_WIN16          400           /* MS Windows 3.x */
#define XVT_OS_WIN            XVT_OS_WIN16  /* for compatibility with docs */

#define XVT_OS_DOS            450      /* MS DOS */


/*--------------------------------------------------------------------------
     Support for XVT/XM, XVT/XOL, XVT/CH Operating Systems
  -------------------------------------------------------------------------*/

#define XVT_OS_SUNOS4         500      /* Sun OS 4.1.x */
#define XVT_OS_SUNOS5         501      /* SunOS 5.x (Solaris) */
#define XVT_OS_SUNOS          XVT_OS_SUNOS5    /* latest SUNOS we support */

#define XVT_OS_HPUX           510      /* HP Unix */

#define XVT_OS_ULTRIX         520      /* DEC Ultrix */
#define XVT_OS_VMS            521      /* DEC VMS, Open VMS */
#define XVT_OS_OSF            522      /* DEC OSF/1 */

#define XVT_OS_AIX            530      /* IBM AIX */

#define XVT_OS_SCOUNIX        540      /* SCO Unix */

#define XVT_OS_IRIX           550      /* SGI IRIX OS */

#define XVT_OS_ISC            560      /* Interactive UNIX */

#define XVT_OS_DGUX           570      /* Data General Aviion */

#define XVT_OS_UNIXWARE       580      /* UnixWare */

#define XVT_OS_NCR            590      /* NCR Unix */

#define XVT_OS_LINUX          595      /* Linux */


/****************************************************************************
 *   Operating System Keyword Default Initialization
 ****************************************************************************/

#ifndef XVT_OS
#define XVT_OS                XVT_OS_UNKNOWN
#endif

#define XVT_OS_ISUNIX         FALSE
#define XVT_OS_IS_SUNOS       FALSE
#define XVT_OS_IS_WINOS       FALSE
#define XVT_OS_IS_DOSBASED    FALSE
#define XVT_OS_IS_MACOS       FALSE

#define XVT_OS_BSD_SIGNALS    FALSE
#define XVT_OS_BSD_TIMERS     FALSE
#define XVT_OS_BSD_DIR        FALSE
#define XVT_OS_BSD_GETWD      FALSE
#define XVT_OS_SYSV_SIGNALS   FALSE
#define XVT_OS_SYSV_TIMERS    FALSE
#define XVT_OS_SYSV_DIR       FALSE
#define XVT_OS_SYSV_GETCWD    FALSE


/****************************************************************************
 *    WINDOW SYSTEM-SPECIFIC MACROS
 ****************************************************************************
 *
 *     XVTWS
 *
 *        Will be defined type of window system
 *
 ****************************************************************************/

/*--------------------------------------------------------------------------
     Support for XVT Window Systems
  -------------------------------------------------------------------------*/

#define XVT_WS_UNKNOWN        0
#define MACWS                 100      /* Apple Macintosh */
#define PMWS                  200      /* IBM OS/2 PM */
#define WIN32WS               300      /* MS Windows 3.1 for NT */
#define WIN16WS               400      /* MS Windows 3.x for Win16 */
#define WMWS                  450      /* Character */
#define MTFWS                 500      /* Motif */
#define XOLWS                 501      /* Open Look */

#define NTWS                  WIN32WS  /* for compatibility with docs */
#define WINWS                 WIN16WS  /* for compatibility with docs */ 

/****************************************************************************
 *   Window System Keyword Default Initialization
 ****************************************************************************/

#define XVTWS                 XVT_WS_UNKNOWN


/****************************************************************************
 *    FILE SYSTEM-SPECIFIC MACROS
 ****************************************************************************
 *
 *     XVT_FILESYS_*
 *
 *        Will be defined to 1 (TRUE) if the file system (denoted in place
 *        of *) is supported
 *
 *     SZ_FNAME
 *
 *        Will define maximum number of characters allowed in a filename
 *        not including null terminator.
 *
 *     SZ_LEAFNAME
 *
 *        Will define maximum number of characters allowed in a leafname
 *        not including null terminator. A leafname is any single part of
 *        a pathname, either directory, file, or link name, not including
 *        any path separator characters.
 *
 ****************************************************************************/

/*--------------------------------------------------------------------------
     Support for XVT File Systems

        need to check all of these macros below in
        file system verification section
  -------------------------------------------------------------------------*/

#define XVT_FILESYS_MAC       FALSE    /* Apple Macintosh file system */
#define XVT_FILESYS_HPFS      FALSE    /* High Performance File System */
#define XVT_FILESYS_NTFS      FALSE    /* NT File System */
#define XVT_FILESYS_DOS       FALSE    /* MS-DOS or OS/2 DOS file system */
#define XVT_FILESYS_UNIX      FALSE    /* Unix file system */
#define XVT_FILESYS_VMS       FALSE    /* VMS file system */


/****************************************************************************
 *   File System Keyword Default Initialization
 ****************************************************************************/

#define SZ_FNAME              256      /* default to long filenames */
#define SZ_LEAFNAME           256      /* default to long leafnames */


/****************************************************************************
 *    COMPILER-SPECIFIC MACROS
 ****************************************************************************
 *
 * Modules including this file may rely on distinguishing two classes of
 * compilers: ANSI-capable compilers and non-ANSI compilers.  The latter
 * are not further classified.  It is assumed that any non-ANSI compiler
 * supports at least the following ANSI features:
 *
 *   enumerations
 *   void pointers
 *   void as a function return type
 *   structure assignment
 *   by-value structure parameter passing
 *
 *    XVT_CC
 *
 *       Will be defined to be one of the list of known C compilers or
 *       XVT_CC_UNKNOWN if the compiler could not be identified.
 *
 *    XVT_CC_ISANSI
 *
 *       Will be defined to 1 (TRUE) if the compiler can handle ANSI C,
 *       0 (FALSE) if not.  This macro is NOT equivalent to __STDC__.
 *       Many compilers only define __STDC__ to be 1 when they are in
 *       "strict ANSI" mode.  In particular, this means that compilers
 *       support prototypes, "stdarg.h", and the keywords const, volatile,
 *       and signed, and support (void*) pointers.
 *
 *    XVT_CC_ISANSICPP
 *
 *       Will be defined to 1 (TRUE) if the compiler pre-processor can
 *       handle ANSI C, inparticular that it used the ## concatenation
 *       directive (rather than the /--/ used with many K&R compilers
 *       This flag may be often true for C++ compilers even when they
 *       do not set __STDC__ as 1.
 *
 *
 *    XVT_CC_PLUS
 *
 *       Will be defined to 1 (TRUE) if the compiler is a C++ compiler,
 *       0 (FALSE) if not.  Note that there are several different types
 *       of C++: this file does not try to classify them further.  This
 *       macro is only defined if the C++ compiler is not a proper
 *       superset of C (ie requires #extern or something to compile C)
 *
 *    XVT_CC_PROTO
 *
 *       Will be defined to 1 (TRUE) if prototypes can be used, 0 (FALSE)
 *       otherwise.  This macro is used to conditionally compile function
 *       prototypes like this:
 *
 *          #if XVT_CC_PROTO
 *             extern void foo( int bar, float baz );
 *          #else
 *             extern void foo();
 *          #endif
 *             :
 *             :
 *          void
 *          #if XVT_CC_PROTO
 *             foo( int bar, float baz )
 *          #else
 *          foo( bar, baz )
 *             int  bar;
 *             float  baz;
 *          #endif
 *          {
 *             body of function foo
 *          }
 *
 *       If this style is used, consistency between the declarations may
 *       be maintained using a prototyping compiler (for ANSI) followed
 *       by lint (for K&R).  Alternatively, the following prototype
 *       construction macros could be used:
 *
 *          extern void foo XVT_CC_ARGS(( int  bar, float  baz ));
 *             :
 *             :
 *          void foo XVT_CC_ARGL(( bar, baz ))
 *          XVT_CC_ARG( int, bar )
 *          XVT_CC_LARG( float, baz )
 *          {
 *             body of function foo
 *          }
 *
 *       Note that functions with no arguments are
 *       declared and defined as:
 *
 *          extern void foo XVT_CC_ARGS(())
 *             :
 *             :
 *          void foo XVT_CC_ARGS(())
 *          {
 *             body of foo
 *          }
 *
 *       Some environments (PC) may not need to make any attempt at
 *       all to be compatible with K&R.
 *
 *
 *    XVT_CC_CALLCONV1, XVT_CC_CALLCONV2, XVT_CALLCONV_TYPEDEF
 *
 *       Will be defined to values reflecting PC linking conventions.
 *
 *****************************************************************************/

/*--------------------------------------------------------------------------
     Unsupported Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_UNKNOWN        0        /* Undefined compiler */
#define XVT_CC_CURL           1        /* XVT-CURL  uses when scanning hdrs */
#define XVT_CC_HELPC          2        /* XVT-HELPC uses when scanning */

/*--------------------------------------------------------------------------
     Support for XVT/MAC Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_MPW            100      /* MPW C compiler */
#define XVT_CC_MPWPPC		  101      /* MPW PowerPC compiler */
#define XVT_CC_MPWSC          102      /* MPW SC (Symantec C) compiler */ 
#define XVT_CC_MPWSCPP        103      /* MPW SCpp (Symantec C++) compiler */

#define XVT_CC_THINK          110      /* THINK C compiler */

#define XVT_CC_SYMCPLUS       120      /* Symantec C++ compiler */

#define XVT_CC_MACCW		  130      /* Metroworks code warrior compiler */

/*--------------------------------------------------------------------------
     Support for XVT/PM Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_IBMCPP         200      /* IBM C Set++ for OS/2 2.x */

#define XVT_CC_BORC           210      /* Borland C++ for OS/2 2.x */

/*--------------------------------------------------------------------------
     Support for XVT/NT Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_MSV80NT        300      /* MS Visual C/C++ 8.0 for Win32 */

/*--------------------------------------------------------------------------
     Support for XVT/Win, XVT/CH Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_MS70           400      /* MS C/C++ 7.0 */
#define XVT_CC_MSV80          401      /* MS Visual C/C++ 8.0 for Win16 */

#define XVT_CC_BCPLUS         410      /* Borland C/C++ 4.0 */

#define XVT_CC_BCPLUS32       420      /* Borland C/C++ 32 bit 4.0 */

/*--------------------------------------------------------------------------
     Support for XVT/XM, XVT/XOL, XVT/CH Compilers
  -------------------------------------------------------------------------*/

#define XVT_CC_SUNB           500      /* bundled Sun C compiler */
#define XVT_CC_SUN_CPP        501      /* Sun SPARCompiler C++ */
#define XVT_CC_SUNSC2         502      /* Sun SparCompiler 2.0 */

#define XVT_CC_HPUXB          510      /* HP-UX ANSI C compiler */
#define XVT_CC_HP_CPP         511      /* HP Softbench C++ */

#define XVT_CC_ULTRIXB        520      /* bundled DEC Ultrix C compiler */
#define XVT_CC_DECVMS         521      /* DEC VAX/VMS C compiler */
#define XVT_CC_DEC            522      /* DEC ANSI C compiler (DEC CC) */
#define XVT_CC_DEC_CPP        523      /* DEC CXX (C plus plus) */

#define XVT_CC_AIXB           530      /* bundled IBM AIX C compiler */
#define XVT_CC_XLC            531      /* IBM's ANSI compiler for AIX */
#define XVT_CC_XLC_CPP        532      /* IBM's C++ compiler for AIX */

#define XVT_CC_SCOUB          540      /* bundled SCO Unix C compiler */

#define XVT_CC_SG             550      /* bundled Silicon Graphics */

#define XVT_CC_ISCB           560      /* bundled 386/ix C compiler */

#define XVT_CC_DG             570      /* Data General AViiON bundled */

#define XVT_CC_UNIXWARE       580      /* UnixWare cc compiler */

#define XVT_CC_NCR            590      /* NCR cc compiler */

#define XVT_CC_CODECENTER     600      /* CodeCenter with ANSI */
#define XVT_CC_OBJECTCENTER   601      /* ObjectCenter */

#define XVT_CC_GNU            610      /* GNU cc compiler */


/****************************************************************************
 *   Compiler Keyword Default Initialization
 ****************************************************************************/

#if defined(CURL_INVOKED)
#define XVT_CC                XVT_CC_CURL       /* scanning for CURL */
#elif defined(__helpc__)
#define XVT_CC                XVT_CC_HELPC
#else
#define XVT_CC                XVT_CC_UNKNOWN
#endif

#define XVT_CC_PROTO          TRUE

/*--------------------------------------------------------------------------
     Support for ANSI Compilers
  -------------------------------------------------------------------------*/
#define XVT_CC_ISANSICPP      TRUE   /* true for most C++ compilers, too */

#if defined(__STDC__) && (__STDC__ == 1)
#define XVT_CC_ISANSI         TRUE
#else
#define XVT_CC_ISANSI         FALSE
#endif

/*--------------------------------------------------------------------------
     Support for C++ Compilers
  -------------------------------------------------------------------------*/

#if defined(_cplusplus) || defined(__cplusplus)
#define XVT_CC_PLUS           TRUE
#else
#define XVT_CC_PLUS           FALSE
#endif


/****************************************************************************
 *   Define Linkage Conventions
 ****************************************************************************/

#define XVT_CALLCONV1           /* Default is no linkage convention defined */
#define XVT_CALLCONV2
#define XVT_EXPORT
                             
                   /* Default to no linkage conventions in callback typedef */
#define XVT_CALLCONV_TYPEDEF(ret, func, args) \
        ret (* func) XVT_CC_ARGS(args)

/* Note: We do not define away _Cdecl & cdecl since the Borland compiler for
         Windows defines it again in one of its header files, causing
         a warning to show everytime xvt.h is include
*/

#if defined(__BORLANDC__) && defined(__WIN32__)
#define _Optlink _USERENTRY
#else
#define cdecl                 /* Default is linkage keywords defined away */
#define _Optlink
#endif
#ifndef __MWERKS__            /* Metrowerks gives an error if you try to undef */
#ifdef  pascal                /* a keyword like pascal */
#undef  pascal
#endif
#define pascal
#endif
#ifdef _export
#undef _export
#endif
#define _Far16
#define _Far32
#define _Fastcall
#define _Pascal
#define _System
#define _asm
#define _cdecl
#define _export
#define _far16
#define _fastcall
#define _pascal
#define _stdcall
#define _syscall
#define _loadds
#define __asm
#define __cdecl 
#define __export
#define __far16
#define __fastcall
#define __pascal
#define __stdcall
#define __syscall
#define __loadds

/****************************************************************************
 *   Define PC Keywords
 ****************************************************************************/

#define far                  /* Default is keywords defined away */
#define huge
#define near
#define _far
#define _huge
#define _near
#define __far
#define __huge
#define __near


/****************************************************************************
 *    THREADING AND DLL SPECIFIC MACROS
 ****************************************************************************
 *
 *     XVT_MULTINSTANCE
 *
 *       Set to TRUE when static variables must be defined for multiple
 *       instances such as for Dynamic Link Libraries or Multiple threads
 *
 *     XVT_MULTTHREAD
 *
 *       Set to TRUE when platform may be creating multiple threads.
 *       NOTE: XVT DOES NOT GUARANTEE THREAD SAFETY, EVEN WHEN TRUE.
 *       ONLY USED INTERNALLY TO XVT FOR PRINT THREADS.
 *
 ****************************************************************************/

#define XVT_MULTINSTANCE      FALSE
#define XVT_MULTTHREAD        FALSE

/****************************************************************************
 ****************************************************************************
 *   INCLUDE PLATFORM SPECIFIC ENVIRONMENT INFORMATION
 ****************************************************************************
 ****************************************************************************
 *   SPECIAL NOTE:
 *   Compilers are assumed to be ANSI and thus support:
 *       const
 *       volatile
 *       signed
 *   If a compiler does not support these keywords then
 *   "xvt_plat.h" must define them away.
 ****************************************************************************
 ****************************************************************************/

#if defined(_cplusplus) || defined(__cplusplus)
}     /* extern "C" */
#endif

#include "xvt_plat.h"

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

/****************************************************************************
 ****************************************************************************
 *
 *   IMPORTANT NOTE:
 *
 *   At this point, the compiler, operating system, window system and
 *   file system should be identified by the "xvt_plat.h" XVT-platform
 *   specific file.  If this is not the case, the developer has requested 
 *   an unsupported system and a preprocessor error will be generated.
 *
 ****************************************************************************
 ****************************************************************************/

#if (!defined(XVT_OS) || (XVT_OS == XVT_OS_UNKNOWN)) && (XVT_CC != XVT_CC_CURL) && (XVT_CC != XVT_CC_HELPC)
#error "Undefined_Operating_System"
#endif

#if !defined(XVTWS) || (XVTWS == XVT_WS_UNKNOWN)
#error "Undefined_Window_System"
#endif

#if !defined(XVT_CC) || (XVT_CC == XVT_CC_UNKNOWN)
#error "Undefined_Compiler"
#endif

/*  need to define all of these macros above in file system section */
#if !(XVT_FILESYS_MAC)  && !(XVT_FILESYS_HPFS) && !(XVT_FILESYS_NTFS) && !(XVT_FILESYS_DOS)  && !(XVT_FILESYS_UNIX) && !(XVT_FILESYS_VMS)  && (XVT_CC != XVT_CC_CURL) && (XVT_CC != XVT_CC_HELPC)
#error "Undefined_File_System"
#endif

#if ((XVT_OS_BSD_SIGNALS) && (XVT_OS_SYSV_SIGNALS)) || ((XVT_OS_BSD_TIMERS)  && (XVT_OS_SYSV_TIMERS))  || ((XVT_OS_BSD_DIR)     && (XVT_OS_SYSV_DIR))     || ((XVT_OS_BSD_GETWD)   && (XVT_OS_SYSV_GETCWD))
#error "Invalid_Unix_Macros"
#endif


/****************************************************************************
 *   Define Prototyping Information
 ****************************************************************************/

#if  XVT_CC_PROTO
#define XVT_CC_ARGL(al__)     (
#define XVT_CC_ARG(t__,a__)   t__ a__,
#define XVT_CC_LARG(t__,a__)  t__ a__)
#define XVT_CC_ARGS(al__)     al__
#define XVT_CC_NOARGS()       (void)
#else
#define XVT_CC_ARGL(al__)     al__
#define XVT_CC_ARG(t__,a__)   t__ a__;
#define XVT_CC_LARG(t__,a__)  t__ a__;
#define XVT_CC_ARGS(al__)     ()
#define XVT_CC_NOARGS()       ()
#endif

#if defined(_cplusplus) || defined(__cplusplus)
}     /* extern "C" */
#endif

#endif /* XVT_INCL_XVTENV */

