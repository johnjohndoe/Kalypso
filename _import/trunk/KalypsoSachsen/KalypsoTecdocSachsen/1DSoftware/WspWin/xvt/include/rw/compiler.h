#ifndef __RWCOMPILER_H__
#define __RWCOMPILER_H__ 1

#if 0
/*
 * Compiler and system related foibles and directives
 *
 * $Id: compiler.h,v 1.2 1996/04/09 18:48:24 rohde Exp $
 * $Revision: 1.2 $
 *
 * (c) Copyright 1989 - 1996 Rogue Wave Software, Inc.
 * ALL RIGHTS RESERVED
 *
 * The software and information contained herein are proprietary to, and
 * comprise valuable trade secrets of, Rogue Wave Software, Inc., which
 * intends to preserve as trade secrets such software and information.
 * This software is furnished pursuant to a written license agreement and
 * may be used, copied, transmitted, and stored only in accordance with
 * the terms of such license and with the inclusion of the above copyright
 * notice.  This software and information or any other copies thereof may
 * not be provided or otherwise made available to any other person.
 *
 * Notwithstanding any other lease or license that may pertain to, or
 * accompany the delivery of, this computer software and information, the
 * rights of the Government regarding its use, reproduction and disclosure
 * are as set forth in Section 52.227-19 of the FARS Computer
 * Software-Restricted Rights clause.
 * 
 * Use, duplication, or disclosure by the Government is subject to
 * restrictions as set forth in subparagraph (c)(1)(ii) of the Rights in
 * Technical Data and Computer Software clause at DFARS 252.227-7013.
 * Contractor/Manufacturer is Rogue Wave Software, Inc.,
 * P.O. Box 2328, Corvallis, Oregon 97339.
 *
 * This computer software and information is distributed with "restricted
 * rights."  Use, duplication or disclosure is subject to restrictions as
 * set forth in NASA FAR SUP 18-52.227-79 (April 1985) "Commercial
 * Computer Software-Restricted Rights (April 1985)."  If the Clause at
 * 18-52.227-74 "Rights in Data General" is specified in the contract,
 * then the "Alternate III" clause applies.
 *
 * The file compiler.h is automatically generated.
 *
 * Following are brief descriptions of each preprocessor macro which
 * is defined or not in each platform's code block.

 * Although you may edit this file if you wish, we encourage you in
 * most cases to use the configuration script which shipped with
 * your Rogue Wave library unless it is making a wrong decision for
 * your compiler.  If that is the case, please contact Rogue Wave
 * Technical Support so that we may correct the problem for you and
 * other future users of our libraries.

 *** Some macros will be "default" configured, either because we cannot
 *** determine the situation on your machine, or because you may have
 *** more than one reasonable choice. Such macros will be marked in
 *** the descriptions below by ">>>" rather than the "***" which marks
 *** macros which we believe we can automatically configure for you.
 *** If you wish to change those macros, please search for them in the
 *** section of this header devoted to your platform, and make the
 *** change there.


 Availability of the standard library
 ************************************
 *** RW_NO_STL: Defined if your compiler doesn't support
        the C++ Standard Library containers, iterators, or algorithms.
 *** RW_NO_NEW_HEADER: Defined if #include <something.h> works, but
        #include <something> does not.

 The sizes of things
 *******************
 *** RW_BYTES_PER_WORD:    ...
 *** RW_BYTES_PER_PTR: Defined to the appropriate numbers for your
        platform.
 *** RW_DEFAULT_PRECISION: Defined to the number of decimal digits of
        precision for your platform's type double.

 Template support
 ****************
 *** RW_NO_TEMPLATES: Defined if your compiler doesn't understand
        template syntax at all.
 *** RW_BROKEN_TEMPLATES: Defined if your compiler understands only
        very simple template code. (Like AT&T CFront V3.0).
 >>> RW_COMPILE_INSTANTIATE: Defined if your compiler requires both
        declarations and definitions of template code to be in the
        same file.  (Some compilers pass our test, for link time
        instantiation, but fail on more complex code)
 *** RW_NO_TEMPLINST_ON_BASE: Defined if your compiler will not
        instantiate a function template when an actual parameter
        type is derived from the corresponding formal parameter
        type (i.e. the compiler demands too exact of a match)
 *** RW_BROKEN_TEMPLATE_TYPEDEFS: Defined if your compiler doesn't
        handle a template class out-of-line member function argument
        list that is based on a typedef scoped in a different class.
 *** RW_NO_EXPRESSION_TEMPLATES: Defined if your compiler cannot
        handle templates that compile into inline code to evaluate
        arithmetic expressions.

 Thread Support
 **************
 >>> RW_SOLARIS_THREADS: Defined if you have a Solaris platform that
        supports threads.  (We choose this if you have it, in
        preference to other thread packages. You may prefer a
        different package.)
 *** RW_POSIX_THREADS: Defined if you have a threads package which
        meets an early Posix draft, and you don't have Solaris threads.
 *** RW_POSIX_D10_THREADS: As above, if you have threads which meet
        the Posix Draft 10 threads description.
 *** RW_NO_THREADS: If you have no threads, or your package doesn't
        meet our expectations for header and function names.

 Exception handling
 ******************
 >>> RW_NO_EXCEPTIONS: Defined if your compiler doesn't adequately
        support exceptions. (You might prefer to compile with no
        exception support, even though your compiler provides it, if
        you have efficiency considerations, and can live without
        "real" exceptions.)
 *** RW_NO_THROW_WITH_SHARED: Defined if your compiler cannot handle
        exceptions thrown from a shared library.
 *** RW_NO_XMSG: We will always define this, since the standard has
        not yet settled... Meanwhile, we provide our own error
        message types.

 Internationalization issues
 ***************************
 *** RW_NO_WSTR: Defined if your compiler doesn't support wide
        character strings such as wslen().
 *** RW_USE_WCHEADERS: Defined if we have to #include <wctype.h> and
        <widec.h> to find wide character string prototypes.
 *** RW_WSTR_C_HEADERS: Defined if we have to #include <ctype.h> and
        <string.h> to find wide character string prototypes.
 *** RW_SUPPLY_WSTR: Defined if we are able to provide the wide string
        support by using the 32-bit Windows API, and you have no other
        wide string support.
 *** RW_NO_OVERLOAD_WCHAR: Defined if your compiler provides wchar_t
        as a typedef rather than a distinct type.
 *** RW_NO_WCSXFRM: Defined if your compiler doesn't provide a
        prototype for function wcsxfrm(). This is an optimization
        that we pass through to you, and don't otherwise use.
 *** RW_NO_LOCALE: Defined if your compiler doesn't fully support the
        ANSI C locale facility. We require setlocale(), strxform()
        strftime() and strcoll().
 *** RW_NO_STRFTIME_CAPC: Defined if your compiler doesn't support
        the %C directive to strftime(). This is an optimization
        opportunity.
 >>> RW_MESSAGE: We will always define this to RW_NOMSG. You should
        set it otherwise if you have, and wish to use, a messaging
        facility. The choices are:
        RW_NOMSG   : we will use imbedded "english" messages.
        RW_CATGETS : we will use catgets() for messages.
        RW_GETTEXT : we will use gettext() for messages.
        RW_DGETTEXT: we will use dgettext() for messages.
 >>> RW_MESSAGE_SET_NUMBER: Defined to 1 whenever you choose the
        RW_CATGETS messaging facility. You may defined it to another
        number if you need to.

 Issues concerning time and zone
 *******************************
 *** RW_NO_CLOCK: Defined if a prototype for the ANSI C function
        clock() cannot be found in <time.h> or <stdlib.h>
 *** RW_NO_GLOBAL_TZ: Defined if we cannot find global variables
        daylight, timezone, and tzname; or the same names with
        leading underscore.
 *** RW_NO_LEADING_UNDERSCORE: Defined if we find variable daylight,
        and not _daylight.
 *** RW_NO_GETTIMEOFDAY: Defined if we cannot find the time variables
        and can also not find the function gettimeofday().
 *** RW_STRUCT_TM_TZ: Defined if your <time.h> has a struct tm with
        extra member data "tm_zone" and "tm_gmtoff." This is an
        optimization opportunity.

 Compilers by name
 *****************
 >>> __ATT2__: Defined if your compiler is based on CFront version 2.x
 >>> __ATT3__: Defined if your compiler is based on CFront version 3.x
        These are set based on whether you tell us your compiler is
        "CFront" during configuration.  If you do not tell us your
	compiler is "CFront" and you get errors in bstream.h (and
	other places) try setting the appropriate one.
 *** __GLOCK__: Defined if your compiler is Glockenspiel.

 Miscellaneous quirks and limitations
 ************************************
 *** RW_DIRECTORY_WO_SLASHES: Defined if, for example, #include
        <sys/types.h> fails, but #include <types.h> succeeds.
 *** RW_NO_CPP_RECURSION: Defined if your C preprocessor cannot
        handle recursive macros.
 *** RW_FRIEND_THEN_INLINE: Defined if inline friend functions must be
        declared as "friend inline ..." in that order.
 *** RW_GLOBAL_ENUMS: Some older compilers make enums declared inside
        classes part of the global scope.
 >>> RW_INLINE86_ASSEMBLY: We will never define this. It is here for
        historical reasons. (Used by older Math.h++ versions)
 *** RW_IOS_XALLOC_BROKEN: Defined if the iostream functions
        ios::xalloc() and ios::pword() fail to initialize the
        xalloc'd storage to zero. Prevents RWLocale::imbue(ios&) from
        being available.
 >>> RW_KR_ONLY: We will never define this. It refers to the style
        of C code which a C (not C++) compiler on your platform can
        handle, and is here for historical reasons. (Used by older
        Math.h++ versions.)
 *** RW_NO_ACCESS_ADJUSTMENT: Defined if your compiler cannot handle
        adjusting the access rights of a base method in a derived
        class using the "::baseMethodName" construct
 *** RW_NO_ANSI_SPRINTF: Defined if your sprintf doesn't return an
        int describing the amount of buffer used.
 *** RW_NO_MEMMOVE: Defined if your compiler doesn't provide ANSI C's
        memmove().
 *** RW_NO_NESTED_QUOTES: Defined if your C preprocessor cannot
        handle macros with nested quotes.
 *** RW_NO_OSTR_REF_CAST: Defined if your compiler cannot cast
        ostream to ostream&.
 *** RW_NO_OVERLOAD_SCHAR: Defined if your compiler doesn't
        distinguish between formal arg "char" and "signed char."
 *** RW_NO_OVERLOAD_UCHAR: As above, but for "unsigned char."
 >>> RW_NO_POSIX_RE: We always define this because POSIX.2 C bindings
        don't support wchar_t or embedded nulls. If you have and can
        use these bindings, you may comment this out.
 *** RW_NO_POSTFIX: If your compiler doesn't distinguish overloading
        postfix operator++ from overloading prefix ++.
 >>> RW_NO_SCHAR: We will always define this to be the same as
        RW_NO_OVERLOAD_SCHAR. Here for historical reasons. (Used by
        Math.h++)
 *** RW_NO_STRICMP: Defined if your compiler doesn't provide
        stricmp() (case independent comparison of char*)
 *** RW_NO_STRNICMP: Defined if your compiler doesn't provide
        strnicmp() (case independent comparison of char*, nchars).
 *** RW_NO_STRSTR: Defined if your compiler doesn't provide ANSI C's
        strstr() (string search).
 *** RW_NO_XDR: Defined if your compiler doesn't support XDR streams.
 *** RW_NON_ANSI_HEADERS: Defined if the prototype for memcpy is in
        <memory.h> rather than <string.h> as ANSI requires.
 *** RW_NOT_POSIX_FSTAT: Defined if you don't have POSIX file status
        functions fstat() and access().
 *** RW_UNLINK_NON_CONST: defined if the system unlink() function
        requires a non-const char* argument.
 *** RW_WRAP_C: Defined if your header files must be wrapped in the
        'extern "C" { ... }' construct.
 *** RW_GLOBAL_BEFORE_CLASS_SCOPE: Under certain circumstances,
        some compilers will instantiate global templates rather than
        use overloaded class member functions.

 ***/

#endif /* 0 for comments */
/* The next line is used to provide a default (tagless) header block */
#undef RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK

/* The next several lines #define constants for later use */
/*** For RW_MESSAGE ***/
#define RW_NOMSG    0x00
#define RW_CATGETS  0x01
#define RW_GETTEXT  0x02
#define RW_DGETTEXT 0x03

/* The next part of this file, using pre-defined macros provided by
 * your compiler, defines some macros for compiler limitations or
 * vagaries that are hard or impossible to test. You may decide that
 * your version of a given compiler no longer suffers from a
 * particular limitation. If so, feel free to change the macro. Be
 * aware, however, that leaving hese macros defined will usually not
 * reduce the general utility of our libraries, although they may
 * cause us to generate code that is less than perfectly optimal.
 */

/*
** hand coded macros for compilers that define a useful macro "name"
*/

/*************************** MetroWerks *******************************/
#if defined( __MWERKS__ )
# define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK  1
# define RW_NO_POSIX_RE                       1
# if defined( macintosh )
#  define RW_NO_GLOBAL_TZ                     1
#  define RW_NO_GETTIMEOFDAY                  1
#  define RW_NO_XDR                           1
#  define RW_FRIEND_THEN_INLINE               1
#  define RW_NO_OVERLOAD_WCHAR                1
#  define RW_NO_WSTR                          1
#  define RW_NO_XMSG                          1
#  define NO_STREAMBUF_STOSSC                 1
#  define NO_NATIVE_GENERIC_H                 1
#  define SYNC_IS_NON_PUBLIC                  1
#  define RW_NOT_POSIX_FSTAT                  1
#  define RW_CRLF_CONVENTION                  1
#  define RW_DIRECTORY_WO_SLASHES             1
#  define RW_COMPILE_INSTANTIATE              1 /* jvd */
#  define RW_NOT_TIME_SINCE_1_1_70            1 /* jvd */
#  define RW_BROKEN_TEMPLATES                 1 /* jvd */   
#  define RW_NO_IOSSTREAM                     1 
#  define RW_NO_NEW_HEADER                    1
#  define RW_NO_LOCALE                        1
#  define RW_NO_POSIX_RE                      1
#  define RW_USE_PUBSEEKOFF                   1
#  define RW_NO_UNBUFFERED                    1
#  define RW_FAIL_ON_EOF                      1 /* djk */
#  define RW_NO_STL                           1 /* djk */
#  define RW_BROKEN_ACCESS_ADJUST             1 /* djk: typedef to base_class issues */
# endif /* macintosh */
#endif  /* __MWERKS__ */


/*************************** Symantec *******************************/
/*
 * No longer supports Zortech.  Must have Symantec V6.0 or greater.
 */
#if defined(__SC__)

/* For Macs ************/
#  if defined(macintosh)
# define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
# define RW_NO_POSIX_RE                 1
#    define RW_NO_GLOBAL_TZ             1
#    define RW_NO_GETTIMEOFDAY          1
#    define RW_NO_XDR                   1
#    define RW_FRIEND_THEN_INLINE       1
#    define RW_NO_OVERLOAD_WCHAR        1
#    define RW_NO_WSTR                  1
#    define RW_NO_XMSG                  1
#    define RW_ACCESS_NON_CONST         1 
#    define RW_UNLINK_NON_CONST         1
#    define RW_NO_STL                   1
#    define RW_NOT_POSIX_FSTAT          1
#    define RW_CRLF_CONVENTION          1
#    define RW_NO_EXCEPTIONS            1
#    define RW_COMPILE_INSTANTIATE      1
#    define RW_NO_LOCALE                1
#    define RW_NO_POSIX_RE              1
#    define RW_NOT_TIME_SINCE_1_1_70    1    /* jvd */
/* Some environments (with Symantec C++ for Power Macintosh v8.0.4
 * being a specific example) cannot completely handle the use of
 * static destructors in RWCollectable-derived classes in cleaning up
 * the RWFactory instance object theFactory. Specifically, the
 * RWStringID instance object used to remove the RWStringIDAssociation
 * is incorrectly created and causes a bus error. As a work-around for
 * this problem in this environment, RW_BAD_STATIC_DESTRUCTORS is
 * defined. When this option is set, the RWStringIDAssociation is not
 * removed. Instead, the library will rely on the fact that RWFactory
 * will clean itself out in its destructor and that the application
 * heap is released by the Macintosh operating system when the
 * application is exiting (which is very imminent when static
 * destructors are being called).  
 *     - jjszucs
 */
#    define RW_BAD_STATIC_DESTRUCTORS     1 /* jjszucs */
#    define RW_DIRECTORY_WO_SLASHES       1
#    define RW_NO_NEW_HEADER              1
#    define RW_HEADER_WITH_EXTENSION      1 /* jvd */
#    define RW_FAIL_ON_EOF                1 /* djk */
#    define RW_GLOBAL_BEFORE_CLASS_SCOPE  1

/* For PCs ************/
#  else
#    define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK 1
#    define RW_NO_POSIX_RE                 1
#    if defined(__NT__)
#      define __WIN32__ 1
#    endif
#    define __MSDOS__                1
#    define RW_NO_STL                1
#    define RW_NO_OVERLOAD_WCHAR     1
#    define RW_NO_FRIEND_INLINE_DECL 1
#    define RW_COMPILE_INSTANTIATE   1
#    define RW_NO_WSTR               1
#    define RW_NO_XDR                1
#    define RW_NO_XMSG               1
#    define RW_NOT_TIME_SINCE_1_1_70 1
#    define RW_TIME_SINCE_1_1_68     1
#    if defined(_RWTOOLSDLL)
#      define RW_DONT_USE_MEMPOOL      1
#    endif
#  endif /*mac else PC */
#endif /*Symantec*/

/********************** Borland **************************/
#if defined(__TURBOC__)
#  define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#  define RW_NO_XDR                     1
#  define RW_NO_POSIX_RE                1
#  define RW_NO_XMSG                    1
#  define RW_COMPILE_INSTANTIATE        1
#  if defined(__MSDOS__) && defined(_Windows)
#    define __WIN16__ 1
#  endif
#  if defined(__MSDOS__)
#    define RW_NO_STL 1
#  endif
/* __export in a function declaration must follow the return type */
#  define RW_TRAILING_RWEXPORT 1
/* Borland won't inline code that contains "while" or "for" loops or
 * that generates temporaries requiring destructors
 */
#  define RW_NO_INLINED_WHILES           1
#  define RW_NO_INLINED_FORS             1
#  define RW_NO_INLINED_TEMP_DESTRUCTORS 1


#  ifndef name2
#    define name2 _Paste2
#  endif

#  if __TURBOC__ <= 0x0295                      /* Turbo C++ V 1.00 */
#    define RW_UNDEFINED_REFERENCE_BUG  1
#    define RW_NO_STL 1
#  endif
#  if __TURBOC__ >=0x200        /* Turbo C++ V1.01 and above */
#    if __TURBOC__ < 0x300 /* Borland V2.0 */
#      define RW_BCC_INLINE_DESTRUCTOR_BUG 1
#      define RW_BCC_STRUCT_POINTER_BUG    1
#      define RW_TCC_DELETE_SIZE_BUG    1
#    endif      /* end Borland C++ V2.0 */
#    if __TURBOC__ < 0x400
#      define RW_NO_TEMPLATES 1
#    endif
#    if __TURBOC__ < 0x451                      /* prior to V 4.0 */
#     define RW_NO_EXCEPTIONS         1
#     define RW_NO_FRIEND_INLINE_DECL 1
#     define RW_NO_OVERLOAD_WCHAR     1
#     define RW_NO_OVERLOAD_SCHAR     1
#     define RW_NO_STL 1
#    endif
#    if __BORLANDC__ < 0x453                    /* Borland 4.0 */
     /* The following are provided, but are not locale-correct */
#      undef RW_NO_STRICMP
#      undef RW_NO_STRNICMP
#      define RW_NO_STRICMP  1
#      define RW_NO_STRNICMP 1
#    endif
#    if __TURBOC__ > 0x459
#      if defined(__WIN32__)
#        define RW_DOUBLE_FLUSH_ON_CLOSE 1
#      endif
#      define RW_DOUBLE_UNDERBAR_POINTER_QUALIFIERS 1
#    endif
#    if __TURBOC__ < 0x461
#      define RW_NO_LEADING_UNDERSCORE       1
#      define RW_NO_FRIEND_INLINE_TMPL_DECL 1
#    endif
#    ifdef __MT__
#      define RW_MULTI_THREAD 1
#    endif
#    ifdef __WIN32__
#       define RW_SUPPLY_WSTR 1
#       define RW_NO_WCSXFRM  1
#    else
#       define RW_NO_WSTR     1
#       undef RW_NO_STL
#       define RW_NO_STL 1
#    endif
#    if __TURBOC__ >= 0x500
#      define RW_NO_EXPRESSION_TEMPLATES 1
#      define RW_NO_TZSET_UNDERSCORE     1
#      define RW_GLOBAL_BEFORE_CLASS_SCOPE
#    endif
#  endif
#endif  /* __TURBOC__ */
/************************ Microsoft C/C++ *****************************/
#if defined(_MSC_VER)
#  define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#  define RW_NO_XDR             1
#  define RW_NO_POSIX_RE        1
#  define RW_NO_REF_TO_REF      1
#  define RW_NO_XMSG            1
#  define RW_MSC_BACKEND        1
#  if defined(_MSDOS) && !defined(WIN32) && !defined(_WIN32)
#    define __MSDOS__   1
#    if defined(_WINDOWS)
#      define __WIN16__ 1
#    endif
#  endif
#  if defined(WIN32) || defined(_WIN32)
#    define __WIN32__
#    define RW_TOLOWER_SIGN_EXTENDS_RESULT_BUG 1
#  else
#    define RW_NO_WSTR 1
#  endif
#  if (_MSC_VER < 900)
#    define RW_NO_STL            1
#    define RW_MSC_BACKEND       1
#    define RW_NO_EXCEPTIONS     1
#    define RW_NO_OVERLOAD_WCHAR 1
#    define RW_NO_TEMPLATES      1
#  else
#    if (_MSC_VER < 1000)
#      define RW_NO_STL              1
#      define RW_MSC_BACKEND         1
#      define RW_COMPILE_INSTANTIATE 1
#      define RW_NO_OVERLOAD_WCHAR   1
#    else
#      define RW_MSC_BACKEND         1
#      define RW_NO_OVERLOAD_WCHAR   1
#      define RW_COMPILE_INSTANTIATE 1
#      define  RW_GLOBAL_BEFORE_CLASS_SCOPE
#      ifndef RW_NO_STL
/* This is to work around a bug in V1.1 of Rogue Waves Standard Library */
#        include<utility>
#        undef RWSTD_NO_DESTROY_NONBUILTIN
#      endif
#    endif
#  endif
#  ifdef _RWBUILDDLL
#    define __DLL__ 1
#  endif
#  ifdef _M_I86SM
#    define __SMALL__ 1
#  endif
#  ifdef _M_I86CM
#    define __COMPACT__ 1
#  endif
#  ifdef _M_I86MM
#    define __MEDIUM__ 1
#  endif
#  ifdef _M_I86LM
#    define __LARGE__ 1
#  endif
#  ifdef _MT
#    define RW_MULTI_THREAD 1
#  endif
#endif
/************************** WATCOM C/C++ ******************************/
#ifdef __WATCOMC__
#  define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#  define RW_NO_XDR             1
#  define RW_NO_POSIX_RE        1
#  define RW_NO_STL 1
#  define RW_NO_XMSG 1
#  define RW_NO_TEMPLINST_ON_BASE 1
#  define RW_NO_WSTR 1
#  define RW_NO_LEADING_UNDERSCORE 1
#  define RW_NO_NESTED_QUOTES 1
#  define RW_COMPILE_INSTANTIATE 1
/* Watcom cannot always handle high precision (RW_DEFAULT_PRECISION) */
#  define RW_DEFAULT_PRECISION 10
#  if defined(__DOS__) && !defined(__MSDOS__)
#    define __MSDOS__ 1
#  endif
#  if defined(__WINDOWS__) || defined(__NT__)
#    if defined(__WINDOWS_386__) || defined(__NT__)
#      define __WIN32__ 1
#    else
#      define __WIN16__ 1
#    endif
#  endif
#endif
/********************** Metaware High C/C++ ***************************/
#if defined(__HIGHC__)
#  define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#  define RW_NO_XDR             1
#  define RW_NO_POSIX_RE        1
#  define RW_NO_STL             1
#  define RW_NO_OVERLOAD_WCHAR  1
#  define RW_NO_XMSG            1
#  if defined(_MSDOS)
#    define __MSDOS__ 1
#    define RW_HIGHC_INLINE_BUG  1
#    define RW_NO_EXCEPTIONS     1
#    define RW_NO_WSTR 1
#  endif
#  if defined(_OS2)
#    undef __OS2__
#    define __OS2__ 1
#    define RW_SUPPLY_WSTR 1
#    define RW_NO_WCSXFRM  1
#    ifdef _REENTRANT
#      define RW_MULTI_THREAD 1
#    endif
#  endif
#  if defined(_MSNT)
#    undef __WIN32__
#    define __WIN32__ 1
#    if defined(_REENTRANT) || defined(_MT)
#      define RW_MULTI_THREAD 1
#    endif
#  endif
#endif  /* __HIGHC__ */

/********************** IBM C++ *********************************/
#if defined(__IBMCPP__) && !defined(_AIX)
#  define RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#  define RW_IOS_XALLOC_BROKEN   1
#  define RW_NO_POSIX_RE         1
#  define RW_NO_XMSG             1
#  define RW_NO_XDR              1
#  define RW_NO_STL              1
#  define RW_COMPILE_INSTANTIATE 1 
#  ifdef __MULTI__
#    define RW_MULTI_THREAD      1
#  endif
#endif

/********************************************************************
 *** Exactly one of the following blocks of code should be entered.
 *** If your compiler #define's some individual tag (such as _MSC_VER
 *** or __BORLANDC__, for example), then there should be a block in 
 *** this file which will be entered if your compiler is being used.
 *** If your compiler does not provide such a tag (as is the case 
 *** with most unix-hosted compilers), _and_ if you have configured 
 *** for more than one platform, then we will ask you to to provide 
 *** an individual tag at configuration time, and you will have to
 *** compile with that tag #define'd in order to enforce entry into
 *** the appropriate block of code.
 ********************************************************************/
#ifndef RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK
#endif /* RW_NEVER_ENTER_DEFAULT_HEADER_BLOCK */


/********************* misc adjustments, extras *********************/
/**** YOU SHOULD NOT NEED TO EVER TOUCH ANYTHING BELOW THIS LINE ****/
/********************************************************************/
/* No way to test Glockenspiel for this bug */
#if defined(__GLOCK__)
#  define RW_CONST_EMIT_BUG 1
#endif

/* some IBM xlC don't #define unix */
#if defined(_AIX) && !defined(unix)
#define unix 1
#endif

/* No Pi for these compilers: */
#if defined(RW_MSC_BACKEND) || defined(__OREGON__) || defined(__HIGHC__) || defined(applec) || defined(CII) || defined(__WATCOMC__)
#  ifndef M_PI
#    define M_PI 3.14159265358979323846
#  endif
#endif

#ifdef RW_GLOBAL_ENUMS
#  define RWSCOPE(a)
#  define RWVECTOR_DELETE(i) delete[i]
#else
#  define RWSCOPE(a) a::
#  define RWVECTOR_DELETE(i) delete[]
#endif

#ifdef RW_WRAP_C
#  define STARTWRAP     extern "C" {
#  define ENDWRAP       }
#else
#  define STARTWRAP
#  define ENDWRAP
#endif

#if defined __ATT1__ || __ATT2__ || defined __ATT3__
#  define __ATT__ 1
#endif

#if defined(__MSDOS__) || defined(__OS2__) || defined(__WIN32__) || defined(__NT__) || defined(__WINDOWS__)
#  define RW_CRLF_CONVENTION 1
#endif

#ifndef RW_BYTES_PER_PTR
# if defined(__SMALL__) || defined(__MEDIUM__)
#   define RW_BYTES_PER_PTR 2
# else
#   define RW_BYTES_PER_PTR 4   /* Assume 32 bit pointers */
# endif
#endif
#ifndef RW_BYTES_PER_WORD
# if defined(__SMALL__) || defined(__COMPACT__) || defined(__MEDIUM__) || defined(__LARGE__)
#   define RW_BYTES_PER_WORD 2
# else
#   define RW_BYTES_PER_WORD 4  /* Assume 32 bit words */
# endif
#endif

#ifndef RW_DEFAULT_PRECISION
#   define RW_DEFAULT_PRECISION 16      /* Assume standard IEEE format */
#endif

/** safety check **/
#if defined(RW_NO_THREADS) && defined(RW_MULTI_THREAD)
error can't have RW_MULTI_THREAD with RW_NO_THREADS #define'd
#endif 

/** Must use MT-safe version of stdlib with MT-safe Tools.h++ **/
#if defined(RW_MULTI_THREAD) && !defined(RWSTD_MULTI_THREAD)
#define RWSTD_MULTI_THREAD 1
#endif

#if 0
/*
 ****************************************************************************
 * $Log: compiler.h,v $
 * Revision 1.2  1996/04/09 18:48:24  rohde
 * 7.0
 *
 * Revision 7.86  1996/03/22 20:56:09  griswolf
 * Fix Typo.
 *
 * Revision 7.85  1996/03/19 20:59:32  jims
 * Remove '//' style comments for use with C-langauge (.c) source files
 *
 * Revision 7.84  1996/03/16 15:29:23  jims
 * Port non-stdlib persistence to DEC and Watcom
 *
 * Revision 7.83  1996/03/15 20:08:39  kyle
 * Removed RW_TIME_DATE_NEEDS_GE
 *
 * Revision 7.82  1996/03/15 17:24:23  kyle
 * Added RW_TIME_DATE_NEEDS_GE which was needed in colltime.h and
 * colldate.h.  It adds operator>= in those two classes.
 *
 * Revision 7.81  1996/03/15 17:19:18  kyle
 * changed Millenium's RW_GLOBL_SCP_BUG and RW_BLBL_SCP_BUG2 to
 * RW_GLOBAL_BEFORE_CLASS_SCOPE for Symantec which, like Borland 5,
 * chooses global scope.
 *
 * Revision 7.80  1996/03/15 01:56:15  griswolf
 * Patch around Microsoft no reference to reference complaint.
 *
 * Revision 7.79  1996/03/15 00:39:43  kyle
 * Updated for Macintosh compilers.
 *
 * Revision 7.78  1996/03/14 00:16:48  jims
 * define RW_NO_STL for IBM on OS/2
 *
 * Revision 7.77  1996/03/07 23:48:26  pearson
 * Change Watcom DLL's over to __declspec(__dllexport) Microsoft syntax
 *
 * Revision 7.76  1996/03/07 19:54:15  kevinj
 * Bug #2684: Added RW_GLOBAL_BEFORE_CLASS_SCOPE to make inline
 * specialized template operator<= for times when <utility>'s
 * templatized operator<= is used instead of
 * the class member operator<=.
 *
 * Revision 7.75  1996/03/06 19:31:39  jims
 * Make sure RWSTD_MULTI_THREAD is defined whenever RW_MULTI_THREAD is
 * defined
 *
 * Revision 7.74  1996/03/02 00:35:16  jims
 * Add RW_NO_XDR to PC compilers
 *
 * Revision 7.73  1996/02/26 21:35:31  griswolf
 * Scopus 2577. If your compiler is CFront and we don't know it, we may
 * die.
 *
 * Revision 7.72  1996/02/22 18:29:38  pearson
 * OS2 supports wide characters
 *
 * Revision 7.71  1996/02/20 20:10:17  pearson
 * No XDR streams for OS2,  Visual Age C++ uses compile time instantiation
 *
 * Revision 7.70  1996/02/20 15:53:13  pearson
 * Propagate RW_COMPILE_INSTANTIATE into new config for Watcom
 *
 * Revision 7.69  1996/02/19 16:46:56  pearson
 * Create RW_NO_TZSET_UNDERSCORE to make zone.cpp compatable with Borland 5.0
 *
 * Revision 7.68  1996/02/18 01:37:35  griswolf
 * Replace tabs with spaces, per Rogue Wave standard.
 *
 * Revision 7.67  1996/02/16 19:45:36  pearson
 * Fixed typo
 *
 * Revision 7.66  1996/02/16 00:06:05  jims
 * No longer use macro RW_INSTANTIATES_ALL_TEMPLATE_METHDOS
 *
 * Revision 7.65  1996/02/14 20:19:18  griswolf
 * Move the configurable blocks below PC hand-coded blocks. Add $Revision: 1.2 $.
 *
 * Revision 7.64  1996/02/14 01:04:37  griswolf
 * Make Borland always RW_COMPILE_INSTANTIATE
 *
 * Revision 7.63  1996/02/13 17:28:09  griswolf
 * Make sure RW_NO_XMSG is in all PC blocks.
 *
 * Revision 7.62  1996/02/12 23:08:01  griswolf
 * Add RW_NO_POSIX_RE to each PC platform's block.
 *
 * Revision 7.61  1996/02/12 21:26:45  pearson
 * Include utility instead of stdcomp.h to decouple STD_LIB from Tools.
 *
 * Revision 7.60  1996/02/12 21:20:02  pearson
 * Workaround for a bug in Rogue Wave's Standard Library:
 * RWSTD_NO_DESTROY_NONBUILTIN does not work with MSVC 4.0
 *
 * Revision 7.59  1996/02/12 17:05:53  sanders
 * Add more PC defaults back.
 *
 * Revision 7.58  1996/02/08 16:26:38  griswolf
 * fix typo.
 *
 * Revision 7.57  1996/02/05 21:05:01  griswolf
 * Add back various PC pre-configurations.
 *
 * Revision 7.56  1996/02/05 18:23:13  griswolf
 * Set __ATT__ based on __ATTn__
 *
 * Revision 7.55  1996/02/01 18:47:28  griswolf
 * fix typo in default-block macro.
 *
 * Revision 7.54  1996/02/01 16:24:05  griswolf
 * Fix some typos.
 *
 * Revision 7.53  1996/02/01 00:18:45  griswolf
 * Fix bug in macintosh/symantec configuration block.
 *
 * Revision 7.52  1996/01/29 23:22:35  griswolf
 * Add new macros for macintosh merge.
 *
 * Revision 7.51  1996/01/29 22:03:57  griswolf
 * Undo RW_NO_STDCONTAINER --> RW_NO_STL
 *
 * Revision 7.50  1996/01/22 22:09:25  griswolf
 * Massively revised to accept the "blocky" configuration format.
 *
 * Revision 7.49  1996/01/19 13:19:10  jims
 * Create THREAD and EXCEPTION groupings in Unix section
 *
 * Revision 7.48  1996/01/18 22:51:47  jims
 * Add macro for non-standard ANSI C time()
 *
 * Revision 7.47  1996/01/05 23:29:59  jims
 * Fix RWMESSAGE to read RW_MESSAGE
 *
 * Revision 7.46  1996/01/03 16:58:23  pearson
 * Watcom requires a trailing export qualifier
 *
 * Revision 7.45  1995/12/15 18:15:15  pearson
 * Define new macro RW_NO_FRIEND_INLINE_TMPL_DECL to accomodate problem with
 * inline friend operators with templated arguments ... the compiler generates
 * operators with a (... argument signature causing multiple definitions to be
 * present at link time
 *
 * Revision 7.44  1995/12/04 16:41:27  pearson
 * Fix comment bugs
 *
 * Revision 7.43  1995/12/01 18:55:13  pearson
 * set RW_DONT_USE_MEMPOOL for Symanted DLL's
 *
 * Revision 7.42  1995/11/23 00:33:03  pearson
 * Remove no threads restriction from Symantec
 *
 * Revision 7.41  1995/11/15 16:16:39  pearson
 * Flag Symantec C++ with RW_NO_FLAGS
 *
 * Revision 7.40  1995/11/14 22:01:42  pearson
 * Make thread defines more comprehensive
 *
 * Revision 7.39  1995/11/14 18:48:28  pearson
 * Add macro RW_POSIX_D10_THREADS to detect support for new pthread interface
 *
 * Revision 7.38  1995/11/03 18:55:43  jims
 * define RW_NO_XDR for non-unix systems
 *
 * Revision 7.37  1995/11/01  21:02:36  pearson
 * Added RW_POSIX_THREADS to compiler dependent defines, it is already
 * used in mutex.h and instmgr.h
 *
 * Revision 7.36  1995/11/01  16:36:25  pearson
 * Define RW_COMPILE_INSTANTIATE for Symantec
 *
 * Revision 7.35  1995/10/30  20:03:02  pearson
 * Update to Symantec section, removed old stuff.
 *
 * Revision 7.34  1995/10/27  19:18:11  pearson
 * Define __WIN32__ for Symantec C++ 7.0
 *
 * Revision 7.33  1995/10/27  18:01:30  pearson
 * Fixed misplaced #endif - was using Symantec DOS defines for unix
 *
 * Revision 7.32  1995/10/26  00:00:28  pearson
 * Add definitions for port of Tools to Symantec C++ 7.0
 *
 * Revision 7.31  1995/10/23  23:31:50  jims
 * define RW_INSTANTIATES_ALL_TEMPLATE_METHDOS for Microsoft
 *
 * Revision 7.30  1995/10/20  20:25:09  pearson
 * Added check for Watcom's predefined macro __NT__
 *
 * Revision 7.29  1995/10/20  06:53:32  jims
 * port to msvc 4.0
 *
 * Revision 7.28  1995/10/17  21:06:13  pearson
 * Revise for port of Tools 7.0 to Watcom C++ 10.5
 *
 * Revision 7.27  1995/10/02  19:37:41  pearson
 * Remove RW_BROKEN_TEMPLATE_TYPEDEFS from Borland 5.0 section, second
 * beta release fixed the problem.
 *
 * Revision 7.26  1995/10/02  17:59:04  pearson
 * Removed BC5_MULT_DECL_BUG variable, no longer needed
 *
 * Revision 7.25  1995/09/25  18:39:40  pearson
 * Added RW_DOUBLE_FLUSH_ON_CLOSE to accomodate BORLAND 4.5 bug in NT where
 * a write to a read-only file causes fclose to fail.  A fflush before close
 * fixes this.
 *
 * Revision 7.24  1995/09/11  22:15:21  pearson
 * Never edit one of these files from DOS!  Fixed ^M disaster.
 *
 * Revision 7.23  1995/09/08  19:13:03  pearson
 * Changed RW_NO_TEMPLATE_TYPEDEF_IN_OUT_OF_LINE_ARG_LIST to
 *         RW_BROKEN_TEMPLATE_TYPEDEFS 
 *
 * Revision 7.22  1995/09/07  22:08:27  pearson
 * Added defines to work around problems with the beta version of the
 * Borland 5.0 compiler (search on 0x500)
 *
 * Revision 7.21  1995/09/05  18:21:38  jims
 * Use new copyright macro
 *
 * Revision 7.19  1995/09/01  21:45:52  kevinj
 * #2135: Standard Library packaging for beta.
 *
 * Revision 7.18  1995/08/29  17:20:13  jims
 * define RW_NO_STL for 16 bit Borland
 *
 * Revision 7.17  1995/08/29  16:38:19  kevinj
 * Moved RWWRExpr to ToolsPro.
 *
 * Revision 7.16  1995/08/21  04:08:43  jims
 * #defined RW_NO_STL for Microsoft
 *
 * Revision 7.15  1995/08/18  17:24:48  pearson
 * #1876 Removed default to RW_NO_STL if __ATT__, in favor of test for STL
 * compatability which was added to ./bin/config
 *
 * Revision 7.14  1995/06/27  23:40:49  kevinj
 * #1876: Added RW_NO_STL to guard against futile compilation of classes
 * that depend on the existence of C++ Standard Library containers
 * and iterators.
 *
 * Revision 7.13  1995/06/27  01:03:15  kevinj
 * #1892: Added RW_DOUBLE_UNDERBAR_POINTER_QUALIFIERS to take care of case
 * where BC++ 4.5 under certain mysterious circumstances is unable to compile:
 * typedef void(far *foo)(const int&)
 * but is able to compile:
 * typedef void(__far *foo)(const int&)
 *
 * Revision 7.12  1995/06/23  19:21:50  griswolf
 * Scopus #1077: Fix the set-number we pass to catgets() in message.cpp
 * to work with Solaris by adding a macro to compiler.h so others may
 * change it as needed.
 *
 * Revision 7.11  1995/06/15  18:01:33  griswolf
 * Add macro RW_NOT_POSIX_FSTAT. Related to scopus bug 1067.
 *
 * Revision 7.10  1995/06/08  20:42:32  sanders
 * Scopus ID# 993.  Part of this bug was fixed in config, but there
 * was yet another place where RW_COMPILE_INSTANTIATE was set in
 * compiler.h.  This was set #if defined(__xlC__)  that define
 * was removed so that it can now be link instantiate.
 *
 * Revision 7.9  1995/04/18  08:24:22  jims
 * Scopus ID# 1084: Added section for MSVC 2.x.
 *
 * Revision 7.8  1995/02/02  00:09:56  griswolf
 * added RW_INSTANTIATE_ALL_TEMPLATE_METHODS
 *
 * Revision 7.7  1995/01/18  20:48:16  kevinj
 * RW_NO_EXPRESSION_TEMPLATES
 *
 * Revision 7.6  1994/12/22  19:47:42  kevinj
 * Added RW_NO_POSIX_RE
 *
 * Revision 7.5  1994/12/13  22:53:31  kevinj
 * RW_NO_TEMPLATE_TYPEDEF_IN_OUT_OF_LINE_ARG_LIST added.
 *
 * Revision 7.1  1994/10/16  03:31:29  josh
 * Merged 6.1 and 7.0 development trees
 *
 * Revision 6.20  1994/07/19  19:11:50  foote
 * Added RW_NO_OSTR_REF_CAST macro
 *
 * Revision 6.19  1994/07/19  18:47:14  foote
 * defined RW_NO_ACCESS_ADJUSTMENT for applec
 *
 * Revision 6.18  1994/07/18  03:07:36  jims
 * Expand section for HIGHC to include OS/2 and NT settings
 *
 * Revision 6.17  1994/07/14  06:07:28  jims
 * Lower DEFAULT_PRECISION for Watcom to 10
 *
 * Revision 6.16  1994/07/13  17:40:55  nevis
 * Changed default precision to 16 match vriezen's change to
 * toolread.doc and to duck a Watcom bug.
 *
 * Revision 6.15  1994/07/12  23:11:07  nevis
 * MetaWare section now checks for OS/2.
 *
 * Revision 6.14  1994/07/12  21:19:30  nevis
 * Fixed typo in MetaWare section: WR_NO_WSTR instead of RW_NO_WSTR
 *
 * Revision 6.13  1994/07/12  19:58:19  vriezen
 * Update Copyright notice
 *
 * Revision 6.12  1994/07/07  22:15:36  nevis
 * Watcom cannot handle default precision of 17...changed to 16...
 *
 * Revision 6.11  1994/06/29  00:14:50  vriezen
 * Set RW_NO_OVERLOAD_SCHAR for Borland 3.1
 *
 * Revision 6.10  1994/06/22  21:35:07  vriezen
 * Add RW_NO_OVERLOAD_SCHAR
 *
 * Revision 6.9  1994/06/16  00:55:07  vriezen
 * Bug #54.  Enhance precision support for RWpostream
 *
 * Revision 6.8  1994/06/06  20:48:34  nevis
 * Added RW_NO_THROW_WITH_SHARED for compilers which cannot
 * throw exceptions from a shared library.
 *
 * Revision 6.7  1994/06/02  22:56:23  foote
 * Defined RW_NO_FRIEND_INLINE_DECL for Symantec 7.0 on Macintosh
 *
 * Revision 6.6  1994/06/02  17:58:04  foote
 * Fixed typo in last revision
 *
 * Revision 6.5  1994/06/02  17:45:10  foote
 * Port to Symantec 7.0 on Macintosh
 *
 * Revision 6.4  1994/05/16  18:06:58  jims
 * Port to Win32 DLL
 *
 * Revision 6.3  1994/05/09  20:02:50  vriezen
 * Added RW_DOUBLE_PRECISION
 *
 * Revision 6.2  1994/05/06  01:58:38  vriezen
 * Add RW_NO_NESTED_QUOTES to Watcom  Bug #385
 *
 * Revision 6.1  1994/04/15  18:40:32  vriezen
 * Move all files to 6.1
 *
 * Revision 2.58  1994/04/01  14:13:33  vriezen
 * Add RW_NO_WSTR to Metaware
 *
 * Revision 2.57  1994/03/10  23:22:58  jims
 * Add RW_IOS_XALLOC_BROKEN to IBMCPP (C Set ++)
 *
 * Revision 2.56  1994/03/04  07:41:30  jims
 * Set __WIN32__ / __WIN16__ for Watcom; make sure RW_CRLF_CONVENTION
 * defined for Watcom
 *
 * Revision 2.55  1994/01/11  05:05:54  myersn
 * make IBM xlC #define unix as do all other unices.
 *
 * Revision 2.54  1993/12/10  20:50:58  jims
 * Add ObjectStore compiler to those with compile-time instantiation
 *
 * Revision 2.53  1993/11/16  08:39:22  myersn
 * add OS/2 multithread flag
 *
 * Revision 2.52  1993/11/03  23:27:03  jims
 * define RW_REVERSED_CR_AND_LF for MPW
 * add __WATCOMC__ to list of compilers that don't define pi
 *
 * Revision 2.51  1993/09/23  21:33:50  alv
 * added CII (computer innovations) to list of compilers without M_PI
 *
 * Revision 2.50  1993/09/21  17:28:21  dealys
 * corrected MPW flags
 *
 * Revision 2.49  1993/09/16  18:48:13  keffer
 * Fine tuned support for Symantec V6.0.
 *
 * Revision 2.48  1993/09/16  05:53:10  keffer
 * MS Visual C++ does not need RW_NO_LEADING_UNDERSCORE
 *
 * Revision 2.47  1993/09/16  01:28:21  keffer
 * Corrected typo in comment.
 *
 * Revision 2.46  1993/09/16  00:36:56  keffer
 * Added RW_NO_XDR
 *
 * Revision 2.45  1993/09/15  20:47:16  keffer
 * Added RW_STRUCT_TM_TZ.
 *
 * Revision 2.44  1993/09/14  17:44:40  keffer
 * Added support for Symantec C/C++ V6.0
 *
 * Revision 2.43  1993/09/14  06:09:59  myersn
 * add flags for IBM C/Set++
 *
 * Revision 2.42  1993/09/13  16:38:41  keffer
 * Added support for WATCOM C/C++ 32 V9.5
 *
 * Revision 2.41  1993/09/13  09:17:40  myersn
 * add support for RW_NO_LEADING_UNDERSCORE.
 *
 * Revision 2.40  1993/09/10  22:38:56  jims
 * Adjust __TURBOC__ value to match latest release of Borland
 *
 * Revision 2.39  1993/09/10  17:46:02  jims
 * Add define for Microsoft tolower sign extension of result bug
 *
 * Revision 2.38  1993/09/07  18:17:37  jims
 * MSC now defines _WIN32 under Windows NT
 *
 * Revision 2.37  1993/08/31  19:47:19  keffer
 * Now detects __GNUG__ instead of __GNUC__.
 *
 * Revision 2.36  1993/08/20  03:15:32  keffer
 * The macro RW_COMPILE_INSTANTIATE is now set by config.
 *
 * Revision 2.35  1993/08/11  00:36:51  myersn
 * set RW_NO_STRFTIME_CAPC according to environment (i.e. Sun or not).
 *
 * Revision 2.34  1993/08/05  11:38:36  jims
 * Adjust Borland and Microsoft sections for DOS / Windows / NT
 *
 * Revision 2.33  1993/08/03  21:55:16  dealys
 * Ported to MPW C++ 3.3
 *
 * Revision 2.32  1993/08/03  21:44:29  jims
 * *** empty log message ***
 *
 * Revision 2.31  1993/07/30  03:20:34  jims
 * Port to MS C7
 *
 * Revision 2.30  1993/07/29  11:19:29  jims
 * Reorganize non-unix compiler section
 *
 * Revision 2.29  1993/07/15  10:37:40  jims
 * Set RW_NO_STR(N)ICMP for Borland 4.0
 *
 * Revision 2.28  1993/06/13  22:05:55  jims
 * Port to Borland 4.0 including check for __WIN32__
 *
 * Revision 2.27  1993/06/06  00:16:44  keffer
 * Introduced RW_BYTES_PER_PTR and RW_BYTES_PER_WORD
 *
 * Revision 2.26  1993/05/19  23:11:19  keffer
 * Added RW_NO_XMSG
 *
 * Revision 2.25  1993/04/13  03:17:35  myersn
 * add RW_IOS_XALLOC_BROKEN flag, restore RW_HIGHC_INLINE_BUG flag.
 *
 * Revision 2.25  1993/03/15  18:39:34  alv
 * added RW_HIGHC_INLINE_BUG
 *
 * Revision 2.24  1993/03/15  18:25:14  keffer
 * Added 'RW_' prefix to remaining macros.
 *
 * Revision 2.23  1993/03/01  17:53:00  alv
 * ported to Metaware High C++
 *
 * Revision 2.22  1993/02/13  23:07:51  keffer
 * Corrected syntax error.
 *
 * Revision 2.21  1993/02/13  22:21:26  keffer
 * Zortech V3.0
 *
 * Revision 2.20  1993/02/12  20:05:34  keffer
 * Added g++ to the list of compilers that do compile-time instantiation.
 *
 * Revision 2.19  1993/02/11  23:58:28  keffer
 * Added the IBM xlC compiler to the list of compilers that do
 * compile time instantiation.
 *
 * Revision 2.18  1993/01/29  22:30:26  alv
 * RW_CRLF_CONVENTION now true for OS2 as well as DOS
 *
 * Revision 2.17  1993/01/27  03:43:49  keffer
 * Added macro RW_COMPILE_INSTANTIATE for compilers that do
 * template instantiation at compile time.
 *
 * Revision 2.16  1993/01/26  23:55:49  alv
 * Wrapped Log in #if 0 ... #endif to avoid problems due to log
 * entries containing cpp macros or comments
 *
 * Revision 2.15  1993/01/26  23:30:45  keffer
 * The macro __ATT?__ now set.
 *
 * Revision 2.14  1993/01/26  02:05:57  keffer
 * RW_NO_OVERLOAD_WCHAR now defined to 1 instead of nothing.
 *
 * Revision 2.13  1993/01/25  22:04:11  keffer
 * Changed comments for RW_NO_CLOCK.
 *
 * Revision 2.12  1993/01/25  18:13:56  keffer
 * RW_NO_CONST_OVERLOADS->RW_NO_CONST_OVERLOAD
 *
 * Revision 2.10  1993/01/22  18:25:41  alv
 * Fixed so that macros only get #define'd, not #undef'd in the
 * bottom half of the file.
 *
 * Revision 2.8  1992/12/01  04:11:37  myersn
 * undefined RW_NO_OVERLOAD_WCHAR for Gnu gcc
 *
 * Revision 2.7  1992/11/30  23:13:36  myersn
 * change RW_NO_WCHAR_OVERLOAD to RW_NO_OVERLOAD_WCHAR
 *
 * Revision 2.6  1992/11/26  03:48:26  myersn
 * add __GNUC__ to list of compilers allowed to claim template support
 *
 * Revision 2.5  1992/11/20  02:35:01  keffer
 * Changed RW_NO_ANSI_PRINTF to RW_NO_ANSI_SPRINTF
 *         RW_NO_WCHAR_T     to RW_NO_WSTR
 *         RW_NO_WCHAR_TYPE  to RW_NO_WCHAR_OVERLOAD
 *
 * Revision 2.3  1992/11/19  04:13:08  keffer
 * Introduced new macro names.
 */
#endif /* log */
#endif /* __RWCOMPILER_H__ */
