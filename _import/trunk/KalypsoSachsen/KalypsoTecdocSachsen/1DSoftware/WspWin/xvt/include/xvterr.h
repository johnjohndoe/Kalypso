/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *  $RCSfile: xvterr.h,v $
 *  $Revision: 1.5 $
 *
 *  Purpose:  Public Error Messaging Facility interface.
 *
 ****************************************************************************/
 
#ifndef XVT_INCL_XVTERR
#define XVT_INCL_XVTERR
/*
	The following macro reduces the amount of literals generated to indicate 
	the filename in error signalling calls. If the source file declares:
		static const char xvt_source_file[] = __FILE__;
    	#define XVT_SOURCE_FILE xvt_source_file
    prior to inclusion of this file, the following macros will refer to the
	shared copy of xvt_source_file string instead of instantiating __FILE__ 
    on each invocation.
*/
#ifdef XVT_SOURCE_FILE
#define __XVTFILE__ XVT_SOURCE_FILE
#else
#define __XVTFILE__ __FILE__
#endif

/*==========================================================================
  Error Messaging Facility Interfaces
----------------------------------------------------------------------------
*/
extern void 						    XVT_CALLCONV1 
       xvt_errmsg_dispatch			    XVT_CALLCONV2
       ( 
       WINDOW              win,        /* Object on which an error occurred */
       XVT_ERRSEV          sev,        /* Error severity */
       XVT_ERRID           msg_id,     /* Error Message Id: category, number */
       const char       *  file,       /* Source file generating an error */
       unsigned short      line        /* Source file line */
       );

extern void 						    XVT_CALLCONV1 
       xvt_errmsg_dispatch_s		    XVT_CALLCONV2
       ( 
       WINDOW              win,        /* Object on which an error occurred */
       XVT_ERRSEV          sev,        /* Error severity */
       XVT_ERRID           msg_id,     /* Error Message Id: category, number */
       const char       *  file,       /* Source file generating an error */
       unsigned short      line,       /* Source file line */
       const char       *  sarg        /* String argument */
       );

extern void                            XVT_CALLCONV1
       xvt_errmsg_push_handler         XVT_CALLCONV2
       ( 
       XVT_ERRMSG_HANDLER  handler,    /* Error message handler pointer */
       DATA_PTR            context     /* Handler instance context */
       );
 
extern void                            XVT_CALLCONV1
       xvt_errmsg_pop_handler          XVT_CALLCONV2
       ( 
       XVT_ERRMSG_HANDLER  handler     /* Error message handler pointer */
       );

/* Beware, macros bracketed in #ifndef XVT_NO_PATCHES are obsolete! */

#define xvt_errmsg_sig( win, sev,  cat,  suffix, num, text )                \
		xvt_errmsg_dispatch ( win, sev, xvt_errid_create_num((cat),(num)), \
		   __XVTFILE__, (unsigned short)__LINE__ )
 
#define xvt_errmsg_sig_std( win, sev, msg_id )          \
		xvt_errmsg_dispatch ( win, sev, msg_id,     \
           __XVTFILE__, (unsigned short)__LINE__ )
#ifndef XVT_NO_PATCHES
#define xvt_errmsg_std( win, sev, msg_id )          \
		xvt_errmsg_dispatch ( win, sev, msg_id,     \
           __XVTFILE__, (unsigned short)__LINE__ )
#endif


#define xvt_errmsg_sig_std_s( win, sev, msg_id, sarg ) \
		xvt_errmsg_dispatch_s ( win, sev, msg_id,      \
           __XVTFILE__, (unsigned short)__LINE__, sarg )
#ifndef XVT_NO_PATCHES
#define xvt_errmsg_std_s( win, sev, msg_id, sarg )     \
		xvt_errmsg_dispatch_s ( win, sev, msg_id,      \
           __XVTFILE__, (unsigned short)__LINE__, sarg )
#endif


#define xvt_errmsg_sig_if( cond, win, sev, cat, suffix, num, text ) \
		{ if ( cond ) xvt_errmsg_sig ( win, sev, cat, suffix, num, text ); }
 
#define xvt_errmsg_sig_std_if( cond, win, sev, msg_id ) \
		{ if ( cond ) xvt_errmsg_std ( win, sev, msg_id ); }
#ifndef XVT_NO_PATCHES
#define xvt_errmsg_std_if( cond, win, sev, msg_id ) \
		{ if ( cond ) xvt_errmsg_std ( win, sev, msg_id ); }
#endif

/*==========================================================================
	Error Message Object Interface
----------------------------------------------------------------------------
*/
extern XVT_ERRSEV                      XVT_CALLCONV1
       xvt_errmsg_get_sev_id           XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_sev_text         XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern XVT_ERRID                       XVT_CALLCONV1
       xvt_errmsg_get_msg_id           XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_msg_text         XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_cat_text         XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern long int                        XVT_CALLCONV1
       xvt_errmsg_get_code_line        XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_code_file        XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_api_name         XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern WINDOW                          XVT_CALLCONV1
       xvt_errmsg_get_tgt_object       XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern WINDOW                          XVT_CALLCONV1
       xvt_errmsg_get_req_object       XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg      /* Error Message Object handle (ptr) */
       );
 
extern const char *                    XVT_CALLCONV1
       xvt_errmsg_get_text             XVT_CALLCONV2
       (
       XVT_ERRMSG          errmsg,     /* Error Message Object handle (ptr) */
       XVT_ERRID           msg_id,     /* Inquired message text id    */
       char*               buf,        /* Buffer for delivered message text */
       long int            bufsiz      /* Buffer size */
       );
 

/*=========================================================================
   Error Message Identifiers (ERRIDs)
---------------------------------------------------------------------------
	ERRID is a concatenation of category and message number fields, which
	can be accessed separately using the set of macros below.
    NO assumptions shall be made about ERRID field sizes and/or placement.

	The standard error categories and a standard set of messages (ERRIDs)
	are defined by the (platform specific) header file xvt_perr.h, which is
	created by the ERRSCAN tool. 
	The standard categories and messages used by the toolkit are maintained
	in the file verrtxt.c (ERRSCAN merely extracts the info from there).
	When defining a new toolkit category/message, use the verrtxt.c and 
	rerun ERRSCAN.

	When defining a new application category/message, include "dummy" code
	following an example below in your code:
	
	xvt_errmsg_def_cat(ERR_APP, "DOC", 2, "Application DOC errors");
	xvt_errmsg_def_num(ERR_APP_DOC, "BADVAL", 3, "Bad document value");

	The first line defines an application category, giving the "suffix" DOC
	and a minor category number (2) within ERR_APP.
	The next line defines an error message within DOC category, giving it
	name suffix BADVAL and number (3) within DOC. This message will be
	identified by name ERR_APP_DOC_BADVAL.
	There may be up to 15 minor categories within ERR_APP, and up to 3000
	messages within each category.
*/

/* The following dummy macros are used to define ERRIDs for ERRSCAN */
#define xvt_errmsg_def_mjr( ERR,      suffix_string, num, msg_text_string )
#define xvt_errmsg_def_cat( mjr_cat,  suffix_string, num, msg_text_string )
#define xvt_errmsg_def_num( category, suffix_string, num, msg_text_string )
#define xvt_errmsg_def_std( category, suffix_string, num, msg_text_string )

/* ERRID field definitions */
#define XVT_ERRID_MASK_NUM 		0x0000FFFF
#define XVT_ERRID_OFF_NUM		0
#define XVT_ERRID_MASK_CAT		0x00FF0000
#define XVT_ERRID_OFF_CAT		16
#define XVT_ERRID_MASK_MNR		0x000F0000
#define XVT_ERRID_OFF_MNR		16
#define XVT_ERRID_MASK_MJR		0x00F00000
#define XVT_ERRID_OFF_MJR		20
#define XVT_ERRID_MASK_STD		0x01000000
#define XVT_ERRID_OFF_STD		24

/* XVT_ERRID "object" building and access macros, note _gf_ are "private" */
#define xvt_errid_create_mjr(dum,mjr) (      (((XVT_ERRID)(mjr))<<XVT_ERRID_OFF_MJR))
#define xvt_errid_create_cat(mjr,mnr) ((mjr)|(((XVT_ERRID)(mnr))<<XVT_ERRID_OFF_MNR))
#define xvt_errid_create_num(cat,num) ((cat)|(((XVT_ERRID)(num))<<XVT_ERRID_OFF_NUM))
#define xvt_errid_create_std(cat,num) ((cat)|(((XVT_ERRID)(num))<<XVT_ERRID_OFF_NUM)\
                                            |   XVT_ERRID_MASK_STD)

#define xvt_errid_gf_cat(msg_id) ((msg_id) & XVT_ERRID_MASK_CAT)
#define xvt_errid_gf_num(msg_id) ((msg_id) & XVT_ERRID_MASK_NUM)
#define xvt_errid_gf_mnr(msg_id) ((msg_id) & XVT_ERRID_MASK_MNR)
#define xvt_errid_gf_mjr(msg_id) ((msg_id) & XVT_ERRID_MASK_MJR)

#define xvt_errid_get_cat(msg_id) (xvt_errid_gf_cat(msg_id)>>XVT_ERRID_OFF_CAT)
#define xvt_errid_get_num(msg_id) (xvt_errid_gf_num(msg_id)>>XVT_ERRID_OFF_NUM)
#define xvt_errid_get_mnr(msg_id) (xvt_errid_gf_mnr(msg_id)>>XVT_ERRID_OFF_MNR)
#define xvt_errid_get_mjr(msg_id) (xvt_errid_gf_mjr(msg_id)>>XVT_ERRID_OFF_MJR)

/* Error category comparisons */
#define xvt_errid_is_mjr(msg_id, cat ) \
       (xvt_errid_gf_mjr(msg_id) == xvt_errid_gf_mjr(cat))
#define xvt_errid_is_mnr(msg_id, cat ) \
       (xvt_errid_gf_mnr(msg_id) == xvt_errid_gf_mnr(cat))
#define xvt_errid_is_cat(msg_id, cat ) \
       (xvt_errid_gf_cat(msg_id) == xvt_errid_gf_cat(cat))				
#define xvt_errid_is_std(msg_id, unused ) \
	   ((msg_id)&XVT_ERRID_MASK_STD)

/*===========================================================================
   To follow the XVT assert4 legacy, the range 0-30000 is reserved for
   application (if it chooses to use any of the XVT error categories).
   The toolkit space 30000 - 65535 should be divided into 1000 message ID 
   chunks as follows:
 
    30000 - 30999:  T-layer ( xvt... common code)
    31000 - 31999:  V-layer ( common code )
    32000 - 32999:  O-layer ( common code )
    33000 - 39999:  -
    40000 - 40999:  K-layer, Mac
    41000 - 42999:  -
    43000 - 43999:  K-layer, Ch
    44000 - 49999:  -
    50000 - 50999:  K-layer, Win common code
    51000 - 51999:  K-layer, Win 3.x
    52000 - 52999:  K-layer, Win NT
    55000 - 55999:  K-layer, PM
    56000 - 59999:  -
    60000 - 60999:  K-layer, X common code
    61000 - 61999:  K-layer, Xm
    62000 - 62999:  K-layer, Xol
    63000 - 65535:  -
 
   Within each of the message ID chunks above, toolkits are free to set
   a policy of their own.

---------------------------------------------------------------------------*/

#endif /* XVT_INCL_XVTERR */
