/****************************************************************************
 *
 *  Copyright 1987-1996 XVT Software.  All rights reserved.
 *  May be used only in accordance with a valid Source Code License
 *  Agreement with XVT Software.
 *
 *    $RCSfile: xvt.h,v $
 *    $Revision: 1.9 $
 *
 *  Purpose:  XVT application programming interface.
 *
 ****************************************************************************/

#ifndef XVT_INCL_XVT
#define XVT_INCL_XVT

/* include version-specific environment information */
#include "xvt_vers.h"

/* include platform-specific environment information */
#include "xvt_env.h"

/* include standard library headers */
#if !defined(NO_INCLUDES) && (XVT_CC != XVT_CC_CURL)
#include <ctype.h>
#include <stdio.h>
#include <stddef.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "xvt_varg.h"   /*  variable args header */
#endif  /* NO_INCLUDES */

#if defined(_cplusplus) || defined(__cplusplus)
extern "C" {
#endif

/* include XVT globals */
#include "xvt_defs.h"
#include "xvt_menu.h"
#include "xvt_type.h"
#include "xvt_perr.h"
#include "xvt_help.h"

/* include function prototypes for the XVT api */
#include "xvt_opt.h"
#include "xvtapp.h"
#include "xvtcb.h"
#include "xvtctl.h"
#include "xvtcxo.h"
#include "xvtdebug.h"
#include "xvtdlg.h"
#include "xvtdm.h"
#include "xvtdwin.h"
#include "xvterr.h"
#include "xvtevent.h"
#include "xvtfont.h"
#include "xvtfsys.h"
#include "xvtgmem.h"
#include "xvtiostr.h"
#include "xvtimage.h"
#include "xvtlist.h"
#include "xvtmenu.h"
#include "xvtmem.h"
#include "xvtnav.h"
#include "xvtpalet.h"
#include "xvtpat.h"
#include "xvtpict.h"
#include "xvtpmap.h"
#include "xvtprint.h"
#include "xvtrect.h"
#include "xvtres.h"
#include "xvtsbar.h"
#include "xvtscr.h"
#include "xvtslist.h"
#include "xvtstr.h"
#include "xvttimer.h"
#include "xvttx.h"
#include "xvtvobj.h"
#include "xvtwin.h"

#if defined(_cplusplus) || defined(__cplusplus)
}     /* extern "C" */
#endif

#endif /* XVT_INCL_XVT */
