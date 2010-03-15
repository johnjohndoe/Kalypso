/**************************************************************************
 * MODULE NAME: xvt_srid.h 
 * DESCRIPTION: XVT strings used by the PTK
 *
 * NOTE: This file will have to be changed during localization. It 
 *       defines the resource id values for strings used inside the PTK.
 *
 * Copyright 1987-1996 XVT Software.  All rights reserved.
 * May not be distributed in any form or by any means, and may
 * not be used to produce derived works, including compiled object
 * code, without permission in writing from XVT Software.
 *************************************************************************/

#ifndef _xvtsrid_h_
#define _xvtsrid_h_

/* Maximum size of buffer needed to contain multibyte strings. 
 * Change if needed */
#define XVT_STRING_BUF_MAX 256

/* define the resource ids for the strings */
#define RID_NULL_ARG_STR              XVTV_STRING_RES_BASE + 100
#define RID_EMPTY_LIST_STR            XVTV_STRING_RES_BASE + 101
#define RID_SLIST_GET_FAIL_STR        XVTV_STRING_RES_BASE + 102
#define RID_NAMED_COL_NOT_FOUND_STR   XVTV_STRING_RES_BASE + 103
#define RID_FATAL_RECURSE_STR         XVTV_STRING_RES_BASE + 104
#define RID_INV_ERRMSG_STR            XVTV_STRING_RES_BASE + 105
#define RID_UNKNOWN_STR               XVTV_STRING_RES_BASE + 106
#define RID_DEF_TWICE_STR             XVTV_STRING_RES_BASE + 107
#define RID_PASSWORD_ECHO_CHAR_STR    XVTV_STRING_RES_BASE + 108

#endif /* _xvtsrid_h_ */
