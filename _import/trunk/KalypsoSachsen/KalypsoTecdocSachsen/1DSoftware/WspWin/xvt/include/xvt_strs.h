/**************************************************************************
 * MODULE NAME: xvt_strs.h 
 * DESCRIPTION: XVT strings used by the PTK
 *
 * NOTE: This file will have to be changed during localization. It contains
 *       the URL string definitions for strings used inside the PTK.
 *
 * Copyright 1987-1996 XVT Software.  All rights reserved.
 * May not be distributed in any form or by any means, and may
 * not be used to produce derived works, including compiled object
 * code, without permission in writing from XVT Software.
 *************************************************************************/

#ifndef _xvtstrs_h_
#define _xvtstrs_h_

#include "xvt_srid.h" /* The #defines for the string resources defined here */

/* define the string resources. The STR_CC_* strings are defined
 * in one of the u<lang><codeset>.h files ("uengasc.h" for english) */
string RID_NULL_ARG_STR            STR_CC_NULL_ARG_STR            
string RID_EMPTY_LIST_STR          STR_CC_EMPTY_LIST_STR          
string RID_SLIST_GET_FAIL_STR      STR_CC_SLIST_GET_FAIL_STR      
string RID_NAMED_COL_NOT_FOUND_STR STR_CC_NAMED_COL_NOT_FOUND_STR 
string RID_FATAL_RECURSE_STR       STR_CC_FATAL_RECURSE_STR       
string RID_INV_ERRMSG_STR          STR_CC_INV_ERRMSG_STR          
string RID_UNKNOWN_STR             STR_CC_UNKNOWN_STR             
string RID_DEF_TWICE_STR           STR_CC_DEF_TWICE_STR           

#endif /* _xvtstrs_h_ */
