/********************************************************************
    Copyright (c) 1993-96 XVT Software.

    $RCSfile: xvtpwr.h,v $
    $Revision: 1.8 $
    $Date: 1996/12/30 23:53:20 $
 
    File Name: XVTPwr.h

    Purpose:
        Top level header for PWR++ applications.

********************************************************************/

#ifndef XVTPWR_H
#define XVTPWR_H

#ifndef XVT_INCL_XVT
#include "xvt.h"
#endif

#if XVT_FILESYS_MAC
#define PWR_INCL_PATH( a, b, c ) #b
#elif XVT_FILESYS_DOS || XVT_FILESYS_NTFS || XVT_FILESYS_HPFS
#define PWR_QUOTE( a ) #a
#define PWR_INCL_PATH( a, b, c ) PWR_QUOTE( a##/##c )
#else
#define PWR_QUOTE( a ) #a
#define PWR_INCL_PATH( a, b, c ) PWR_QUOTE( a##/##b )
#endif

#ifndef PwrDef_H
#include "PwrDef.h"
#endif

#ifndef PwrNames_H
#include "PwrNames.h"
#endif

#include "PwrRid.h"

#include PWR_INCL_PATH( pwr, Global.h, global.h )

#endif


