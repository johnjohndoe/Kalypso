// stdafx.h : include file for standard system include files,
//  or project specific include files that are used frequently, but
//      are changed infrequently
//

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers

#pragma warning ( disable : 4786 )

#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions

#include <afxcmn.h>			// MFC support for Windows 95 Common Controls

#include <afxtempl.h>
#include <afxole.h>
#include <afxpriv.h>
#include <afxodlgs.h>

// Win-Api
// and C-Headers ( diese sollten nicht mehr benutzt werden )
#include <dde.h>
#include <locale.h>
#include <dos.h>
#include <direct.h>
#include <fstream.h>
#include <math.h>
#include <iomanip.h>

// STL - Headers
#include <fstream>
#include <cfloat>

#include "resource.h"