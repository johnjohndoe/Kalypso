// Global.h
//
/////////////////////////////////////////////////////////////////////////////

#ifndef GLOBAL_H
#define GLOBAL_H

// include file for standard system include files,
// or project specific include files that are used frequently, but
// are changed infrequently
//
// wird für Vorkompilierte Header benutzt

#define VC_EXTRALEAN		// Exclude rarely-used stuff from Windows headers


// MFC Includes
#include <afx.h>
#include <afxwin.h>         // MFC core and standard components
#include <afxext.h>         // MFC extensions
#include <afxctl.h>
#include <atlbase.h> // für CComVariant
#include <afxdao.h>
#include <afxtempl.h>
#include <afxcmn.h>
#include <afxadv.h>
#include <afxpriv.h>
#include <afxodlgs.h>

// Standard Includes
#include <dde.h>
#include <cderr.h> // für CommDlgExtendedError
#include <shlobj.h>
#include <comutil.h>
#include <dos.h>
#include <direct.h>
#include <math.h>
#include <float.h>
#include <locale.h>

#include <utility>
#include <list>
#include <vector>
#include <map>


// MapObjects Includes
#include "..\mapObjects\mo2defs.h"
#include "..\mapObjects\MapObjects2.h"
#include "..\mapObjects\moLegend.h"
#include "..\mapObjects\moSBExtent.h"
#include "..\mapObjects\moScalebar.h"

typedef CArray<CMoPolygon, CMoPolygon&> CMoPolyArray;

#endif	// GLOBAL_H