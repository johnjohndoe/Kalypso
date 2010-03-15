// plotterhelpmap.h: Schnittstelle für die Klasse CPlotterHelpMap.
//
//////////////////////////////////////////////////////////////////////

#ifndef _STEMPELHELP_MAP_INCLUDED_
#define _STEMPELHELP_MAP_INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\commonMfc\commonMfc.h"

// Diese Klasse stellt nur eine statische Zuordnung von Resource IDs zu Sprungzielen
// in der Hilfe dar
class CStempelHelpMap  
{
public:
  static FillHelpMap( CMapUIntToString& idMap );
};

#endif // _STEMPELHELP_MAP_INCLUDED_
