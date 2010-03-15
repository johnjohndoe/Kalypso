// plotterhelpmap.cpp: Implementierung der Klasse CPlotterHelpMap.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "resource.h"

#include "stempelhelpmap.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

/* static */ CStempelHelpMap::FillHelpMap( CMapUIntToString& idMap )
// Füllt eine CMapUIntToString mit Werten
{
  // das Default-Sprungziel:
  idMap.SetAt( 0, TEXT("cmd:extras-stempel") );


  //idMap.SetAt( 0, TEXT("") );

} // FillHelpMap