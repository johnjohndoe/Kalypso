// plotterhelpmap.h: Schnittstelle für die Klasse CPlotterHelpMap.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_PLOTTERHELPMAP_H__A79357F3_9495_11D6_B2FB_00104BB3E525__INCLUDED_)
#define AFX_PLOTTERHELPMAP_H__A79357F3_9495_11D6_B2FB_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\commonMfc\commonMfc.h"

// Diese Klasse stellt nur eine statische Zuordnung von Resource IDs zu Sprungzielen
// in der Hilfe dar
class CPlotterHelpMap
{
  enum { count = 1000 };

  // Internes Struct für statische IdMap
  struct IdMap
  {
    // Standardkonstruktor, um überzählige Elemente zu initialisieren
    IdMap() { id = 0xffff; target = ""; }
    IdMap( const int i, LPCTSTR t ) { id = i; target = t; }

    int id;
    LPCTSTR target;
  };

public:
  static FillHelpMap( CMapUIntToString& idMap );

private:
  static IdMap m_idMap[count];
};

#endif // !defined(AFX_PLOTTERHELPMAP_H__A79357F3_9495_11D6_B2FB_00104BB3E525__INCLUDED_)
