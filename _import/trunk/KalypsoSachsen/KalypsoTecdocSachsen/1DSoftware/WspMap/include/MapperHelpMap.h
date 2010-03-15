// MapperHelpMap.h: Schnittstelle für die Klasse CMapperHelpMap.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_MAPPERHELPMAP_H__599E3E94_F1B0_4E3B_82C8_9B0A54A007E4__INCLUDED_)
#define AFX_MAPPERHELPMAP_H__599E3E94_F1B0_4E3B_82C8_9B0A54A007E4__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\commonMfc\commonMfc.h"

// Diese Klasse stellt nur eine statische Zuordnung von Resource IDs zu Sprungzielen
// in der Hilfe dar
class CMapperHelpMap
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

#endif 


