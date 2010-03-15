// TableKey2.h: Schnittstelle für die Klasse CTableKey2.
//
//////////////////////////////////////////////////////////////////////

#if !defined(AFX_TABLEKEY2_H__97111D34_7943_11D6_B2E4_00104BB3E525__INCLUDED_)
#define AFX_TABLEKEY2_H__97111D34_7943_11D6_B2E4_00104BB3E525__INCLUDED_

#if _MSC_VER > 1000
#pragma once
#endif // _MSC_VER > 1000

#include "..\..\DRAW\INCLUDE\Drawobj.h"

class CTableKey2 : public CDrawObjListArray  
{
  //////////////////////////////////////////////////////////////////////
  // Operationen
  //////////////////////////////////////////////////////////////////////
public:
  void Update( CPlotterDoc* pDoc, const UINT wmax, const CIntPoint& basePoint, const CUIntArray& tableHeights );
};

#endif // !defined(AFX_TABLEKEY2_H__97111D34_7943_11D6_B2E4_00104BB3E525__INCLUDED_)
