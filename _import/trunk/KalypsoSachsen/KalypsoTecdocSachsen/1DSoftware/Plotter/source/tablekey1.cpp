// TableKey1.cpp: Implementierung der Klasse CTableKey1.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "plotdoc.h"

#include "TableKey1.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CTableKey1::Update( CPlotterDoc* pDoc, const UINT wmax, const CIntPoint& basePoint, const CUIntArray& tableHeights )
// setzt die Grösse und Positionen aller Elemente des Key1 relativ zum Ursprung
// Parameter:
//        const UINT wmax: die Breite des Key1
//        const CPoint& basePoint: hierin soll der Key1
{
  // muss 0 sein wegen LogicalToMeters
  SetRect( CIntIRect( 0, 0, 0, 0 ) );

  CIntPoint offsetPoint = basePoint;

  CIntIRect total( INT_MAX, -INT_MAX, -INT_MAX, INT_MAX );
  for( int i = GetSize() - 1; i >= 0; i-- )
  {
    CIntIRect rect( 0, tableHeights[i], wmax, 0 );

    CDrawObjList* tableRow = GetAt( i );
    tableRow->SetRect( CIntIRect( 0, 0, 0, 0 ) );
    
    POSITION pos = tableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = tableRow->GetNextObject(pos);
      if (!pObj->IsInvisible())
      {
        pObj->m_position = rect;

        double top = pObj->m_dPosition.top;
        double bottom = pObj->m_dPosition.bottom;
        pDoc->LogicalToMeters( pObj->m_position, pObj->m_dPosition, pObj );
        pObj->m_dPosition.top = top;
        pObj->m_dPosition.bottom = bottom;
      }
    } // while pos
    rect += offsetPoint;
    offsetPoint.y = rect.top;
    tableRow->SetRect( rect );
    total.CompareAndExpand( rect );
  }
  
  CIntIRect rect( total.left, total.top, total.right, total.bottom );
  SetRect( rect );
} // UpdateKey1