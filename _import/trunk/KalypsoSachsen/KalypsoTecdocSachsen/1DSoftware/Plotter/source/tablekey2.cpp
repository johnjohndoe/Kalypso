// TableKey2.cpp: Implementierung der Klasse CTableKey2.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "plotdoc.h"

#include "TableKey2.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

//////////////////////////////////////////////////////////////////////
// Operationen
//////////////////////////////////////////////////////////////////////

void CTableKey2::Update( CPlotterDoc* pDoc, const UINT wmax, const CIntPoint& basePoint, const CUIntArray& tableHeights )
{
  CIntPoint offsetPoint = basePoint;

  SetRect( CIntIRect( 0, 0, 0, 0 ) );

  // die einzelnen Elemente relativ zum Ursprung positionieren
  CIntIRect total( INT_MAX, -INT_MAX, -INT_MAX, INT_MAX );
  for( int i = GetSize() - 1; i >= 0; i-- )
  {
    CDrawObjList* tableRow = GetAt( i );
    tableRow->SetRect( CIntIRect( 0, 0, 0, 0 ) ); // damit Logical to Meters hier richtig funktioniert

    CIntIRect rect( 0, tableHeights[i], wmax, 0 );
    POSITION pos = tableRow->GetHeadPosition();
    while( pos != NULL )
    {
      CDrawObj* pObj = tableRow->GetNextObject( pos );
      if( !pObj->IsInvisible() )
      {
        if( pObj->IsConnected() )
          pObj->m_position = CIntIRect( wmax/10, tableHeights[i]/2, wmax*9/10, 0 );
        else
          pObj->m_position = rect;

        double top = pObj->m_dPosition.top;
        double bottom = pObj->m_dPosition.bottom;
        pDoc->LogicalToMeters(pObj->m_position, pObj->m_dPosition, pObj);
        pObj->m_dPosition.top = top;
        pObj->m_dPosition.bottom = bottom;
      }
    }
    rect += offsetPoint;
    offsetPoint.y = rect.top;
    tableRow->SetRect( rect );
    total.CompareAndExpand( rect );
  }

  CIntIRect rect( total.left, total.top, total.right, total.bottom );
  SetRect( rect );
} // UpdateKey2
