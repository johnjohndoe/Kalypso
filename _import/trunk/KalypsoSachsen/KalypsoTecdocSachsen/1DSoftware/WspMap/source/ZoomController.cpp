// ZoomController.cpp: Implementierung der Klasse CZoomController.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"

#include "mapView.h"
#include "mapDoc.h"

#include "ZoomController.h"

#ifdef _DEBUG
#undef THIS_FILE
static char THIS_FILE[]=__FILE__;
#define new DEBUG_NEW
#endif

void CZoomController::OnMapMouseDown( CMoPoint& cursor, short Button, short Shift )
{
  CMoMap* map = m_view->GetDocument()->GetMap();
  
  CMoRectangle r( map->TrackRectangle() );
  if( LPDISPATCH(r) )
    m_view->GetDocument()->SetExtent( r );
}
